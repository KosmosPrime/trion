use core::fmt;
use core::mem;
use std::borrow::Cow;
use std::error::Error;
use std::fs::OpenOptions;
use std::io::{self, Read};
use std::sync::Arc;

use crate::asm::{ConstantError, Context, ErrorLevel, SegmentError};
use crate::asm::constant::Realm;
use crate::asm::directive::{Directive, DirectiveErrorKind};
use crate::asm::simplify::{evaluate, EvalError, Evaluation};
use crate::text::{Positioned, PosNamed};
use crate::text::parse::{Argument, ArgumentType};
use crate::text::token::Number;

enum DataOp<'c>
{
	Completed,
	Deferred{cause: Cow<'c, str>},
}

struct DataExpr<'l>
{
	dir_name: &'static str,
	file_name: Arc<String>,
	line: u32,
	col: u32,
	addr: u32,
	arg: Argument<'l>,
	writer: fn(&mut Context, &mut DataExpr<'_>) -> Result<(), ErrorLevel>,
}

impl<'l> DataExpr<'l>
{
	fn new(dir_name: &'static str, file_name: Arc<String>, line: u32, col: u32, addr: u32, arg: Argument<'l>,
		writer: fn(&mut Context, &mut DataExpr<'_>) -> Result<(), ErrorLevel>) -> Self
	{
		Self{dir_name, file_name, line, col, addr, arg, writer}
	}
	
	fn push_error<E: Error + 'static>(&mut self, ctx: &mut Context, source: E)
	{
		self.push_error_raw(ctx, DirectiveErrorKind::Apply{dir: self.dir_name.to_owned(), source: Box::new(source)});
	}
	
	fn push_error_raw(&mut self, ctx: &mut Context, err: DirectiveErrorKind)
	{
		ctx.push_error_in(PosNamed{name: self.file_name.clone(), line: self.line, col: self.col, value: err});
	}
	
	fn into_owned(self) -> DataExpr<'static>
	{
		DataExpr
		{
			dir_name: self.dir_name,
			file_name: self.file_name,
			line: self.line,
			col: self.col,
			addr: self.addr,
			arg: self.arg.into_owned(),
			writer: self.writer,
		}
	}
	
	fn apply<'s>(&'s mut self, ctx: &mut Context, local: bool) -> Result<DataOp<'s>, ErrorLevel>
	{
		match evaluate(&mut self.arg, ctx)
		{
			Ok(Evaluation::Complete{..}) =>
			{
				match (self.writer)(ctx, self)
				{
					Ok(()) => Ok(DataOp::Completed),
					Err(e) => Err(e),
				}
			},
			Ok(Evaluation::Deferred{cause, ..}) => return Ok(DataOp::Deferred{cause}),
			Err(e) =>
			{
				if local
				{
					if let EvalError::NoSuchVariable{name, ..} = e
					{
						return Ok(DataOp::Deferred{cause: Cow::Owned(name)});
					}
				}
				self.push_error(ctx, e);
				Err(ErrorLevel::Trivial)
			},
		}
	}
	
	fn write_data(&mut self, ctx: &mut Context, data: &[u8]) -> Result<(), ErrorLevel>
	{
		match ctx.active_mut()
		{
			Some(active) if self.addr >= active.base_addr() && self.addr <= active.curr_addr() =>
			{
				if let Err(e) = active.write_at(self.addr, data)
				{
					self.push_error(ctx, DataError::Write(e));
					return Err(ErrorLevel::Fatal);
				}
			},
			_ =>
			{
				match ctx.output_mut().put(self.addr, data)
				{
					Ok(n) => assert_eq!(n, 0), // the active segment has changed, this ensures the bytes have been pre-allocated
					Err(e) =>
					{
						self.push_error(ctx, DataError::Write(SegmentError::Write(e)));
						return Err(ErrorLevel::Fatal);
					},
				}
			},
		};
		Ok(())
	}
}

impl DataExpr<'static>
{
	fn schedule(mut self, ctx: &mut Context, global: bool)
	{
		ctx.add_task(Box::new(move |ctx|
		{
			match self.apply(ctx, false)
			{
				Ok(DataOp::Completed) => Ok(()),
				Ok(DataOp::Deferred{cause}) =>
				{
					if global
					{
						let name = cause.into_owned();
						self.push_error(ctx, ConstantError::NotFound{name, realm: Realm::Global});
						return Err(ErrorLevel::Trivial);
					}
					self.schedule(ctx, true);
					Ok(())
				},
				Err(e) => Err(e),
			}
		}), if global {Realm::Global} else {Realm::Local});
	}
}

macro_rules!generate_expr
{
	($name:ident($type:ty, $size:expr) as $label:literal => |$value:ident| {$($write:tt)+}) =>
	{
		#[derive(Clone, Copy, Debug)]
		pub struct $name;
		
		impl Directive for $name
		{
			fn get_name(&self) -> &str
			{
				$label
			}
			
			fn apply(&self, ctx: & mut Context, mut args: Positioned<Vec<Argument>>) -> Result<(), ErrorLevel>
			{
				let addr = match ctx.active()
				{
					None =>
					{
						let source = Box::new(DataError::Inactive);
						ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: $label.to_owned(), source}));
						return Err(ErrorLevel::Fatal);
					},
					Some(seg) => seg.curr_addr(),
				};
				const NUM_ARGS: usize = 1;
				if args.value.len() != NUM_ARGS
				{
					let dir = self.get_name().to_owned();
					ctx.push_error(args.convert(
						if args.value.len() < NUM_ARGS {DirectiveErrorKind::NotEnoughArguments{dir, need: NUM_ARGS, have: args.value.len()}}
						else {DirectiveErrorKind::TooManyArguments{dir, max: NUM_ARGS, have: args.value.len()}}
					));
					return Err(ErrorLevel::Trivial);
				}
				let mut data = DataExpr::new($label, ctx.curr_file_name(), args.line, args.col, addr, args.value.pop().unwrap(), |ctx, data|
				{
					match data.arg
					{
						Argument::Constant(Number::Integer(val)) =>
						{
							match <$type>::try_from(val)
							{
								Ok($value) => data.write_data(ctx, &{$($write)+}),
								_ =>
								{
									data.push_error(ctx, DataError::Range{min: i64::from(<$type>::MIN), max: i64::from(<$type>::MAX), have: val});
									Err(ErrorLevel::Trivial)
								},
							}
						},
						ref arg =>
						{
							let dir = $label.to_owned();
							let have = arg.get_type();
							data.push_error_raw(ctx, DirectiveErrorKind::ArgumentType{dir, idx: 0, expect: ArgumentType::Constant, have});
							Err(ErrorLevel::Trivial)
						},
					}
				});
				match data.apply(ctx, true)
				{
					Ok(DataOp::Completed) => Ok(()),
					_ =>
					{
						data.write_data(ctx, &[0xBE; $size])?; // padding for whatever follows
						data.into_owned().schedule(ctx, false);
						Ok(())
					},
				}
			}
		}
	};
}

generate_expr!(DataU8(u8, mem::size_of::<u8>()) as "du8" => |value| {[value as u8]});
generate_expr!(DataU16(u16, mem::size_of::<u16>()) as "du16" => |value| {u16::to_le_bytes(value)});
generate_expr!(DataU32(u32, mem::size_of::<u32>()) as "du32" => |value| {u32::to_le_bytes(value)});

macro_rules!generate
{
	($name:ident($($argt:tt)+) as $label:literal => |$self:ident, $ctx:ident, $active:ident, $args:ident, $value:ident| {$($write:tt)+}) =>
	{
		#[derive(Clone, Copy, Debug)]
		pub struct $name;
		
		impl Directive for $name
		{
			fn get_name(&self) -> &str
			{
				$label
			}
			
			fn apply(&$self, $ctx: &mut Context, $args: Positioned<Vec<Argument>>) -> Result<(), ErrorLevel>
			{
				let Some($active) = $ctx.active_mut()
				else
				{
					let source = Box::new(DataError::Inactive);
					$ctx.push_error($args.convert(DirectiveErrorKind::Apply{dir: $label.to_owned(), source}));
					return Err(ErrorLevel::Fatal);
				};
				const NUM_ARGS: usize = 1;
				if $args.value.len() != NUM_ARGS
				{
					let dir = $label.to_owned();
					$ctx.push_error($args.convert(
						if $args.value.len() < NUM_ARGS {DirectiveErrorKind::NotEnoughArguments{dir, need: NUM_ARGS, have: $args.value.len()}}
						else {DirectiveErrorKind::TooManyArguments{dir, max: NUM_ARGS, have: $args.value.len()}}
					));
					return Err(ErrorLevel::Trivial);
				}
				let $value = generate!(@impl($self) $($argt)+ = $args[0], $ctx, $active);
				if let Err(e) = {$($write)+}
				{
					let source = Box::new(DataError::Write(e));
					$ctx.push_error($args.convert(DirectiveErrorKind::Apply{dir: $label.to_owned(), source}));
					return Err(ErrorLevel::Fatal);
				}
				Ok(())
			}
		}
	};
	(@impl($self:ident) String = $args:ident[$idx:expr], $ctx:ident, $active:ident) =>
	{
		match $args.value[$idx]
		{
			Argument::String(ref string) => string.as_ref(),
			ref arg =>
			{
				let dir = $self.get_name().to_owned();
				let err = DirectiveErrorKind::ArgumentType{dir, idx: $idx, expect: ArgumentType::String, have: arg.get_type()};
				$ctx.push_error($args.convert(err));
				return Err(ErrorLevel::Trivial);
			},
		}
	};
}

generate!
{
	DataHex(String) as "dhex" => |self, ctx, active, args, in_hex|
	{
		let mut out_data = Vec::new();
		let mut carry: Option<u8> = None;
		for (pos, c) in in_hex.chars().enumerate().filter(|(_, c)| !c.is_ascii_whitespace())
		{
			match c.to_digit(16)
			{
				None =>
				{
					let source = Box::new(DataError::HexChar{pos, value: c});
					ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
					return Err(ErrorLevel::Fatal);
				},
				Some(v) =>
				{
					let v = v as u8;
					match carry
					{
						None => carry = Some(v << 4),
						Some(c) =>
						{
							out_data.push(v | c);
							carry = None;
						},
					}
				},
			}
		}
		if carry.is_some()
		{
			let source = Box::new(DataError::HexEof);
			ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
			return Err(ErrorLevel::Fatal);
		}
		active.write(out_data.as_slice())
	}
}
generate!(DataStr(String) as "dstr" => |self, ctx, active, args, value| {active.write(value.as_bytes())});
generate!
{
	DataFile(String) as "dfile" => |self, ctx, _active, args, path|
	{
		let mut path_buff = ctx.curr_file_path().unwrap().to_path_buf();
		if !path_buff.pop() {path_buff.push("..");}
		path_buff.push(path);
		let active = ctx.active_mut().unwrap();
		match OpenOptions::new().read(true).open(path_buff)
		{
			Ok(mut f) =>
			{
				let len = match f.metadata()
				{
					Ok(meta) =>
					{
						match usize::try_from(meta.len())
						{
							Ok(len) =>
							{
								if !active.has_remaining(len)
								{
									let source = Box::new(DataError::Write(SegmentError::Overflow{need: len, have: active.remaining()}));
									ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
									return Err(ErrorLevel::Fatal);
								}
								len
							},
							Err(..) =>
							{
								let source = Box::new(DataError::Write(SegmentError::Overflow{need: usize::MAX, have: active.remaining()}));
								ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
								return Err(ErrorLevel::Fatal);
							},
						}
					},
					Err(e) =>
					{
						let source = Box::new(DataError::File(e));
						ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
						return Err(ErrorLevel::Fatal);
					},
				};
				
				let mut pos = 0;
				let mut temp = [0u8; 1024];
				while pos < len
				{
					match f.read(&mut temp)
					{
						Ok(cnt) =>
						{
							if len - pos < cnt
							{
								if let Err(e) = active.write(&temp[..len - pos])
								{
									let source = Box::new(DataError::Write(e));
									ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
									return Err(ErrorLevel::Fatal);
								}
								// no need to update pos again
								//pos = len;
								break;
							}
							else
							{
								if let Err(e) = active.write(&temp[..cnt])
								{
									let source = Box::new(DataError::Write(e));
									ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
									return Err(ErrorLevel::Fatal);
								}
								pos += cnt;
							}
						},
						Err(e) =>
						{
							let source = Box::new(DataError::File(e));
							ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
							return Err(ErrorLevel::Fatal);
						},
					}
				}
				// reading less than expected is fine
				drop(f);
				Ok(())
			},
			Err(e) =>
			{
				let source = Box::new(DataError::File(e));
				ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
				return Err(ErrorLevel::Fatal);
			},
		}
	}
}

#[derive(Debug)]
pub enum DataError
{
	Inactive,
	Range{min: i64, max: i64, have: i64},
	HexChar{pos: usize, value: char},
	HexEof,
	File(io::Error),
	Write(SegmentError),
}

impl fmt::Display for DataError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Inactive => f.write_str("no active segment to write to"),
			&Self::Range{min, max, have} => write!(f, "constant out of range ({min} to {max}, got {have})"),
			&Self::HexChar{pos, value} => write!(f, "invalid hex char {value:?} at {pos}"),
			Self::HexEof => f.write_str("unexpected eof in hex string"),
			Self::File(..) => f.write_str("could not access referenced file"),
			Self::Write(..) => f.write_str("could not write data to segment"),
		}
	}
}

impl Error for DataError
{
	fn source(&self) -> Option<&(dyn Error + 'static)>
	{
		match self
		{
			Self::File(e) => Some(e),
			Self::Write(e) => Some(e),
			_ => None,
		}
	}
}
