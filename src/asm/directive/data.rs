use core::fmt;
use core::mem::size_of;
use std::error::Error;
use std::fs::OpenOptions;
use std::io::{self, Read};

use crate::asm::{Context, ErrorLevel, SegmentError};
use crate::asm::constant::{Lookup, Realm};
use crate::asm::directive::{Directive, DirectiveErrorKind};
use crate::asm::memory::map::PutError;
use crate::text::{Positioned, PosNamed};
use crate::text::parse::{Argument, ArgumentType};
use crate::text::token::Number;

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
			
			fn apply(&self, ctx: & mut Context, args: Positioned<&[Argument]>) -> Result<(), ErrorLevel>
			{
				fn write($value: $type) -> [u8; $size] {$($write)+}
				
				fn deferred_write(ctx: &mut Context, addr: PosNamed<u32>, name: String) -> Result<(), ErrorLevel>
				{
					let realm = if ctx.curr_path().is_none() {Realm::Global} else {Realm::Local};
					match ctx.get_constant(name.as_str(), realm)
					{
						Lookup::NotFound =>
						{
							let source = Box::new(DataError::NotFound{name, realm});
							ctx.push_error_in(addr.convert(DirectiveErrorKind::Apply{name: $name.get_name().to_owned(), source}));
						},
						Lookup::Deferred =>
						{
							if realm == Realm::Global
							{
								// we've reached the end of the stack, this constant won't be defined
								let source = Box::new(DataError::NotFound{name, realm});
								ctx.push_error_in(addr.convert(DirectiveErrorKind::Apply{name: $name.get_name().to_owned(), source}));
								return Err(ErrorLevel::Trivial);
							}
							else
							{
								// the constant could be set by a different module later
								ctx.add_task(Box::new(move |ctx| deferred_write(ctx, addr, name)), Realm::Global);
								return Ok(());
							}
						},
						Lookup::Found(val) =>
						{
							match <$type>::try_from(val)
							{
								Ok(v) =>
								{
									let tmp = write(v);
									match ctx.active_mut()
									{
										Some(seg) if addr.value <= seg.curr_addr() && addr.value + (tmp.len() as u32) > seg.base_addr() =>
										{
											// write to active segment if possible
											if let Err(e) = seg.write_at(addr.value, &tmp[..])
											{
												let source = Box::new(DataError::Write(e));
												ctx.push_error_in(addr.convert(DirectiveErrorKind::Apply{name: $name.get_name().to_owned(), source}));
												return Err(ErrorLevel::Trivial);
											}
										},
										_ =>
										{
											// otherwise write directly to output
											if let Err(e) = ctx.output_mut().put(addr.value, &tmp[..])
											{
												let source = Box::new(DataError::Put(e));
												ctx.push_error_in(addr.convert(DirectiveErrorKind::Apply{name: $name.get_name().to_owned(), source}));
												return Err(ErrorLevel::Trivial);
											}
										},
									}
								},
								_ =>
								{
									let source = Box::new(DataError::Range{min: i64::from(<$type>::MIN), max: i64::from(<$type>::MAX), have: val});
									ctx.push_error_in(addr.convert(DirectiveErrorKind::Apply{name: $name.get_name().to_owned(), source}));
									return Err(ErrorLevel::Trivial);
								},
							}
						},
					}
					Ok(())
				}
				
				if ctx.active().is_none()
				{
					let source = Box::new(DataError::Inactive);
					ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
					return Err(ErrorLevel::Fatal);
				}
				if args.value.len() != 1
				{
					ctx.push_error(args.convert_fn(|v| DirectiveErrorKind::ArgumentCount{min: Some(1), max: Some(1), have: v.len()}));
					return Err(ErrorLevel::Trivial);
				}
				let value = match args.value[0]
				{
					Argument::Constant(Number::Integer(v)) => v,
					Argument::Identifier(ref name) =>
					{
						match ctx.get_constant(name.as_ref(), Realm::Local)
						{
							Lookup::Found(v) => v,
							_ =>
							{
								let addr = args.convert(ctx.curr_addr().unwrap()).with_name(ctx.curr_path().unwrap().to_string_lossy().into_owned());
								let name = name.as_ref().to_owned();
								if let Err(e) = ctx.active_mut().unwrap().write(&[0; $size])
								{
									let source = Box::new(DataError::Write(e));
									ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
									return Err(ErrorLevel::Fatal);
								}
								ctx.add_task(Box::new(move |ctx| deferred_write(ctx, addr, name)), Realm::Local);
								return Ok(());
							},
						}
					},
					ref arg =>
					{
						let err = DirectiveErrorKind::Argument{idx: 0, expect: ArgumentType::Constant, have: arg.get_type()};
						ctx.push_error(args.convert(err));
						return Err(ErrorLevel::Trivial);
					},
				};
				match <$type>::try_from(value)
				{
					Ok(v) =>
					{
						if let Err(e) = ctx.active_mut().unwrap().write(&write(v))
						{
							let source = Box::new(DataError::Write(e));
							ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
							return Err(ErrorLevel::Fatal);
						}
					},
					_ =>
					{
						let source = Box::new(DataError::Range{min: i64::from(<$type>::MIN), max: i64::from(<$type>::MAX), have: value});
						ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
						return Err(ErrorLevel::Fatal);
					},
				}
				Ok(())
			}
		}
	};
}

generate_expr!(DataU8(u8, size_of::<u8>()) as "du8" => |value| {[value as u8]});
generate_expr!(DataU16(u16, size_of::<u16>()) as "du16" => |value| {u16::to_le_bytes(value)});
generate_expr!(DataU32(u32, size_of::<u32>()) as "du32" => |value| {u32::to_le_bytes(value)});

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
			
			fn apply(&$self, $ctx: &mut Context, $args: Positioned<&[Argument]>) -> Result<(), ErrorLevel>
			{
				let Some($active) = $ctx.active_mut()
				else
				{
					let source = Box::new(DataError::Inactive);
					$ctx.push_error($args.convert(DirectiveErrorKind::Apply{name: $self.get_name().to_owned(), source}));
					return Err(ErrorLevel::Fatal);
				};
				if $args.value.len() != 1
				{
					$ctx.push_error($args.convert_fn(|v| DirectiveErrorKind::ArgumentCount{min: Some(1), max: Some(1), have: v.len()}));
					return Err(ErrorLevel::Trivial);
				}
				let $value = generate!(@impl($self) $($argt)+ = $args[0], $ctx, $active);
				if let Err(e) = {$($write)+}
				{
					let source = Box::new(DataError::Write(e));
					$ctx.push_error($args.convert(DirectiveErrorKind::Apply{name: $self.get_name().to_owned(), source}));
					return Err(ErrorLevel::Fatal);
				}
				Ok(())
			}
		}
	};
	(@impl($self:ident) Constant($type:ty) = $args:ident[$idx:expr], $ctx:ident, $active:ident) =>
	{
		match $args.value[$idx]
		{
			Argument::Constant(ref num) =>
			{
				let &Number::Integer(val) = num;
				match <$type>::try_from(val)
				{
					Ok(val) => val,
					_ =>
					{
						let source = Box::new(DataError::Range{min: i64::from(<$type>::MIN), max: i64::from(<$type>::MAX), have: val});
						$ctx.push_error($args.convert(DirectiveErrorKind::Apply{name: $self.get_name().to_owned(), source}));
						return Err(ErrorLevel::Fatal);
					},
				}
			},
			ref arg =>
			{
				let err = DirectiveErrorKind::Argument{idx: 0, expect: ArgumentType::Constant, have: arg.get_type()};
				$ctx.push_error($args.convert(err));
				return Err(ErrorLevel::Trivial);
			},
		}
	};
	(@impl($self:ident) String = $args:ident[$idx:expr], $ctx:ident, $active:ident) =>
	{
		match $args.value[$idx]
		{
			Argument::String(ref string) => string.as_ref(),
			ref arg =>
			{
				let err = DirectiveErrorKind::Argument{idx: $idx, expect: ArgumentType::String, have: arg.get_type()};
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
					ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
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
			ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
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
		let mut path_buff = ctx.curr_path().unwrap().to_path_buf();
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
									ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
									return Err(ErrorLevel::Fatal);
								}
								len
							},
							Err(..) =>
							{
								let source = Box::new(DataError::Write(SegmentError::Overflow{need: usize::MAX, have: active.remaining()}));
								ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
								return Err(ErrorLevel::Fatal);
							},
						}
					},
					Err(e) =>
					{
						let source = Box::new(DataError::File(e));
						ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
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
									ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
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
									ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
									return Err(ErrorLevel::Fatal);
								}
								pos += cnt;
							}
						},
						Err(e) =>
						{
							let source = Box::new(DataError::File(e));
							ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
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
				ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
				return Err(ErrorLevel::Fatal);
			},
		}
	}
}

#[derive(Debug)]
pub enum DataError
{
	Inactive,
	NotFound{name: String, realm: Realm},
	Range{min: i64, max: i64, have: i64},
	HexChar{pos: usize, value: char},
	HexEof,
	File(io::Error),
	Write(SegmentError),
	Put(PutError),
}

impl fmt::Display for DataError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Inactive => f.write_str("no active segment to write to"),
			&Self::NotFound{ref name, realm} => write!(f, "no such {realm} constant {name:?}"),
			&Self::Range{min, max, have} => write!(f, "constant out of range ({min} to {max}, got {have})"),
			&Self::HexChar{pos, value} => write!(f, "invalid hex char {value:?} at {pos}"),
			Self::HexEof => f.write_str("unexpected eof in hex string"),
			Self::File(..) => f.write_str("could not access referenced file"),
			Self::Write(..) => f.write_str("could not write segment"),
			Self::Put(..) => f.write_str("could not write data bytes"),
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
