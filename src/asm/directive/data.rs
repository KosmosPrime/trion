use core::fmt;
use std::error::Error;
use std::fs::OpenOptions;
use std::io::{self, Read};

use crate::asm::{Context, ErrorLevel, SegmentError};
use crate::asm::directive::{Directive, DirectiveErrorKind};
use crate::text::Positioned;
use crate::text::parse::{Argument, ArgumentType};
use crate::text::token::Number;

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
			
			fn apply<'c>(&$self, $ctx: &'c mut Context, $args: Positioned<&[Argument]>) -> Result<(), ErrorLevel>
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

generate!(DataU8(Constant(u8)) as "du8" => |self, ctx, active, args, value| {active.write(core::slice::from_ref(&(value as u8)))});
generate!(DataU16(Constant(u16)) as "du16" => |self, ctx, active, args, value| {active.write(&u16::to_le_bytes(value))});
generate!(DataU32(Constant(u32)) as "du32" => |self, ctx, active, args, value| {active.write(&u32::to_le_bytes(value))});
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
			Self::Range{min, max, have} => write!(f, "constant out of range ({min} to {max}, got {have})"),
			Self::HexChar{pos, value} => write!(f, "invalid hex char {value:?} at {pos}"),
			Self::HexEof => f.write_str("unexpected eof in hex string"),
			Self::File(..) => f.write_str("could not access referenced file"),
			Self::Write(..) => f.write_str("could not write alignment bytes"),
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
