use core::fmt;
use std::error::Error;
use std::fs::OpenOptions;
use std::io::{self, Read};

use crate::asm::{Context, WriteError};
use crate::asm::directive::{Directive, DirectiveError, DirectiveErrorKind};
use crate::text::Positioned;
use crate::text::parse::{Argument, ArgumentType};
use crate::text::token::Number;

macro_rules!generate
{
	($name:ident($($argt:tt)+) as $label:literal => |$ctx:ident, $active:ident, $args:ident, $value:ident| {$($write:tt)+}) =>
	{
		#[derive(Clone, Copy, Debug)]
		pub struct $name;
		
		impl Directive for $name
		{
			fn get_name(&self) -> &str
			{
				$label
			}
			
			fn apply<'c>(&self, $ctx: &'c mut Context, $args: Positioned<&[Argument]>) -> Result<(), &'c DirectiveError>
			{
				let Some($active) = $ctx.active_mut()
				else
				{
					return Err($ctx.push_error($args.convert(DirectiveErrorKind::Apply(Box::new(DataError::Inactive)))));
				};
				if $args.value.len() != 1
				{
					return Err($ctx.push_error($args.convert_fn(|v| DirectiveErrorKind::ArgumentCount{min: Some(1), max: Some(1), have: v.len()})))
				}
				let $value = generate!(@impl $($argt)+ = $args[0], $ctx, $active);
				if let Err(e) = {$($write)+}
				{
					return Err($ctx.push_error($args.convert(DirectiveErrorKind::Apply(Box::new(DataError::Write(e))))));
				}
				Ok(())
			}
		}
	};
	(@impl Constant($type:ty) = $args:ident[$idx:expr], $ctx:ident, $active:ident) =>
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
						let err = Box::new(DataError::Range{min: i64::from(<$type>::MIN), max: i64::from(<$type>::MAX), have: val});
						return Err($ctx.push_error($args.convert(DirectiveErrorKind::Apply(err))));
					},
				}
			},
			ref arg =>
			{
				let err = DirectiveErrorKind::Argument{idx: 0, expect: ArgumentType::Constant, have: arg.get_type()};
				return Err($ctx.push_error($args.convert(err)));
			},
		}
	};
	(@impl String = $args:ident[$idx:expr], $ctx:ident, $active:ident) =>
	{
		match $args.value[$idx]
		{
			Argument::String(ref string) => string.as_ref(),
			ref arg =>
			{
				let err = DirectiveErrorKind::Argument{idx: $idx, expect: ArgumentType::String, have: arg.get_type()};
				return Err($ctx.push_error($args.convert(err)));
			},
		}
	};
}

generate!(DataU8(Constant(u8)) as "du8" => |ctx, active, args, value| {active.write(core::slice::from_ref(&(value as u8)))});
generate!(DataU16(Constant(u16)) as "du16" => |ctx, active, args, value| {active.write(&u16::to_le_bytes(value))});
generate!(DataU32(Constant(u32)) as "du32" => |ctx, active, args, value| {active.write(&u32::to_le_bytes(value))});
generate!
{
	DataHex(String) as "dhex" => |ctx, active, args, in_hex|
	{
		let mut out_data = Vec::new();
		let mut carry: Option<u8> = None;
		for (pos, c) in in_hex.chars().enumerate().filter(|(_, c)| !c.is_ascii_whitespace())
		{
			match c.to_digit(16)
			{
				None =>
				{
					let err = Box::new(DataError::HexChar{pos, value: c});
					return Err(ctx.push_error(args.convert(DirectiveErrorKind::Apply(err))));
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
			return Err(ctx.push_error(args.convert(DirectiveErrorKind::Apply(Box::new(DataError::HexEof)))));
		}
		active.write(out_data.as_slice())
	}
}
generate!(DataStr(String) as "dstr" => |ctx, active, args, value| {active.write(value.as_bytes())});
generate!
{
	DataFile(String) as "dfile" => |ctx, _active, args, path|
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
									let err = Box::new(DataError::Write(WriteError::Overflow{need: len, have: active.remaining()}));
									return Err(ctx.push_error(args.convert(DirectiveErrorKind::Apply(err))));
								}
								len
							},
							Err(..) =>
							{
								let err = Box::new(DataError::Write(WriteError::Overflow{need: usize::MAX, have: active.remaining()}));
								return Err(ctx.push_error(args.convert(DirectiveErrorKind::Apply(err))));
							},
						}
					},
					Err(e) => return Err(ctx.push_error(args.convert(DirectiveErrorKind::Apply(Box::new(DataError::File(e)))))),
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
									return Err(ctx.push_error(args.convert(DirectiveErrorKind::Apply(Box::new(DataError::Write(e))))));
								}
								// no need to update pos again
								//pos = len;
								break;
							}
							else
							{
								if let Err(e) = active.write(&temp[..cnt])
								{
									return Err(ctx.push_error(args.convert(DirectiveErrorKind::Apply(Box::new(DataError::Write(e))))));
								}
								pos += cnt;
							}
						},
						Err(e) => return Err(ctx.push_error(args.convert(DirectiveErrorKind::Apply(Box::new(DataError::File(e)))))),
					}
				}
				// reading less than expected is fine
				drop(f);
				Ok(())
			},
			Err(e) => return Err(ctx.push_error(args.convert(DirectiveErrorKind::Apply(Box::new(DataError::File(e)))))),
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
	Write(WriteError),
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
