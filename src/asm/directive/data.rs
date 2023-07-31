use core::fmt;
use std::error::Error;

use crate::asm::{Context, WriteError};
use crate::asm::directive::{Directive, DirectiveError, DirectiveErrorKind};
use crate::text::Positioned;
use crate::text::parse::{Argument, ArgumentType};
use crate::text::token::Number;

macro_rules!generate
{
	($name:ident($type:ty) as $label:literal => |$ctx:ident, $active:ident, $value:ident| {$write:expr}) =>
	{
		#[derive(Clone, Copy, Debug)]
		pub struct $name;
		
		impl Directive for $name
		{
			fn get_name(&self) -> &str
			{
				$label
			}
			
			fn apply<'c>(&self, $ctx: &'c mut Context, args: Positioned<&[Argument]>) -> Result<(), &'c DirectiveError>
			{
				let Some($active) = $ctx.active_mut()
				else
				{
					return Err($ctx.push_error(args.convert(DirectiveErrorKind::Apply(Box::new(DataError::Inactive)))));
				};
				if args.value.len() != 1
				{
					return Err($ctx.push_error(args.convert_fn(|v| DirectiveErrorKind::ArgumentCount{min: Some(1), max: Some(1), have: v.len()})))
				}
				let $value = match args.value[0]
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
								return Err($ctx.push_error(args.convert(DirectiveErrorKind::Apply(err))));
							},
						}
					},
					ref arg =>
					{
						let err = DirectiveErrorKind::Argument{idx: 0, expect: ArgumentType::Constant, have: arg.get_type()};
						return Err($ctx.push_error(args.convert(err)));
					},
				};
				if let Err(e) = $write
				{
					return Err($ctx.push_error(args.convert(DirectiveErrorKind::Apply(Box::new(DataError::Write(e))))));
				}
				Ok(())
			}
		}
	}
}

generate!(DataU8(u8) as "du8" => |ctx, active, value| {active.write(core::slice::from_ref(&(value as u8)))});
generate!(DataU16(u16) as "du16" => |ctx, active, value| {active.write(&u16::to_le_bytes(value))});
generate!(DataU32(u32) as "du32" => |ctx, active, value| {active.write(&u32::to_le_bytes(value))});

#[derive(Debug)]
pub enum DataError
{
	Inactive,
	Range{min: i64, max: i64, have: i64},
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
			Self::Write(e) => Some(e),
			_ => None,
		}
	}
}
