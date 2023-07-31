use core::fmt;
use std::error::Error;

use crate::asm::{Context, SegmentError};
use crate::asm::directive::{Directive, DirectiveError, DirectiveErrorKind};
use crate::text::Positioned;
use crate::text::parse::{Argument, ArgumentType};
use crate::text::token::Number;

#[derive(Clone, Copy, Debug)]
pub struct Addr;

impl Directive for Addr
{
	fn get_name(&self) -> &str
	{
		"addr"
	}
	
	fn apply<'c>(&self, ctx: &'c mut Context, args: Positioned<&[Argument]>) -> Result<(), &'c DirectiveError>
	{
		if args.value.len() != 1
		{
			return Err(ctx.push_error(args.convert_fn(|v| DirectiveErrorKind::ArgumentCount{min: Some(1), max: Some(1), have: v.len()})))
		}
		let tgt = match args.value[0]
		{
			Argument::Constant(ref num) =>
			{
				let &Number::Integer(val) = num;
				let Ok(val) = u32::try_from(val) else
				{
					return Err(ctx.push_error(args.convert(DirectiveErrorKind::Apply(Box::new(AddrError::Range(val))))));
				};
				val
			},
			ref arg => return Err(ctx.push_error(args.convert(DirectiveErrorKind::Argument{idx: 0, expect: ArgumentType::Constant, have: arg.get_type()}))),
		};
		if let Err(e) = ctx.change_segment(tgt)
		{
			return Err(ctx.push_error(args.convert(DirectiveErrorKind::Apply(Box::new(AddrError::Segment(e))))));
		}
		Ok(())
	}
}

#[derive(Debug)]
pub enum AddrError
{
	Range(i64),
	Segment(SegmentError),
}

impl fmt::Display for AddrError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Range(addr) => write!(f, "address out of range ({addr:08X})"),
			Self::Segment(..) => f.write_str("could not change active section"),
		}
	}
}

impl Error for AddrError
{
	fn source(&self) -> Option<&(dyn Error + 'static)>
	{
		match self
		{
			Self::Segment(e) => Some(e),
			_ => None,
		}
	}
}
