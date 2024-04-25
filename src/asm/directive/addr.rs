use core::fmt;
use std::error::Error;

use crate::asm::{ConstantError, Context, ErrorLevel, SegmentError};
use crate::asm::constant::Realm;
use crate::asm::directive::{Directive, DirectiveErrorKind};
use crate::asm::simplify::{evaluate, Evaluation};
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
	
	fn apply(&self, ctx: &mut Context, mut args: Positioned<Vec<Argument>>) -> Result<(), ErrorLevel>
	{
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
		match evaluate(&mut args.value[0], ctx)
		{
			Ok(Evaluation::Complete{..}) => (),
			Ok(Evaluation::Deferred{cause, ..}) =>
			{
				let source = Box::new(ConstantError::NotFound{name: cause.into_owned(), realm: Realm::Local});
				ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
				return Err(ErrorLevel::Fatal);
			},
			Err(e) =>
			{
				ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source: Box::new(e)}));
				return Err(ErrorLevel::Fatal);
			},
		}
		let tgt = match args.value[0]
		{
			Argument::Constant(ref num) =>
			{
				let &Number::Integer(val) = num;
				let Ok(val) = u32::try_from(val) else
				{
					let source = Box::new(AddrError::Range(val));
					ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
					return Err(ErrorLevel::Fatal);
				};
				val
			},
			ref arg =>
			{
				let dir = self.get_name().to_owned();
				ctx.push_error(args.convert(DirectiveErrorKind::ArgumentType{dir, idx: 0, expect: ArgumentType::Constant, have: arg.get_type()}));
				return Err(ErrorLevel::Trivial);
			},
		};
		if let Err(e) = ctx.change_segment(tgt)
		{
			let source = Box::new(AddrError::Segment(e));
			ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
			return Err(ErrorLevel::Fatal);
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
