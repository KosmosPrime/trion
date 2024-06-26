use core::fmt;
use std::error::Error;

use crate::asm::{ConstantError, Context, ErrorLevel, SegmentError};
use crate::asm::constant::Realm;
use crate::asm::directive::{Directive, DirectiveErrorKind};
use crate::asm::simplify::{evaluate, Evaluation};
use crate::text::Positioned;
use crate::text::parse::{Argument, ArgumentType};
use crate::text::parse::choice::ArgChoice;
use crate::text::token::Number;

#[derive(Clone, Copy, Debug)]
pub struct Align;

impl Directive for Align
{
	fn get_name(&self) -> &str
	{
		"align"
	}
	
	fn apply(&self, ctx: & mut Context, mut args: Positioned<Vec<Argument>>) -> Result<(), ErrorLevel>
	{
		if ctx.active().is_none()
		{
			let source = Box::new(AlignError::Inactive);
			ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
			return Err(ErrorLevel::Fatal);
		}
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
				let source = Box::new(ConstantError::NotFound{name: cause.as_ref().to_owned(), realm: Realm::Local});
				ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
				return Err(ErrorLevel::Fatal);
			},
			Err(e) =>
			{
				ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source: Box::new(e)}));
				return Err(ErrorLevel::Fatal);
			},
		}
		let active = ctx.active_mut().unwrap();
		let len = match args.value[0]
		{
			Argument::Constant(ref num) =>
			{
				let &Number::Integer(val) = num;
				match u32::try_from(val)
				{
					Ok(val) if val > 0 => val,
					_ =>
					{
						let source = Box::new(AlignError::Range(val));
						ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
						return Err(ErrorLevel::Fatal);
					},
				}
			},
			ref arg =>
			{
			let dir = self.get_name().to_owned();
				ctx.push_error(args.convert(DirectiveErrorKind::ArgumentType
				{
					dir,
					idx: 0,
					expect: ArgChoice::of(ArgumentType::Constant),
					have: arg.get_type(),
				}));
				return Err(ErrorLevel::Trivial);
			},
		};
		let off = active.curr_addr() % len;
		if off != 0
		{
			match usize::try_from(len - off)
			{
				Ok(new_len) =>
				{
					const PADDING: [u8; 256] = [0xBE; 256];
					if !active.has_remaining(new_len)
					{
						let source = Box::new(AlignError::Write(SegmentError::Overflow{need: new_len, have: active.remaining()}));
						ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
						return Err(ErrorLevel::Fatal);
					}
					for p in (0..new_len).step_by(256)
					{
						let result = if new_len - p >= 256 {active.write(&PADDING)}
						else {active.write(&PADDING[..new_len - p])};
						if let Err(e) = result
						{
							let source = Box::new(AlignError::Write(e));
							ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
							return Err(ErrorLevel::Fatal);
						}
					}
				},
				Err(..) =>
				{
					let source = Box::new(AlignError::Overflow{need: len - off, have: active.remaining()});
					ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
					return Err(ErrorLevel::Fatal);
				},
			}
		}
		Ok(())
	}
}

#[derive(Debug)]
pub enum AlignError
{
	Inactive,
	Range(i64),
	Overflow{need: u32, have: usize},
	Write(SegmentError),
}

impl fmt::Display for AlignError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Inactive => f.write_str("no active segment to align"),
			Self::Range(align) => write!(f, "alignment out of range ({align})"),
			Self::Overflow{need, have} => write!(f, "alignment too long (need {need}, max {have})"),
			Self::Write(..) => f.write_str("could not write alignment bytes"),
		}
	}
}

impl Error for AlignError
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
