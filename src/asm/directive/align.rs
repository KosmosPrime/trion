use core::fmt;
use std::error::Error;

use crate::asm::{Context, ErrorLevel, SegmentError};
use crate::asm::directive::{Directive, DirectiveErrorKind};
use crate::text::Positioned;
use crate::text::parse::{Argument, ArgumentType};
use crate::text::token::Number;

#[derive(Clone, Copy, Debug)]
pub struct Align;

impl Directive for Align
{
	fn get_name(&self) -> &str
	{
		"align"
	}
	
	fn apply<'c>(&self, ctx: &'c mut Context, args: Positioned<&[Argument]>) -> Result<(), ErrorLevel>
	{
		let Some(active) = ctx.active_mut()
		else
		{
			let source = Box::new(AlignError::Inactive);
			ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
			return Err(ErrorLevel::Fatal);
		};
		if args.value.len() != 1
		{
			ctx.push_error(args.convert_fn(|v| DirectiveErrorKind::ArgumentCount{min: Some(1), max: Some(1), have: v.len()}));
			return Err(ErrorLevel::Trivial);
		}
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
						ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
						return Err(ErrorLevel::Fatal);
					},
				}
			},
			ref arg =>
			{
				ctx.push_error(args.convert(DirectiveErrorKind::Argument{idx: 0, expect: ArgumentType::Constant, have: arg.get_type()}));
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
						ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
						return Err(ErrorLevel::Fatal);
					}
					for p in (0..new_len).step_by(256)
					{
						let result = if new_len - p >= 256 {active.write(&PADDING)}
						else {active.write(&PADDING[..new_len - p])};
						if let Err(e) = result
						{
							let source = Box::new(AlignError::Write(e));
							ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
							return Err(ErrorLevel::Fatal);
						}
					}
				},
				Err(..) =>
				{
					let source = Box::new(AlignError::Overflow{need: len - off, have: active.remaining()});
					ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
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
