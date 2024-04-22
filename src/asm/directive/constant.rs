use core::fmt;
use std::error::Error;

use crate::asm::{ConstantError, Context, ErrorLevel};
use crate::asm::constant::Realm;
use crate::asm::directive::{Directive, DirectiveErrorKind};
use crate::asm::simplify::{evaluate, Evaluation};
use crate::text::Positioned;
use crate::text::parse::{Argument, ArgumentType};
use crate::text::token::Number;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Const;

impl Directive for Const
{
	fn get_name(&self) -> &str
	{
		"const"
	}
	
	fn apply(&self, ctx: & mut Context, mut args: Positioned<Vec<Argument>>) -> Result<(), ErrorLevel>
	{
		if args.value.len() != 2
		{
			ctx.push_error(args.convert_fn(|v| DirectiveErrorKind::ArgumentCount{min: Some(2), max: Some(2), have: v.len()}));
			return Err(ErrorLevel::Trivial);
		}
		match args.value[0].get_type()
		{
			ArgumentType::Identifier => (),
			have =>
			{
				ctx.push_error(args.convert(DirectiveErrorKind::Argument{idx: 0, expect: ArgumentType::Identifier, have}));
				return Err(ErrorLevel::Trivial);
			},
		}
		match evaluate(&mut args.value[1], ctx)
		{
			Ok(Evaluation::Complete{..}) => (),
			Ok(Evaluation::Deferred{..}) =>
			{
				let source = Box::new(ConstantError::NotFound{name: "<unknown>".to_owned(), realm: Realm::Local});
				ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
				return Err(ErrorLevel::Fatal);
			},
			Err(e) =>
			{
				ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source: Box::new(e)}));
				return Err(ErrorLevel::Fatal);
			},
		}
		let Argument::Identifier(ref name) = args.value[0] else {unreachable!()};
		let value = match args.value[1]
		{
			Argument::Constant(Number::Integer(v)) => v,
			ref arg =>
			{
				ctx.push_error(args.convert(DirectiveErrorKind::Argument{idx: 1, expect: ArgumentType::Constant, have: arg.get_type()}));
				return Err(ErrorLevel::Trivial);
			},
		};
		match ctx.insert_constant(name, value, Realm::Local)
		{
			Ok(..) => (),
			Err(ConstantError::Duplicate{name, ..}) =>
			{
				let source = Box::new(ConstError::Duplicate(name));
				ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
				return Err(ErrorLevel::Fatal);
			},
			Err(e) => unreachable!("{e:?}"),
		}
		Ok(())
	}
}

#[derive(Debug)]
pub enum ConstError
{
	Duplicate(String),
}

impl fmt::Display for ConstError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Duplicate(name) => write!(f, "duplicate constant {name}"),
		}
	}
}

impl Error for ConstError {}
