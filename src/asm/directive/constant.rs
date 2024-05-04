use core::fmt;
use std::error::Error;

use crate::asm::{ConstantError, Context, ErrorLevel};
use crate::asm::constant::Realm;
use crate::asm::directive::{Directive, DirectiveErrorKind};
use crate::asm::simplify::{evaluate, Evaluation};
use crate::text::Positioned;
use crate::text::parse::{Argument, ArgumentType};
use crate::text::parse::choice::ArgChoice;
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
		const NUM_ARGS: usize = 2;
		if args.value.len() != NUM_ARGS
		{
			let dir = self.get_name().to_owned();
			ctx.push_error(args.convert(
				if args.value.len() < NUM_ARGS {DirectiveErrorKind::NotEnoughArguments{dir, need: NUM_ARGS, have: args.value.len()}}
				else {DirectiveErrorKind::TooManyArguments{dir, max: NUM_ARGS, have: args.value.len()}}
			));
			return Err(ErrorLevel::Trivial);
		}
		match args.value[0].get_type()
		{
			ArgumentType::Identifier => (),
			have =>
			{
				let dir = self.get_name().to_owned();
				ctx.push_error(args.convert(DirectiveErrorKind::ArgumentType{dir, idx: 0, expect: ArgChoice::of(ArgumentType::Identifier), have}));
				return Err(ErrorLevel::Trivial);
			},
		}
		match evaluate(&mut args.value[1], ctx)
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
		let Argument::Identifier(ref name) = args.value[0] else {unreachable!()};
		let value = match args.value[1]
		{
			Argument::Constant(Number::Integer(v)) => v,
			ref arg =>
			{
				let dir = self.get_name().to_owned();
				ctx.push_error(args.convert(DirectiveErrorKind::ArgumentType
				{
					dir,
					idx: 1,
					expect: ArgChoice::of(ArgumentType::Constant),
					have: arg.get_type(),
				}));
				return Err(ErrorLevel::Trivial);
			},
		};
		match ctx.insert_constant(name, value, Realm::Local)
		{
			Ok(..) => (),
			Err(ConstantError::Duplicate{name, ..}) =>
			{
				let source = Box::new(ConstError::Duplicate(name));
				ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
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
