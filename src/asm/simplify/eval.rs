use core::fmt;
use std::error::Error;

use crate::asm::Context;
use crate::asm::simplify::{OverflowError, SimplifyError, simplify_raw};
use crate::asm::constant::{Lookup, Realm};
use crate::text::parse::{Argument, ArgumentType};
use crate::text::token::Number;

pub fn evaluate<'l>(arg: &mut Argument<'l>, ctx: &Context<'_>) -> Result<Evaluation, EvalError>
{
	match arg
	{
		Argument::Constant(..) => Ok(Evaluation::Complete),
		Argument::Identifier(name) =>
		{
			if !ctx.get_instruction_set().is_register(name)
			{
				let realm = if ctx.curr_path().is_none() {Realm::Global} else {Realm::Local};
				return match ctx.get_constant(name, realm)
				{
					Lookup::NotFound => Err(EvalError::NoSuchVariable{name: name.to_owned(), realm: Realm::Local}),
					Lookup::Deferred => Ok(Evaluation::Deferred),
					Lookup::Found(val) =>
					{
						*arg = Argument::Constant(Number::Integer(val));
						Ok(Evaluation::Complete)
					},
				};
			}
			else {Ok(Evaluation::Complete)}
		},
		Argument::String(..) => Ok(Evaluation::Complete),
		_ =>
		{
			let eval = match arg
			{
				Argument::Add(args) | Argument::Subtract(args) | Argument::Multiply(args) | Argument::BitAnd(args) | Argument::BitOr(args) 
					| Argument::BitXor(args) | Argument::Sequence(args) | Argument::Function{args, ..} =>
				{
					args.iter_mut().try_fold(Evaluation::Complete, |e, v|
					{
						match evaluate(v, ctx)
						{
							Ok(r) => Ok(if e == Evaluation::Complete {r} else {e}),
							Err(e) => Err(e),
						}
					})?
				},
				Argument::Negate(inner) | Argument::Not(inner) | Argument::Address(inner) => evaluate(inner, ctx)?,
				Argument::Divide{value, divisor} | Argument::Modulo{value, divisor} =>
				{
					let r0 = evaluate(value, ctx)?;
					let r1 = evaluate(divisor, ctx)?;
					if r0 == Evaluation::Complete {r1} else {r0}
				},
				Argument::LeftShift{value, shift} | Argument::RightShift{value, shift} =>
				{
					let r0 = evaluate(value, ctx)?;
					let r1 = evaluate(shift, ctx)?;
					if r0 == Evaluation::Complete {r1} else {r0}
				},
				_ => unreachable!(),
			};
			simplify_raw(arg)?;
			Ok(eval)
		},
	}
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Evaluation
{
	Complete, Deferred,
}

#[derive(Clone, Debug)]
pub enum EvalError
{
	NoSuchVariable{name: String, realm: Realm},
	BadType{kind: ArgumentType, op: ArgumentType},
	Overflow(OverflowError),
}

impl From<SimplifyError> for EvalError
{
	fn from(value: SimplifyError) -> Self
	{
		match value
		{
			SimplifyError::BadType{kind, op} => Self::BadType{kind, op},
			SimplifyError::Overflow(e) => Self::Overflow(e),
		}
	}
}

impl fmt::Display for EvalError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::NoSuchVariable{ref name, realm} => write!(f, "no such {realm} constant {name:?}"),
			Self::BadType{kind, op} => write!(f, "{op} not supported for {kind}"),
			Self::Overflow(..) => f.write_str("arithmetic overflow"),
		}
	}
}

impl Error for EvalError
{
	fn source(&self) -> Option<&(dyn Error + 'static)>
	{
		match self
		{
			Self::Overflow(e) => Some(e),
			_ => None,
		}
	}
}
