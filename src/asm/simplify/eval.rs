use core::fmt;
use core::ops::BitOr;
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
		Argument::Constant(..) => Ok(Evaluation::Complete{changed: false}),
		Argument::Identifier(name) =>
		{
			if !ctx.get_instruction_set().is_register(name)
			{
				let realm = if ctx.curr_path().is_none() {Realm::Global} else {Realm::Local};
				return match ctx.get_constant(name, realm)
				{
					Lookup::NotFound => Err(EvalError::NoSuchVariable{name: name.as_ref().to_owned(), realm: Realm::Local}),
					Lookup::Deferred => Ok(Evaluation::Deferred{changed: false}),
					Lookup::Found(val) =>
					{
						*arg = Argument::Constant(Number::Integer(val));
						Ok(Evaluation::Complete{changed: true})
					},
				};
			}
			else {Ok(Evaluation::Complete{changed: false})}
		},
		Argument::String(..) => Ok(Evaluation::Complete{changed: false}),
		_ =>
		{
			let eval = match arg
			{
				Argument::Add{lhs, rhs} | Argument::Subtract{lhs, rhs} | Argument::Multiply{lhs, rhs} | Argument::Divide{lhs, rhs}
					| Argument::Modulo{lhs, rhs} | Argument::BitAnd{lhs, rhs} | Argument::BitOr{lhs, rhs} | Argument::BitXor{lhs, rhs}
					| Argument::LeftShift{lhs, rhs} | Argument::RightShift{lhs, rhs} =>
				{
					evaluate(lhs, ctx)? | evaluate(rhs, ctx)?
				},
				Argument::Negate(inner) | Argument::Not(inner) | Argument::Address(inner) => evaluate(inner, ctx)?,
				_ => unreachable!(),
			};
			Ok(eval | Evaluation::Complete{changed: simplify_raw(arg)?})
		},
	}
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Evaluation
{
	Complete{changed: bool},
	Deferred{changed: bool},
}

impl Evaluation
{
	pub fn is_changed(&self) -> bool
	{
		match *self
		{
			Self::Complete{changed} => changed,
			Self::Deferred{changed} => changed,
		}
	}
}

impl BitOr for Evaluation
{
	type Output = Self;
	
	fn bitor(self, rhs: Self) -> Self::Output
	{
		let (ch_lhs, def_lhs) = match self
		{
			Self::Complete{changed} => (changed, false),
			Self::Deferred{changed} => (changed, true),
		};
		let (ch_rhs, def_rhs) = match rhs
		{
			Self::Complete{changed} => (changed, false),
			Self::Deferred{changed} => (changed, true),
		};
		if def_lhs || def_rhs {Self::Deferred{changed: ch_lhs | ch_rhs}} else {Self::Complete{changed: ch_lhs | ch_rhs}}
	}
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
