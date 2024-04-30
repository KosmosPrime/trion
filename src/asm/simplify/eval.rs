use core::fmt;
use core::ops::BitOr;
use std::error::Error;

use crate::asm::arcob::Arcob;
use crate::asm::Context;
use crate::asm::simplify::{OverflowError, SimplifyError, simplify_raw};
use crate::asm::constant::{Lookup, Realm};
use crate::text::parse::{Argument, ArgumentType};
use crate::text::token::Number;

pub fn evaluate<'l>(arg: &mut Argument<'l>, ctx: &Context<'_>) -> Result<Evaluation<'l>, EvalError>
{
	match arg
	{
		Argument::Constant(..) => Ok(Evaluation::Complete{changed: false}),
		Argument::Identifier(name) =>
		{
			if !ctx.get_instruction_set().is_register(name)
			{
				let realm = if ctx.has_curr_file() {Realm::Local} else {Realm::Global};
				return match ctx.get_constant(name, realm)
				{
					Lookup::NotFound => return Err(EvalError::NoSuchVariable{name: name.as_ref().to_owned(), realm: Realm::Local}),
					Lookup::Deferred => return Ok(Evaluation::Deferred{changed: false, cause: name.clone()}),
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
		Argument::Sequence(args) | Argument::Function{args, ..} =>
		{
			args.iter_mut().try_fold(Evaluation::Complete{changed: false}, |c, a|
			{
				match evaluate(a, ctx)
				{
					Ok(r) => Ok(c | r),
					Err(e) => Err(e),
				}
			})
		},
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Evaluation<'c>
{
	Complete{changed: bool},
	Deferred{changed: bool, cause: Arcob<'c, str>},
}

impl<'c> Evaluation<'c>
{
	pub fn is_changed(&self) -> bool
	{
		match *self
		{
			Self::Complete{changed} => changed,
			Self::Deferred{changed, ..} => changed,
		}
	}
}

impl<'c> BitOr for Evaluation<'c>
{
	type Output = Self;
	
	fn bitor(self, rhs: Self) -> Self::Output
	{
		let (ch_lhs, dc_lhs) = match self
		{
			Self::Complete{changed} => (changed, None),
			Self::Deferred{changed, cause} => (changed, Some(cause)),
		};
		let (ch_rhs, dc_rhs) = match rhs
		{
			Self::Complete{changed} => (changed, None),
			Self::Deferred{changed, cause} => (changed, Some(cause)),
		};
		match dc_lhs.or(dc_rhs)
		{
			None => Self::Complete{changed: ch_lhs | ch_rhs},
			Some(cause) => Self::Deferred{changed: ch_lhs | ch_rhs, cause},
		}
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
