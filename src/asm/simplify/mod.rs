use core::fmt;
use std::error::Error;

use crate::text::parse::{Argument, ArgumentType};
use crate::text::token::Number;

mod eval;
pub use eval::*;
#[cfg(test)]
mod test;

#[derive(Clone, Debug)]
pub enum OverflowError
{
	Add{value: Number, arg: Number},
	Negate, // only `i64::MIN` overflows negation
	Subtract{value: Number, arg: Number},
	Multiply{value: Number, arg: Number},
	DivideByZero(Number),
	Divide{value: Number, divisor: Number},
	ModuloByZero(Number),
	Modulo{value: Number, divisor: Number},
	LeftShift{value: Number, shift: Number},
	RightShift{value: Number, shift: Number},
}

impl fmt::Display for OverflowError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match *self
		{
			Self::Add{value: Number::Integer(value), arg: Number::Integer(arg)} => write!(f, "overflow in {value} plus {arg}"),
			Self::Negate => write!(f, "overflow in negative {}", i64::MIN),
			Self::Subtract{value: Number::Integer(value), arg: Number::Integer(arg)} => write!(f, "overflow in {value} minus {arg}"),
			Self::Multiply{value: Number::Integer(value), arg: Number::Integer(arg)} => write!(f, "overflow in {value} times {arg}"),
			Self::DivideByZero(Number::Integer(value)) => write!(f, "cannot divide {value} by zero"),
			Self::Divide{value: Number::Integer(value), divisor: Number::Integer(divisor)} => write!(f, "overflow in {value} divided by {divisor}"),
			Self::ModuloByZero(Number::Integer(value)) => write!(f, "cannot modulo {value} by zero"),
			Self::Modulo{value: Number::Integer(value), divisor: Number::Integer(divisor)} => write!(f, "overflow in {value} modulo {divisor}"),
			Self::LeftShift{value: Number::Integer(value), shift: Number::Integer(shift)} => write!(f, "overflow in {value} left shifted by {shift}"),
			Self::RightShift{value: Number::Integer(value), shift: Number::Integer(shift)} => write!(f, "overflow in {value} right shifted by {shift}"),
		}
	}
}

impl Error for OverflowError {}

fn simplify_raw<'l>(arg: &mut Argument<'l>) -> Result<(), SimplifyError>
{
	let arg_ty = arg.get_type();
	match arg
	{
		Argument::Add(values) | Argument::Subtract(values) | Argument::Multiply(values) =>
		{
			if let Some(fail) = values.iter().find(|&v| matches!(v, Argument::String(..) | Argument::Address(..) | Argument::Sequence(..)))
			{
				return Err(SimplifyError::BadType{kind: fail.get_type(), op: arg_ty});
			}
			if let Some(start) = values.iter().position(|v| matches!(v, Argument::Constant(..)))
			{
				let (neutral, op): (i64, fn(i64, i64) -> Option<i64>) = match arg_ty
				{
					ArgumentType::Add => (0, i64::checked_add),
					ArgumentType::Subtract => (0, if start == 0 {i64::checked_sub} else {i64::checked_add}),
					ArgumentType::Multiply => (1, i64::checked_mul),
					_ => unreachable!(),
				};
				let Argument::Constant(Number::Integer(mut result)) = values[start] else {unreachable!()};
				let mut changed = false;
				for curr in values[start + 1..].iter()
				{
					if let Argument::Constant(Number::Integer(v)) = curr
					{
						match op(result, *v)
						{
							None =>
							{
								return Err(SimplifyError::Overflow(match arg_ty
								{
									ArgumentType::Add => OverflowError::Add{value: Number::Integer(result), arg: Number::Integer(*v)},
									ArgumentType::Subtract => OverflowError::Subtract{value: Number::Integer(result), arg: Number::Integer(*v)},
									ArgumentType::Multiply => OverflowError::Multiply{value: Number::Integer(result), arg: Number::Integer(*v)},
									_ => unreachable!(),
								}));
							},
							Some(r) => result = r,
						}
						changed = true;
					}
				}
				if changed
				{
					values[start] = Argument::Constant(Number::Integer(result));
					for curr in values[start + 1..].iter_mut()
					{
						if let Argument::Constant(Number::Integer(v)) = curr
						{
							*v = neutral;
						}
					}
					values.retain(|v| !matches!(v, &Argument::Constant(Number::Integer(v)) if v == neutral));
					match values.len()
					{
						0 => *arg = Argument::Constant(Number::Integer(neutral)),
						1 => *arg = values.swap_remove(0),
						_ => (),
					}
				}
			}
			Ok(())
		},
		Argument::Negate(value) =>
		{
			match value.as_ref()
			{
				&Argument::Constant(Number::Integer(value)) =>
				{
					if value == i64::MIN
					{
						return Err(SimplifyError::Overflow(OverflowError::Negate));
					}
					*arg = Argument::Constant(Number::Integer(-value))
				},
				Argument::String(..) | Argument::Address(..) | Argument::Sequence(..) =>
				{
					return Err(SimplifyError::BadType{kind: value.get_type(), op: arg_ty});
				},
				_ => (),
			}
			Ok(())
		},
		Argument::Divide{value, divisor} | Argument::Modulo{value, divisor} =>
		{
			if let (&Argument::Constant(Number::Integer(value)), &Argument::Constant(Number::Integer(divisor))) = (value.as_ref(), divisor.as_ref())
			{
				if divisor == 0
				{
					return Err(SimplifyError::Overflow(match arg_ty
					{
						ArgumentType::Divide => OverflowError::DivideByZero(Number::Integer(value)),
						ArgumentType::Modulo => OverflowError::ModuloByZero(Number::Integer(value)),
						_ => unreachable!(),
					}));
				}
				let op: fn(i64, i64) -> Option<i64> = match arg_ty
				{
					ArgumentType::Divide => i64::checked_div,
					ArgumentType::Modulo => i64::checked_rem,
					_ => unreachable!(),
				};
				let Some(result) = op(value, divisor)
				else
				{
					return Err(SimplifyError::Overflow(match arg_ty
					{
						ArgumentType::Divide => OverflowError::Divide{value: Number::Integer(value), divisor: Number::Integer(divisor)},
						ArgumentType::Modulo => OverflowError::Modulo{value: Number::Integer(value), divisor: Number::Integer(divisor)},
						_ => unreachable!(),
					}));
				};
				*arg = Argument::Constant(Number::Integer(result));
			}
			Ok(())
		},
		Argument::Not(value) =>
		{
			match value.as_ref()
			{
				&Argument::Constant(Number::Integer(value)) => *arg = Argument::Constant(Number::Integer(!value)),
				Argument::String(..) | Argument::Address(..) | Argument::Sequence(..) =>
				{
					return Err(SimplifyError::BadType{kind: value.get_type(), op: arg_ty});
				},
				_ => (),
			}
			Ok(())
		},
		Argument::BitAnd(values) | Argument::BitOr(values) | Argument::BitXor(values) =>
		{
			if let Some(fail) = values.iter().find(|&v| matches!(v, Argument::String(..) | Argument::Address(..) | Argument::Sequence(..)))
			{
				return Err(SimplifyError::BadType{kind: fail.get_type(), op: arg_ty});
			}
			if let Some(start) = values.iter().position(|v| matches!(v, Argument::Constant(..)))
			{
				let (neutral, op): (i64, fn(i64, i64) -> i64) = match arg_ty
				{
					ArgumentType::BitAnd => (-1, |l, r| l & r),
					ArgumentType::BitOr => (0, |l, r| l | r),
					ArgumentType::BitXor => (0, |l, r| l ^ r),
					_ => unreachable!(),
				};
				let Argument::Constant(Number::Integer(mut result)) = values[start] else {unreachable!()};
				let mut changed = false;
				for curr in values[start + 1..].iter_mut()
				{
					if let Argument::Constant(Number::Integer(v)) = curr
					{
						result = op(result, *v);
						changed = true;
						*v = neutral;
					}
				}
				if changed
				{
					values[start] = Argument::Constant(Number::Integer(result));
					values.retain(|v| !matches!(v, &Argument::Constant(Number::Integer(v)) if v == neutral));
					match values.len()
					{
						0 => *arg = Argument::Constant(Number::Integer(neutral)),
						1 => *arg = values.swap_remove(0),
						_ => (),
					}
				}
			}
			Ok(())
		},
		Argument::LeftShift{value, shift} | Argument::RightShift{value, shift} =>
		{
			if matches!(value.as_ref(), Argument::String(..) | Argument::Address(..) | Argument::Sequence(..))
			{
				return Err(SimplifyError::BadType{kind: value.get_type(), op: arg_ty});
			}
			if matches!(shift.as_ref(), Argument::String(..) | Argument::Address(..) | Argument::Sequence(..))
			{
				return Err(SimplifyError::BadType{kind: shift.get_type(), op: arg_ty});
			}
			if let (&Argument::Constant(Number::Integer(value)), &Argument::Constant(Number::Integer(shift))) = (value.as_ref(), shift.as_ref())
			{
				match u32::try_from(shift).ok().filter(|&v| v < i64::BITS)
				{
					None =>
					{
						return Err(SimplifyError::Overflow(match arg_ty
						{
							ArgumentType::LeftShift => OverflowError::LeftShift{value: Number::Integer(value), shift: Number::Integer(shift)},
							ArgumentType::RightShift => OverflowError::RightShift{value: Number::Integer(value), shift: Number::Integer(shift)},
							_ => unreachable!(),
						}));
					},
					Some(shift) =>
					{
						let result = match arg_ty
						{
							ArgumentType::LeftShift => value << shift,
							ArgumentType::RightShift => value >> shift,
							_ => unreachable!(),
						};
						*arg = Argument::Constant(Number::Integer(result));
					},
				}
			}
			Ok(())
		},
		Argument::Address(addr) =>
		{
			if matches!(addr.as_ref(), Argument::String(..) | Argument::Address(..) | Argument::Sequence(..))
			{
				return Err(SimplifyError::BadType{kind: addr.get_type(), op: arg_ty});
			}
			Ok(())
		}
		_ => Ok(()),
	}
}

pub fn simplify<'l>(arg: &mut Argument<'l>) -> Result<(), SimplifyError>
{
	match arg
	{
		Argument::Add(values) | Argument::Subtract(values) | Argument::Multiply(values) => values.iter_mut().try_for_each(simplify)?,
		Argument::Negate(value) | Argument::Not(value) => simplify(value)?,
		Argument::Divide{value, divisor} | Argument::Modulo{value, divisor} =>
		{
			simplify(value)?;
			simplify(divisor)?;
		},
		Argument::BitAnd(values) | Argument::BitOr(values) | Argument::BitXor(values) => values.iter_mut().try_for_each(simplify)?,
		Argument::LeftShift{value, shift} | Argument::RightShift{value, shift} =>
		{
			simplify(value)?;
			simplify(shift)?;
		},
		Argument::Address(addr) => simplify(addr)?,
		Argument::Sequence(args) | Argument::Function{args, ..} => return args.iter_mut().try_for_each(simplify),
		_ => (),
	}
	simplify_raw(arg)
}

#[derive(Clone, Debug)]
pub enum SimplifyError
{
	BadType{kind: ArgumentType, op: ArgumentType},
	Overflow(OverflowError),
}

impl fmt::Display for SimplifyError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::BadType{kind, op} => write!(f, "{op} not supported for {kind}"),
			Self::Overflow(..) => f.write_str("arithmetic overflow"),
		}
	}
}

impl Error for SimplifyError
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
