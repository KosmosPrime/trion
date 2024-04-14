use core::fmt;
use core::mem;
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
	Add{lhs: Number, rhs: Number},
	Negate, // only `i64::MIN` overflows negation
	Subtract{lhs: Number, rhs: Number},
	Multiply{lhs: Number, rhs: Number},
	DivideByZero(Number),
	Divide{lhs: Number, rhs: Number},
	ModuloByZero(Number),
	Modulo{lhs: Number, rhs: Number},
	LeftShift{lhs: Number, rhs: Number},
	RightShift{lhs: Number, rhs: Number},
}

impl fmt::Display for OverflowError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match *self
		{
			Self::Add{lhs: Number::Integer(lhs), rhs: Number::Integer(rhs)} => write!(f, "overflow in {lhs} plus {rhs}"),
			Self::Negate => write!(f, "overflow in negative {}", i64::MIN),
			Self::Subtract{lhs: Number::Integer(lhs), rhs: Number::Integer(rhs)} => write!(f, "overflow in {lhs} minus {rhs}"),
			Self::Multiply{lhs: Number::Integer(lhs), rhs: Number::Integer(rhs)} => write!(f, "overflow in {lhs} times {rhs}"),
			Self::DivideByZero(Number::Integer(lhs)) => write!(f, "cannot divide {lhs} by zero"),
			Self::Divide{lhs: Number::Integer(lhs), rhs: Number::Integer(rhs)} => write!(f, "overflow in {lhs} divided by {rhs}"),
			Self::ModuloByZero(Number::Integer(lhs)) => write!(f, "cannot modulo {lhs} by zero"),
			Self::Modulo{lhs: Number::Integer(lhs), rhs: Number::Integer(rhs)} => write!(f, "overflow in {lhs} modulo {rhs}"),
			Self::LeftShift{lhs: Number::Integer(lhs), rhs: Number::Integer(rhs)} => write!(f, "overflow in {lhs} left shifted by {rhs}"),
			Self::RightShift{lhs: Number::Integer(lhs), rhs: Number::Integer(rhs)} => write!(f, "overflow in {lhs} right shifted by {rhs}"),
		}
	}
}

impl Error for OverflowError {}

pub fn neutralize_raw<'l>(arg: &mut Argument<'l>) -> Result<(), SimplifyError>
{
	// optimize out adding or subtracting a negative by flipping the binary operator
	while matches!(arg, Argument::Add{rhs, ..} | Argument::Subtract{rhs, ..} if matches!(**rhs, Argument::Negate(..)))
	{
		match mem::replace(arg, Argument::Constant(Number::Integer(0)))
		{
			Argument::Add{lhs, rhs} =>
			{
				let Argument::Negate(neg) = *rhs else {unreachable!()};
				*arg = Argument::Subtract{lhs, rhs: neg};
			},
			Argument::Subtract{lhs, rhs} =>
			{
				let Argument::Negate(neg) = *rhs else {unreachable!()};
				*arg = Argument::Add{lhs, rhs: neg};
			},
			_ => unreachable!(),
		}
	}
	
	// final pass which normalizes addition or subtraction of a negative constant
	if let Argument::Add{rhs, ..} | Argument::Subtract{rhs, ..} = arg
	{
		if let Argument::Constant(Number::Integer(val)) = rhs.as_mut()
		{
			if *val < 0
			{
				match val.checked_neg()
				{
					None => return Err(SimplifyError::Overflow(OverflowError::Negate)),
					Some(neg_val) => *val = neg_val,
				}
				*arg = match mem::replace(arg, Argument::Constant(Number::Integer(0)))
				{
					Argument::Add{lhs, rhs} => Argument::Subtract{lhs, rhs},
					Argument::Subtract{lhs, rhs} => Argument::Add{lhs, rhs},
					_ => unreachable!(),
				};
			}
		}
	}
	
	// main neutralization operation
	let arg_ty = arg.get_type();
	match arg
	{
		Argument::Add{lhs, rhs} | Argument::Subtract{lhs, rhs} | Argument::Multiply{lhs, rhs} | Argument::Divide{lhs, rhs}
			| Argument::Modulo{lhs, rhs} | Argument::BitAnd{lhs, rhs} | Argument::BitOr{lhs, rhs} | Argument::BitXor{lhs, rhs}
			| Argument::LeftShift{lhs, rhs} | Argument::RightShift{lhs, rhs} =>
		{
			if matches!(lhs.as_ref(), Argument::String(..) | Argument::Address(..) | Argument::Sequence(..))
			{
				return Err(SimplifyError::BadType{kind: lhs.get_type(), op: arg_ty});
			}
			if matches!(rhs.as_ref(), Argument::String(..) | Argument::Address(..) | Argument::Sequence(..))
			{
				return Err(SimplifyError::BadType{kind: rhs.get_type(), op: arg_ty});
			}
			let (neutral, neut_rhs) = match arg_ty
			{
				ArgumentType::Add => (Some(0), Some(0)),
				// `lhs` being neutral still has the negation effect on `rhs`
				ArgumentType::Subtract => (None, Some(0)),
				ArgumentType::Multiply => (Some(1), Some(1)),
				// these lack lhs neutralization because division by zero would still fail
				ArgumentType::Divide | ArgumentType::Modulo => (None, Some(1)),
				ArgumentType::BitAnd => (Some(-1), Some(-1)),
				ArgumentType::BitOr => (Some(0), Some(0)),
				ArgumentType::BitXor => (Some(0), Some(0)),
				// these lack lhs neutralization because the shift could still fail
				ArgumentType::LeftShift | ArgumentType::RightShift => (None, Some(0)),
				_ => unreachable!(),
			};
			if let &Argument::Constant(Number::Integer(val)) = lhs.as_ref()
			{
				if Some(val) == neutral
				{
					*arg = mem::replace(rhs.as_mut(), Argument::Constant(Number::Integer(0)));
				}
				else if arg_ty == ArgumentType::Subtract && val == 0
				{
					let curr = mem::replace(arg, Argument::Constant(Number::Integer(0)));
					let Argument::Subtract{rhs, ..} = curr else {unreachable!()};
					*arg = Argument::Negate(rhs);
				}
			}
			else if let &Argument::Constant(Number::Integer(val)) = rhs.as_ref()
			{
				if Some(val) == neut_rhs
				{
					*arg = mem::replace(lhs.as_mut(), Argument::Constant(Number::Integer(0)));
				}
			}
		},
		_ => (),
	}
	Ok(())
}

pub fn neutralize<'l>(arg: &mut Argument<'l>) -> Result<(), SimplifyError>
{
	match arg
	{
		Argument::Add{lhs, rhs} | Argument::Subtract{lhs, rhs} | Argument::Multiply{lhs, rhs} | Argument::Divide{lhs, rhs}
			| Argument::Modulo{lhs, rhs} | Argument::BitAnd{lhs, rhs} | Argument::BitOr{lhs, rhs} | Argument::BitXor{lhs, rhs}
			| Argument::LeftShift{lhs, rhs} | Argument::RightShift{lhs, rhs} =>
		{
			neutralize(lhs.as_mut())?;
			neutralize(rhs.as_mut())?;
		},
		Argument::Negate(value) | Argument::Not(value) | Argument::Address(value) => neutralize(value)?,
		Argument::Sequence(args) | Argument::Function{args, ..} => return args.iter_mut().try_for_each(neutralize),
		_ => (),
	}
	neutralize_raw(arg)
}

fn simplify_raw<'l>(arg: &mut Argument<'l>) -> Result<(), SimplifyError>
{
	let arg_ty = arg.get_type();
	match arg
	{
		Argument::Add{lhs, rhs} | Argument::Subtract{lhs, rhs} | Argument::Multiply{lhs, rhs} | Argument::Divide{lhs, rhs}
			| Argument::Modulo{lhs, rhs} | Argument::BitAnd{lhs, rhs} | Argument::BitOr{lhs, rhs} | Argument::BitXor{lhs, rhs}
			| Argument::LeftShift{lhs, rhs} | Argument::RightShift{lhs, rhs} =>
		{
			if matches!(lhs.as_ref(), Argument::String(..) | Argument::Address(..) | Argument::Sequence(..))
			{
				return Err(SimplifyError::BadType{kind: lhs.get_type(), op: arg_ty});
			}
			if matches!(rhs.as_ref(), Argument::String(..) | Argument::Address(..) | Argument::Sequence(..))
			{
				return Err(SimplifyError::BadType{kind: rhs.get_type(), op: arg_ty});
			}
			
			if let (&Argument::Constant(Number::Integer(lhs)), &Argument::Constant(Number::Integer(rhs))) = (lhs.as_ref(), rhs.as_ref())
			{
				// both sides have known values so we can simply compute the result
				let result = match arg_ty
				{
					ArgumentType::Add =>
						lhs.checked_add(rhs).ok_or_else(|| OverflowError::Add{lhs: Number::Integer(lhs), rhs: Number::Integer(rhs)}),
					ArgumentType::Subtract =>
						lhs.checked_sub(rhs).ok_or_else(|| OverflowError::Subtract{lhs: Number::Integer(lhs), rhs: Number::Integer(rhs)}),
					ArgumentType::Multiply =>
						lhs.checked_mul(rhs).ok_or_else(|| OverflowError::Multiply{lhs: Number::Integer(lhs), rhs: Number::Integer(rhs)}),
					ArgumentType::Divide =>
					{
						if rhs == 0 {Err(OverflowError::DivideByZero(Number::Integer(lhs)))}
						else {lhs.checked_div(rhs).ok_or_else(|| OverflowError::Divide{lhs: Number::Integer(lhs), rhs: Number::Integer(rhs)})}
					},
					ArgumentType::Modulo =>
					{
						if rhs == 0 {Err(OverflowError::ModuloByZero(Number::Integer(lhs)))}
						else {lhs.checked_rem(rhs).ok_or_else(|| OverflowError::Modulo{lhs: Number::Integer(lhs), rhs: Number::Integer(rhs)})}
					},
					ArgumentType::BitAnd => Ok(lhs & rhs),
					ArgumentType::BitOr => Ok(lhs | rhs),
					ArgumentType::BitXor => Ok(lhs ^ rhs),
					ArgumentType::LeftShift | ArgumentType::RightShift =>
					{
						match u32::try_from(rhs).ok().and_then(|s| if arg_ty == ArgumentType::LeftShift {lhs.checked_shl(s)} else {lhs.checked_shr(s)})
						{
							None =>
							{
								Err(
									if arg_ty == ArgumentType::LeftShift {OverflowError::LeftShift{lhs: Number::Integer(lhs), rhs: Number::Integer(rhs)}}
									else {OverflowError::LeftShift{lhs: Number::Integer(lhs), rhs: Number::Integer(rhs)}}
								)
							},
							Some(r) => Ok(r),
						}
					},
					_ => unreachable!(),
				}?;
				*arg = Argument::Constant(Number::Integer(result));
			}
			else if arg_ty == ArgumentType::Modulo
			{
				let changed = if let &Argument::Constant(Number::Integer(divisor)) = rhs.as_ref()
				{
					if let Argument::Modulo{rhs: lrhs, ..} = lhs.as_ref()
					{
						match lrhs.as_ref()
						{
							&Argument::Constant(Number::Integer(inner_div)) if inner_div <= divisor =>
							{
								// special case where `(x % y) % z == x % y` because `y <= z`
								*arg  = mem::replace(lhs.as_mut(), Argument::Constant(Number::Integer(0)));
								true
							},
							_ => false,
						}
					}
					else {false}
				}
				else {false};
				if !changed {neutralize_raw(arg)?;}
			}
			else if matches!(arg_ty, ArgumentType::LeftShift | ArgumentType::RightShift)
			{
				neutralize_raw(arg)?;
			}
			else
			{
				// !(ArgumentType::Modulo | ArgumentType::LeftShift | ArgumentType::RightShift)
				
				fn search<'l, 'a>(arg_ty: ArgumentType, mut curr: &'a mut Argument<'l>, mut invert: bool) -> Option<(&'a mut Argument<'l>, bool)>
				{
					loop
					{
						let curr_ty = curr.get_type();
						match curr
						{
							Argument::Add{lhs, rhs} | Argument::Subtract{lhs, rhs} | Argument::Multiply{lhs, rhs}
								| Argument::BitAnd{lhs, rhs} | Argument::BitOr{lhs, rhs} | Argument::BitXor{lhs, rhs} =>
							{
								if arg_ty == curr_ty || (matches!(arg_ty, ArgumentType::Add | ArgumentType::Subtract)
									&& matches!(curr_ty, ArgumentType::Add | ArgumentType::Subtract))
								{
									let lhs_const = lhs.get_type() == ArgumentType::Constant;
									let rhs_const = rhs.get_type() == ArgumentType::Constant;
									assert!(!lhs_const || !rhs_const, "missed simplification of {curr:?}");
									if lhs_const || rhs_const
									{
										return Some((curr, invert ^ (curr_ty == ArgumentType::Subtract && rhs_const)));
									}
									match curr
									{
										Argument::Add{lhs, rhs} | Argument::Subtract{lhs, rhs} | Argument::Multiply{lhs, rhs}
											| Argument::BitAnd{lhs, rhs} | Argument::BitOr{lhs, rhs} | Argument::BitXor{lhs, rhs} =>
										{
											// process lhs recursively
											if let Some(result) = search(arg_ty, lhs.as_mut(), invert)
											{
												return Some(result);
											}
											// process rhs iteratively
											(curr, invert) = (rhs.as_mut(), invert ^ (curr_ty == ArgumentType::Subtract));
										},
										_ => unreachable!(),
									}
								}
								else {return None;}
							},
							Argument::Negate(value) =>
							{
								if matches!(arg_ty, ArgumentType::Add | ArgumentType::Subtract)
								{
									(curr, invert) = (value.as_mut(), !invert);
								}
								else {return None;}
							},
							Argument::Divide{lhs, rhs} =>
							{
								if arg_ty == ArgumentType::Divide
								{
									let lhs_const = lhs.get_type() == ArgumentType::Constant;
									let rhs_const = rhs.get_type() == ArgumentType::Constant;
									assert!(!lhs_const || !rhs_const, "missed simplification of {curr:?}");
									// special case, simplifying from `rhs` requires it to be an immediate constant
									if lhs_const || rhs_const
									{
										return Some((curr, invert ^ rhs_const));
									}
									// regular iteration
									let Argument::Divide{lhs, ..} = curr else {unreachable!()};
									curr = lhs.as_mut();
								}
								else {return None;}
							},
							_ => return None,
						}
					}
				}
				
				// lhs and rhs are both simplified, so they contain at most one relevant constant each
				let (lhs_const, lhs_inv) = if lhs.get_type() == ArgumentType::Constant {(Some(lhs.as_mut()), false)}
				else if let Some((la, li)) = search(arg_ty, lhs.as_mut(), false)
				{
					(Some(match la
					{
						Argument::Constant(..) => la,
						Argument::Add{lhs, rhs} | Argument::Subtract{lhs, rhs} | Argument::Multiply{lhs, rhs} | Argument::Divide{lhs, rhs}
							| Argument::BitAnd{lhs, rhs} | Argument::BitOr{lhs, rhs} | Argument::BitXor{lhs, rhs} =>
						{
							if lhs.get_type() == ArgumentType::Constant {lhs.as_mut()} else {rhs.as_mut()}
						},
						_ => unreachable!(),
					}), li)
				}
				else {(None, false /* ignored */)};
				let rhs_pre_inv = arg_ty == ArgumentType::Subtract || arg_ty == ArgumentType::Divide;
				let (mut rhs_arg, rhs_inv) = if rhs.get_type() == ArgumentType::Constant {(Some(rhs.as_mut()), rhs_pre_inv)}
				else if arg_ty == ArgumentType::Divide {(None, false /* ignored */)} // we can't search `rhs` of a division
				else if let Some((ra, ri)) = search(arg_ty, rhs.as_mut(), rhs_pre_inv) {(Some(ra), ri)}
				else {(None, false /* ignored */)};
				let rhs_const = rhs_arg.as_mut().map(|ra|
				{
					match ra
					{
						Argument::Constant(..) => ra,
						Argument::Add{lhs, rhs} | Argument::Subtract{lhs, rhs} | Argument::Multiply{lhs, rhs} | Argument::Divide{lhs, rhs}
							| Argument::BitAnd{lhs, rhs} | Argument::BitOr{lhs, rhs} | Argument::BitXor{lhs, rhs} =>
						{
							if lhs.get_type() == ArgumentType::Constant {lhs.as_mut()} else {rhs.as_mut()}
						},
						_ => unreachable!(),
					}
				});
				// if there's 2 relevant constants, attempt to simplify
				match (lhs_const, rhs_const)
				{
					(Some(lhs_const), Some(rhs_const)) =>
					{
						let Argument::Constant(Number::Integer(lhs_val)) = lhs_const else {unreachable!()};
						let &mut Argument::Constant(Number::Integer(rhs_val)) = rhs_const else {unreachable!()};
						// apply the rhs value to lhs' operation
						*lhs_val = match arg_ty
						{
							ArgumentType::Add | ArgumentType::Subtract =>
							{
								let r = if lhs_inv ^ rhs_inv {lhs_val.checked_sub(rhs_val)} else {lhs_val.checked_add(rhs_val)};
								r.ok_or_else(||
								{
									if lhs_inv ^ rhs_inv {OverflowError::Subtract{lhs: Number::Integer(*lhs_val), rhs: Number::Integer(rhs_val)}}
									else {OverflowError::Add{lhs: Number::Integer(*lhs_val), rhs: Number::Integer(rhs_val)}}
								})
							},
							ArgumentType::Multiply =>
							{
								lhs_val.checked_mul(rhs_val)
									.ok_or_else(|| OverflowError::Multiply{lhs: Number::Integer(*lhs_val), rhs: Number::Integer(rhs_val)})
							},
							ArgumentType::Divide =>
							{
								if lhs_inv
								{
									lhs_val.checked_mul(rhs_val)
										.ok_or_else(|| OverflowError::Multiply{lhs: Number::Integer(*lhs_val), rhs: Number::Integer(rhs_val)})
								}
								else
								{
									lhs_val.checked_div(rhs_val)
										.ok_or_else(|| OverflowError::Divide{lhs: Number::Integer(*lhs_val), rhs: Number::Integer(rhs_val)})
								}
							},
							_ => unreachable!(),
						}?;
						// splice out the rhs constant argument
						let rhs_arg = rhs_arg.unwrap();
						match rhs_arg
						{
							Argument::Constant(..) =>
							{
								// rewrite arg instead because `rhs_arg` is `rhs`
								*arg = match mem::replace(arg, Argument::Constant(Number::Integer(0)))
								{
									Argument::Add{lhs, ..} | Argument::Subtract{lhs, ..} | Argument::Multiply{lhs, ..}
										| Argument::Divide{lhs, ..} | Argument::BitAnd{lhs, ..} | Argument::BitOr{lhs, ..}
										| Argument::BitXor{lhs, ..} => *lhs,
									_ => unreachable!(),
								};
							},
							Argument::Add{lhs, rhs} | Argument::Multiply{lhs, rhs} | Argument::BitAnd{lhs, rhs}
								| Argument::BitOr{lhs, rhs} | Argument::BitXor{lhs, rhs} =>
							{
								let in_mut = if lhs.get_type() == ArgumentType::Constant {rhs.as_mut()} else {lhs.as_mut()};
								*rhs_arg = mem::replace(in_mut, Argument::Constant(Number::Integer(0)));
							},
							Argument::Subtract{lhs, ..} =>
							{
								if lhs.get_type() == ArgumentType::Constant
								{
									let Argument::Subtract{rhs, ..} = mem::replace(rhs_arg, Argument::Constant(Number::Integer(0)))
									else {unreachable!()};
									*rhs_arg = Argument::Negate(rhs);
								}
								else
								{
									*rhs_arg = mem::replace(lhs.as_mut(), Argument::Constant(Number::Integer(0)));
								}
							},
							_ => unreachable!(),
						}
						// deep neutralization because we've changed `lhs` and `rhs` so up to `arg` may be subject to neutralization again
						neutralize(arg)?;
					},
					_ => neutralize_raw(arg)?, // nothing to do but check neutralization
				}
			}
		},
		Argument::Negate(value) =>
		{
			match value.as_ref()
			{
				Argument::Subtract{..} =>
				{
					let Argument::Subtract{lhs, rhs} = mem::replace(value.as_mut(), Argument::Constant(Number::Integer(0))) else {unreachable!()};
					*arg = Argument::Subtract{lhs: rhs, rhs: lhs};
				},
				&Argument::Constant(Number::Integer(value)) =>
				{
					if value == i64::MIN
					{
						return Err(SimplifyError::Overflow(OverflowError::Negate));
					}
					*arg = Argument::Constant(Number::Integer(-value));
				},
				Argument::String(..) | Argument::Address(..) | Argument::Sequence(..) =>
				{
					return Err(SimplifyError::BadType{kind: value.get_type(), op: arg_ty});
				},
				_ => (),
			}
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
		},
		Argument::Address(addr) =>
		{
			if matches!(addr.as_ref(), Argument::String(..) | Argument::Address(..) | Argument::Sequence(..))
			{
				return Err(SimplifyError::BadType{kind: addr.get_type(), op: arg_ty});
			}
		},
		_ => (),
	}
	Ok(())
}

pub fn simplify<'l>(arg: &mut Argument<'l>) -> Result<(), SimplifyError>
{
	match arg
	{
		Argument::Add{lhs, rhs} | Argument::Subtract{lhs, rhs} | Argument::Multiply{lhs, rhs} | Argument::Divide{lhs, rhs}
			| Argument::Modulo{lhs, rhs} | Argument::BitAnd{lhs, rhs} | Argument::BitOr{lhs, rhs} | Argument::BitXor{lhs, rhs}
			| Argument::LeftShift{lhs, rhs} | Argument::RightShift{lhs, rhs} =>
		{
			simplify(lhs)?;
			simplify(rhs)?;
		},
		Argument::Negate(value) | Argument::Not(value) | Argument::Address(value) => simplify(value)?,
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

impl From<OverflowError> for SimplifyError
{
	fn from(value: OverflowError) -> Self
	{
		SimplifyError::Overflow(value)
	}
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
