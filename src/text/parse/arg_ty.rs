use core::fmt;

use crate::macros::numeric_enum;

numeric_enum!
{
	#[enum(pub u8)]
	#[TryFrom(u8 => TryFromU8Error: as(pub struct), derive(Display), derive(Error))]
	#[Into(u8)]
	enum ArgumentType
	{
		Constant, Identifier, String,
		Add, Negate, Subtract, Multiply, Divide, Modulo,
		Not, BitAnd, BitOr, BitXor, LeftShift, RightShift,
		Address, Sequence, Function,
	}
}

impl fmt::Display for ArgumentType
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Constant => f.write_str("constant"),
			Self::Identifier => f.write_str("identifier"),
			Self::String => f.write_str("string"),
			Self::Add => f.write_str("addition"),
			Self::Negate => f.write_str("negation"),
			Self::Subtract => f.write_str("subtraction"),
			Self::Multiply => f.write_str("multiplication"),
			Self::Divide => f.write_str("division"),
			Self::Modulo => f.write_str("modulo"),
			Self::Not => f.write_str("binary not"),
			Self::BitAnd => f.write_str("binary and"),
			Self::BitOr => f.write_str("binary or"),
			Self::BitXor => f.write_str("binary xor"),
			Self::LeftShift => f.write_str("left shift"),
			Self::RightShift => f.write_str("right shift"),
			Self::Address => f.write_str("address"),
			Self::Sequence => f.write_str("sequence"),
			Self::Function => f.write_str("function call"),
		}
	}
}
