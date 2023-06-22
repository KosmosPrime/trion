use core::fmt;

use crate::macros::numeric_enum;

numeric_enum!
{
	pub enum Condition for u8 | TryFromU8Error
	{
		Equal = 0,
		NonEqual = 1,
		CarrySet = 2, // HigherSame
		CarryClear = 3, // Lower
		Minus = 4,
		Plus = 5,
		Overflow = 6,
		NoOverflow = 7,
		Higher = 8,
		LowerEqual = 9,
		GreaterEqual = 10,
		Less = 11,
		Greater = 12,
		LessEqual = 13,
		Always = 14,
	}
}

impl fmt::Display for Condition
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Equal => f.write_str("EQ"),
			Self::NonEqual => f.write_str("NE"),
			Self::CarrySet => f.write_str("CS"),
			Self::CarryClear => f.write_str("CC"),
			Self::Minus => f.write_str("MI"),
			Self::Plus => f.write_str("PL"),
			Self::Overflow => f.write_str("VS"),
			Self::NoOverflow => f.write_str("VC"),
			Self::Higher => f.write_str("HI"),
			Self::LowerEqual => f.write_str("LS"),
			Self::GreaterEqual => f.write_str("GE"),
			Self::Less => f.write_str("LT"),
			Self::Greater => f.write_str("GT"),
			Self::LessEqual => f.write_str("LE"),
			Self::Always => Ok(()),
		}
	}
}
