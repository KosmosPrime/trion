use core::fmt;

use crate::macros::numeric_enum;

numeric_enum!
{
	#[enum(pub u8)]
	#[TryFrom(u8 => TryFromU8Error: as(pub struct), derive(Display), derive(Error))]
	#[Into(u8)]
	enum Register
	{
		R0 = 0,
		R1 = 1,
		R2 = 2,
		R3 = 3,
		R4 = 4,
		R5 = 5,
		R6 = 6,
		R7 = 7,
		R8 = 8,
		R9 = 9,
		R10 = 10,
		R11 = 11,
		R12 = 12,
		SP = 13,
		LR = 14,
		PC = 15,
	}
}

impl fmt::Display for Register
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		fmt::Debug::fmt(self, f)
	}
}
