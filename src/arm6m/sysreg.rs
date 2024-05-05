use core::fmt;

use crate::macros::numeric_enum;

numeric_enum!
{
	#[enum(pub u8)]
	#[TryFrom(u8 => TryFromU8Error: as(pub struct), derive(Display), derive(Error))]
	#[Into(u8)]
	enum SystemReg
	{
		APSR = 0,
		IAPSR = 1,
		EAPSR = 2,
		XPSR = 3,
		IPSR = 5,
		EPSR = 6,
		IEPSR = 7,
		MSP = 8,
		PSP = 9,
		PRIMASK = 16,
		CONTROL = 20,
	}
}

impl fmt::Display for SystemReg
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		fmt::Debug::fmt(self, f)
	}
}
