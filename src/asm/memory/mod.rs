use core::fmt;

pub mod map;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct MemoryRange
{
	first: u32,
	last: u32,
}

impl MemoryRange
{
	pub const MIN: Self = MemoryRange{first: 0, last: 0};
	pub const MAX: Self = MemoryRange{first: u32::MAX, last: u32::MAX};
	pub const ALL: Self = MemoryRange{first: 0, last: u32::MAX};
	
	pub fn new(first: u32, last: u32) -> Self
	{
		if first > last {panic!("invalid range {first} -> {last}");}
		Self{first, last}
	}
	
	pub fn get_first(&self) -> u32
	{
		self.first
	}
	
	pub fn get_last(&self) -> u32
	{
		self.last
	}
}

macro_rules!mem_fmt
{
	($name:ident) =>
	{
		impl fmt::$name for MemoryRange
		{
			fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
			{
				<u32 as fmt::$name>::fmt(&self.first, f)?;
				f.write_str(" -> ")?;
				<u32 as fmt::$name>::fmt(&self.last, f)
			}
		}
	};
}
mem_fmt!(Display);
mem_fmt!(UpperHex);
mem_fmt!(LowerHex);
mem_fmt!(Octal);
mem_fmt!(Binary);
