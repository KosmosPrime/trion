use core::fmt;
use core::iter::FusedIterator;
use core::ops::{BitAnd, BitOr, BitXor, Not};

use crate::text::parse::ArgumentType;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ArgChoice(u32);

const ALL: u32 =
{
	let mut result = 0u32;
	let mut i = 0;
	while i < ArgumentType::ALL.len()
	{
		result |= 1u32 << ArgumentType::ALL[i] as u32;
		i += 1;
	}
	result
};

impl ArgChoice
{
	pub fn none() -> Self
	{
		Self(0)
	}
	
	pub fn of(ty: ArgumentType) -> Self
	{
		Self(1 << u8::from(ty))
	}
	
	pub fn all() -> Self
	{
		Self(ALL)
	}
	
	pub fn is_empty(&self) -> bool
	{
		self.0 == 0
	}
	
	pub fn size(&self) -> usize
	{
		// usize is at least u16, so this is fine
		self.0.count_ones() as usize
	}
	
	pub fn add(&mut self, ty: ArgumentType) -> bool
	{
		let mask = 1 << u8::from(ty);
		let prev = (self.0 & mask) == 0;
		self.0 |= mask;
		prev
	}
	
	pub fn remove(&mut self, ty: ArgumentType) -> bool
	{
		let mask = 1 << u8::from(ty);
		let prev = (self.0 & mask) != 0;
		self.0 &= !mask;
		prev
	}
	
	pub fn flip(&mut self, ty: ArgumentType) -> bool
	{
		let mask = 1 << u8::from(ty);
		let prev = (self.0 & mask) != 0;
		self.0 ^= mask;
		prev
	}
	
	pub fn contains(&self, ty: ArgumentType) -> bool
	{
		(self.0 & (1 << u8::from(ty))) != 0
	}
	
	pub fn iter(&self) -> Iter
	{
		Iter{more: self.0, offset: 0}
	}
	
	pub fn clear(&mut self)
	{
		self.0 = 0;
	}
}

impl Default for ArgChoice
{
	fn default() -> Self
	{
		Self::none()
	}
}

impl From<ArgumentType> for ArgChoice
{
	fn from(value: ArgumentType) -> Self
	{
		Self::of(value)
	}
}

impl<const N: usize> From<[ArgumentType; N]> for ArgChoice
{
	fn from(value: [ArgumentType; N]) -> Self
	{
		Self::from(&value as &[_])
	}
}

impl From<&[ArgumentType]> for ArgChoice
{
	fn from(value: &[ArgumentType]) -> Self
	{
		let mut bits = 0;
		for ty in value
		{
			bits |= 1 << u8::from(*ty);
		}
		Self(bits)
	}
}

impl fmt::Display for ArgChoice
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		if !self.is_empty()
		{
			let mut first = true;
			self.iter().try_for_each(|ty|
			{
				if first {first = false;}
				else {f.write_str(", ")?;}
				ty.fmt(f)
			})
		}
		else {f.write_str("<no arguments>")}
	}
}

impl Not for ArgChoice
{
	type Output = ArgChoice;
	
	fn not(self) -> Self::Output
	{
		Self(self.0 ^ ALL)
	}
}

impl BitAnd for ArgChoice
{
	type Output = ArgChoice;
	
	fn bitand(self, rhs: Self) -> Self::Output
	{
		Self(self.0 & rhs.0)
	}
}

impl BitOr for ArgChoice
{
	type Output = ArgChoice;
	
	fn bitor(self, rhs: Self) -> Self::Output
	{
		Self(self.0 | rhs.0)
	}
}

impl BitXor for ArgChoice
{
	type Output = ArgChoice;
	
	fn bitxor(self, rhs: Self) -> Self::Output
	{
		Self(self.0 ^ rhs.0)
	}
}

pub struct Iter
{
	more: u32,
	offset: u8,
}

impl Iterator for Iter
{
	type Item = ArgumentType;
	
	fn next(&mut self) -> Option<Self::Item>
	{
		if self.more != 0
		{
			let skip = self.more.trailing_zeros();
			// fine because we test that there's no more than 32 variants (and 32 < 255)
			let next = self.offset + skip as u8;
			if skip == u32::BITS - 1
			{
				// we have to add 1, so this shift would overflow
				self.more = 0;
			}
			else
			{
				self.more >>= skip + 1;
				self.offset = next + 1;
			}
			Some(next.try_into().unwrap())
		}
		else {None}
	}
	
	fn size_hint(&self) -> (usize, Option<usize>)
	{
		// fine for the same reason as `ArgChoice::size()`
		let n = self.more.count_ones() as usize;
		(n, Some(n))
	}
}

impl FusedIterator for Iter {}

#[cfg(test)]
mod test
{
	use super::*;
	
	#[test]
	fn test_variants_fit_u32()
	{
		for &curr in ArgumentType::ALL
		{
			assert!(u32::from(u8::from(curr)) < u32::BITS, "argument type index out of bounds: {curr} maps to {}", u8::from(curr));
		}
	}
	
	#[test]
	fn test_all_size()
	{
		let size = ArgumentType::ALL.len();
		assert_eq!(ArgChoice::all().size(), size);
		let size_hint = ArgChoice::all().iter().size_hint();
		assert_eq!(size_hint.0, size);
		assert_eq!(size_hint.1, Some(size));
	}
	
	#[test]
	fn test_one_to_string()
	{
		for &curr in ArgumentType::ALL
		{
			let string = ArgChoice::of(curr).to_string();
			let expect = curr.to_string();
			assert_eq!(string, expect, "`ArgChoice::of({curr:?}).to_string()` produces {string:?}, expected {expect:?}");
		}
	}
	
	#[test]
	fn test_all_to_string()
	{
		let string = ArgChoice::all().to_string();
		let mut expect = String::new();
		for i in 0..ArgumentType::ALL.len()
		{
			if i > 0 {expect.push_str(", ");}
			use fmt::Write;
			write!(&mut expect, "{}", ArgumentType::ALL[i]).unwrap();
		}
		assert_eq!(string, expect, "`ArgChoice::all().to_string()` produces {string:?}, expected {expect:?}");
	}
}
