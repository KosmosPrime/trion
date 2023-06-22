use core::fmt::{self, Write};

use crate::arm6m::reg::Register;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RegisterSet(u16);

impl RegisterSet
{
	pub fn new() -> Self
	{
		Self(0)
	}
	
	pub fn of(value: u16) -> Self
	{
		Self(value)
	}
	
	pub fn is_empty(&self) -> bool
	{
		self.0 == 0
	}
	
	pub fn count(&self) -> usize
	{
		self.0.count_ones() as usize
	}
	
	pub fn add(&mut self, reg: Register) -> bool
	{
		let mask = 1 << u8::from(reg);
		let changed = (self.0 & mask) == 0;
		self.0 |= mask;
		changed
	}
	
	pub fn set_bits(&mut self, bits: u16) -> u16
	{
		let prev = !self.0 & bits;
		self.0 |= bits;
		prev
	}
	
	pub fn remove(&mut self, reg: Register) -> bool
	{
		let mask = 1 << u8::from(reg);
		let changed = (self.0 & mask) != 0;
		self.0 &= !mask;
		changed
	}
	
	pub fn unset_bits(&mut self, bits: u16) -> u16
	{
		let prev = self.0 & bits;
		self.0 &= !bits;
		prev
	}
	
	pub fn contains(&self, reg: Register) -> bool
	{
		(self.0 & (1 << u8::from(reg))) != 0
	}
	
	pub fn get_bits(&self) -> u16
	{
		self.0
	}
	
	pub fn iter(&self) -> RegIter
	{
		if self.0 > 0
		{
			let first = self.0.trailing_zeros() as u8;
			RegIter{next: Some(first), more: self.0 >> first}
		}
		else {RegIter{next: None, more: 0}}
	}
	
	pub fn clear(&mut self)
	{
		self.0 = 0;
	}
}

impl fmt::Display for RegisterSet
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result 
	{
		f.write_char('{')?;
		let mut first = true;
		for i in 0..16u8
		{
			if ((self.0 >> i) & 1) != 0
			{
				if !first {f.write_str(", ")?;}
				Register::try_from(i).unwrap().fmt(f)?;
				first = false;
			}
		}
		f.write_char('}')
	}
}

impl IntoIterator for RegisterSet
{
	type Item = Register;
	type IntoIter = RegIter;
	
	fn into_iter(self) -> Self::IntoIter
	{
		self.iter()
	}
}

pub struct RegIter
{
	next: Option<u8>,
	more: u16,
}

impl Iterator for RegIter
{
	type Item = Register;
	
	fn next(&mut self) -> Option<Self::Item>
	{
		match self.next
		{
			None => None,
			Some(curr) =>
			{
				self.next = if self.more > 0
				{
					let dist = self.more.trailing_zeros();
					self.more >>= dist;
					Some(curr + dist as u8)
				}
				else {None};
				Some(Register::try_from(curr).unwrap())
			},
		}
	}
	
	fn size_hint(&self) -> (usize, Option<usize>)
	{
		if self.next.is_some()
		{
			let cnt = 1 + self.more.count_ones() as usize;
			(cnt, Some(cnt))
		}
		else {(0, Some(0))}
	}
}
