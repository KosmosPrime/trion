use core::fmt;
use std::error::Error;
use std::iter::FusedIterator;
use std::slice::Iter as SliceIter;

use crate::asm::mem::MemoryRange;

#[derive(Clone, Debug)]
struct MemorySegment
{
	range: MemoryRange,
	data: Vec<u8>,
}

#[derive(Clone, Debug)]
pub struct MemoryMap
{
	parts: Vec<MemorySegment>,
}

impl MemoryMap
{
	pub fn new() -> Self
	{
		Self{parts: Vec::new()}
	}
	
	pub fn len(&self) -> usize
	{
		self.parts.len()
	}
	
	fn locate(&self, addr: u32, below: bool) -> Option<usize>
	{
		if self.parts.is_empty() {return None;}
		let mut first = 0;
		let mut last = self.parts.len() - 1;
		loop
		{
			let mid = first + (last - first) / 2;
			let range = self.parts[mid].range;
			if addr < range.first
			{
				if mid == first
				{
					return if below && first > 0 {Some(first - 1)} else {None};
				}
				debug_assert!(mid > first);
				last = mid - 1;
			}
			else if addr > range.last
			{
				if mid == last
				{
					return if below {Some(last)} else {None};
				}
				debug_assert!(mid < last);
				first = mid + 1;
			}
			else {return Some(mid);}
		}
	}
	
	pub fn get(&self, addr: u32) -> Option<&[u8]>
	{
		self.locate(addr, false).map(|i| &self.parts[i]).map(|p| &p.data[(addr - p.range.first) as usize..])
	}
	
	pub fn get_mut(&mut self, addr: u32) -> Option<&mut [u8]>
	{
		self.locate(addr, false).map(|i| &mut self.parts[i]).map(|p| &mut p.data[(addr - p.range.first) as usize..])
	}
	
	pub fn put(&mut self, addr: u32, data: &[u8]) -> Result<usize, PutError>
	{
		if !data.is_empty()
		{
			let have_last = usize::try_from(u32::MAX - addr).unwrap_or(usize::MAX);
			if data.len() - 1 > have_last
			{
				return Err(PutError::Overflow{need: data.len(), have: have_last.saturating_add(1)})
			}
			let addr_last = addr + (data.len() - 1) as u32;
			
			// compute the index of the final segment (and the first one involved in a merge)
			let idx_first = match self.locate(addr, true)
			{
				None => 0,
				Some(idx) =>
				{
					if self.parts[idx].range.last.saturating_add(1) < addr {idx + 1} else {idx}
				},
			};
			// compute the last index involved in a merge (`None` if no merging)
			let idx_last = match self.locate(addr_last.saturating_add(1), true)
			{
				None =>
				{
					if self.parts.is_empty() || self.parts[0].range.first - 1 > addr_last {None} else {Some(0)}
				},
				Some(idx) =>
				{
					if self.parts[idx].range.last >= addr {Some(idx)} else {None}
				},
			};
			
			match idx_last
			{
				None =>
				{
					self.parts.insert(idx_first, MemorySegment{range: MemoryRange{first: addr, last: addr_last}, data: data.to_owned()});
					Ok(data.len())
				},
				Some(idx_last) =>
				{
					let first = &mut self.parts[idx_first];
					let mut added = data.len();
					if addr <= first.range.first
					{
						if addr_last >= first.range.last
						{
							// completely replaces existing data
							added -= first.data.len();
							first.data.clear();
							first.data.extend_from_slice(data);
						}
						else
						{
							// prepend and overwrite
							let num_remove = data.len() - usize::try_from(first.range.first - addr).unwrap();
							first.data.splice(..num_remove, data.iter().copied());
							added -= num_remove;
						}
					}
					else
					{
						let offset = usize::try_from(addr - first.range.first).unwrap();
						if addr_last > first.range.last
						{
							// overwrite and append
							let num_overwrite = first.data.len() - offset;
							first.data.truncate(offset);
							first.data.extend_from_slice(data);
							added -= num_overwrite;
						}
						else
						{
							// interior overwrite only
							first.data[offset..offset + data.len()].copy_from_slice(data);
							added = 0;
						}
					}
					
					if idx_last > idx_first
					{
						// `idx_first + 1..idx_last` are always overwritten completely
						for idx in idx_first + 1..idx_last
						{
							added -= self.parts[idx].data.len();
						}
						// crazy shenanigans to copy from one segment to another
						let (first, last) = {
							let split = self.parts.split_at_mut(idx_last);
							(&mut split.0[idx_first], &split.1[0])
						};
						if addr_last < last.range.last
						{
							// only partial replaces the final segment, copy what remains
							let end_off = usize::try_from(last.range.last - addr_last).unwrap();
							let overwritten = last.data.len() - end_off;
							first.data.extend_from_slice(&last.data[last.data.len() - end_off..]);
							first.range.last = last.range.last;
							added -= overwritten;
						}
						else
						{
							// nothing of the final segment remains
							added -= last.data.len();
							first.range.last = addr_last;
						}
						first.range.first = first.range.first.min(addr);
						self.parts.drain(idx_first + 1..=idx_last);
					}
					else
					{
						first.range.first = first.range.first.min(addr);
						first.range.last = first.range.last.max(addr_last);
					}
					Ok(added)
				},
			}
		}
		else {Ok(0)}
	}
	
	pub fn iter<'s>(&'s self) -> Iter<'s>
	{
		Iter(self.parts.iter())
	}
	
	pub fn remove(&mut self, addr: u32) -> Option<(MemoryRange, Vec<u8>)>
	{
		match self.locate(addr, false)
		{
			None => None,
			Some(idx) =>
			{
				let seg = self.parts.remove(idx);
				Some((seg.range, seg.data))
			},
		}
	}
	
	pub fn remove_range(&mut self, range: MemoryRange)
	{
		match self.locate(range.last, true)
		{
			Some(mut last_idx) if self.parts[last_idx].range.last >= range.first =>
			{
				let last = &mut self.parts[last_idx];
				if range.first <= last.range.first
				{
					if range.last >= last.range.last
					{
						// range contains this entire segment
						last_idx += 1; // mark for removal
					}
					else
					{
						// range is a prefix of this segment
						let offset = usize::try_from(range.last - last.range.first).unwrap();
						last.range.first = range.last + 1;
						last.data.drain(..=offset);
					}
					
					let mut first_idx = last_idx;
					while first_idx > 0 && self.parts[first_idx - 1].range.first >= range.first
					{
						first_idx -= 1;
					}
					if first_idx > 0 && self.parts[first_idx - 1].range.last >= range.first
					{
						// won't get removed but truncated
						let first = &mut self.parts[first_idx - 1];
						let remain = usize::try_from(range.first - first.range.first).unwrap();
						first.range.last = range.first - 1;
						first.data.truncate(remain);
					}
					self.parts.drain(first_idx..last_idx);
				}
				else
				{
					let before_len = usize::try_from(range.first - last.range.first).unwrap();
					if range.last >= last.range.last
					{
						// range is a suffix of this segment
						last.range.last = range.first - 1;
						last.data.truncate(before_len);
					}
					else
					{
						// range is contained within this segment, splitting it in two
						let after_len = usize::try_from(last.range.last - range.last).unwrap();
						let after_data = last.data[last.data.len() - after_len..].to_owned();
						let after = MemorySegment{range: MemoryRange{first: range.last + 1, last: last.range.last}, data: after_data};
						last.range.first = range.first - 1;
						last.data.truncate(before_len);
						self.parts.insert(last_idx + 1, after);
					}
				}
			},
			_ => (),
		}
	}
	
	pub fn clear(&mut self)
	{
		self.parts.clear();
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PutError
{
	Overflow{need: usize, have: usize},
}

impl fmt::Display for PutError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Overflow{need, have} => write!(f, "segment overflow (expected {need} but got {have})"),
		}
	}
}

impl Error for PutError {}

#[derive(Clone, Debug)]
pub struct Iter<'l>(SliceIter<'l, MemorySegment>);

impl<'l> Iterator for Iter<'l>
{
	type Item = (MemoryRange, &'l [u8]);
	
	fn next(&mut self) -> Option<Self::Item>
	{
		self.0.next().map(|e| (e.range, e.data.as_ref()))
	}
}

impl<'l> FusedIterator for Iter<'l> where SliceIter<'l, MemorySegment>: FusedIterator {}

#[cfg(test)]
mod test
{
	use super::*;
	
	#[test]
	fn internal_locate()
	{
		let mut map = MemoryMap::new();
		assert_eq!(map.put(100, b"hello"), Ok(5));
		assert_eq!(map.put(120, b"world"), Ok(5));
		assert_eq!(map.locate(99, false), None);
		assert_eq!(map.locate(99, true), None);
		assert_eq!(map.locate(100, false), Some(0));
		assert_eq!(map.locate(100, true), Some(0));
		assert_eq!(map.locate(104, false), Some(0));
		assert_eq!(map.locate(104, true), Some(0));
		assert_eq!(map.locate(105, false), None);
		assert_eq!(map.locate(105, true), Some(0));
		assert_eq!(map.locate(119, false), None);
		assert_eq!(map.locate(119, true), Some(0));
		assert_eq!(map.locate(120, false), Some(1));
		assert_eq!(map.locate(120, true), Some(1));
	}
	
	#[test]
	fn sparse()
	{
		let mut map = MemoryMap::new();
		assert_eq!(map.put(100, b"hello"), Ok(5));
		assert_eq!(map.put(120, b"world"), Ok(5));
		assert_eq!(map.len(), 2);
		let mut iter = map.iter();
		assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
		assert_eq!(iter.next(), None);
		
		let mut map = MemoryMap::new();
		assert_eq!(map.put(0, b"first"), Ok(5));
		assert_eq!(map.put(((1u64 << u32::BITS) - 4) as u32, b"last"), Ok(4));
		assert_eq!(map.len(), 2);
		let mut iter = map.iter();
		assert_eq!(iter.next(), Some((MemoryRange{first: 0, last: 4}, b"first".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: u32::MAX - 3, last: u32::MAX}, b"last".as_ref())));
		assert_eq!(iter.next(), None);
	}
	
	#[test]
	fn overwrite()
	{
		let mut map = MemoryMap::new();
		assert_eq!(map.put(100, b"world hello"), Ok(11));
		assert_eq!(map.put(100, b"hello world"), Ok(0));
		assert_eq!(map.len(), 1);
		assert_eq!(map.iter().next(), Some((MemoryRange{first: 100, last: 110}, b"hello world".as_ref())));
		assert_eq!(map.put(106, b" last"), Ok(0));
		assert_eq!(map.len(), 1);
		assert_eq!(map.iter().next(), Some((MemoryRange{first: 100, last: 110}, b"hello  last".as_ref())));
		assert_eq!(map.put(100, b"first"), Ok(0));
		assert_eq!(map.len(), 1);
		assert_eq!(map.iter().next(), Some((MemoryRange{first: 100, last: 110}, b"first  last".as_ref())));
		assert_eq!(map.put(105, b"__"), Ok(0));
		assert_eq!(map.len(), 1);
		assert_eq!(map.iter().next(), Some((MemoryRange{first: 100, last: 110}, b"first__last".as_ref())));
		assert_eq!(map.put(96, b"from first"), Ok(4));
		assert_eq!(map.len(), 1);
		assert_eq!(map.iter().next(), Some((MemoryRange{first: 96, last: 110}, b"from first_last".as_ref())));
		assert_eq!(map.put(106, b" to last"), Ok(3));
		assert_eq!(map.len(), 1);
		assert_eq!(map.iter().next(), Some((MemoryRange{first: 96, last: 113}, b"from first to last".as_ref())));
		
		let mut map = MemoryMap::new();
		assert_eq!(map.put(0, b"f"), Ok(1));
		assert_eq!(map.put(u32::MAX, b"t"), Ok(1));
		assert_eq!(map.len(), 2);
		let mut iter = map.iter();
		assert_eq!(iter.next(), Some((MemoryRange{first: 0, last: 0}, b"f".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: u32::MAX, last: u32::MAX}, b"t".as_ref())));
		assert_eq!(iter.next(), None);
		assert_eq!(map.put(0, b"first"), Ok(4));
		assert_eq!(map.len(), 2);
		assert_eq!(map.put(((1u64 << u32::BITS) - 4) as u32, b"last"), Ok(3));
		assert_eq!(map.len(), 2);
		let mut iter = map.iter();
		assert_eq!(iter.next(), Some((MemoryRange{first: 0, last: 4}, b"first".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: u32::MAX - 3, last: u32::MAX}, b"last".as_ref())));
		assert_eq!(iter.next(), None);
	}
	
	#[test]
	fn join()
	{
		let mut map = MemoryMap::new();
		assert_eq!(map.put(104, b"o w"), Ok(3));
		assert_eq!(map.len(), 1);
		assert_eq!(map.iter().next(), Some((MemoryRange{first: 104, last: 106}, b"o w".as_ref())));
		assert_eq!(map.put(102, b"ll"), Ok(2));
		assert_eq!(map.len(), 1);
		assert_eq!(map.iter().next(), Some((MemoryRange{first: 102, last: 106}, b"llo w".as_ref())));
		assert_eq!(map.put(100, b"hell"), Ok(2));
		assert_eq!(map.len(), 1);
		assert_eq!(map.iter().next(), Some((MemoryRange{first: 100, last: 106}, b"hello w".as_ref())));
		assert_eq!(map.put(109, b"ld"), Ok(2));
		assert_eq!(map.len(), 2);
		let mut iter = map.iter();
		assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 106}, b"hello w".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 109, last: 110}, b"ld".as_ref())));
		assert_eq!(map.put(106, b"world "), Ok(3));
		assert_eq!(map.len(), 1);
		assert_eq!(map.iter().next(), Some((MemoryRange{first: 100, last: 111}, b"hello world ".as_ref())));
	}
	
	#[test]
	fn remove()
	{
		let mut template = MemoryMap::new();
		assert_eq!(template.put(100, b"hello"), Ok(5));
		assert_eq!(template.put(110, b"test"), Ok(4));
		assert_eq!(template.put(120, b"world"), Ok(5));
		assert_eq!(template.len(), 3);
		let mut iter = template.iter();
		assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 110, last: 113}, b"test".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
		assert_eq!(iter.next(), None);
		
		let mut map = template.clone();
		map.remove_range(MemoryRange::new(90, 130));
		assert_eq!(map.len(), 0);
		let mut map = template.clone();
		map.remove_range(MemoryRange::new(110, 114));
		assert_eq!(map.len(), 2);
		let mut iter = map.iter();
		assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
		assert_eq!(iter.next(), None);
		let mut map = template.clone();
		map.remove_range(MemoryRange::new(110, 111));
		assert_eq!(map.len(), 3);
		let mut iter = map.iter();
		assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 112, last: 113}, b"st".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
		assert_eq!(iter.next(), None);
		map.remove_range(MemoryRange::new(108, 112));
		assert_eq!(map.len(), 3);
		let mut iter = map.iter();
		assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 113, last: 113}, b"t".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
		assert_eq!(iter.next(), None);
		let mut map = template.clone();
		map.remove_range(MemoryRange::new(112, 114));
		assert_eq!(map.len(), 3);
		let mut iter = map.iter();
		assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 110, last: 111}, b"te".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
		assert_eq!(iter.next(), None);
		map.remove_range(MemoryRange::new(111, 118));
		assert_eq!(map.len(), 3);
		let mut iter = map.iter();
		assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 110, last: 110}, b"t".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
		assert_eq!(iter.next(), None);
		
		let mut map = template.clone();
		map.remove_range(MemoryRange::new(102, 111));
		assert_eq!(map.len(), 3);
		let mut iter = map.iter();
		assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 101}, b"he".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 112, last: 113}, b"st".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
		assert_eq!(iter.next(), None);
		map.remove_range(MemoryRange::new(90, 112));
		assert_eq!(map.len(), 2);
		let mut iter = map.iter();
		assert_eq!(iter.next(), Some((MemoryRange{first: 113, last: 113}, b"t".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
		assert_eq!(iter.next(), None);
		let mut map = template.clone();
		map.remove_range(MemoryRange::new(112, 122));
		assert_eq!(map.len(), 3);
		let mut iter = map.iter();
		assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 110, last: 111}, b"te".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 123, last: 124}, b"ld".as_ref())));
		assert_eq!(iter.next(), None);
		map.remove_range(MemoryRange::new(111, 128));
		assert_eq!(map.len(), 2);
		let mut iter = map.iter();
		assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 110, last: 110}, b"t".as_ref())));
		assert_eq!(iter.next(), None);
		
		assert_eq!(template.put(0, b"first"), Ok(5));
		assert_eq!(template.put(((1u64 << u32::BITS) - 4) as u32, b"last"), Ok(4));
		template.remove_range(MemoryRange::new(101, 123));
		assert_eq!(template.len(), 4);
		let mut iter = template.iter();
		assert_eq!(iter.next(), Some((MemoryRange{first: 0, last: 4}, b"first".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 100}, b"h".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 124, last: 124}, b"d".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: u32::MAX - 3, last: u32::MAX}, b"last".as_ref())));
		assert_eq!(iter.next(), None);
		template.remove_range(MemoryRange::ALL);
		assert_eq!(template.len(), 0);
	}
}
