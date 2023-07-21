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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Search
{
	Exact, Below, Above,
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
	
	fn locate(&self, addr: u32, search: Search) -> Option<usize>
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
					return match search
					{
						Search::Exact => None,
						Search::Below => if first > 0 {Some(first - 1)} else {None},
						Search::Above => Some(first),
					};
				}
				debug_assert!(mid > first);
				last = mid - 1;
			}
			else if addr > range.last
			{
				if mid == last
				{
					return match search
					{
						Search::Exact => None,
						Search::Below => Some(last),
						Search::Above => if last < self.parts.len() - 1 {Some(last + 1)} else {None},
					};
				}
				debug_assert!(mid < last);
				first = mid + 1;
			}
			else {return Some(mid);}
		}
	}
	
	pub fn get(&self, addr: u32) -> Option<&[u8]>
	{
		self.locate(addr, Search::Exact).map(|i| &self.parts[i]).map(|p| &p.data[(addr - p.range.first) as usize..])
	}
	
	pub fn get_mut(&mut self, addr: u32) -> Option<&mut [u8]>
	{
		self.locate(addr, Search::Exact).map(|i| &mut self.parts[i]).map(|p| &mut p.data[(addr - p.range.first) as usize..])
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
			let idx_first = match self.locate(addr.saturating_sub(1), Search::Above)
			{
				None => self.parts.len(),
				Some(idx) => idx,
			};
			// compute the last index involved in a merge (`None` if no merging)
			let idx_last = match self.locate(addr_last.saturating_add(1), Search::Below)
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
	
	pub fn count(&self) -> (u32, usize)
	{
		let mut addrs = 0;
		for seg in self.parts.iter()
		{
			// off by one but cannot overflow (we compensate afterwards)
			addrs += seg.range.last - seg.range.first;
		}
		// `self.parts.len() <= 1 << (u32::BITS - 1)` otherwise segments would get merged
		addrs = addrs.saturating_add(self.parts.len() as u32);
		(addrs, self.parts.len())
	}
	
	pub fn iter<'s>(&'s self) -> Iter<'s>
	{
		Iter(self.parts.iter())
	}
	
	pub fn count_range(&self, range: MemoryRange) -> (u32, usize)
	{
		let first = match self.locate(range.first, Search::Above)
		{
			None => self.parts.len(),
			Some(idx) => idx,
		};
		let mut addrs = 0;
		let mut cnt = 0;
		for seg in self.parts[first..].iter().filter(|seg| seg.range.first <= range.last)
		{
			debug_assert!(seg.range.last >= range.first);
			let min_addr = seg.range.first.max(range.first);
			let max_addr = seg.range.last.min(range.last);
			// off by one, see `Self::count`
			addrs += max_addr - min_addr;
			cnt += 1;
		}
		// safe for the same reason as in `Self::count`
		addrs = addrs.saturating_add(cnt as u32);
		(addrs, cnt)
	}
	
	pub fn iter_range<'s>(&'s self, range: MemoryRange) -> RangeIter<'s>
	{
		let first = match self.locate(range.first, Search::Above)
		{
			None => self.parts.len(),
			Some(idx) => idx,
		};
		RangeIter{base: self, next: first, range}
	}
	
	pub fn remove(&mut self, addr: u32) -> Option<(MemoryRange, Vec<u8>)>
	{
		match self.locate(addr, Search::Exact)
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
		match self.locate(range.first, Search::Above)
		{
			Some(first_idx) if self.parts[first_idx].range.last >= range.first =>
			{
				let first = &mut self.parts[first_idx];
				if range.first > first.range.first && range.last < first.range.last
				{
					// only one segment is affected, but we have to split it into 2
					// both casts are fine as the values are smaller than `first.data.len()`
					let start_off = (range.first - first.range.first) as usize;
					let end_off = (first.range.last - range.last) as usize;
					
					let end_range = MemoryRange{first: range.last + 1, last: first.range.last};
					let end_data = first.data[first.data.len() - end_off..].to_owned();
					first.range.last = range.first - 1;
					first.data.truncate(start_off);
					self.parts.insert(first_idx + 1, MemorySegment{range: end_range, data: end_data});
				}
				else
				{
					let remove_first = if range.first <= first.range.first
					{
						if range.last < first.range.last
						{
							// cut from the beginning
							first.data.drain(..(range.last + 1 - first.range.first) as usize);
							first.range.first = range.last + 1;
							false
						}
						else {true}
					}
					else
					{
						// no overflow because `range.first <= first.range.last` (from locate)
						first.data.truncate((range.first - first.range.first) as usize);
						first.range.last = range.first - 1;
						false // never drop because `range.first > first.range.first`
					};
					
					// we know that something exists not after `range.last` so this must exist too
					let last_idx = self.locate(range.last, Search::Below).unwrap();
					if last_idx > first_idx
					{
						let last = &mut self.parts[last_idx];
						// we know `range.first < last.range.last` because `range.first <= first.range.last` (from locate)
						let remove_last = if range.last < last.range.last
						{
							// cut from the beginning
							last.data.drain(..(range.last + 1 - last.range.first) as usize);
							last.range.first = range.last + 1;
							false
						}
						else {true};
						self.parts.drain(first_idx + (!remove_first) as usize..last_idx + remove_last as usize);
					}
					else
					{
						if remove_first {self.parts.remove(first_idx);}
					}
				}
			},
			_ => return,
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

#[derive(Clone, Debug)]
pub struct RangeIter<'l>
{
	base: &'l MemoryMap,
	next: usize,
	range: MemoryRange,
}

impl<'l> RangeIter<'l>
{
	pub fn range(&self) -> MemoryRange
	{
		self.range
	}
}

impl<'l> Iterator for RangeIter<'l>
{
	type Item = (MemoryRange, &'l [u8]);
	
	fn next(&mut self) -> Option<Self::Item>
	{
		match self.base.parts.get(self.next)
		{
			None => None,
			Some(seg) =>
			{
				if seg.range.first <= self.range.last
				{
					self.next += 1;
					let (start_off, first_addr) = if self.range.first <= seg.range.first {(0, seg.range.first)}
					else {((self.range.first - seg.range.first) as usize, self.range.first)};
					let (end_off, last_addr) = if self.range.last >= seg.range.last {(0, seg.range.last)}
					else {((seg.range.last - self.range.last) as usize, self.range.last)};
					Some((MemoryRange{first: first_addr, last: last_addr}, &seg.data[start_off..seg.data.len() - end_off]))
				}
				else
				{
					self.next = self.base.parts.len();
					None
				}
			},
		}
	}
}

impl<'l> FusedIterator for RangeIter<'l> {}

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
		// just before first word
		assert_eq!(map.locate(99, Search::Exact), None);
		assert_eq!(map.locate(99, Search::Below), None);
		assert_eq!(map.locate(99, Search::Above), Some(0));
		// first addr of second word
		assert_eq!(map.locate(100, Search::Exact), Some(0));
		assert_eq!(map.locate(100, Search::Below), Some(0));
		assert_eq!(map.locate(100, Search::Above), Some(0));
		// last addr of second word
		assert_eq!(map.locate(104, Search::Exact), Some(0));
		assert_eq!(map.locate(104, Search::Below), Some(0));
		assert_eq!(map.locate(104, Search::Above), Some(0));
		// just after first word (before second)
		assert_eq!(map.locate(105, Search::Exact), None);
		assert_eq!(map.locate(105, Search::Below), Some(0));
		assert_eq!(map.locate(105, Search::Above), Some(1));
		
		// just before second word (after first)
		assert_eq!(map.locate(119, Search::Exact), None);
		assert_eq!(map.locate(119, Search::Below), Some(0));
		assert_eq!(map.locate(119, Search::Above), Some(1));
		// first addr of second word
		assert_eq!(map.locate(120, Search::Exact), Some(1));
		assert_eq!(map.locate(120, Search::Below), Some(1));
		assert_eq!(map.locate(120, Search::Above), Some(1));
		// last addr of second word
		assert_eq!(map.locate(124, Search::Exact), Some(1));
		assert_eq!(map.locate(124, Search::Below), Some(1));
		assert_eq!(map.locate(124, Search::Above), Some(1));
		// just after second word
		assert_eq!(map.locate(125, Search::Exact), None);
		assert_eq!(map.locate(125, Search::Below), Some(1));
		assert_eq!(map.locate(125, Search::Above), None);
	}
	
	#[test]
	fn sparse()
	{
		let mut map = MemoryMap::new();
		assert_eq!(map.put(100, b"hello"), Ok(5));
		assert_eq!(map.put(120, b"world"), Ok(5));
		assert_eq!(map.len(), 2);
		assert_eq!(map.count(), (10, 2));
		let mut iter = map.iter();
		assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
		assert_eq!(iter.next(), None);
		assert_eq!(map.count_range(MemoryRange::new(90, 102)), (3, 1));
		let mut iter = map.iter_range(MemoryRange::new(90, 102));
		assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 102}, b"hel".as_ref())));
		assert_eq!(iter.next(), None);
		assert_eq!(map.count_range(MemoryRange::new(90, 110)), (5, 1));
		let mut iter = map.iter_range(MemoryRange::new(90, 110));
		assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
		assert_eq!(iter.next(), None);
		assert_eq!(map.count_range(MemoryRange::new(100, 110)), (5, 1));
		assert!(map.iter_range(MemoryRange::new(100, 110)).eq(map.iter_range(MemoryRange::new(90, 110))));
		assert_eq!(map.count_range(MemoryRange::new(103, 104)), (2, 1));
		let mut iter = map.iter_range(MemoryRange::new(103, 104));
		assert_eq!(iter.next(), Some((MemoryRange{first: 103, last: 104}, b"lo".as_ref())));
		assert_eq!(iter.next(), None);
		assert_eq!(map.count_range(MemoryRange::new(103, 110)), (2, 1));
		assert!(map.iter_range(MemoryRange::new(103, 110)).eq(map.iter_range(MemoryRange::new(103, 104))));
		assert_eq!(map.count_range(MemoryRange::new(105, 119)), (0, 0));
		assert_eq!(map.iter_range(MemoryRange::new(105, 119)).next(), None);
		assert_eq!(map.count_range(MemoryRange::new(104, 120)), (2, 2));
		let mut iter = map.iter_range(MemoryRange::new(104, 120));
		assert_eq!(iter.next(), Some((MemoryRange{first: 104, last: 104}, b"o".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 120}, b"w".as_ref())));
		assert_eq!(iter.next(), None);
		assert_eq!(map.count_range(MemoryRange::new(103, 121)), (4, 2));
		let mut iter = map.iter_range(MemoryRange::new(103, 121));
		assert_eq!(iter.next(), Some((MemoryRange{first: 103, last: 104}, b"lo".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 121}, b"wo".as_ref())));
		assert_eq!(iter.next(), None);
		assert_eq!(map.count_range(MemoryRange::new(90, 121)), (7, 2));
		let mut iter = map.iter_range(MemoryRange::new(90, 121));
		assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 121}, b"wo".as_ref())));
		assert_eq!(iter.next(), None);
		assert_eq!(map.count_range(MemoryRange::new(90, 130)), (10, 2));
		let mut iter = map.iter_range(MemoryRange::new(90, 130));
		assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
		assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
		assert_eq!(iter.next(), None);
		
		let mut map = MemoryMap::new();
		assert_eq!(map.put(0, b"first"), Ok(5));
		assert_eq!(map.put(((1u64 << u32::BITS) - 4) as u32, b"last"), Ok(4));
		assert_eq!(map.len(), 2);
		assert_eq!(map.count(), (9, 2));
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
