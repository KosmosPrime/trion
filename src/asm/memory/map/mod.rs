use core::fmt;
use std::error::Error;
use std::iter::FusedIterator;
use std::slice::Iter as SliceIter;

use crate::asm::memory::MemoryRange;

#[cfg(test)]
mod test;

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
	
	pub fn find(&self, addr: u32, search: Search) -> Option<MemoryRange>
	{
		self.locate(addr, search).map(|i| self.parts[i].range)
	}
	
	pub fn get(&self, addr: u32, search: Search) -> Option<(MemoryRange, &[u8])>
	{
		self.locate(addr, search).map(|i| &self.parts[i]).map(|p| (p.range, &p.data[(addr - p.range.first) as usize..]))
	}
	
	pub fn get_mut(&mut self, addr: u32, search: Search) -> Option<(MemoryRange, &mut [u8])>
	{
		self.locate(addr, search).map(|i| &mut self.parts[i]).map(|p| (p.range, &mut p.data[(addr - p.range.first) as usize..]))
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
				None => None,
				Some(idx) =>
				{
					if self.parts[idx].range.last >= addr.saturating_sub(1) {Some(idx)} else {None}
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
