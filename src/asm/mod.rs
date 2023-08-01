use core::fmt;
use std::collections::HashMap;
use std::error::Error;
use std::path::{Path, PathBuf};

use crate::asm::mem::map::{MemoryMap, PutError, Search};

pub mod directive;
pub mod mem;

pub struct Context
{
	base_path: PathBuf,
	output: MemoryMap,
	active: Segment,
	labels: HashMap<String, u32>,
	tasks: Vec<Box<dyn FnOnce(&mut Context)>>,
	errors: Vec<Box<dyn Error + 'static>>,
}

impl Context
{
	pub fn new(base_path: PathBuf) -> Self
	{
		Self
		{
			base_path,
			output: MemoryMap::new(),
			active: Segment::Inactive(Vec::new()),
			labels: HashMap::new(),
			tasks: Vec::new(),
			errors: Vec::new(),
		}
	}
	
	pub fn base_path(&self) -> &Path
	{
		self.base_path.as_ref()
	}
	
	pub fn output(&self) -> &MemoryMap
	{
		&self.output
	}
	
	pub fn output_mut(&mut self) -> &mut MemoryMap
	{
		&mut self.output
	}
	
	pub fn active(&self) -> Option<&ActiveSegment>
	{
		match self.active
		{
			Segment::Empty => unreachable!("empty segment wrapper"),
			Segment::Inactive(..) => None,
			Segment::Active(ref seg) => Some(seg),
		}
	}
	
	pub fn active_mut(&mut self) -> Option<&mut ActiveSegment>
	{
		match self.active
		{
			Segment::Empty => unreachable!("empty segment wrapper"),
			Segment::Inactive(..) => None,
			Segment::Active(ref mut seg) => Some(seg),
		}
	}
	
	pub fn curr_addr(&self) -> Option<u32>
	{
		self.active().map(ActiveSegment::curr_addr)
	}
	
	pub fn change_segment(&mut self, addr: u32) -> Result<bool, SegmentError>
	{
		match self.active
		{
			Segment::Empty => unreachable!("empty segment wrapper"),
			Segment::Active(ref seg) =>
			{
				if addr == seg.base_addr {return Ok(false);}
				self.close_segment()?;
			},
			Segment::Inactive(..) => (),
		}
		let next = self.output.find(addr, Search::Above).map(|r| r.get_first());
		self.active.make_active(addr, next);
		Ok(true)
	}
	
	pub fn close_segment(&mut self) -> Result<bool, SegmentError>
	{
		match self.active
		{
			Segment::Empty => unreachable!("empty segment wrapper"),
			Segment::Active(ref seg) =>
			{
				match self.output.put(seg.base_addr, seg.buffer.as_ref())
				{
					Ok(n) => assert_eq!(n, seg.buffer.len()),
					Err(e) => return Err(SegmentError::Write(e)),
				}
				self.active.make_inactive();
				Ok(true)
			},
			_ => Ok(false),
		}
	}
	
	pub fn labels(&self) -> &HashMap<String, u32>
	{
		&self.labels
	}
	
	pub fn labels_mut(&mut self) -> &mut HashMap<String, u32>
	{
		&mut self.labels
	}
	
	pub fn add_task(&mut self, task: Box<dyn FnOnce(&mut Context)>)
	{
		self.tasks.push(task)
	}
	
	pub fn run_tasks(&mut self)
	{
		for task in core::mem::replace(&mut self.tasks, Vec::new())
		{
			task(self);
		}
	}
	
	pub fn has_errored(&self) -> bool
	{
		!self.errors.is_empty()
	}
	
	pub fn push_error<T: Error + 'static>(&mut self, error: T) -> &T
	{
		self.errors.push(Box::new(error));
		self.errors.last().unwrap().as_ref().downcast_ref().unwrap()
	}
	
	pub fn get_errors(&self) -> &[Box<dyn Error>]
	{
		self.errors.as_ref()
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SegmentError
{
	Write(PutError),
	Occupied(u32),
}

impl fmt::Display for SegmentError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Write(..) => f.write_str("could not write complete segment"),
			Self::Occupied(addr) => write!(f, "address {addr:08X} is already occupied"),
		}
	}
}

impl Error for SegmentError
{
	fn source(&self) -> Option<&(dyn Error + 'static)>
	{
		match self
		{
			Self::Write(e) => Some(e),
			_ => None,
		}
	}
}

#[derive(Debug, Clone)]
enum Segment
{
	Empty,
	Inactive(Vec<u8>), // to avoid re-allocating
	Active(ActiveSegment),
}

impl Segment
{
	pub fn make_active(&mut self, addr: u32, next: Option<u32>)
	{
		let Segment::Inactive(buffer) = core::mem::replace(self, Segment::Empty) else {panic!("segment not inactive");};
		let max_len = match next
		{
			None => usize::try_from(u32::MAX - addr).unwrap_or(usize::MAX).saturating_add(1),
			Some(next) => usize::try_from(next - addr).unwrap_or(usize::MAX),
		};
		*self = Segment::Active(ActiveSegment{base_addr: addr, buffer, max_len})
	}
	
	pub fn make_inactive(&mut self)
	{
		let Segment::Active(mut seg) = core::mem::replace(self, Segment::Empty) else {panic!("segment not active");};
		seg.buffer.clear();
		*self = Segment::Inactive(seg.buffer);
	}
}

#[derive(Debug, Clone)]
pub struct ActiveSegment
{
	base_addr: u32,
	buffer: Vec<u8>,
	max_len: usize,
}

impl ActiveSegment
{
	pub fn base_addr(&self) -> u32
	{
		self.base_addr
	}
	
	pub fn curr_addr(&self) -> u32
	{
		// for segments that go up to the end of address space
		self.base_addr.saturating_add(self.buffer.len() as u32)
	}
	
	pub fn remaining(&self) -> usize
	{
		self.max_len - self.buffer.len()
	}
	
	pub fn has_remaining(&self, len: usize) -> bool
	{
		len <= self.remaining()
	}
	
	pub fn write(&mut self, data: &[u8]) -> Result<(), WriteError>
	{
		if self.has_remaining(data.len())
		{
			self.buffer.extend_from_slice(data);
			Ok(())
		}
		else
		{
			Err(WriteError::Overflow{need: data.len(), have: self.remaining()})
		}
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum WriteError
{
	Overflow{need: usize, have: usize},
}

impl fmt::Display for WriteError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Overflow{need, have} => write!(f, "segment overflow (need {need}, capacity {have})"),
		}
	}
}

impl Error for WriteError {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LabelError
{
	NotFound(String),
	Range{min: i64, max: i64, have: i64},
	Alignment{align: u32, have: i64},
}

impl fmt::Display for LabelError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::NotFound(name) => write!(f, "no such label {:?}", name),
			Self::Range{min, max, have} => write!(f, "label out of range ({min} to {max}, got {have})"),
			Self::Alignment{align, have} => write!(f, "misaligned label (expect {align}, got {have})"),
		}
	}
}

impl Error for LabelError {}
