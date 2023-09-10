use core::fmt;
use core::num::NonZeroUsize;
use std::collections::HashMap;
use std::error::Error;
use std::path::{Path, PathBuf};

use crate::asm::directive::DirectiveList;
use crate::asm::instr::{InstructionSet, InstructionError};
use crate::asm::mem::map::{MemoryMap, PutError, Search};
use crate::text::Positioned;
use crate::text::parse::{Argument, ElementValue, Parser, ParseErrorKind};

pub mod directive;
pub mod instr;
pub mod mem;

pub struct Context<'l>
{
	path_stack: Vec<PathBuf>,
	instructions: &'l dyn InstructionSet,
	directives: &'l DirectiveList,
	output: MemoryMap,
	active: Segment,
	labels: HashMap<String, u32>,
	tasks: Vec<Box<dyn FnOnce(&mut Context)>>,
	errors: Vec<Box<dyn Error + 'static>>,
}

impl<'l> Context<'l>
{
	pub fn new(instructions: &'l dyn InstructionSet, directives: &'l DirectiveList) -> Self
	{
		Self
		{
			path_stack: Vec::new(),
			instructions,
			directives,
			output: MemoryMap::new(),
			active: Segment::Inactive(Vec::new()),
			labels: HashMap::new(),
			tasks: Vec::new(),
			errors: Vec::new(),
		}
	}
	
	pub fn curr_path(&self) -> Option<&Path>
	{
		self.path_stack.last().map(PathBuf::as_path)
	}
	
	pub fn get_instruction_set(&self) -> &'l dyn InstructionSet
	{
		self.instructions
	}
	
	pub fn get_directives(&self) -> &'l DirectiveList
	{
		self.directives
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
	
	fn do_assemble<'s>(&'s mut self, data: &[u8]) -> bool
	{
		for element in Parser::new(data)
		{
			let element = match element
			{
				Ok(el) => el,
				Err(e) =>
				{
					self.push_error(e);
					return false;
				},
			};
			
			match element.value
			{
				ElementValue::Directive{name, args} =>
				{
					if self.directives.process(self, Positioned{value: (name, args.as_slice()), line: element.line, col: element.col}).is_err()
					{
						return false;
					}
				},
				ElementValue::Label(name) =>
				{
					let curr_addr = match self.active()
					{
						None =>
						{
							self.push_error(element.convert(AsmErrorKind::Inactive));
							return false;
						},
						Some(seg) => seg.curr_addr(),
					};
					let prev = self.labels.insert(name.to_owned(), curr_addr);
					if prev.is_some()
					{
						self.push_error(element.convert(AsmErrorKind::DuplicateLabel(name.to_owned())));
						return false;
					}
				},
				ElementValue::Instruction{name, args} =>
				{
					if self.active().is_none()
					{
						self.push_error(Positioned{value: AsmErrorKind::Inactive, line: element.line, col: element.line});
						return false;
					}
					if self.assemble_instr(element.line, element.col, name, args).is_err()
					{
						return false;
					}
				},
			}
		}
		true
	}
	
	pub fn assemble<'s>(&'s mut self, data: &[u8], path: PathBuf) -> (bool, PathBuf)
	{
		self.path_stack.push(path);
		let count = NonZeroUsize::try_from(self.path_stack.len()).unwrap();
		let frame = PathFrame{ctx: self, count: Some(count)};
		let result = frame.ctx.do_assemble(data);
		(result, frame.into_inner())
	}
	
	pub fn assemble_instr<'s>(&'s mut self, line: u32, col: u32, name: &str, args: Vec<Argument>) -> Result<(), &'s InstructionError>
	{
		self.instructions.assemble(self, line, col, name, args)
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

struct PathFrame<'l, 'c: 'l>
{
	ctx: &'l mut Context<'c>,
	count: Option<NonZeroUsize>
}

impl<'l, 'c: 'l> PathFrame<'l, 'c>
{
	pub fn into_inner(self) -> PathBuf
	{
		let count = self.count.unwrap();
		assert!(self.ctx.path_stack.len() == count.get());
		let buff = self.ctx.path_stack.pop().unwrap();
		// this replaces drop as this call takes the value back out
		core::mem::forget(self);
		buff
	}
}

impl<'l, 'c: 'l> Drop for PathFrame<'l, 'c>
{
	fn drop(&mut self)
	{
		if let Some(count) = self.count
		{
			assert!(self.ctx.path_stack.len() == count.get());
			self.ctx.path_stack.pop();
		}
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SegmentError
{
	Write(PutError),
	Occupied(u32),
	Overflow{need: usize, have: usize},
}

impl fmt::Display for SegmentError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Write(..) => f.write_str("could not write complete segment"),
			Self::Occupied(addr) => write!(f, "address {addr:08X} is already occupied"),
			Self::Overflow{need, have} => write!(f, "segment overflow (need {need}, capacity {have})"),
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
	
	pub fn write(&mut self, data: &[u8]) -> Result<(), SegmentError>
	{
		if self.has_remaining(data.len())
		{
			self.buffer.extend_from_slice(data);
			Ok(())
		}
		else
		{
			Err(SegmentError::Overflow{need: data.len(), have: self.remaining()})
		}
	}
}

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
			Self::NotFound(name) => write!(f, "no such label {name:?}"),
			Self::Range{min, max, have} => write!(f, "label out of range ({min} to {max}, got {have})"),
			Self::Alignment{align, have} => write!(f, "misaligned label (expect {align}, got {have})"),
		}
	}
}

impl Error for LabelError {}

pub type AssembleError = Positioned<AsmErrorKind>;

#[derive(Debug)]
pub enum AsmErrorKind
{
	Parse(ParseErrorKind),
	Inactive,
	DuplicateLabel(String),
}

impl fmt::Display for AsmErrorKind
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Parse(..) => f.write_str("parsing failed"),
			Self::Inactive => f.write_str("no active segment"),
			Self::DuplicateLabel(name) => write!(f, "duplicate label {name:?}"),
		}
	}
}

impl Error for AsmErrorKind
{
	fn source(&self) -> Option<&(dyn Error + 'static)>
	{
		match self
		{
			Self::Parse(e) => Some(e),
			_ => None,
		}
	}
}
