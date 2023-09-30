use core::fmt;
use core::num::NonZeroUsize;
use std::collections::HashMap;
use std::error::Error;
use std::path::{Path, PathBuf};

use crate::asm::constant::{Lookup, Realm};
use crate::asm::directive::DirectiveList;
use crate::asm::instr::InstructionSet;
use crate::asm::mem::map::{MemoryMap, PutError, Search};
use crate::text::{Positioned, PosNamed};
use crate::text::parse::{Argument, ElementValue, Parser, ParseErrorKind};

pub mod constant;
pub mod directive;
pub mod instr;
pub mod mem;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ErrorLevel
{
	Trivial, Fatal,
}

pub struct Context<'l>
{
	path_stack: Vec<PathBuf>,
	instructions: &'l dyn InstructionSet,
	directives: &'l DirectiveList,
	output: MemoryMap,
	active: Segment,
	globals: HashMap<String, Option<i64>>,
	locals: Option<HashMap<String, Option<i64>>>,
	global_tasks: Vec<Box<dyn FnOnce(&mut Context)>>,
	local_tasks: Option<Vec<Box<dyn FnOnce(&mut Context)>>>,
	errors: Vec<Box<PosNamed<dyn Error>>>,
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
			globals: HashMap::new(),
			locals: None,
			global_tasks: Vec::new(),
			local_tasks: None,
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
	
	pub fn get_constant(&self, name: &str, realm: Realm) -> Lookup
	{
		let constants = match realm
		{
			Realm::Global => &self.globals,
			Realm::Local =>
			{
				match self.locals
				{
					None => panic!("no local scope"),
					Some(ref l) => l,
				}
			},
		};
		match constants.get(name)
		{
			None => Lookup::NotFound,
			Some(None) => Lookup::Deferred,
			Some(&Some(v)) => Lookup::Found(v),
		}
	}
	
	pub fn insert_constant(&mut self, name: &str, value: i64, realm: Realm) -> Result<bool, ConstantError>
	{
		let constants = match realm
		{
			Realm::Global => &mut self.globals,
			Realm::Local =>
			{
				match self.locals
				{
					None => panic!("no local scope"),
					Some(ref mut l) => l,
				}
			},
		};
		match constants.get_mut(name)
		{
			None =>
			{
				constants.insert(name.to_owned(), Some(value));
				Ok(true)
			},
			Some(dst @ None) =>
			{
				*dst = Some(value);
				Ok(false)
			},
			Some(Some(..)) => Err(ConstantError::Duplicate{name: name.to_owned(), realm}),
		}
	}
	
	pub fn replace_constant(&mut self, name: &str, value: i64, realm: Realm) -> Lookup
	{
		let constants = match realm
		{
			Realm::Global => &mut self.globals,
			Realm::Local =>
			{
				match self.locals
				{
					None => panic!("no local scope"),
					Some(ref mut l) => l,
				}
			},
		};
		match constants.get_mut(name)
		{
			None =>
			{
				constants.insert(name.to_owned(), Some(value));
				Lookup::NotFound
			},
			Some(dst @ None) =>
			{
				*dst = Some(value);
				Lookup::Deferred
			},
			Some(Some(have)) =>
			{
				let prev = *have;
				*have = value;
				Lookup::Found(prev)
			},
		}
	}
	
	fn do_assemble<'s>(&'s mut self, data: &[u8]) -> Result<(), ErrorLevel>
	{
		for element in Parser::new(data)
		{
			let element = match element
			{
				Ok(el) => el,
				Err(e) =>
				{
					self.push_error(e);
					return Err(ErrorLevel::Fatal);
				},
			};
			
			match element.value
			{
				ElementValue::Directive{name, args} =>
				{
					if let Err(e) = self.directives.process(self, Positioned{line: element.line, col: element.col, value: (name, args.as_slice())})
					{
						return Err(e);
					}
				},
				ElementValue::Label(name) =>
				{
					let curr_addr = match self.active()
					{
						None =>
						{
							self.push_error(element.convert(AsmErrorKind::Inactive));
							return Err(ErrorLevel::Fatal);
						},
						Some(seg) => seg.curr_addr(),
					};
					if let Err(e) = self.insert_constant(name, i64::from(curr_addr), Realm::Local)
					{
						self.push_error(element.convert(e));
						return Err(ErrorLevel::Fatal);
					}
				},
				ElementValue::Instruction{name, args} =>
				{
					if self.active().is_none()
					{
						self.push_error(Positioned{line: element.line, col: element.line, value: AsmErrorKind::Inactive});
						return Err(ErrorLevel::Fatal);
					}
					if let Err(e) = self.assemble_instr(element.line, element.col, name, args)
					{
						return Err(e);
					}
				},
			}
		}
		Ok(())
	}
	
	pub fn assemble<'s>(&'s mut self, data: &[u8], path: PathBuf) -> (Result<(), ErrorLevel>, PathBuf)
	{
		self.path_stack.push(path);
		let count = NonZeroUsize::try_from(self.path_stack.len()).unwrap();
		let constants = core::mem::replace(&mut self.locals, Some(HashMap::new())).map(|c| core::mem::replace(&mut self.globals, c));
		let tasks = core::mem::replace(&mut self.local_tasks, Some(Vec::new())).map(|t| core::mem::replace(&mut self.global_tasks, t));
		let mut frame = PathFrame{ctx: self, count: Some(count), constants, tasks};
		let result = frame.ctx.do_assemble(data);
		if result != Err(ErrorLevel::Fatal)
		{
			let mut tasks = frame.ctx.local_tasks.replace(Vec::new()).unwrap();
			while !tasks.is_empty()
			{
				for task in tasks.drain(..)
				{
					task(&mut frame.ctx);
				}
				core::mem::swap(frame.ctx.local_tasks.as_mut().unwrap(), &mut tasks);
			}
		}
		(result, frame.into_inner())
	}
	
	pub fn assemble_instr<'s>(&'s mut self, line: u32, col: u32, name: &str, args: Vec<Argument>) -> Result<(), ErrorLevel>
	{
		self.instructions.assemble(self, line, col, name, args)
	}
	
	pub fn add_task(&mut self, task: Box<dyn FnOnce(&mut Context)>, realm: Realm)
	{
		let tasks = match realm
		{
			Realm::Global => &mut self.global_tasks,
			Realm::Local =>
			{
				match self.local_tasks
				{
					None => panic!("no local scope"),
					Some(ref mut l) => l,
				}
			},
		};
		tasks.push(task);
	}
	
	pub fn finalize(&mut self) -> bool
	{
		if !self.has_errored()
		{
			for task in core::mem::replace(&mut self.global_tasks, Vec::new())
			{
				task(self);
			}
		}
		!self.has_errored()
	}
	
	pub fn has_errored(&self) -> bool
	{
		!self.errors.is_empty()
	}
	
	pub fn get_errors(&self) -> &[Box<PosNamed<dyn Error>>]
	{
		self.errors.as_ref()
	}
	
	pub fn push_error<T: Error + 'static>(&mut self, error: Positioned<T>)
	{
		let name = self.path_stack.last().map_or_else(String::new, |p| p.to_string_lossy().into_owned());
		self.errors.push(Box::new(error.with_name(name)));
	}
	
	pub fn push_error_in<T: Error + 'static>(&mut self, error: PosNamed<T>)
	{
		self.errors.push(Box::new(error));
	}
}

struct PathFrame<'l, 'c: 'l>
{
	ctx: &'l mut Context<'c>,
	count: Option<NonZeroUsize>,
	constants: Option<HashMap<String, Option<i64>>>,
	tasks: Option<Vec<Box<dyn FnOnce(&mut Context)>>>,
}

impl<'l, 'c: 'l> PathFrame<'l, 'c>
{
	pub fn into_inner(mut self) -> PathBuf
	{
		let count = self.count.unwrap();
		assert_eq!(self.ctx.path_stack.len(), count.get());
		let buff = self.ctx.path_stack.pop().unwrap();
		self.ctx.locals = self.constants.take().map(|c| core::mem::replace(&mut self.ctx.globals, c));
		self.ctx.local_tasks = self.tasks.take().map(|t| core::mem::replace(&mut self.ctx.global_tasks, t));
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
			self.ctx.locals = self.constants.take().map(|c| core::mem::replace(&mut self.ctx.globals, c));
			self.ctx.local_tasks = self.tasks.take().map(|t| core::mem::replace(&mut self.ctx.global_tasks, t));
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
	
	pub fn write_at(&mut self, addr: u32, data: &[u8]) -> Result<(), SegmentError>
	{
		assert!(addr >= self.base_addr && addr <= self.curr_addr());
		// safe cast because `Self::curr_addr` uses the buffer's length (usize)
		let start = (addr - self.base_addr) as usize;
		if self.buffer.len() - start < data.len()
		{
			let overwrite = self.buffer.len() - start;
			if !self.has_remaining(overwrite)
			{
				return Err(SegmentError::Overflow{need: data.len() - overwrite, have: self.remaining()});
			}
			// mixed overwrite & append, truncate to simplify into only appending
			self.buffer.truncate(start);
			self.buffer.extend_from_slice(data);
		}
		else if start < self.buffer.len()
		{
			// all contained within the buffer, overwrite only
			self.buffer[start..start + data.len()].copy_from_slice(data);
		}
		else
		{
			// all appended to the buffer, no overwrite
			self.buffer.extend_from_slice(data);
		}
		Ok(())
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ConstantError
{
	NotFound{name: String, realm: Realm},
	Duplicate{name: String, realm: Realm},
	Range{min: i64, max: i64, have: i64},
	Alignment{align: u32, have: i64},
}

impl fmt::Display for ConstantError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match *self
		{
			Self::NotFound{ref name, realm} => write!(f, "no such {realm} constant {name:?}"),
			Self::Duplicate{ref name, realm} => write!(f, "duplicate {realm} constant {name}"),
			Self::Range{min, max, have} => write!(f, "label out of range ({min} to {max}, got {have})"),
			Self::Alignment{align, have} => write!(f, "misaligned label (expect {align}, got {have})"),
		}
	}
}

impl Error for ConstantError {}

pub type AssembleError = Positioned<AsmErrorKind>;

#[derive(Debug)]
pub enum AsmErrorKind
{
	Parse(ParseErrorKind),
	Inactive,
}

impl fmt::Display for AsmErrorKind
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Parse(..) => f.write_str("parsing failed"),
			Self::Inactive => f.write_str("no active segment"),
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
