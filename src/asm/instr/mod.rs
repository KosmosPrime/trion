use core::fmt;
use std::error::Error;

use crate::asm::{Context, ErrorLevel, SegmentError};
use crate::text::Positioned;
use crate::text::parse::{Argument, ArgumentType};

pub trait InstructionSet
{
	fn is_register(&self, name: &str) -> bool;
	
	fn assemble<'c>(&self, ctx: &'c mut Context, line: u32, col: u32, name: &str, args: Vec<Argument>) -> Result<(), ErrorLevel>;
}

pub type InstructionError = Positioned<InstrErrorKind>;

#[derive(Debug)]
pub enum InstrErrorKind
{
	NoSuchInstruction(String),
	TooManyArguments{instr: String, need: usize, have: usize},
	NotEnoughArguments{instr: String, need: usize, have: usize},
	ArgumentType{instr: String, idx: usize, need: ArgumentType, have: ArgumentType},
	ValueRange{instr: String, idx: usize},
	NoSuchRegister{instr: String, idx: usize, what: String},
	Write(SegmentError),
	Generic(Box<dyn Error>),
}

impl fmt::Display for InstrErrorKind
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::NoSuchInstruction(name) => write!(f, "no such instruction {name:?}"),
			Self::TooManyArguments{instr, need, have} => write!(f, "too many arguments for {instr} (max {need}, have {have})"),
			Self::NotEnoughArguments{instr, need, have} => write!(f, "not enough arguments for {instr} (need {need}, have {have})"),
			Self::ArgumentType{instr, idx, need, have} => write!(f, "incorrect argument #{idx} for {instr} (expected {need}, got {have})"),
			Self::ValueRange{instr, idx} => write!(f, "argument #{idx} for {instr} is out of range"),
			Self::NoSuchRegister{instr, idx, what} => write!(f, "argument #{idx} for {instr} has invalid register {what:?}"),
			Self::Write(..) => f.write_str("could not write instruction"),
			Self::Generic(..) => f.write_str("instruction assembly failed"),
		}
	}
}

impl Error for InstrErrorKind
{
	fn source(&self) -> Option<&(dyn Error + 'static)>
	{
		match self
		{
			Self::Write(e) => Some(e),
			Self::Generic(e) => Some(e.as_ref()),
			_ => None,
		}
	}
}
