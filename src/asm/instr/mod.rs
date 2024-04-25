use core::fmt;
use std::error::Error;

use crate::asm::{Context, ErrorLevel};
use crate::text::Positioned;
use crate::text::parse::{Argument, ArgumentType};

pub trait InstructionSet
{
	fn is_register(&self, name: &str) -> bool;
	
	fn assemble(&self, ctx: &mut Context, line: u32, col: u32, name: &str, args: Vec<Argument>) -> Result<(), ErrorLevel>;
}

pub type InstructionError = Positioned<InstrErrorKind>;

#[derive(Debug)]
pub enum InstrErrorKind
{
	NotFound(String),
	TooManyArguments{instr: String, max: usize, have: usize},
	NotEnoughArguments{instr: String, need: usize, have: usize},
	ArgumentType{instr: String, idx: usize, expect: ArgumentType, have: ArgumentType},
	Assemble(Box<dyn Error>),
}

impl fmt::Display for InstrErrorKind
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::NotFound(name) => write!(f, "no such instruction {name:?}"),
			Self::TooManyArguments{instr, max, have} => write!(f, "too many arguments for {instr} (max {max}, have {have})"),
			Self::NotEnoughArguments{instr, need, have} => write!(f, "not enough arguments for {instr} (need {need}, have {have})"),
			Self::ArgumentType{instr, idx, expect, have} => write!(f, "incorrect argument #{idx} for {instr} (expected {expect}, got {have})"),
			Self::Assemble(..) => f.write_str("instruction assembly failed"),
		}
	}
}

impl Error for InstrErrorKind
{
	fn source(&self) -> Option<&(dyn Error + 'static)>
	{
		match self
		{
			Self::Assemble(e) => Some(e.as_ref()),
			_ => None,
		}
	}
}
