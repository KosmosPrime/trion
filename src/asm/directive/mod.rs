use core::fmt;
use std::collections::HashMap;
use std::error::Error;

use crate::asm::{Context, ErrorLevel};
use crate::text::parse::choice::ArgChoice;
use crate::text::Positioned;
use crate::text::parse::{Argument, ArgumentType};

pub mod addr;
pub mod align;
pub mod constant;
pub mod data;
pub mod global;
pub mod include;

#[derive(Debug)]
pub struct DirectiveList(HashMap<String, Box<dyn Directive>>);

impl DirectiveList
{
	pub fn new() -> Self
	{
		Self(HashMap::new())
	}
	
	pub fn generate() -> Self
	{
		let mut list = Self::new();
		list.register(Box::new(addr::Addr)).unwrap();
		list.register(Box::new(align::Align)).unwrap();
		list.register(Box::new(constant::Const)).unwrap();
		list.register(Box::new(data::DataU8)).unwrap();
		list.register(Box::new(data::DataU16)).unwrap();
		list.register(Box::new(data::DataU32)).unwrap();
		list.register(Box::new(data::DataHex)).unwrap();
		list.register(Box::new(data::DataStr)).unwrap();
		list.register(Box::new(data::DataFile)).unwrap();
		list.register(Box::new(global::Global::Global)).unwrap();
		list.register(Box::new(global::Global::Import)).unwrap();
		list.register(Box::new(global::Global::Export)).unwrap();
		list.register(Box::new(include::Include)).unwrap();
		list
	}
	
	pub fn register(&mut self, directive: Box<dyn Directive>) -> Result<(), Box<dyn Directive>>
	{
		let name = directive.get_name().to_owned();
		match self.0.insert(name, directive)
		{
			None => Ok(()),
			Some(prev) => Err(prev),
		}
	}
	
	pub fn process<'c>(&self, ctx: &'c mut Context, contents: Positioned<(&str, Vec<Argument>)>) -> Result<(), ErrorLevel>
	{
		match self.0.get(contents.value.0)
		{
			None =>
			{
				ctx.push_error(contents.convert_fn(|v| DirectiveErrorKind::NotFound(v.0.to_owned())));
				Err(ErrorLevel::Fatal)
			},
			Some(directive) => directive.apply(ctx, contents.convert_fn(|v| v.1)),
		}
	}
}

pub trait Directive: fmt::Debug
{
	fn get_name(&self) -> &str;
	
	fn apply(&self, ctx: & mut Context, args: Positioned<Vec<Argument>>) -> Result<(), ErrorLevel>;
}

pub type DirectiveError = Positioned<DirectiveErrorKind>;

#[derive(Debug)]
pub enum DirectiveErrorKind
{
	NotFound(String),
	TooManyArguments{dir: String, max: usize, have: usize},
	NotEnoughArguments{dir: String, need: usize, have: usize},
	ArgumentType{dir: String, idx: usize, expect: ArgChoice, have: ArgumentType},
	Apply{dir: String, source: Box<dyn Error + 'static>},
}

impl fmt::Display for DirectiveErrorKind
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::NotFound(dir) => write!(f, "no such directive \".{}\"", dir.escape_debug()),
			Self::TooManyArguments{dir, max, have} => write!(f, "too many arguments for \".{}\" (max {max}, have {have})", dir.escape_debug()),
			Self::NotEnoughArguments{dir, need, have} => write!(f, "not enough arguments for\".{}\" (need {need}, have {have})", dir.escape_debug()),
			Self::ArgumentType{dir, idx, expect, have} =>
			{
				match expect.size()
				{
					0 => write!(f, "invalid argument #{idx} to \".{}\" (got {have})", dir.escape_debug()),
					1 => write!(f, "invalid argument #{idx} to \".{}\" (expect {expect}, got {have})", dir.escape_debug()),
					_ => write!(f, "invalid argument #{idx} to \".{}\" (expect one of {{{expect}}}; got {have})", dir.escape_debug()),
				}
			},
			Self::Apply{dir, ..} => write!(f, "failed to apply .{dir}"),
		}
	}
}

impl Error for DirectiveErrorKind
{
	fn source(&self) -> Option<&(dyn Error + 'static)>
	{
		match self
		{
			Self::Apply{source, ..} => Some(source.as_ref()),
			_ => None,
		}
	}
}
