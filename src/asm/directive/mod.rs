use core::fmt;
use std::collections::HashMap;
use std::error::Error;

use crate::asm::{Context, ErrorLevel};
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
	ArgumentCount{min: Option<usize>, max: Option<usize>, have: usize},
	Argument{idx: usize, expect: ArgumentType, have: ArgumentType},
	Apply{name: String, source: Box<dyn Error + 'static>},
}

impl fmt::Display for DirectiveErrorKind
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::NotFound(name) => write!(f, "no such directive \".{}\"", name.escape_debug()),
			Self::ArgumentCount{min, max, have} =>
			{
				f.write_str("invalid argument count (")?;
				match (*min, *max)
				{
					(None, None) => (),
					(None, Some(max)) => write!(f, "at most{max}, ")?,
					(Some(min), None) => write!(f, "at least {min}, ")?,
					(Some(min), Some(max)) => write!(f, "need {min} to {max}, ")?,
				}
				write!(f, "got {have})")
			},
			Self::Argument{idx, expect, have} => write!(f, "invalid argument #{idx} (expect {expect}, got {have})"),
			Self::Apply{name, ..} => write!(f, "failed to apply .{name}"),
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
