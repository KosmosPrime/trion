use core::fmt;
use std::collections::HashMap;
use std::error::Error;

use crate::asm::Context;
use crate::text::Positioned;
use crate::text::parse::{Argument, ArgumentType};

#[derive(Debug)]
pub struct DirectiveList(HashMap<String, Box<dyn Directive>>);

impl DirectiveList
{
	pub fn new() -> Self
	{
		Self(HashMap::new())
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
	
	pub fn process<'c>(&self, ctx: &'c mut Context, contents: Positioned<(&str, &[Argument])>) -> Result<(), &'c DirectiveError>
	{
		match self.0.get(contents.value.0)
		{
			None => Err(ctx.push_error(contents.convert_fn(|v| DirectiveErrorKind::NotFound(v.0.to_owned())))),
			Some(directive) => directive.apply(ctx, contents.convert_fn(|v| v.1)),
		}
	}
}

pub trait Directive: fmt::Debug
{
	fn get_name(&self) -> &str;
	
	fn apply<'c>(&self, ctx: &'c mut Context, args: Positioned<&[Argument]>) -> Result<(), &'c DirectiveError>;
}

pub type DirectiveError = Positioned<DirectiveErrorKind>;

#[derive(Debug)]
pub enum DirectiveErrorKind
{
	NotFound(String),
	ArgumentCount{min: Option<usize>, max: Option<usize>, have: usize},
	Argument{idx: usize, expect: ArgumentType, have: ArgumentType},
	Apply(Box<dyn Error + 'static>),
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
			Self::Apply(..) => f.write_str("directive application failed"),
		}
	}
}

impl Error for DirectiveErrorKind
{
	fn source(&self) -> Option<&(dyn Error + 'static)>
	{
		match self
		{
			Self::Apply(source) => Some(source.as_ref()),
			_ => None,
		}
	}
}
