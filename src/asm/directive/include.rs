use core::fmt;
use std::error::Error;
use std::fs::OpenOptions;
use std::io::{self, Read};
use std::path::PathBuf;

use crate::asm::{Context, ErrorLevel};
use crate::asm::directive::{Directive, DirectiveErrorKind};
use crate::text::Positioned;
use crate::text::parse::{Argument, ArgumentType};

#[derive(Clone, Copy, Debug)]
pub struct Include;

impl Directive for Include
{
	fn get_name(&self) -> &str
	{
		"include"
	}
	
	fn apply(&self, ctx: & mut Context, args: Positioned<Vec<Argument>>) -> Result<(), ErrorLevel>
	{
		if args.value.len() != 1
		{
			ctx.push_error(args.convert_fn(|v| DirectiveErrorKind::ArgumentCount{min: Some(1), max: Some(1), have: v.len()}));
			return Err(ErrorLevel::Trivial);
		}
		let path = match args.value[0]
		{
			Argument::String(ref path) =>
			{
				let mut curr = match ctx.curr_path()
				{
					None => PathBuf::new(),
					Some(prev) => prev.to_path_buf(),
				};
				if !curr.pop() {curr.push("..");}
				curr.push(path.as_ref());
				curr
			},
			ref arg =>
			{
				ctx.push_error(args.convert(DirectiveErrorKind::Argument{idx: 0, expect: ArgumentType::String, have: arg.get_type()}));
				return Err(ErrorLevel::Trivial);
			},
		};
		let mut f = match OpenOptions::new().read(true).open(&path)
		{
			Ok(f) => f,
			Err(err) =>
			{
				let source = Box::new(IncludeError::NoSuchFile{path, err});
				ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
				return Err(ErrorLevel::Fatal);
			},
		};
		let mut data = Vec::new();
		if let Err(err) = f.read_to_end(&mut data)
		{
			let source = Box::new(IncludeError::FileRead{path, err});
			ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
			return Err(ErrorLevel::Fatal);
		}
		if let (Err(..), path) = ctx.assemble(data.as_ref(), path)
		{
			let source = Box::new(IncludeError::AssemblyFailed{path});
			ctx.push_error(args.convert(DirectiveErrorKind::Apply{name: self.get_name().to_owned(), source}));
			return Err(ErrorLevel::Fatal);
		}
		Ok(())
	}
}

#[derive(Debug)]
pub enum IncludeError
{
	NoSuchFile{path: PathBuf, err: io::Error},
	FileRead{path: PathBuf, err: io::Error},
	AssemblyFailed{path: PathBuf}, // TODO should reference the original error
}

impl fmt::Display for IncludeError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::NoSuchFile{path, ..} => write!(f, "no such file {}", path.display()),
			Self::FileRead{path, ..} => write!(f, "could not read file {}", path.display()),
			Self::AssemblyFailed{path} => write!(f, "assembly of {} failed", path.display()),
		}
	}
}

impl Error for IncludeError
{
	fn source(&self) -> Option<&(dyn Error + 'static)>
	{
		match self
		{
			Self::NoSuchFile{err, ..} => Some(err),
			Self::FileRead{err, ..} => Some(err),
			_ => None,
		}
	}
}
