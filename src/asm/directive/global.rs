use core::fmt;
use std::error::Error;

use crate::asm::{ConstantError, Context, ErrorLevel};
use crate::asm::constant::{Lookup, Realm};
use crate::asm::directive::{Directive, DirectiveErrorKind};
use crate::text::Positioned;
use crate::text::parse::{Argument, ArgumentType};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Global
{
	Global, Import,
}

impl Directive for Global
{
	fn get_name(&self) -> &str
	{
		match self
		{
			Self::Global => "global",
			Self::Import => "import",
		}
	}
	
	fn apply<'c>(&self, ctx: &'c mut Context, args: Positioned<&[Argument]>) -> Result<(), ErrorLevel>
	{
		if args.value.len() != 1
		{
			ctx.push_error(args.convert_fn(|v| DirectiveErrorKind::ArgumentCount{min: Some(1), max: Some(1), have: v.len()}));
			return Err(ErrorLevel::Trivial);
		}
		let name = match args.value[0]
		{
			Argument::Identifier(n) => n,
			ref arg =>
			{
				ctx.push_error(args.convert(DirectiveErrorKind::Argument{idx: 0, expect: ArgumentType::String, have: arg.get_type()}));
				return Err(ErrorLevel::Trivial);
			},
		};
		match self
		{
			Self::Global =>
			{
				match ctx.get_constant(name, Realm::Local)
				{
					Lookup::Found(..) =>
					{
						let err = Box::new(GlobalError::Duplicate{name: name.to_owned(), realm: Realm::Local});
						ctx.push_error(args.convert(DirectiveErrorKind::Apply(err)));
						return Err(ErrorLevel::Fatal);
					},
					_ => (),
				}
				let name = name.to_owned();
				let (line, col) = (args.line, args.col);
				ctx.add_task(Box::new(move |ctx|
				{
					let value = match ctx.get_constant(name.as_str(), Realm::Local)
					{
						Lookup::NotFound =>
						{
							let err = Box::new(GlobalError::NotFound{name, realm: Realm::Local});
							ctx.push_error(Positioned{line, col, value: DirectiveErrorKind::Apply(err)});
							return;
						},
						Lookup::Deferred =>
						{
							let err = Box::new(GlobalError::Deferred{name, realm: Realm::Local});
							ctx.push_error(Positioned{line, col, value: DirectiveErrorKind::Apply(err)});
							return;
						},
						Lookup::Found(v) => v,
					};
					match ctx.insert_constant(name.as_str(), value, Realm::Global)
					{
						Ok(..) => (),
						Err(ConstantError::Duplicate{name, realm}) =>
						{
							let err = Box::new(GlobalError::Duplicate{name, realm});
							ctx.push_error(Positioned{line, col, value: DirectiveErrorKind::Apply(err)});
						},
						Err(e) => unreachable!("{e:?}"),
					}
				}), Realm::Local);
			},
			Self::Import =>
			{
				let value = match ctx.get_constant(name, Realm::Global)
				{
					Lookup::NotFound =>
					{
						let err = Box::new(GlobalError::NotFound{name: name.to_owned(), realm: Realm::Global});
						ctx.push_error(args.convert(DirectiveErrorKind::Apply(err)));
						return Err(ErrorLevel::Fatal);
					},
					Lookup::Deferred =>
					{
						let err = Box::new(GlobalError::Deferred{name: name.to_owned(), realm: Realm::Global});
						ctx.push_error(args.convert(DirectiveErrorKind::Apply(err)));
						return Err(ErrorLevel::Fatal);
					},
					Lookup::Found(v) => v,
				};
				match ctx.insert_constant(name, value, Realm::Local)
				{
					Ok(..) => (),
					Err(ConstantError::Duplicate{name, realm}) =>
					{
						let err = Box::new(GlobalError::Duplicate{name, realm});
						ctx.push_error(args.convert(DirectiveErrorKind::Apply(err)));
						return Err(ErrorLevel::Fatal);
					},
					Err(e) => unreachable!("{e:?}"),
				}
			},
		}
		Ok(())
	}
}

#[derive(Debug)]
pub enum GlobalError
{
	NotFound{name: String, realm: Realm},
	Deferred{name: String, realm: Realm},
	Duplicate{name: String, realm: Realm},
}

impl fmt::Display for GlobalError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			&Self::NotFound{ref name, realm} => write!(f, "no such {realm} constant {name:?}"),
			&Self::Deferred{ref name, realm} => write!(f, "declared {realm} constant {name:?} is deferred"),
			&Self::Duplicate{ref name, realm} => write!(f, "duplicate {realm} constant {name}"),
		}
	}
}

impl Error for GlobalError {}
