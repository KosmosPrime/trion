use core::fmt;
use std::error::Error;

use crate::asm::{ConstantError, Context, ErrorLevel};
use crate::asm::constant::{Lookup, Realm};
use crate::asm::directive::{Directive, DirectiveErrorKind};
use crate::text::Positioned;
use crate::text::parse::{Argument, ArgumentType};
use crate::text::parse::choice::ArgChoice;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Global
{
	Global, Import, Export,
}

impl Directive for Global
{
	fn get_name(&self) -> &str
	{
		match self
		{
			Self::Global => "global",
			Self::Import => "import",
			Self::Export => "export",
		}
	}
	
	fn apply(&self, ctx: & mut Context, args: Positioned<Vec<Argument>>) -> Result<(), ErrorLevel>
	{
		const NUM_ARGS: usize = 1;
		if args.value.len() != NUM_ARGS
		{
			let dir = self.get_name().to_owned();
			ctx.push_error(args.convert(
				if args.value.len() < NUM_ARGS {DirectiveErrorKind::NotEnoughArguments{dir, need: NUM_ARGS, have: args.value.len()}}
				else {DirectiveErrorKind::TooManyArguments{dir, max: NUM_ARGS, have: args.value.len()}}
			));
			return Err(ErrorLevel::Trivial);
		}
		let name = match args.value[0]
		{
			Argument::Identifier(ref n) => n.as_ref(),
			ref arg =>
			{
				let dir = self.get_name().to_owned();
				ctx.push_error(args.convert(DirectiveErrorKind::ArgumentType{dir, idx: 0, expect: ArgChoice::of(ArgumentType::String), have: arg.get_type()}));
				return Err(ErrorLevel::Trivial);
			},
		};
		match self
		{
			Self::Global =>
			{
				match ctx.defer_constant(name, Realm::Global)
				{
					Ok(()) => (),
					Err(ConstantError::Duplicate{name, realm}) =>
					{
						let source = Box::new(GlobalError::Duplicate{name, realm});
						ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
						return Err(ErrorLevel::Fatal);
					},
					Err(e) => unreachable!("{e:?}"),
				}
				let curr_local = ctx.get_constant(name, Realm::Local);
				match curr_local
				{
					Lookup::NotFound | Lookup::Deferred =>
					{
						if matches!(curr_local, Lookup::NotFound)
						{
							ctx.defer_constant(name, Realm::Local).unwrap();
						}
						let name = name.to_owned();
						let (line, col) = (args.line, args.col);
						let this = *self;
						ctx.add_task(Box::new(move |ctx|
						{
							let value = match ctx.get_constant(name.as_str(), Realm::Local)
							{
								Lookup::NotFound =>
								{
									let source = Box::new(GlobalError::NotFound{name, realm: Realm::Local});
									ctx.push_error(Positioned{line, col, value: DirectiveErrorKind::Apply{dir: this.get_name().to_owned(), source}});
									return Err(ErrorLevel::Trivial);
								},
								Lookup::Deferred =>
								{
									let source = Box::new(GlobalError::Deferred{name, realm: Realm::Local});
									ctx.push_error(Positioned{line, col, value: DirectiveErrorKind::Apply{dir: this.get_name().to_owned(), source}});
									return Err(ErrorLevel::Trivial);
								},
								Lookup::Found(v) => v,
							};
							match ctx.insert_constant(name.as_str(), value, Realm::Global)
							{
								Ok(..) => Ok(()),
								Err(ConstantError::Duplicate{name, realm}) =>
								{
									let source = Box::new(GlobalError::Duplicate{name, realm});
									ctx.push_error(Positioned{line, col, value: DirectiveErrorKind::Apply{dir: this.get_name().to_owned(), source}});
									Err(ErrorLevel::Trivial)
								},
								Err(e) => unreachable!("{e:?}"),
							}
						}), Realm::Local);
					},
					Lookup::Found(v) =>
					{
						// must return false since we've deferred it above
						assert!(!ctx.insert_constant(name, v, Realm::Global).unwrap());
					},
				}
			},
			Self::Import | Self::Export =>
			{
				let src_realm = if *self == Self::Export {Realm::Local} else {Realm::Global};
				let dst_realm = if *self == Self::Export {Realm::Global} else {Realm::Local};
				let value = match ctx.get_constant(name, src_realm)
				{
					Lookup::NotFound =>
					{
						let source = Box::new(GlobalError::NotFound{name: name.to_owned(), realm: src_realm});
						ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
						return Err(ErrorLevel::Fatal);
					},
					Lookup::Deferred =>
					{
						if *self == Self::Import
						{
							match ctx.defer_constant(name, dst_realm)
							{
								Ok(()) => return Ok(()),
								Err(ConstantError::Duplicate{name, realm}) =>
								{
									let source = Box::new(GlobalError::Duplicate{name, realm});
									ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
									return Err(ErrorLevel::Fatal);
								},
								Err(e) => unreachable!("{e:?}"),
							}
						}
						else
						{
							let source = Box::new(GlobalError::Deferred{name: name.to_owned(), realm: src_realm});
							ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
							return Err(ErrorLevel::Fatal);
						}
					},
					Lookup::Found(v) => v,
				};
				match ctx.insert_constant(name, value, dst_realm)
				{
					Ok(..) => (),
					Err(ConstantError::Duplicate{name, realm}) =>
					{
						let source = Box::new(GlobalError::Duplicate{name, realm});
						ctx.push_error(args.convert(DirectiveErrorKind::Apply{dir: self.get_name().to_owned(), source}));
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
