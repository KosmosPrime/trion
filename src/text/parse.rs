use core::fmt;
use core::iter::FusedIterator;
use std::borrow::Cow;
use std::error::Error;

use crate::text::Positioned;
use crate::text::token::{Number, Token, TokenError, Tokenizer, TokenValue};

// FIXME these should contain their own location info (or skip element parsing)
#[derive(Clone, Debug)]
pub enum Argument<'l>
{
	Constant(Number),
	Identifier(&'l str),
	String(Cow<'l, str>),
	Add(Vec<Argument<'l>>),
	Subtract(Vec<Argument<'l>>),
	Multiply(Vec<Argument<'l>>),
	Not(Box<Argument<'l>>),
	And(Vec<Argument<'l>>),
	Or(Vec<Argument<'l>>),
	ExclusiveOr(Vec<Argument<'l>>),
	LeftShift{value: Box<Argument<'l>>, shift: Box<Argument<'l>>},
	RightShift{value: Box<Argument<'l>>, shift: Box<Argument<'l>>},
	Address(Box<Argument<'l>>),
	Sequence(Vec<Argument<'l>>),
	Function{name: &'l str, args: Vec<Argument<'l>>},
}

impl<'l> Argument<'l>
{
	pub fn get_type(&self) -> ArgumentType
	{
		match self
		{
			Self::Constant(..) => ArgumentType::Constant,
			Self::Identifier(..) => ArgumentType::Identifier,
			Self::String(..) => ArgumentType::String,
			Self::Add(..) => ArgumentType::Add,
			Self::Subtract(..) => ArgumentType::Subtract,
			Self::Multiply(..) => ArgumentType::Multiply,
			Self::Not(..) => ArgumentType::Not,
			Self::And(..) => ArgumentType::And,
			Self::Or(..) => ArgumentType::Or,
			Self::ExclusiveOr(..) => ArgumentType::ExclusiveOr,
			Self::LeftShift{..} => ArgumentType::LeftShift,
			Self::RightShift{..} => ArgumentType::RightShift,
			Self::Address(..) => ArgumentType::Address,
			Self::Sequence(..) => ArgumentType::Sequence,
			Self::Function{..} => ArgumentType::Function,
		}
	}
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum ArgumentType
{
	Constant, Identifier, String,
	Add, Subtract, Multiply, Not, And, Or, ExclusiveOr, LeftShift, RightShift,
	Address, Sequence, Function,
}

impl fmt::Display for ArgumentType
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Constant => f.write_str("constant"),
			Self::Identifier => f.write_str("identifier"),
			Self::String => f.write_str("string"),
			Self::Add => f.write_str("addition"),
			Self::Subtract => f.write_str("subtraction"),
			Self::Multiply => f.write_str("multiplication"),
			Self::Not => f.write_str("binary not"),
			Self::And => f.write_str("binary and"),
			Self::Or => f.write_str("binary or"),
			Self::ExclusiveOr => f.write_str("binary xor"),
			Self::LeftShift => f.write_str("left shift"),
			Self::RightShift => f.write_str("right shift"),
			Self::Address => f.write_str("address"),
			Self::Sequence => f.write_str("sequence"),
			Self::Function => f.write_str("function call"),
		}
	}
}

pub type Element<'l> = Positioned<ElementValue<'l>>;

#[derive(Clone, Debug)]
pub enum ElementValue<'l>
{
	Label(&'l str),
	Directive{name: &'l str, args: Vec<Argument<'l>>},
	Instruction{name: &'l str, args: Vec<Argument<'l>>},
}

#[derive(Clone, Debug)]
pub struct Parser<'l>(Tokenizer<'l>);

impl<'l> Parser<'l>
{
	pub fn new(data: &'l [u8]) -> Self
	{
		Self(Tokenizer::new(data))
	}
	
	pub fn get_line(&self) -> u32
	{
		self.0.get_line()
	}
	
	pub fn get_column(&self) -> u32
	{
		self.0.get_column()
	}
	
	fn expect(expect: &'static str, have: Token) -> ParseError
	{
		ParseError{value: ParseErrorKind::Expected{expect, have: have.value.desc()}, line: have.line, col: have.col}
	}
	
	fn eof(&self, expect: &'static str) -> ParseError
	{
		ParseError{value: ParseErrorKind::Expected{expect, have: "<eof>"}, line: self.0.get_line(), col: self.0.get_column()}
	}
	
	pub fn next_element(&mut self) -> Option<Result<Element<'l>, ParseError>>
	{
		match self.0.next()
		{
			Some(t) =>
			{
				match self.do_next(t)
				{
					Ok(e) => Some(Ok(e)),
					Err(e) =>
					{
						self.0.clear();
						Some(Err(e))
					},
				}
			},
			None => None,
		}
	}
	
	fn do_next(&mut self, first: Result<Token<'l>, TokenError>) -> Result<Element<'l>, ParseError>
	{
		match first
		{
			Ok(Token{value: TokenValue::DirectiveMark, line, col}) =>
			{
				let name = match self.0.next()
				{
					Some(Ok(Token{value: TokenValue::Identifier(ident), ..})) => ident,
					Some(Ok(t)) => return Err(Self::expect("identifier", t)),
					Some(Err(e)) => return Err(e.into()),
					None => return Err(self.eof("';'")),
				};
				match self.parse_args(None)
				{
					Ok(args) => Ok(Element{value: ElementValue::Directive{name, args}, line, col}),
					Err(e) => Err(e),
				}
			},
			Ok(Token{value: TokenValue::Identifier(name), line, col}) =>
			{
				let curr = match self.0.next()
				{
					Some(Ok(Token{value: TokenValue::LabelMark, ..})) => return Ok(Element{value: ElementValue::Label(name), line, col}),
					Some(Ok(t)) => t,
					Some(Err(e)) => return Err(e.into()),
					None => return Err(self.eof("';'")),
				};
				match self.parse_args(Some(curr))
				{
					Ok(args) => Ok(Element{value: ElementValue::Instruction{name, args}, line, col}),
					Err(e) => Err(e),
				}
			},
			Ok(t) => Err(Self::expect("';'", t)),
			Err(e) => Err(e.into()),
		}
	}
	
	fn next_inner(&mut self, expect: &'static str) -> Result<Token<'l>, ParseError>
	{
		match self.0.next()
		{
			Some(Ok(t)) => Ok(t),
			Some(Err(e)) => Err(e.into()),
			None => Err(self.eof(expect)),
		}
	}
	
	/*
	fn parse_arg(&mut self, peek: &mut Option<Token<'l>>, until: impl Fn(&Token) -> bool) -> Result<Argument<'l>, ParseError>
	{
		// TODO
	}
	*/
	
	fn parse_args(&mut self, peek: Option<Token<'l>>) -> Result<Vec<Argument<'l>>, ParseError>
	{
		let mut args = Vec::new();
		let mut curr = match peek
		{
			None => self.next_inner("';'")?,
			Some(t) => t,
		};
		if let TokenValue::Terminator = curr.value
		{
			return Ok(args);
		}
		
		loop
		{
			match curr.value
			{
				TokenValue::Number(val) =>
				{
					args.push(Argument::Constant(val));
					// TODO parse expressions
				},
				TokenValue::Identifier(ident) =>
				{
					args.push(Argument::Identifier(ident));
					// TODO parse expressions
				},
				TokenValue::String(val) =>
				{
					args.push(Argument::String(val));
					// TODO parse expressions
				},
				TokenValue::BeginAddr =>
				{
					curr = self.next_inner("<expression>")?;
					let mut expr = match curr.value
					{
						TokenValue::Number(val) => Argument::Constant(val),
						TokenValue::Identifier(ident) => Argument::Identifier(ident),
						_ => return Err(Self::expect("<expression>", curr)),
					};
					loop
					{
						curr = self.next_inner("']'")?;
						match curr.value
						{
							TokenValue::Add => (),
							TokenValue::EndAddr => break,
							_ => return Err(Self::expect("']'", curr)),
						}
						// TODO track operation
						curr = self.next_inner("<expression>")?;
						let new = match curr.value
						{
							TokenValue::Number(val) => Argument::Constant(val),
							TokenValue::Identifier(ident) => Argument::Identifier(ident),
							_ => return Err(Self::expect("<expression>", curr)),
						};
						match expr
						{
							Argument::Constant(..) | Argument::Identifier(..) => expr = Argument::Add(vec![expr, new]),
							Argument::Add(ref mut parts) => parts.push(new),
							_ => panic!("unsupported expression {expr:?}"),
						}
					}
					args.push(Argument::Address(Box::new(expr)));
				},
				TokenValue::BeginSeq =>
				{
					curr = self.next_inner("<expression>")?;
					if let TokenValue::EndSeq = curr.value
					{
						args.push(Argument::Sequence(Vec::new()));
					}
					else
					{
						let mut seq = Vec::new();
						loop
						{
							match curr.value
							{
								TokenValue::Number(val) => seq.push(Argument::Constant(val)),
								TokenValue::Identifier(ident) => seq.push(Argument::Identifier(ident)),
								TokenValue::String(val) => seq.push(Argument::String(val)),
								_ => return Err(Self::expect("<expression>", curr)),
							}
							curr = self.next_inner("'}'")?;
							match curr.value
							{
								TokenValue::Separator => (),
								TokenValue::EndSeq => break,
								_ => return Err(Self::expect("'}'", curr)),
							}
							curr = self.next_inner("<expression>")?;
						}
						args.push(Argument::Sequence(seq));
					}
				},
				_ => return Err(Self::expect("<expression>", curr)),
			}
			curr = self.next_inner("';'")?;
			match curr.value
			{
				TokenValue::Separator => (),
				TokenValue::Terminator => return Ok(args),
				_ => return Err(Self::expect("';'", curr)),
			}
			curr = self.next_inner("<expression>")?;
		}
	}
}

impl<'l> Iterator for Parser<'l>
{
	type Item = Result<Element<'l>, ParseError>;
	
	fn next(&mut self) -> Option<Self::Item>
	{
		self.next_element()
	}
}

impl<'l> FusedIterator for Parser<'l> {}

pub type ParseError = Positioned<ParseErrorKind>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParseErrorKind
{
	Token(TokenError),
	Expected{have: &'static str, expect: &'static str},
}

impl From<TokenError> for ParseError
{
	fn from(value: TokenError) -> Self
	{
		let (line, col) = (value.line, value.col);
		Self{value: ParseErrorKind::Token(value), line, col}
	}
}

impl fmt::Display for ParseErrorKind
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			ParseErrorKind::Token(..) => f.write_str("input token error"),
			ParseErrorKind::Expected{expect, have} => write!(f, "expected {expect}, got {have}"),
		}
	}
}

impl Error for ParseErrorKind
{
	fn source(&self) -> Option<&(dyn Error + 'static)>
	{
		match self
		{
			Self::Token(e) => Some(e),
			_ => None,
		}
	}
}
