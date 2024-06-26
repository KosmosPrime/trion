use core::cmp::Ordering;
use core::fmt;
use core::iter::FusedIterator;
use std::error::Error;

use crate::asm::arcob::Arcob;
use crate::text::operator::{BinOp, BinOpGroup};
use crate::text::Positioned;
pub use crate::text::parse::arg_ty::ArgumentType;
use crate::text::token::{Number, Token, TokenError, Tokenizer, TokenValue};

pub mod arg_ty;
pub mod choice;
#[cfg(test)]
mod test;

// FIXME these should contain their own location info (or skip element parsing)
#[derive(Clone, Debug)]
pub enum Argument<'l>
{
	Constant(Number),
	Identifier(Arcob<'l, str>),
	String(Arcob<'l, str>),
	Add{lhs: Box<Argument<'l>>, rhs: Box<Argument<'l>>},
	Negate(Box<Argument<'l>>),
	Subtract{lhs: Box<Argument<'l>>, rhs: Box<Argument<'l>>},
	Multiply{lhs: Box<Argument<'l>>, rhs: Box<Argument<'l>>},
	Divide{lhs: Box<Argument<'l>>, rhs: Box<Argument<'l>>},
	Modulo{lhs: Box<Argument<'l>>, rhs: Box<Argument<'l>>},
	Not(Box<Argument<'l>>),
	BitAnd{lhs: Box<Argument<'l>>, rhs: Box<Argument<'l>>},
	BitOr{lhs: Box<Argument<'l>>, rhs: Box<Argument<'l>>},
	BitXor{lhs: Box<Argument<'l>>, rhs: Box<Argument<'l>>},
	LeftShift{lhs: Box<Argument<'l>>, rhs: Box<Argument<'l>>},
	RightShift{lhs: Box<Argument<'l>>, rhs: Box<Argument<'l>>},
	Address(Box<Argument<'l>>),
	Sequence(Vec<Argument<'l>>),
	Function{name: Arcob<'l, str>, args: Vec<Argument<'l>>},
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
			Self::Add{..} => ArgumentType::Add,
			Self::Negate(..) => ArgumentType::Negate,
			Self::Subtract{..} => ArgumentType::Subtract,
			Self::Multiply{..} => ArgumentType::Multiply,
			Self::Divide{..} => ArgumentType::Divide,
			Self::Modulo{..} => ArgumentType::Modulo,
			Self::Not(..) => ArgumentType::Not,
			Self::BitAnd{..} => ArgumentType::BitAnd,
			Self::BitOr{..} => ArgumentType::BitOr,
			Self::BitXor{..} => ArgumentType::BitXor,
			Self::LeftShift{..} => ArgumentType::LeftShift,
			Self::RightShift{..} => ArgumentType::RightShift,
			Self::Address(..) => ArgumentType::Address,
			Self::Sequence(..) => ArgumentType::Sequence,
			Self::Function{..} => ArgumentType::Function,
		}
	}
	
	pub fn vec_into_owned(args: Vec<Self>) -> Vec<Argument<'static>>
	{
		let mut owned_args = Vec::new();
		for arg in args
		{
			owned_args.push(arg.into_owned());
		}
		owned_args
	}
	
	pub fn into_owned(self) -> Argument<'static>
	{
		let arg_ty = self.get_type();
		match self
		{
			Self::Constant(n) => Argument::Constant(n),
			Self::Identifier(ident) => Argument::Identifier(Arcob::Arced(ident.into_arc())),
			Self::String(value) => Argument::String(Arcob::Arced(value.into_arc())),
			Self::Add{lhs, rhs} | Self::Subtract{lhs, rhs} | Self::Multiply{lhs, rhs} | Self::Divide{lhs, rhs}
				| Self::Modulo{lhs, rhs} | Self::BitAnd{lhs, rhs} | Self::BitOr{lhs, rhs} | Self::BitXor{lhs, rhs}
				| Self::LeftShift{lhs, rhs} | Self::RightShift{lhs, rhs} =>
			{
				let lhs = Box::new(lhs.into_owned());
				let rhs = Box::new(rhs.into_owned());
				match arg_ty
				{
					ArgumentType::Add => Argument::Add{lhs, rhs},
					ArgumentType::Subtract => Argument::Subtract{lhs, rhs},
					ArgumentType::Multiply => Argument::Multiply{lhs, rhs},
					ArgumentType::Divide => Argument::Divide{lhs, rhs},
					ArgumentType::Modulo => Argument::Modulo{lhs, rhs},
					ArgumentType::BitAnd => Argument::BitAnd{lhs, rhs},
					ArgumentType::BitOr => Argument::BitOr{lhs, rhs},
					ArgumentType::BitXor => Argument::BitXor{lhs, rhs},
					ArgumentType::LeftShift => Argument::LeftShift{lhs, rhs},
					ArgumentType::RightShift => Argument::RightShift{lhs, rhs},
					_ => unreachable!(),
				}
			},
			Self::Negate(value) | Self::Not(value) | Self::Address(value) =>
			{
				let value = Box::new(value.into_owned());
				match arg_ty
				{
					ArgumentType::Negate => Argument::Negate(value),
					ArgumentType::Not => Argument::Not(value),
					ArgumentType::Address => Argument::Address(value),
					_ => unreachable!(),
				}
			},
			Self::Sequence(args) => Argument::Sequence(Self::vec_into_owned(args)),
			Self::Function{name, args} => Argument::Function{name: Arcob::Arced(name.into_arc()), args: Self::vec_into_owned(args)},
		}
	}
}

pub type Element<'l> = Positioned<ElementValue<'l>>;

#[derive(Clone, Debug)]
pub enum ElementValue<'l>
{
	Label(Arcob<'l, str>),
	Directive{name: Arcob<'l, str>, args: Vec<Argument<'l>>},
	Instruction{name: Arcob<'l, str>, args: Vec<Argument<'l>>},
}

impl<'l> ElementValue<'l>
{
	pub fn into_owned(self) -> ElementValue<'static>
	{
		match self
		{
			Self::Label(name) => ElementValue::Label(Arcob::Arced(name.into_arc())),
			Self::Directive{name, args} => ElementValue::Directive{name: Arcob::Arced(name.into_arc()), args: Argument::vec_into_owned(args)},
			Self::Instruction{name, args} => ElementValue::Instruction{name: Arcob::Arced(name.into_arc()), args: Argument::vec_into_owned(args)},
		}
	}
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
	
	fn expect(expect: &'static str, have: &Token) -> ParseError
	{
		ParseError{line: have.line, col: have.col, value: ParseErrorKind::Expected{expect, have: have.value.desc()}}
	}
	
	fn eof(&self, expect: &'static str) -> ParseError
	{
		ParseError{line: self.0.get_line(), col: self.0.get_column(), value: ParseErrorKind::Expected{expect, have: "<eof>"}}
	}
	
	fn do_next(&mut self, first: Result<Token<'l>, TokenError>) -> Result<Element<'l>, ParseError>
	{
		match first
		{
			Ok(Token{line, col, value: TokenValue::DirectiveMark}) =>
			{
				let name = match self.0.next()
				{
					Some(Ok(Token{value: TokenValue::Identifier(ident), ..})) => ident,
					Some(Ok(t)) => return Err(Self::expect("identifier", &t)),
					Some(Err(e)) => return Err(e.into()),
					None => return Err(self.eof("';'")),
				};
				match self.parse_args()
				{
					Ok(args) =>
					{
						self.next_inner("';'")?;
						Ok(Element{line, col, value: ElementValue::Directive{name: Arcob::Borrowed(name), args}})
					},
					Err(e) => Err(e),
				}
			},
			Ok(Token{line, col, value: TokenValue::Identifier(name)}) =>
			{
				match self.0.peek()
				{
					Some(Ok(t)) =>
					{
						if matches!(t.value, TokenValue::LabelMark)
						{
							drop(self.0.next());
							return Ok(Element{value: ElementValue::Label(Arcob::Borrowed(name)), line, col});
						}
					},
					Some(Err(..)) => return Err(Positioned{line, col, value: ParseErrorKind::Token(self.0.next().unwrap().unwrap_err())}),
					None => return Err(self.eof("<argument-list>")),
				};
				match self.parse_args()
				{
					Ok(args) =>
					{
						self.next_inner("';'")?;
						Ok(Element{line, col, value: ElementValue::Instruction{name: Arcob::Borrowed(name), args}})
					},
					Err(e) => Err(e),
				}
			},
			Ok(t) => Err(Self::expect("<instruction>", &t)),
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
	
	fn parse_unary(&mut self) -> Result<Argument<'l>, ParseError>
	{
		let token = self.next_inner("<unary>")?;
		match token.value
		{
			TokenValue::Minus => Ok(Argument::Negate(Box::new(self.parse_unary()?))),
			TokenValue::Not => Ok(Argument::Not(Box::new(self.parse_unary()?))),
			TokenValue::Number(val) => Ok(Argument::Constant(val)),
			TokenValue::Identifier(ident) =>
			{
				if let Some(&Positioned{value: TokenValue::BeginGroup, ..}) = self.0.peek().and_then(Result::ok)
				{
					drop(self.0.next()); // (identifier '(') is enough to know it must be a function
					let args = self.parse_args()?;
					let end = self.next_inner("')'")?;
					if !matches!(end.value, TokenValue::EndGroup)
					{
						return Err(Self::expect("')'", &end));
					}
					Ok(Argument::Function{name: Arcob::Borrowed(ident), args})
				}
				else {Ok(Argument::Identifier(Arcob::Borrowed(ident)))}
			},
			TokenValue::String(val) => Ok(Argument::String(val)),
			TokenValue::BeginGroup =>
			{
				let expr_start = match self.0.peek()
				{
					Some(Ok(t)) => t.convert(()),
					_ => Positioned{line: self.0.get_line(), col: self.0.get_column(), value: ()},
				};
				let inner = self.parse_binary(BinOpGroup::BitOr, expr_start)?; // any operator
				let end = self.next_inner("')'")?;
				if !matches!(end.value, TokenValue::EndGroup)
				{
					return Err(Self::expect("')'", &end));
				}
				Ok(inner)
			},
			TokenValue::BeginAddr =>
			{
				let expr_start = match self.0.peek()
				{
					Some(Ok(t)) => t.convert(()),
					_ => Positioned{line: self.0.get_line(), col: self.0.get_column(), value: ()},
				};
				let inner = self.parse_binary(BinOpGroup::BitOr, expr_start)?; // any operator
				let end = self.next_inner("']'")?;
				if !matches!(end.value, TokenValue::EndAddr)
				{
					return Err(Self::expect("']'", &end));
				}
				Ok(Argument::Address(Box::new(inner)))
			},
			TokenValue::BeginSeq =>
			{
				let args = self.parse_args()?;
				let end = self.next_inner("'}'")?;
				if !matches!(end.value, TokenValue::EndSeq)
				{
					return Err(Self::expect("'}'", &end));
				}
				Ok(Argument::Sequence(args))
			},
			_ => Err(Self::expect("<unary>", &token)),
		}
	}
	
	fn parse_binary(&mut self, group: BinOpGroup, expr_start: Positioned<()>) -> Result<Argument<'l>, ParseError>
	{
		let mut lhs = match group.higher()
		{
			None => self.parse_unary()?,
			Some(part) => self.parse_binary(part, expr_start)?,
		};
		loop
		{
			let op = match self.0.peek()
			{
				None => break,
				Some(Ok(op_token)) =>
				{
					match op_token.value
					{
						TokenValue::Separator | TokenValue::Terminator | TokenValue::EndGroup
							| TokenValue::EndAddr | TokenValue::EndSeq => break,
						_ => (),
					}
					match BinOp::decode(&op_token.value)
					{
						Some(op) =>
						{
							match Ord::cmp(&op.into(), &group)
							{
								// an outer recursive call will handle this one
								Ordering::Less => break,
								Ordering::Equal => (),
								// an inner recursive call should have consumed this operator at some stage
								Ordering::Greater => panic!("encountered operator {op:?} in group {group:?}"),
							}
							self.0.next();
							op
						},
						// returning an error drops the tokenizer so we don't have to consume the inner error
						_ => return Err(Self::expect("<operator>", op_token)),
					}
				},
				Some(Err(..)) => return Err(expr_start.convert(ParseErrorKind::Token(self.0.next().unwrap().unwrap_err()))),
			};
			
			let rhs = match group.higher()
			{
				None => self.parse_unary()?,
				Some(part) => self.parse_binary(part, expr_start)?,
			};
			lhs = match op
			{
				BinOp::Add => Argument::Add{lhs: Box::new(lhs), rhs: Box::new(rhs)},
				BinOp::Subtract => Argument::Subtract{lhs: Box::new(lhs), rhs: Box::new(rhs)},
				BinOp::Multiply => Argument::Multiply{lhs: Box::new(lhs), rhs: Box::new(rhs)},
				BinOp::Divide => Argument::Divide{lhs: Box::new(lhs), rhs: Box::new(rhs)},
				BinOp::Modulo => Argument::Modulo{lhs: Box::new(lhs), rhs: Box::new(rhs)},
				BinOp::BitAnd => Argument::BitAnd{lhs: Box::new(lhs), rhs: Box::new(rhs)},
				BinOp::BitOr => Argument::BitOr{lhs: Box::new(lhs), rhs: Box::new(rhs)},
				BinOp::BitXor => Argument::BitXor{lhs: Box::new(lhs), rhs: Box::new(rhs)},
				BinOp::LeftShift => Argument::LeftShift{lhs: Box::new(lhs), rhs: Box::new(rhs)},
				BinOp::RightShift => Argument::RightShift{lhs: Box::new(lhs), rhs: Box::new(rhs)},
			};
		}
		Ok(lhs)
	}
	
	fn parse_args(&mut self) -> Result<Vec<Argument<'l>>, ParseError>
	{
		match self.0.peek()
		{
			// having an argument list end with (<eof>) might be valid in some cases
			None => return Ok(Vec::new()),
			Some(Ok(t)) =>
			{
				if matches!(t.value, TokenValue::Terminator | TokenValue::EndGroup | TokenValue::EndAddr | TokenValue::EndSeq)
				{
					// the argument list is empty
					return Ok(Vec::new());
				}
			},
			Some(Err(..)) =>
			{
				let err = self.0.next().unwrap().unwrap_err();
				return Err(Positioned{line: err.line, col: err.col, value: ParseErrorKind::Token(err)});
			},
		}
		
		let mut args = Vec::new();
		loop
		{
			let expr_start = match self.0.peek()
			{
				Some(Ok(t)) => t.convert(()),
				_ => Positioned{line: self.0.get_line(), col: self.0.get_column(), value: ()},
			};
			let arg = self.parse_binary(BinOpGroup::BitOr, expr_start)?; // any operator
			args.push(arg);
			
			match self.0.peek()
			{
				None => return Err(self.eof("<separator>")),
				Some(Ok(t)) =>
				{
					match t.value
					{
						TokenValue::Separator =>
						{
							drop(self.0.next());
							
						},
						TokenValue::Terminator | TokenValue::EndGroup | TokenValue::EndAddr | TokenValue::EndSeq => break,
						_ => return Err(Self::expect("<separator>", t)),
					}
				},
				Some(Err(..)) =>
				{
					let err = self.0.next().unwrap().unwrap_err();
					return Err(Positioned{line: err.line, col: err.col, value: ParseErrorKind::Token(err)});
				},
			}
		}
		Ok(args)
	}
}

impl<'l> Iterator for Parser<'l>
{
	type Item = Result<Element<'l>, ParseError>;
	
	fn next(&mut self) -> Option<Self::Item>
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
