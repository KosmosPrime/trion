use core::cmp::Ordering;
use core::fmt;
use core::iter::FusedIterator;
use std::borrow::Cow;
use std::error::Error;

use crate::text::operator::{BinOp, BinOpGroup};
use crate::text::Positioned;
use crate::text::token::{Number, Token, TokenError, Tokenizer, TokenValue};

#[cfg(test)]
mod test;

// FIXME these should contain their own location info (or skip element parsing)
#[derive(Clone, Debug)]
pub enum Argument<'l>
{
	Constant(Number),
	Identifier(&'l str),
	String(Cow<'l, str>),
	Add(Vec<Argument<'l>>),
	Negate(Box<Argument<'l>>),
	Subtract(Vec<Argument<'l>>),
	Multiply(Vec<Argument<'l>>),
	Divide{value: Box<Argument<'l>>, divisor: Box<Argument<'l>>},
	Modulo{value: Box<Argument<'l>>, divisor: Box<Argument<'l>>},
	Not(Box<Argument<'l>>),
	BitAnd(Vec<Argument<'l>>),
	BitOr(Vec<Argument<'l>>),
	BitXor(Vec<Argument<'l>>),
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
			Self::Negate(..) => ArgumentType::Negate,
			Self::Subtract(..) => ArgumentType::Subtract,
			Self::Multiply(..) => ArgumentType::Multiply,
			Self::Divide{..} => ArgumentType::Divide,
			Self::Modulo{..} => ArgumentType::Modulo,
			Self::Not(..) => ArgumentType::Not,
			Self::BitAnd(..) => ArgumentType::BitAnd,
			Self::BitOr(..) => ArgumentType::BitOr,
			Self::BitXor(..) => ArgumentType::BitXor,
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
	Add, Negate, Subtract, Multiply, Divide, Modulo,
	Not, BitAnd, BitOr, BitXor, LeftShift, RightShift,
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
			Self::Negate => f.write_str("negation"),
			Self::Subtract => f.write_str("subtraction"),
			Self::Multiply => f.write_str("multiplication"),
			Self::Divide => f.write_str("division"),
			Self::Modulo => f.write_str("modulo"),
			Self::Not => f.write_str("binary not"),
			Self::BitAnd => f.write_str("binary and"),
			Self::BitOr => f.write_str("binary or"),
			Self::BitXor => f.write_str("binary xor"),
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
						Ok(Element{line, col, value: ElementValue::Directive{name, args}})
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
							return Ok(Element{value: ElementValue::Label(name), line, col});
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
						Ok(Element{line, col, value: ElementValue::Instruction{name, args}})
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
					Ok(Argument::Function{name: ident, args})
				}
				else {Ok(Argument::Identifier(ident))}
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
		let mut first_arg = Some(match group.higher()
		{
			None => self.parse_unary()?,
			Some(part) => self.parse_binary(part, expr_start)?,
		});
		let mut operator = None;
		let mut args = Vec::new();
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
			
			match operator
			{
				Some(prev_op) if prev_op != op =>
				{
					// we've encountered a repeatable binary operator which means at least 2 operands
					assert!(first_arg.is_none() && args.len() >= 2);
					first_arg = Some(match prev_op
					{
						BinOp::Add => Argument::Add(args),
						BinOp::Subtract => Argument::Subtract(args),
						BinOp::Multiply => Argument::Multiply(args),
						BinOp::BitAnd => Argument::BitAnd(args),
						BinOp::BitOr => Argument::BitOr(args),
						BinOp::BitXor => Argument::BitXor(args),
						_ => unreachable!("{prev_op:?} is strictly binary"),
					});
					args = Vec::new();
				},
				_ => (),
			}
			
			let next_arg = match group.higher()
			{
				None => self.parse_unary()?,
				Some(part) => self.parse_binary(part, expr_start)?,
			};
			match op
			{
				BinOp::Divide | BinOp::Modulo | BinOp::LeftShift | BinOp::RightShift =>
				{
					assert!(first_arg.is_some() && args.is_empty());
					first_arg = Some(match op
					{
						BinOp::Divide => Argument::Divide{value: Box::new(first_arg.unwrap()), divisor: Box::new(next_arg)},
						BinOp::Modulo => Argument::Modulo{value: Box::new(first_arg.unwrap()), divisor: Box::new(next_arg)},
						BinOp::LeftShift => Argument::LeftShift{value: Box::new(first_arg.unwrap()), shift: Box::new(next_arg)},
						BinOp::RightShift => Argument::RightShift{value: Box::new(first_arg.unwrap()), shift: Box::new(next_arg)},
						_ => unreachable!(),
					});
					operator = None;
				},
				BinOp::Add | BinOp::Subtract | BinOp::Multiply | BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor =>
				{
					if let Some(first) = first_arg.take() {args.push(first);}
					args.push(next_arg);
					operator = Some(op);
				},
			}
		}
		
		if let Some(arg) = first_arg
		{
			assert!(args.is_empty());
			return Ok(arg);
		}
		assert!(args.len() >= 2);
		Ok(match operator
		{
			Some(BinOp::Add) => Argument::Add(args),
			Some(BinOp::Subtract) => Argument::Subtract(args),
			Some(BinOp::Multiply) => Argument::Multiply(args),
			Some(BinOp::BitAnd) => Argument::BitAnd(args),
			Some(BinOp::BitOr) => Argument::BitOr(args),
			Some(BinOp::BitXor) => Argument::BitXor(args),
			op => unreachable!("{op:?} is strictly binary"),
		})
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
