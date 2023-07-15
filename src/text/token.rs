use core::fmt;
use std::error::Error;

use crate::text::Positioned;
use crate::text::matcher::Matcher;

#[derive(Clone, Copy, Debug)]
pub enum Number
{
	Integer(i64),
}

pub type Token<'l> = Positioned<TokenValue<'l>>;

#[derive(Clone, Copy, Debug)]
pub enum TokenValue<'l>
{
	Separator, Terminator,
	LabelMark, DirectiveMark,
	Add, Subtract, Multiply, Divide,
	Not, And, Or, ExclusiveOr, LeftShift, RightShift,
	Number(Number),
	Identifier(&'l str),
	// TODO implement String(Cow<'l, str>),
	// TODO implement ByteString(Cow<'l, [u8]>),
	BeginGroup, EndGroup,
	BeginAddr, EndAddr,
	BeginSeq, EndSeq,
}

impl<'l> TokenValue<'l>
{
	pub fn desc(&self) -> &'static str
	{
		match self
		{
			TokenValue::Separator => "','",
			TokenValue::Terminator => "';'",
			TokenValue::LabelMark => "':'",
			TokenValue::DirectiveMark => "'.'",
			TokenValue::Add => "'+'",
			TokenValue::Subtract => "'-'",
			TokenValue::Multiply => "'*'",
			TokenValue::Divide => "'/'",
			TokenValue::Not => "'!'",
			TokenValue::And => "'&'",
			TokenValue::Or => "'|'",
			TokenValue::ExclusiveOr => "'^'",
			TokenValue::LeftShift => "\"<<\"",
			TokenValue::RightShift => "\">>\"",
			TokenValue::Number(..) => "number",
			TokenValue::Identifier(..) => "identifier",
			TokenValue::BeginGroup => "'('",
			TokenValue::EndGroup => "')'",
			TokenValue::BeginAddr => "'['",
			TokenValue::EndAddr => "']'",
			TokenValue::BeginSeq => "'{'",
			TokenValue::EndSeq => "'}'",
		}
	}
}

impl<'l> fmt::Display for TokenValue<'l>
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		f.write_str(self.desc())
	}
}

#[derive(Clone, Debug)]
pub struct Tokenizer<'l>(Matcher<'l>);

fn consume_line_comment<'l>(m: &mut Matcher<'l>) -> Result<(), TokenError>
{
	match m.search('\n')
	{
		Ok(..) => Ok(()),
		Err(e) => Err(e),
	}
}

fn consume_block_comment<'l>(m: &mut Matcher<'l>, line: u32, col: u32) -> Result<(), TokenError>
{
	let mut depth = 1usize;
	let mut prev: Option<char> = None;
	while let Some(curr) = m.next()
	{
		match curr
		{
			Ok(c) =>
			{
				if prev == Some('/') && c == '*'
				{
					depth += 1;
					prev = None; // avoid matching the '*' again as "*/"
				}
				else if prev == Some('*') && c == '/'
				{
					depth -= 1;
					if depth == 0 {return Ok(());}
					prev = None; // avoid matching the '/' again as '/*'
				}
				else {prev = Some(c);}
			},
			Err(e) => return Err(e),
		}
	}
	Err(Positioned{value: TokenErrorKind::BlockComment, line, col})
}

impl<'l> Tokenizer<'l>
{
	pub fn new(data: &'l [u8]) -> Self
	{
		Self(Matcher::new(data))
	}
	
	pub fn get_line(&self) -> u32
	{
		self.0.line()
	}
	
	pub fn get_column(&self) -> u32
	{
		self.0.column()
	}
	
	pub fn clear(&mut self)
	{
		self.0.clear();
	}
	
	pub fn next_token(&mut self) -> Option<Result<Token<'l>, TokenError>>
	{
		let mut start = self.0.clone();
		while let Some(curr) = self.0.next()
		{
			let c = match curr
			{
				Ok(c) => c,
				Err(e) => return Some(Err(e)),
			};
			
			match c
			{
				'\t' | '\n' | '\r' | ' ' => (),
				'/' =>
				{
					let mut t = self.0.clone();
					match t.next()
					{
						None => return Some(Ok(start.positioned(TokenValue::Divide))),
						Some(Ok(c)) if c == '/' =>
						{
							match consume_line_comment(&mut t)
							{
								Ok(..) => self.0 = t,
								Err(e) =>
								{
									self.0 = t;
									return Some(Err(e));
								},
							}
						},
						Some(Ok(c)) if c == '*' =>
						{
							match consume_block_comment(&mut t, start.line(), start.column())
							{
								Ok(..) => self.0 = t,
								Err(e) =>
								{
									self.0 = t;
									return Some(Err(e));
								},
							}
						},
						Some(Ok(..)) => return Some(Ok(start.positioned(TokenValue::Divide))),
						Some(Err(e)) =>
						{
							self.0 = t;
							return Some(Err(e));
						},
					}
				},
				',' => return Some(Ok(start.positioned(TokenValue::Separator))),
				';' => return Some(Ok(start.positioned(TokenValue::Terminator))),
				':' => return Some(Ok(start.positioned(TokenValue::LabelMark))),
				'.' => return Some(Ok(start.positioned(TokenValue::DirectiveMark))),
				'+' => return Some(Ok(start.positioned(TokenValue::Add))),
				'-' => return Some(Ok(start.positioned(TokenValue::Subtract))),
				'*' => return Some(Ok(start.positioned(TokenValue::Multiply))),
				'!' => return Some(Ok(start.positioned(TokenValue::Not))),
				'&' => return Some(Ok(start.positioned(TokenValue::And))),
				'|' => return Some(Ok(start.positioned(TokenValue::Or))),
				'^' => return Some(Ok(start.positioned(TokenValue::ExclusiveOr))),
				'<' | '>' =>
				{
					let mut t = self.0.clone();
					match t.next()
					{
						Some(Ok(c)) =>
						{
							self.0 = t;
							return Some(Ok(start.positioned(if c == '<' {TokenValue::LeftShift} else {TokenValue::RightShift})));
						},
						Some(Err(e)) =>
						{
							self.0 = t;
							return Some(Err(e));
						},
						_ =>
						{
							self.0.clear();
							return Some(Err(start.positioned(TokenErrorKind::Unexpected(c))));
						}
					}
				},
				'0'..='9' =>
				{
					let (radix, num_start) = if c == '0'
					{
						let mut t = self.0.clone();
						match t.next()
						{
							None => (10, start.clone()),
							Some(Ok(c)) =>
							{
								if c == 'x' {(16, t)}
								else if c == 'b' {(2, t)}
								else if c.is_digit(8) {(8, self.0.clone())}
								else {(10, start.clone())}
							},
							Some(Err(e)) =>
							{
								self.0 = t;
								return Some(Err(e));
							},
						}
					}
					else {(10, start.clone())};
					let mut num_end = num_start.clone();
					self.0 = num_start.clone();
					while let Some(curr) = self.0.next()
					{
						match curr
						{
							Ok(c) if c.is_digit(radix) => num_end = self.0.clone(),
							Ok(..) => break,
							Err(e) => return Some(Err(e)),
						}
					}
					let text = num_end.since(&num_start);
					return Some(match i64::from_str_radix(text, radix)
					{
						Ok(v) =>
						{
							self.0 = num_end;
							Ok(start.positioned(TokenValue::Number(Number::Integer(v))))
						},
						Err(..) =>
						{
							self.0.clear();
							Err(start.positioned(TokenErrorKind::BadNumber))
						},
					});
				},
				'\'' =>
				{
					let mut end = self.0.clone();
					let c = match end.next()
					{
						None | Some(Ok('\'')) =>
						{
							self.0.clear();
							return Some(Err(start.positioned(TokenErrorKind::BadCharacter)));
						},
						Some(Ok('\\')) =>
						{
							match end.next()
							{
								Some(Ok('t')) => '\t',
								Some(Ok('n')) => '\n',
								Some(Ok('r')) => '\r',
								Some(Ok(c @ ('"' | '\'' | '\\'))) => c,
								None | Some(Ok(..)) =>
								{
									self.0.clear();
									return Some(Err(start.positioned(TokenErrorKind::BadCharacter)));
								},
								Some(Err(e)) =>
								{
									self.0 = end;
									return Some(Err(e));
								},
							}
						},
						Some(Ok(c)) => c,
						Some(Err(e)) =>
						{
							self.0 = end;
							return Some(Err(e));
						},
					};
					match end.next()
					{
						Some(Ok('\'')) =>
						{
							self.0 = end;
							return Some(Ok(start.positioned(TokenValue::Number(Number::Integer(u32::from(c).into())))));
						},
						None | Some(Ok(..)) =>
						{
							self.0.clear();
							return Some(Err(start.positioned(TokenErrorKind::BadCharacter)));
						},
						Some(Err(e)) =>
						{
							self.0 = end;
							return Some(Err(e));
						},
					}
				},
				'A'..='Z' | '_' | 'a'..='z' =>
				{
					let mut end = self.0.clone();
					let mut prev = end.clone();
					while let Some(curr) = end.next()
					{
						match curr
						{
							Ok('$' | '.' | '0'..='9' | '@' | 'A'..='Z' | '_' | 'a'..='z') => prev = end.clone(),
							Ok(..) => break,
							Err(e) =>
							{
								self.0 = end;
								return Some(Err(e));
							},
						}
					}
					let val = prev.since(&mut start);
					self.0 = prev;
					return Some(Ok(start.positioned(TokenValue::Identifier(val))));
				},
				'(' => return Some(Ok(start.positioned(TokenValue::BeginGroup))),
				')' => return Some(Ok(start.positioned(TokenValue::EndGroup))),
				'[' => return Some(Ok(start.positioned(TokenValue::BeginAddr))),
				']' => return Some(Ok(start.positioned(TokenValue::EndAddr))),
				'{' => return Some(Ok(start.positioned(TokenValue::BeginSeq))),
				'}' => return Some(Ok(start.positioned(TokenValue::EndSeq))),
				c =>
				{
					self.0.clear();
					return Some(Err(start.positioned(TokenErrorKind::Unexpected(c))));
				},
			}
			start = self.0.clone();
		}
		None
	}
}

impl<'l> Iterator for Tokenizer<'l>
{
	type Item = Result<Token<'l>, TokenError>;
	
	fn next(&mut self) -> Option<Self::Item>
	{
		self.next_token()
	}
}

pub type TokenError = Positioned<TokenErrorKind>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenErrorKind
{
	BadUnicode,
	Invalid,
	BlockComment,
	BadNumber,
	BadCharacter,
	Unexpected(char),
}

impl fmt::Display for TokenErrorKind
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			TokenErrorKind::BadUnicode => f.write_str("malformed UTF-8 code-point"),
			TokenErrorKind::Invalid => f.write_str("invalid character"),
			TokenErrorKind::BlockComment => f.write_str("unclosed block comment"),
			TokenErrorKind::BadNumber => f.write_str("malformed number"),
			TokenErrorKind::BadCharacter => f.write_str("malformed character literal"),
			TokenErrorKind::Unexpected(c) => write!(f, "unexpected character {c:?}"),
		}
	}
}

impl Error for TokenErrorKind {}

#[cfg(test)]
mod test
{
	use super::*;
	
	macro_rules!expect
	{
		($t:ident, $want:pat, {$($then:tt)*}) =>
		{
			match $t.next()
			{
				None => panic!("end of token stream"),
				Some(Ok($want)) => {$($then)*},
				Some(Ok(v)) => panic!("unexpected output {v:?}"),
				Some(Err(e)) => panic!("unexpected error {e}"),
			}
		};
	}
	
	#[test]
	fn whitespace()
	{
		let mut t = Tokenizer::new("  \t\t\n\t \n  \r\n  \t ".as_bytes());
		assert!(t.next().is_none());
		assert_eq!((t.0.line(), t.0.column()), (4, 5));
	}
	
	#[test]
	fn comments()
	{
		let mut t = Tokenizer::new("
		// hello world
		
		/*
		comment
		// comment */".as_bytes());
		assert!(t.next().is_none());
		assert_eq!((t.0.line(), t.0.column()), (6, 16));
	}
	
	#[test]
	fn numbers()
	{
		let mut t = Tokenizer::new("0 0000 1 9 10 11 19 100 0100 0x100 0b100 0".as_bytes());
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 0);});
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 0);});
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 1);});
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 9);});
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 10);});
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 11);});
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 19);});
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 100);});
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 64);});
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 0x100);});
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 0b100);});
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 0);});
		assert!(t.next().is_none());
		
		assert_eq!(Tokenizer::new("0x".as_bytes()).next().unwrap().unwrap_err().value, TokenErrorKind::BadNumber);
		assert_eq!(Tokenizer::new("0x".as_bytes()).next().unwrap().unwrap_err().value, TokenErrorKind::BadNumber);
		assert_eq!(Tokenizer::new("0xz".as_bytes()).next().unwrap().unwrap_err().value, TokenErrorKind::BadNumber);
		assert_eq!(Tokenizer::new("0bz".as_bytes()).next().unwrap().unwrap_err().value, TokenErrorKind::BadNumber);
	}
	
	#[test]
	fn chars()
	{
		let mut t = Tokenizer::new("' '   'a' '\t' '\n' '\r'".as_bytes());
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, ' ' as i64);});
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 'a' as i64);});
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, '\t' as i64);});
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, '\n' as i64);});
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, '\r' as i64);});
		assert!(t.next().is_none());
		
		assert_eq!(Tokenizer::new("'".as_bytes()).next().unwrap().unwrap_err().value, TokenErrorKind::BadCharacter);
		assert_eq!(Tokenizer::new("'a".as_bytes()).next().unwrap().unwrap_err().value, TokenErrorKind::BadCharacter);
		assert_eq!(Tokenizer::new("'aa'".as_bytes()).next().unwrap().unwrap_err().value, TokenErrorKind::BadCharacter);
		assert_eq!(Tokenizer::new("''".as_bytes()).next().unwrap().unwrap_err().value, TokenErrorKind::BadCharacter);
	}
	
	#[test]
	fn identifiers()
	{
		let mut t = Tokenizer::new("hello _world /*nope*/ te$Tw@rd".as_bytes());
		expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "hello");});
		expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "_world");});
		expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "te$Tw@rd");});
		assert!(t.next().is_none());
		
		let mut t = Tokenizer::new("keyäöüword".as_bytes());
		expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "key");});
		assert_eq!(t.next().unwrap().unwrap_err().value, TokenErrorKind::Unexpected('ä'));
		assert!(t.next().is_none());
	}
	
	#[test]
	fn symbols()
	{
		let mut t = Tokenizer::new("/,;:.+-*!&|^<<>>()[]{}".as_bytes());
		expect!(t, Positioned{value: TokenValue::Divide, ..}, {});
		expect!(t, Positioned{value: TokenValue::Separator, ..}, {});
		expect!(t, Positioned{value: TokenValue::Terminator, ..}, {});
		expect!(t, Positioned{value: TokenValue::LabelMark, ..}, {});
		expect!(t, Positioned{value: TokenValue::DirectiveMark, ..}, {});
		expect!(t, Positioned{value: TokenValue::Add, ..}, {});
		expect!(t, Positioned{value: TokenValue::Subtract, ..}, {});
		expect!(t, Positioned{value: TokenValue::Multiply, ..}, {});
		expect!(t, Positioned{value: TokenValue::Not, ..}, {});
		expect!(t, Positioned{value: TokenValue::And, ..}, {});
		expect!(t, Positioned{value: TokenValue::Or, ..}, {});
		expect!(t, Positioned{value: TokenValue::ExclusiveOr, ..}, {});
		expect!(t, Positioned{value: TokenValue::LeftShift, ..}, {});
		expect!(t, Positioned{value: TokenValue::RightShift, ..}, {});
		expect!(t, Positioned{value: TokenValue::BeginGroup, ..}, {});
		expect!(t, Positioned{value: TokenValue::EndGroup, ..}, {});
		expect!(t, Positioned{value: TokenValue::BeginAddr, ..}, {});
		expect!(t, Positioned{value: TokenValue::EndAddr, ..}, {});
		expect!(t, Positioned{value: TokenValue::BeginSeq, ..}, {});
		expect!(t, Positioned{value: TokenValue::EndSeq, ..}, {});
		assert!(t.next().is_none());
	}
	
	#[test]
	fn instructions()
	{
		let mut t = Tokenizer::new("LDR R7, [R0 + 0x24]; // read the thing from somewhere".as_bytes());
		expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "LDR");});
		expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "R7");});
		expect!(t, Positioned{value: TokenValue::Separator, ..}, {});
		expect!(t, Positioned{value: TokenValue::BeginAddr, ..}, {});
		expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "R0");});
		expect!(t, Positioned{value: TokenValue::Add, ..}, {});
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 0x24);});
		expect!(t, Positioned{value: TokenValue::EndAddr, ..}, {});
		expect!(t, Positioned{value: TokenValue::Terminator, ..}, {});
		assert!(t.next().is_none());
		
		let mut t = Tokenizer::new("SUBS R0, 'A'; // free comparison < 'A'".as_bytes());
		expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "SUBS");});
		expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "R0");});
		expect!(t, Positioned{value: TokenValue::Separator, ..}, {});
		expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 'A' as i64);});
		expect!(t, Positioned{value: TokenValue::Terminator, ..}, {});
		assert!(t.next().is_none());
	}
}
