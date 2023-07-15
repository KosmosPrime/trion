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
