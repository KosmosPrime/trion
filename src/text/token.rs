use core::fmt;
use std::error::Error;

#[derive(Clone, Copy, Debug)]
pub enum Number
{
	Integer(i64),
}

#[derive(Clone, Copy, Debug)]
pub struct Token<'l>
{
	pub value: TokenValue<'l>,
	pub line: u32,
	pub col: u32,
}

#[derive(Clone, Copy, Debug)]
pub enum TokenValue<'l>
{
	Separator, Terminator,
	LabelMark, DirectiveMark,
	Add, Subtract, Multiply, Divide,
	Not, And, Or, ExclusiveOr, LeftShift, RightShift,
	Number(Number),
	Identifier(&'l str),
	// TODO implement Character(char),
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
pub struct Tokenizer<'l>
{
	data: &'l [u8],
	line: u32,
	col: u32,
}

impl<'l> Tokenizer<'l>
{
	pub fn new(data: &'l [u8]) -> Self
	{
		Self{data, line: 1, col: 1}
	}
	
	pub fn get_line(&self) -> u32
	{
		self.line
	}
	
	pub fn get_column(&self) -> u32
	{
		self.col
	}
	
	fn advance(&mut self, lines: usize, cols: usize)
	{
		let (lines, cols) = if u32::BITS < usize::BITS
		{
			(
				if lines < u32::MAX as usize {lines as u32} else {u32::MAX},
				if cols < u32::MAX as usize {cols as u32} else {u32::MAX},
			)
		}
		else {(lines as u32, cols as u32)};
		
		if lines > 0
		{
			self.line = self.line.saturating_add(lines);
			self.col = cols;
		}
		else
		{
			self.col = self.col.saturating_add(cols);
		}
	}
	
	fn consume_line_comment(&mut self, p: &mut usize) -> Option<TokenError>
	{
		let mut pos = *p + 2;
		while pos < self.data.len()
		{
			match self.data[pos]
			{
				b'\n' =>
				{
					self.line = self.line.saturating_add(1);
					self.col = 1;
					*p = pos + 1;
					return None;
				},
				// non-ascii is allowed in comments
				_ => pos += 1,
			}
		}
		self.advance(0, *p - pos);
		*p = pos;
		None
	}
	
	fn consume_block_comment(&mut self, p: &mut usize, mut depth: usize) -> Option<TokenError>
	{
		let mut pos = *p + 2;
		let mut lines = 0;
		let mut col_base = *p;
		while depth > 0 && pos < self.data.len()
		{
			match self.data[pos]
			{
				b'\n' =>
				{
					lines += 1;
					col_base = pos - 1;
					pos += 1;
				},
				b'/' if pos + 1 < self.data.len() && self.data[pos + 1] == b'*' =>
				{
					pos += 2;
					if let Some(d) = depth.checked_add(1) {depth = d;}
					else
					{
						return Some(TokenError{kind: TokenErrorKind::BlockComment, line: self.line, col: self.col});
					}
				},
				b'*' if pos + 1 < self.data.len() && self.data[pos + 1] == b'/' =>
				{
					pos += 2;
					depth -= 1;
				},
				_ => pos += 1,
			}
		}
		if depth == 0
		{
			*p = pos;
			self.advance(lines, pos - col_base);
			None
		}
		else {Some(TokenError{kind: TokenErrorKind::BlockComment, line: self.line, col: self.col})}
	}
	
	pub fn clear(&mut self)
	{
		self.data = b"";
	}
	
	pub fn next_token(&mut self) -> Option<Result<Token<'l>, TokenError>>
	{
		let mut pos = 0;
		while pos < self.data.len()
		{
			let c = self.data[pos];
			let _: &Self = match c
			{
				b'\n' =>
				{
					pos += 1;
					self.line = self.line.saturating_add(1);
					self.col = 1;
					continue;
				},
				b'\t' | b'\r' | b' ' =>
				{
					pos += 1;
					self.col = self.col.saturating_add(1);
					continue;
				},
				0x00..=0x1F | 0x7F.. =>
				{
					self.data = b"";
					return Some(Err(TokenError{kind: TokenErrorKind::Invalid, line: self.line, col: self.col}));
				},
				b'/' if pos + 1 < self.data.len() && self.data[pos + 1] == b'/' =>
				{
					match self.consume_line_comment(&mut pos)
					{
						None => continue,
						Some(e) => return Some(Err(e)),
					}
				},
				b'/' if pos + 1 < self.data.len() && self.data[pos + 1] == b'*' =>
				{
					match self.consume_block_comment(&mut pos, 1)
					{
						None => continue,
						Some(e) => return Some(Err(e)),
					}
				},
				_ => self,
			};
			
			return Some(match c
			{
				b',' =>
				{
					self.data = &self.data[pos + 1..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::Separator};
					self.col = self.col.saturating_add(1);
					Ok(t)
				},
				b';' =>
				{
					self.data = &self.data[pos + 1..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::Terminator};
					self.col = self.col.saturating_add(1);
					Ok(t)
				},
				b':' =>
				{
					self.data = &self.data[pos + 1..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::LabelMark};
					self.col = self.col.saturating_add(1);
					Ok(t)
				},
				b'.' =>
				{
					self.data = &self.data[pos + 1..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::DirectiveMark};
					self.col = self.col.saturating_add(1);
					Ok(t)
				},
				b'+' =>
				{
					self.data = &self.data[pos + 1..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::Add};
					self.col = self.col.saturating_add(1);
					Ok(t)
				},
				b'-' =>
				{
					self.data = &self.data[pos + 1..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::Subtract};
					self.col = self.col.saturating_add(1);
					Ok(t)
				},
				b'*' =>
				{
					self.data = &self.data[pos + 1..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::Multiply};
					self.col = self.col.saturating_add(1);
					Ok(t)
				},
				b'/' =>
				{
					self.data = &self.data[pos + 1..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::Divide};
					self.col = self.col.saturating_add(1);
					Ok(t)
				},
				b'!' =>
				{
					self.data = &self.data[pos + 1..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::Not};
					self.col = self.col.saturating_add(1);
					Ok(t)
				},
				b'&' =>
				{
					self.data = &self.data[pos + 1..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::And};
					self.col = self.col.saturating_add(1);
					Ok(t)
				},
				b'|' =>
				{
					self.data = &self.data[pos + 1..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::Or};
					self.col = self.col.saturating_add(1);
					Ok(t)
				},
				b'^' =>
				{
					self.data = &self.data[pos + 1..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::ExclusiveOr};
					self.col = self.col.saturating_add(1);
					Ok(t)
				},
				b'<' if pos + 1 < self.data.len() && self.data[pos + 1] == b'<' =>
				{
					self.data = &self.data[pos + 2..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::LeftShift};
					self.col = self.col.saturating_add(2);
					Ok(t)
				},
				b'>' if pos + 1 < self.data.len() && self.data[pos + 1] == b'>' =>
				{
					self.data = &self.data[pos + 2..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::RightShift};
					self.col = self.col.saturating_add(2);
					Ok(t)
				},
				b'0'..=b'9' =>
				{
					let base = pos;
					let radix = if self.data[pos] == b'0' && pos + 1 < self.data.len()
					{
						pos += 1;
						if self.data[pos] == b'x'
						{
							pos += 1;
							16
						}
						else if self.data[pos] == b'b'
						{
							pos += 1;
							2
						}
						else {8}
					}
					else {10};
					let start = pos;
					
					while pos < self.data.len()
					{
						if !(self.data[pos] as char).is_digit(radix) {break;}
						pos += 1;
					}
					// SAFETY: digits are a subset of alphanumerics
					let text = unsafe{core::str::from_utf8_unchecked(&self.data[start..pos])};
					if text.is_empty() && radix == 8
					{
						assert_eq!(pos - base, 1); // only a zero (assumed octal)
						self.data = &self.data[pos..];
						let t = Token{line: self.line, col: self.col, value: TokenValue::Number(Number::Integer(0))};
						self.col = self.col.saturating_add(1);
						Ok(t)
					}
					else
					{
						match i64::from_str_radix(text, radix)
						{
							Ok(v) =>
							{
								self.data = &self.data[pos..];
								let t = Token{line: self.line, col: self.col, value: TokenValue::Number(Number::Integer(v))};
								self.advance(0, pos - base);
								Ok(t)
							},
							Err(..) =>
							{
								self.data = b"";
								Err(TokenError{kind: TokenErrorKind::BadNumber, line: self.line, col: self.col})
							},
						}
					}
				},
				b'A'..=b'Z' | b'_' | b'a'..=b'z' =>
				{
					let base = pos;
					pos += 1;
					while pos < self.data.len()
					{
						match self.data[pos]
						{
							// FIXME including '.' to simplify .W and .N (but that should be part of the parser)
							b'$' | b'.' | b'0'..=b'9' | b'@' | b'A'..=b'Z' | b'_' | b'a'..=b'z' => pos += 1,
							_ => break,
						}
					}
					// SAFETY: checked to be ASCII by matcher
					let val = unsafe{core::str::from_utf8_unchecked(&self.data[base..pos])};
					self.data = &self.data[pos..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::Identifier(val)};
					self.advance(0, pos - base);
					Ok(t)
				},
				b'(' =>
				{
					self.data = &self.data[pos + 1..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::BeginGroup};
					self.col = self.col.saturating_add(1);
					Ok(t)
				},
				b')' =>
				{
					self.data = &self.data[pos + 1..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::EndGroup};
					self.col = self.col.saturating_add(1);
					Ok(t)
				},
				b'[' =>
				{
					self.data = &self.data[pos + 1..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::BeginAddr};
					self.col = self.col.saturating_add(1);
					Ok(t)
				},
				b']' =>
				{
					self.data = &self.data[pos + 1..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::EndAddr};
					self.col = self.col.saturating_add(1);
					Ok(t)
				},
				b'{' =>
				{
					self.data = &self.data[pos + 1..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::BeginSeq};
					self.col = self.col.saturating_add(1);
					Ok(t)
				},
				b'}' =>
				{
					self.data = &self.data[pos + 1..];
					let t = Token{line: self.line, col: self.col, value: TokenValue::EndSeq};
					self.col = self.col.saturating_add(1);
					Ok(t)
				},
				c =>
				{
					self.data = b"";
					Err(TokenError{kind: TokenErrorKind::Unexpected(c as char), line: self.line, col: self.col})
				},
			});
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TokenError
{
	pub kind: TokenErrorKind,
	pub line: u32,
	pub col: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenErrorKind
{
	Invalid,
	BlockComment,
	BadNumber,
	Unexpected(char),
}

impl fmt::Display for TokenError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self.kind
		{
			TokenErrorKind::Invalid => f.write_str("invalid character")?,
			TokenErrorKind::BlockComment => f.write_str("unclosed block comment")?,
			TokenErrorKind::BadNumber => f.write_str("malformed number")?,
			TokenErrorKind::Unexpected(c) => write!(f, "unexpected character {c:?}")?,
		}
		write!(f, " ({}:{})", self.line, self.col)
	}
}

impl Error for TokenError {}