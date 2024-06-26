use core::fmt;
use std::collections::VecDeque;
use std::error::Error;

use crate::asm::arcob::Arcob;
use crate::text::Positioned;

#[cfg(test)]
mod test;

#[derive(Clone, Copy, Debug)]
pub enum Number
{
	Integer(i64),
}

pub type Token<'l> = Positioned<TokenValue<'l>>;

#[derive(Clone, Debug)]
pub enum TokenValue<'l>
{
	Separator, Terminator,
	LabelMark, DirectiveMark,
	Plus, Minus, Multiply, Divide, Modulo,
	Not, BitAnd, BitOr, BitXor, LeftShift, RightShift,
	Number(Number),
	Identifier(&'l str),
	String(Arcob<'l, str>),
	// TODO implement ByteString(Arcob<'l, [u8]>),
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
			TokenValue::Plus => "'+'",
			TokenValue::Minus => "'-'",
			TokenValue::Multiply => "'*'",
			TokenValue::Divide => "'/'",
			TokenValue::Modulo => "'%'",
			TokenValue::Not => "'!'",
			TokenValue::BitAnd => "'&'",
			TokenValue::BitOr => "'|'",
			TokenValue::BitXor => "'^'",
			TokenValue::LeftShift => "\"<<\"",
			TokenValue::RightShift => "\">>\"",
			TokenValue::Number(..) => "number",
			TokenValue::Identifier(..) => "identifier",
			TokenValue::String(..) => "string",
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
	data: &'l str,
	utf_err: bool,
	col: u32,
	line: u32,
	tokens: VecDeque<Token<'l>>,
	token_err: Option<TokenError>,
}

impl<'l> Tokenizer<'l>
{
	pub fn new(data: &'l [u8]) -> Self
	{
		let (data, utf_err) = match core::str::from_utf8(data)
		{
			Ok(s) => (s, false),
			Err(e) =>
			{
				// SAFETY: the error guarantees this slice is valid
				(unsafe{core::str::from_utf8_unchecked(&data[..e.valid_up_to()])}, true)
			},
		};
		Self
		{
			data, utf_err,
			line: 1,
			col: 1,
			tokens: VecDeque::new(),
			token_err: None,
		}
	}
	
	pub fn get_line(&self) -> u32
	{
		self.line
	}
	
	pub fn get_column(&self) -> u32
	{
		self.col
	}
	
	pub fn clear(&mut self)
	{
		self.data = "";
		self.utf_err = false;
	}
	
	fn update_pos(&mut self, data: &str)
	{
		let lines = u32::try_from(data.bytes().filter(|&b| b == b'\n').count()).unwrap_or(u32::MAX);
		if lines > 0
		{
			self.line = self.line.saturating_add(lines);
			self.col = 1;
		}
		let data = &data[data.bytes().rposition(|b| b == b'\n').map_or(0, |v| v + 1)..];
		// faster character count by ignoring continuation bytes (instead of `str::chars`)
		let coluns = u32::try_from(data.bytes().filter(|&b| (b & 0b11_000000) != 0b10_000000).count()).unwrap_or(u32::MAX);
		self.col = self.col.saturating_add(coluns);
	}
	
	fn positioned<T>(&self, value: T) -> Positioned<T>
	{
		Positioned{line: self.line, col: self.col, value}
	}
	
	fn next_token(&mut self) -> Option<Result<Token<'l>, TokenError>>
	{
		while self.data.len() > 0
		{
			let space_bytes = match self.data.bytes().position(|b| b != b'\t' && b != b'\n' && b != b'\r' && b != b' ')
			{
				None => self.data.len(),
				Some(cnt) => cnt,
			};
			if space_bytes > 0
			{
				self.update_pos(&self.data[..space_bytes]);
				self.data = &self.data[space_bytes..];
			}
			
			if self.data.starts_with("//")
			{
				match self.data.bytes().position(|b| b == b'\n')
				{
					None =>
					{
						self.update_pos(self.data);
						let utf_err = self.utf_err;
						self.clear();
						if utf_err
						{
							return Some(Err(self.positioned(TokenErrorKind::BadUnicode)));
						}
					},
					Some(line_len) =>
					{
						self.data = &self.data[line_len + 1..];
						self.line = self.line.saturating_add(1);
						self.col = 1;
					},
				}
			}
			else if self.data.starts_with("/*")
			{
				let mut depth = 1; // support nested block comments
				let mut start = 2; // to avoid combinations like "/*/" and "*/*"
				let comment_bytes = self.data[..self.data.len() - 1].bytes().enumerate().skip(2).position(|(p, b)|
				{
					if b == b'/' && p >= start && self.data.as_bytes()[p + 1] == b'*'
					{
						// no overflow because `p < self.data.len() - 1 <= usize::MAX - 1`
						start = p + 2;
						// no overflow because this can't happen more than `usize::MAX / 2` times
						depth += 1;
					}
					else if b == b'*' && p >= start && self.data.as_bytes()[p + 1] == b'/'
					{
						// no overflow because `p < self.data.len() - 1 <= usize::MAX - 1`
						start = p + 2;
						// no overflow because this can't happen more than `usize::MAX / 2` times
						depth -= 1;
					}
					depth == 0
				});
				if let Some(comment_bytes) = comment_bytes
				{
					self.update_pos(&self.data[..4 + comment_bytes]);
					self.data = &self.data[4 + comment_bytes..];
				}
				else
				{
					self.update_pos(self.data);
					let err = if self.utf_err {TokenErrorKind::BadUnicode} else {TokenErrorKind::BlockComment};
					self.clear();
					return Some(Err(self.positioned(err)))
				}
			}
			else {break;}
		}
		
		if !self.data.is_empty()
		{
			match self.do_next()
			{
				Some(Err(e)) =>
				{
					self.clear();
					Some(Err(e))
				},
				r => r,
			}
		}
		else
		{
			if self.utf_err
			{
				self.utf_err = false;
				Some(Err(self.positioned(TokenErrorKind::BadUnicode)))
			}
			else {None}
		}
	}
	
	fn do_next(&mut self) -> Option<Result<Token<'l>, TokenError>>
	{
		let (n, ascii_ln, token) = match self.data.as_bytes()[0]
		{
			b',' => (1, true, TokenValue::Separator),
			b';' => (1, true, TokenValue::Terminator),
			b':' => (1, true, TokenValue::LabelMark),
			b'.' => (1, true, TokenValue::DirectiveMark),
			b'+' => (1, true, TokenValue::Plus),
			b'-' => (1, true, TokenValue::Minus),
			b'*' => (1, true, TokenValue::Multiply),
			b'/' => (1, true, TokenValue::Divide),
			b'%' => (1, true, TokenValue::Modulo),
			b'!' => (1, true, TokenValue::Not),
			b'&' => (1, true, TokenValue::BitAnd),
			b'|' => (1, true, TokenValue::BitOr),
			b'^' => (1, true, TokenValue::BitXor),
			b'<' if self.data.len() >= 2 && self.data.as_bytes()[1] == b'<' => (2, true, TokenValue::LeftShift),
			b'>' if self.data.len() >= 2 && self.data.as_bytes()[1] == b'>' => (2, true, TokenValue::RightShift),
			b'0'..=b'9' =>
			{
				let (off, radix) =
				{
					if self.data.starts_with("0b") {(2, 2)}
					else if self.data.starts_with("0o") {(2, 8)}
					else if self.data.starts_with("0x") {(2, 16)}
					else {(0, 10)}
				};
				let len = match self.data[off..].bytes().position(|b| !(b as char).is_digit(radix))
				{
					None if self.utf_err =>
					{
						// numbers are ASCII so `str::bytes().count() == str::chars().count()`
						self.col = self.col.saturating_add(u32::try_from(self.data.len()).unwrap_or(u32::MAX));
						self.clear();
						return Some(Err(self.positioned(TokenErrorKind::BadUnicode)));
					},
					None => self.data.len() - off,
					Some(n) => n,
				};
				let text = &self.data[off..off + len];
				match i64::from_str_radix(text, radix)
				{
					Ok(v) => (off + len, true, TokenValue::Number(Number::Integer(v))),
					Err(..) =>
					{
						self.clear();
						return Some(Err(self.positioned(TokenErrorKind::BadNumber)));
					},
				}
			},
			b'\'' =>
			{
				let mut chars = self.data[1..].chars();
				let result = match chars.next()
				{
					None => Err(self.utf_err),
					Some('\\') =>
					{
						match chars.next()
						{
							None => Err(self.utf_err),
							Some('t') => Ok((2, '\t')),
							Some('n') => Ok((2, '\n')),
							Some('r') => Ok((2, '\r')),
							Some(c @ ('"' | '\'' | '\\')) => Ok((2, c)),
							Some(..) => Err(false),
						}
					},
					Some(c @ ('\t' | ' '..='~' | '\u{80}'..)) => Ok((c.len_utf8(), c)),
					_ => Err(false),
				}.and_then(|v|
				{
					match chars.next()
					{
						None => Err(self.utf_err),
						Some('\'') => Ok(v),
						Some(..) => Err(false),
					}
				});
				match result
				{
					Ok((n, c)) => (1 + n + 1, c < '\u{80}', TokenValue::Number(Number::Integer(u32::from(c).into()))),
					Err(utf_err) =>
					{
						let err = if utf_err
						{
							self.update_pos(self.data);
							TokenErrorKind::BadUnicode
						}
						else {TokenErrorKind::BadCharacter};
						self.clear();
						return Some(Err(self.positioned(err)));
					},
				}
			},
			b'A'..=b'Z' | b'_' | b'a'..=b'z' =>
			{
				let len = match self.data.bytes().position(|b| !matches!(b, b'$' | b'.' | b'0'..=b'9' | b'@' | b'A'..=b'Z' | b'_' | b'a'..=b'z'))
				{
					None if self.utf_err =>
					{
						// identifiers are ASCII so `str::bytes().count() == str::chars().count()`
						self.col = self.col.saturating_add(u32::try_from(self.data.len()).unwrap_or(u32::MAX));
						self.clear();
						return Some(Err(self.positioned(TokenErrorKind::BadUnicode)));
					},
					None => self.data.len(),
					Some(n) => n,
				};
				(len, true, TokenValue::Identifier(&self.data[..len]))
			},
			b'"' =>
			{
				let mut pos = 1usize;
				let mut escaped = String::new();
				loop
				{
					match self.data[pos..].bytes().position(|b| b == b'"' || b == b'\\' || (b < b' ' && b != b'\t') || b == 0x7F)
					{
						None =>
						{
							let err = if self.utf_err
							{
								self.update_pos(self.data);
								TokenErrorKind::BadUnicode
							}
							else {TokenErrorKind::BadString};
							self.clear();
							return Some(Err(self.positioned(err)));
						},
						Some(off) =>
						{
							let c = self.data.as_bytes()[pos + off];
							// simple condition, none of the false positives are matched by above `str::bytes().position()` call
							if c < b' ' && c >= 0x7F
							{
								self.clear();
								return Some(Err(self.positioned(TokenErrorKind::BadString)));
							}
							if c == b'\\'
							{
								if off > 0
								{
									escaped.push_str(&self.data[pos..pos + off]);
									pos += off;
								}
								// at least 3 characters are needed: backslash, escape character and closing quotation mark
								if self.data.len() - pos < 3
								{
									self.clear();
									return Some(Err(self.positioned(TokenErrorKind::BadString)));
								}
								match self.data.as_bytes()[pos + 1]
								{
									b'0' => escaped.push('\0'),
									b't' => escaped.push('\t'),
									b'n' => escaped.push('\n'),
									b'r' => escaped.push('\r'),
									c @ (b'"' | b'\'' | b'\\') => escaped.push(c as char),
									// safe index, we checked for an extra character (see above comment)
									b'u' if self.data.as_bytes()[pos + 2] == b'{' =>
									{
										// longest escape sequence is "\u{10FFFF}"
										match self.data[pos + 3..].bytes().take(7).position(|b| b == b'}')
										{
											None =>
											{
												let err = if self.utf_err
												{
													self.update_pos(self.data);
													TokenErrorKind::BadUnicode
												}
												else {TokenErrorKind::BadString};
												self.clear();
												return Some(Err(self.positioned(err)));
											},
											Some(end) =>
											{
												match u32::from_str_radix(&self.data[pos + 3..pos + 3 + end], 16).ok().and_then(char::from_u32)
												{
													Some(c) =>
													{
														escaped.push(c);
														pos += end + 2; // 2 curly brackets + character value
													},
													None =>
													{
														self.clear();
														return Some(Err(self.positioned(TokenErrorKind::BadString)));
													},
												}
											},
										}
									},
									_ =>
									{
										self.clear();
										return Some(Err(self.positioned(TokenErrorKind::BadString)));
									},
								}
								pos += 2; // backslash + escape character
							}
							else
							{
								assert_eq!(c, b'"');
								if !escaped.is_empty() && off > 0
								{
									escaped.push_str(&self.data[pos..pos + off]);
								}
								pos += off + 1; // no overflow because this is at most `self.data.len()`
								break;
							}
						},
					}
				}
				// it's simpler to just use `Self::update_pos()` instead of manually checking if the input (!) is an ASCII line
				(pos, false, TokenValue::String(if !escaped.is_empty() {Arcob::Arced(escaped.into())} else {Arcob::Borrowed(&self.data[1..pos - 1])}))
			},
			b'(' => (1, true, TokenValue::BeginGroup),
			b')' => (1, true, TokenValue::EndGroup),
			b'[' => (1, true, TokenValue::BeginAddr),
			b']' => (1, true, TokenValue::EndAddr),
			b'{' => (1, true, TokenValue::BeginSeq),
			b'}' => (1, true, TokenValue::EndSeq),
			_ =>
			{
				let c = self.data.chars().next().unwrap();
				self.clear();
				return Some(Err(self.positioned(TokenErrorKind::Unexpected(c))));
			},
		};
		let token = self.positioned(token);
		if ascii_ln
		{
			self.col = self.col.saturating_add(u32::try_from(n).unwrap_or(u32::MAX));
		}
		else
		{
			self.update_pos(&self.data[..n]);
		}
		self.data = &self.data[n..];
		Some(Ok(token))
	}
	
	pub fn peek<'s>(&'s mut self) -> Option<Result<&'s Token<'l>, &'s TokenError>>
	{
		if !self.tokens.is_empty()
		{
			return Some(Ok(self.tokens.front().unwrap()));
		}
		if let Some(ref e) = self.token_err {Some(Err(e))}
		else
		{
			match self.next_token()
			{
				None => None,
				Some(Ok(t)) =>
				{
					self.tokens.push_back(t);
					self.tokens.back().map(Result::Ok)
				},
				Some(Err(e)) =>
				{
					self.token_err = Some(e);
					self.token_err.as_ref().map(Result::Err)
				},
			}
		}
	}
	
	pub fn peek_nth(&mut self, idx: usize) -> Option<Result<&Token<'l>, &TokenError>>
	{
		while self.tokens.len() < idx
		{
			if self.token_err.is_some() {return None;}
			match self.next_token()
			{
				None => return None,
				Some(Ok(t)) => self.tokens.push_back(t),
				Some(Err(e)) => self.token_err = Some(e),
			}
		}
		if idx == self.tokens.len()
		{
			if let Some(ref e) = self.token_err {Some(Err(e))}
			else {None}
		}
		else
		{
			self.tokens.get(idx).map(Result::Ok)
		}
	}
}

impl<'l> Iterator for Tokenizer<'l>
{
	type Item = Result<Token<'l>, TokenError>;
	
	fn next(&mut self) -> Option<Self::Item>
	{
		match self.tokens.pop_front()
		{
			None =>
			{
				if let Some(e) = self.token_err.take() {Some(Err(e))}
				else {self.next_token()}
			},
			Some(t) => Some(Ok(t)),
		}
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
	BadString,
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
			TokenErrorKind::BadString => f.write_str("malformed string literal"),
			TokenErrorKind::Unexpected(c) => write!(f, "unexpected character {c:?}"),
		}
	}
}

impl Error for TokenErrorKind {}
