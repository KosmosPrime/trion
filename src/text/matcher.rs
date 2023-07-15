use crate::text::Positioned;
use crate::text::token::{TokenError, TokenErrorKind};

#[derive(Clone, Debug)]
pub struct Matcher<'l>
{
	data: &'l [u8],
	pos: usize,
	eof: bool,
	line: u32,
	col: u32,
}

impl<'l> Matcher<'l>
{
	pub fn new(data: &'l [u8]) -> Self
	{
		Self{data, pos: 0, eof: false, line: 1, col: 1}
	}
	
	pub fn data(&self) -> &'l [u8]
	{
		self.data
	}
	
	pub fn position(&self) -> usize
	{
		self.pos
	}
	
	pub fn line(&self) -> u32
	{
		self.line
	}
	
	pub fn column(&self) -> u32
	{
		self.col
	}
	
	pub fn positioned<T>(&self, value: T) -> Positioned<T>
	{
		Positioned{value, line: self.line, col: self.col}
	}
	
	pub fn clear(&mut self)
	{
		self.eof = true;
	}
	
	pub fn has_next(&self) -> bool
	{
		!self.eof && self.pos < self.data.len()
	}
	
	pub fn next(&mut self) -> Option<Result<char, TokenError>>
	{
		if !self.has_next()
		{
			return None;
		}
		let b0 = self.data[self.pos];
		Some(Ok(if b0 < 0x80
		{
			self.pos += 1;
			if b0 == b'\n'
			{
				self.col = 1;
				self.line = self.line.saturating_add(1);
			}
			else {self.col = self.col.saturating_add(1);}
			b0 as char
		}
		else
		{
			let mut val = (b0 & 0x3F) as u32;
			if (b0 & 0x40) != 0
			{
				if self.data.len() - self.pos < 2 || (self.data[self.pos + 1] & 0xC0) != 0x80
				{
					self.eof = true;
					return Some(Err(self.positioned(TokenErrorKind::BadUnicode)));
				}
				val = (val << 6) | (self.data[self.pos + 1] & 0x3F) as u32;
				
				if (b0 & 0x20) != 0
				{
					if self.data.len() - self.pos < 3 || (self.data[self.pos + 2] & 0xC0) != 0x80
					{
						self.eof = true;
						return Some(Err(self.positioned(TokenErrorKind::BadUnicode)));
					}
					val = ((val & 0b1111_111111) << 6) | (self.data[self.pos + 2] & 0x3F) as u32;
					
					if (b0 & 0x10) != 0
					{
						if self.data.len() - self.pos < 4 || (self.data[self.pos + 3] & 0xC0) != 0x80
						{
							self.eof = true;
							return Some(Err(self.positioned(TokenErrorKind::BadUnicode)));
						}
						val = ((val & 0b111_111111_111111) << 6) | (self.data[self.pos + 3] & 0x3F) as u32;
						
						if (b0 & 0x08) != 0 || val < 0x10000 || val > 0x10FFFF
						{
							self.eof = true;
							return Some(Err(self.positioned(TokenErrorKind::BadUnicode)));
						}
						else {self.pos += 4;}
					}
					else if val < 0x800 || (val >= 0xD800 && val <= 0xDFFF)
					{
						self.eof = true;
						return Some(Err(self.positioned(TokenErrorKind::BadUnicode)));
					}
					else {self.pos += 3;}
				}
				else if val < 0x80
				{
					self.eof = true;
					return Some(Err(self.positioned(TokenErrorKind::BadUnicode)));
				}
				else {self.pos += 2;}
			}
			else
			{
				self.eof = true;
				return Some(Err(self.positioned(TokenErrorKind::BadUnicode)));
			}
			self.col = self.col.saturating_add(1);
			char::from_u32(val).unwrap()
		}))
	}
	
	pub fn search(&mut self, needle: char) -> Result<Option<usize>, TokenError>
	{
		let mut cnt = 0usize;
		while let Some(curr) = self.next()
		{
			match curr
			{
				Ok(haystack) =>
				{
					if haystack == needle {return Ok(Some(cnt));}
					cnt += 1;
				},
				Err(e) => return Err(e),
			}
		}
		Ok(None)
	}
	
	pub fn since(&self, before: &Matcher<'l>) -> &'l str
	{
		if self.data.as_ptr() != before.data.as_ptr() || self.data.len() != before.data.len()
		{
			panic!("incompatible matchers");
		}
		if before.pos > self.pos
		{
			panic!("incorrect macher ordering");
		}
		// SAFETY: we've already checked each char as we processed them
		unsafe{core::str::from_utf8_unchecked(&self.data[before.pos..self.pos])}
	}
}

#[cfg(test)]
mod test
{
	use super::*;
	
	#[test]
	fn matcher_valid()
	{
		let mut tmp = [0u8; 4];
		for c in ['\0', '\u{7F}', '\u{80}', '\u{7FF}', '\u{800}', '\u{D7FF}', '\u{E000}', '\u{FFFF}', '\u{10000}', '\u{10FFFF}']
		{
			let mut m = Matcher::new(c.encode_utf8(&mut tmp).as_bytes());
			assert_eq!(m.next(), Some(Ok(c)), "for value {}", c as u32);
			assert_eq!(m.next(), None);
		}
	}
	
	#[test]
	fn matcher_overlong()
	{
		for i in [0u32, 0x7Fu32]
		{
			let input = [
				0xC0 | ((i >> 6) & 0x1F) as u8, 0x80 | ((i >> 0) & 0x3F) as u8,
				0xE0 | ((i >> 12) & 0x0F) as u8, 0x80 | ((i >> 6) & 0x3F) as u8, 0x80 | ((i >> 0) & 0x3F) as u8,
				0xF0 | ((i >> 18) & 0x07) as u8, 0x80 | ((i >> 12) & 0x3F) as u8, 0x80 | ((i >> 6) & 0x3F) as u8, 0x80 | ((i >> 0) & 0x3F) as u8,
			];
			let mut m = Matcher::new(&input[0..2]);
			assert_eq!(m.next().unwrap().unwrap_err().value, TokenErrorKind::BadUnicode);
			assert_eq!(m.next(), None);
			let mut m = Matcher::new(&input[2..5]);
			assert_eq!(m.next().unwrap().unwrap_err().value, TokenErrorKind::BadUnicode);
			assert_eq!(m.next(), None);
			let mut m = Matcher::new(&input[5..9]);
			assert_eq!(m.next().unwrap().unwrap_err().value, TokenErrorKind::BadUnicode);
			assert_eq!(m.next(), None);
		}
		for i in [0x80u32, 0x7FFu32]
		{
			let input = [
				0xE0 | ((i >> 12) & 0x0F) as u8, 0x80 | ((i >> 6) & 0x3F) as u8, 0x80 | ((i >> 0) & 0x3F) as u8,
				0xF0 | ((i >> 18) & 0x07) as u8, 0x80 | ((i >> 12) & 0x3F) as u8, 0x80 | ((i >> 6) & 0x3F) as u8, 0x80 | ((i >> 0) & 0x3F) as u8,
			];
			let mut m = Matcher::new(&input[0..3]);
			assert_eq!(m.next().unwrap().unwrap_err().value, TokenErrorKind::BadUnicode);
			assert_eq!(m.next(), None);
			let mut m = Matcher::new(&input[3..7]);
			assert_eq!(m.next().unwrap().unwrap_err().value, TokenErrorKind::BadUnicode);
			assert_eq!(m.next(), None);
		}
		for i in [0x800u32, 0xD7FFu32, 0xE000u32, 0xFFFFu32]
		{
			let input = [
				0xF0 | ((i >> 18) & 0x07) as u8, 0x80 | ((i >> 12) & 0x3F) as u8, 0x80 | ((i >> 6) & 0x3F) as u8, 0x80 | ((i >> 0) & 0x3F) as u8,
			];
			let mut m = Matcher::new(&input[0..4]);
			assert_eq!(m.next().unwrap().unwrap_err().value, TokenErrorKind::BadUnicode);
			assert_eq!(m.next(), None);
		}
	}
	
	#[test]
	fn matcher_surrogate()
	{
		for i in [0xD800u32, 0xDBFFu32, 0xDC00u32, 0xDFFFu32]
		{
			let input = [0xE0 | ((i >> 12) & 0x0F) as u8, 0x80 | ((i >> 6) & 0x3F) as u8, 0x80 | ((i >> 0) & 0x3F) as u8];
			let mut m = Matcher::new(&input);
			assert_eq!(m.next().unwrap().unwrap_err().value, TokenErrorKind::BadUnicode);
			assert_eq!(m.next(), None);
		}
	}
	
	#[test]
	fn matcher_truncated()
	{
		let mut tmp = [0u8; 4];
		for c in ['\u{80}', '\u{7FF}', '\u{800}', '\u{D7FF}', '\u{E000}', '\u{FFFF}', '\u{10000}', '\u{10FFFF}']
		{
			let len = c.encode_utf8(&mut tmp).len();
			let mut m = Matcher::new(&tmp[..len - 1]);
			assert_eq!(m.next().unwrap().unwrap_err().value, TokenErrorKind::BadUnicode);
			assert_eq!(m.next(), None);
		}
	}
	
	#[test]
	fn matcher_illegal_start()
	{
		for i in (0x80..0xC2).into_iter().chain(0xF5..0x100)
		{
			let input = [i as u8, 0x80u8, 0x80u8, 0x80u8];
			let mut m = Matcher::new(&input);
			assert_eq!(m.next().unwrap().unwrap_err().value, TokenErrorKind::BadUnicode);
		}
	}
}
