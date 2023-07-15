use core::fmt;
use std::error::Error;

pub mod matcher;
pub mod parse;
pub mod token;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Positioned<T>
{
	pub value: T,
	pub line: u32,
	pub col: u32,
}

impl<T: fmt::Display> fmt::Display for Positioned<T>
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		write!(f, "{} ({}:{})", self.value, self.line, self.col)
	}
}

impl<T: Error> Error for Positioned<T>
{
	fn source(&self) -> Option<&(dyn Error + 'static)>
	{
		self.value.source()
	}
}
