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

impl<T> Positioned<T>
{
	pub fn convert<R>(&self, value: R) -> Positioned<R>
	{
		Positioned{value, line: self.line, col: self.col}
	}
	
	pub fn convert_fn<R>(self, func: impl FnOnce(T) -> R) -> Positioned<R>
	{
		let Positioned{value, line, col} = self;
		Positioned{value: func(value), line, col}
	}
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
