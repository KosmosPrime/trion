use core::fmt;
use std::error::Error;

pub mod matcher;
pub mod parse;
pub mod token;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Positioned<T: ?Sized>
{
	pub line: u32,
	pub col: u32,
	pub value: T,
}

impl<T> Positioned<T>
{
	pub fn with_name(self, name: String) -> PosNamed<T>
	{
		PosNamed
		{
			name,
			line: self.line,
			col: self.col,
			value: self.value,
		}
	}
	
	pub fn convert<R>(&self, value: R) -> Positioned<R>
	{
		Positioned{line: self.line, col: self.col, value}
	}
	
	pub fn convert_fn<R>(self, func: impl FnOnce(T) -> R) -> Positioned<R>
	{
		let Positioned{line, col, value} = self;
		Positioned{line, col, value: func(value)}
	}
}

impl<T> From<PosNamed<T>> for Positioned<T>
{
	fn from(value: PosNamed<T>) -> Self
	{
		Positioned
		{
			line: value.line,
			col: value.col,
			value: value.value,
		}
	}
}

impl<T: fmt::Display + ?Sized> fmt::Display for Positioned<T>
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		write!(f, "{} ({}:{})", &self.value, self.line, self.col)
	}
}

impl<T: Error + ?Sized> Error for Positioned<T>
{
	fn source(&self) -> Option<&(dyn Error + 'static)>
	{
		self.value.source()
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PosNamed<T: ?Sized>
{
	pub name: String,
	pub line: u32,
	pub col: u32,
	pub value: T,
}

impl<T: fmt::Display + ?Sized> fmt::Display for PosNamed<T>
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		write!(f, "{} ({}:{}:{})", &self.value, self.name, self.line, self.col)
	}
}

impl<T: Error + ?Sized> Error for PosNamed<T>
{
	fn source(&self) -> Option<&(dyn Error + 'static)>
	{
		self.value.source()
	}
}
