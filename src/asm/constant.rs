use core::fmt;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Realm
{
	Global, Local,
}

impl fmt::Display for Realm
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Global => f.write_str("global"),
			Self::Local => f.write_str("local"),
		}
	}
}

#[derive(Clone, Copy, Debug)]
pub enum Lookup
{
	NotFound,
	Deferred,
	Found(i64),
}

impl Lookup
{
	pub fn exists(&self) -> bool
	{
		match self
		{
			Self::NotFound => false,
			_ => true,
		}
	}
	
	pub fn value(&self) -> Option<i64>
	{
		match self
		{
			&Self::Found(v) => Some(v),
			_ => None,
		}
	}
	
	pub fn unwrap(self) -> i64
	{
		match self
		{
			Self::NotFound => panic!("called `Lookup::unwrap()` on a `NotFound` value"),
			Self::Deferred => panic!("called `Lookup::unwrap()` on a `None` value"),
			Self::Found(v) => v,
		}
	}
	
	pub fn is_deferred(&self) -> bool
	{
		match self
		{
			Self::Deferred => true,
			_ => false,
		}
	}
}
