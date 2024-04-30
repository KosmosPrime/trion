use core::ops::Deref;
use std::sync::Arc;

#[derive(Debug)]
pub enum Arcob<'r, T: ?Sized + 'r>
{
	Borrowed(&'r T),
	Arced(Arc<T>),
}

impl<'r, T: ?Sized + 'r> Arcob<'r, T> where Arc<T>: From<&'r T>
{
	pub fn make_arced(&mut self)
	{
		if let Self::Borrowed(r) = *self
		{
			*self = Self::Arced(Arc::from(r));
		}
	}
	
	pub fn to_arc(&self) -> Arc<T>
	{
		match self
		{
			Self::Borrowed(r) => Arc::from(*r),
			Self::Arced(arc) => arc.clone(),
		}
	}
	
	pub fn into_arc(self) -> Arc<T>
	{
		match self
		{
			Self::Borrowed(r) => Arc::from(r),
			Self::Arced(arc) => arc,
		}
	}
}

impl<'r, T: ?Sized + 'r> From<&'r T> for Arcob<'r, T>
{
	fn from(value: &'r T) -> Self
	{
		Self::Borrowed(value)
	}
}

impl<'r, T: ?Sized + 'r> From<Arc<T>> for Arcob<'r, T>
{
	fn from(value: Arc<T>) -> Self
	{
		Self::Arced(value)
	}
}

impl<'r, T: ?Sized + 'r> AsRef<T> for Arcob<'r, T>
{
	fn as_ref(&self) -> &T
	{
		match self
		{
			Self::Borrowed(r) => r,
			Self::Arced(arc) => arc.as_ref(),
		}
	}
}

impl<'r, T: ?Sized + 'r> Clone for Arcob<'r, T>
{
	fn clone(&self) -> Self
	{
		match self
		{
			Self::Borrowed(r) => Self::Borrowed(*r),
			Self::Arced(arc) => Self::Arced(arc.clone()),
		}
	}
}

impl<'r, T: ?Sized + 'r> Deref for Arcob<'r, T>
{
	type Target = T;
	
	fn deref(&self) -> &Self::Target
	{
		self.as_ref()
	}
}

impl<'a, 'b, A: ?Sized + 'a, B: ?Sized + 'b> PartialEq<Arcob<'b, B>> for Arcob<'a, A> where A: PartialEq<B>
{
	fn eq(&self, other: &Arcob<'b, B>) -> bool
	{
		PartialEq::eq(self.as_ref(), other.as_ref())
	}
}

impl<'r, T: ?Sized + 'r> Eq for Arcob<'r, T> where T: Eq {}
