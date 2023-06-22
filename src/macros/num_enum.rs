macro_rules!numeric_enum
{
	($vis:vis enum $tname:ident for $numeric:ty | $error:ident {$($name:ident $(= $val:literal)?),* $(,)?}) =>
	{
		crate::macros::numeric_enum!($vis enum $tname for $numeric | $error? {$($name $(= $val)?),*});
		
		#[derive(Copy, Clone, Debug, Eq, PartialEq)]
		$vis struct $error($vis $numeric);
		
		impl core::fmt::Display for $error
		{
			fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result
			{
				write!(f, "no variant of {} for value {}", stringify!($tname), self.0)
			}
		}
		
		impl std::error::Error for $error {}
	};
	($vis:vis enum $tname:ident for $numeric:ty | $error:ident? {$($name:ident $(= $val:literal)?),* $(,)?}) =>
	{
		#[repr($numeric)]
		#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
		$vis enum $tname
		{
			$($name $(= $val)?,)+
		}
		
		impl TryFrom<$numeric> for $tname
		{
			type Error = $error;
			
			#[allow(non_upper_case_globals)]
			fn try_from(value: $numeric) -> Result<Self, $error>
			{
				$(const $name: $numeric = $tname::$name as $numeric;)+
				match value
				{
					$($name => Ok(Self::$name),)+
					_ => Err($error(value)),
				}
			}
		}
		
		impl From<$tname> for $numeric
		{
			fn from(value: $tname) -> $numeric
			{
				value as $numeric
			}
		}
	};
}
pub(crate) use numeric_enum;
