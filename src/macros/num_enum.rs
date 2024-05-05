macro_rules!numeric_enum
{
	{
		$(#[$ext:ident($($ext_args:tt)*)])*
		enum $tname:ident {$($var_name:ident $(= $var_val:literal)?),* $(,)?}
	} =>
	{
		// have to use something recursive because rust wants to zip `$ext` with `$var_name` otherwise (which extensions probably need)
		$crate::macros::numeric_enum!(@impl/ext $tname {$($var_name $(= $var_val)?),*} $({$ext($($ext_args)*)})*);
	};
	(@impl/ext $tname:ident {$($var_name:ident $(= $var_val:literal)?),*}) => {};
	{
		@impl/ext $tname:ident {$($var_name:ident $(= $var_val:literal)?),*}
		{$ext0:ident($($ext0_args:tt)*)} $($more_ext:tt)*
	} =>
	{
		$crate::macros::numeric_enum!(@impl/ext/$ext0 $tname {$($var_name $(= $var_val)?),*} ($($ext0_args)*));
		$crate::macros::numeric_enum!(@impl/ext $tname {$($var_name $(= $var_val)?),*} $($more_ext)*);
	};
	{
		@impl/ext/enum $tname:ident {$($var_name:ident $(= $var_val:literal)?),*}
		($vis:vis $numeric:ty $(, derive($derives:ident))*)
	} =>
	{
		#[repr($numeric)]
		#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd $(, $derives)*)]
		$vis enum $tname
		{
			$($var_name $(= $var_val)?,)+
		}
	};
	{
		@impl/ext/TryFrom $tname:ident {$($var_name:ident $(= $var_val:literal)?),*}
		($numeric:ty => $error:ident $(: $($feat:ident($($feat_args:tt)*)),+)?)
	} =>
	{
		$crate::macros::numeric_enum!(@impl/ext/TryFrom/feat $tname {$($var_name),*} $numeric => $error $($({$feat($($feat_args)*)})+)?);
		
		impl TryFrom<$numeric> for $tname
		{
			type Error = $error;
			
			#[allow(non_upper_case_globals)]
			fn try_from(value: $numeric) -> Result<Self, $error>
			{
				$(const $var_name: $numeric = $tname::$var_name as $numeric;)+
				match value
				{
					$($var_name => Ok(Self::$var_name),)+
					_ => Err($error(value)),
				}
			}
		}
	};
	{
		@impl/ext/TryFrom/feat $tname:ident {$($var_name:ident),*}
		$numeric:ty => $error:ident
	} => {};
	{
		@impl/ext/TryFrom/feat $tname:ident {$($var_name:ident),*}
		$numeric:ty => $error:ident {$feat0:ident($($feat0_args:tt)*)} $($more_feat:tt)*
	} =>
	{
		$crate::macros::numeric_enum!(@impl/ext/TryFrom/feat/$feat0 $tname {$($var_name),*} $numeric => $error ($($feat0_args)*));
		$crate::macros::numeric_enum!(@impl/ext/TryFrom/feat $tname {$($var_name),*} $numeric => $error $($more_feat)*);
	};
	{
		@impl/ext/TryFrom/feat/as $tname:ident {$($var_name:ident),*}
		$numeric:ty => $error:ident ($vis:vis struct)
	} =>
	{
		#[derive(Copy, Clone, Debug, Eq, PartialEq)]
		$vis struct $error($vis $numeric);
	};
	{
		@impl/ext/TryFrom/feat/derive $tname:ident {$($var_name:ident),*}
		$numeric:ty => $error:ident (Display)
	} =>
	{
		impl core::fmt::Display for $error
		{
			fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result
			{
				write!(f, "no variant of {} for value {}", stringify!($tname), self.0)
			}
		}
	};
	{
		@impl/ext/TryFrom/feat/derive $tname:ident {$($var_name:ident $(= $var_val:literal)?),*}
		$numeric:ty => $error:ident (Error)
	} =>
	{
		impl std::error::Error for $error {}
	};
	{
		@impl/ext/Into $tname:ident {$($var_name:ident $(= $var_val:literal)?),*}
		($numeric:ty)
	} =>
	{
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
