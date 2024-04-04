use crate::text::Positioned;
use crate::text::parse::{ElementValue, Parser};

use super::*;

macro_rules!expect
{
	($($text:expr => {$($tvar:tt)+} in {$($func:tt)*}),+ $(,)?) =>
	{
		{
			let text = expect!(@expand $($text),+);
			let mut p = Parser::new(text.as_bytes());
			let element = match p.next()
			{
				None => panic!("no token in {text:?}"),
				Some(Ok(v)) => v,
				Some(Err(e)) => panic!("could not parse {text:?}: {e:?}"),
			};
			let Positioned{value: ElementValue::Instruction{args, ..}, ..} = element else {panic!("parsed {element:?}")};
			assert_eq!(args.len(), expect!(@count $({$text})+));
			let mut iter = args.into_iter();
			$({expect!(@validate iter: $($tvar)+ in {$($func)*});})+
			assert!(iter.next().is_none());
		}
	};
	(@validate $iter:ident: Ok($tok:ident) in {$($func:tt)*}) =>
	{
		let Some(mut $tok) = $iter.next() else {panic!("size mismatch")};
		if let Err(e) = simplify(&mut $tok) {panic!("could not simplify {:?}: {e:?}", $tok);}
		$($func)*
	};
	(@validate $iter:ident: Err($tok:ident, $terr:ident) in {$($func:tt)*}) =>
	{
		let Some(mut $tok) = $iter.next() else {panic!("size mismatch")};
		match simplify(&mut $tok)
		{
			Ok(()) => panic!("simplify succeeded, got {:?}", $tok),
			Err($terr) => {$($func)*},
		}
	};
	(@expand $first:expr $(, $more:expr)*) =>
	{
		concat!("TEST ", $first $(, ", ", $more)*, ";")
	};
	(@count $({$($inner:tt)*})*) =>
	{
		[$(expect!(@count/map {$($inner)*})),*].len()
	};
	(@count/map {$($_:tt)*}) => {()};
}

#[test]
fn simplify_str()
{
	expect!
	{
		"1 + \"fail\"" => {Err(v, e)} in
		{
			assert!(matches!(e, SimplifyError::BadType{kind: ArgumentType::String, op: ArgumentType::Add}), "{v:?} -> {e:?}");
		},
		"\"fail\" * 2" => {Err(v, e)} in
		{
			assert!(matches!(e, SimplifyError::BadType{kind: ArgumentType::String, op: ArgumentType::Multiply}), "{v:?} -> {e:?}");
		},
		"\"hello\" + \"world\"" => {Err(v, e)} in
		{
			assert!(matches!(e, SimplifyError::BadType{kind: ArgumentType::String, op: ArgumentType::Add}), "{v:?} -> {e:?}");
		},
		"reg << \"world\"" => {Err(v, e)} in
		{
			assert!(matches!(e, SimplifyError::BadType{kind: ArgumentType::String, op: ArgumentType::LeftShift}), "{v:?} -> {e:?}");
		},
		"5 - 2 & \"order\"" => {Err(v, e)} in
		{
			let Argument::BitAnd(ref args) = v else {panic!("structure error, got {v:?}")};
			assert_eq!(args.len(), 2);
			assert!(matches!(args[0], Argument::Constant(Number::Integer(3))), "{:?}", args[0]);
			assert!(matches!(args[1], Argument::String(ref s) if s == "order"), "{:?}", args[1]);
			assert!(matches!(e, SimplifyError::BadType{kind: ArgumentType::String, op: ArgumentType::BitAnd}), "{v:?} -> {e:?}");
		},
		"[\"insane\"]" => {Err(v, e)} in
		{
			assert!(matches!(e, SimplifyError::BadType{kind: ArgumentType::String, op: ArgumentType::Address}), "{v:?} -> {e:?}");
		},
	}
}

#[test]
fn simplify_addsub()
{
	expect!
	{
		"2 + 3 + pre" => {Ok(val)} in
		{
			let Argument::Add(ref args) = val else {panic!("{val:?}")};
			assert_eq!(args.len(), 2);
			assert!(matches!(args[0], Argument::Constant(Number::Integer(5))), "{:?}", args[0]);
			assert!(matches!(args[1], Argument::Identifier("pre")), "{:?}", args[1]);
		},
		"post + 2 + 3" => {Ok(val)} in
		{
			let Argument::Add(ref args) = val else {panic!("{val:?}")};
			assert_eq!(args.len(), 2);
			assert!(matches!(args[0], Argument::Identifier("post")), "{:?}", args[0]);
			assert!(matches!(args[1], Argument::Constant(Number::Integer(5))), "{:?}", args[1]);
		},
		"in + 2 + 3 + ner" => {Ok(val)} in
		{
			let Argument::Add(ref args) = val else {panic!("{val:?}")};
			assert_eq!(args.len(), 3);
			assert!(matches!(args[0], Argument::Identifier("in")), "{:?}", args[0]);
			assert!(matches!(args[1], Argument::Constant(Number::Integer(5))), "{:?}", args[1]);
			assert!(matches!(args[2], Argument::Identifier("ner")), "{:?}", args[2]);
		},
		"2 + 3 + outer + 6" => {Ok(val)} in
		{
			let Argument::Add(ref args) = val else {panic!("{val:?}")};
			assert_eq!(args.len(), 2);
			assert!(matches!(args[0], Argument::Constant(Number::Integer(11))), "{:?}", args[0]);
			assert!(matches!(args[1], Argument::Identifier("outer")), "{:?}", args[1]);
		},
		"((2 + 3) + 4 - 1)" => {Ok(val)} in
		{
			#[allow(unused_parens)]
			const EXPECT: i64 = ((2 + 3) + 4 - 1);
			assert!(matches!(val, Argument::Constant(Number::Integer(EXPECT))), "{val:?}");
		},
		"9 - 3 - 7 + 1 - 5 - -2" => {Ok(val)} in
		{
			const EXPECT: i64 = 9 - 3 - 7 + 1 - 5 - -2;
			assert!(matches!(val, Argument::Constant(Number::Integer(EXPECT))), "{val:?}");
		},
	};
}

#[test]
fn simplify_muldiv()
{
	expect!
	{
		"8 * 7 * 6 * 5 * 4 * 3 * 2 * 1" => {Ok(val)} in
		{
			assert!(matches!(val, Argument::Constant(Number::Integer(40320))), "{val:?}");
		},
		"1234 / 11 / 4" => {Ok(val)} in
		{
			const EXPECT: i64 = 1234 / 44;
			assert!(matches!(val, Argument::Constant(Number::Integer(EXPECT))), "{val:?}");
		},
		"567 % 10" => {Ok(val)} in
		{
			assert!(matches!(val, Argument::Constant(Number::Integer(7))), "{val:?}");
		},
		"1 / 0" => {Err(v, e)} in
		{
			assert!(matches!(e, SimplifyError::Overflow(OverflowError::DivideByZero(Number::Integer(1)))), "{v:?} -> {e:?}");
		},
		"0 / 0" => {Err(v, e)} in
		{
			assert!(matches!(e, SimplifyError::Overflow(OverflowError::DivideByZero(Number::Integer(0)))), "{v:?} -> {e:?}");
		},
		"1 % 0" => {Err(v, e)} in
		{
			assert!(matches!(e, SimplifyError::Overflow(OverflowError::ModuloByZero(Number::Integer(1)))), "{v:?} -> {e:?}");
		},
	};
}

#[test]
fn simplify_bits()
{
	expect!
	{
		"0x5555 & 0x3FFF ^ (!0x0300 & 0x0F00) ^ 0x0030 | 0x0003" => {Ok(val)} in
		{
			assert!(matches!(val, Argument::Constant(Number::Integer(0b0001_1001_0110_0111))), "{val:?}");
		},
		"!!!!123" => {Ok(val)} in
		{
			assert!(matches!(val, Argument::Constant(Number::Integer(123))), "{val:?}");
		},
		"0b1010 << 16 << 16 >> 34" => {Ok(val)} in
		{
			assert!(matches!(val, Argument::Constant(Number::Integer(2))), "{val:?}");
		},
		// no overflow-based tests because that's not something we want to rely on
	};
}

#[test]
fn simplify_expr()
{
	expect!
	{
		"(0xDE << 24) | (0xAD << 16) | (0xF0 << 8) | (0x0D << 0)" => {Ok(val)} in
		{
			assert!(matches!(val, Argument::Constant(Number::Integer(0xDEADF00D))), "{val:?}");
		},
		"((((0xDEADF00D >> 1 & 0x55555555) + (0xDEADF00D & 0x55555555) >> 2 & 0x33333333) \
			+ ((0xDEADF00D >> 1 & 0x55555555) + (0xDEADF00D & 0x55555555) & 0x33333333) >> 4 & 0x0F0F0F0F) \
		+ (((0xDEADF00D >> 1 & 0x55555555) + (0xDEADF00D & 0x55555555) >> 2 & 0x33333333) \
			+ ((0xDEADF00D >> 1 & 0x55555555) + (0xDEADF00D & 0x55555555) & 0x33333333) & 0x0F0F0F0F \
		)) * 0x01010101 >> 24 & 0xFF" => {Ok(val)} in
		{
			const EXPECT: i64 = ((((0xDEADF00D >> 1 & 0x55555555) + (0xDEADF00D & 0x55555555) >> 2 & 0x33333333)
				+ ((0xDEADF00D >> 1 & 0x55555555) + (0xDEADF00D & 0x55555555) & 0x33333333) >> 4 & 0x0F0F0F0F)
			+ (((0xDEADF00D >> 1 & 0x55555555) + (0xDEADF00D & 0x55555555) >> 2 & 0x33333333)
				+ ((0xDEADF00D >> 1 & 0x55555555) + (0xDEADF00D & 0x55555555) & 0x33333333) & 0x0F0F0F0F
			)) * 0x01010101 >> 24 & 0xFF;
			assert_eq!(EXPECT, 0xDEADF00Di64.count_ones() as i64);
			assert!(matches!(val, Argument::Constant(Number::Integer(EXPECT))), "{val:?}");
		},
		"(1 << 8 + 8) * 33 - 89 * 33 & (0b1000 | 2) * 0x1111 | 222" => {Ok(val)} in
		{
			const EXPECT: i64 = (1 << 8 + 8) * 33 - 89 * 33 & (0b1000 | 2) * 0x1111 | 222;
			assert!(matches!(val, Argument::Constant(Number::Integer(EXPECT))), "{val:?}");
		},
	};
}
