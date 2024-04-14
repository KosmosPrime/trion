use crate::asm::{Context, ErrorLevel};
use crate::asm::constant::Realm;
use crate::asm::directive::DirectiveList;
use crate::asm::instr::InstructionSet;
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
			$({expect!(@validate/simplify iter: $($tvar)+ in {$($func)*});})+
			assert!(iter.next().is_none());
		}
	};
	(@validate/simplify $iter:ident: Ok($tok:ident) in {$($func:tt)*}) =>
	{
		let Some(mut $tok) = $iter.next() else {panic!("size mismatch")};
		if let Err(e) = simplify(&mut $tok) {panic!("could not simplify {:?}: {e:?}", $tok);}
		$($func)*
	};
	(@validate/simplify $iter:ident: Err($tok:ident, $terr:ident) in {$($func:tt)*}) =>
	{
		let Some(mut $tok) = $iter.next() else {panic!("size mismatch")};
		match simplify(&mut $tok)
		{
			Ok(()) => panic!("simplify succeeded, got {:?}", $tok),
			Err($terr) => {$($func)*},
		}
	};
	($($text:expr, $ctx:expr => {$($tvar:tt)+} in {$($func:tt)*}),+ $(,)?) =>
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
			$({expect!(@validate/evaluate iter, ($ctx): $($tvar)+ in {$($func)*});})+
			assert!(iter.next().is_none());
		}
	};
	(@validate/evaluate $iter:ident, ($ctx:expr): Ok($tok:ident, $tres:ident) in {$($func:tt)*}) =>
	{
		let Some(mut $tok) = $iter.next() else {panic!("size mismatch")};
		match evaluate(&mut $tok, &$ctx)
		{
			Ok($tres) => {$($func)*},
			Err(e) => panic!("could not evaluate {:?}: {e:?}", $tok),
		}
	};
	(@validate/evaluate $iter:ident, ($ctx:expr): Err($tok:ident, $terr:ident) in {$($func:tt)*}) =>
	{
		let Some(mut $tok) = $iter.next() else {panic!("size mismatch")};
		match evaluate(&mut $tok, &$ctx)
		{
			Ok(r) => panic!("evaluate succeeded (with {r:?}), got {:?}", $tok),
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
			let Argument::BitAnd{ref lhs, ref rhs} = v else {panic!("structure error, got {v:?}")};
			assert!(matches!(**lhs, Argument::Constant(Number::Integer(3))), "{lhs:?}");
			assert!(matches!(**rhs, Argument::String(ref s) if s == "order"), "{rhs:?}");
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
			let Argument::Add{ref lhs, ref rhs} = val else {panic!("{val:?}")};
			assert!(matches!(**lhs, Argument::Constant(Number::Integer(5))), "{lhs:?}");
			assert!(matches!(**rhs, Argument::Identifier("pre")), "{rhs:?}");
		},
		"post + 2 + 3" => {Ok(val)} in
		{
			let Argument::Add{ref lhs, ref rhs} = val else {panic!("{val:?}")};
			assert!(matches!(**lhs, Argument::Identifier("post")), "{lhs:?}");
			assert!(matches!(**rhs, Argument::Constant(Number::Integer(5))), "{rhs:?}");
		},
		"in + 2 + 3 + ner" => {Ok(val)} in
		{
			let Argument::Add{ref lhs, ref rhs} = val else {panic!("{val:?}")};
			let Argument::Add{lhs: ref llhs, rhs: ref rlhs} = **lhs else {panic!("{val:?}")};
			assert!(matches!(**llhs, Argument::Identifier("in")), "{llhs:?}");
			assert!(matches!(**rlhs, Argument::Constant(Number::Integer(5))), "{rlhs:?}");
			assert!(matches!(**rhs, Argument::Identifier("ner")), "{rhs:?}");
		},
		"2 + 3 + outer + 6" => {Ok(val)} in
		{
			let Argument::Add{ref lhs, ref rhs} = val else {panic!("{val:?}")};
			assert!(matches!(**lhs, Argument::Constant(Number::Integer(11))), "{lhs:?}");
			assert!(matches!(**rhs, Argument::Identifier("outer")), "{rhs:?}");
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
		"2 - start - 3" => {Ok(val)} in
		{
			let Argument::Subtract{ref lhs, ref rhs} = val else {panic!("{val:?}")};
			assert!(matches!(**lhs, Argument::Constant(Number::Integer(-1))), "{lhs:?}");
			assert!(matches!(**rhs, Argument::Identifier("start")), "{rhs:?}");
		},
		"start - 2 - 3" => {Ok(val)} in
		{
			let Argument::Subtract{ref lhs, ref rhs} = val else {panic!("{val:?}")};
			assert!(matches!(**lhs, Argument::Identifier("start")), "{lhs:?}");
			assert!(matches!(**rhs, Argument::Constant(Number::Integer(5))), "{rhs:?}");
		},
		"(v0 - 2) + (3 - v1)" => {Ok(val)} in
		{
			let Argument::Subtract{ref lhs, ref rhs} = val else {panic!("{val:?}")};
			let Argument::Add{lhs: ref llhs, rhs: ref rlhs} = **lhs else {panic!("{lhs:?}")};
			assert!(matches!(**llhs, Argument::Identifier("v0")), "{llhs:?}");
			assert!(matches!(**rlhs, Argument::Constant(Number::Integer(1))), "{rlhs:?}");
			assert!(matches!(**rhs, Argument::Identifier("v1")), "{rhs:?}");
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
		"lhs / 7 / rhs / 13" => {Ok(val)} in
		{
			const EXPECT: i64 = 7 * 13;
			let Argument::Divide{ref lhs, ref rhs} = val else {panic!("{val:?}")};
			let Argument::Divide{lhs: ref llhs, rhs: ref rlhs} = **lhs else {panic!("{lhs:?}")};
			assert!(matches!(**llhs, Argument::Identifier("lhs")), "{llhs:?}");
			assert!(matches!(**rlhs, Argument::Constant(Number::Integer(EXPECT))), "{rlhs:?}");
			assert!(matches!(**rhs, Argument::Identifier("rhs")), "{rhs:?}");
		},
		"14 / lhs / rhs / 13" => {Ok(val)} in
		{
			const EXPECT: i64 = 14 / 13;
			let Argument::Divide{ref lhs, ref rhs} = val else {panic!("{val:?}")};
			let Argument::Divide{lhs: ref llhs, rhs: ref rlhs} = **lhs else {panic!("{lhs:?}")};
			assert!(matches!(**llhs, Argument::Constant(Number::Integer(EXPECT))), "{llhs:?}");
			assert!(matches!(**rlhs, Argument::Identifier("lhs")), "{rlhs:?}");
			assert!(matches!(**rhs, Argument::Identifier("rhs")), "{rhs:?}");
		},
		"567 % 10" => {Ok(val)} in
		{
			assert!(matches!(val, Argument::Constant(Number::Integer(7))), "{val:?}");
		},
		"value % 10 % 11" => {Ok(val)} in
		{
			let Argument::Modulo{ref lhs, ref rhs} = val else {panic!("{val:?}")};
			assert!(matches!(**lhs, Argument::Identifier("value")), "{lhs:?}");
			assert!(matches!(**rhs, Argument::Constant(Number::Integer(10))), "{rhs:?}");
		},
		"1 / 0" => {Err(v, e)} in
		{
			assert!(matches!(e, SimplifyError::Overflow(OverflowError::DivideByZero(Number::Integer(1)))), "{v:?} -> {e:?}");
		},
		"0 / 0" => {Err(v, e)} in
		{
			assert!(matches!(e, SimplifyError::Overflow(OverflowError::DivideByZero(Number::Integer(0)))), "{v:?} -> {e:?}");
		},
		"value / 0" => {Ok(val)} in
		{
			let Argument::Divide{ref lhs, ref rhs} = val else {panic!("{val:?}")};
			assert!(matches!(**lhs, Argument::Identifier("value")), "{lhs:?}");
			assert!(matches!(**rhs, Argument::Constant(Number::Integer(0))), "{rhs:?}");
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
		"(value / 4 + 0) / 2" => {Ok(val)} in
		{
			let Argument::Divide{ref lhs, ref rhs} = val else {panic!("{val:?}")};
			assert!(matches!(**lhs, Argument::Identifier("value")), "{lhs:?}");
			assert!(matches!(**rhs, Argument::Constant(Number::Integer(8))), "{rhs:?}");
		},
	};
}

struct FakeIsa;

impl InstructionSet for FakeIsa
{
	fn is_register(&self, name: &str) -> bool
	{
		// anything of the form /^r\d+$/i
		name.len() >= 2 && name.as_bytes()[0].to_ascii_lowercase() == b'r' && name.as_bytes()[1..].iter().all(|b| b.is_ascii_digit())
	}
	
	fn assemble<'c>(&self, _: &'c mut Context, _: u32, _: u32, _: &str, _: Vec<Argument>) -> Result<(), ErrorLevel>
	{
		panic!() // assembling not supported
	}
}

fn setup_context() -> Context<'static>
{
	let directives = Box::leak(Box::new(DirectiveList::new()));
	let mut ctx = Context::new(&FakeIsa, directives);
	assert!(ctx.insert_constant("zero", 0, Realm::Global).unwrap());
	assert!(ctx.insert_constant("one", 1, Realm::Global).unwrap());
	assert!(ctx.insert_constant("two", 2, Realm::Global).unwrap());
	assert!(ctx.insert_constant("four", 4, Realm::Global).unwrap());
	ctx.defer_constant("paragraph", Realm::Global).unwrap();
	ctx.defer_constant("base", Realm::Global).unwrap();
	assert!(ctx.insert_constant("addr1", 0x10000000, Realm::Global).unwrap());
	assert!(ctx.insert_constant("addr2", 0x20000000, Realm::Global).unwrap());
	ctx
}

#[test]
fn evaluate_ok()
{
	let ctx = setup_context();
	expect!
	{
		"zero", ctx => {Ok(val, r)} in
		{
			assert_eq!(r, Evaluation::Complete);
			assert!(matches!(val, Argument::Constant(Number::Integer(0))), "{val:?}");
		},
		"two + 3", ctx => {Ok(val, r)} in
		{
			assert_eq!(r, Evaluation::Complete);
			assert!(matches!(val, Argument::Constant(Number::Integer(5))), "{val:?}");
		},
		"3 * two << 4", ctx => {Ok(val, r)} in
		{
			assert_eq!(r, Evaluation::Complete);
			const EXPECT: i64 = 3 * 2 << 4;
			assert!(matches!(val, Argument::Constant(Number::Integer(EXPECT))), "{val:?}");
		},
		"addr1 + r0 * 4 + 3", ctx => {Ok(val, r)} in
		{
			assert_eq!(r, Evaluation::Complete);
			let Argument::Add{ref lhs, ref rhs} = val else {panic!("{val:?}")};
			assert!(matches!(**lhs, Argument::Constant(Number::Integer(0x10000003))), "{lhs:?}");
			let Argument::Multiply{lhs: ref llhs, rhs: ref rlhs} = **rhs else {panic!("{rhs:?}")};
			assert!(matches!(**llhs, Argument::Identifier("r0")), "{llhs:?}");
			assert!(matches!(**rlhs, Argument::Constant(Number::Integer(4))), "{rlhs:?}");
		},
		"base + r0 * 4 + 3", ctx => {Ok(val, r)} in
		{
			assert_eq!(r, Evaluation::Deferred);
			let Argument::Add{lhs: ref mid, ref rhs} = val else {panic!("{val:?}")};
			let Argument::Add{ref lhs, rhs: ref mhs} = **mid else {panic!("{mid:?}")};
			assert!(matches!(**lhs, Argument::Identifier("base")), "{lhs:?}");
			let Argument::Multiply{lhs: ref lmhs, rhs: ref rmhs} = **mhs else {panic!("{mhs:?}")};
			assert!(matches!(**lmhs, Argument::Identifier("r0")), "{lmhs:?}");
			assert!(matches!(**rmhs, Argument::Constant(Number::Integer(4))), "{rmhs:?}");
			assert!(matches!(**rhs, Argument::Constant(Number::Integer(3))), "{rhs:?}");
		},
		"paragraph % zero", ctx => {Ok(val, r)} in
		{
			assert_eq!(r, Evaluation::Deferred);
			let Argument::Modulo{ref lhs, ref rhs} = val else {panic!("{val:?}")};
			assert!(matches!(**lhs, Argument::Identifier("paragraph")), "{lhs:?}");
			assert!(matches!(**rhs, Argument::Constant(Number::Integer(0))), "{rhs:?}");
		}
	};
}

#[test]
fn evaluate_err()
{
	let ctx = setup_context();
	expect!
	{
		"nope", ctx => {Err(v, e)} in
		{
			assert!(matches!(e, EvalError::NoSuchVariable{ref name, realm: Realm::Local} if name == "nope"), "{v:?} -> {e:?}");
		},
		"1 + r0 * nope", ctx => {Err(v, e)} in
		{
			assert!(matches!(e, EvalError::NoSuchVariable{ref name, realm: Realm::Local} if name == "nope"), "{v:?} -> {e:?}");
		},
		"one / zero", ctx => {Err(v, e)} in
		{
			assert!(matches!(e, EvalError::Overflow(OverflowError::DivideByZero(Number::Integer(1)))), "{v:?} -> {e:?}");
		}
	};
}

#[test]
fn evaluate_simplify()
{
	let ctx = setup_context();
	expect!
	{
		"(1 << 8 + 8) * 33 - 89 * 33 & (0b1000 | 2) * 0x1111 | 222", ctx => {Ok(val, r)} in
		{
			assert_eq!(r, Evaluation::Complete);
			const EXPECT: i64 = (1 << 8 + 8) * 33 - 89 * 33 & (0b1000 | 2) * 0x1111 | 222;
			assert!(matches!(val, Argument::Constant(Number::Integer(EXPECT))), "{val:?}");
		},
		"1 + \"fail\"", ctx => {Err(v, e)} in
		{
			assert!(matches!(e, EvalError::BadType{kind: ArgumentType::String, op: ArgumentType::Add}), "{v:?} -> {e:?}");
		},
		"[\"insane\"]", ctx => {Err(v, e)} in
		{
			assert!(matches!(e, EvalError::BadType{kind: ArgumentType::String, op: ArgumentType::Address}), "{v:?} -> {e:?}");
		},
	};
}
