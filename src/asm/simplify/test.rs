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
		"2 - start - 3" => {Ok(val)} in
		{
			let Argument::Subtract(ref args) = val else {panic!("{val:?}")};
			assert_eq!(args.len(), 2);
			assert!(matches!(args[0], Argument::Constant(Number::Integer(-1))), "{:?}", args[0]);
			assert!(matches!(args[1], Argument::Identifier("start")), "{:?}", args[1]);
		},
		"start - 2 - 3" => {Ok(val)} in
		{
			let Argument::Subtract(ref args) = val else {panic!("{val:?}")};
			assert_eq!(args.len(), 2);
			assert!(matches!(args[0], Argument::Identifier("start")), "{:?}", args[0]);
			assert!(matches!(args[1], Argument::Constant(Number::Integer(5))), "{:?}", args[1]);
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
			let Argument::Add(ref args) = val else {panic!("{val:?}")};
			assert_eq!(args.len(), 2);
			assert!(matches!(args[0], Argument::Constant(Number::Integer(0x10000003))), "{:?}", args[0]);
			let Argument::Multiply(ref inner) = args[1] else {panic!("{:?}", args[1])};
			assert_eq!(inner.len(), 2);
			assert!(matches!(inner[0], Argument::Identifier("r0")), "{:?}", inner[0]);
			assert!(matches!(inner[1], Argument::Constant(Number::Integer(4))), "{:?}", inner[1]);
		},
		"base + r0 * 4 + 3", ctx => {Ok(val, r)} in
		{
			assert_eq!(r, Evaluation::Deferred);
			let Argument::Add(ref args) = val else {panic!("{val:?}")};
			assert_eq!(args.len(), 3);
			assert!(matches!(args[0], Argument::Identifier("base")), "{:?}", args[0]);
			let Argument::Multiply(ref inner) = args[1] else {panic!("{:?}", args[1])};
			assert_eq!(inner.len(), 2);
			assert!(matches!(inner[0], Argument::Identifier("r0")), "{:?}", inner[0]);
			assert!(matches!(inner[1], Argument::Constant(Number::Integer(4))), "{:?}", inner[1]);
			assert!(matches!(args[2], Argument::Constant(Number::Integer(3))), "{:?}", args[2]);
		},
		"paragraph % zero", ctx => {Ok(val, r)} in
		{
			assert_eq!(r, Evaluation::Deferred);
			let Argument::Modulo{value, divisor} = val else {panic!("{val:?}")};
			assert!(matches!(*value, Argument::Identifier("paragraph")), "{value:?}");
			assert!(matches!(*divisor, Argument::Constant(Number::Integer(0))), "{divisor:?}");
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
