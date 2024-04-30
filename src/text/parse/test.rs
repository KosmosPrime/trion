use super::*;
	
macro_rules!expect
{
	($want0:pat = $val0:expr $(; $want:pat = $val:expr)* $(; {$($then:tt)+})?) =>
	{
		match $val0
		{
			None => panic!("end of token stream"),
			Some(Ok($want0)) =>
			{
				$(
					let $want = $val
					else
					{
						panic!("expected {}, got {:?}", stringify!($want), $val);
					};
				)*
				$({$($then)*})?
			},
			Some(Ok(v)) => panic!("expected {}, got {v:?}", stringify!($want0)),
			Some(Err(e)) => panic!("unexpected error {e}"),
		}
	};
}

#[test]
fn internal_unary()
{
	let mut p = Parser::new(b"!ident - fake 1 (-!!x) - bogus (1)");
	expect!(Argument::Not(v) = Some(p.parse_unary()); Argument::Identifier(n) = *v; {assert_eq!(n.as_ref(), "ident");});
	expect!(Argument::Negate(v) = Some(p.parse_unary()); Argument::Identifier(n) = *v; {assert_eq!(n.as_ref(), "fake");});
	expect!(Argument::Constant(Number::Integer(v)) = Some(p.parse_unary()); {assert_eq!(v, 1);});
	expect!
	{
		Argument::Negate(v) = Some(p.parse_unary()); Argument::Not(v) = *v; Argument::Not(v) = *v;
		Argument::Identifier(n) = *v; {assert_eq!(n.as_ref(), "x");}
	};
	expect!
	{
		Argument::Negate(v) = Some(p.parse_unary()); Argument::Function{name, args} = *v;
		Argument::Constant(Number::Integer(v0)) = args[0];
		{assert_eq!(name.as_ref(), "bogus"); assert_eq!(v0, 1);}
	};
	assert!(p.next().is_none());
}

#[test]
fn internal_func()
{
	let mut p = Parser::new(b"get() map(10) plot(x, y, -1)");
	expect!(Argument::Function{name, args} = Some(p.parse_unary()); {assert_eq!(name.as_ref(), "get"); assert_eq!(args.len(), 0);});
	expect!
	{
		Argument::Function{name, args} = Some(p.parse_unary());
		Argument::Constant(Number::Integer(v0)) = args[0];
		{assert_eq!(name.as_ref(), "map"); assert_eq!(args.len(), 1); assert_eq!(v0, 10);}
	};
	expect!
	{
		Argument::Function{name, args} = Some(p.parse_unary());
		Argument::Identifier(ref n0) = args[0];
		Argument::Identifier(ref n1) = args[1]; 
		Argument::Negate(ref v2) = args[2]; Argument::Constant(Number::Integer(v2)) = **v2;
		{assert_eq!(name.as_ref(), "plot"); assert_eq!(args.len(), 3); assert_eq!(n0.as_ref(), "x"); assert_eq!(n1.as_ref(), "y"); assert_eq!(v2, 1);}
	};
	assert!(p.next().is_none());
}

#[test]
fn internal_address()
{
	let mut p = Parser::new(b"[0x1234] [base] [base + 0x10] [base + 4 * offset]");
	expect!(Argument::Address(addr) = Some(p.parse_unary()); Argument::Constant(Number::Integer(v)) = *addr; {assert_eq!(v, 0x1234);});
	expect!(Argument::Address(addr) = Some(p.parse_unary()); Argument::Identifier(n) = *addr; {assert_eq!(n.as_ref(), "base");});
	expect!
	{
		Argument::Address(addr) = Some(p.parse_unary()); Argument::Add{lhs: ref sum0, rhs: ref sum1} = *addr;
		Argument::Identifier(ref n0) = **sum0; Argument::Constant(Number::Integer(v1)) = **sum1;
		{assert_eq!(n0.as_ref(), "base"); assert_eq!(v1, 0x10);}
	};
	expect!
	{
		Argument::Address(addr) = Some(p.parse_unary()); Argument::Add{lhs: ref sum0, rhs: ref sum1} = *addr;
		Argument::Identifier(ref n0) = **sum0; Argument::Multiply{lhs: ref mul0, rhs: ref mul1} = **sum1;
		Argument::Constant(Number::Integer(v1)) = **mul0; Argument::Identifier(ref n2) = **mul1;
		{assert_eq!(n0.as_ref(), "base"); assert_eq!(v1, 4); assert_eq!(n2.as_ref(), "offset");}
	};
	assert!(p.next().is_none());
}

#[test]
fn internal_sequence()
{
	let mut p = Parser::new(b"{} {-5} {r0, r1, r2} {0, ident, [r0], {}, 2 + 3}");
	expect!(Argument::Sequence(items) = Some(p.parse_unary()); {assert_eq!(items.len(), 0);});
	expect!
	{
		Argument::Sequence(items) = Some(p.parse_unary());
		Argument::Negate(ref v0) = items[0]; Argument::Constant(Number::Integer(v0)) = **v0;
		{assert_eq!(items.len(), 1); assert_eq!(v0, 5);}
	};
	expect!
	{
		Argument::Sequence(items) = Some(p.parse_unary());
		Argument::Identifier(ref n0) = items[0]; Argument::Identifier(ref n1) = items[1]; Argument::Identifier(ref n2) = items[2];
		{assert_eq!(items.len(), 3); assert_eq!(n0.as_ref(), "r0"); assert_eq!(n1.as_ref(), "r1"); assert_eq!(n2.as_ref(), "r2");}
	};
	expect!
	{
		Argument::Sequence(items) = Some(p.parse_unary());
		Argument::Constant(Number::Integer(v0)) = items[0];
		Argument::Identifier(ref n1) = items[1];
		Argument::Address(ref a2) = items[2]; Argument::Identifier(n21) = a2.as_ref();
		Argument::Sequence(ref s3) = items[3];
		Argument::Add{lhs: ref sum40, rhs: ref sum41} = items[4];
			Argument::Constant(Number::Integer(v40)) = **sum40; Argument::Constant(Number::Integer(v41)) = **sum41;
		{
			assert_eq!(items.len(), 5); assert_eq!(v0, 0); assert_eq!(n1.as_ref(), "ident"); assert_eq!(n21.as_ref(), "r0");
			assert_eq!(s3.len(), 0); assert_eq!(v40, 2); assert_eq!(v41, 3);
		}
	};
	assert!(p.next().is_none());
}

#[test]
fn assembly()
{
	let mut p = Parser::new(b"main:\n\tmov r8d, 10;\n\tloop:\n\t\tdec r8d;\n\t\tjnz loop;\nhcf;\n.start \"main\";");
	expect!(Positioned{line: 1, col: 1, value: ElementValue::Label(l)} = p.next(); {assert_eq!(l.as_ref(), "main");});
	expect!
	{
		Positioned{line: 2, col: 2, value: ElementValue::Instruction{name, args}} = p.next();
		Argument::Identifier(ref n0) = args[0]; Argument::Constant(Number::Integer(v1)) = args[1];
		{assert_eq!(name.as_ref(), "mov"); assert_eq!(args.len(), 2); assert_eq!(n0.as_ref(), "r8d"); assert_eq!(v1, 10);}
	};
	expect!(Positioned{line: 3, col: 2, value: ElementValue::Label(l)} = p.next(); {assert_eq!(l.as_ref(), "loop");});
	expect!
	{
		Positioned{line: 4, col: 3, value: ElementValue::Instruction{name, args}} = p.next(); Argument::Identifier(ref n0) = args[0];
		{assert_eq!(name.as_ref(), "dec"); assert_eq!(args.len(), 1); assert_eq!(n0.as_ref(), "r8d");}
	};
	expect!
	{
		Positioned{line: 5, col: 3, value: ElementValue::Instruction{name, args}} = p.next(); Argument::Identifier(ref n0) = args[0];
		{assert_eq!(name.as_ref(), "jnz"); assert_eq!(args.len(), 1); assert_eq!(n0.as_ref(), "loop");}
	};
	expect!
	{
		Positioned{line: 6, col: 1, value: ElementValue::Instruction{name, args}} = p.next();
		{assert_eq!(name.as_ref(), "hcf"); assert_eq!(args.len(), 0);}
	};
	expect!
	{
		Positioned{line: 7, col: 1, value: ElementValue::Directive{name, args}} = p.next();
		Argument::String(ref v0) = args[0];
		{assert_eq!(name.as_ref(), "start"); assert_eq!(args.len(), 1); assert_eq!(v0.as_ref(), "main");}
	};
	assert!(p.next().is_none());
}
