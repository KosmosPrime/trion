use super::*;
	
macro_rules!expect
{
	($t:ident, $want:pat, {$($then:tt)*}) =>
	{
		match $t.next()
		{
			None => panic!("end of token stream"),
			Some(Ok($want)) => {$($then)*},
			Some(Ok(v)) => panic!("unexpected output {v:?}"),
			Some(Err(e)) => panic!("unexpected error {e}"),
		}
	};
}

#[test]
fn whitespace()
{
	let mut t = Tokenizer::new("  \t\t\n\t \n  \r\n  \t ".as_bytes());
	assert!(t.next().is_none());
	assert_eq!((t.get_line(), t.get_column()), (4, 5));
}

#[test]
fn unicode()
{
	let mut t = Tokenizer::new(b"\n\tevil: \x83");
	expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "evil");});
	expect!(t, Positioned{value: TokenValue::LabelMark, ..}, {});
	assert_eq!(t.next().unwrap().unwrap_err().value, TokenErrorKind::BadUnicode);
	assert_eq!((t.get_line(), t.get_column()), (2, 8));
}

#[test]
fn comments()
{
	let mut t = Tokenizer::new("// hello world\n\n/*\ncomment\n// comment */".as_bytes());
	assert!(t.next().is_none());
	assert_eq!((t.get_line(), t.get_column()), (5, 14));
	let mut t = Tokenizer::new("\t// hello world".as_bytes());
	assert!(t.next().is_none());
	assert_eq!((t.get_line(), t.get_column()), (1, 16));
	let mut t = Tokenizer::new("// hello world\n".as_bytes());
	assert!(t.next().is_none());
	assert_eq!((t.get_line(), t.get_column()), (2, 1));
}

#[test]
fn numbers()
{
	let mut t = Tokenizer::new("0 0000 1 9 10 11 19 100 0o100 0x100 0b100 0".as_bytes());
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 0);});
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 0);});
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 1);});
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 9);});
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 10);});
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 11);});
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 19);});
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 100);});
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 0o100);});
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 0x100);});
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 0b100);});
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 0);});
	assert!(t.next().is_none());
	
	assert_eq!(Tokenizer::new("0x".as_bytes()).next().unwrap().unwrap_err().value, TokenErrorKind::BadNumber);
	assert_eq!(Tokenizer::new("0x".as_bytes()).next().unwrap().unwrap_err().value, TokenErrorKind::BadNumber);
	assert_eq!(Tokenizer::new("0xz".as_bytes()).next().unwrap().unwrap_err().value, TokenErrorKind::BadNumber);
	assert_eq!(Tokenizer::new("0bz".as_bytes()).next().unwrap().unwrap_err().value, TokenErrorKind::BadNumber);
}

#[test]
fn chars()
{
	let mut t = Tokenizer::new("' '   'a' '\t' '\\t' '\\n' '\\r'".as_bytes());
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, ' ' as i64);});
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 'a' as i64);});
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, '\t' as i64);});
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, '\t' as i64);});
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, '\n' as i64);});
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, '\r' as i64);});
	assert!(t.next().is_none());
	
	assert_eq!(Tokenizer::new("'".as_bytes()).next().unwrap().unwrap_err().value, TokenErrorKind::BadCharacter);
	assert_eq!(Tokenizer::new("'a".as_bytes()).next().unwrap().unwrap_err().value, TokenErrorKind::BadCharacter);
	assert_eq!(Tokenizer::new("'aa'".as_bytes()).next().unwrap().unwrap_err().value, TokenErrorKind::BadCharacter);
	assert_eq!(Tokenizer::new("''".as_bytes()).next().unwrap().unwrap_err().value, TokenErrorKind::BadCharacter);
}

#[test]
fn identifiers()
{
	let mut t = Tokenizer::new("hello _world /*nope*/ te$Tw@rd".as_bytes());
	expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "hello");});
	expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "_world");});
	expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "te$Tw@rd");});
	assert!(t.next().is_none());
	
	let mut t = Tokenizer::new("keyäöüword".as_bytes());
	expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "key");});
	assert_eq!(t.next().unwrap().unwrap_err().value, TokenErrorKind::Unexpected('ä'));
	assert!(t.next().is_none());
}

#[test]
fn symbols()
{
	let mut t = Tokenizer::new("/,;:.+-*!&|^<<>>()[]{}".as_bytes());
	expect!(t, Positioned{value: TokenValue::Divide, ..}, {});
	expect!(t, Positioned{value: TokenValue::Separator, ..}, {});
	expect!(t, Positioned{value: TokenValue::Terminator, ..}, {});
	expect!(t, Positioned{value: TokenValue::LabelMark, ..}, {});
	expect!(t, Positioned{value: TokenValue::DirectiveMark, ..}, {});
	expect!(t, Positioned{value: TokenValue::Plus, ..}, {});
	expect!(t, Positioned{value: TokenValue::Minus, ..}, {});
	expect!(t, Positioned{value: TokenValue::Multiply, ..}, {});
	expect!(t, Positioned{value: TokenValue::Not, ..}, {});
	expect!(t, Positioned{value: TokenValue::BitAnd, ..}, {});
	expect!(t, Positioned{value: TokenValue::BitOr, ..}, {});
	expect!(t, Positioned{value: TokenValue::BitXor, ..}, {});
	expect!(t, Positioned{value: TokenValue::LeftShift, ..}, {});
	expect!(t, Positioned{value: TokenValue::RightShift, ..}, {});
	expect!(t, Positioned{value: TokenValue::BeginGroup, ..}, {});
	expect!(t, Positioned{value: TokenValue::EndGroup, ..}, {});
	expect!(t, Positioned{value: TokenValue::BeginAddr, ..}, {});
	expect!(t, Positioned{value: TokenValue::EndAddr, ..}, {});
	expect!(t, Positioned{value: TokenValue::BeginSeq, ..}, {});
	expect!(t, Positioned{value: TokenValue::EndSeq, ..}, {});
	assert!(t.next().is_none());
}

#[test]
fn strings()
{
	let mut t = Tokenizer::new(r#" ""  "hello world" "\0\r\n" "a\u{62}c" "#.as_bytes());
	expect!(t, Positioned{value: TokenValue::String(s), ..}, {assert_eq!(s.as_ref(), "")});
	expect!(t, Positioned{value: TokenValue::String(s), ..}, {assert_eq!(s.as_ref(), "hello world")});
	expect!(t, Positioned{value: TokenValue::String(s), ..}, {assert_eq!(s.as_ref(), "\0\r\n")});
	expect!(t, Positioned{value: TokenValue::String(s), ..}, {assert_eq!(s.as_ref(), "abc")});
	assert!(t.next().is_none());
}

#[test]
fn instructions()
{
	let mut t = Tokenizer::new("LDR R7, [R0 + 0x24]; // read the thing from somewhere".as_bytes());
	expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "LDR");});
	expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "R7");});
	expect!(t, Positioned{value: TokenValue::Separator, ..}, {});
	expect!(t, Positioned{value: TokenValue::BeginAddr, ..}, {});
	expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "R0");});
	expect!(t, Positioned{value: TokenValue::Plus, ..}, {});
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 0x24);});
	expect!(t, Positioned{value: TokenValue::EndAddr, ..}, {});
	expect!(t, Positioned{value: TokenValue::Terminator, ..}, {});
	assert!(t.next().is_none());
	
	let mut t = Tokenizer::new("SUBS R0, 'A'; // free comparison < 'A'".as_bytes());
	expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "SUBS");});
	expect!(t, Positioned{value: TokenValue::Identifier(s), ..}, {assert_eq!(s, "R0");});
	expect!(t, Positioned{value: TokenValue::Separator, ..}, {});
	expect!(t, Positioned{value: TokenValue::Number(Number::Integer(n)), ..}, {assert_eq!(n, 'A' as i64);});
	expect!(t, Positioned{value: TokenValue::Terminator, ..}, {});
	assert!(t.next().is_none());
}
