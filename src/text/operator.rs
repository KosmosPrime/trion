use crate::text::token::TokenValue;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum BinOpGroup
{
	// ordered so that higher precedence compares as higher by `derive(Ord)`
	BitOr, BitXor, BitAnd, Shift, AddSub, DivMul,
}

impl BinOpGroup
{
	pub fn higher(self) -> Option<Self>
	{
		match self
		{
			Self::BitOr => Some(Self::BitXor),
			Self::BitXor => Some(Self::BitAnd),
			Self::BitAnd => Some(Self::Shift),
			Self::Shift => Some(Self::AddSub),
			Self::AddSub => Some(Self::DivMul),
			Self::DivMul => None,
		}
	}
}

impl From<BinOp> for BinOpGroup
{
	fn from(value: BinOp) -> Self
	{
		match value
		{
			BinOp::Multiply | BinOp::Divide | BinOp::Modulo => BinOpGroup::DivMul,
			BinOp::Add | BinOp::Subtract => BinOpGroup::AddSub,
			BinOp::LeftShift | BinOp::RightShift => BinOpGroup::Shift,
			BinOp::BitAnd => BinOpGroup::BitAnd,
			BinOp::BitXor => BinOpGroup::BitXor,
			BinOp::BitOr => BinOpGroup::BitOr,
		}
	}
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BinOp
{
	Add, Subtract, Multiply, Divide, Modulo,
	BitAnd, BitOr, BitXor, LeftShift, RightShift,
}

impl BinOp
{
	pub fn decode<'a>(value: &TokenValue<'a>) -> Option<Self>
	{
		Some(match value
		{
			TokenValue::Plus => Self::Add,
			TokenValue::Minus => Self::Subtract,
			TokenValue::Multiply => Self::Multiply,
			TokenValue::Divide => Self::Divide,
			TokenValue::Modulo => Self::Modulo,
			TokenValue::BitAnd => Self::BitAnd,
			TokenValue::BitOr => Self::BitOr,
			TokenValue::BitXor => Self::BitXor,
			TokenValue::LeftShift => Self::LeftShift,
			TokenValue::RightShift => Self::RightShift,
			_ => return None,
		})
	}
}
