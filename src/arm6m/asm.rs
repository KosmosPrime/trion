use core::fmt;
use std::error::Error;

use crate::arm6m::cond::Condition;
use crate::arm6m::reg::Register;
use crate::arm6m::regset::RegisterSet;
use crate::arm6m::sysreg::SystemReg;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ImmReg
{
	Immediate(i32),
	Register(Register),
}

impl fmt::Display for ImmReg
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Immediate(val) => val.fmt(f),
			Self::Register(reg) => reg.fmt(f),
		}
	}
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Instruction
{
	Adc{dst: Register, rhs: Register},
	Add{flags: bool, dst: Register, lhs: Register, rhs: ImmReg},
	Adr{dst: Register, off: u16},
	And{dst: Register, rhs: Register},
	Asr{dst: Register, value: Register, shift: ImmReg},
	B{cond: Condition, off: i32},
	Bic{dst: Register, rhs: Register},
	Bkpt{info: u8},
	Bl{off: i32},
	Blx{off: Register},
	Bx{off: Register},
	Cmn{lhs: Register, rhs: Register},
	Cmp{lhs: Register, rhs: ImmReg},
	Cps{enable: bool},
	Dmb,
	Dsb,
	Eor{dst: Register, rhs: Register},
	Isb,
	Ldm{addr: Register, registers: RegisterSet},
	Ldr{dst: Register, addr: Register, off: ImmReg},
	Ldrb{dst: Register, addr: Register, off: ImmReg},
	Ldrh{dst: Register, addr: Register, off: ImmReg},
	Ldrsb{dst: Register, addr: Register, off: Register},
	Ldrsh{dst: Register, addr: Register, off: Register},
	Lsl{dst: Register, value: Register, shift: ImmReg},
	Lsr{dst: Register, value: Register, shift: ImmReg},
	Mov{flags: bool, dst: Register, src: ImmReg},
	Mrs{dst: Register, src: SystemReg},
	Msr{dst: SystemReg, src: Register},
	Mul{dst: Register, rhs: Register},
	Mvn{dst: Register, value: Register},
	Nop,
	Orr{dst: Register, rhs: Register},
	Pop{registers: RegisterSet},
	Push{registers: RegisterSet},
	Rev{dst: Register, value: Register},
	Rev16{dst: Register, value: Register},
	Revsh{dst: Register, value: Register},
	Ror{dst: Register, rhs: Register},
	Rsb{dst: Register, lhs: Register},
	Sbc{dst: Register, rhs: Register},
	Sev,
	Stm{addr: Register, registers: RegisterSet},
	Str{src: Register, addr: Register, off: ImmReg},
	Strb{src: Register, addr: Register, off: ImmReg},
	Strh{src: Register, addr: Register, off: ImmReg},
	Sub{flags: bool, dst: Register, lhs: Register, rhs: ImmReg},
	Svc{info: u8},
	Sxtb{dst: Register, value: Register},
	Sxth{dst: Register, value: Register},
	Tst{lhs: Register, rhs: Register},
	Udf{info: u8},
	Udfw{info: u16},
	Uxtb{dst: Register, value: Register},
	Uxth{dst: Register, value: Register},
	Wfe,
	Wfi,
	Yield,
}

impl Instruction
{
	pub fn at(&self, addr: u32) -> InstrAt
	{
		InstrAt{instr: *self, addr}
	}
	
	pub fn get_name(&self) -> &'static str
	{
		match self
		{
			Instruction::Adc{..} => "ADCS",
			Instruction::Add{flags, ..} => if *flags {"ADDS"} else {"ADD"},
			Instruction::Adr{..} => "ADR",
			Instruction::And{..} => "ANDS",
			Instruction::Asr{..} => "ASRS",
			Instruction::B{cond, ..} =>
			{
				match cond
				{
					Condition::Equal => "BEQ",
					Condition::NonEqual => "BNE",
					//Condition::CarrySet => "BCS",
					Condition::CarrySet => "BHS",
					//Condition::CarryClear => "BCC",
					Condition::CarryClear => "BLO",
					Condition::Minus => "BMI",
					Condition::Plus => "BPL",
					Condition::Overflow => "BVS",
					Condition::NoOverflow => "BVC",
					Condition::Higher => "BHI",
					Condition::LowerEqual => "BLS",
					Condition::GreaterEqual => "BGE",
					Condition::Less => "BLT",
					Condition::Greater => "BGT",
					Condition::LessEqual => "BLE",
					Condition::Always => "B",
				}
			},
			Instruction::Bic{..} => "BIC",
			Instruction::Bkpt{..} => "BKPT",
			Instruction::Bl{..} => "BL",
			Instruction::Blx{..} => "BLX",
			Instruction::Bx{..} => "BX",
			Instruction::Cmn{..} => "CMN",
			Instruction::Cmp{..} => "CMP",
			Instruction::Cps{enable} => if *enable {"CPSIE"} else {"CPSID"},
			Instruction::Dmb => "DMB",
			Instruction::Dsb => "DSB",
			Instruction::Eor{..} => "EORS",
			Instruction::Isb => "ISB",
			Instruction::Ldm{..} => "LDM",
			Instruction::Ldr{..} => "LDR",
			Instruction::Ldrb{..} => "LDRB",
			Instruction::Ldrh{..} => "LDRH",
			Instruction::Ldrsb{..} => "LDRSB",
			Instruction::Ldrsh{..} => "LDRSH",
			Instruction::Lsl{..} => "LSLS",
			Instruction::Lsr{..} => "LSRS",
			Instruction::Mov{flags, ..} => if *flags {"MOVS"} else {"MOV"},
			Instruction::Mrs{..} => "MRS",
			Instruction::Msr{..} => "MSR",
			Instruction::Mul{..} => "MULS",
			Instruction::Mvn{..} => "MVNS",
			Instruction::Nop => "NOP",
			Instruction::Orr{..} => "ORRS",
			Instruction::Pop{..} => "POP",
			Instruction::Push{..} => "PUSH",
			Instruction::Rev{..} => "REV",
			Instruction::Rev16{..} => "REV16",
			Instruction::Revsh{..} => "REVSH",
			Instruction::Ror{..} => "RORS",
			Instruction::Rsb{..} => "RSBS",
			Instruction::Sbc{..} => "SBCS",
			Instruction::Sev => "SEV",
			Instruction::Stm{..} => "STM",
			Instruction::Str{..} => "STR",
			Instruction::Strb{..} => "STRB",
			Instruction::Strh{..} => "STRH",
			Instruction::Sub{flags, ..} => if *flags {"SUBS"} else {"SUB"},
			Instruction::Svc{..} => "SVC",
			Instruction::Sxtb{..} => "SXTB",
			Instruction::Sxth{..} => "SXTH",
			Instruction::Tst{..} => "TST",
			Instruction::Udf{..} => "UDF.N",
			Instruction::Udfw{..} => "UDF.W",
			Instruction::Uxtb{..} => "UXTB",
			Instruction::Uxth{..} => "UXTH",
			Instruction::Wfe => "WFE",
			Instruction::Wfi => "WFI",
			Instruction::Yield => "YIELD",
		}
	}
	
	pub fn get_branch(&self, addr: u32) -> Option<u32>
	{
		if (addr & 1) == 0
		{
			match self
			{
				Self::B{off, ..} if (off & 1) == 0 => Some(addr.wrapping_add(4).wrapping_add_signed(*off)),
				Self::Bl{off, ..} if (off & 1) == 0 => Some(addr.wrapping_add(4).wrapping_add_signed(*off)),
				_ => None,
			}
		}
		else {None}
	}
	
	pub fn get_returns(&self) -> bool
	{
		match self
		{
			Self::Add{dst: Register::PC, ..} => false,
			Self::B{cond, ..} => *cond != Condition::Always,
			Self::Bx{..} => false,
			Self::Bkpt{..} => false,
			Self::Mov{dst: Register::PC, ..} => false,
			Self::Pop{registers} if registers.contains(Register::PC) => false,
			Self::Sub{dst: Register::PC, ..} => false,
			Self::Udf{..} => false,
			Self::Udfw{..} => false,
			_ => true,
		}
	}
	
	pub fn decode(src: &[u8]) -> Result<(usize, Self), DecodeError>
	{
		if src.len() < 2
		{
			return Err(DecodeError::Underflow{need: 2, have: src.len()});
		}
		let instr0 = u16::from_le_bytes(<[u8; 2]>::try_from(&src[..2]).unwrap());
		match instr0 >> 11
		{
			0b00000 =>
			{
				if ((instr0 >> 6) & 0b11111) == 0
				{
					Ok((2, Self::Mov{flags: true, dst: Register::try_from(((instr0 >> 0) & 0b111) as u8).unwrap(),
						src: ImmReg::Register(Register::try_from(((instr0 >> 3) & 0b111) as u8).unwrap())}))
				}
				else
				{
					Ok((2, Self::Lsl{dst: Register::try_from(((instr0 >> 0) & 0b111) as u8).unwrap(),
						value: Register::try_from(((instr0 >> 3) & 0b111) as u8).unwrap(),
						shift: ImmReg::Immediate(((instr0 >> 6) & 0b11111) as i32)}))
				}
			},
			0b00001 =>
			{
				let mut shift = (instr0 >> 6) & 0b11111;
				if shift == 0 {shift = 32;}
				Ok((2, Self::Lsr{dst: Register::try_from(((instr0 >> 0) & 0b111) as u8).unwrap(),
					value: Register::try_from(((instr0 >> 3) & 0b111) as u8).unwrap(),
					shift: ImmReg::Immediate(shift as i32)}))
			},
			0b00010 =>
			{
				let mut shift = (instr0 >> 6) & 0b11111;
				if shift == 0 {shift = 32;}
				Ok((2, Self::Asr{dst: Register::try_from(((instr0 >> 0) & 0b111) as u8).unwrap(),
					value: Register::try_from(((instr0 >> 3) & 0b111) as u8).unwrap(),
					shift: ImmReg::Immediate(shift as i32)}))
			},
			0b00011 =>
			{
				let dst = Register::try_from(((instr0 >> 0) & 0b111) as u8).unwrap();
				let lhs = Register::try_from(((instr0 >> 3) & 0b111) as u8).unwrap();
				let rhs = if ((instr0 >> 10) & 1) == 0 {ImmReg::Register(Register::try_from(((instr0 >> 6) & 0b111) as u8).unwrap())}
				else {ImmReg::Immediate(((instr0 >> 6) & 0b111) as i32)};
				Ok((2, if ((instr0 >> 9) & 1) == 0 {Self::Add{flags: true, dst, lhs, rhs}} else {Self::Sub{flags: true, dst, lhs, rhs}}))
			},
			0b00100 =>
			{
				Ok((2, Self::Mov{flags: true, dst: Register::try_from(((instr0 >> 8) & 0b111) as u8).unwrap(),
					src: ImmReg::Immediate(((instr0 >> 0) & 0b11111111) as i32)}))
			},
			0b00101 =>
			{
				Ok((2, Self::Cmp{lhs: Register::try_from(((instr0 >> 8) & 0b111) as u8).unwrap(),
					rhs: ImmReg::Immediate(((instr0 >> 0) & 0b11111111) as i32)}))
			},
			0b00110 =>
			{
				let dst = Register::try_from(((instr0 >> 8) & 0b111) as u8).unwrap();
				Ok((2, Self::Add{flags: true, dst, lhs: dst, rhs: ImmReg::Immediate(((instr0 >> 0) & 0b11111111) as i32)}))
			},
			0b00111 =>
			{
				let dst = Register::try_from(((instr0 >> 8) & 0b111) as u8).unwrap();
				Ok((2, Self::Sub{flags: true, dst, lhs: dst, rhs: ImmReg::Immediate(((instr0 >> 0) & 0b11111111) as i32)}))
			},
			0b01000 =>
			{
				if ((instr0 >> 10) & 1) == 0
				{
					let r0 = Register::try_from(((instr0 >> 0) & 0b111) as u8).unwrap();
					let r1 = Register::try_from(((instr0 >> 3) & 0b111) as u8).unwrap();
					Ok((2, match (instr0 >> 6) & 0b1111
					{
						0b0000 => Self::And{dst: r0, rhs: r1},
						0b0001 => Self::Eor{dst: r0, rhs: r1},
						0b0010 => Self::Lsl{dst: r0, value: r0, shift: ImmReg::Register(r1)},
						0b0011 => Self::Lsr{dst: r0, value: r0, shift: ImmReg::Register(r1)},
						0b0100 => Self::Asr{dst: r0, value: r0, shift: ImmReg::Register(r1)},
						0b0101 => Self::Adc{dst: r0, rhs: r1},
						0b0110 => Self::Sbc{dst: r0, rhs: r1},
						0b0111 => Self::Ror{dst: r0, rhs: r1},
						0b1000 => Self::Tst{lhs: r0, rhs: r1},
						0b1001 => Self::Rsb{dst: r0, lhs: r1},
						0b1010 => Self::Cmp{lhs: r0, rhs: ImmReg::Register(r1)},
						0b1011 => Self::Cmn{lhs: r0, rhs: r1},
						0b1100 => Self::Orr{dst: r0, rhs: r1},
						0b1101 => Self::Mul{dst: r0, rhs: r1},
						0b1110 => Self::Bic{dst: r0, rhs: r1},
						0b1111 => Self::Mvn{dst: r0, value: r1},
						_ => unreachable!("thumb16 {instr0:04X} ({instr0:016b})"),
					}))
				}
				else
				{
					Ok((2, match (instr0 >> 8) & 0b11
					{
						0b00 =>
						{
							let dst = Register::try_from((((instr0 >> 0) & 0b111) | ((instr0 >> 4) & 0b1000)) as u8).unwrap();
							let rhs = Register::try_from(((instr0 >> 3) & 0b1111) as u8).unwrap();
							if dst < Register::R8 && rhs < Register::R8
							{
								return Err(DecodeError::Unpredictable{instr0, instr1: None});
							}
							if dst == Register::PC || rhs == Register::PC
							{
								return Err(DecodeError::Unpredictable{instr0, instr1: None});
							}
							Self::Add{flags: false, dst, lhs: dst, rhs: ImmReg::Register(rhs)}
						},
						0b01 =>
						{
							let lhs = Register::try_from((((instr0 >> 0) & 0b111) | ((instr0 >> 4) & 0b1000)) as u8).unwrap();
							let rhs = Register::try_from(((instr0 >> 3) & 0b1111) as u8).unwrap();
							if lhs < Register::R8 && rhs < Register::R8
							{
								return Err(DecodeError::Unpredictable{instr0, instr1: None});
							}
							if lhs == Register::PC || rhs == Register::PC
							{
								return Err(DecodeError::Unpredictable{instr0, instr1: None});
							}
							Self::Cmp{lhs, rhs: ImmReg::Register(rhs)}
						},
						0b10 =>
						{
							let dst = Register::try_from((((instr0 >> 0) & 0b111) | ((instr0 >> 4) & 0b1000)) as u8).unwrap();
							let src = Register::try_from(((instr0 >> 3) & 0b1111) as u8).unwrap();
							Self::Mov{flags: false, dst, src: ImmReg::Register(src)}
						},
						0b11 =>
						{
							if ((instr0 >> 0) & 0b111) != 0
							{
								return Err(DecodeError::Unpredictable{instr0, instr1: None});
							}
							let off = Register::try_from(((instr0 >> 3) & 0b1111) as u8).unwrap();
							if off == Register::PC
							{
								return Err(DecodeError::Unpredictable{instr0, instr1: None});
							}
							if ((instr0 >> 7) & 1) == 0 {Self::Bx{off}} else {Self::Blx{off}}
						},
						_ => unreachable!("thumb16 {instr0:04X} ({instr0:016b})"),
					}))
				}
			},
			0b01001 =>
			{
				Ok((2, Self::Ldr{dst: Register::try_from(((instr0 >> 8) & 0b111) as u8).unwrap(),
					addr: Register::PC, off: ImmReg::Immediate((((instr0 >> 0) & 0b11111111) as i32) << 2)}))
			},
			0b01010..=0b01011 =>
			{
				let reg = Register::try_from(((instr0 >> 0) & 0b111) as u8).unwrap();
				let addr = Register::try_from(((instr0 >> 3) & 0b111) as u8).unwrap();
				let off = Register::try_from(((instr0 >> 6) & 0b111) as u8).unwrap();
				Ok((2, match (instr0 >> 9) & 0b111
				{
					0b000 => Self::Str{src: reg, addr, off: ImmReg::Register(off)},
					0b001 => Self::Strh{src: reg, addr, off: ImmReg::Register(off)},
					0b010 => Self::Strb{src: reg, addr, off: ImmReg::Register(off)},
					0b011 => Self::Ldrsb{dst: reg, addr, off},
					0b100 => Self::Ldr{dst: reg, addr, off: ImmReg::Register(off)},
					0b101 => Self::Ldrh{dst: reg, addr, off: ImmReg::Register(off)},
					0b110 => Self::Ldrb{dst: reg, addr, off: ImmReg::Register(off)},
					0b111 => Self::Ldrsh{dst: reg, addr, off},
					_ => unreachable!("thumb16 {instr0:04X} ({instr0:016b})"),
				}))
			},
			0b01100..=0b01101 =>
			{
				let reg = Register::try_from(((instr0 >> 0) & 0b111) as u8).unwrap();
				let addr = Register::try_from(((instr0 >> 3) & 0b111) as u8).unwrap();
				let off = ImmReg::Immediate((((instr0 >> 6) & 0b11111) as i32) << 2);
				Ok((2, if ((instr0 >> 11) & 1) == 0 {Self::Str{src: reg, addr, off}} else {Self::Ldr{dst: reg, addr, off}}))
			},
			0b01110..=0b01111 =>
			{
				let reg = Register::try_from(((instr0 >> 0) & 0b111) as u8).unwrap();
				let addr = Register::try_from(((instr0 >> 3) & 0b111) as u8).unwrap();
				let off = ImmReg::Immediate(((instr0 >> 6) & 0b11111) as i32);
				Ok((2, if ((instr0 >> 11) & 1) == 0 {Self::Strb{src: reg, addr, off}} else {Self::Ldrb{dst: reg, addr, off}}))
			},
			0b10000..=0b10001 =>
			{
				let reg = Register::try_from(((instr0 >> 0) & 0b111) as u8).unwrap();
				let addr = Register::try_from(((instr0 >> 3) & 0b111) as u8).unwrap();
				let off = ImmReg::Immediate((((instr0 >> 6) & 0b11111) as i32) << 1);
				Ok((2, if ((instr0 >> 11) & 1) == 0 {Self::Strh{src: reg, addr, off}} else {Self::Ldrh{dst: reg, addr, off}}))
			},
			0b10010..=0b10011 =>
			{
				let reg = Register::try_from(((instr0 >> 8) & 0b111) as u8).unwrap();
				let off = ImmReg::Immediate((((instr0 >> 0) & 0b11111111) as i32) << 2);
				Ok((2, if ((instr0 >> 11) & 1) == 0 {Self::Str{src: reg, addr: Register::SP, off}} else {Self::Ldr{dst: reg, addr: Register::SP, off}}))
			},
			0b10100 =>
			{
				let dst = Register::try_from(((instr0 >> 8) & 0b111) as u8).unwrap();
				let off = ((instr0 >> 0) & 0b11111111) << 2;
				Ok((2, Self::Adr{dst, off}))
			},
			0b10101 =>
			{
				let dst = Register::try_from(((instr0 >> 8) & 0b111) as u8).unwrap();
				let rhs = ImmReg::Immediate((((instr0 >> 0) & 0b11111111) as i32) << 2);
				Ok((2, Self::Add{flags: false, dst, lhs: Register::SP, rhs}))
			},
			0b10110 =>
			{
				match (instr0 >> 8) & 0b111
				{
					0b000 =>
					{
						let rhs = ImmReg::Immediate((((instr0 >> 0) & 0b1111111) as i32) << 2);
						if ((instr0 >> 7) & 1) == 0 {Ok((2, Self::Add{flags: false, dst: Register::SP, lhs: Register::SP, rhs}))}
						else {Ok((2, Self::Sub{flags: false, dst: Register::SP, lhs: Register::SP, rhs}))}
					},
					0b001 => Err(DecodeError::Undefined{instr0, instr1: None}),
					0b010 =>
					{
						let dst = Register::try_from(((instr0 >> 0) & 0b111) as u8).unwrap();
						let value = Register::try_from(((instr0 >> 3) & 0b111) as u8).unwrap();
						Ok((2, if ((instr0 >> 7) & 1) == 0
						{
							if ((instr0 >> 6) & 1) == 0 {Self::Sxth{dst, value}} else {Self::Sxtb{dst, value}}
						}
						else
						{
							if ((instr0 >> 6) & 1) == 0 {Self::Uxth{dst, value}} else {Self::Uxtb{dst, value}}
						}))
					},
					0b011 => Err(DecodeError::Undefined{instr0, instr1: None}),
					0b100..=0b101 =>
					{
						let mut registers = RegisterSet::of((instr0 >> 0) & 0b11111111);
						if ((instr0 >> 8) & 1) != 0 {registers.add(Register::LR);}
						if registers.is_empty()
						{
							return Err(DecodeError::Unpredictable{instr0, instr1: None});
						}
						Ok((2, Self::Push{registers}))
					},
					0b110 =>
					{
						if ((instr0 >> 5) & 0b111) == 0b011
						{
							if ((instr0 >> 0) & 0b1111) != 0b0010
							{
								return Err(DecodeError::Unpredictable{instr0, instr1: None});
							}
							let enable = ((instr0 >> 4) & 1) != 0;
							Ok((2, Self::Cps{enable}))
						}
						else {Err(DecodeError::Undefined{instr0, instr1: None})}
					},
					0b111 => Err(DecodeError::Undefined{instr0, instr1: None}),
					_ => unreachable!("thumb16 {instr0:04X} ({instr0:016b})"),
				}
			},
			0b10111 =>
			{
				match (instr0 >> 8) & 0b111
				{
					0b000..=0b001 => Err(DecodeError::Undefined{instr0, instr1: None}),
					0b010 =>
					{
						let dst = Register::try_from(((instr0 >> 0) & 0b111) as u8).unwrap();
						let value = Register::try_from(((instr0 >> 3) & 0b111) as u8).unwrap();
						match (instr0 >> 6) & 0b11
						{
							0b00 => Ok((2, Self::Rev{dst, value})),
							0b01 => Ok((2, Self::Rev16{dst, value})),
							0b10 => Err(DecodeError::Undefined{instr0, instr1: None}),
							0b11 => Ok((2, Self::Revsh{dst, value})),
							_ => unreachable!("thumb16 {instr0:04X} ({instr0:016b})"),
						}
					},
					0b011 => Err(DecodeError::Undefined{instr0, instr1: None}),
					0b100..=0b101 =>
					{
						let mut registers = RegisterSet::of((instr0 >> 0) & 0b11111111);
						if ((instr0 >> 8) & 1) != 0 {registers.add(Register::PC);}
						if registers.is_empty()
						{
							return Err(DecodeError::Unpredictable{instr0, instr1: None});
						}
						Ok((2, Self::Pop{registers}))
					},
					0b110 => Ok((2, Self::Bkpt{info: ((instr0 >> 0) & 0b11111111) as u8})),
					0b111 =>
					{
						if ((instr0 >> 0) & 0b1111) == 0b0000
						{
							match (instr0 >> 4) & 0b1111
							{
								0b0000 => Ok((2, Self::Nop)),
								0b0001 => Ok((2, Self::Yield)),
								0b0010 => Ok((2, Self::Wfe)),
								0b0011 => Ok((2, Self::Wfi)),
								0b0100 => Ok((2, Self::Sev)),
								0b0101..=0b1111 => Err(DecodeError::Reserved{instr0, instr1: None}),
								_ => unreachable!("thumb16 {instr0:04X} ({instr0:016b})"),
							}
						}
						else {Err(DecodeError::Undefined{instr0, instr1: None})}
					},
					_ => unreachable!("thumb16 {instr0:04X} ({instr0:016b})"),
				}
			},
			0b11000..=0b11001 =>
			{
				let registers = RegisterSet::of((instr0 >> 0) & 0b11111111);
				let addr = Register::try_from(((instr0 >> 8) & 0b111) as u8).unwrap();
				Ok((2, if ((instr0 >> 11) & 1) == 0 {Self::Stm{addr, registers}} else {Self::Ldm{addr, registers}}))
			},
			0b11010..=0b11011 =>
			{
				match (instr0 >> 8) & 0b1111
				{
					c @ (0b0000..=0b1101) =>
					{
						let cond = Condition::try_from(c as u8).unwrap();
						let off = ((instr0 >> 0) & 0b11111111) as i32;
						Ok((2, Self::B{cond, off: (off << (i32::BITS - 8)) >> (i32::BITS - 9)}))
					},
					0b1110 => Ok((2, Self::Udf{info: ((instr0 >> 0) & 0b11111111) as u8})),
					0b1111 => Ok((2, Self::Svc{info: ((instr0 >> 0) & 0b11111111) as u8})),
					_ => unreachable!("thumb16 {instr0:04X} ({instr0:016b})"),
				}
			},
			0b11100 =>
			{
				let off = ((instr0 >> 0) & 0b11111111111) as i32;
				Ok((2, Self::B{cond: Condition::Always, off: (off << (i32::BITS - 11)) >> (i32::BITS - 12)}))
			},
			0b11101..=0b11111 =>
			{
				if src.len() < 4
				{
					return Err(DecodeError::Underflow{need: 4, have: src.len()});
				}
				let instr1 = u16::from_le_bytes(<[u8; 2]>::try_from(&src[2..4]).unwrap());
				if ((instr0 >> 11) & 0b11) == 0b10 && ((instr1 >> 15) & 1) == 1
				{
					if ((instr0 >> 5) & 0b111111) == 0b011100 && ((instr1 >> 12) & 0b101) == 0b000
					{
						if ((instr0 >> 4) & 1) != 0 || ((instr1 >> 8) & 0b101111) != 0b001000
						{
							return Err(DecodeError::Unpredictable{instr0, instr1: Some(instr1)});
						}
						let reg = Register::try_from(((instr0 >> 0) & 0b1111) as u8).unwrap();
						if reg == Register::SP || reg == Register::PC
						{
							return Err(DecodeError::Unpredictable{instr0, instr1: Some(instr1)});
						}
						match SystemReg::try_from(((instr1 >> 0) & 0b11111111) as u8)
						{
							Ok(sys) => Ok((4, Self::Msr{dst: sys, src: reg})),
							Err(..) => Err(DecodeError::Unpredictable{instr0, instr1: Some(instr1)}),
						}
					}
					else if ((instr0 >> 4) & 0b1111111) == 0b0111011 && ((instr1 >> 12) & 0b101) == 0b000
					{
						match (instr1 >> 4) & 0b1111
						{
							0b0000..=0b0011 => Err(DecodeError::Undefined{instr0, instr1: Some(instr1)}),
							0b0100 =>
							{
								if ((instr0 >> 0) & 0b1111) != 0b1111 || ((instr1 >> 8) & 0b101111) != 0b001111
								{
									return Err(DecodeError::Unpredictable{instr0, instr1: Some(instr1)});
								}
								let option = (instr1 >> 0) & 0b1111;
								if option != 0b1111
								{
									return Err(DecodeError::Reserved{instr0, instr1: Some(instr1)});
								}
								Ok((4, Self::Dsb))
							},
							0b0101 =>
							{
								if ((instr0 >> 0) & 0b1111) != 0b1111 || ((instr1 >> 8) & 0b101111) != 0b001111
								{
									return Err(DecodeError::Unpredictable{instr0, instr1: Some(instr1)});
								}
								let option = (instr1 >> 0) & 0b1111;
								if option != 0b1111
								{
									return Err(DecodeError::Reserved{instr0, instr1: Some(instr1)});
								}
								Ok((4, Self::Dmb))
							},
							0b0110 =>
							{
								if ((instr0 >> 0) & 0b1111) != 0b1111 || ((instr1 >> 8) & 0b101111) != 0b001111
								{
									return Err(DecodeError::Unpredictable{instr0, instr1: Some(instr1)});
								}
								let option = (instr1 >> 0) & 0b1111;
								if option != 0b1111
								{
									return Err(DecodeError::Reserved{instr0, instr1: Some(instr1)});
								}
								Ok((4, Self::Isb))
							},
							0b0111..=0b1111 => Err(DecodeError::Undefined{instr0, instr1: Some(instr1)}),
							_ => unreachable!("thumb32 {instr0:04X}{instr1:04X} ({instr0:016b} {instr1:016b})"),
						}
					}
					else if ((instr0 >> 5) & 0b111111) == 0b011111 && ((instr1 >> 12) & 0b101) == 0b000
					{
						if ((instr0 >> 0) & 0b11111) != 0b01111 || ((instr1 >> 13) & 1) != 0
						{
							return Err(DecodeError::Unpredictable{instr0, instr1: Some(instr1)});
						}
						let reg = Register::try_from(((instr1 >> 8) & 0b1111) as u8).unwrap();
						if reg == Register::SP || reg == Register::PC
						{
							return Err(DecodeError::Unpredictable{instr0, instr1: Some(instr1)});
						}
						match SystemReg::try_from(((instr1 >> 0) & 0b11111111) as u8)
						{
							Ok(sys) => Ok((4, Self::Mrs{dst: reg, src: sys})),
							Err(..) => Err(DecodeError::Unpredictable{instr0, instr1: Some(instr1)}),
						}
					}
					else if ((instr0 >> 4) & 0b1111111) == 0b1111111 && ((instr1 >> 12) & 0b111) == 0b010
					{
						Ok((4, Self::Udfw{info: ((instr0 << 12) & 0b11110000_00000000) | ((instr1 >> 0) & 0b1111_11111111)}))
					}
					else if ((instr1 >> 12) & 0b101) == 0b101
					{
						let mut off = (((instr1 >> 0) & 0b11111111111) as i32) << 1;
						off |= (((instr0 >> 0) & 0b1111111111) as i32) << 12;
						off |= ((!(instr1 >> 11) & 1) as i32) << 22;
						off |= ((!(instr1 >> 13) & 1) as i32) << 23;
						off ^= ((((instr0 >> 10) & 1) as i32) << 31) >> 9;
						Ok((4, Self::Bl{off}))
					}
					else {Err(DecodeError::Undefined{instr0, instr1: Some(instr1)})}
				}
				else {Err(DecodeError::Undefined{instr0, instr1: Some(instr1)})}
			},
			_ => unreachable!("thumb16 {instr0:04X} ({instr0:016b})"),
		}
	}
	
	pub fn encode(&self, out: &mut [u8]) -> Result<usize, EncodeError>
	{
		enum Encoding
		{
			Single(u16),
			Double(u16, u16),
		}
		
		#[inline(always)]
		fn s(instr0: u16) -> Encoding {Encoding::Single(instr0)}
		#[inline(always)]
		fn d(instr0: u16, instr1: u16) -> Encoding {Encoding::Double(instr0, instr1)}
		
		let result = match *self
		{
			Instruction::Adc{dst, rhs} =>
			{
				if dst >= Register::R8 && rhs >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0100000101_000_000 | ((u8::from(rhs) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Add{flags, dst, lhs, rhs: ImmReg::Immediate(rhs)} =>
			{
				if lhs == Register::SP
				{
					if dst == Register::SP
					{
						if flags || rhs < 0 || rhs > 0b1111111_00 || (rhs & 0b11) != 0
						{
							return Err(EncodeError::Unrepresentable);
						}
						s(0b101100000_0000000 | (((rhs >> 2) as u16) << 0))
					}
					else
					{
						if flags || dst >= Register::R8 || rhs < 0 || rhs > 0b11111111_00 || (rhs & 0b11) != 0
						{
							return Err(EncodeError::Unrepresentable);
						}
						s(0b10101_000_00000000 | ((u8::from(dst) as u16) << 8) | (((rhs >> 2) as u16) << 0))
					}
				}
				else if dst != lhs
				{
					if !flags || dst >= Register::R8 || lhs >= Register::R8 || rhs < 0 || rhs > 0b111
					{
						return Err(EncodeError::Unrepresentable);
					}
					s(0b0001110_000_000_000 | ((rhs as u16) << 6) | ((u8::from(lhs) as u16) << 3) | ((u8::from(dst) as u16) << 0))
				}
				else
				{
					if !flags || dst >= Register::R8 || rhs < 0 || rhs > 0b11111111
					{
						return Err(EncodeError::Unrepresentable);
					}
					s(0b00110_000_00000000 | ((u8::from(dst) as u16) << 8) | ((rhs as u16) << 0))
				}
			},
			Instruction::Add{flags, dst, lhs, rhs: ImmReg::Register(rhs)} =>
			{
				if !flags || dst >= Register::R8 || rhs >= Register::R8
				{
					if lhs != dst || (lhs == Register::PC && rhs == Register::PC)
					{
						return Err(EncodeError::Unrepresentable);
					}
					let dst = u8::from(dst) as u16;
					s(0b01000100_0_0000_000 | ((dst & 0b1000) << 4) | ((u8::from(rhs) as u16) << 3) | ((dst & 0b111) << 0))
				}
				else
				{
					if !flags || dst >= Register::R8 || lhs >= Register::R8 || rhs >= Register::R8
					{
						return Err(EncodeError::Unrepresentable);
					}
					s(0b0001100_000_000_000 | ((u8::from(rhs) as u16) << 6) | ((u8::from(lhs) as u16) << 3) | ((u8::from(dst) as u16) << 0))
				}
			},
			Instruction::Adr{dst, off} =>
			{
				if dst >= Register::R8 || off > 0b11111111_00 || (off & 0b11) != 0
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b10100_000_00000000 | ((u8::from(dst) as u16) << 8) | (((off >> 2) as u16) << 0))
			},
			Instruction::And{dst, rhs} =>
			{
				if dst >= Register::R8 || rhs >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0100000000_000_000 | ((u8::from(rhs) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Asr{dst, value, shift: ImmReg::Immediate(shift)} =>
			{
				if dst >= Register::R8 || value >= Register::R8 || shift <= 0 || shift > 0b100000
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b00010_00000_000_000 | (((shift & 0b11111) as u16) << 6) | ((u8::from(value) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Asr{dst, value, shift: ImmReg::Register(shift)} =>
			{
				if dst >= Register::R8 || value != dst || shift >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0100000100_000_000 | ((u8::from(shift) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::B{cond: Condition::Always, off} =>
			{
				if off < -0b10000000000_0 || off >= 0b10000000000_0 || (off & 0b1) != 0
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b11100_00000000000 | ((((off >> 1) as u16) & 0b11111111111) << 0))
			},
			Instruction::B{cond, off} =>
			{
				if cond == Condition::Always || off < -0b10000000_0 || off >= 0b10000000_0 || (off & 0b1) != 0
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b1101_0000_00000000 | ((u8::from(cond) as u16) << 8) | ((((off >> 1) as u16) & 0b11111111) << 0))
			},
			Instruction::Bic{dst, rhs} =>
			{
				if dst >= Register::R8 || rhs >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0100001110_000_000 | ((u8::from(rhs) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Bkpt{info} =>
			{
				s(0b10111110_00000000 | ((info as u16) << 0))
			},
			Instruction::Bl{off} =>
			{
				if off < -0b1_0_0_0000000000_00000000000_0 || off >= 0b1_0_0_0000000000_00000000000_0 || (off & 0b1) != 0
				{
					return Err(EncodeError::Unrepresentable);
				}
				let sii = (((off >> 22) as u16) & 0b11) ^ (((off >> 31) as u16) & 0b111) ^ 0b11;
				d(0b11110_0_0000000000 | ((sii & 0b100) << 8) | (((off >> 12) as u16) & 0b1111111111),
					0b11_0_1_0_00000000000 | ((sii & 0b010) << 12) | ((sii & 0b001) << 11) | (((off >> 1) as u16) & 0b11111111111))
			},
			Instruction::Blx{off} =>
			{
				if off == Register::PC
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b010001111_0000_000 | ((u8::from(off) as u16) << 3))
			},
			Instruction::Bx{off} =>
			{
				if off == Register::PC
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b010001110_0000_000 | ((u8::from(off) as u16) << 3))
			},
			Instruction::Cmn{lhs, rhs} =>
			{
				if lhs >= Register::R8 || rhs >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0100001011_000_000 | ((u8::from(rhs) as u16) << 3) | ((u8::from(lhs) as u16) << 0))
			},
			Instruction::Cmp{lhs, rhs: ImmReg::Immediate(rhs)} =>
			{
				if lhs >= Register::R8 || rhs < 0 || rhs > 0b11111111
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b00101_000_00000000 | ((u8::from(lhs) as u16) << 8) | ((rhs as u16) << 0))
			},
			Instruction::Cmp{lhs, rhs: ImmReg::Register(rhs)} =>
			{
				if lhs >= Register::R8 || rhs >= Register::R8
				{
					let lhs = u8::from(lhs) as u16;
					s(0b01000101_0_0000_000 | ((lhs & 0b1000) << 4) | ((u8::from(rhs) as u16) << 3) | ((lhs & 0b111) << 0))
				}
				else
				{
					s(0b0100001010_000_000 | ((u8::from(rhs) as u16) << 3) | ((u8::from(lhs) as u16) << 0))
				}
			},
			Instruction::Cps{enable} =>
			{
				s(0b10110110011_0_0010 | ((enable as u16) << 4))
			},
			Instruction::Dmb =>
			{
				d(0b1111001110111111, 0b100011110101_0000 | (0b1111 << 0))
			},
			Instruction::Dsb =>
			{
				d(0b1111001110111111, 0b100011110100_0000 | (0b1111 << 0))
			},
			Instruction::Eor{dst, rhs} =>
			{
				if dst >= Register::R8 || rhs >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0100000001_000_000 | ((u8::from(rhs) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Isb =>
			{
				d(0b1111001110111111, 0b100011110110_0000 | (0b1111 << 0))
			},
			Instruction::Ldm{addr, registers} =>
			{
				if addr >= Register::R8 || registers.get_bits() > 0b11111111
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b11001_000_00000000 | ((u8::from(addr) as u16) << 8) | (registers.get_bits() << 0))
			},
			Instruction::Ldr{dst, addr, off: ImmReg::Immediate(off)} =>
			{
				if addr == Register::PC
				{
					if dst >= Register::R8 || off < 0 || off > 0b11111111_00 || (off & 0b11) != 0
					{
						return Err(EncodeError::Unrepresentable);
					}
					s(0b01001_000_00000000 | ((u8::from(dst) as u16) << 8) | (((off >> 2) as u16) << 0))
				}
				else if addr == Register::SP
				{
					if dst >= Register::R8 || off < 0 || off > 0b11111111_00 || (off & 0b11) != 0
					{
						return Err(EncodeError::Unrepresentable);
					}
					s(0b10011_000_00000000 | ((u8::from(dst) as u16) << 8) | (((off >> 2) as u16) << 0))
				}
				else
				{
					if dst >= Register::R8 || addr >= Register::R8 || off < 0 || off > 0b11111_00 || (off & 0b11) != 0
					{
						return Err(EncodeError::Unrepresentable);
					}
					s(0b01101_00000_000_000 | (((off >> 2) as u16) << 6) | ((u8::from(addr) as u16) << 3) | ((u8::from(dst) as u16) << 0))
				}
			},
			Instruction::Ldr{dst, addr, off: ImmReg::Register(off)} =>
			{
				if dst >= Register::R8 || addr >= Register::R8 || off >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0101100_000_000_000 | ((u8::from(off) as u16) << 6) | ((u8::from(addr) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Ldrb{dst, addr, off: ImmReg::Immediate(off)} =>
			{
				if dst >= Register::R8 || addr >= Register::R8 || off < 0 || off > 0b11111
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b01111_00000_000_000 | ((off as u16) << 6) | ((u8::from(addr) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Ldrb{dst, addr, off: ImmReg::Register(off)} =>
			{
				if dst >= Register::R8 || addr >= Register::R8 || off >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0101110_000_000_000 | ((u8::from(off) as u16) << 6) | ((u8::from(addr) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Ldrh{dst, addr, off: ImmReg::Immediate(off)} =>
			{
				if dst >= Register::R8 || addr >= Register::R8 || off < 0 || off > 0b11111_0 || (off & 0b1) != 0
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b10001_00000_000_000 | (((off >> 1) as u16) << 6) | ((u8::from(addr) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Ldrh{dst, addr, off: ImmReg::Register(off)} =>
			{
				if dst >= Register::R8 || addr >= Register::R8 || off >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0101101_000_000_000 | ((u8::from(off) as u16) << 6) | ((u8::from(addr) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Ldrsb{dst, addr, off} =>
			{
				if dst >= Register::R8 || addr >= Register::R8 || off >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0101011_000_000_000 | ((u8::from(off) as u16) << 6) | ((u8::from(addr) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Ldrsh{dst, addr, off} =>
			{
				if dst >= Register::R8 || addr >= Register::R8 || off >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0101111_000_000_000 | ((u8::from(off) as u16) << 6) | ((u8::from(addr) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Lsl{dst, value, shift: ImmReg::Immediate(shift)} =>
			{
				if dst >= Register::R8 || value >= Register::R8 || shift <= 0 || shift > 0b11111
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b00000_00000_000_000 | ((shift as u16) << 6) | ((u8::from(value) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Lsl{dst, value, shift: ImmReg::Register(shift)} =>
			{
				if dst >= Register::R8 || value != dst || shift >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0100000010_000_000 | ((u8::from(shift) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Lsr{dst, value, shift: ImmReg::Immediate(shift)} =>
			{
				if dst >= Register::R8 || value >= Register::R8 || shift <= 0 || shift > 0b100000
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b00001_00000_000_000 | (((shift & 0b11111) as u16) << 6) | ((u8::from(value) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Lsr{dst, value, shift: ImmReg::Register(shift)} =>
			{
				if dst >= Register::R8 || value != dst || shift >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0100000011_000_000 | ((u8::from(shift) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Mov{flags, dst, src: ImmReg::Immediate(src)} =>
			{
				if !flags || dst >= Register::R8 || src < 0 || src > 0b11111111
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b00100_000_00000000 | ((u8::from(dst) as u16) << 8) | ((src as u16) << 0))
			},
			Instruction::Mov{flags, dst, src: ImmReg::Register(src)} =>
			{
				if !flags || dst >= Register::R8 || src >= Register::R8
				{
					if flags
					{
						return Err(EncodeError::Unrepresentable);
					}
					let dst = u8::from(dst) as u16;
					s(0b01000110_0_0000_000 | ((dst & 0b1000) << 4) | ((u8::from(src) as u16) << 3) | ((dst & 0b111) << 0))
				}
				else
				{
					s(0b0000000000_000_000 | ((u8::from(src) as u16) << 3) | ((u8::from(dst) as u16) << 0))
				}
			},
			Instruction::Mrs{dst, src} =>
			{
				if dst == Register::SP || dst == Register::PC
				{
					return Err(EncodeError::Unrepresentable);
				}
				d(0b1111001111101111, 0b1000_0000_00000000 | ((u8::from(dst) as u16) << 8) | ((u8::from(src) as u16) << 0))
			},
			Instruction::Msr{dst, src} =>
			{
				if src == Register::SP || src == Register::PC
				{
					return Err(EncodeError::Unrepresentable);
				}
				d(0b111100111000_0000 | ((u8::from(src) as u16) << 0), 0b10001000_00000000 | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Mul{dst, rhs} =>
			{
				if dst >= Register::R8 || rhs >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0100001101_000_000 | ((u8::from(rhs) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Mvn{dst, value} =>
			{
				if dst >= Register::R8 || value >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0100001111_000_000 | ((u8::from(value) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Nop => s(0b1011111100000000),
			Instruction::Orr{dst, rhs} =>
			{
				if dst >= Register::R8 || rhs >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0100001100_000_000 | ((u8::from(rhs) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Pop{registers} =>
			{
				if registers.is_empty() || (registers.get_bits() & 0b01111111_00000000) != 0
				{
					return Err(EncodeError::Unrepresentable);
				}
				let registers = registers.get_bits();
				s(0b1011110_0_00000000 | ((registers & 0b10000000_00000000) >> 7) | ((registers & 0b11111111) << 0))
			},
			Instruction::Push{registers} =>
			{
				if registers.is_empty() || (registers.get_bits() & 0b10111111_00000000) != 0
				{
					return Err(EncodeError::Unrepresentable);
				}
				let registers = registers.get_bits();
				s(0b1011010_0_00000000 | ((registers & 0b01000000_00000000) >> 6) | ((registers & 0b11111111) << 0))
			},
			Instruction::Rev{dst, value} =>
			{
				if dst >= Register::R8 || value >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b1011101000_000_000 | ((u8::from(value) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Rev16{dst, value} =>
			{
				if dst >= Register::R8 || value >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b1011101001_000_000 | ((u8::from(value) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Revsh{dst, value} =>
			{
				if dst >= Register::R8 || value >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b1011101011_000_000 | ((u8::from(value) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Ror{dst, rhs} =>
			{
				if dst >= Register::R8 || rhs >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0100000111_000_000 | ((u8::from(rhs) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Rsb{dst, lhs} =>
			{
				if dst >= Register::R8 || lhs >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0100001001_000_000 | ((u8::from(lhs) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Sbc{dst, rhs} =>
			{
				if dst >= Register::R8 || rhs >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0100000110_000_000 | ((u8::from(rhs) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Sev => s(0b1011111101000000),
			Instruction::Stm{addr, registers} =>
			{
				if addr >= Register::R8 || registers.get_bits() > 0b11111111
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b11000_000_00000000 | ((u8::from(addr) as u16) << 8) | (registers.get_bits() << 0))
			},
			Instruction::Str{src, addr, off: ImmReg::Immediate(off)} =>
			{
				if addr == Register::SP
				{
					if src >= Register::R8 || off < 0 || off > 0b11111111_00 || (off & 0b11) != 0
					{
						return Err(EncodeError::Unrepresentable);
					}
					s(0b10010_000_00000000 | ((u8::from(src) as u16) << 8) | (((off >> 2) as u16) << 0))
				}
				else
				{
					if src >= Register::R8 || addr >= Register::R8 || off < 0 || off > 0b11111_00 || (off & 0b11) != 0
					{
						return Err(EncodeError::Unrepresentable);
					}
					s(0b01100_00000_000_000 | (((off >> 2) as u16) << 6) | ((u8::from(addr) as u16) << 3) | ((u8::from(src) as u16) << 0))
				}
			},
			Instruction::Str{src, addr, off: ImmReg::Register(off)} =>
			{
				if src >= Register::R8 || addr >= Register::R8 || off >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0101000_000_000_000 | ((u8::from(off) as u16) << 6) | ((u8::from(addr) as u16) << 3) | ((u8::from(src) as u16) << 0))
			},
			
			Instruction::Strb{src, addr, off: ImmReg::Immediate(off)} =>
			{
				if src >= Register::R8 || addr >= Register::R8 || off < 0 || off > 0b11111
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b01110_00000_000_000 | ((off as u16) << 6) | ((u8::from(addr) as u16) << 3) | ((u8::from(src) as u16) << 0))
			},
			Instruction::Strb{src, addr, off: ImmReg::Register(off)} =>
			{
				if src >= Register::R8 || addr >= Register::R8 || off >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0101010_000_000_000 | ((u8::from(off) as u16) << 6) | ((u8::from(addr) as u16) << 3) | ((u8::from(src) as u16) << 0))
			},
			Instruction::Strh{src, addr, off: ImmReg::Immediate(off)} =>
			{
				if src >= Register::R8 || addr >= Register::R8 || off < 0 || off > 0b11111_0 || (off & 0b1) != 0
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b10000_00000_000_000 | (((off >> 1) as u16) << 6) | ((u8::from(addr) as u16) << 3) | ((u8::from(src) as u16) << 0))
			},
			Instruction::Strh{src, addr, off: ImmReg::Register(off)} =>
			{
				if src >= Register::R8 || addr >= Register::R8 || off >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0101001_000_000_000 | ((u8::from(off) as u16) << 6) | ((u8::from(addr) as u16) << 3) | ((u8::from(src) as u16) << 0))
			},
			Instruction::Sub{flags, dst, lhs, rhs: ImmReg::Immediate(rhs)} =>
			{
				if lhs == Register::SP
				{
					if flags || dst != Register::SP || rhs < 0 || rhs > 0b1111111_00 || (rhs & 0b11) != 0
					{
						return Err(EncodeError::Unrepresentable);
					}
					s(0b101100001_0000000 | ((rhs >> 2) as u16))
				}
				else if dst != lhs
				{
					if !flags || dst >= Register::R8 || lhs >= Register::R8 || rhs < 0 || rhs > 0b111
					{
						return Err(EncodeError::Unrepresentable);
					}
					s(0b0001111_000_000_000 | ((rhs as u16) << 6) | ((u8::from(lhs) as u16) << 3) | ((u8::from(dst) as u16) << 0))
				}
				else
				{
					if !flags || dst >= Register::R8 || rhs < 0 || rhs > 0b11111111
					{
						return Err(EncodeError::Unrepresentable);
					}
					s(0b00111_000_00000000 | ((u8::from(dst) as u16) << 8) | ((rhs as u16) << 0))
				}
			},
			Instruction::Sub{flags, dst, lhs, rhs: ImmReg::Register(rhs)} =>
			{
				if !flags || dst >= Register::R8 || lhs >= Register::R8 || rhs >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0001101_000_000_000 | ((u8::from(rhs) as u16) << 6) | ((u8::from(lhs) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Svc{info} =>
			{
				s(0b11011111_00000000 | ((info as u16) << 0))
			},
			Instruction::Sxtb{dst, value} =>
			{
				if dst >= Register::R8 || value >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b1011001001_000_000 | ((u8::from(value) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Sxth{dst, value} =>
			{
				if dst >= Register::R8 || value >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b1011001000_000_000 | ((u8::from(value) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Tst{lhs, rhs} =>
			{
				if lhs >= Register::R8 || rhs >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b0100001000_000_000 | ((u8::from(rhs) as u16) << 3) | ((u8::from(lhs) as u16) << 0))
			},
			Instruction::Udf{info} =>
			{
				s(0b11011110_00000000 | ((info as u16) << 0))
			},
			Instruction::Udfw{info} =>
			{
				d(0b111101111111_0000 | (((info & 0b1111000000000000) >> 12) << 0), 0b1010_000000000000 | ((info & 0b111111111111) << 0))
			},
			Instruction::Uxtb{dst, value} =>
			{
				if dst >= Register::R8 || value >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b1011001011_000_000 | ((u8::from(value) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Uxth{dst, value} =>
			{
				if dst >= Register::R8 || value >= Register::R8
				{
					return Err(EncodeError::Unrepresentable);
				}
				s(0b1011001010_000_000 | ((u8::from(value) as u16) << 3) | ((u8::from(dst) as u16) << 0))
			},
			Instruction::Wfe => s(0b1011111100100000),
			Instruction::Wfi => s(0b1011111100110000),
			Instruction::Yield => s(0b1011111100010000),
		};
		
		const LEN: usize = core::mem::size_of::<u16>();
		match result
		{
			Encoding::Single(instr0) =>
			{
				if out.len() < LEN
				{
					return Err(EncodeError::Overflow{need: LEN, have: out.len()});
				}
				out[..LEN].copy_from_slice(u16::to_le_bytes(instr0).as_ref());
				Ok(LEN)
			},
			Encoding::Double(instr0, instr1) =>
			{
				if out.len() < 2 * LEN
				{
					return Err(EncodeError::Overflow{need: 2 * LEN, have: out.len()});
				}
				out[..LEN].copy_from_slice(u16::to_le_bytes(instr0).as_ref());
				out[LEN..2 * LEN].copy_from_slice(u16::to_le_bytes(instr1).as_ref());
				Ok(2 * LEN)
			},
		}
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DecodeError
{
	Underflow{need: usize, have: usize},
	Undefined{instr0: u16, instr1: Option<u16>},
	Unpredictable{instr0: u16, instr1: Option<u16>},
	Reserved{instr0: u16, instr1: Option<u16>},
}

impl fmt::Display for DecodeError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		fn helper(f: &mut fmt::Formatter<'_>, text: &str, instr0: u16, instr1: Option<u16>) -> fmt::Result
		{
			match instr1
			{
				None => write!(f, "{text} 0x{instr0:04X} ({instr0:016b})"),
				Some(instr1) => write!(f, "{text} 0x{instr0:04X}{instr1:04X} ({instr0:016b} {instr1:016b})"),
			}
		}
		
		match self
		{
			Self::Underflow{need, have} => write!(f, "input buffer underflow (need {need}, got {have})"),
			Self::Undefined{instr0, instr1} => helper(f, "undefined instruction", *instr0, *instr1),
			Self::Unpredictable{instr0, instr1} => helper(f, "unpredictable instruction", *instr0, *instr1),
			Self::Reserved{instr0, instr1} => helper(f, "reserved instruction", *instr0, *instr1),
		}
	}
}

impl Error for DecodeError {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum EncodeError
{
	Overflow{need: usize, have: usize},
	Unrepresentable,
}

impl fmt::Display for EncodeError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Overflow{need, have} => write!(f, "output buffer overflow (need {need}, got {have})"),
			Self::Unrepresentable => f.write_str("the instruction cannot be represented"),
		}
	}
}

impl Error for EncodeError {}

pub struct InstrAt
{
	pub instr: Instruction,
	pub addr: u32,
}

impl fmt::Display for InstrAt
{
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
	{
		match self.instr
		{
			Instruction::Adc{dst, rhs} => write!(f, "ADCS {dst}, {rhs};"),
			Instruction::Add{flags, dst, lhs, rhs} =>
			{
				if rhs == ImmReg::Register(Register::SP) {write!(f, "ADD{} {dst}, {rhs}, {lhs};", if flags {"S"} else {""})}
				else {write!(f, "ADD{} {dst}, {lhs}, {rhs};", if flags {"S"} else {""})}
			},
			Instruction::Adr{dst, off} => write!(f, "ADR {dst}, l_{:08X};", (self.addr & !0b11).wrapping_add(4).wrapping_add(off as u32)),
			Instruction::And{dst, rhs} => write!(f, "ANDS {dst}, {rhs};"),
			Instruction::Asr{dst, value, shift} => write!(f, "ASRS {dst}, {value}, {shift};"),
			Instruction::B{cond: Condition::Always, off} => write!(f, "B l_{:08X};", self.addr.wrapping_add(4).wrapping_add(off as u32)),
			Instruction::B{cond, off} => write!(f, "B{cond} l_{:08X};", self.addr.wrapping_add(4).wrapping_add(off as u32)),
			Instruction::Bic{dst, rhs} => write!(f, "BICS {dst}, {rhs};"),
			Instruction::Bkpt{info} => write!(f, "BKPT {info};"),
			Instruction::Bl{off} => write!(f, "BL l_{:08X};", self.addr.wrapping_add(4).wrapping_add(off as u32)),
			Instruction::Blx{off} => write!(f, "BLX {off};"),
			Instruction::Bx{off} => write!(f, "BX {off};"),
			Instruction::Cmn{lhs, rhs} => write!(f, "CMN {lhs}, {rhs};"),
			Instruction::Cmp{lhs, rhs} => write!(f, "CMP {lhs}, {rhs};"),
			Instruction::Cps{enable} => f.write_str(if enable {"CPSIE i;"} else {"CPSID i;"}),
			Instruction::Dmb => f.write_str("DMB SY;"),
			Instruction::Dsb => f.write_str("DSB SY;"),
			Instruction::Eor{dst, rhs} => write!(f, "EORS {dst}, {rhs};"),
			Instruction::Isb => f.write_str("ISB SY;"),
			Instruction::Ldm{addr, registers} => write!(f, "LDM {addr}, {registers};"),
			Instruction::Ldr{dst, addr: Register::PC, off: ImmReg::Immediate(off)} =>
			{
				write!(f, "LDR {dst}, l_{:08X};", (self.addr & !0b11).wrapping_add(4).wrapping_add_signed(off))
			},
			Instruction::Ldr{dst, addr, off} => write!(f, "LDR {dst}, [{addr} + {off}];"),
			Instruction::Ldrb{dst, addr, off} => write!(f, "LDRB {dst}, [{addr} + {off}];"),
			Instruction::Ldrh{dst, addr, off} => write!(f, "LDRH {dst}, [{addr} + {off}];"),
			Instruction::Ldrsb{dst, addr, off} => write!(f, "LDRSB {dst}, [{addr} + {off}];"),
			Instruction::Ldrsh{dst, addr, off} => write!(f, "LDRSH {dst}, [{addr} + {off}];"),
			Instruction::Lsl{dst, value, shift} => write!(f, "LSLS {dst}, {value}, {shift};"),
			Instruction::Lsr{dst, value, shift} => write!(f, "LSRS {dst}, {value}, {shift};"),
			Instruction::Mov{flags, dst, src} => write!(f, "MOV{} {dst}, {src}", if flags {"S"} else {""}),
			Instruction::Mrs{dst, src} => write!(f, "MRS {dst}, {src};"),
			Instruction::Msr{dst, src} => write!(f, "MSR {dst}, {src};"),
			Instruction::Mul{dst, rhs} => write!(f, "MULS {dst}, {rhs};"),
			Instruction::Mvn{dst, value} => write!(f, "MVNS {dst}, {value};"),
			Instruction::Nop => f.write_str("NOP;"),
			Instruction::Orr{dst, rhs} => write!(f, "ORRS {dst}, {rhs};"),
			Instruction::Pop{registers} => write!(f, "POP {registers};"),
			Instruction::Push{registers} => write!(f, "PUSH {registers};"),
			Instruction::Rev{dst, value} => write!(f, "REV {dst}, {value};"),
			Instruction::Rev16{dst, value} => write!(f, "REV16 {dst}, {value};"),
			Instruction::Revsh{dst, value} => write!(f, "REVSH {dst}, {value};"),
			Instruction::Ror{dst, rhs} => write!(f, "RORS {dst}, {rhs};"),
			Instruction::Rsb{dst, lhs} => write!(f, "RSBS {dst}, {lhs}, 0;"),
			Instruction::Sbc{dst, rhs} => write!(f, "SBCS {dst}, {rhs};"),
			Instruction::Sev => f.write_str("SEV"),
			Instruction::Stm{addr, registers} => write!(f, "STM {addr}, {registers};"),
			Instruction::Str{src, addr, off} => write!(f, "STR {src}, [{addr} + {off}];"),
			Instruction::Strb{src, addr, off} => write!(f, "STRB {src}, [{addr} + {off}];"),
			Instruction::Strh{src, addr, off} => write!(f, "STRH {src}, [{addr} + {off}];"),
			Instruction::Sub{flags, dst, lhs, rhs} => write!(f, "SUB{} {dst}, {lhs}, {rhs};", if flags {"S"} else {""}),
			Instruction::Svc{info} => write!(f, "SVC {info};"),
			Instruction::Sxtb{dst, value} => write!(f, "SXTB {dst}, {value};"),
			Instruction::Sxth{dst, value} => write!(f, "SXTH {dst}, {value};"),
			Instruction::Tst{lhs, rhs} => write!(f, "TST {lhs}, {rhs};"),
			Instruction::Udf{info} => write!(f, "UDF.N {info};"),
			Instruction::Udfw{info} => write!(f, "UDF.W {info};"),
			Instruction::Uxtb{dst, value} => write!(f, "UXTB {dst}, {value};"),
			Instruction::Uxth{dst, value} => write!(f, "UXTH {dst}, {value};"),
			Instruction::Wfe => f.write_str("WFE;"),
			Instruction::Wfi => f.write_str("WFI;"),
			Instruction::Yield => f.write_str("YIELD;"),
		}
	}
}
