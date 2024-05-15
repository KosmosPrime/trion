use crate::arm6m::asm::{ImmReg, Instruction};
use crate::arm6m::cond::Condition;
use crate::arm6m::reg::Register;
use crate::arm6m::regset::RegisterSet;
use crate::arm6m::sysreg::SystemReg;

#[test]
fn test_default_instruction()
{
	let instrs = [
		Instruction::Adc{dst: Register::R0, rhs: Register::R0},
		Instruction::Add{flags: false, dst: Register::SP, lhs: Register::SP, rhs: ImmReg::Immediate(0)},
		Instruction::Add{flags: true, dst: Register::R0, lhs: Register::R0, rhs: ImmReg::Immediate(0)},
		Instruction::Adr{dst: Register::R0, off: 0},
		Instruction::And{dst: Register::R0, rhs: Register::R0},
		Instruction::Asr{dst: Register::R0, value: Register::R0, shift: ImmReg::Immediate(1)},
		Instruction::B{cond: Condition::Always, off: 0},
		Instruction::B{cond: Condition::CarryClear, off: 0},
		Instruction::B{cond: Condition::CarrySet, off: 0},
		Instruction::B{cond: Condition::Equal, off: 0},
		Instruction::B{cond: Condition::GreaterEqual, off: 0},
		Instruction::B{cond: Condition::Greater, off: 0},
		Instruction::B{cond: Condition::Higher, off: 0},
		Instruction::B{cond: Condition::CarrySet, off: 0},
		Instruction::Bic{dst: Register::R0, rhs: Register::R0},
		Instruction::Bkpt{info: 0},
		Instruction::Bl{off: 0},
		Instruction::B{cond: Condition::LessEqual, off: 0},
		Instruction::B{cond: Condition::CarryClear, off: 0},
		Instruction::B{cond: Condition::LowerEqual, off: 0},
		Instruction::B{cond: Condition::Less, off: 0},
		Instruction::Blx{off: Register::R0},
		Instruction::B{cond: Condition::Minus, off: 0},
		Instruction::B{cond: Condition::NonEqual, off: 0},
		Instruction::B{cond: Condition::Plus, off: 0},
		Instruction::B{cond: Condition::NoOverflow, off: 0},
		Instruction::B{cond: Condition::Overflow, off: 0},
		Instruction::Bx{off: Register::R0},
		Instruction::Cmn{lhs: Register::R0, rhs: Register::R0},
		Instruction::Cmp{lhs: Register::R0, rhs: ImmReg::Immediate(0)},
		Instruction::Cps{enable: false},
		Instruction::Cps{enable: true},
		Instruction::Dmb,
		Instruction::Dsb,
		Instruction::Eor{dst: Register::R0, rhs: Register::R0},
		Instruction::Isb,
		Instruction::Ldm{addr: Register::R0, registers: RegisterSet::new()},
		Instruction::Ldr{dst: Register::R0, addr: Register::R0, off: ImmReg::Immediate(0)},
		Instruction::Ldrb{dst: Register::R0, addr: Register::R0, off: ImmReg::Immediate(0)},
		Instruction::Ldrh{dst: Register::R0, addr: Register::R0, off: ImmReg::Immediate(0)},
		Instruction::Ldrsb{dst: Register::R0, addr: Register::R0, off: Register::R0},
		Instruction::Ldrsh{dst: Register::R0, addr: Register::R0, off: Register::R0},
		Instruction::Lsl{dst: Register::R0, value: Register::R0, shift: ImmReg::Immediate(1)},
		Instruction::Lsr{dst: Register::R0, value: Register::R0, shift: ImmReg::Immediate(1)},
		Instruction::Mov{flags: false, dst: Register::R0, src: ImmReg::Register(Register::R0)},
		Instruction::Mov{flags: true, dst: Register::R0, src: ImmReg::Register(Register::R0)},
		Instruction::Mrs{dst: Register::R0, src: SystemReg::XPSR},
		Instruction::Msr{dst: SystemReg::XPSR, src: Register::R0},
		Instruction::Mul{dst: Register::R0, rhs: Register::R0},
		Instruction::Mvn{dst: Register::R0, value: Register::R0},
		Instruction::Nop,
		Instruction::Orr{dst: Register::R0, rhs: Register::R0},
		Instruction::Pop{registers: RegisterSet::of(1 << u8::from(Register::R0))},
		Instruction::Push{registers: RegisterSet::of(1 << u8::from(Register::R0))},
		Instruction::Rev{dst: Register::R0, value: Register::R0},
		Instruction::Rev16{dst: Register::R0, value: Register::R0},
		Instruction::Revsh{dst: Register::R0, value: Register::R0},
		Instruction::Ror{dst: Register::R0, rhs: Register::R0},
		Instruction::Rsb{dst: Register::R0, lhs: Register::R0},
		Instruction::Sbc{dst: Register::R0, rhs: Register::R0},
		Instruction::Sev,
		Instruction::Stm{addr: Register::R0, registers: RegisterSet::new()},
		Instruction::Str{src: Register::R0, addr: Register::R0, off: ImmReg::Immediate(0)},
		Instruction::Strb{src: Register::R0, addr: Register::R0, off: ImmReg::Immediate(0)},
		Instruction::Strh{src: Register::R0, addr: Register::R0, off: ImmReg::Immediate(0)},
		Instruction::Sub{flags: false, dst: Register::SP, lhs: Register::SP, rhs: ImmReg::Immediate(0)},
		Instruction::Sub{flags: true, dst: Register::R0, lhs: Register::R0, rhs: ImmReg::Immediate(0)},
		Instruction::Svc{info: 0},
		Instruction::Sxtb{dst: Register::R0, value: Register::R0},
		Instruction::Sxth{dst: Register::R0, value: Register::R0},
		Instruction::Tst{lhs: Register::R0, rhs: Register::R0},
		Instruction::Udf{info: 0},
		Instruction::Udfw{info: 0},
		Instruction::Uxtb{dst: Register::R0, value: Register::R0},
		Instruction::Uxth{dst: Register::R0, value: Register::R0},
		Instruction::Wfe,
		Instruction::Wfi,
		Instruction::Yield,
	];
	
	for instr in instrs
	{
		if let Err(e) = instr.encode(&mut [0u8; 4])
		{
			panic!("cannot encode {instr:?}: {e}");
		}
	}
}
