use core::fmt;
use std::error::Error;
use std::sync::Arc;

use crate::arm6m::asm::{ImmReg, Instruction, EncodeError};
use crate::arm6m::cond::Condition;
use crate::arm6m::reg::Register;
use crate::arm6m::regset::RegisterSet;
use crate::arm6m::sysreg::SystemReg;
use crate::asm::arcob::Arcob;
use crate::asm::{ConstantError, Context, ErrorLevel, SegmentError};
use crate::asm::constant::Realm;
use crate::asm::instr::{InstrErrorKind, InstructionError, InstructionSet};
use crate::asm::simplify::{EvalError, evaluate, Evaluation};
use crate::text::{PosNamed, Positioned};
use crate::text::parse::{Argument, ArgumentType};
use crate::text::parse::choice::ArgChoice;
use crate::text::token::Number;

pub mod asm;
pub mod cond;
pub mod sysreg;
pub mod reg;
pub mod regset;

enum AsmOp<'c>
{
	Completed,
	Deferred{cause: Arcob<'c, str>},
}

enum AddrOffset
{
	Address(Register, Option<ImmReg>),
	Offset(u32),
}

pub struct Arm6M;

impl InstructionSet for Arm6M
{
	fn is_register(&self, name: &str) -> bool
	{
		if name.len() <= 8
		{
			let mut temp = [0u8; 8];
			temp[..name.len()].copy_from_slice(name.as_bytes());
			temp.make_ascii_uppercase();
			match &temp[..name.len()]
			{
				b"R0" | b"R1" | b"R2" | b"R3" | b"R4" | b"R5" | b"R6" | b"R7" => true,
				b"R8" | b"R9" | b"R10" | b"R11" | b"R12" => true,
				b"R13" | b"SP" | b"R14" | b"LR" | b"R15" | b"PC" => true,
				b"APSR" | b"IAPSR" | b"EAPSR" | b"XPSR" | b"IPSR" | b"EPSR" | b"IEPSR" => true,
				b"MSP" | b"PSP" | b"PRIMASK" | b"CONTROL" => true,
				_ => false
			}
		}
		else {false}
	}
	
	fn assemble(&self, ctx: &mut Context, line: u32, col: u32, name: &str, args: Vec<Argument>) -> Result<(), ErrorLevel>
	{
		let addr = ctx.active().unwrap().curr_addr();
		match ArmInstr::new(ctx, line, col, name, addr, args)
		{
			Ok(mut instr) =>
			{
				match instr.assemble(ctx, true)
				{
					Ok(AsmOp::Completed) =>
					{
						instr.write_instr(ctx, false)?;
						Ok(())
					},
					_ =>
					{
						instr.write_instr(ctx, true)?; // padding for whatever follows
						instr.into_owned().schedule(ctx, false);
						Ok(())
					},
				}
			},
			Err(e) =>
			{
				ctx.push_error(e);
				Err(ErrorLevel::Fatal)
			},
		}
	}
}

struct ArmInstr<'l>
{
	file_name: Arc<String>,
	line: u32,
	col: u32,
	addr: u32,
	instr: Instruction,
	args_done: usize,
	args: Vec<Argument<'l>>,
}

impl<'l> ArmInstr<'l>
{
	fn new(ctx: &Context, line: u32, col: u32, name: &str, addr: u32, args: Vec<Argument<'l>>) -> Result<Self, InstructionError>
	{
		let mut temp_name = [0u8; 16];
		let name = if name.len() < temp_name.len()
		{
			temp_name[..name.len()].copy_from_slice(name.as_bytes());
			// SAFETY: we've copied this from a `str`
			let temp_name = unsafe{core::str::from_utf8_unchecked_mut(&mut temp_name[..name.len()])};
			temp_name.make_ascii_uppercase();
			&*temp_name
		}
		else {&""};
		let instr = match name
		{
			"ADCS" => Instruction::Adc{dst: Register::R0, rhs: Register::R0},
			"ADD" | "ADDS" => Instruction::Add{flags: name.len() > 3, dst: Register::R0, lhs: Register::R0, rhs: ImmReg::Immediate(0)},
			"ADR" => Instruction::Adr{dst: Register::R0, off: 0},
			"ANDS" => Instruction::And{dst: Register::R0, rhs: Register::R0},
			"ASRS" => Instruction::Asr{dst: Register::R0, value: Register::R0, shift: ImmReg::Immediate(0)},
			"B" => Instruction::B{cond: Condition::Always, off: 0},
			"BCC" => Instruction::B{cond: Condition::CarryClear, off: 0},
			"BCS" => Instruction::B{cond: Condition::CarrySet, off: 0},
			"BEQ" => Instruction::B{cond: Condition::Equal, off: 0},
			"BGE" => Instruction::B{cond: Condition::GreaterEqual, off: 0},
			"BGT" => Instruction::B{cond: Condition::Greater, off: 0},
			"BHI" => Instruction::B{cond: Condition::Higher, off: 0},
			"BHS" => Instruction::B{cond: Condition::CarrySet, off: 0},
			"BIC" => Instruction::Bic{dst: Register::R0, rhs: Register::R0},
			"BKPT" => Instruction::Bkpt{info: 0},
			"BL" => Instruction::Bl{off: 0},
			"BLE" => Instruction::B{cond: Condition::LessEqual, off: 0},
			"BLO" => Instruction::B{cond: Condition::CarryClear, off: 0},
			"BLS" => Instruction::B{cond: Condition::LowerEqual, off: 0},
			"BLT" => Instruction::B{cond: Condition::Less, off: 0},
			"BLX" => Instruction::Blx{off: Register::R0},
			"BMI" => Instruction::B{cond: Condition::Minus, off: 0},
			"BNE" => Instruction::B{cond: Condition::NonEqual, off: 0},
			"BPL" => Instruction::B{cond: Condition::Plus, off: 0},
			"BVC" => Instruction::B{cond: Condition::NoOverflow, off: 0},
			"BVS" => Instruction::B{cond: Condition::Overflow, off: 0},
			"BX" => Instruction::Bx{off: Register::R0},
			"CMN" => Instruction::Cmn{lhs: Register::R0, rhs: Register::R0},
			"CMP" => Instruction::Cmp{lhs: Register::R0, rhs: ImmReg::Immediate(0)},
			"CPSID" => Instruction::Cps{enable: false},
			"CPSIE" => Instruction::Cps{enable: true},
			"DMB" => Instruction::Dmb,
			"DSB" => Instruction::Dsb,
			"EORS" => Instruction::Eor{dst: Register::R0, rhs: Register::R0},
			"ISB" => Instruction::Isb,
			"LDM" => Instruction::Ldm{addr: Register::R0, registers: RegisterSet::new()},
			"LDR" => Instruction::Ldr{dst: Register::R0, addr: Register::R0, off: ImmReg::Immediate(0)},
			"LDRB" => Instruction::Ldrb{dst: Register::R0, addr: Register::R0, off: ImmReg::Immediate(0)},
			"LDRH" => Instruction::Ldrh{dst: Register::R0, addr: Register::R0, off: ImmReg::Immediate(0)},
			"LDRSB" => Instruction::Ldrsb{dst: Register::R0, addr: Register::R0, off: Register::R0},
			"LDRSH" => Instruction::Ldrsh{dst: Register::R0, addr: Register::R0, off: Register::R0},
			"LSLS" => Instruction::Lsl{dst: Register::R0, value: Register::R0, shift: ImmReg::Immediate(0)},
			"LSRS" => Instruction::Lsr{dst: Register::R0, value: Register::R0, shift: ImmReg::Immediate(0)},
			"MOV" | "MOVS" => Instruction::Mov{flags: name.len() > 3, dst: Register::R0, src: ImmReg::Immediate(0)},
			"MRS" => Instruction::Mrs{dst: Register::R0, src: SystemReg::XPSR},
			"MSR" => Instruction::Msr{dst: SystemReg::XPSR, src: Register::R0},
			"MULS" => Instruction::Mul{dst: Register::R0, rhs: Register::R0},
			"MVNS" => Instruction::Mvn{dst: Register::R0, value: Register::R0},
			"NOP" => Instruction::Nop,
			"ORRS" => Instruction::Orr{dst: Register::R0, rhs: Register::R0},
			"POP" => Instruction::Pop{registers: RegisterSet::new()},
			"PUSH" => Instruction::Push{registers: RegisterSet::new()},
			"REV" => Instruction::Rev{dst: Register::R0, value: Register::R0},
			"REV16" => Instruction::Rev16{dst: Register::R0, value: Register::R0},
			"REVSH" => Instruction::Revsh{dst: Register::R0, value: Register::R0},
			"RORS" => Instruction::Ror{dst: Register::R0, rhs: Register::R0},
			"RSBS" => Instruction::Rsb{dst: Register::R0, lhs: Register::R0},
			"SBCS" => Instruction::Sbc{dst: Register::R0, rhs: Register::R0},
			"SEV" => Instruction::Sev,
			"STM" => Instruction::Stm{addr: Register::R0, registers: RegisterSet::new()},
			"STR" => Instruction::Str{src: Register::R0, addr: Register::R0, off: ImmReg::Immediate(0)},
			"STRB" => Instruction::Strb{src: Register::R0, addr: Register::R0, off: ImmReg::Immediate(0)},
			"STRH" => Instruction::Strh{src: Register::R0, addr: Register::R0, off: ImmReg::Immediate(0)},
			"SUB" | "SUBS" => Instruction::Sub{flags: name.len() > 3, dst: Register::R0, lhs: Register::R0, rhs: ImmReg::Immediate(0)},
			"SVC" => Instruction::Svc{info: 0},
			"SXTB" => Instruction::Sxtb{dst: Register::R0, value: Register::R0},
			"SXTH" => Instruction::Sxth{dst: Register::R0, value: Register::R0},
			"TST" => Instruction::Tst{lhs: Register::R0, rhs: Register::R0},
			"UDF.N" => Instruction::Udf{info: 0},
			"UDF.W" => Instruction::Udfw{info: 0},
			"UXTB" => Instruction::Uxtb{dst: Register::R0, value: Register::R0},
			"UXTH" => Instruction::Uxth{dst: Register::R0, value: Register::R0},
			"WFE" => Instruction::Wfe,
			"WFI" => Instruction::Wfi,
			"YIELD" => Instruction::Yield,
			_ => return Err(Positioned{line, col, value: InstrErrorKind::NotFound(name.to_owned())}),
		};
		Ok(Self{file_name: ctx.curr_file_name(), line, col, addr, instr, args_done: 0, args})
	}
	
	fn push_error<E: Error + 'static>(&mut self, ctx: &mut Context, source: E)
	{
		self.push_error_raw(ctx, InstrErrorKind::Assemble(Box::new(source)));
	}
	
	fn push_error_raw(&mut self, ctx: &mut Context, err: InstrErrorKind)
	{
		ctx.push_error_in(PosNamed{name: self.file_name.clone(), line: self.line, col: self.col, value: err});
	}
	
	fn into_owned(self) -> ArmInstr<'static>
	{
		ArmInstr
		{
			file_name: self.file_name,
			line: self.line,
			col: self.col,
			addr: self.addr,
			instr: self.instr,
			args_done: self.args_done,
			args: Argument::vec_into_owned(self.args),
		}
	}
	
	fn assemble<'s>(&'s mut self, ctx: &mut Context, local: bool) -> Result<AsmOp<'s>, ErrorLevel>
	{
		let mut arg_pos = 0;
		let instr_name = self.instr.get_name();
		macro_rules!convert
		{
			($(($data:tt: $which:ident))*) =>
			{
				if self.args.len() - arg_pos > convert!(@impl/count {$($which)*})
				{
					let max = convert!(@impl/count {$($which)*});
					self.push_error_raw(ctx, InstrErrorKind::TooManyArguments{instr: instr_name.to_owned(), max, have: self.args.len()});
					return Err(ErrorLevel::Trivial);
				}
				if self.args.len() - arg_pos < convert!(@impl/count {$($which)*})
				{
					let need = convert!(@impl/count {$($which)*});
					self.push_error_raw(ctx, InstrErrorKind::NotEnoughArguments{instr: instr_name.to_owned(), need, have: self.args.len()});
					return Err(ErrorLevel::Trivial);
				}
				$(convert!(@impl/set $data $which);)*
			};
			(@impl/count {$($arg:tt)*}) => {<[()]>::len(&[$(convert!(@impl/count/map $arg)),*])};
			(@impl/count/map $_:tt) => {()};
			(@impl/set $dst:ident $which:ident) =>
			{
				let $dst = convert!(@impl/get $which);
			};
			(@impl/set {$dst:expr} $which:ident) =>
			{
				$dst = convert!(@impl/get $which);
			};
			(@impl/get Immediate) =>
			{
				{
					let arg = &mut self.args[arg_pos];
					if self.args_done <= arg_pos
					{
						match evaluate(arg, ctx)
						{
							Ok(Evaluation::Complete{..}) => (),
							Ok(Evaluation::Deferred{cause, ..}) => return Ok(AsmOp::Deferred{cause}),
							Err(e) =>
							{
								if local
								{
									if let EvalError::NoSuchVariable{name, ..} = e
									{
										return Ok(AsmOp::Deferred{cause: name});
									}
								}
								self.push_error(ctx, e);
								return Err(ErrorLevel::Trivial);
							},
						}
						self.args_done = arg_pos + 1;
					}
					match *arg
					{
						Argument::Constant(Number::Integer(val)) =>
						{
							let Ok(val) = i32::try_from(val)
							else
							{
								self.push_error(ctx, AsmError::ValueRange{instr: instr_name.to_owned(), idx: arg_pos + 1});
								return Err(ErrorLevel::Trivial);
							};
							#[allow(unused_assignments)] // always ignored for final argument
							{arg_pos += 1;}
							val
						},
						ref arg =>
						{
							let have = arg.get_type();
							self.push_error_raw(ctx, InstrErrorKind::ArgumentType
							{
								instr: instr_name.to_owned(),
								idx: arg_pos + 1,
								expect: ArgChoice::of(ArgumentType::Constant),
								have,
							});
							return Err(ErrorLevel::Trivial);
						},
					}
				}
			};
			(@impl/get Identifier) =>
			{
				{
					match self.args[arg_pos]
					{
						Argument::Identifier(..) => (),
						ref arg =>
						{
							let have = arg.get_type();
							self.push_error_raw(ctx, InstrErrorKind::ArgumentType
							{
								instr: instr_name.to_owned(),
								idx: arg_pos + 1,
								expect: ArgChoice::of(ArgumentType::Identifier),
								have,
							});
							return Err(ErrorLevel::Trivial);
						},
					}
					let Argument::Identifier(ref ident) = self.args[arg_pos] else {unreachable!()};
					#[allow(unused_assignments)] // always ignored for final argument
					{arg_pos += 1;}
					ident.as_ref()
				}
			};
			(@impl/get Register) =>
			{
				match self.args[arg_pos]
				{
					Argument::Identifier(ref ident) =>
					{
						let reg = match regl(ident.as_ref(), instr_name, arg_pos + 1)
						{
							Ok(r) => r,
							Err(e) =>
							{
								self.push_error_raw(ctx, e);
								return Err(ErrorLevel::Trivial);
							},
						};
						#[allow(unused_assignments)] // always ignored for final argument
						{arg_pos += 1;}
						reg
					},
					ref arg =>
					{
						let have = arg.get_type();
						self.push_error_raw(ctx, InstrErrorKind::ArgumentType
						{
							instr: instr_name.to_owned(),
							idx: arg_pos + 1,
							expect: ArgChoice::of(ArgumentType::Identifier),
							have,
						});
						return Err(ErrorLevel::Trivial);
					},
				}
			};
			(@impl/get SystemReg) =>
			{
				match self.args[arg_pos]
				{
					Argument::Identifier(ref ident) =>
					{
						let reg = match sysl(ident.as_ref(), instr_name, arg_pos + 1)
						{
							Ok(r) => r,
							Err(e) =>
							{
								self.push_error_raw(ctx, e);
								return Err(ErrorLevel::Trivial);
							},
						};
						#[allow(unused_assignments)] // always ignored for final argument
						{arg_pos += 1;}
						reg
					},
					ref arg =>
					{
						let have = arg.get_type();
						self.push_error_raw(ctx, InstrErrorKind::ArgumentType
						{
							instr: instr_name.to_owned(),
							idx: arg_pos + 1,
							expect: ArgChoice::of(ArgumentType::Identifier),
							have,
						});
						return Err(ErrorLevel::Trivial);
					},
				}
			};
			(@impl/get ImmReg) =>
			{
				{
					let arg = &mut self.args[arg_pos];
					if self.args_done <= arg_pos
					{
						match evaluate(arg, ctx)
						{
							Ok(Evaluation::Complete{..}) => (),
							Ok(Evaluation::Deferred{cause, ..}) => return Ok(AsmOp::Deferred{cause}),
							Err(e) =>
							{
								if local
								{
									if let EvalError::NoSuchVariable{name, ..} = e
									{
										return Ok(AsmOp::Deferred{cause: name});
									}
								}
								self.push_error(ctx, e);
								return Err(ErrorLevel::Trivial);
							},
						}
						self.args_done = arg_pos + 1;
					}
					match *arg
					{
						Argument::Constant(Number::Integer(val)) =>
						{
							let Ok(val) = i32::try_from(val)
							else
							{
								self.push_error(ctx, AsmError::ValueRange{instr: instr_name.to_owned(), idx: arg_pos + 1});
								return Err(ErrorLevel::Trivial);
							};
							#[allow(unused_assignments)] // always ignored for final argument
							{arg_pos += 1;}
							ImmReg::Immediate(val)
						},
						Argument::Identifier(ref ident) =>
						{
							let reg = match regl(ident.as_ref(), instr_name, arg_pos + 1)
							{
								Ok(r) => r,
								Err(e) =>
								{
									self.push_error_raw(ctx, e);
									return Err(ErrorLevel::Trivial);
								},
							};
							#[allow(unused_assignments)] // always ignored for final argument
							{arg_pos += 1;}
							ImmReg::Register(reg)
						},
						ref arg =>
						{
							let have = arg.get_type();
							self.push_error_raw(ctx, InstrErrorKind::ArgumentType
							{
								instr: instr_name.to_owned(),
								idx: arg_pos + 1,
								expect: ArgChoice::from([ArgumentType::Constant, ArgumentType::Identifier]),
								have,
							});
							return Err(ErrorLevel::Trivial);
						},
					}
				}
			};
			(@impl/get RegSet) =>
			{
				match self.args[arg_pos]
				{
					Argument::Sequence(ref items) =>
					{
						let regs = match regset(items, instr_name, arg_pos + 1)
						{
							Ok(rs) => rs,
							Err(e) =>
							{
								self.push_error_raw(ctx, e);
								return Err(ErrorLevel::Trivial);
							},
						};
						#[allow(unused_assignments)] // always ignored for final argument
						{arg_pos += 1;}
						regs
					},
					ref arg =>
					{
						let have = arg.get_type();
						self.push_error_raw(ctx, InstrErrorKind::ArgumentType
						{
							instr: instr_name.to_owned(),
							idx: arg_pos + 1,
							expect: ArgChoice::of(ArgumentType::Sequence),
							have,
						});
						return Err(ErrorLevel::Trivial);
					},
				}
			};
			(@impl/get Address) =>
			{
				{
					let arg = &mut self.args[arg_pos];
					if self.args_done <= arg_pos
					{
						match evaluate(arg, ctx)
						{
							Ok(Evaluation::Complete{..}) => (),
							Ok(Evaluation::Deferred{cause, ..}) => return Ok(AsmOp::Deferred{cause}),
							Err(e) =>
							{
								if local
								{
									if let EvalError::NoSuchVariable{name, ..} = e
									{
										return Ok(AsmOp::Deferred{cause: name});
									}
								}
								self.push_error(ctx, e);
								return Err(ErrorLevel::Trivial);
							},
						}
						self.args_done = arg_pos + 1;
					}
					match *arg
					{
						Argument::Address(ref addr) =>
						{
							let (addr, off) = match addr_off(&*addr, instr_name, arg_pos + 1)
							{
								Ok((a, o)) => (a, o),
								Err(e) =>
								{
									self.push_error_raw(ctx, e);
									return Err(ErrorLevel::Trivial);
								},
							};
							#[allow(unused_assignments)] // always ignored for final argument
							{arg_pos += 1;}
							(addr, off)
						},
						ref arg =>
						{
							let have = arg.get_type();
							self.push_error_raw(ctx, InstrErrorKind::ArgumentType
							{
								instr: instr_name.to_owned(),
								idx: arg_pos + 1,
								expect: ArgChoice::of(ArgumentType::Address),
								have,
							});
							return Err(ErrorLevel::Trivial);
						},
					}
				}
			};
			(@impl/get Offset) =>
			{
				{
					let arg = &mut self.args[arg_pos];
					if self.args_done <= arg_pos
					{
						match evaluate(arg, ctx)
						{
							Ok(Evaluation::Complete{..}) => (),
							Ok(Evaluation::Deferred{cause, ..}) => return Ok(AsmOp::Deferred{cause}),
							Err(e) =>
							{
								if local
								{
									if let EvalError::NoSuchVariable{name, ..} = e
									{
										return Ok(AsmOp::Deferred{cause: name});
									}
								}
								self.push_error(ctx, e);
								return Err(ErrorLevel::Trivial);
							},
						}
						self.args_done = arg_pos + 1;
					}
					match *arg
					{
						Argument::Constant(Number::Integer(val)) =>
						{
							let Ok(val) = u32::try_from(val)
							else
							{
								self.push_error(ctx, AsmError::ValueRange{instr: instr_name.to_owned(), idx: arg_pos + 1});
								return Err(ErrorLevel::Trivial);
							};
							#[allow(unused_assignments)] // always ignored for final argument
							{arg_pos += 1;}
							val
						},
						ref arg =>
						{
							let have = arg.get_type();
							self.push_error_raw(ctx, InstrErrorKind::ArgumentType
							{
								instr: instr_name.to_owned(),
								idx: arg_pos + 1,
								expect: ArgChoice::of(ArgumentType::Constant),
								have,
							});
							return Err(ErrorLevel::Trivial);
						},
					}
				}
			};
			(@impl/get AddrOffset) =>
			{
				{
					let arg = &mut self.args[arg_pos];
					if self.args_done <= arg_pos
					{
						match evaluate(arg, ctx)
						{
							Ok(Evaluation::Complete{..}) => (),
							Ok(Evaluation::Deferred{cause, ..}) => return Ok(AsmOp::Deferred{cause}),
							Err(e) =>
							{
								if local
								{
									if let EvalError::NoSuchVariable{name, ..} = e
									{
										return Ok(AsmOp::Deferred{cause: name});
									}
								}
								self.push_error(ctx, e);
								return Err(ErrorLevel::Trivial);
							},
						}
						self.args_done = arg_pos + 1;
					}
					match *arg
					{
						Argument::Constant(Number::Integer(val)) =>
						{
							let Ok(val) = u32::try_from(val)
							else
							{
								self.push_error(ctx, AsmError::ValueRange{instr: instr_name.to_owned(), idx: arg_pos + 1});
								return Err(ErrorLevel::Trivial);
							};
							#[allow(unused_assignments)] // always ignored for final argument
							{arg_pos += 1;}
							AddrOffset::Offset(val)
						},
						Argument::Address(ref addr) =>
						{
							let (addr, off) = match addr_off(addr.as_ref(), instr_name, arg_pos + 1)
							{
								Ok((a, o)) => (a, o),
								Err(e) =>
								{
									self.push_error_raw(ctx, e);
									return Err(ErrorLevel::Trivial);
								},
							};
							#[allow(unused_assignments)] // always ignored for final argument
							{arg_pos += 1;}
							AddrOffset::Address(addr, off)
						},
						ref arg =>
						{
							let have = arg.get_type();
							self.push_error_raw(ctx, InstrErrorKind::ArgumentType
							{
								instr: instr_name.to_owned(),
								idx: arg_pos + 1,
								expect: ArgChoice::from([ArgumentType::Constant, ArgumentType::Address]),
								have,
							});
							return Err(ErrorLevel::Trivial);
						},
					}
				}
			};
		}
		
		//let mut defer_label = None;
		match &mut self.instr
		{
			Instruction::Adc{dst, rhs} => {convert!(({*dst}: Register) ({*rhs}: Register));},
			Instruction::Add{dst, lhs, rhs, ..} => {convert!(({*dst}: Register) ({*lhs}: Register) ({*rhs}: ImmReg));},
			Instruction::Adr{dst, off} =>
			{
				convert!(({*dst}: Register) (tgt: Offset));
				let al_pc = (self.addr & !0b11).wrapping_add(4);
				if tgt < al_pc || tgt - al_pc > 0xFF << 2
				{
					self.push_error(ctx, ConstantError::Range{min: 0, max: 0xFF << 2, have: tgt as i64 - al_pc as i64});
					return Err(ErrorLevel::Trivial);
				}
				else if ((tgt - al_pc) & 0b11) != 0
				{
					self.push_error(ctx, ConstantError::Alignment{align: 0b100, have: (tgt - al_pc) as i64});
					return Err(ErrorLevel::Trivial);
				}
				else
				{
					*off = (tgt - al_pc) as u16;
				}
			},
			Instruction::And{dst, rhs} => {convert!(({*dst}: Register) ({*rhs}: Register));},
			Instruction::Asr{dst, value, shift} => {convert!(({*dst}: Register) ({*value}: Register) ({*shift}: ImmReg));},
			&mut Instruction::B{cond, ref mut off} =>
			{
				convert!((tgt: Offset));
				let pc = self.addr.wrapping_add(4);
				let off_val = i64::from(tgt) - i64::from(pc);
				let (min, max) = if cond == Condition::Always{(-0b10000000000_0, 0b01111111111_0)} else {(-0b10000000_0, 0b01111111_0)};
				if off_val < min || off_val > max
				{
					self.push_error(ctx, ConstantError::Range{min, max, have: off_val});
					return Err(ErrorLevel::Trivial);
				}
				else if (off_val & 0b1) != 0
				{
					self.push_error(ctx, ConstantError::Alignment{align: 0b10, have: off_val});
					return Err(ErrorLevel::Trivial);
				}
				else
				{
					*off = off_val as i32;
				}
			},
			Instruction::Bic{dst, rhs} => {convert!(({*dst}: Register) ({*rhs}: Register));},
			Instruction::Bkpt{info} =>
			{
				convert!((a_info: Offset));
				match u8::try_from(a_info)
				{
					Ok(i) => *info = i,
					Err(..) =>
					{
						self.push_error(ctx, AsmError::ValueRange{instr: instr_name.to_owned(), idx: arg_pos});
						return Err(ErrorLevel::Trivial);
					},
				}
			},
			Instruction::Bl{off} =>
			{
				convert!((tgt: Offset));
				let pc = self.addr.wrapping_add(4);
				let off_val = i64::from(tgt) - i64::from(pc);
				let (min, max) = (-1 << 24, (1 << 24) - 1);
				if off_val < min || off_val > max
				{
					self.push_error(ctx, ConstantError::Range{min, max, have: off_val});
					return Err(ErrorLevel::Trivial);
				}
				else if (off_val & 0b1) != 0
				{
					self.push_error(ctx, ConstantError::Alignment{align: 0b10, have: off_val});
					return Err(ErrorLevel::Trivial);
				}
				else
				{
					*off = off_val as i32;
				}
			},
			Instruction::Blx{off} => {convert!(({*off}: Register));},
			Instruction::Bx{off} => {convert!(({*off}: Register));},
			Instruction::Cmn{lhs, rhs} => {convert!(({*lhs}: Register) ({*rhs}: Register));},
			Instruction::Cmp{lhs, rhs} => {convert!(({*lhs}: Register) ({*rhs}: ImmReg));},
			Instruction::Cps{..} =>
			{
				convert!((pm: Identifier));
				if !pm.eq_ignore_ascii_case("i")
				{
					self.push_error(ctx, AsmError::ValueRange{instr: instr_name.to_owned(), idx: arg_pos});
					return Err(ErrorLevel::Trivial);
				}
			},
			Instruction::Dmb | Instruction::Dsb | Instruction::Isb =>
			{
				convert!((opt: Identifier));
				if !opt.eq_ignore_ascii_case("SV")
				{
					self.push_error(ctx, AsmError::ValueRange{instr: instr_name.to_owned(), idx: arg_pos});
					return Err(ErrorLevel::Trivial);
				}
			},
			Instruction::Eor{dst, rhs} => {convert!(({*dst}: Register) ({*rhs}: Register));},
			Instruction::Ldm{addr, registers} => {convert!(({*addr}: Register) ({*registers}: RegSet));},
			Instruction::Ldr{dst, addr, off} =>
			{
				convert!(({*dst}: Register) (a_addr: AddrOffset));
				match a_addr
				{
					AddrOffset::Address(a_addr, a_off) => {(*addr, *off) = (a_addr, a_off.unwrap_or(ImmReg::Immediate(0)));},
					AddrOffset::Offset(tgt) =>
					{
						let al_pc = (self.addr & !0b11).wrapping_add(4);
						if tgt < al_pc || tgt - al_pc > 0xFF << 2
						{
							self.push_error(ctx, ConstantError::Range{min: 0, max: 0xFF << 2, have: tgt as i64 - al_pc as i64});
							return Err(ErrorLevel::Trivial);
						}
						else if ((tgt - al_pc) & 0b11) != 0
						{
							self.push_error(ctx, ConstantError::Alignment{align: 0b100, have: (tgt - al_pc) as i64});
							return Err(ErrorLevel::Trivial);
						}
						else
						{
							*addr = Register::PC;
							*off = ImmReg::Immediate((tgt - al_pc) as i32);
						}
					},
				}
			},
			Instruction::Ldrb{dst, addr, off} | Instruction::Ldrh{dst, addr, off} =>
			{
				convert!(({*dst}: Register) (a_addr: Address));
				(*addr, *off) = (a_addr.0, a_addr.1.unwrap_or(ImmReg::Immediate(0)));
			},
			Instruction::Ldrsb{dst, addr, off} | Instruction::Ldrsh{dst, addr, off} =>
			{
				convert!(({*dst}: Register) (a_addr: Address));
				match a_addr.1
				{
					None | Some(ImmReg::Immediate(..)) =>
					{
						self.push_error(ctx, AsmError::ValueRange{instr: instr_name.to_owned(), idx: arg_pos});
						return Err(ErrorLevel::Trivial);
					},
					Some(ImmReg::Register(a_off)) => {(*addr, *off) = (a_addr.0, a_off);},
				}
			},
			Instruction::Lsl{dst, value, shift} => {convert!(({*dst}: Register) ({*value}: Register) ({*shift}: ImmReg));},
			Instruction::Lsr{dst, value, shift} => {convert!(({*dst}: Register) ({*value}: Register) ({*shift}: ImmReg));},
			Instruction::Mov{dst, src, ..} => {convert!(({*dst}: Register) ({*src}: ImmReg));},
			Instruction::Mrs{dst, src} => {convert!(({*dst}: Register) ({*src}: SystemReg));},
			Instruction::Msr{dst, src} => {convert!(({*dst}: SystemReg) ({*src}: Register));},
			Instruction::Mul{dst, rhs} => {convert!(({*dst}: Register) ({*rhs}: Register));},
			Instruction::Mvn{dst, value} => {convert!(({*dst}: Register) ({*value}: Register));},
			Instruction::Nop => {convert!();},
			Instruction::Orr{dst, rhs} => {convert!(({*dst}: Register) ({*rhs}: Register));},
			Instruction::Pop{registers} => {convert!(({*registers}: RegSet));},
			Instruction::Push{registers} => {convert!(({*registers}: RegSet));},
			Instruction::Rev{dst, value} => {convert!(({*dst}: Register) ({*value}: Register));},
			Instruction::Rev16{dst, value} => {convert!(({*dst}: Register) ({*value}: Register));},
			Instruction::Revsh{dst, value} => {convert!(({*dst}: Register) ({*value}: Register));},
			Instruction::Ror{dst, rhs} => {convert!(({*dst}: Register) ({*rhs}: Register));},
			Instruction::Rsb{dst, lhs} =>
			{
				convert!(({*dst}: Register) ({*lhs}: Register) (rhs: Immediate));
				if rhs != 0
				{
					self.push_error(ctx, AsmError::ValueRange{instr: instr_name.to_owned(), idx: arg_pos});
					return Err(ErrorLevel::Trivial);
				}
			},
			Instruction::Sbc{dst, rhs} => {convert!(({*dst}: Register) ({*rhs}: Register));},
			Instruction::Sev => {convert!();},
			Instruction::Stm{addr, registers} => {convert!(({*addr}: Register) ({*registers}: RegSet));},
			Instruction::Str{src, addr, off} | Instruction::Strb{src, addr, off} | Instruction::Strh{src, addr, off} =>
			{
				convert!(({*src}: Register) (a_addr: Address));
				(*addr, *off) = (a_addr.0, a_addr.1.unwrap_or(ImmReg::Immediate(0)));
			},
			Instruction::Sub{dst, lhs, rhs, ..} => {convert!(({*dst}: Register) ({*lhs}: Register) ({*rhs}: ImmReg));},
			Instruction::Svc{info} =>
			{
				convert!((a_info: Immediate));
				match u8::try_from(a_info)
				{
					Ok(i) => *info = i,
					Err(..) =>
					{
						self.push_error(ctx, AsmError::ValueRange{instr: instr_name.to_owned(), idx: arg_pos});
						return Err(ErrorLevel::Trivial);
					},
				}
			},
			Instruction::Sxtb{dst, value} => {convert!(({*dst}: Register) ({*value}: Register));},
			Instruction::Sxth{dst, value} => {convert!(({*dst}: Register) ({*value}: Register));},
			Instruction::Tst{lhs, rhs} => {convert!(({*lhs}: Register) ({*rhs}: Register));},
			Instruction::Udf{info} =>
			{
				convert!((a_info: Immediate));
				match u8::try_from(a_info)
				{
					Ok(i) => *info = i,
					Err(..) =>
					{
						self.push_error(ctx, AsmError::ValueRange{instr: instr_name.to_owned(), idx: arg_pos});
						return Err(ErrorLevel::Trivial);
					},
				}
			},
			Instruction::Udfw{info} =>
			{
				convert!((a_info: Immediate));
				match u16::try_from(a_info)
				{
					Ok(i) => *info = i,
					Err(..) =>
					{
						self.push_error(ctx, AsmError::ValueRange{instr: instr_name.to_owned(), idx: arg_pos});
						return Err(ErrorLevel::Trivial);
					},
				}
			},
			Instruction::Uxtb{dst, value} => {convert!(({*dst}: Register) ({*value}: Register));},
			Instruction::Uxth{dst, value} => {convert!(({*dst}: Register) ({*value}: Register));},
			Instruction::Wfe => {convert!();},
			Instruction::Wfi => {convert!();},
			Instruction::Yield => {convert!();},
		};
		Ok(AsmOp::Completed)
	}
	
	fn write_instr(&mut self, ctx: &mut Context, deferred: bool) -> Result<(), ErrorLevel>
	{
		let mut tmp = [0u8; 4];
		match self.instr.encode(&mut tmp)
		{
			Ok(len) =>
			{
				if deferred
				{
					tmp.fill(0xBE); // BKPT 0xBE;
				}
				match ctx.active_mut()
				{
					Some(active) if self.addr >= active.base_addr() && self.addr <= active.curr_addr() =>
					{
						if let Err(e) = active.write_at(self.addr, &tmp[..len])
						{
							self.push_error(ctx, AsmError::Write(e));
							return Err(ErrorLevel::Fatal);
						}
					},
					_ =>
					{
						match ctx.output_mut().put(self.addr, &tmp[..len])
						{
							Ok(n) => assert_eq!(n, 0), // the active segment has changed, this ensures the bytes have been pre-allocated
							Err(e) =>
							{
								self.push_error(ctx, AsmError::Write(SegmentError::Write(e)));
								return Err(ErrorLevel::Fatal);
							},
						}
					},
				};
				Ok(())
			},
			Err(e) =>
			{
				self.push_error(ctx, AsmError::Encode(e));
				return Err(ErrorLevel::Fatal);
			},
		}
	}
}

impl ArmInstr<'static>
{
	fn schedule(mut self, ctx: &mut Context, global: bool)
	{
		ctx.add_task(Box::new(move |ctx|
		{
			match self.assemble(ctx, false)
			{
				Ok(AsmOp::Completed) =>
				{
					self.write_instr(ctx, false)?;
					Ok(())
				},
				Ok(AsmOp::Deferred{cause}) =>
				{
					if global
					{
						let name = cause.as_ref().to_owned();
						self.push_error(ctx, ConstantError::NotFound{name, realm: Realm::Global});
						return Err(ErrorLevel::Trivial);
					}
					self.schedule(ctx, true);
					Ok(())
				},
				Err(e) => Err(e),
			}
		}), if global {Realm::Global} else {Realm::Local});
	}
}

fn regl<'l>(name: &'l str, instr: &'l str, idx: usize) -> Result<Register, InstrErrorKind>
{
	if name.len() <= 4
	{
		let mut temp = [0u8; 4];
		temp[..name.len()].copy_from_slice(name.as_bytes());
		temp.make_ascii_uppercase();
		match &temp[..name.len()]
		{
			b"R0" => Ok(Register::R0),
			b"R1" => Ok(Register::R1),
			b"R2" => Ok(Register::R2),
			b"R3" => Ok(Register::R3),
			b"R4" => Ok(Register::R4),
			b"R5" => Ok(Register::R5),
			b"R6" => Ok(Register::R6),
			b"R7" => Ok(Register::R7),
			b"R8" => Ok(Register::R8),
			b"R9" => Ok(Register::R9),
			b"R10" => Ok(Register::R10),
			b"R11" => Ok(Register::R11),
			b"R12" => Ok(Register::R12),
			b"R13" => Ok(Register::SP),
			b"SP" => Ok(Register::SP),
			b"R14" => Ok(Register::LR),
			b"LR" => Ok(Register::LR),
			b"R15" => Ok(Register::PC),
			b"PC" => Ok(Register::PC),
			_ =>
			{
				let source = Box::new(AsmError::NoSuchRegister{instr: instr.to_owned(), idx, what: name.to_owned()});
				Err(InstrErrorKind::Assemble(source))
			},
		}
	}
	else
	{
		let source = Box::new(AsmError::NoSuchRegister{instr: instr.to_owned(), idx, what: name.to_owned()});
		Err(InstrErrorKind::Assemble(source))
	}
}

fn sysl<'l>(name: &'l str, instr: &'l str, idx: usize) -> Result<SystemReg, InstrErrorKind>
{
	if name.len() <= 8
	{
		let mut temp = [0u8; 8];
		temp[..name.len()].copy_from_slice(name.as_bytes());
		temp.make_ascii_uppercase();
		match &temp[..name.len()]
		{
			b"APSR" => Ok(SystemReg::APSR),
			b"IAPSR" => Ok(SystemReg::IAPSR),
			b"EAPSR" => Ok(SystemReg::EAPSR),
			b"XPSR" => Ok(SystemReg::XPSR),
			b"IPSR" => Ok(SystemReg::IPSR),
			b"EPSR" => Ok(SystemReg::EPSR),
			b"IEPSR" => Ok(SystemReg::IEPSR),
			b"MSP" => Ok(SystemReg::MSP),
			b"PSP" => Ok(SystemReg::PSP),
			b"PRIMASK" => Ok(SystemReg::PRIMASK),
			b"CONTROL" => Ok(SystemReg::CONTROL),
			_ =>
			{
				let source = Box::new(AsmError::NoSuchRegister{instr: instr.to_owned(), idx, what: name.to_owned()});
				Err(InstrErrorKind::Assemble(source))
			},
		}
	}
	else
	{
		let source = Box::new(AsmError::NoSuchRegister{instr: instr.to_owned(), idx, what: name.to_owned()});
		Err(InstrErrorKind::Assemble(source))
	}
}

fn regset<'l>(items: &Vec<Argument<'l>>, instr: &'l str, idx: usize) -> Result<RegisterSet, InstrErrorKind>
{
	let mut regs = RegisterSet::new();
	for arg in items
	{
		match arg
		{
			Argument::Identifier(ident) => {regs.add(regl(ident, instr, idx)?);},
			_ => return Err(InstrErrorKind::ArgumentType{instr: instr.to_owned(), idx, expect: ArgChoice::of(ArgumentType::Identifier), have: arg.get_type()}),
		}
	}
	Ok(regs)
}

fn addr_off<'l>(addr: &Argument<'l>, instr: &'l str, idx: usize) -> Result<(Register, Option<ImmReg>), InstrErrorKind>
{
	match addr
	{
		Argument::Identifier(name) =>
		{
			Ok((regl(name.as_ref(), instr, idx)?, Some(ImmReg::Immediate(0))))
		},
		Argument::Add{lhs, rhs} =>
		{
			match (lhs.as_ref(), rhs.as_ref())
			{
				(Argument::Identifier(addr), Argument::Identifier(off)) =>
				{
					Ok((regl(addr.as_ref(), instr, idx)?, Some(ImmReg::Register(regl(off.as_ref(), instr, idx)?))))
				},
				(Argument::Identifier(addr), &Argument::Constant(Number::Integer(off))) |
					(&Argument::Constant(Number::Integer(off)), Argument::Identifier(addr)) =>
				{
					let Ok(off) = i32::try_from(off)
					else
					{
						let source = Box::new(AsmError::ValueRange{instr: instr.to_owned(), idx});
						return Err(InstrErrorKind::Assemble(source));
					};
					Ok((regl(addr.as_ref(), instr, idx)?, Some(ImmReg::Immediate(off))))
				},
				_ =>
				{
					let source = Box::new(AsmError::ValueRange{instr: instr.to_owned(), idx});
					return Err(InstrErrorKind::Assemble(source));
				},
			}
		},
		_ =>
		{
			let source = Box::new(AsmError::ValueRange{instr: instr.to_owned(), idx});
			return Err(InstrErrorKind::Assemble(source));
		},
	}
}

#[derive(Debug)]
pub enum AsmError
{
	ValueRange{instr: String, idx: usize},
	NoSuchRegister{instr: String, idx: usize, what: String},
	Encode(EncodeError),
	Write(SegmentError),
}

impl fmt::Display for AsmError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::ValueRange{instr, idx} => write!(f, "argument #{idx} for {instr} is out of range"),
			Self::NoSuchRegister{instr, idx, what} => write!(f, "argument #{idx} for {instr} has invalid register {what:?}"),
			Self::Encode(..) => f.write_str("could not encode instruction"),
			Self::Write(..) => f.write_str("could not write instruction to segment"),
		}
	}
}

impl Error for AsmError
{
	fn source(&self) -> Option<&(dyn Error + 'static)>
	{
		match self
		{
			Self::Encode(e) => Some(e),
			Self::Write(e) => Some(e),
			_ => None,
		}
	}
}
