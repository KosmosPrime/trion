use crate::arm6m::asm::{ImmReg, Instruction};
use crate::arm6m::cond::Condition;
use crate::arm6m::reg::Register;
use crate::arm6m::regset::RegisterSet;
use crate::arm6m::sysreg::SystemReg;
use crate::asm::{Context, LabelError};
use crate::asm::instr::{InstructionSet, InstrErrorKind, InstructionError};
use crate::text::Positioned;
use crate::text::parse::{Argument, ArgumentType};
use crate::text::token::Number;

pub mod asm;
pub mod cond;
pub mod sysreg;
pub mod reg;
pub mod regset;

enum AddrLabel<'l>
{
	Address(Register, Option<ImmReg>),
	Label(&'l str),
}

pub struct Arm6M;

impl InstructionSet for Arm6M
{
	fn assemble<'c>(&self, ctx: &'c mut Context, line: u32, col: u32, name: &str, args: Vec<Argument>) -> Result<(), &'c InstructionError>
	{
		let active = ctx.active_mut().unwrap();
		let mut arg_pos = 0;
		macro_rules!convert
		{
			($($data:ident: $which:ident),* {$($result:tt)+}) =>
			{
				{
					if args.len() - arg_pos > convert!(@impl/count {$($which)*})
					{
						let need = arg_pos.saturating_add(convert!(@impl/count {$($which)*}));
						let err = InstrErrorKind::TooManyArguments{instr: name.to_owned(), need, have: args.len()};
						return Err(ctx.push_error(Positioned{value: err, line, col}));
					}
					if args.len() - arg_pos < convert!(@impl/count {$($which)*})
					{
						let need = arg_pos.saturating_add(convert!(@impl/count {$($which)*}));
						let err = InstrErrorKind::NotEnoughArguments{instr: name.to_owned(), need, have: args.len()};
						return Err(ctx.push_error(Positioned{value: err, line, col}));
					}
					$(
						#[allow(unused_assignments)] // for blind arg_pos updates
						let $data = convert!(@impl/get $which);
					)*
					$($result)+
				}
			};
			(@impl/count {$($arg:tt)*}) => {<[()]>::len(&[$(convert!(@impl/count/map $arg)),*])};
			(@impl/count/map $_:tt) => {()};
			(@impl/get Immediate) =>
			{
				match args[arg_pos]
				{
					Argument::Constant(Number::Integer(val)) =>
					{
						let Ok(val) = i32::try_from(val)
						else
						{
							let err = InstrErrorKind::ValueRange{instr: name.to_owned(), idx: arg_pos + 1};
							return Err(ctx.push_error(Positioned{value: err, line, col}));
						};
						arg_pos += 1;
						val
					},
					ref arg =>
					{
						let err = InstrErrorKind::ArgumentType{instr: name.to_owned(), idx: arg_pos + 1, need: ArgumentType::Constant, have: arg.get_type()};
						return Err(ctx.push_error(Positioned{value: err, line, col}));
					},
				}
			};
			(@impl/get Identifier) =>
			{
				match args[arg_pos]
				{
					Argument::Identifier(ident) =>
					{
						arg_pos += 1;
						ident
					},
					ref arg =>
					{
						let err = InstrErrorKind::ArgumentType{instr: name.to_owned(), idx: arg_pos + 1, need: ArgumentType::Identifier, have: arg.get_type()};
						return Err(ctx.push_error(Positioned{value: err, line, col}));
					},
				}
			};
			(@impl/get Register) =>
			{
				match args[arg_pos]
				{
					Argument::Identifier(ident) =>
					{
						let reg = match regl(ident, name, arg_pos + 1)
						{
							Ok(r) => r,
							Err(e) => return Err(ctx.push_error(Positioned{value: e, line, col})),
						};
						arg_pos += 1;
						reg
					},
					ref arg =>
					{
						let err = InstrErrorKind::ArgumentType{instr: name.to_owned(), idx: arg_pos + 1, need: ArgumentType::Identifier, have: arg.get_type()};
						return Err(ctx.push_error(Positioned{value: err, line, col}));
					},
				}
			};
			(@impl/get SystemReg) =>
			{
				match args[arg_pos]
				{
					Argument::Identifier(ident) =>
					{
						let reg = match sysl(ident, name, arg_pos + 1)
						{
							Ok(r) => r,
							Err(e) => return Err(ctx.push_error(Positioned{value: e, line, col})),
						};
						arg_pos += 1;
						reg
					},
					ref arg =>
					{
						let err = InstrErrorKind::ArgumentType{instr: name.to_owned(), idx: arg_pos + 1, need: ArgumentType::Identifier, have: arg.get_type()};
						return Err(ctx.push_error(Positioned{value: err, line, col}));
					},
				}
			};
			(@impl/get ImmReg) =>
			{
				match args[arg_pos]
				{
					Argument::Constant(Number::Integer(val)) =>
					{
						let Ok(val) = i32::try_from(val)
						else
						{
							let err = InstrErrorKind::ValueRange{instr: name.to_owned(), idx: arg_pos + 1};
							return Err(ctx.push_error(Positioned{value: err, line, col}));
						};
						arg_pos += 1;
						ImmReg::Immediate(val)
					},
					Argument::Identifier(ident) =>
					{
						let reg = match regl(ident, name, arg_pos + 1)
						{
							Ok(r) => r,
							Err(e) => return Err(ctx.push_error(Positioned{value: e, line, col})),
						};
						arg_pos += 1;
						ImmReg::Register(reg)
					},
					ref arg =>
					{
						// REM should indicate that an immediate value would also work
						let err = InstrErrorKind::ArgumentType{instr: name.to_owned(), idx: arg_pos + 1, need: ArgumentType::Identifier, have: arg.get_type()};
						return Err(ctx.push_error(Positioned{value: err, line, col}));
					},
				}
			};
			(@impl/get RegSet) =>
			{
				match args[arg_pos]
				{
					Argument::Sequence(ref items) =>
					{
						let regs = match regset(items, name, arg_pos + 1)
						{
							Ok(rs) => rs,
							Err(e) => return Err(ctx.push_error(Positioned{value: e, line, col})),
						};
						arg_pos += 1;
						regs
					},
					ref arg =>
					{
						let err = InstrErrorKind::ArgumentType{instr: name.to_owned(), idx: arg_pos + 1, need: ArgumentType::Sequence, have: arg.get_type()};
						return Err(ctx.push_error(Positioned{value: err, line, col}));
					},
				}
			};
			(@impl/get Address) =>
			{
				match args[arg_pos]
				{
					Argument::Address(ref addr) =>
					{
						let (addr, off) = match addr_off(&*addr, name, arg_pos + 1)
						{
							Ok((a, o)) => (a, o),
							Err(e) => return Err(ctx.push_error(Positioned{value: e, line, col})),
						};
						arg_pos += 1;
						(addr, off)
					},
					ref arg =>
					{
						let err = InstrErrorKind::ArgumentType{instr: name.to_owned(), idx: arg_pos + 1, need: ArgumentType::Address, have: arg.get_type()};
						return Err(ctx.push_error(Positioned{value: err, line, col}));
					},
				}
			};
			(@impl/get AddrLabel) =>
			{
				match args[arg_pos]
				{
					Argument::Address(ref addr) =>
					{
						let (addr, off) = match addr_off(&*addr, name, arg_pos + 1)
						{
							Ok((a, o)) => (a, o),
							Err(e) => return Err(ctx.push_error(Positioned{value: e, line, col})),
						};
						arg_pos += 1;
						AddrLabel::Address(addr, off)
					},
					Argument::Identifier(ident) =>
					{
						arg_pos += 1;
						AddrLabel::Label(ident)
					},
					ref arg =>
					{
						// REM should indicate that an address would also work
						let err = InstrErrorKind::ArgumentType{instr: name.to_owned(), idx: arg_pos + 1, need: ArgumentType::Identifier, have: arg.get_type()};
						return Err(ctx.push_error(Positioned{value: err, line, col}));
					},
				}
			};
		}
		
		let mut defer_label = None;
		let mut temp_name = [0u8; 16];
		let temp_name = if name.len() < temp_name.len()
		{
			temp_name[..name.len()].copy_from_slice(name.as_bytes());
			temp_name.make_ascii_uppercase();
			// SAFETY: above operation preserves ASCII-ness and leaves other unicode as-is
			unsafe{core::str::from_utf8_unchecked(&temp_name[..name.len()])}
		}
		else {&""};
		let instr = match temp_name
		{
			"ADCS" => convert!(dst: Register, rhs: Register{Instruction::Adc{dst, rhs}}),
			"ADD" | "ADDS" => convert!(dst: Register, lhs: Register, rhs: ImmReg{Instruction::Add{flags: name.len() > 3, dst, lhs, rhs}}),
			"ADR" =>
			{
				let (dst, label) = convert!(dst: Register, label: Identifier{(dst, label)});
				let instr = Instruction::Adr{dst, off: 0};
				defer_label = Some(label);
				instr
			},
			"ANDS" => convert!(dst: Register, rhs: Register{Instruction::And{dst, rhs}}),
			"ASRS" => convert!(dst: Register, value: Register, shift: ImmReg{Instruction::Asr{dst, value, shift}}),
			"B" | "BEQ" | "BNE" | "BCS" | "BHS" | "BCC" | "BLO" | "BMI" | "BPL" | "BVS" | "BVC" | "BHI" | "BLS" | "BGE" | "BLT" | "BGT" | "BLE" =>
			{
				let cond = match name
				{
					"BEQ" => Condition::Equal,
					"BNE" => Condition::NonEqual,
					"BCS" => Condition::CarrySet,
					"BHS" => Condition::CarrySet,
					"BCC" => Condition::CarryClear,
					"BLO" => Condition::CarryClear,
					"BMI" => Condition::Minus,
					"BPL" => Condition::Plus,
					"BVS" => Condition::Overflow,
					"BVC" => Condition::NoOverflow,
					"BHI" => Condition::Higher,
					"BLS" => Condition::LowerEqual,
					"BGE" => Condition::GreaterEqual,
					"BLT" => Condition::Less,
					"BGT" => Condition::Greater,
					"BLE" => Condition::LessEqual,
					"B" => Condition::Always,
					_ => unreachable!("unmapped conditional branch {name}"),
				};
				let label = convert!(label: Identifier{label});
				let instr = Instruction::B{cond, off: 0};
				defer_label = Some(label);
				instr
			},
			"BIC" => convert!(dst: Register, rhs: Register{Instruction::And{dst, rhs}}),
			"BKPT" =>
			{
				match convert!(info: Immediate{info})
				{
					info @ 0.. if info <= u8::MAX as i32 => Instruction::Bkpt{info: info as u8},
					_ =>
					{
						let err = InstrErrorKind::ValueRange{instr: name.to_owned(), idx: arg_pos};
						return Err(ctx.push_error(Positioned{value: err, line, col}));
					},
				}
			},
			"BL" =>
			{
				let label = convert!(label: Identifier{label});
				let instr = Instruction::Bl{off: 0};
				defer_label = Some(label);
				instr
			},
			"BLX" => convert!(off: Register{Instruction::Blx{off}}),
			"BX" => convert!(off: Register{Instruction::Bx{off}}),
			"CMN" => convert!(lhs: Register, rhs: Register{Instruction::Cmn{lhs, rhs}}),
			"CMP" => convert!(lhs: Register, rhs: ImmReg{Instruction::Cmp{lhs, rhs}}),
			"CPSID" | "CPSIE" =>
			{
				convert!(pm: Identifier
				{
					if pm != "i"
					{
						let err = InstrErrorKind::ValueRange{instr: name.to_owned(), idx: arg_pos};
						return Err(ctx.push_error(Positioned{value: err, line, col}));
					}
				});
				Instruction::Cps{enable: name == "CPSIE"}
			},
			"DMB" =>
			{
				convert!(opt: Identifier
				{
					if opt != "SV"
					{
						let err = InstrErrorKind::ValueRange{instr: name.to_owned(), idx: arg_pos};
						return Err(ctx.push_error(Positioned{value: err, line, col}));
					}
				});
				Instruction::Dmb
			},
			"DSB" =>
			{
				convert!(opt: Identifier
				{
					if opt != "SV"
					{
						let err = InstrErrorKind::ValueRange{instr: name.to_owned(), idx: arg_pos};
						return Err(ctx.push_error(Positioned{value: err, line, col}));
					}
				});
				Instruction::Dsb
			},
			"EORS" => convert!(dst: Register, rhs: Register{Instruction::Eor{dst, rhs}}),
			"ISB" =>
			{
				convert!(opt: Identifier
				{
					if opt != "SV"
					{
						let err = InstrErrorKind::ValueRange{instr: name.to_owned(), idx: arg_pos};
						return Err(ctx.push_error(Positioned{value: err, line, col}));
					}
				});
				Instruction::Isb
			},
			"LDM" => convert!(addr: Register, registers: RegSet{Instruction::Ldm{addr, registers}}),
			"LDR" =>
			{
				convert!(dst: Register, addr: AddrLabel
				{
					match addr
					{
						AddrLabel::Address(addr, off) => Instruction::Ldr{dst, addr, off: off.unwrap_or(ImmReg::Immediate(0))},
						AddrLabel::Label(label) =>
						{
							let instr = Instruction::Ldr{dst, addr: Register::PC, off: ImmReg::Immediate(0)};
							defer_label = Some(label);
							instr
						},
					}
				})
			},
			"LDRB" | "LDRH" | "LDRSB" | "LDRSH" =>
			{
				convert!(dst: Register, addr: Address
				{
					match name
					{
						"LDRB" => Instruction::Ldrb{dst, addr: addr.0, off: addr.1.unwrap_or(ImmReg::Immediate(0))},
						"LDRH" => Instruction::Ldrh{dst, addr: addr.0, off: addr.1.unwrap_or(ImmReg::Immediate(0))},
						"LDRSB" =>
						{
							match addr.1
							{
								None | Some(ImmReg::Immediate(..)) =>
								{
									let err = InstrErrorKind::ValueRange{instr: name.to_owned(), idx: arg_pos};
									return Err(ctx.push_error(Positioned{value: err, line, col}));
								},
								Some(ImmReg::Register(off)) => Instruction::Ldrsb{dst, addr: addr.0, off},
							}
						},
						"LDRSH" =>
						{
							match addr.1
							{
								None | Some(ImmReg::Immediate(..)) =>
								{
									let err = InstrErrorKind::ValueRange{instr: name.to_owned(), idx: arg_pos};
									return Err(ctx.push_error(Positioned{value: err, line, col}));
								},
								Some(ImmReg::Register(off)) => Instruction::Ldrsh{dst, addr: addr.0, off},
							}
						},
						_ => unreachable!("unmapped load register opcode {name}"),
					}
				})
			},
			"LSLS" => convert!(dst: Register, value: Register, shift: ImmReg{Instruction::Lsl{dst, value, shift}}),
			"LSRS" => convert!(dst: Register, value: Register, shift: ImmReg{Instruction::Lsr{dst, value, shift}}),
			"MOV" | "MOVS" => convert!(dst: Register, src: ImmReg{Instruction::Mov{flags: name.len() > 3, dst, src}}),
			"MRS" => convert!(dst: Register, src: SystemReg{Instruction::Mrs{dst, src}}),
			"MSR" => convert!(dst: SystemReg, src: Register{Instruction::Msr{dst, src}}),
			"MULS" => convert!(dst: Register, rhs: Register{Instruction::Mul{dst, rhs}}),
			"MVNS" => convert!(dst: Register, value: Register{Instruction::Mvn{dst, value}}),
			"NOP" => convert!({Instruction::Nop}),
			"ORRS" => convert!(dst: Register, rhs: Register{Instruction::Orr{dst, rhs}}),
			"POP" => convert!(registers: RegSet{Instruction::Pop{registers}}),
			"PUSH" => convert!(registers: RegSet{Instruction::Push{registers}}),
			"REV" => convert!(dst: Register, value: Register{Instruction::Rev{dst, value}}),
			"REV16" => convert!(dst: Register, value: Register{Instruction::Rev16{dst, value}}),
			"REVSH" => convert!(dst: Register, value: Register{Instruction::Revsh{dst, value}}),
			"RORS" => convert!(dst: Register, rhs: Register{Instruction::Ror{dst, rhs}}),
			"RSBS" =>
			{
				convert!(dst: Register, lhs: Register, rhs: Immediate
				{
					if rhs != 0
					{
						let err = InstrErrorKind::ValueRange{instr: name.to_owned(), idx: arg_pos};
						return Err(ctx.push_error(Positioned{value: err, line, col}));
					}
					Instruction::Rsb{dst, lhs}
				})
			},
			"SBCS" => convert!(dst: Register, rhs: Register{Instruction::Sbc{dst, rhs}}),
			"SEV" => convert!({Instruction::Sev}),
			"STM" => convert!(addr: Register, registers: RegSet{Instruction::Stm{addr, registers}}),
			"STR" | "STRB" | "STRH" =>
			{
				convert!(src: Register, addr: Address
				{
					match name
					{
						"STR" => Instruction::Str{src, addr: addr.0, off: addr.1.unwrap_or(ImmReg::Immediate(0))},
						"STRB" => Instruction::Strb{src, addr: addr.0, off: addr.1.unwrap_or(ImmReg::Immediate(0))},
						"STRH" => Instruction::Strh{src, addr: addr.0, off: addr.1.unwrap_or(ImmReg::Immediate(0))},
						_ => unreachable!("unmapped store register opcode {name}"),
					}
				})
			},
			"SUB" | "SUBS" => convert!(dst: Register, lhs: Register, rhs: ImmReg{Instruction::Sub{flags: name.len() > 3, dst, lhs, rhs}}),
			"SVC" =>
			{
				match convert!(info: Immediate{info})
				{
					info @ 0.. if info <= u8::MAX as i32 => Instruction::Svc{info: info as u8},
					_ =>
					{
						let err = InstrErrorKind::ValueRange{instr: name.to_owned(), idx: arg_pos};
						return Err(ctx.push_error(Positioned{value: err, line, col}));
					},
				}
			},
			"SXTB" => convert!(dst: Register, value: Register{Instruction::Sxtb{dst, value}}),
			"SXTH" => convert!(dst: Register, value: Register{Instruction::Sxth{dst, value}}),
			"TST" => convert!(lhs: Register, rhs: Register{Instruction::Tst{lhs, rhs}}),
			"UDF" | "UDF.N" | "UDF.W" =>
			{
				match convert!(info: Immediate{info})
				{
					info @ 0.. if info <= u8::MAX as i32 && name.as_bytes()[name.len() - 1] != b'W' => Instruction::Udf{info: info as u8},
					info @ 0.. if info <= u16::MAX as i32 && name.as_bytes()[name.len() - 1] != b'N' => Instruction::Udfw{info: info as u16},
					_ =>
					{
						let err = InstrErrorKind::ValueRange{instr: name.to_owned(), idx: arg_pos};
						return Err(ctx.push_error(Positioned{value: err, line, col}));
					},
				}
			},
			"UXTB" => convert!(dst: Register, value: Register{Instruction::Uxtb{dst, value}}),
			"UXTH" => convert!(dst: Register, value: Register{Instruction::Uxth{dst, value}}),
			"WFE" => convert!({Instruction::Wfe}),
			"WFI" => convert!({Instruction::Wfi}),
			"YIELD" => convert!({Instruction::Yield}),
			_ => return Err(ctx.push_error(Positioned{value: InstrErrorKind::NoSuchInstruction(name.to_owned()), line, col})),
		};
		let mut tmp = [0u8; 4];
		let curr_addr = active.curr_addr();
		match instr.write(&mut tmp)
		{
			Ok(len) =>
			{
				if let Err(e) = active.write(&tmp[..len])
				{
					return Err(ctx.push_error(Positioned{value: InstrErrorKind::Write(e), line, col}));
				}
			},
			Err(e) => return Err(ctx.push_error(Positioned{value: InstrErrorKind::Generic(Box::new(e)), line, col})),
		}
		if let Some(label) = defer_label
		{
			let label = label.to_owned();
			ctx.add_task(Box::new(move |ctx| deferred_instr(ctx, line, col, curr_addr, instr, label.as_str())));
		}
		Ok(())
	}
}

fn deferred_instr(ctx: &mut Context, line: u32, col: u32, addr: u32, mut instr: Instruction, label: &str)
{
	let tgt = match ctx.labels().get(label)
	{
		None =>
		{
			eprintln!("No such label {label:?} ({line}:{col})");
			ctx.push_error(LabelError::NotFound(label.to_owned()));
			return;
		},
		Some(&t) => t,
	};
	
	match instr
	{
		Instruction::Adr{ref mut off, ..} =>
		{
			let al_pc = (addr & !0b11).wrapping_add(4);
			if tgt < al_pc || tgt - al_pc > 0xFF << 2
			{
				eprintln!("Label out of range ({line}:{col})");
				ctx.push_error(LabelError::Range{min: 0, max: 0xFF << 2, have: tgt as i64 - al_pc as i64});
			}
			else if ((tgt - al_pc) & 0b11) != 0
			{
				eprintln!("Misaligned label ({line}:{col})");
				ctx.push_error(LabelError::Alignment{align: 0b100, have: (tgt - al_pc) as i64});
			}
			else {*off = (tgt - al_pc) as u16;}
		},
		Instruction::B{cond, ref mut off} =>
		{
			let pc = addr.wrapping_add(4);
			let off_val = i64::from(tgt) - i64::from(pc);
			let (min, max) = if cond == Condition::Always{(-0b10000000000_0, 0b01111111111_0)} else {(-0b10000000_0, 0b01111111_0)};
			if off_val < min || off_val > max
			{
				eprintln!("Label out of range ({line}:{col})");
				ctx.push_error(LabelError::Range{min, max, have: off_val});
			}
			else if (off_val & 0b1) != 0
			{
				eprintln!("Misaligned label ({line}:{col})");
				ctx.push_error(LabelError::Alignment{align: 0b10, have: off_val});
			}
			else {*off = off_val as i32}
		},
		Instruction::Bl{ref mut off} =>
		{
			let pc = addr.wrapping_add(4);
			let off_val = i64::from(tgt) - i64::from(pc);
			let (min, max) = (-1 << 24, (1 << 24) - 1);
			if off_val < min || off_val > max
			{
				eprintln!("Label out of range ({line}:{col})");
				ctx.push_error(LabelError::Range{min, max, have: off_val});
			}
			else if (off_val & 0b1) != 0
			{
				eprintln!("Misaligned label ({line}:{col})");
				ctx.push_error(LabelError::Alignment{align: 0b10, have: off_val});
			}
			else {*off = off_val as i32}
		},
		Instruction::Ldr{addr: Register::PC, ref mut off, ..} =>
		{
			let al_pc = (addr & !0b11).wrapping_add(4);
			if tgt < al_pc || tgt - al_pc > 0xFF << 2
			{
				eprintln!("Label out of range ({line}:{col})");
				ctx.push_error(LabelError::Range{min: 0, max: 0xFF << 2, have: tgt as i64 - al_pc as i64});
			}
			else if ((tgt - al_pc) & 0b11) != 0
			{
				eprintln!("Misaligned label ({line}:{col})");
				ctx.push_error(LabelError::Alignment{align: 0b100, have: (tgt - al_pc) as i64});
			}
			else {*off = ImmReg::Immediate((tgt - al_pc) as i32);}
		},
		_ => unreachable!("missing deferred handler for {instr:?}"),
	}
	
	let mut tmp = [0u8; 4];
	match instr.write(&mut tmp)
	{
		Ok(len) =>
		{
			if let Err(e) = ctx.output_mut().put(addr, &tmp[..len])
			{
				ctx.push_error(Positioned{value: InstrErrorKind::Generic(Box::new(e)), line, col});
			}
		},
		Err(e) =>
		{
			ctx.push_error(Positioned{value: InstrErrorKind::Generic(Box::new(e)), line, col});
		},
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
			_ => Err(InstrErrorKind::NoSuchRegister{instr: instr.to_owned(), idx, what: name.to_owned()}),
		}
	}
	else {Err(InstrErrorKind::NoSuchRegister{instr: instr.to_owned(), idx, what: name.to_owned()})}
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
			_ => Err(InstrErrorKind::NoSuchRegister{instr: instr.to_owned(), idx, what: name.to_owned()}),
		}
	}
	else {Err(InstrErrorKind::NoSuchRegister{instr: instr.to_owned(), idx, what: name.to_owned()})}
}

fn regset<'l>(items: &Vec<Argument<'l>>, instr: &'l str, idx: usize) -> Result<RegisterSet, InstrErrorKind>
{
	let mut regs = RegisterSet::new();
	for arg in items
	{
		match arg
		{
			Argument::Identifier(ident) => {regs.add(regl(ident, instr, idx)?);},
			_ => return Err(InstrErrorKind::ArgumentType{instr: instr.to_owned(), idx, need: ArgumentType::Identifier, have: arg.get_type()}),
		}
	}
	Ok(regs)
}

fn addr_off<'l>(addr: &Argument<'l>, instr: &'l str, idx: usize) -> Result<(Register, Option<ImmReg>), InstrErrorKind>
{
	match addr
	{
		&Argument::Identifier(name) =>
		{
			Ok((regl(name, instr, idx)?, Some(ImmReg::Immediate(0))))
		},
		Argument::Add(parts) =>
		{
			if parts.len() != 2
			{
				return Err(InstrErrorKind::ValueRange{instr: instr.to_owned(), idx});
			}
			match (&parts[0], &parts[1])
			{
				(&Argument::Identifier(addr), &Argument::Identifier(off)) =>
				{
					Ok((regl(addr, instr, idx)?, Some(ImmReg::Register(regl(off, instr, idx)?))))
				},
				(&Argument::Identifier(addr), &Argument::Constant(Number::Integer(off))) |
					(&Argument::Constant(Number::Integer(off)), &Argument::Identifier(addr)) =>
				{
					let Ok(off) = i32::try_from(off)
					else
					{
						return Err(InstrErrorKind::ValueRange{instr: instr.to_owned(), idx});
					};
					Ok((regl(addr, instr, idx)?, Some(ImmReg::Immediate(off))))
				},
				_ => return Err(InstrErrorKind::ValueRange{instr: instr.to_owned(), idx}),
			}
		},
		_ => return Err(InstrErrorKind::ValueRange{instr: instr.to_owned(), idx}),
	}
}
