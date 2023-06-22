use std::collections::HashMap;
use std::error::Error;

use crate::arm6m::asm::{ImmReg, Instruction};
use crate::arm6m::cond::Condition;
use crate::arm6m::reg::Register;
use crate::arm6m::regset::RegisterSet;
use crate::arm6m::sysreg::SystemReg;
use crate::text::parse::{Argument, ElementValue, Parser};
use crate::text::token::Number;
use crate::uf2::codec::encode;
use crate::uf2::crc::Crc;

fn print_err_trace(msg: &str, err: impl Error)
{
	eprintln!("{msg}: {err}");
	let mut source = err.source();
	while let Some(src) = source
	{
		eprintln!("\tsource: {src}");
		source = src.source();
	}
}

pub fn assemble(buff: &mut Vec<u8>) -> bool
{
	const BASE: u32 = 0x20000000; // FIXME remove this, should use an initial .addr
	let mut output = Vec::new();
	let mut image_addr: u32 = BASE;
	let mut temp_str = String::new();
	let mut labels = HashMap::new();
	let mut deferred = Vec::new();
	
	for element in Parser::new(buff.as_ref())
	{
		let element = match element
		{
			Ok(el) => el,
			Err(err) =>
			{
				print_err_trace("parsing failed", err);
				return false;
			},
		};
		
		match element.value
		{
			ElementValue::Directive{name, args} =>
			{
				temp_str.clear();
				temp_str.push_str(name);
				temp_str.make_ascii_lowercase();
				match temp_str.as_ref()
				{
					"addr" =>
					{
						if args.len() != 1
						{
							eprintln!("addr requires exactly one argument ({}:{})", element.line, element.col);
							return false;
						}
						let tgt = match args[0]
						{
							Argument::Constant(Number::Integer(val)) =>
							{
								let Ok(val) = u32::try_from(val) else
								{
									eprintln!("address out of range ({}:{})", element.line, element.col);
									return false;
								};
								val
							},
							_ =>
							{
								eprintln!("invalid address ({}:{})", element.line, element.col);
								return false;
							},
						};
						if tgt > image_addr
						{
							// FIXME this should not be padding for small offsets (say < 256) or maybe at all and pad in the linker or UF2 encoder
							let len = tgt - image_addr;
							image_addr = tgt;
							output.resize(output.len() + len as usize, 0xBE); // BKPT
						}
						else if image_addr > tgt
						{
							eprintln!("addr cannot seek backwards ({}:{})", element.line, element.col);
							return false;
						}
					},
					"align" =>
					{
						if args.len() != 1
						{
							eprintln!("align requires exactly one argument ({}:{})", element.line, element.col);
							return false;
						}
						let len = match args[0]
						{
							Argument::Constant(Number::Integer(val)) =>
							{
								let Ok(val) = u32::try_from(val) else
								{
									eprintln!("alignment out of range ({}:{})", element.line, element.col);
									return false;
								};
								val
							},
							_ =>
							{
								eprintln!("invalid alignment ({}:{})", element.line, element.col);
								return false;
							},
						};
						if len == 0
						{
							eprintln!("alignment out of range ({}:{})", element.line, element.col);
							return false;
						}
						let off = image_addr % len;
						if off != 0
						{
							image_addr += len - off;
							output.resize(output.len() + (len - off) as usize, 0xBE); // BKPT
						}
					},
					"du8" | "du16" | "du32" =>
					{
						let (min, max) = match name
						{
							"du8" => (u8::MIN as i64, u8::MAX as i64),
							"du16" => (u16::MIN as i64, u16::MAX as i64),
							"du32" => (u32::MIN as i64, u32::MAX as i64),
							_ => unreachable!(),
						};
						if args.len() != 1
						{
							eprintln!("{name} requires exactly one argument ({}:{})", element.line, element.col);
							return false;
						}
						let val = match args[0]
						{
							Argument::Constant(Number::Integer(val)) =>
							{
								if val < min || val > max
								{
									eprintln!("data value out of range ({}:{})", element.line, element.col);
									return false;
								}
								val
							},
							_ =>
							{
								eprintln!("invalid data value ({}:{})", element.line, element.col);
								return false;
							},
						};
						
						match name
						{
							"du8" =>
							{
								image_addr += u32::try_from(core::mem::size_of::<u8>()).unwrap();
								output.push(val as u8);
							},
							"du16" =>
							{
								image_addr += u32::try_from(core::mem::size_of::<u16>()).unwrap();
								output.extend_from_slice(&u16::to_le_bytes(val as u16));
							},
							"du32" =>
							{
								image_addr += u32::try_from(core::mem::size_of::<u32>()).unwrap();
								output.extend_from_slice(&u32::to_le_bytes(val as u32));
							},
							_ => unreachable!(),
						}
					},
					_ =>
					{
						eprintln!("unknown directive {name:?} ({}:{})", element.line, element.col);
						return false;
					},
				}
			},
			ElementValue::Label(name) =>
			{
				let prev = labels.insert(name, image_addr);
				if prev.is_some()
				{
					eprintln!("duplicate label {name} ({}:{})", element.line, element.col);
					return false;
				}
			},
			ElementValue::Instruction{name, args} =>
			{
				enum AddrLabel<'l>
				{
					Address(Register, Option<ImmReg>),
					Label(&'l str),
				}
				
				let mut arg_pos = 0;
				macro_rules!convert
				{
					($($data:ident: $which:ident),* {$($result:tt)+}) =>
					{
						{
							if args.len() - arg_pos > convert!(@impl/count {$($which)*})
							{
								eprintln!("too many arguments for {name} ({}:{})", element.line, element.col);
								return false;
							}
							if args.len() - arg_pos < convert!(@impl/count {$($which)*})
							{
								eprintln!("not enough arguments for {name} ({}:{})", element.line, element.col);
								return false;
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
									eprintln!("argument {}: out of range ({}:{})", arg_pos + 1, element.line, element.col);
									return false;
								};
								arg_pos += 1;
								val
							},
							_ =>
							{
								eprintln!("argument {}: expected immediate ({}:{})", arg_pos + 1, element.line, element.col);
								return false;
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
							_ =>
							{
								eprintln!("argument {}: expected identifier ({}:{})", arg_pos + 1, element.line, element.col);
								return false;
							},
						}
					};
					(@impl/get Register) =>
					{
						match args[arg_pos]
						{
							Argument::Identifier(ident) =>
							{
								let Some(reg) = regl(ident)
								else
								{
									eprintln!("argument {}: no such register ({}:{})", arg_pos + 1, element.line, element.col);
									return false;
								};
								arg_pos += 1;
								reg
							},
							_ =>
							{
								eprintln!("argument {}: expected immediate ({}:{})", arg_pos + 1, element.line, element.col);
								return false;
							},
						}
					};
					(@impl/get SystemReg) =>
					{
						match args[arg_pos]
						{
							Argument::Identifier(ident) =>
							{
								let Some(reg) = sysl(ident)
								else
								{
									eprintln!("argument {}: no such register ({}:{})", arg_pos + 1, element.line, element.col);
									return false;
								};
								arg_pos += 1;
								reg
							},
							_ =>
							{
								eprintln!("argument {}: expected immediate ({}:{})", arg_pos + 1, element.line, element.col);
								return false;
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
									eprintln!("argument {}: out of range ({}:{})", arg_pos + 1, element.line, element.col);
									return false;
								};
								arg_pos += 1;
								ImmReg::Immediate(val)
							},
							Argument::Identifier(ident) =>
							{
								let Some(reg) = regl(ident)
								else
								{
									eprintln!("argument {}: no such register ({}:{})", arg_pos + 1, element.line, element.col);
									return false;
								};
								arg_pos += 1;
								ImmReg::Register(reg)
							},
							_ =>
							{
								eprintln!("argument {}: expected immediate ({}:{})", arg_pos + 1, element.line, element.col);
								return false;
							},
						}
					};
					(@impl/get RegSet) =>
					{
						match args[arg_pos]
						{
							Argument::Sequence(ref items) =>
							{
								let Some(regs) = regset(items)
								else
								{
									eprintln!("argument {}: malformed register set ({}:{})", arg_pos + 1, element.line, element.col);
									return false;
								};
								arg_pos += 1;
								regs
							},
							_ =>
							{
								eprintln!("argument {}: expected register set ({}:{})", arg_pos + 1, element.line, element.col);
								return false;
							},
						}
					};
					(@impl/get Address) =>
					{
						match args[arg_pos]
						{
							Argument::Address(ref addr) =>
							{
								let Some((addr, off)) = addr_off(&*addr, arg_pos + 1, element.line, element.col)
								else
								{
									return false;
								};
								arg_pos += 1;
								(addr, off)
							},
							_ =>
							{
								eprintln!("argument {}: expected address ({}:{})", arg_pos + 1, element.line, element.col);
								return false;
							},
						}
					};
					(@impl/get AddrLabel) =>
					{
						match args[arg_pos]
						{
							Argument::Address(ref addr) =>
							{
								let Some((addr, off)) = addr_off(&*addr, arg_pos + 1, element.line, element.col)
								else
								{
									return false;
								};
								arg_pos += 1;
								AddrLabel::Address(addr, off)
							},
							Argument::Identifier(ident) =>
							{
								arg_pos += 1;
								AddrLabel::Label(ident)
							},
							_ =>
							{
								eprintln!("argument {}: expected address ({}:{})", arg_pos + 1, element.line, element.col);
								return false;
							},
						}
					};
				}
				
				temp_str.clear();
				temp_str.push_str(name);
				temp_str.make_ascii_uppercase(); // REM lowercase for consistency with directives
				let instr = match temp_str.as_ref()
				{
					"ADCS" => convert!(dst: Register, rhs: Register {Instruction::Adc{dst, rhs}}),
					"ADD" | "ADDS" => convert!(dst: Register, lhs: Register, rhs: ImmReg {Instruction::Add{flags: temp_str.len() > 3, dst, lhs, rhs}}),
					"ADR" =>
					{
						let (dst, label) = convert!(dst: Register, label: Identifier {(dst, label)});
						let instr = Instruction::Adr{dst, off: 0};
						deferred.push((element.line, element.col, image_addr, instr, label));
						instr
					},
					"ANDS" => convert!(dst: Register, rhs: Register {Instruction::And{dst, rhs}}),
					"ASRS" => convert!(dst: Register, value: Register, shift: ImmReg {Instruction::Asr{dst, value, shift}}),
					"B" | "BEQ" | "BNE" | "BCS" | "BHS" | "BCC" | "BLO" | "BMI" | "BPL" | "BVS" | "BVC" | "BHI" | "BLS" | "BGE" | "BLT" | "BGT" | "BLE" =>
					{
						let cond = match temp_str.as_ref()
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
							_ => unreachable!("unmapped conditional branch {temp_str}"),
						};
						let label = convert!(label: Identifier {label});
						let instr = Instruction::B{cond, off: 0};
						deferred.push((element.line, element.col, image_addr, instr, label));
						instr
					},
					"BIC" => convert!(dst: Register, rhs: Register {Instruction::And{dst, rhs}}),
					"BKPT" =>
					{
						match convert!(info: Immediate {info})
						{
							info @ 0.. if info <= u8::MAX as i32 => Instruction::Bkpt{info: info as u8},
							_ =>
							{
								eprintln!("argument {arg_pos}: out of range ({}:{})", element.line, element.col);
								return false;
							},
						}
					},
					"BL" =>
					{
						let label = convert!(label: Identifier {label});
						let instr = Instruction::Bl{off: 0};
						deferred.push((element.line, element.col, image_addr, instr, label));
						instr
					},
					"BLX" => convert!(off: Register {Instruction::Blx{off}}),
					"BX" => convert!(off: Register {Instruction::Bx{off}}),
					"CMN" => convert!(lhs: Register, rhs: Register {Instruction::Cmn{lhs, rhs}}),
					"CMP" => convert!(lhs: Register, rhs: ImmReg {Instruction::Cmp{lhs, rhs}}),
					"CPSID" | "CPSIE" =>
					{
						convert!(pm: Identifier
						{
							if pm != "i"
							{
								eprintln!("argument {arg_pos}: \"i\" flag required to modify PRIMASK ({}:{})", element.line, element.col);
								return false;
							}
						});
						Instruction::Cps{enable: temp_str == "CPSIE"}
					},
					"DMB" =>
					{
						convert!(opt: Identifier
						{
							if opt != "SV"
							{
								eprintln!("argument {arg_pos}: no such barrier option ({}:{})", element.line, element.col);
								return false;
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
								eprintln!("argument {arg_pos}: no such barrier option ({}:{})", element.line, element.col);
								return false;
							}
						});
						Instruction::Dsb
					},
					"EORS" => convert!(dst: Register, rhs: Register {Instruction::Eor{dst, rhs}}),
					"ISB" =>
					{
						convert!(opt: Identifier
						{
							if opt != "SV"
							{
								eprintln!("argument {arg_pos}: no such barrier option ({}:{})", element.line, element.col);
								return false;
							}
						});
						Instruction::Isb
					},
					// TODO spec says this must have an '!' when writing back (i.e. addr in registers)
					"LDM" => convert!(addr: Register, registers: RegSet {Instruction::Ldm{addr, registers}}),
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
									deferred.push((element.line, element.col, image_addr, instr, label));
									instr
								},
							}
						})
					},
					"LDRB" | "LDRH" | "LDRSB" | "LDRSH" =>
					{
						convert!(dst: Register, addr: Address
						{
							match temp_str.as_ref()
							{
								"LDRB" => Instruction::Ldrb{dst, addr: addr.0, off: addr.1.unwrap_or(ImmReg::Immediate(0))},
								"LDRH" => Instruction::Ldrh{dst, addr: addr.0, off: addr.1.unwrap_or(ImmReg::Immediate(0))},
								"LDRSB" =>
								{
									match addr.1
									{
										None | Some(ImmReg::Immediate(..)) =>
										{
											eprintln!("argument {arg_pos}: unreachable address ({}:{})", element.line, element.col);
											return false;
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
											eprintln!("argument {arg_pos}: unreachable address ({}:{})", element.line, element.col);
											return false;
										},
										Some(ImmReg::Register(off)) => Instruction::Ldrsh{dst, addr: addr.0, off},
									}
								},
								_ => unreachable!("unmapped load register opcode {temp_str}"),
							}
						})
					},
					"LSLS" => convert!(dst: Register, value: Register, shift: ImmReg {Instruction::Lsl{dst, value, shift}}),
					"LSRS" => convert!(dst: Register, value: Register, shift: ImmReg {Instruction::Lsr{dst, value, shift}}),
					"MOV" | "MOVS" => convert!(dst: Register, src: ImmReg {Instruction::Mov{flags: name.len() > 3, dst, src}}),
					"MRS" => convert!(dst: Register, src: SystemReg {Instruction::Mrs{dst, src}}),
					"MSR" => convert!(dst: SystemReg, src: Register {Instruction::Msr{dst, src}}),
					"MULS" => convert!(dst: Register, rhs: Register {Instruction::Mul{dst, rhs}}),
					"MVNS" => convert!(dst: Register, value: Register {Instruction::Mvn{dst, value}}),
					"NOP" => convert!({Instruction::Nop}),
					"ORRS" => convert!(dst: Register, rhs: Register {Instruction::Orr{dst, rhs}}),
					"POP" => convert!(registers: RegSet {Instruction::Pop{registers}}),
					"PUSH" => convert!(registers: RegSet {Instruction::Push{registers}}),
					"REV" => convert!(dst: Register, value: Register {Instruction::Rev{dst, value}}),
					"REV16" => convert!(dst: Register, value: Register {Instruction::Rev16{dst, value}}),
					"REVSH" => convert!(dst: Register, value: Register {Instruction::Revsh{dst, value}}),
					"RORS" => convert!(dst: Register, rhs: Register {Instruction::Ror{dst, rhs}}),
					"RSBS" =>
					{
						convert!(dst: Register, lhs: Register, rhs: Immediate
						{
							if rhs != 0
							{
								eprintln!("argument {arg_pos}: unsupported operand, must be zero ({}:{})", element.line, element.col);
								return false;
							}
							Instruction::Rsb{dst, lhs}
						})
					},
					"SBCS" => convert!(dst: Register, rhs: Register {Instruction::Sbc{dst, rhs}}),
					"SEV" => convert!({Instruction::Sev}),
					"STM" => convert!(addr: Register, registers: RegSet {Instruction::Stm{addr, registers}}),
					"STR" | "STRB" | "STRH" =>
					{
						convert!(src: Register, addr: Address
						{
							match temp_str.as_ref()
							{
								"STR" => Instruction::Str{src, addr: addr.0, off: addr.1.unwrap_or(ImmReg::Immediate(0))},
								"STRB" => Instruction::Strb{src, addr: addr.0, off: addr.1.unwrap_or(ImmReg::Immediate(0))},
								"STRH" => Instruction::Strh{src, addr: addr.0, off: addr.1.unwrap_or(ImmReg::Immediate(0))},
								_ => unreachable!("unmapped store register opcode {temp_str}"),
							}
						})
					},
					"SUBS" => convert!(dst: Register, lhs: Register, rhs: ImmReg {Instruction::Sub{dst, lhs, rhs}}),
					"SVC" =>
					{
						match convert!(info: Immediate {info})
						{
							info @ 0.. if info <= u8::MAX as i32 => Instruction::Svc{info: info as u8},
							_ =>
							{
								eprintln!("argument {arg_pos}: out of range ({}:{})", element.line, element.col);
								return false;
							},
						}
					},
					"SXTB" => convert!(dst: Register, value: Register {Instruction::Sxtb{dst, value}}),
					"SXTH" => convert!(dst: Register, value: Register {Instruction::Sxth{dst, value}}),
					"TST" => convert!(lhs: Register, rhs: Register {Instruction::Tst{lhs, rhs}}),
					"UDF" | "UDF.N" | "UDF.W" =>
					{
						match convert!(info: Immediate {info})
						{
							info @ 0.. if info <= u8::MAX as i32 && name.as_bytes()[name.len() - 1] != b'W' => Instruction::Udf{info: info as u8},
							info @ 0.. if info <= u16::MAX as i32 && name.as_bytes()[name.len() - 1] != b'N' => Instruction::Udfw{info: info as u16},
							_ =>
							{
								eprintln!("argument {arg_pos}: out of range ({}:{})", element.line, element.col);
								return false;
							},
						}
					},
					"UXTB" => convert!(dst: Register, value: Register {Instruction::Uxtb{dst, value}}),
					"UXTH" => convert!(dst: Register, value: Register {Instruction::Uxth{dst, value}}),
					"WFE" => convert!({Instruction::Wfe}),
					"WFI" => convert!({Instruction::Wfi}),
					"YIELD" => convert!({Instruction::Yield}),
					_ =>
					{
						eprintln!("no such instruction {temp_str}");
						return false;
					},
				};
				let mut tmp = [0u8; 4];
				match instr.write(&mut tmp)
				{
					Ok(len) =>
					{
						image_addr += len as u32;
						output.extend_from_slice(&tmp[..len]);
					},
					Err(e) =>
					{
						print_err_trace("encoding failed", e);
						return false;
					},
				}
			},
		}
	}
	
	for (line, col, addr, mut instr, label) in deferred
	{
		let tgt = match labels.get(label)
		{
			None =>
			{
				eprintln!("No such label {label:?} ({line}:{col})");
				return false;
			},
			Some(&t) => t,
		};
		
		match &mut instr
		{
			Instruction::Adr{off, ..} =>
			{
				let al_pc = (addr & !0b11).wrapping_add(4);
				if tgt < al_pc || tgt - al_pc > u16::MAX as u32
				{
					eprintln!("Label out of range ({line}:{col})");
					return false;
				}
				if ((tgt - al_pc) & 0b11) != 0
				{
					eprintln!("Misaligned label ({line}:{col})");
					return false;
				}
				*off = (tgt - al_pc) as u16;
			},
			Instruction::B{off, ..} | Instruction::Bl{off} =>
			{
				let pc = addr.wrapping_add(4);
				if let Ok(diff) = i32::try_from(i64::from(tgt) - i64::from(pc))
				{
					*off = diff;
				}
				else
				{
					eprintln!("Label out of range ({line}:{col})");
					return false;
				}
			},
			Instruction::Ldr{addr: Register::PC, off, ..} =>
			{
				let al_pc = (addr & !0b11).wrapping_add(4);
				if tgt < al_pc || tgt - al_pc > i32::MAX as u32
				{
					eprintln!("Label out of range ({line}:{col})");
					return false;
				}
				if ((tgt - al_pc) & 0b11) != 0
				{
					eprintln!("Misaligned label ({line}:{col})");
					return false;
				}
				*off = ImmReg::Immediate((tgt - al_pc) as i32);
			},
			_ => unreachable!("missing deferred handler for {instr:?}"),
		}
		
		let mut tmp = [0u8; 4];
		match instr.write(&mut tmp)
		{
			Ok(len) =>
			{
				let start = (addr - BASE) as usize;
				output[start..start + len].copy_from_slice(&tmp[..len]);
			},
			Err(e) =>
			{
				print_err_trace("late encoding failed", e);
				return false;
			},
		}
	}
	
	if output.len() > 0
	{
		if (output.len() & 0xFF) != 0
		{
			output.resize(output.len() + (0x100 - (output.len() & 0xFF)), 0);
		}
		let mut crc = Crc::new();
		crc.update_slice(&output[..0xFC]);
		output[0xFC..0x100].copy_from_slice(&u32::to_le_bytes(crc.get_value()));
		
		buff.clear();
		encode(output.as_ref(), buff, BASE, Some(0xE48BFF56)).unwrap();
		true
	}
	else {false}
}

fn regl(name: &str) -> Option<Register>
{
	if name.len() <= 4
	{
		let mut temp = [0u8; 4];
		temp[..name.len()].copy_from_slice(name.as_bytes());
		temp.make_ascii_uppercase();
		match &temp[..name.len()]
		{
			b"R0" => Some(Register::R0),
			b"R1" => Some(Register::R1),
			b"R2" => Some(Register::R2),
			b"R3" => Some(Register::R3),
			b"R4" => Some(Register::R4),
			b"R5" => Some(Register::R5),
			b"R6" => Some(Register::R6),
			b"R7" => Some(Register::R7),
			b"R8" => Some(Register::R8),
			b"R9" => Some(Register::R9),
			b"R10" => Some(Register::R10),
			b"R11" => Some(Register::R11),
			b"R12" => Some(Register::R12),
			b"R13" => Some(Register::SP),
			b"SP" => Some(Register::SP),
			b"R14" => Some(Register::LR),
			b"LR" => Some(Register::LR),
			b"R15" => Some(Register::PC),
			b"PC" => Some(Register::PC),
			_ => None,
		}
	}
	else {None}
}

fn sysl(name: &str) -> Option<SystemReg>
{
	if name.len() <= 8
	{
		let mut temp = [0u8; 8];
		temp[..name.len()].copy_from_slice(name.as_bytes());
		temp.make_ascii_uppercase();
		match &temp[..name.len()]
		{
			b"APSR" => Some(SystemReg::APSR),
			b"IAPSR" => Some(SystemReg::IAPSR),
			b"EAPSR" => Some(SystemReg::EAPSR),
			b"XPSR" => Some(SystemReg::XPSR),
			b"IPSR" => Some(SystemReg::IPSR),
			b"EPSR" => Some(SystemReg::EPSR),
			b"IEPSR" => Some(SystemReg::IEPSR),
			b"MSP" => Some(SystemReg::MSP),
			b"PSP" => Some(SystemReg::PSP),
			b"PRIMASK" => Some(SystemReg::PRIMASK),
			b"CONTROL" => Some(SystemReg::CONTROL),
			_ => None,
		}
	}
	else {None}
}

fn regset<'l>(items: &Vec<Argument<'l>>) -> Option<RegisterSet>
{
	let mut regs = RegisterSet::new();
	for arg in items
	{
		match arg
		{
			Argument::Identifier(ident) =>
			{
				match regl(ident)
				{
					None => return None,
					Some(reg) => regs.add(reg),
				};
			},
			_ => return None,
		}
	}
	Some(regs)
}

fn addr_off<'l>(addr: &Argument<'l>, arg_pos: usize, line: u32, col: u32) -> Option<(Register, Option<ImmReg>)>
{
	match addr
	{
		&Argument::Identifier(name) =>
		{
			let Some(addr) = regl(name)
			else
			{
				eprintln!("argument {arg_pos}: no such register ({line}:{col})");
				return None;
			};
			Some((addr, Some(ImmReg::Immediate(0))))
		},
		Argument::Add(parts) =>
		{
			if parts.len() != 2
			{
				eprintln!("argument {arg_pos}: unreachable address ({line}:{col})");
				return None;
			}
			match (&parts[0], &parts[1])
			{
				(&Argument::Identifier(addr), &Argument::Identifier(off)) =>
				{
					let Some(addr) = regl(addr)
					else
					{
						eprintln!("argument {arg_pos}: unreachable address ({line}:{col})");
						return None;
					};
					let Some(off) = regl(off)
					else
					{
						eprintln!("argument {arg_pos}: unreachable address ({line}:{col})");
						return None;
					};
					Some((addr, Some(ImmReg::Register(off))))
				},
				(&Argument::Identifier(addr), &Argument::Constant(Number::Integer(off))) |
					(&Argument::Constant(Number::Integer(off)), &Argument::Identifier(addr)) =>
				{
					let Some(addr) = regl(addr)
					else
					{
						eprintln!("argument {arg_pos}: unreachable address ({line}:{col})");
						return None;
					};
					let Ok(off) = i32::try_from(off)
					else
					{
						eprintln!("argument {arg_pos}: unreachable address ({line}:{col})");
						return None;
					};
					Some((addr, Some(ImmReg::Immediate(off))))
				},
				_ =>
				{
					eprintln!("argument {arg_pos}: unreachable address ({line}:{col})");
					return None;
				},
			}
		},
		_ =>
		{
			eprintln!("argument {arg_pos}: unreachable address ({line}:{col})");
			return None;
		},
	}
}
