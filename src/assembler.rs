use std::fs::OpenOptions;
use std::io::Read;
use std::path::Path;

use crate::arm6m::asm::{ImmReg, Instruction};
use crate::arm6m::cond::Condition;
use crate::arm6m::reg::Register;
use crate::arm6m::regset::RegisterSet;
use crate::arm6m::sysreg::SystemReg;
use crate::asm::{Context, LabelError};
use crate::asm::mem::MemoryRange;
use crate::asm::mem::map::Search;
use crate::text::parse::{Argument, ElementValue, Parser};
use crate::text::token::Number;
use crate::uf2::write::Uf2Write;
use crate::uf2::crc::Crc;

macro_rules!print_err
{
	($err:ident, $($print:expr),+) =>
	{
		{
			use std::error::Error;
			use std::io::Write;
			let mut stderr = std::io::stderr().lock();
			write!(stderr, $($print),+).unwrap();
			write!(stderr, ": {}\n", $err).unwrap();
			let mut source = $err.source();
			while let Some(src) = source
			{
				write!(stderr, "\tsource: {src}\n").unwrap();
				source = src.source();
			}
		}
	};
}

macro_rules!space_check
{
	($ctx:ident, $active:ident, $need:expr, $line:expr, $col:expr) =>
	{
		if !$active.has_remaining($need)
		{
			eprintln!("Segment overflow at {:08X} ({}:{})", $active.curr_addr(), $line, $col);
			return false;
		}
	};
}

macro_rules!get_active
{
	(let $dst:ident = $ctx:ident; print($($print:expr),+) @ ($line:expr, $col:expr)) =>
	{
		// stderr simply uses unwrap because failing to print to stderr means whatever message won't go through
		let $dst = match $ctx.active_mut()
		{
			None =>
			{
				use std::io::Write;
				let mut stderr = std::io::stderr().lock();
				write!(stderr, "Image address unset for ").unwrap();
				write!(stderr, $($print),+).unwrap();
				write!(stderr, " ({}:{})\n", $line, $col).unwrap();
				return false;
			},
			Some(v) => v,
		};
	};
}

pub fn assemble(buff: &mut Vec<u8>, path: &Path) -> bool
{
	let base = path.parent().unwrap();
	let mut ctx = Context::new();
	let mut temp_str = String::new();
	
	for element in Parser::new(buff.as_ref())
	{
		let element = match element
		{
			Ok(el) => el,
			Err(err) =>
			{
				print_err!(err, "Parsing failed");
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
							eprintln!("{name:?} requires exactly one argument ({}:{})", element.line, element.col);
							return false;
						}
						let tgt = match args[0]
						{
							Argument::Constant(Number::Integer(val)) =>
							{
								let Ok(val) = u32::try_from(val) else
								{
									eprintln!("Address out of range ({}:{})", element.line, element.col);
									return false;
								};
								val
							},
							_ =>
							{
								eprintln!("Invalid address ({}:{})", element.line, element.col);
								return false;
							},
						};
						if let Err(e) = ctx.change_segment(tgt)
						{
							print_err!(e, "Could change address to {tgt:08X} ({}:{})", element.line, element.col);
							return false;
						}
					},
					"align" =>
					{
						get_active!(let active = ctx; print(".align") @ (element.line, element.col));
						if args.len() != 1
						{
							eprintln!("Align requires exactly one argument ({}:{})", element.line, element.col);
							return false;
						}
						let len = match args[0]
						{
							Argument::Constant(Number::Integer(val)) =>
							{
								let Ok(val) = u32::try_from(val) else
								{
									eprintln!("Alignment out of range ({}:{})", element.line, element.col);
									return false;
								};
								val
							},
							_ =>
							{
								eprintln!("Invalid alignment ({}:{})", element.line, element.col);
								return false;
							},
						};
						if len == 0
						{
							eprintln!("Alignment out of range ({}:{})", element.line, element.col);
							return false;
						}
						let off = active.curr_addr() % len;
						if off != 0
						{
							match usize::try_from(len - off)
							{
								Ok(new_len) =>
								{
									const PADDING: [u8; 256] = [0xBE; 256];
									
									space_check!(ctx, active, new_len, element.line, element.col);
									for p in (0..new_len).step_by(256)
									{
										if new_len - p >= 256
										{
											active.write(&PADDING).unwrap();
										}
										else
										{
											active.write(&PADDING[..new_len - p]).unwrap();
										}
									}
								},
								Err(..) =>
								{
									eprintln!("Buffer overflow during .align ({}:{})", element.line, element.col);
									return false;
								},
							}
						}
					},
					"du8" | "du16" | "du32" =>
					{
						get_active!(let active = ctx; print(".{name}") @ (element.line, element.col));
						let (min, max) = match name
						{
							"du8" => (u8::MIN as i64, u8::MAX as i64),
							"du16" => (u16::MIN as i64, u16::MAX as i64),
							"du32" => (u32::MIN as i64, u32::MAX as i64),
							_ => unreachable!(),
						};
						if args.len() != 1
						{
							eprintln!("{name:?} requires exactly one argument ({}:{})", element.line, element.col);
							return false;
						}
						let val = match args[0]
						{
							Argument::Constant(Number::Integer(val)) =>
							{
								if val < min || val > max
								{
									eprintln!("Data value out of range ({}:{})", element.line, element.col);
									return false;
								}
								val
							},
							_ =>
							{
								eprintln!("Invalid data value ({}:{})", element.line, element.col);
								return false;
							},
						};
						
						let result = match name
						{
							"du8" => active.write(core::slice::from_ref(&(val as u8))),
							"du16" => active.write(&u16::to_le_bytes(val as u16)),
							"du32" => active.write(&u32::to_le_bytes(val as u32)),
							_ => unreachable!(),
						};
						if let Err(e) = result
						{
							print_err!(e, "Could not write constant ({}, {})", element.line, element.col);
							return false;
						}
					},
					"dhex" =>
					{
						get_active!(let active = ctx; print(".dhex") @ (element.line, element.col));
						if args.len() != 1
						{
							eprintln!("{name:?} requires exactly one argument ({}:{})", element.line, element.col);
							return false;
						}
						let val = match args[0]
						{
							Argument::String(ref in_hex) =>
							{
								let mut out_data = Vec::new();
								let mut carry: Option<u8> = None;
								for c in in_hex.as_ref().chars().filter(|&c| !c.is_ascii_whitespace())
								{
									match c.to_digit(16)
									{
										None =>
										{
											eprintln!("malformed hex string ({}:{})", element.line, element.col);
											eprintln!("\tin {in_hex:?} {carry:?} {c:?}");
											return false;
										},
										Some(v) =>
										{
											let v = v as u8;
											match carry
											{
												None => carry = Some(v << 4),
												Some(c) =>
												{
													out_data.push(v | c);
													carry = None;
												},
											}
										},
									}
								}
								if carry.is_some()
								{
									eprintln!("Malformed hex string ({}:{})", element.line, element.col);
									return false;
								}
								out_data
							},
							_ =>
							{
								eprintln!("Invalid data value ({}:{})", element.line, element.col);
								return false;
							},
						};
						if let Err(e) = active.write(val.as_slice())
						{
							print_err!(e, "Could not write hex blob ({}:{})", element.line, element.col);
							return false;
						}
					},
					"dstr" =>
					{
						get_active!(let active = ctx; print(".dstr") @ (element.line, element.col));
						if args.len() != 1
						{
							eprintln!("{name:?} requires exactly one argument ({}:{})", element.line, element.col);
							return false;
						}
						let val = match args[0]
						{
							Argument::String(ref val) => val.as_ref().as_bytes(),
							_ =>
							{
								eprintln!("Invalid data value ({}:{})", element.line, element.col);
								return false;
							},
						};
						if let Err(e) = active.write(val)
						{
							print_err!(e, "Could not write string ({}:{})", element.line, element.col);
							return false;
						}
					},
					"dfile" =>
					{
						get_active!(let active = ctx; print(".dfile") @ (element.line, element.col));
						if args.len() != 1
						{
							eprintln!("{name:?} requires exactly one argument ({}:{})", element.line, element.col);
							return false;
						}
						let path = match args[0]
						{
							Argument::String(ref s) => s.as_ref() as &str,
							_ =>
							{
								eprintln!("Invalid path value ({}:{})", element.line, element.col);
								return false;
							},
						};
						let mut path_buff = base.to_path_buf();
						path_buff.push(path);
						match OpenOptions::new().read(true).open(path_buff)
						{
							Ok(mut f) =>
							{
								let len = match f.metadata()
								{
									Ok(meta) =>
									{
										match usize::try_from(meta.len())
										{
											Ok(len) => len,
											Err(..) =>
											{
												eprintln!("File too long ({}:{})", element.line, element.col);
												return false;
											},
										}
									},
									Err(e) =>
									{
										print_err!(e, "Cannot access metadata of {path:?} ({}:{})", element.line, element.col);
										return false;
									},
								};
								space_check!(ctx, active, len, element.line, element.col);
								
								let mut pos = 0;
								let mut temp = [0u8; 1024];
								while pos < len
								{
									match f.read(&mut temp)
									{
										Ok(cnt) =>
										{
											if len - pos < cnt
											{
												if let Err(e) = active.write(&temp[..len - pos])
												{
													print_err!(e, "Could not write file data at {pos} ({}:{})", element.line, element.col);
													return false;
												}
												// no need to update pos again
												//pos = len;
												break;
											}
											else
											{
												if let Err(e) = active.write(&temp[..cnt])
												{
													print_err!(e, "Could not write file data at {pos} ({}:{})", element.line, element.col);
													return false;
												}
												pos += cnt;
											}
										},
										Err(e) =>
										{
											print_err!(e, "Could not read file {path:?} ({}:{})", element.line, element.col);
											return false;
										},
									}
								}
								// reading less than expected is fine
								drop(f);
							},
							Err(e) =>
							{
								print_err!(e, "Could not open file {path:?} ({}:{})", element.line, element.col);
								return false;
							},
						}
					},
					_ =>
					{
						eprintln!("Unknown directive {name:?} ({}:{})", element.line, element.col);
						return false;
					},
				}
			},
			ElementValue::Label(name) =>
			{
				get_active!(let active = ctx; print("label") @ (element.line, element.col));
				let curr_addr = active.curr_addr();
				let prev = ctx.labels_mut().insert(name.to_owned(), curr_addr);
				if prev.is_some()
				{
					eprintln!("Duplicate label {name} ({}:{})", element.line, element.col);
					return false;
				}
			},
			ElementValue::Instruction{name, args} =>
			{
				get_active!(let active = ctx; print("instruction") @ (element.line, element.col));
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
								eprintln!("Too many arguments for {name} ({}:{})", element.line, element.col);
								return false;
							}
							if args.len() - arg_pos < convert!(@impl/count {$($which)*})
							{
								eprintln!("Not enough arguments for {name} ({}:{})", element.line, element.col);
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
									eprintln!("Argument {}: out of range ({}:{})", arg_pos + 1, element.line, element.col);
									return false;
								};
								arg_pos += 1;
								val
							},
							_ =>
							{
								eprintln!("Argument {}: expected immediate ({}:{})", arg_pos + 1, element.line, element.col);
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
								eprintln!("Argument {}: expected identifier ({}:{})", arg_pos + 1, element.line, element.col);
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
									eprintln!("Argument {}: no such register ({}:{})", arg_pos + 1, element.line, element.col);
									return false;
								};
								arg_pos += 1;
								reg
							},
							_ =>
							{
								eprintln!("Argument {}: expected register ({}:{})", arg_pos + 1, element.line, element.col);
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
									eprintln!("Argument {}: no such register ({}:{})", arg_pos + 1, element.line, element.col);
									return false;
								};
								arg_pos += 1;
								reg
							},
							_ =>
							{
								eprintln!("Argument {}: expected register ({}:{})", arg_pos + 1, element.line, element.col);
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
									eprintln!("Argument {}: out of range ({}:{})", arg_pos + 1, element.line, element.col);
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
									eprintln!("Argument {}: no such register ({}:{})", arg_pos + 1, element.line, element.col);
									return false;
								};
								arg_pos += 1;
								ImmReg::Register(reg)
							},
							_ =>
							{
								eprintln!("Argument {}: expected immediate or register ({}:{})", arg_pos + 1, element.line, element.col);
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
									eprintln!("Argument {}: malformed register set ({}:{})", arg_pos + 1, element.line, element.col);
									return false;
								};
								arg_pos += 1;
								regs
							},
							_ =>
							{
								eprintln!("Argument {}: expected register set ({}:{})", arg_pos + 1, element.line, element.col);
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
								eprintln!("Argument {}: expected address ({}:{})", arg_pos + 1, element.line, element.col);
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
								eprintln!("Argument {}: expected address or label ({}:{})", arg_pos + 1, element.line, element.col);
								return false;
							},
						}
					};
				}
				
				temp_str.clear();
				temp_str.push_str(name);
				temp_str.make_ascii_uppercase(); // REM lowercase for consistency with directives
				let mut defer_label = None;
				let instr = match temp_str.as_ref()
				{
					"ADCS" => convert!(dst: Register, rhs: Register {Instruction::Adc{dst, rhs}}),
					"ADD" | "ADDS" => convert!(dst: Register, lhs: Register, rhs: ImmReg {Instruction::Add{flags: temp_str.len() > 3, dst, lhs, rhs}}),
					"ADR" =>
					{
						let (dst, label) = convert!(dst: Register, label: Identifier {(dst, label)});
						let instr = Instruction::Adr{dst, off: 0};
						defer_label = Some(label);
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
						defer_label = Some(label);
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
								eprintln!("Argument {arg_pos}: out of range ({}:{})", element.line, element.col);
								return false;
							},
						}
					},
					"BL" =>
					{
						let label = convert!(label: Identifier {label});
						let instr = Instruction::Bl{off: 0};
						defer_label = Some(label);
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
								eprintln!("Argument {arg_pos}: \"i\" flag required to modify PRIMASK ({}:{})", element.line, element.col);
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
								eprintln!("Argument {arg_pos}: no such barrier option ({}:{})", element.line, element.col);
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
								eprintln!("Argument {arg_pos}: no such barrier option ({}:{})", element.line, element.col);
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
								eprintln!("Argument {arg_pos}: no such barrier option ({}:{})", element.line, element.col);
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
											eprintln!("Argument {arg_pos}: unreachable address ({}:{})", element.line, element.col);
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
											eprintln!("Argument {arg_pos}: unreachable address ({}:{})", element.line, element.col);
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
								eprintln!("Argument {arg_pos}: unsupported operand, must be zero ({}:{})", element.line, element.col);
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
					"SUB" | "SUBS" => convert!(dst: Register, lhs: Register, rhs: ImmReg{Instruction::Sub{flags: temp_str.len() > 3, dst, lhs, rhs}}),
					"SVC" =>
					{
						match convert!(info: Immediate {info})
						{
							info @ 0.. if info <= u8::MAX as i32 => Instruction::Svc{info: info as u8},
							_ =>
							{
								eprintln!("Argument {arg_pos}: out of range ({}:{})", element.line, element.col);
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
								eprintln!("Argument {arg_pos}: out of range ({}:{})", element.line, element.col);
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
						eprintln!("No such instruction {temp_str} ({}:{})", element.line, element.col);
						return false;
					},
				};
				let mut tmp = [0u8; 4];
				let curr_addr = active.curr_addr();
				match instr.write(&mut tmp)
				{
					Ok(len) =>
					{
						if let Err(e) = active.write(&tmp[..len])
						{
							print_err!(e, "Instruction write failed ({}:{})", element.line, element.col);
							return false;
						}
					},
					Err(e) =>
					{
						print_err!(e, "Instruction encoding failed ({}:{})", element.line, element.col);
						return false;
					},
				}
				if let Some(label) = defer_label
				{
					let (line, col) = (element.line, element.col);
					let label = label.to_owned();
					ctx.add_task(Box::new(move |ctx| deferred_instr(ctx, line, col, curr_addr, instr, label.as_str())));
				}
			},
		}
	}
	if let Err(e) = ctx.close_segment()
	{
		print_err!(e, "Could not close final segment");
		return false;
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
				let (min, max) = if cond == Condition::Always {(-0b10000000000_0, 0b01111111111_0)} else {(-0b10000000_0, 0b01111111_0)};
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
					print_err!(e, "Late instruction write failed ({}:{})", line, col);
					ctx.push_error(e);
				}
			},
			Err(e) =>
			{
				print_err!(e, "Late instruction encoding failed ({}:{})", line, col);
				ctx.push_error(e);
			},
		}
	}
	
	ctx.run_tasks();
	if ctx.has_errored() {return false;}
	if ctx.output().len() > 0
	{
		const FLASH_BASE: u32 = 0x10000000;
		const FLASH_CRC: u32 = FLASH_BASE + 0xFC;
		// use `Search::Exact` because if code doesn't start right here it's not bootable
		if ctx.output().find(FLASH_BASE, Search::Exact).is_some()
		{
			let mut temp = [0u8; 0xFC];
			// also search in the space the CRC goes to find existing data we can't overwrite
			for (range, data) in ctx.output().iter_range(MemoryRange::new(FLASH_BASE, FLASH_BASE + 0xFF))
			{
				if range.get_last() >= FLASH_CRC
				{
					eprintln!("Checksum would overwrite existing data");
					return false;
				}
				let first = (range.get_first() - FLASH_BASE) as usize;
				let last = (range.get_last() - FLASH_BASE) as usize;
				temp[first..=last].copy_from_slice(data);
			}
			let mut crc = Crc::new();
			crc.update_slice(&temp[..0xFC]);
			if let Err(e) = ctx.output_mut().put(FLASH_CRC, &u32::to_le_bytes(crc.get_value()))
			{
				print_err!(e, "Checksum write failed");
				return false;
			}
		}
		
		// pad all blocks to start on multiples of block size
		const PAGE_SIZE: usize = 256;
		if let Some(first) = ctx.output().find(0, Search::Above)
		{
			const BLANK_PAGE: [u8; PAGE_SIZE] = [0x00; PAGE_SIZE];
			if first.get_first() % (PAGE_SIZE as u32) > 0
			{
				let off = first.get_first() % (PAGE_SIZE as u32);
				assert_eq!(ctx.output_mut().put(first.get_first() - off, &BLANK_PAGE[..off as usize]), Ok(off as usize));
			}
			let mut prev = first.get_last();
			while prev < u32::MAX
			{
				if let Some(range) = ctx.output().find(prev + 1, Search::Above)
				{
					let off = range.get_first() % (PAGE_SIZE as u32);
					if off > 0
					{
						let base = range.get_first() - off;
						if prev >= base
						{
							let cnt = (range.get_first() - prev - 1) as usize;
							assert_eq!(ctx.output_mut().put(prev + 1, &BLANK_PAGE[..cnt as usize]), Ok(cnt));
						}
						else
						{
							assert_eq!(ctx.output_mut().put(base, &BLANK_PAGE[..off as usize]), Ok(off as usize));
						}
					}
					prev = range.get_last();
				}
				else {break;}
			}
		}
		
		buff.clear();
		let mut dst = Uf2Write::new_vec(Some(0xE48BFF56), PAGE_SIZE, PAGE_SIZE, buff).unwrap();
		for (range, seg) in ctx.output().iter()
		{
			match dst.write_all(range.get_first(), seg, false)
			{
				Ok(..) => (),
				Err(e) =>
				{
					print_err!(e, "UF2 write failed");
					return false;
				},
			};
		}
		drop(dst);
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
				eprintln!("Argument {arg_pos}: no such register ({line}:{col})");
				return None;
			};
			Some((addr, Some(ImmReg::Immediate(0))))
		},
		Argument::Add(parts) =>
		{
			if parts.len() != 2
			{
				eprintln!("Argument {arg_pos}: unreachable address ({line}:{col})");
				return None;
			}
			match (&parts[0], &parts[1])
			{
				(&Argument::Identifier(addr), &Argument::Identifier(off)) =>
				{
					let Some(addr) = regl(addr)
					else
					{
						eprintln!("Argument {arg_pos}: unreachable address ({line}:{col})");
						return None;
					};
					let Some(off) = regl(off)
					else
					{
						eprintln!("Argument {arg_pos}: unreachable address ({line}:{col})");
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
						eprintln!("Argument {arg_pos}: unreachable address ({line}:{col})");
						return None;
					};
					let Ok(off) = i32::try_from(off)
					else
					{
						eprintln!("Argument {arg_pos}: unreachable address ({line}:{col})");
						return None;
					};
					Some((addr, Some(ImmReg::Immediate(off))))
				},
				_ =>
				{
					eprintln!("Argument {arg_pos}: unreachable address ({line}:{col})");
					return None;
				},
			}
		},
		_ =>
		{
			eprintln!("Argument {arg_pos}: unreachable address ({line}:{col})");
			return None;
		},
	}
}
