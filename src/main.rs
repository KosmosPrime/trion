use std::collections::HashMap;
use std::fs::OpenOptions;
use std::io::{Read, Write};
use std::path::Path;

use crate::arm6m::asm::{Instruction, ImmReg};
use crate::arm6m::cond::Condition;
use crate::arm6m::reg::Register;

pub mod arm6m;
pub mod asm;
mod assembler;
mod disassembler;
pub(crate) mod macros;
pub mod text;
pub mod uf2;

pub fn main()
{
	let mut args = std::env::args();
	assert!(args.next().is_some());
	match args.next()
	{
		None => eprintln!("missing operation argument"),
		Some(op) => match op.as_ref()
		{
			"assemble" =>
			{
				let Some(fip) = args.next()
				else
				{
					eprintln!("missing assembly file argument");
					return;
				};
				let out_arg = args.next();
				
				let path = Path::new(&fip);
				let mut buff = Vec::new();
				let mut fi = OpenOptions::new().read(true).open(path).unwrap();
				fi.read_to_end(&mut buff).unwrap();
				drop(fi);
				if assembler::assemble(&mut buff, path)
				{
					if let Some(fop) = out_arg
					{
						let mut fo = OpenOptions::new().write(true).create(true).open(fop).unwrap();
						fo.write_all(&buff.as_ref()).unwrap();
						fo.set_len(u64::try_from(buff.len()).unwrap()).unwrap();
						drop(fo);
					}
					else {println!("Assembled successfully");}
				}
			},
			"disassemble" =>
			{
				let Some(fp) = args.next()
				else
				{
					eprintln!("missing input file argument");
					return;
				};
				
				let mut buff = Vec::new();
				let mut f = OpenOptions::new().read(true).open(fp).unwrap();
				f.read_to_end(&mut buff).unwrap();
				drop(f);
				disassembler::disassemble(buff.as_ref(), 0x20000000);
			},
			op => eprintln!("unsupported operation {op:?}"),
		},
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PreInstr<'l>
{
	instr: Instruction,
	label: Option<&'l str>,
}

impl<'l> PreInstr<'l>
{
	pub fn get_instruction(&self) -> &Instruction
	{
		&self.instr
	}
	
	pub fn needs_conversion(&self) -> bool
	{
		match self.instr
		{
			Instruction::Adr{..} => true,
			Instruction::B{..} => true,
			Instruction::Bl{..} => true,
			Instruction::Ldr{addr: Register::PC, ..} => true,
			_ => false,
		}
	}
	
	pub fn convert(self, base: u32, labels: &HashMap<String, u32>) -> Option<Instruction>
	{
		match self.instr
		{
			Instruction::Adr{dst, ..} =>
			{
				if let Some(&tgt) = labels.get(self.label.unwrap())
				{
					let base = (base & !0b11).wrapping_add(4);
					if tgt >= base && tgt - base <= 0b11111111_00 && ((tgt - base) & 0b11) == 0
					{
						Some(Instruction::Adr{dst, off: (tgt - base) as u16})
					}
					else {None}
				}
				else {None}
			},
			Instruction::B{cond, ..} =>
			{
				let (p_max, n_max) = if cond == Condition::Always {(0b01111111111_0, 0b10000000000_0)} else {(0b01111111_0, 0b10000000_0)};
				if let Some(&tgt) = labels.get(self.label.unwrap())
				{
					let base = base.wrapping_add(4);
					if tgt >= base && tgt - base <= p_max && ((tgt - base) & 0b1) == 0
					{
						Some(Instruction::B{cond, off: (tgt - base) as i32})
					}
					else if base >= tgt && tgt - base <= n_max && ((tgt - base) & 0b1) == 0
					{
						Some(Instruction::B{cond, off: -((base - tgt) as i32)})
					}
					else {None}
				}
				else {None}
			},
			Instruction::Bl{..} =>
			{
				if let Some(&tgt) = labels.get(self.label.unwrap())
				{
					let base = base.wrapping_add(4);
					if tgt >= base && tgt - base <= 0b0_1_1_1111111111_11111111111_0 && ((tgt - base) & 0b1) == 0
					{
						Some(Instruction::Bl{off: (tgt - base) as i32})
					}
					else if base >= tgt && tgt - base <= 0b1_0_0_0000000000_00000000000_0 && ((tgt - base) & 0b1) == 0
					{
						Some(Instruction::Bl{off: -((base - tgt) as i32)})
					}
					else {None}
				}
				else {None}
			},
			Instruction::Ldr{dst, addr: Register::PC, ..} =>
			{
				if let Some(&tgt) = labels.get(self.label.unwrap())
				{
					let base = (base & !0b11).wrapping_add(4);
					if tgt >= base && tgt - base <= 0b11111111_00 && ((tgt - base) & 0b11) == 0
					{
						Some(Instruction::Ldr{dst, addr: Register::PC, off: ImmReg::Immediate((tgt - base) as i32)})
					}
					else {None}
				}
				else {None}
			},
			_ => None,
		}
	}
}
