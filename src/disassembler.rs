use std::collections::{BTreeMap, BTreeSet};

use crate::arm6m::asm::Instruction;

pub fn disassemble(input: &[u8], base: u32)
{
	let mut queries = BTreeSet::new();
	let mut instrs = BTreeMap::new();
	let mut branches = BTreeSet::new();
	queries.insert(base);
	while let Some(&start) = queries.iter().next()
	{
		queries.remove(&start);
		if start >= base && start - base < input.len() as u32
		{
			let mut pos = (start - base) as usize;
			while pos < input.len()
			{
				let result = Instruction::decode(&input[pos..]).unwrap();
				let addr = base + pos as u32;
				queries.remove(&addr);
				if let Some(dst) = result.1.get_branch(addr)
				{
					if dst != addr && !instrs.contains_key(&addr)
					{
						queries.insert(dst);
					}
					branches.insert(dst);
				}
				let last = !result.1.get_returns();
				instrs.insert(addr, (result.1, addr + result.0 as u32));
				pos += result.0;
				if last {break;}
			}
		}
	}
	
	println!(".addr 0x{base:08X}");
	let mut space = false;
	let mut last = base;
	for (addr, (instr, after)) in instrs
	{
		if addr != last
		{
			println!();
			space = false;
		}
		if branches.contains(&addr)
		{
			println!("{}l_{addr:08X}:", if space {"\n"} else {""});
		}
		println!("\t{}", instr.at(addr));
		space = !instr.get_returns();
		last = after;
	}
}
