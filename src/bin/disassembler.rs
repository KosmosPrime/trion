use std::collections::{BTreeMap, BTreeSet};
use std::fs::OpenOptions;
use std::io::Read;

use trion::arm6m::asm::Instruction;

pub fn main()
{
	let mut args = std::env::args();
	assert!(args.next().is_some());
	let Some(fp) = args.next()
	else
	{
		eprintln!("Missing input file argument");
		return;
	};
	
	let mut buff = Vec::new();
	let mut f = OpenOptions::new().read(true).open(fp).unwrap();
	f.read_to_end(&mut buff).unwrap();
	drop(f);
	
	const BASE: u32 = 0x20000000;
	let mut queries = BTreeSet::new();
	let mut instrs = BTreeMap::new();
	let mut branches = BTreeSet::new();
	queries.insert(BASE);
	while let Some(&start) = queries.iter().next()
	{
		queries.remove(&start);
		if start >= BASE && start - BASE < buff.len() as u32
		{
			let mut pos = (start - BASE) as usize;
			while pos < buff.len()
			{
				let result = Instruction::decode(&buff[pos..]).unwrap();
				let addr = BASE + pos as u32;
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
	
	println!(".addr 0x{BASE:08X}");
	let mut space = false;
	let mut last = BASE;
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
