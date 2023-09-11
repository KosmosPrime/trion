use std::path::PathBuf;

use crate::arm6m::Arm6M;
use crate::asm::directive::DirectiveList;
use crate::asm::Context;
use crate::asm::mem::MemoryRange;
use crate::asm::mem::map::Search;
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

pub fn assemble(buff: &mut Vec<u8>, path: PathBuf) -> bool
{
	let directives = DirectiveList::generate();
	let mut ctx = Context::new(&Arm6M, &directives);
	drop(ctx.assemble(buff.as_ref(), path));
	if let Err(e) = ctx.close_segment()
	{
		print_err!(e, "Could not close final segment");
		return false;
	}
	
	if !ctx.has_errored() {ctx.run_tasks();}
	if ctx.has_errored()
	{
		for err in ctx.get_errors()
		{
			eprintln!("Error: {err}");
			let mut source = err.source();
			while let Some(src) = source
			{
				eprintln!("\tsource: {src}");
				source = src.source();
			}
		}
		return false;
	}
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
