use std::fs::OpenOptions;
use std::io::{Read, Write};
use std::path::Path;

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
				if assembler::assemble(&mut buff, path.to_path_buf())
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
