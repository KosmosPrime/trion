use core::fmt;
use std::error::Error;

use crate::uf2::{BLOCK_LEN, Info, ReadError, Uf2Block};

const DATA_LEN: usize = 256;

pub fn encode(src: &[u8], dst: &mut Vec<u8>, addr: u32, family: Option<u32>) -> Result<(usize, usize), EncodeError>
{
	if src.is_empty()
	{
		return Err(EncodeError::Underflow);
	}
	let chunks = (src.len() + (DATA_LEN - 1)) / DATA_LEN;
	// no rounding up because we round blocks up to multiples of 256 bytes
	let addr_chunks = (u32::MAX - addr) / DATA_LEN as u32;
	if chunks > addr_chunks as usize
	{
		const MAX_ADDRS: u32 = u32::MAX / DATA_LEN as u32;
		let need = if addr_chunks <= MAX_ADDRS {addr_chunks * DATA_LEN as u32} else {u32::MAX};
		return Err(EncodeError::AddrOverflow{have: u32::MAX - addr, need})
	}
	let info = match family
	{
		None => Info::Unused,
		Some(fid) => Info::BoardFamily(fid),
	};
	for (i, s) in src.chunks(DATA_LEN).enumerate()
	{
		let pos = dst.len();
		dst.reserve(BLOCK_LEN);
		dst.resize(pos + BLOCK_LEN, 0u8);
		if s.len() == DATA_LEN
		{
			let block = Uf2Block::new(false, addr + (i * DATA_LEN) as u32, i as u32, chunks as u32, info, s).unwrap();
			block.write(&mut dst[pos..pos + BLOCK_LEN]).unwrap();
		}
		else
		{
			let len = src.len() % DATA_LEN;
			let mut temp = [0u8; 256];
			temp[..len].copy_from_slice(&src[src.len() - len..]);
			let block = Uf2Block::new(false, addr + (i * DATA_LEN) as u32, i as u32, chunks as u32, info, &temp).unwrap();
			block.write(&mut dst[pos..pos + BLOCK_LEN]).unwrap();
		}
	}
	Ok((src.len(), BLOCK_LEN * chunks))
}

#[derive(Clone, Debug)]
pub enum EncodeError
{
	Underflow,
	Overflow{need: usize, have: usize},
	AddrOverflow{need: u32, have: u32},
}

impl fmt::Display for EncodeError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Underflow => f.write_str("empty input buffer"),
			Self::Overflow{need, have} => write!(f, "output buffer overflow (need {need}, got {have})"),
			Self::AddrOverflow{need, have} => write!(f, "address overflow (need {need}, got {have})"),
		}
	}
}

impl Error for EncodeError {}

pub fn decode(src: &[u8], dst: &mut Vec<u8>, family: Option<u32>) -> Result<DecodeInfo, DecodeError>
{
	if src.len() < BLOCK_LEN
	{
		return Err(DecodeError::Underflow{need: BLOCK_LEN, have: src.len()});
	}
	let chunks = src.len() / BLOCK_LEN;
	if src.len() % BLOCK_LEN != 0
	{
		let need = if chunks > usize::MAX / BLOCK_LEN {usize::MAX} else {chunks * BLOCK_LEN};
		return Err(DecodeError::Underflow{need, have: src.len()});
	}
	let mut index = 0;
	let mut in_pos = 0;
	let mut out_pos = 0;
	let mut start_addr: Option<u32> = None;
	let mut prev: Option<Uf2Block<'_>> = None;
	let info = match family
	{
		None => Info::Unused,
		Some(fid) => Info::BoardFamily(fid),
	};
	while in_pos < src.len()
	{
		match Uf2Block::read(&src[in_pos..in_pos + BLOCK_LEN])
		{
			Ok((count, block)) =>
			{
				assert_eq!(count, BLOCK_LEN);
				in_pos += count;
				if block.index != index
				{
					return Err(DecodeError::IndexJump{idx: block.index, expect: index})
				}
				if !block.no_flash
				{
					if u32::MAX - block.addr < DATA_LEN as u32
					{
						return Err(DecodeError::AddressOverflow{idx: block.index, addr: block.addr});
					}
					if let Some(prev) = &prev
					{
						if prev.addr >= block.addr || block.addr - prev.addr != DATA_LEN as u32
						{
							let from = prev.addr.saturating_add(DATA_LEN as u32);
							return Err(DecodeError::AddressJump{idx: block.index, from, to: block.addr});
						}
					}
					if start_addr.is_none() {start_addr = Some(block.addr);}
					if block.info != info
					{
						return Err(DecodeError::BadInfo{idx: block.index, expect: info, have: block.info});
					}
					if block.data.len() != DATA_LEN
					{
						return Err(DecodeError::DataSize{idx: block.index, expect: DATA_LEN, have: block.data.len()});
					}
					dst.extend_from_slice(block.data);
					out_pos += DATA_LEN;
					prev = Some(block);
				}
				index += 1;
			},
			Err(err) => return Err(DecodeError::BlockRead{idx: index, err}),
		};
	}
	
	if let Some(prev) = prev
	{
		if prev.index != prev.count - 1
		{
			return Err(DecodeError::BlockCount{idx: prev.index, count: prev.count});
		}
		Ok(DecodeInfo{in_pos, out_pos, addr: start_addr.unwrap()})
	}
	else
	{
		let need = if chunks > usize::MAX / BLOCK_LEN {usize::MAX} else {chunks * BLOCK_LEN};
		return Err(DecodeError::Underflow{have: src.len(), need});
	}
}

#[derive(Clone, Copy, Debug)]
pub struct DecodeInfo
{
	pub in_pos: usize,
	pub out_pos: usize,
	pub addr: u32,
}

#[derive(Clone, Debug)]
pub enum DecodeError
{
	Underflow{need: usize, have: usize},
	BlockRead{idx: u32, err: ReadError},
	IndexJump{idx: u32, expect: u32},
	AddressOverflow{idx: u32, addr: u32},
	AddressJump{idx: u32, from: u32, to: u32},
	BadInfo{idx: u32, expect: Info, have: Info},
	DataSize{idx: u32, expect: usize, have: usize},
	BlockCount{idx: u32, count: u32},
}

impl fmt::Display for DecodeError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Underflow{need, have} => write!(f, "input buffer underflow (need {need}, got {have})"),
			Self::BlockRead{idx, ..} => write!(f, "malformed uf2 block (index {idx})"),
			Self::IndexJump{idx, expect} => write!(f, "index discontinuity at block {idx} (should be {expect})"),
			Self::AddressOverflow{idx, addr} => write!(f, "address overflow at block {idx} (starts at {addr})"),
			Self::AddressJump{idx, from, to} => write!(f, "address discontinuity at block {idx} ({from} -> {to})"),
			Self::BadInfo{idx, expect, have} => write!(f, "incorrect info value at block {idx} (expected {expect:?}, got {have:?})"),
			Self::DataSize{idx, expect, have} => write!(f, "incorrect data size of block {idx} (expected {expect}, got {have})"),
			Self::BlockCount{idx, count} =>
			{
				if idx < count {write!(f, "not enough blocks (expected {count}, got {idx})")}
				else {write!(f, "too many blocks (max {count}, got {idx})")}
			},
		}
	}
}

impl Error for DecodeError
{
	fn source(&self) -> Option<&(dyn Error + 'static)>
	{
		match self
		{
			Self::BlockRead{err, ..} => Some(err),
			_ => None,
		}
	}
}
