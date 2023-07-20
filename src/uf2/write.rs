use core::fmt;
use std::error::Error;
use std::ops::{Index, IndexMut};

use crate::uf2::{BLOCK_LEN, DATA_START, FLAG_FAMILY_ID, FLAG_NO_FLASH, FTR_MAGIC, HDR_MAGIC, Info, MAGIC, MAX_DATA_LEN, PADDING_END};

enum Data<'l>
{
	Slice(&'l mut [u8]),
	Vector(&'l mut Vec<u8>),
}

impl<'l> Data<'l>
{
	fn capacity(&self) -> usize
	{
		match self
		{
			Data::Slice(dst) => dst.len(),
			Data::Vector(..) => isize::MAX as usize,
		}
	}
	
	fn check_write(&mut self, pos: &mut usize, len: usize) -> Result<(), WriteError>
	{
		match self
		{
			Data::Slice(dst) =>
			{
				if dst.len() - *pos < len
				{
					return Err(WriteError::Overflow{need: (dst.len() - *pos).saturating_add(len), have: dst.len()});
				}
			},
			Data::Vector(dst) =>
			{
				if dst.len() - *pos < len
				{
					if isize::MAX as usize - *pos < len
					{
						return Err(WriteError::Overflow{need: pos.saturating_add(len), have: isize::MAX as usize});
					}
					dst.resize(*pos + len, 0);
				}
			},
		}
		Ok(())
	}
}

impl<'l, T> Index<T> for Data<'l> where [u8]: Index<T>, Vec<u8>: Index<T>
{
	type Output = <[u8] as Index<T>>::Output;
	
	fn index(&self, index: T) -> &Self::Output
	{
		match self
		{
			Data::Slice(dst) => &dst[index],
			Data::Vector(dst) => &dst.as_slice()[index],
		}
	}
}

impl<'l, T> IndexMut<T> for Data<'l> where [u8]: IndexMut<T>, Vec<u8>: Index<T>
{
	fn index_mut(&mut self, index: T) -> &mut Self::Output
	{
		match self
		{
			Data::Slice(dst) => &mut dst[index],
			Data::Vector(dst) => &mut dst.as_mut_slice()[index],
		}
	}
}

pub struct Uf2Write<'l>
{
	info: Info,
	block_size: usize,
	align: usize,
	dst: Data<'l>,
	pos: usize,
	count: u32,
}

impl<'l> Uf2Write<'l>
{
	pub fn new(family_id: Option<u32>, block_size: usize, align: usize, dst: &'l mut [u8]) -> Result<Self, NewError>
	{
		if block_size == 0 || block_size > MAX_DATA_LEN as usize
		{
			return Err(NewError::BlockSize(block_size));
		}
		if align == 0 || block_size % align != 0
		{
			return Err(NewError::Alignment{align, block_size});
		}
		let info = match family_id
		{
			None => Info::Unused,
			Some(fid) => Info::BoardFamily(fid),
		};
		Ok(Self{info, block_size, align, dst: Data::Slice(dst), pos: 0, count: 0})
	}
	
	pub fn new_vec(family_id: Option<u32>, block_size: usize, align: usize, dst: &'l mut Vec<u8>) -> Result<Self, NewError>
	{
		if block_size == 0 || block_size > MAX_DATA_LEN as usize
		{
			return Err(NewError::BlockSize(block_size));
		}
		if align == 0 || block_size % align != 0
		{
			return Err(NewError::Alignment{align, block_size});
		}
		let info = match family_id
		{
			None => Info::Unused,
			Some(fid) => Info::BoardFamily(fid),
		};
		let pos = dst.len();
		Ok(Self{info, block_size, align, dst: Data::Vector(dst), pos, count: 0})
	}
	
	fn encode(&mut self, addr: u32, block: &[u8], block_len: u32, no_flash: bool)
	{
		let dst = &mut self.dst[self.pos..self.pos + BLOCK_LEN];
		dst[0x000..0x004].copy_from_slice(&u32::to_le_bytes(MAGIC));
		dst[0x004..0x008].copy_from_slice(&u32::to_le_bytes(HDR_MAGIC));
		let mut flags = 0;
		if no_flash {flags |= FLAG_NO_FLASH;}
		if let Info::BoardFamily(..) = self.info {flags |= FLAG_FAMILY_ID;}
		dst[0x008..0x00C].copy_from_slice(&u32::to_le_bytes(flags));
		dst[0x00C..0x010].copy_from_slice(&u32::to_le_bytes(addr));
		dst[0x010..0x014].copy_from_slice(&u32::to_le_bytes(block_len));
		dst[0x014..0x018].copy_from_slice(&u32::to_le_bytes(self.count));
		//dst[0x018..0x01C].copy_from_slice(&u32::to_le_bytes(count));
		let info_val = match self.info
		{
			Info::Unused => 0,
			Info::FileSize(sz) => sz,
			Info::BoardFamily(fid) => fid,
		};
		dst[0x01C..DATA_START].copy_from_slice(&u32::to_le_bytes(info_val));
		let data_end = DATA_START + block.len();
		dst[DATA_START..data_end].copy_from_slice(block);
		dst[data_end..PADDING_END].fill(0);
		dst[PADDING_END..BLOCK_LEN].copy_from_slice(&u32::to_le_bytes(FTR_MAGIC));
		self.pos += BLOCK_LEN;
		self.count += 1;
	}
	
	pub fn write(&mut self, addr: u32, block: &[u8], no_flash: bool) -> Result<(), WriteError>
	{
		if !block.is_empty()
		{
			if block.len() % self.align != 0
			{
				return Err(WriteError::Alignment{len: block.len(), align: block.len() % self.align});
			}
			self.dst.check_write(&mut self.pos, BLOCK_LEN)?;
			self.encode(addr, block, self.block_size as u32, no_flash);
		}
		Ok(())
	}
	
	pub fn write_all(&mut self, addr: u32, data: &[u8], no_flash: bool) -> Result<usize, WriteError>
	{
		if !data.is_empty()
		{
			let mis_align = data.len() % self.align;
			let aligned = if mis_align != 0
			{
				if usize::MAX - data.len() < self.align - mis_align
				{
					return Err(WriteError::Alignment{len: data.len(), align: self.align});
				}
				data.len() - mis_align + self.align
			}
			else {data.len()};
			match usize::try_from(u32::MAX - addr)
			{
				Ok(space) =>
				{
					if space < aligned - 1
					{
						return Err(WriteError::Address{need: aligned, have: (u32::MAX - addr).saturating_add(1)});
					}
				},
				Err(..) => (), // the address space we have is larger than `data` could ever be
			}
			let block_cnt = aligned / self.block_size + (aligned % self.block_size > 0) as usize;
			match u32::try_from(block_cnt)
			{
				Ok(cnt) =>
				{
					// off by one because we must also store the final count (so u32::MAX is an invalid index)
					if u32::MAX - self.count < cnt
					{
						return Err(WriteError::BlockCount{need: block_cnt, have: u32::MAX - self.count});
					}
				},
				Err(..) =>
				{
					return Err(WriteError::BlockCount{need: block_cnt, have: u32::MAX - self.count});
				},
			}
			match block_cnt.checked_mul(BLOCK_LEN)
			{
				None => return Err(WriteError::Overflow{need: usize::MAX, have: self.dst.capacity()}),
				Some(len) => self.dst.check_write(&mut self.pos, len)?,
			}
			for (i, block) in data.chunks(self.block_size).enumerate()
			{
				debug_assert!(i < block_cnt); // implies `i as u32` is also safe
				let block_len = if i < block_cnt - 1 {self.block_size as u32} else {(aligned - i * self.block_size) as u32};
				self.encode(addr + (i * self.block_size) as u32, block, block_len, no_flash);
			}
			Ok(block_cnt)
		}
		else {Ok(0)}
	}
}

impl<'l> Drop for Uf2Write<'l>
{
	fn drop(&mut self)
	{
		// write the block count to all files
		for i in 0..self.count
		{
			// no overflow because we've accessed these addresses before
			let base = self.pos - BLOCK_LEN * (self.count - i) as usize;
			self.dst[base + 0x018..base + 0x01C].copy_from_slice(&u32::to_le_bytes(self.count));
		}
	}
}

#[derive(Clone, Debug)]
pub enum NewError
{
	BlockSize(usize),
	Alignment{align: usize, block_size: usize},
}

impl fmt::Display for NewError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::BlockSize(block_size) => write!(f, "invalid block size ({block_size})"),
			Self::Alignment{align, block_size} => write!(f, "invalid alignment ({align} for block size {block_size})"),
		}
	}
}

impl Error for NewError {}

#[derive(Clone, Debug)]
pub enum WriteError
{
	Overflow{need: usize, have: usize},
	Alignment{len: usize, align: usize},
	Address{need: usize, have: u32},
	BlockCount{need: usize, have: u32},
}

impl fmt::Display for WriteError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Overflow{need, have} => write!(f, "output buffer overflow (need {need}, capacity {have})"),
			Self::Alignment{len, align} => write!(f, "misaligned buffer (length {len}, for alignment {align})"),
			Self::Address{need, have} => write!(f, "not enough address space (need {need}, have {have})"),
			Self::BlockCount{need, have} => write!(f, "too many blocks (need {need}, have {have})"),
		}
	}
}

impl Error for WriteError {}
