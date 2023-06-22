use core::fmt;
use std::error::Error;

pub mod codec;
pub mod crc;

pub const BLOCK_LEN: usize = 512;
pub const MAGIC: u32 = 0x0A324655;
const HDR_MAGIC: u32 = 0x9E5D5157;
const FLAG_NO_FLASH: u32 = 0x00000001;
const FLAG_CONTAINER: u32 = 0x00001000;
const FLAG_FAMILY_ID: u32 = 0x00002000;
const FLAGS: u32 = FLAG_NO_FLASH | /*FLAG_CONTAINER |*/ FLAG_FAMILY_ID;
const MAX_DATA_LEN: u32 = BLOCK_LEN as u32 - DATA_START as u32 - 4;
const DATA_START: usize = 0x20;
const PADDING_END: usize = BLOCK_LEN - 4;
const FTR_MAGIC: u32 = 0x0AB16F30;

pub struct Uf2Block<'l>
{
	no_flash: bool,
	addr: u32,
	index: u32,
	count: u32,
	info: Info,
	data: &'l [u8],
}

impl<'l> Uf2Block<'l>
{
	pub const fn new(no_flash: bool, addr: u32, index: u32, count: u32, info: Info, data: &'l [u8]) -> Result<Self, NewError>
	{
		if data.is_empty()
		{
			return Err(NewError::Empty);
		}
		if data.len() > MAX_DATA_LEN as usize
		{
			return Err(NewError::TooLong{have: data.len(), max: MAX_DATA_LEN as usize});
		}
		Ok(Self
		{
			no_flash,
			addr,
			index,
			count,
			info,
			data,
		})
	}
	
	pub fn read(src: &'l [u8]) -> Result<(usize, Self), ReadError>
	{
		if src.len() < BLOCK_LEN
		{
			return Err(ReadError::Underflow{need: BLOCK_LEN, have: src.len()});
		}
		let hdr0 = u32::from_le_bytes(<[u8; 4]>::try_from(&src[0x000..0x004]).unwrap());
		let hdr1 = u32::from_le_bytes(<[u8; 4]>::try_from(&src[0x004..0x008]).unwrap());
		if hdr0 != MAGIC || hdr1 != HDR_MAGIC
		{
			return Err(ReadError::BadHeader(hdr0, hdr1));
		}
		let flags = u32::from_le_bytes(<[u8; 4]>::try_from(&src[0x008..0x00C]).unwrap());
		if (flags & !FLAGS) != 0
		{
			return Err(ReadError::BadFlags(flags & !FLAGS));
		}
		let addr = u32::from_le_bytes(<[u8; 4]>::try_from(&src[0x00C..0x010]).unwrap());
		let len = u32::from_le_bytes(<[u8; 4]>::try_from(&src[0x010..0x014]).unwrap());
		if len == 0 || len > MAX_DATA_LEN
		{
			return Err(ReadError::BadLength{max: MAX_DATA_LEN, have: len});
		}
		let index = u32::from_le_bytes(<[u8; 4]>::try_from(&src[0x014..0x018]).unwrap());
		let count = u32::from_le_bytes(<[u8; 4]>::try_from(&src[0x018..0x01C]).unwrap());
		if index >= count
		{
			return Err(ReadError::BadIndex{index, count});
		}
		let info_val = u32::from_le_bytes(<[u8; 4]>::try_from(&src[0x01C..DATA_START]).unwrap());
		let info = {
			if (flags & FLAG_FAMILY_ID) != 0 {Info::BoardFamily(info_val)}
			else if (flags & FLAG_CONTAINER) != 0 {Info::FileSize(info_val)}
			else if info_val != 0
			{
				return Err(ReadError::BadInfo);
			}
			else {Info::Unused}
		};
		let data_end = DATA_START + len as usize;
		let data = &src[DATA_START..data_end];
		if let Some(off) = src[data_end..PADDING_END].iter().position(|&v| v != 0)
		{
			return Err(ReadError::BadPadding{at: data_end as usize + off});
		}
		let ftr = u32::from_le_bytes(<[u8; 4]>::try_from(&src[PADDING_END..BLOCK_LEN]).unwrap());
		if ftr != FTR_MAGIC
		{
			return Err(ReadError::BadFooter(ftr));
		}
		Ok((BLOCK_LEN, Self
		{
			no_flash: (flags & FLAG_NO_FLASH) != 0,
			addr,
			index,
			count,
			info,
			data,
		}))
	}
	
	pub fn write(&self, dst: &mut [u8]) -> Result<usize, WriteError>
	{
		if dst.len() < BLOCK_LEN
		{
			return Err(WriteError::Overflow{need: BLOCK_LEN, have: dst.len()});
		}
		dst[0x000..0x004].copy_from_slice(&u32::to_le_bytes(MAGIC));
		dst[0x004..0x008].copy_from_slice(&u32::to_le_bytes(HDR_MAGIC));
		let mut flags = 0;
		if self.no_flash {flags |= FLAG_NO_FLASH;}
		if let Info::BoardFamily(..) = self.info {flags |= FLAG_FAMILY_ID;}
		dst[0x008..0x00C].copy_from_slice(&u32::to_le_bytes(flags));
		dst[0x00C..0x010].copy_from_slice(&u32::to_le_bytes(self.addr));
		dst[0x010..0x014].copy_from_slice(&u32::to_le_bytes(self.data.len() as u32));
		dst[0x014..0x018].copy_from_slice(&u32::to_le_bytes(self.index));
		dst[0x018..0x01C].copy_from_slice(&u32::to_le_bytes(self.count));
		let info_val = match self.info
		{
			Info::Unused => 0,
			Info::FileSize(sz) => sz,
			Info::BoardFamily(fid) => fid,
		};
		dst[0x01C..DATA_START].copy_from_slice(&u32::to_le_bytes(info_val));
		let data_end = DATA_START + self.data.len();
		dst[DATA_START..data_end].copy_from_slice(self.data);
		dst[data_end..PADDING_END].fill(0);
		dst[PADDING_END..BLOCK_LEN].copy_from_slice(&u32::to_le_bytes(FTR_MAGIC));
		Ok(BLOCK_LEN)
	}
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Info
{
	Unused,
	FileSize(u32),
	BoardFamily(u32),
}

#[derive(Clone, Debug)]
pub enum NewError
{
	Empty,
	TooLong{have: usize, max: usize},
}

impl fmt::Display for NewError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Empty => f.write_str("data cannot be empty"),
			Self::TooLong{have, max} => write!(f, "data is {have} bytes long, max is {max})"),
		}
	}
}

impl Error for NewError {}

#[derive(Clone, Debug)]
pub enum ReadError
{
	Underflow{need: usize, have: usize},
	BadHeader(u32, u32),
	BadFlags(u32),
	BadLength{max: u32, have: u32},
	BadIndex{index: u32, count: u32},
	BadInfo,
	BadPadding{at: usize},
	BadFooter(u32),
}

impl fmt::Display for ReadError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Underflow{need, have} => write!(f, "input buffer underflow (need {need}, got {have})"),
			Self::BadHeader(hdr0, hdr1) => write!(f, "incorrect header magic ({hdr0:08X} {hdr1:08X})"),
			Self::BadFlags(flags) => write!(f, "unsupported flags ({flags:X})"),
			Self::BadLength{have: 0, ..} => f.write_str("data section is empty"),
			Self::BadLength{max, have} => write!(f, "data section too long (max {max}, got {have})"),
			Self::BadIndex{index, count} => write!(f, "invalid block index {index} (count {count})"),
			Self::BadInfo => f.write_str("invalid info field value"),
			Self::BadPadding{at} => write!(f, "malformed padding (at {at})"),
			Self::BadFooter(ftr) => write!(f, "incorrect footer magic ({ftr:08X})"),
		}
	}
}

impl Error for ReadError {}

#[derive(Clone, Debug)]
pub enum WriteError
{
	Overflow{need: usize, have: usize},
}

impl fmt::Display for WriteError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self
		{
			Self::Overflow{need, have} => write!(f, "output buffer overflow (need {need}, got {have})"),
		}
	}
}

impl Error for WriteError {}
