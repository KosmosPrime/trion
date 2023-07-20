pub mod crc;
pub mod write;

pub const BLOCK_LEN: usize = 512;
pub const MAGIC: u32 = 0x0A324655;
const HDR_MAGIC: u32 = 0x9E5D5157;
const FLAG_NO_FLASH: u32 = 0x00000001;
const FLAG_FAMILY_ID: u32 = 0x00002000;
const MAX_DATA_LEN: u32 = BLOCK_LEN as u32 - DATA_START as u32 - 4;
const DATA_START: usize = 0x20;
const PADDING_END: usize = BLOCK_LEN - 4;
const FTR_MAGIC: u32 = 0x0AB16F30;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Info
{
	Unused,
	FileSize(u32),
	BoardFamily(u32),
}
