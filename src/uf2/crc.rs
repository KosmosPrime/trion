pub struct Crc(u32);

impl Crc
{
	pub const POLYNOMIAL: u32 = 0x04C11DB7;
	
	pub const TABLE: [u32; 256] = {
		let mut table = [0u32; 256];
		table[1] = 0x04C11DB7;
		let mut pos = 2;
		while pos < 256
		{
			let prev = table[pos >> 1];
			let curr = prev << 1 ^ if ((prev >> (u32::BITS - 1)) & 1) != 0 {Self::POLYNOMIAL} else {0};
			table[pos + 0] = curr;
			table[pos + 1] = curr ^ Self::POLYNOMIAL;
			pos += 2;
		}
		table
	};
	
	pub fn new() -> Self
	{
		Self(0xFFFFFFFF)
	}
	
	pub fn update(&mut self, value: u8)
	{
		self.0 = (self.0 << 8) ^ Self::TABLE[(value ^ (self.0 >> 24) as u8) as usize];
	}
	
	pub fn update_slice(&mut self, value: &[u8])
	{
		value.iter().for_each(|&v| self.update(v));
	}
	
	pub fn get_value(&self) -> u32
	{
		self.0
	}
}

impl Default for Crc
{
	fn default() -> Self
	{
		Self::new()
	}
}
