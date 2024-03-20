use super::*;
	
#[test]
fn internal_locate()
{
	let mut map = MemoryMap::new();
	assert_eq!(map.put(100, b"hello"), Ok(5));
	assert_eq!(map.put(120, b"world"), Ok(5));
	// just before first word
	assert_eq!(map.locate(99, Search::Exact), None);
	assert_eq!(map.locate(99, Search::Below), None);
	assert_eq!(map.locate(99, Search::Above), Some(0));
	// first addr of second word
	assert_eq!(map.locate(100, Search::Exact), Some(0));
	assert_eq!(map.locate(100, Search::Below), Some(0));
	assert_eq!(map.locate(100, Search::Above), Some(0));
	// last addr of second word
	assert_eq!(map.locate(104, Search::Exact), Some(0));
	assert_eq!(map.locate(104, Search::Below), Some(0));
	assert_eq!(map.locate(104, Search::Above), Some(0));
	// just after first word (before second)
	assert_eq!(map.locate(105, Search::Exact), None);
	assert_eq!(map.locate(105, Search::Below), Some(0));
	assert_eq!(map.locate(105, Search::Above), Some(1));
	
	// just before second word (after first)
	assert_eq!(map.locate(119, Search::Exact), None);
	assert_eq!(map.locate(119, Search::Below), Some(0));
	assert_eq!(map.locate(119, Search::Above), Some(1));
	// first addr of second word
	assert_eq!(map.locate(120, Search::Exact), Some(1));
	assert_eq!(map.locate(120, Search::Below), Some(1));
	assert_eq!(map.locate(120, Search::Above), Some(1));
	// last addr of second word
	assert_eq!(map.locate(124, Search::Exact), Some(1));
	assert_eq!(map.locate(124, Search::Below), Some(1));
	assert_eq!(map.locate(124, Search::Above), Some(1));
	// just after second word
	assert_eq!(map.locate(125, Search::Exact), None);
	assert_eq!(map.locate(125, Search::Below), Some(1));
	assert_eq!(map.locate(125, Search::Above), None);
}

#[test]
fn sparse()
{
	let mut map = MemoryMap::new();
	assert_eq!(map.put(100, b"hello"), Ok(5));
	assert_eq!(map.put(120, b"world"), Ok(5));
	assert_eq!(map.len(), 2);
	assert_eq!(map.count(), (10, 2));
	let mut iter = map.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
	assert_eq!(iter.next(), None);
	assert_eq!(map.count_range(MemoryRange::new(90, 102)), (3, 1));
	let mut iter = map.iter_range(MemoryRange::new(90, 102));
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 102}, b"hel".as_ref())));
	assert_eq!(iter.next(), None);
	assert_eq!(map.count_range(MemoryRange::new(90, 110)), (5, 1));
	let mut iter = map.iter_range(MemoryRange::new(90, 110));
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
	assert_eq!(iter.next(), None);
	assert_eq!(map.count_range(MemoryRange::new(100, 110)), (5, 1));
	assert!(map.iter_range(MemoryRange::new(100, 110)).eq(map.iter_range(MemoryRange::new(90, 110))));
	assert_eq!(map.count_range(MemoryRange::new(103, 104)), (2, 1));
	let mut iter = map.iter_range(MemoryRange::new(103, 104));
	assert_eq!(iter.next(), Some((MemoryRange{first: 103, last: 104}, b"lo".as_ref())));
	assert_eq!(iter.next(), None);
	assert_eq!(map.count_range(MemoryRange::new(103, 110)), (2, 1));
	assert!(map.iter_range(MemoryRange::new(103, 110)).eq(map.iter_range(MemoryRange::new(103, 104))));
	assert_eq!(map.count_range(MemoryRange::new(105, 119)), (0, 0));
	assert_eq!(map.iter_range(MemoryRange::new(105, 119)).next(), None);
	assert_eq!(map.count_range(MemoryRange::new(104, 120)), (2, 2));
	let mut iter = map.iter_range(MemoryRange::new(104, 120));
	assert_eq!(iter.next(), Some((MemoryRange{first: 104, last: 104}, b"o".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 120}, b"w".as_ref())));
	assert_eq!(iter.next(), None);
	assert_eq!(map.count_range(MemoryRange::new(103, 121)), (4, 2));
	let mut iter = map.iter_range(MemoryRange::new(103, 121));
	assert_eq!(iter.next(), Some((MemoryRange{first: 103, last: 104}, b"lo".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 121}, b"wo".as_ref())));
	assert_eq!(iter.next(), None);
	assert_eq!(map.count_range(MemoryRange::new(90, 121)), (7, 2));
	let mut iter = map.iter_range(MemoryRange::new(90, 121));
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 121}, b"wo".as_ref())));
	assert_eq!(iter.next(), None);
	assert_eq!(map.count_range(MemoryRange::new(90, 130)), (10, 2));
	let mut iter = map.iter_range(MemoryRange::new(90, 130));
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
	assert_eq!(iter.next(), None);
	
	let mut map = MemoryMap::new();
	assert_eq!(map.put(0, b"first"), Ok(5));
	assert_eq!(map.put(((1u64 << u32::BITS) - 4) as u32, b"last"), Ok(4));
	assert_eq!(map.len(), 2);
	assert_eq!(map.count(), (9, 2));
	let mut iter = map.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 0, last: 4}, b"first".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: u32::MAX - 3, last: u32::MAX}, b"last".as_ref())));
	assert_eq!(iter.next(), None);
}

#[test]
fn overwrite()
{
	let mut map = MemoryMap::new();
	assert_eq!(map.put(100, b"world hello"), Ok(11));
	assert_eq!(map.put(100, b"hello world"), Ok(0));
	assert_eq!(map.len(), 1);
	assert_eq!(map.iter().next(), Some((MemoryRange{first: 100, last: 110}, b"hello world".as_ref())));
	assert_eq!(map.put(106, b" last"), Ok(0));
	assert_eq!(map.len(), 1);
	assert_eq!(map.iter().next(), Some((MemoryRange{first: 100, last: 110}, b"hello  last".as_ref())));
	assert_eq!(map.put(100, b"first"), Ok(0));
	assert_eq!(map.len(), 1);
	assert_eq!(map.iter().next(), Some((MemoryRange{first: 100, last: 110}, b"first  last".as_ref())));
	assert_eq!(map.put(105, b"__"), Ok(0));
	assert_eq!(map.len(), 1);
	assert_eq!(map.iter().next(), Some((MemoryRange{first: 100, last: 110}, b"first__last".as_ref())));
	assert_eq!(map.put(96, b"from first"), Ok(4));
	assert_eq!(map.len(), 1);
	assert_eq!(map.iter().next(), Some((MemoryRange{first: 96, last: 110}, b"from first_last".as_ref())));
	assert_eq!(map.put(106, b" to last"), Ok(3));
	assert_eq!(map.len(), 1);
	assert_eq!(map.iter().next(), Some((MemoryRange{first: 96, last: 113}, b"from first to last".as_ref())));
	
	let mut map = MemoryMap::new();
	assert_eq!(map.put(0, b"f"), Ok(1));
	assert_eq!(map.put(u32::MAX, b"t"), Ok(1));
	assert_eq!(map.len(), 2);
	let mut iter = map.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 0, last: 0}, b"f".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: u32::MAX, last: u32::MAX}, b"t".as_ref())));
	assert_eq!(iter.next(), None);
	assert_eq!(map.put(0, b"first"), Ok(4));
	assert_eq!(map.len(), 2);
	assert_eq!(map.put(((1u64 << u32::BITS) - 4) as u32, b"last"), Ok(3));
	assert_eq!(map.len(), 2);
	let mut iter = map.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 0, last: 4}, b"first".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: u32::MAX - 3, last: u32::MAX}, b"last".as_ref())));
	assert_eq!(iter.next(), None);
}

#[test]
fn join()
{
	let mut map = MemoryMap::new();
	assert_eq!(map.put(104, b"o w"), Ok(3));
	assert_eq!(map.len(), 1);
	assert_eq!(map.iter().next(), Some((MemoryRange{first: 104, last: 106}, b"o w".as_ref())));
	assert_eq!(map.put(102, b"ll"), Ok(2));
	assert_eq!(map.len(), 1);
	assert_eq!(map.iter().next(), Some((MemoryRange{first: 102, last: 106}, b"llo w".as_ref())));
	assert_eq!(map.put(100, b"hell"), Ok(2));
	assert_eq!(map.len(), 1);
	assert_eq!(map.iter().next(), Some((MemoryRange{first: 100, last: 106}, b"hello w".as_ref())));
	assert_eq!(map.put(109, b"ld"), Ok(2));
	assert_eq!(map.len(), 2);
	let mut iter = map.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 106}, b"hello w".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 109, last: 110}, b"ld".as_ref())));
	assert_eq!(map.put(106, b"world "), Ok(3));
	assert_eq!(map.len(), 1);
	assert_eq!(map.iter().next(), Some((MemoryRange{first: 100, last: 111}, b"hello world ".as_ref())));
	
	let mut map = MemoryMap::new();
	assert_eq!(map.put(98, b"0"), Ok(1));
	assert_eq!(map.put(106, b"1"), Ok(1));
	assert_eq!(map.len(), 2);
	assert_eq!(map.put(101, b"s"), Ok(1));
	assert_eq!(map.put(103, b"f"), Ok(1));
	assert_eq!(map.len(), 4);
	let mut iter = map.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 98, last: 98}, b"0".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 101, last: 101}, b"s".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 103, last: 103}, b"f".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 106, last: 106}, b"1".as_ref())));
	assert_eq!(iter.next(), None);
	assert_eq!(map.put(100, b"a"), Ok(1));
	assert_eq!(map.len(), 4);
	let mut iter = map.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 98, last: 98}, b"0".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 101}, b"as".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 103, last: 103}, b"f".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 106, last: 106}, b"1".as_ref())));
	assert_eq!(iter.next(), None);
	
	println!("{map:?}");
	assert_eq!(map.put(104, b"g"), Ok(1));
	println!("{map:?}");
	assert_eq!(map.len(), 4);
	let mut iter = map.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 98, last: 98}, b"0".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 101}, b"as".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 103, last: 104}, b"fg".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 106, last: 106}, b"1".as_ref())));
	assert_eq!(iter.next(), None);
	
	assert_eq!(map.put(102, b"d"), Ok(1));
	assert_eq!(map.len(), 3);
	let mut iter = map.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 98, last: 98}, b"0".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"asdfg".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 106, last: 106}, b"1".as_ref())));
	assert_eq!(iter.next(), None);
}

#[test]
fn remove()
{
	let mut template = MemoryMap::new();
	assert_eq!(template.put(100, b"hello"), Ok(5));
	assert_eq!(template.put(110, b"test"), Ok(4));
	assert_eq!(template.put(120, b"world"), Ok(5));
	assert_eq!(template.len(), 3);
	let mut iter = template.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 110, last: 113}, b"test".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
	assert_eq!(iter.next(), None);
	
	let mut map = template.clone();
	map.remove_range(MemoryRange::new(90, 130));
	assert_eq!(map.len(), 0);
	let mut map = template.clone();
	map.remove_range(MemoryRange::new(110, 114));
	assert_eq!(map.len(), 2);
	let mut iter = map.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
	assert_eq!(iter.next(), None);
	let mut map = template.clone();
	map.remove_range(MemoryRange::new(110, 111));
	assert_eq!(map.len(), 3);
	let mut iter = map.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 112, last: 113}, b"st".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
	assert_eq!(iter.next(), None);
	map.remove_range(MemoryRange::new(108, 112));
	assert_eq!(map.len(), 3);
	let mut iter = map.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 113, last: 113}, b"t".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
	assert_eq!(iter.next(), None);
	let mut map = template.clone();
	map.remove_range(MemoryRange::new(112, 114));
	assert_eq!(map.len(), 3);
	let mut iter = map.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 110, last: 111}, b"te".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
	assert_eq!(iter.next(), None);
	map.remove_range(MemoryRange::new(111, 118));
	assert_eq!(map.len(), 3);
	let mut iter = map.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 110, last: 110}, b"t".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
	assert_eq!(iter.next(), None);
	
	let mut map = template.clone();
	map.remove_range(MemoryRange::new(102, 111));
	assert_eq!(map.len(), 3);
	let mut iter = map.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 101}, b"he".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 112, last: 113}, b"st".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
	assert_eq!(iter.next(), None);
	map.remove_range(MemoryRange::new(90, 112));
	assert_eq!(map.len(), 2);
	let mut iter = map.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 113, last: 113}, b"t".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 120, last: 124}, b"world".as_ref())));
	assert_eq!(iter.next(), None);
	let mut map = template.clone();
	map.remove_range(MemoryRange::new(112, 122));
	assert_eq!(map.len(), 3);
	let mut iter = map.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 110, last: 111}, b"te".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 123, last: 124}, b"ld".as_ref())));
	assert_eq!(iter.next(), None);
	map.remove_range(MemoryRange::new(111, 128));
	assert_eq!(map.len(), 2);
	let mut iter = map.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 104}, b"hello".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 110, last: 110}, b"t".as_ref())));
	assert_eq!(iter.next(), None);
	
	assert_eq!(template.put(0, b"first"), Ok(5));
	assert_eq!(template.put(((1u64 << u32::BITS) - 4) as u32, b"last"), Ok(4));
	template.remove_range(MemoryRange::new(101, 123));
	assert_eq!(template.len(), 4);
	let mut iter = template.iter();
	assert_eq!(iter.next(), Some((MemoryRange{first: 0, last: 4}, b"first".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 100, last: 100}, b"h".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: 124, last: 124}, b"d".as_ref())));
	assert_eq!(iter.next(), Some((MemoryRange{first: u32::MAX - 3, last: u32::MAX}, b"last".as_ref())));
	assert_eq!(iter.next(), None);
	template.remove_range(MemoryRange::ALL);
	assert_eq!(template.len(), 0);
}
