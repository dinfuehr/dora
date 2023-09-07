use crate::constpool::ConstPool;
use crate::gc::Address;
use crate::masm::CodeDescriptor;
use crate::vm::{
    CommentTable, GcPoint, GcPointTable, LazyCompilationData, LocationTable, RelocationTable,
    CODE_ALIGNMENT,
};
use dora_bytecode::Location;

pub fn decode_code_descriptor(reader: &mut ByteReader) -> CodeDescriptor {
    let constpool = decode_const_pool(reader);
    let code = decode_code(reader);
    let lazy_compilation = decode_lazy_compilation(reader);
    let gcpoints = decode_gcpoint_table(reader);
    let positions = decode_location_table(reader);
    let comments = decode_comment_table(reader);
    CodeDescriptor {
        code,
        comments,
        constpool,
        lazy_compilation,
        gcpoints,
        positions,
        relocations: RelocationTable::new(),
    }
}

fn decode_const_pool(reader: &mut ByteReader) -> ConstPool {
    let length = reader.read_u32() as usize;
    let mut pool = ConstPool::new();

    for _ in 0..length {
        pool.add_addr(reader.read_address());
    }

    pool.align(CODE_ALIGNMENT as i32);
    pool
}

fn decode_code(reader: &mut ByteReader) -> Vec<u8> {
    decode_array_u8(reader)
}

fn decode_location_table(reader: &mut ByteReader) -> LocationTable {
    let length = reader.read_u32() as usize;
    let mut result = LocationTable::new();

    for _ in 0..length {
        let pos = reader.read_u32();
        let line = reader.read_u32();
        let col = reader.read_u32();
        result.insert(pos, Location::new(line, col));
    }

    result
}

fn decode_array_u8(reader: &mut ByteReader) -> Vec<u8> {
    let length = reader.read_u32() as usize;
    let mut result = Vec::with_capacity(length);

    for _ in 0..length {
        result.push(reader.read_u8());
    }

    result
}

fn decode_comment_table(reader: &mut ByteReader) -> CommentTable {
    let mut result = CommentTable::new();
    let length = reader.read_u32() as usize;

    for _ in 0..length {
        let offset = reader.read_u32();
        let comment = decode_string(reader);
        result.insert(offset, comment)
    }

    result
}

fn decode_lazy_compilation(reader: &mut ByteReader) -> LazyCompilationData {
    let length = reader.read_u32() as usize;
    assert_eq!(length, 0);
    LazyCompilationData::new()
}

fn decode_gcpoint_table(reader: &mut ByteReader) -> GcPointTable {
    let length = reader.read_u32() as usize;
    let mut result = GcPointTable::new();

    for _ in 0..length {
        let offset = reader.read_u32();
        let gcpoint = decode_gcpoint(reader);
        result.insert(offset, gcpoint);
    }

    result
}

fn decode_gcpoint(reader: &mut ByteReader) -> GcPoint {
    let length = reader.read_u32() as usize;
    let mut offsets = Vec::with_capacity(length);

    for _ in 0..length {
        offsets.push(reader.read_u32() as i32);
    }

    GcPoint::from_offsets(offsets)
}

fn decode_string(reader: &mut ByteReader) -> String {
    let array = decode_array_u8(reader);
    String::from_utf8(array).expect("invalid encoding")
}

pub struct ByteReader {
    idx: usize,
    data: Vec<u8>,
}

impl ByteReader {
    pub fn new(data: Vec<u8>) -> ByteReader {
        ByteReader { idx: 0, data }
    }

    pub fn read_u8(&mut self) -> u8 {
        let result = self.data[self.idx];
        self.idx += 1;
        result
    }

    pub fn read_u32(&mut self) -> u32 {
        let b1 = self.read_u8() as u32;
        let b2 = self.read_u8() as u32;
        let b3 = self.read_u8() as u32;
        let b4 = self.read_u8() as u32;

        (b4 << 24) | (b3 << 16) | (b2 << 8) | b1
    }

    pub fn read_u64(&mut self) -> u64 {
        let w1 = self.read_u32() as u64;
        let w2 = self.read_u32() as u64;

        (w2 << 32) | w1
    }

    pub fn read_address(&mut self) -> Address {
        (self.read_u64() as usize).into()
    }

    pub fn has_more(&self) -> bool {
        self.idx < self.data.len()
    }
}
