use crate::constpool::ConstPool;
use crate::masm::CodeDescriptor;
use crate::vm::{CommentTable, GcPointTable, LazyCompilationData, LocationTable, RelocationTable};

pub fn decode_code_descriptor(reader: &mut ByteReader) -> CodeDescriptor {
    let code = decode_code(reader);
    let comments = decode_comment_table(reader);
    CodeDescriptor {
        code,
        comments,
        constpool: ConstPool::new(),
        lazy_compilation: LazyCompilationData::new(),
        gcpoints: GcPointTable::new(),
        positions: LocationTable::new(),
        relocations: RelocationTable::new(),
    }
}

fn decode_code(reader: &mut ByteReader) -> Vec<u8> {
    decode_array_u8(reader)
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

    pub fn has_more(&self) -> bool {
        self.idx < self.data.len()
    }
}
