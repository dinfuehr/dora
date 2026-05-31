use byteorder::{LittleEndian, WriteBytesExt};
use dora_bytecode::opcode as opc;
use dora_bytecode::{BytecodeTraitType, BytecodeType, BytecodeTypeArray, TraitId};

pub struct ByteBuffer {
    data: Vec<u8>,
}

impl ByteBuffer {
    pub fn new() -> ByteBuffer {
        ByteBuffer { data: Vec::new() }
    }

    pub fn data(&self) -> &[u8] {
        &self.data
    }

    #[allow(dead_code)]
    pub fn len(&self) -> usize {
        self.data.len()
    }

    #[allow(dead_code)]
    pub fn extend_from_slice(&mut self, data: &[u8]) {
        self.data.extend_from_slice(data);
    }

    pub fn emit_u8(&mut self, data: u8) {
        self.data.push(data);
    }

    pub fn emit_bool(&mut self, value: bool) {
        self.emit_u8(if value { 1 } else { 0 });
    }

    pub fn emit_u32(&mut self, data: u32) {
        self.data.write_u32::<LittleEndian>(data).unwrap();
    }

    pub fn emit_u64(&mut self, data: u64) {
        self.data.write_u64::<LittleEndian>(data).unwrap();
    }

    pub fn emit_id(&mut self, data: usize) {
        assert!(data <= i32::MAX as usize);
        self.emit_u32(data as u32);
    }
}

pub fn encode_bytecode_type_array(sta: &BytecodeTypeArray, buffer: &mut ByteBuffer) {
    buffer.emit_u32(sta.len() as u32);

    for ty in sta.iter() {
        encode_bytecode_type(&ty, buffer);
    }
}

pub fn encode_bytecode_type(ty: &BytecodeType, buffer: &mut ByteBuffer) {
    match ty {
        BytecodeType::Unit => {
            buffer.emit_u8(opc::BYTECODE_TYPE_UNIT);
        }
        BytecodeType::Bool => {
            buffer.emit_u8(opc::BYTECODE_TYPE_BOOL);
        }
        BytecodeType::Char => {
            buffer.emit_u8(opc::BYTECODE_TYPE_CHAR);
        }
        BytecodeType::UInt8 => {
            buffer.emit_u8(opc::BYTECODE_TYPE_U_INT8);
        }
        BytecodeType::Int32 => {
            buffer.emit_u8(opc::BYTECODE_TYPE_INT32);
        }
        BytecodeType::Int64 => {
            buffer.emit_u8(opc::BYTECODE_TYPE_INT64);
        }
        BytecodeType::Float32 => {
            buffer.emit_u8(opc::BYTECODE_TYPE_FLOAT32);
        }
        BytecodeType::Float64 => {
            buffer.emit_u8(opc::BYTECODE_TYPE_FLOAT64);
        }
        BytecodeType::Ptr => {
            buffer.emit_u8(opc::BYTECODE_TYPE_PTR);
        }
        BytecodeType::Address => {
            buffer.emit_u8(opc::BYTECODE_TYPE_ADDRESS);
        }
        BytecodeType::This => {
            buffer.emit_u8(opc::BYTECODE_TYPE_THIS);
        }
        BytecodeType::Tuple(subtypes) => {
            buffer.emit_u8(opc::BYTECODE_TYPE_TUPLE);
            encode_bytecode_type_array(subtypes, buffer);
        }
        BytecodeType::TypeParam(type_param_id) => {
            buffer.emit_u8(opc::BYTECODE_TYPE_TYPE_PARAM);
            buffer.emit_id(*type_param_id as usize);
        }
        BytecodeType::Enum(enum_id, source_type_array) => {
            buffer.emit_u8(opc::BYTECODE_TYPE_ENUM);
            buffer.emit_id(enum_id.index());
            encode_bytecode_type_array(source_type_array, buffer);
        }
        BytecodeType::Struct(struct_id, source_type_array) => {
            buffer.emit_u8(opc::BYTECODE_TYPE_STRUCT);
            buffer.emit_id(struct_id.index());
            encode_bytecode_type_array(source_type_array, buffer);
        }
        BytecodeType::Class(class_id, source_type_array) => {
            buffer.emit_u8(opc::BYTECODE_TYPE_CLASS);
            buffer.emit_id(class_id.index());
            encode_bytecode_type_array(source_type_array, buffer);
        }
        BytecodeType::TraitObject(trait_id, source_type_array, assoc_types) => {
            buffer.emit_u8(opc::BYTECODE_TYPE_TRAIT_OBJECT);
            buffer.emit_id(trait_id.index());
            encode_bytecode_type_array(source_type_array, buffer);
            encode_bytecode_type_array(assoc_types, buffer);
        }
        BytecodeType::Lambda(params, ret) => {
            buffer.emit_u8(opc::BYTECODE_TYPE_LAMBDA);
            encode_bytecode_type_array(params, buffer);
            encode_bytecode_type(ret.as_ref(), buffer);
        }
        BytecodeType::Assoc {
            ty,
            trait_ty,
            assoc_id,
        } => {
            buffer.emit_u8(opc::BYTECODE_TYPE_ASSOC);
            encode_bytecode_type(ty.as_ref(), buffer);
            encode_bytecode_trait_type(trait_ty, buffer);
            buffer.emit_id(assoc_id.index());
        }
        BytecodeType::TypeAlias(..) => {
            unreachable!()
        }
        BytecodeType::Ref(inner) => {
            buffer.emit_u8(opc::BYTECODE_TYPE_REF);
            encode_bytecode_type(inner.as_ref(), buffer);
        }
    }
}

fn encode_bytecode_trait_type(trait_ty: &BytecodeTraitType, buffer: &mut ByteBuffer) {
    buffer.emit_u32(trait_ty.trait_id.index_as_u32());
    encode_bytecode_type_array(&trait_ty.type_params, buffer);
    buffer.emit_u32(trait_ty.bindings.len() as u32);

    for (alias_id, ty) in &trait_ty.bindings {
        buffer.emit_u32(alias_id.index_as_u32());
        encode_bytecode_type(ty, buffer);
    }
}

pub struct ByteReader {
    idx: usize,
    data: Vec<u8>,
}

impl ByteReader {
    pub fn new(data: Vec<u8>) -> ByteReader {
        ByteReader { idx: 0, data }
    }

    pub fn read_bool(&mut self) -> bool {
        self.read_u8() != 0
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

    pub fn read_u128(&mut self) -> u128 {
        let w1 = self.read_u64() as u128;
        let w2 = self.read_u64() as u128;

        (w2 << 64) | w1
    }

    pub fn has_more(&self) -> bool {
        self.idx < self.data.len()
    }
}

pub fn decode_bytecode_type(reader: &mut ByteReader) -> BytecodeType {
    let kind = reader.read_u8();

    match kind {
        opc::BYTECODE_TYPE_UNIT => BytecodeType::Unit,
        opc::BYTECODE_TYPE_BOOL => BytecodeType::Bool,
        opc::BYTECODE_TYPE_U_INT8 => BytecodeType::UInt8,
        opc::BYTECODE_TYPE_CHAR => BytecodeType::Char,
        opc::BYTECODE_TYPE_INT32 => BytecodeType::Int32,
        opc::BYTECODE_TYPE_INT64 => BytecodeType::Int64,
        opc::BYTECODE_TYPE_FLOAT32 => BytecodeType::Float32,
        opc::BYTECODE_TYPE_FLOAT64 => BytecodeType::Float64,
        opc::BYTECODE_TYPE_PTR => BytecodeType::Ptr,
        opc::BYTECODE_TYPE_ADDRESS => BytecodeType::Address,
        opc::BYTECODE_TYPE_THIS => BytecodeType::This,
        opc::BYTECODE_TYPE_CLASS => {
            let cls_id = reader.read_u32();
            let type_params = decode_bytecode_type_array(reader);
            BytecodeType::Class((cls_id as usize).into(), type_params)
        }
        opc::BYTECODE_TYPE_STRUCT => {
            let struct_id = reader.read_u32();
            let type_params = decode_bytecode_type_array(reader);
            BytecodeType::Struct((struct_id as usize).into(), type_params)
        }
        opc::BYTECODE_TYPE_ENUM => {
            let enum_id = reader.read_u32();
            let type_params = decode_bytecode_type_array(reader);
            BytecodeType::Enum((enum_id as usize).into(), type_params)
        }
        opc::BYTECODE_TYPE_TRAIT_OBJECT => {
            let trait_id = reader.read_u32();
            let type_params = decode_bytecode_type_array(reader);
            let bindings = decode_bytecode_type_array(reader);
            BytecodeType::TraitObject((trait_id as usize).into(), type_params, bindings)
        }
        opc::BYTECODE_TYPE_TYPE_PARAM => {
            let id = reader.read_u32();
            BytecodeType::TypeParam(id)
        }
        opc::BYTECODE_TYPE_TUPLE => {
            let type_params = decode_bytecode_type_array(reader);
            BytecodeType::Tuple(type_params)
        }

        opc::BYTECODE_TYPE_LAMBDA => {
            let params = decode_bytecode_type_array(reader);
            let return_ty = decode_bytecode_type(reader);
            BytecodeType::Lambda(params, Box::new(return_ty))
        }

        opc::BYTECODE_TYPE_ASSOC => {
            let ty = decode_bytecode_type(reader);
            let trait_ty = decode_bytecode_trait_ty(reader);
            let assoc_id = (reader.read_u32() as usize).into();
            BytecodeType::Assoc {
                ty: Box::new(ty),
                trait_ty,
                assoc_id,
            }
        }

        opc::BYTECODE_TYPE_TYPE_ALIAS => unreachable!(),
        opc::BYTECODE_TYPE_REF => {
            let inner = decode_bytecode_type(reader);
            BytecodeType::Ref(Box::new(inner))
        }
        _ => panic!("unknown bytecode type kind: {kind}"),
    }
}

fn decode_bytecode_trait_ty(reader: &mut ByteReader) -> BytecodeTraitType {
    let trait_id: TraitId = (reader.read_u32() as usize).into();
    let type_params = decode_bytecode_type_array(reader);
    let length = reader.read_u32() as usize;
    let mut bindings = Vec::with_capacity(length);

    for _ in 0..length {
        let alias_id = (reader.read_u32() as usize).into();
        let ty = decode_bytecode_type(reader);
        bindings.push((alias_id, ty));
    }

    BytecodeTraitType {
        trait_id,
        type_params,
        bindings,
    }
}

pub fn decode_bytecode_type_array(reader: &mut ByteReader) -> BytecodeTypeArray {
    let length = reader.read_u32() as usize;
    let mut types = Vec::with_capacity(length);

    for _ in 0..length {
        let ty = decode_bytecode_type(reader);
        types.push(ty);
    }

    BytecodeTypeArray::new(types)
}
