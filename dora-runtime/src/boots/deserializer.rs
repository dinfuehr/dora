use crate::boots::data::{LazyCompilationSiteKind, RelocationKindKind};
use crate::compiler::codegen::SpecializeSelf;
use crate::gc::Address;
use crate::vm::{
    CodeDescriptor, CommentTable, GcPoint, GcPointTable, InlinedFunction, InlinedFunctionId,
    InlinedLocation, LazyCompilationData, LazyCompilationSite, LocationTable, RelocationKind,
    RelocationTable,
};
use dora_bytecode::{
    AliasId, BytecodeTraitType, BytecodeType, BytecodeTypeArray, BytecodeTypeKind, ClassId, EnumId,
    FunctionId, ImplId, Location, StructId, TraitId,
};

pub fn decode_code_descriptor(reader: &mut ByteReader) -> CodeDescriptor {
    let code = decode_code(reader);
    let lazy_compilation = decode_lazy_compilation(reader);
    let gcpoints = decode_gcpoint_table(reader);
    let positions = decode_location_table(reader);
    let comments = decode_comment_table(reader);
    let relocations = decode_relocation_table(reader);
    let inlined_functions = decode_inlined_function_table(reader);
    CodeDescriptor {
        code,
        comments,
        lazy_compilation,
        gcpoints,
        positions,
        relocations,
        inlined_functions,
    }
}

fn decode_code(reader: &mut ByteReader) -> Vec<u8> {
    decode_array_u8(reader)
}

fn decode_location_table(reader: &mut ByteReader) -> LocationTable {
    let length = reader.read_u32() as usize;
    let mut result = LocationTable::new();

    for _ in 0..length {
        let pos = reader.read_u32();
        let inlined_location = decode_inlined_location(reader);
        result.insert(pos, inlined_location);
    }

    result
}

fn decode_relocation_table(reader: &mut ByteReader) -> RelocationTable {
    let length = reader.read_u32() as usize;
    let mut result = RelocationTable::new();

    for _ in 0..length {
        let pos = reader.read_u32();
        let kind = decode_relocation_kind(reader);
        result.insert(pos, kind);
    }

    result
}

fn decode_relocation_kind(reader: &mut ByteReader) -> RelocationKind {
    let kind = RelocationKindKind::try_from(reader.read_u8()).expect("wrong kind");

    match kind {
        RelocationKindKind::JumpTableEntry => {
            let target = reader.read_u32();
            RelocationKind::JumpTableEntry(target)
        }

        RelocationKindKind::Code | RelocationKindKind::TargetObject => unreachable!(),
    }
}

fn decode_inlined_location(reader: &mut ByteReader) -> InlinedLocation {
    let inlined_function_id = if reader.read_bool() {
        Some(InlinedFunctionId(reader.read_u32()))
    } else {
        None
    };
    let line = reader.read_u32();
    let col = reader.read_u32();
    InlinedLocation {
        inlined_function_id,
        location: Location::new(line, col),
    }
}

fn decode_inlined_function_table(reader: &mut ByteReader) -> Vec<InlinedFunction> {
    let length = reader.read_u32() as usize;
    let mut result = Vec::new();
    result.reserve(length);

    for _ in 0..length {
        let inlined = decode_inlined_function(reader);
        result.push(inlined);
    }

    result
}

fn decode_inlined_function(reader: &mut ByteReader) -> InlinedFunction {
    let fct_id = FunctionId(reader.read_u32());
    let type_params = decode_bytecode_type_array(reader);
    let location = decode_inlined_location(reader);

    InlinedFunction {
        fct_id,
        type_params,
        inlined_location: location,
    }
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
    let mut result = LazyCompilationData::new();
    let length = reader.read_u32() as usize;

    for _ in 0..length {
        let offset = reader.read_u32();
        let comment = decode_lazy_compilation_site(reader);
        result.insert(offset, comment)
    }

    result
}

fn decode_lazy_compilation_site(reader: &mut ByteReader) -> LazyCompilationSite {
    let kind = LazyCompilationSiteKind::try_from(reader.read_u8()).expect("wrong kind");

    match kind {
        LazyCompilationSiteKind::Direct => {
            let fct_id = FunctionId(reader.read_u32());
            let type_params = decode_bytecode_type_array(reader);
            let const_pool_offset = reader.read_u32() as i32;
            LazyCompilationSite::Direct {
                fct_id,
                type_params,
                const_pool_offset_from_ra: const_pool_offset,
            }
        }
        LazyCompilationSiteKind::Virtual => {
            let receiver_is_first = reader.read_bool();
            let trait_object_ty = decode_bytecode_type(reader);
            let vtable_index = reader.read_u32();
            LazyCompilationSite::Virtual {
                receiver_is_first,
                trait_object_ty,
                vtable_index,
            }
        }
        LazyCompilationSiteKind::Lambda => {
            let receiver_is_first = reader.read_bool();
            let params = decode_bytecode_type_array(reader);
            let return_type = decode_bytecode_type(reader);
            LazyCompilationSite::Lambda {
                receiver_is_first,
                params,
                return_type,
            }
        }
    }
}

pub fn decode_bytecode_type(reader: &mut ByteReader) -> BytecodeType {
    let kind = BytecodeTypeKind::try_from(reader.read_u8()).expect("wrong kind");

    match kind {
        BytecodeTypeKind::Unit => BytecodeType::Unit,
        BytecodeTypeKind::Bool => BytecodeType::Bool,
        BytecodeTypeKind::UInt8 => BytecodeType::UInt8,
        BytecodeTypeKind::Char => BytecodeType::Char,
        BytecodeTypeKind::Int32 => BytecodeType::Int32,
        BytecodeTypeKind::Int64 => BytecodeType::Int64,
        BytecodeTypeKind::Float32 => BytecodeType::Float32,
        BytecodeTypeKind::Float64 => BytecodeType::Float64,
        BytecodeTypeKind::Ptr => BytecodeType::Ptr,
        BytecodeTypeKind::This => BytecodeType::This,
        BytecodeTypeKind::Class => {
            let cls_id = reader.read_u32();
            let type_params = decode_bytecode_type_array(reader);
            BytecodeType::Class(ClassId(cls_id), type_params)
        }
        BytecodeTypeKind::Struct => {
            let struct_id = reader.read_u32();
            let type_params = decode_bytecode_type_array(reader);
            BytecodeType::Struct(StructId(struct_id), type_params)
        }
        BytecodeTypeKind::Enum => {
            let enum_id = reader.read_u32();
            let type_params = decode_bytecode_type_array(reader);
            BytecodeType::Enum(EnumId(enum_id), type_params)
        }
        BytecodeTypeKind::TraitObject => {
            let trait_id = reader.read_u32();
            let type_params = decode_bytecode_type_array(reader);
            let bindings = decode_bytecode_type_array(reader);
            BytecodeType::TraitObject(TraitId(trait_id), type_params, bindings)
        }
        BytecodeTypeKind::TypeParam => {
            let id = reader.read_u32();
            BytecodeType::TypeParam(id)
        }
        BytecodeTypeKind::Tuple => {
            let type_params = decode_bytecode_type_array(reader);
            BytecodeType::Tuple(type_params)
        }

        BytecodeTypeKind::Lambda => {
            let params = decode_bytecode_type_array(reader);
            let return_ty = decode_bytecode_type(reader);
            BytecodeType::Lambda(params, Box::new(return_ty))
        }

        BytecodeTypeKind::GenericAssoc => {
            let type_param_id = reader.read_u32();
            let trait_ty = decode_bytecode_trait_ty(reader);
            let assoc_id = AliasId(reader.read_u32());
            BytecodeType::GenericAssoc {
                type_param_id,
                trait_ty,
                assoc_id,
            }
        }

        BytecodeTypeKind::Assoc => {
            let trait_ty = decode_bytecode_trait_ty(reader);
            let assoc_id = AliasId(reader.read_u32());
            BytecodeType::Assoc { trait_ty, assoc_id }
        }

        BytecodeTypeKind::TypeAlias => unreachable!(),
    }
}

pub fn decode_bytecode_trait_ty(reader: &mut ByteReader) -> BytecodeTraitType {
    let trait_id = TraitId(reader.read_u32());
    let type_params = decode_bytecode_type_array(reader);
    let length = reader.read_u32() as usize;
    let mut bindings = Vec::with_capacity(length);

    for _ in 0..length {
        let alias_id = AliasId(reader.read_u32());
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

pub fn decode_specialize_self(reader: &mut ByteReader) -> Option<SpecializeSelf> {
    if reader.read_bool() {
        let impl_id = ImplId(reader.read_u32());
        let container_type_params = reader.read_u32() as usize;
        let trait_ty = decode_bytecode_trait_ty(reader);
        let extended_ty = decode_bytecode_type(reader);
        Some(SpecializeSelf {
            impl_id,
            container_type_params,
            trait_ty,
            extended_ty,
        })
    } else {
        None
    }
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

    pub fn read_address(&mut self) -> Address {
        (self.read_u64() as usize).into()
    }

    pub fn has_more(&self) -> bool {
        self.idx < self.data.len()
    }
}
