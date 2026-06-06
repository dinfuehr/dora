use dora_bytecode::opcode as opc;
use dora_bytecode::{BytecodeTraitType, ConstPoolIdx, Location, TraitId};
use dora_compiler::wire::{ByteReader, decode_bytecode_type, decode_bytecode_type_array};
use dora_compiler::{
    AotShapeKey, Arm64LoadWidth, CodeDescriptor, CommentTable, GcPoint, GcPointTable,
    InlinedFunction, InlinedFunctionId, InlinedLocation, LocationTable, RelocationForm,
    RelocationKind, RelocationTable, RuntimeFunction, SpecializeSelf,
};

pub fn decode_code_descriptor(reader: &mut ByteReader) -> CodeDescriptor {
    let code = decode_code(reader);
    let gcpoints = decode_gcpoint_table(reader);
    let positions = decode_location_table(reader);
    let comments = decode_comment_table(reader);
    let relocations = decode_relocation_table(reader);
    let inlined_functions = decode_inlined_function_table(reader);
    CodeDescriptor {
        code,
        comments,
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
        let form = decode_relocation_form(reader);
        result.insert(pos, kind, form);
    }

    result
}

fn decode_relocation_form(reader: &mut ByteReader) -> RelocationForm {
    let form = reader.read_u8();

    match form {
        opc::RELOCATION_FORM_ABSOLUTE_ADDRESS => RelocationForm::AbsoluteAddress,

        opc::RELOCATION_FORM_X64_CALL_REL32 => RelocationForm::X64CallRel32,

        opc::RELOCATION_FORM_X64_RIP_RELATIVE_LOAD64 => RelocationForm::X64RipRelativeLoad64 {
            disp_offset: reader.read_u8(),
            dst_reg: reader.read_u8(),
        },

        opc::RELOCATION_FORM_X64_RIP_RELATIVE_LOAD32 => RelocationForm::X64RipRelativeLoad32 {
            disp_offset: reader.read_u8(),
            dst_reg: reader.read_u8(),
        },

        opc::RELOCATION_FORM_X64_RIP_RELATIVE_LEA => RelocationForm::X64RipRelativeLea {
            disp_offset: reader.read_u8(),
            dst_reg: reader.read_u8(),
        },

        opc::RELOCATION_FORM_ARM64_BRANCH26 => RelocationForm::Arm64Branch26,

        opc::RELOCATION_FORM_ARM64_ADRP_LDR => RelocationForm::Arm64AdrpLdr {
            page_reg: reader.read_u8(),
            base_reg: reader.read_u8(),
            dst_reg: reader.read_u8(),
            width: decode_arm64_load_width(reader),
        },

        opc::RELOCATION_FORM_ARM64_ADRP_ADD => RelocationForm::Arm64AdrpAdd {
            page_reg: reader.read_u8(),
            base_reg: reader.read_u8(),
            dst_reg: reader.read_u8(),
        },

        _ => panic!("wrong relocation form"),
    }
}

fn decode_arm64_load_width(reader: &mut ByteReader) -> Arm64LoadWidth {
    match reader.read_u8() {
        opc::ARM64_LOAD_WIDTH_U32 => Arm64LoadWidth::U32,
        opc::ARM64_LOAD_WIDTH_U64 => Arm64LoadWidth::U64,
        _ => panic!("wrong arm64 load width"),
    }
}

fn decode_relocation_kind(reader: &mut ByteReader) -> RelocationKind {
    let kind = reader.read_u8();

    match kind {
        opc::RELOCATION_KIND_JUMP_TABLE_ENTRY => {
            let target = reader.read_u32();
            RelocationKind::JumpTableEntry(target)
        }
        opc::RELOCATION_KIND_STRING_CONST => {
            let owner_fct_id = (reader.read_u32() as usize).into();
            let const_pool_idx = ConstPoolIdx(reader.read_u32());
            RelocationKind::StringConst {
                owner_fct_id,
                const_pool_idx,
            }
        }
        opc::RELOCATION_KIND_RUNTIME_FUNCTION => {
            let runtime_function = decode_runtime_function(reader.read_u8());
            RelocationKind::RuntimeFunction(runtime_function)
        }

        opc::RELOCATION_KIND_SHAPE => RelocationKind::Shape {
            key: decode_aot_shape_key(reader),
        },

        opc::RELOCATION_KIND_GLOBAL_VALUE_ADDRESS => {
            let global_id = (reader.read_u32() as usize).into();
            RelocationKind::GlobalValueAddress { global_id }
        }

        opc::RELOCATION_KIND_GLOBAL_STATE_ADDRESS => {
            let global_id = (reader.read_u32() as usize).into();
            RelocationKind::GlobalStateAddress { global_id }
        }

        opc::RELOCATION_KIND_DIRECT_CALL => {
            let fct_id = (reader.read_u32() as usize).into();
            let type_params = decode_bytecode_type_array(reader);
            RelocationKind::DirectCall {
                fct_id,
                type_params,
            }
        }

        opc::RELOCATION_KIND_CODE | opc::RELOCATION_KIND_TARGET_OBJECT => unreachable!(),

        _ => panic!("wrong relocation kind"),
    }
}

fn decode_aot_shape_key(reader: &mut ByteReader) -> AotShapeKey {
    let kind = reader.read_u8();

    match kind {
        opc::AOT_SHAPE_KEY_FILLER_WORD => AotShapeKey::FillerWord,
        opc::AOT_SHAPE_KEY_STRING => AotShapeKey::String,
        opc::AOT_SHAPE_KEY_CLASS => {
            let class_id = (reader.read_u32() as usize).into();
            let type_params = decode_bytecode_type_array(reader);
            AotShapeKey::Class(class_id, type_params)
        }
        opc::AOT_SHAPE_KEY_ARRAY => {
            let class_id = (reader.read_u32() as usize).into();
            let type_params = decode_bytecode_type_array(reader);
            AotShapeKey::Array(class_id, type_params)
        }
        opc::AOT_SHAPE_KEY_ENUM_VARIANT => {
            let enum_id = (reader.read_u32() as usize).into();
            let type_params = decode_bytecode_type_array(reader);
            let variant_id = reader.read_u32();
            AotShapeKey::EnumVariant {
                enum_id,
                type_params,
                variant_id,
            }
        }
        opc::AOT_SHAPE_KEY_LAMBDA => {
            let fct_id = (reader.read_u32() as usize).into();
            let type_params = decode_bytecode_type_array(reader);
            AotShapeKey::Lambda(fct_id, type_params)
        }
        opc::AOT_SHAPE_KEY_TRAIT_OBJECT => {
            let trait_ty = decode_bytecode_type(reader);
            let actual_object_ty = decode_bytecode_type(reader);
            AotShapeKey::TraitObject {
                trait_ty,
                actual_object_ty,
            }
        }
        opc::AOT_SHAPE_KEY_FILLER_ARRAY => AotShapeKey::FillerArray,
        opc::AOT_SHAPE_KEY_FREE_SPACE => AotShapeKey::FreeSpace,
        opc::AOT_SHAPE_KEY_CODE => AotShapeKey::Code,
        _ => panic!("wrong AOT shape key kind"),
    }
}

fn decode_runtime_function(value: u8) -> RuntimeFunction {
    match value {
        opc::RUNTIME_FUNCTION_TRAP_TRAMPOLINE => RuntimeFunction::TrapTrampoline,
        opc::RUNTIME_FUNCTION_SAFEPOINT_TRAMPOLINE => RuntimeFunction::SafepointTrampoline,
        opc::RUNTIME_FUNCTION_ALLOCATION_SLOW_TRAMPOLINE => {
            RuntimeFunction::AllocationSlowTrampoline
        }
        opc::RUNTIME_FUNCTION_WRITE_BARRIER_SLOW_PATH => RuntimeFunction::WriteBarrierSlowPath,
        opc::RUNTIME_FUNCTION_UNREACHABLE_TRAMPOLINE => RuntimeFunction::UnreachableTrampoline,
        opc::RUNTIME_FUNCTION_FATAL_ERROR_TRAMPOLINE => RuntimeFunction::FatalErrorTrampoline,
        _ => panic!("wrong runtime function id {}", value),
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
    let fct_id = (reader.read_u32() as usize).into();
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

pub fn decode_bytecode_trait_ty(reader: &mut ByteReader) -> BytecodeTraitType {
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

pub fn decode_specialize_self(reader: &mut ByteReader) -> Option<SpecializeSelf> {
    if reader.read_bool() {
        let impl_id = (reader.read_u32() as usize).into();
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
