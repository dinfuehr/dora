use std::collections::HashMap;

use dora_bytecode::{
    BytecodeType, BytecodeTypeArray, ClassId, EnumId, FunctionId, GlobalId, Location,
};

use crate::AotShapeKey;
use crate::FieldInstance;
use crate::RelocationForm;
use crate::wire::{ByteBuffer, encode_bytecode_type, encode_bytecode_type_array};

pub const AOT_SHAPE_KIND_FILLER_WORD: u8 = 0;
pub const AOT_SHAPE_KIND_STRING: u8 = 1;
pub const AOT_SHAPE_KIND_CLASS: u8 = 2;
pub const AOT_SHAPE_KIND_ARRAY: u8 = 3;
pub const AOT_SHAPE_KIND_ENUM_VARIANT: u8 = 4;
pub const AOT_SHAPE_KIND_LAMBDA: u8 = 5;
pub const AOT_SHAPE_KIND_TRAIT_OBJECT: u8 = 6;
pub const AOT_SHAPE_KIND_FILLER_ARRAY: u8 = 7;
pub const AOT_SHAPE_KIND_FREE_SPACE: u8 = 8;
pub const AOT_SHAPE_KIND_CODE: u8 = 9;

// Stored in the runtime Shape::visitor word and decoded from .dora.shapes.
pub const AOT_SHAPE_VISITOR_REGULAR: usize = 0;
pub const AOT_SHAPE_VISITOR_POINTER_ARRAY: usize = 1;
pub const AOT_SHAPE_VISITOR_RECORD_ARRAY: usize = 2;
pub const AOT_SHAPE_VISITOR_NONE: usize = 3;
pub const AOT_SHAPE_VISITOR_INVALID: usize = 4;

pub const AOT_CODE_KIND_OPTIMIZED: u32 = 0;
pub const AOT_CODE_KIND_RUNTIME_ENTRY_TRAMPOLINE: u32 = 1;
pub const AOT_CODE_KIND_DORA_ENTRY_TRAMPOLINE: u32 = 2;
pub const AOT_CODE_KIND_ALLOCATION_FAILURE_TRAMPOLINE: u32 = 3;
pub const AOT_CODE_KIND_TRAP_TRAMPOLINE: u32 = 4;
pub const AOT_CODE_KIND_SAFEPOINT_TRAMPOLINE: u32 = 5;
pub const AOT_CODE_KIND_UNREACHABLE_TRAMPOLINE: u32 = 6;
pub const AOT_CODE_KIND_FATAL_ERROR_TRAMPOLINE: u32 = 7;
pub const AOT_CODE_KIND_STACK_OVERFLOW_TRAMPOLINE: u32 = 8;

#[derive(Clone, Debug)]
pub enum ShapeKind {
    Array(ClassId, BytecodeTypeArray),
    Class(ClassId, BytecodeTypeArray),
    String,
    FillerWord,
    FillerArray,
    FreeSpace,
    Code,
    Lambda(FunctionId, BytecodeTypeArray),
    TraitObject {
        trait_ty: BytecodeType,
        actual_object_ty: BytecodeType,
    },
    EnumVariant(EnumId, BytecodeTypeArray, u32),
}

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum ShapeVisitor {
    Regular,
    PointerArray,
    RecordArray,
    None,
    Invalid,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TargetArch {
    X64,
    Arm64,
}

impl TargetArch {
    pub fn host() -> TargetArch {
        if cfg!(target_arch = "x86_64") {
            TargetArch::X64
        } else if cfg!(target_arch = "aarch64") {
            TargetArch::Arm64
        } else {
            panic!("unsupported host architecture")
        }
    }

    pub fn is_arm64(self) -> bool {
        self == TargetArch::Arm64
    }
}

pub fn parse_target_arch(s: &str) -> Result<TargetArch, String> {
    match s {
        "x64" | "x86_64" | "x86-64" => Ok(TargetArch::X64),
        "arm64" | "aarch64" => Ok(TargetArch::Arm64),
        _ => Err(format!("unknown target '{}', expected: x64, arm64", s)),
    }
}

#[derive(Copy, Clone, Debug)]
pub enum CollectorName {
    Zero,
    Copy,
    Sweep,
    Swiper,
}

pub fn parse_collector(s: &str) -> Result<CollectorName, String> {
    match s {
        "zero" => Ok(CollectorName::Zero),
        "copy" => Ok(CollectorName::Copy),
        "sweep" => Ok(CollectorName::Sweep),
        "swiper" => Ok(CollectorName::Swiper),
        _ => Err(format!(
            "unknown collector '{}', expected: zero, copy, sweep, swiper",
            s
        )),
    }
}

pub fn encode_shape_kind(kind: &ShapeKind) -> Vec<u8> {
    let mut buffer = ByteBuffer::new();

    match kind {
        ShapeKind::FillerWord => buffer.emit_u8(AOT_SHAPE_KIND_FILLER_WORD),
        ShapeKind::FillerArray => buffer.emit_u8(AOT_SHAPE_KIND_FILLER_ARRAY),
        ShapeKind::FreeSpace => buffer.emit_u8(AOT_SHAPE_KIND_FREE_SPACE),
        ShapeKind::Code => buffer.emit_u8(AOT_SHAPE_KIND_CODE),
        ShapeKind::String => buffer.emit_u8(AOT_SHAPE_KIND_STRING),
        ShapeKind::Class(class_id, type_params) => {
            buffer.emit_u8(AOT_SHAPE_KIND_CLASS);
            buffer.emit_id(class_id.index());
            encode_bytecode_type_array(type_params, &mut buffer);
        }
        ShapeKind::Array(class_id, type_params) => {
            buffer.emit_u8(AOT_SHAPE_KIND_ARRAY);
            buffer.emit_id(class_id.index());
            encode_bytecode_type_array(type_params, &mut buffer);
        }
        ShapeKind::EnumVariant(enum_id, type_params, variant_id) => {
            buffer.emit_u8(AOT_SHAPE_KIND_ENUM_VARIANT);
            buffer.emit_id(enum_id.index());
            encode_bytecode_type_array(type_params, &mut buffer);
            buffer.emit_u32(*variant_id);
        }
        ShapeKind::Lambda(fct_id, type_params) => {
            buffer.emit_u8(AOT_SHAPE_KIND_LAMBDA);
            buffer.emit_id(fct_id.index());
            encode_bytecode_type_array(type_params, &mut buffer);
        }
        ShapeKind::TraitObject {
            trait_ty,
            actual_object_ty,
        } => {
            buffer.emit_u8(AOT_SHAPE_KIND_TRAIT_OBJECT);
            encode_bytecode_type(trait_ty, &mut buffer);
            encode_bytecode_type(actual_object_ty, &mut buffer);
        }
    }

    buffer.data().to_vec()
}

pub fn encode_shape_fields(fields: &[FieldInstance]) -> Vec<u8> {
    let mut buffer = ByteBuffer::new();
    buffer.emit_u32(fields.len() as u32);

    for field in fields {
        buffer.emit_u32(field.offset as u32);
        encode_bytecode_type(&field.ty, &mut buffer);
    }

    buffer.data().to_vec()
}

pub struct AotRelocation {
    /// Offset of the relocatable instruction or instruction sequence.
    pub offset: u32,
    pub target: AotRelocationTarget,
    pub form: RelocationForm,
}

pub enum AotRelocationTarget {
    /// Final symbol name of the call target.
    Call(String),
    /// Interned UTF-8 string payload referenced through an AOT string slot.
    StringSlot(AotStringId),
    ShapeSlot(AotShapeId),
    Global(AotGlobalRelocationTarget),
}

#[derive(Clone)]
pub enum AotCodeKind {
    Optimized,
    RuntimeEntryTrampoline { native_target: String },
    AllocationFailureTrampoline,
    TrapTrampoline,
    SafepointTrampoline,
    UnreachableTrampoline,
    FatalErrorTrampoline,
    StackOverflowTrampoline,
    DoraEntryTrampoline,
}

pub struct AotGcPoint {
    pub pc_offset: u32,
    pub offsets: Vec<i32>,
}

#[derive(Clone)]
pub struct AotLocation {
    pub pc_offset: u32,
    pub inlined_function_id: Option<u32>,
    pub line: u32,
    pub column: u32,
}

pub struct AotFunctionInfo {
    pub name: AotStringId,
    pub file: AotStringId,
    pub loc: Location,
}

pub struct AotInlinedFunction {
    pub function: AotFunctionInfo,
    pub inlined_function_id: Option<u32>,
    pub line: u32,
    pub column: u32,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct AotStringId(u32);

impl AotStringId {
    pub fn index(self) -> u32 {
        self.0
    }
}

#[derive(Clone)]
pub struct AotStringTable {
    entries: Vec<String>,
    map: HashMap<String, AotStringId>,
}

impl AotStringTable {
    pub fn new() -> AotStringTable {
        AotStringTable {
            entries: Vec::new(),
            map: HashMap::new(),
        }
    }

    pub fn intern(&mut self, value: &str) -> AotStringId {
        if let Some(&id) = self.map.get(value) {
            return id;
        }

        let id = AotStringId(
            u32::try_from(self.entries.len()).expect("too many strings in AOT string table"),
        );
        let value = value.to_string();
        self.entries.push(value.clone());
        self.map.insert(value, id);
        id
    }

    pub fn entries(&self) -> &[String] {
        &self.entries
    }
}

#[derive(Clone, Copy)]
pub enum AotGlobalRelocationTarget {
    State(GlobalId),
    Value(GlobalId),
}

pub struct AotFunction {
    pub symbol_name: String,
    pub fct_id: u32,
    pub function: AotFunctionInfo,
    pub kind: AotCodeKind,
    pub code: Vec<u8>,
    pub relocations: Vec<AotRelocation>,
    pub gcpoints: Vec<AotGcPoint>,
    pub locations: Vec<AotLocation>,
    pub inlined_functions: Vec<AotInlinedFunction>,
}

pub struct AotTestFunction {
    pub symbol_name: String,
    pub fct_id: u32,
}

pub struct AotShape {
    pub id: u32,
    pub kind: ShapeKind,
    pub fields: Vec<u8>,
    pub visitor: ShapeVisitor,
    pub refs: Vec<i32>,
    pub instance_size: u64,
    pub element_size: u64,
    pub vtable_entries: Vec<Option<String>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum AotKnownShapeKind {
    ByteArray,
    Int32Array,
    String,
    Thread,
    FillerWord,
    FillerArray,
    FreeSpace,
    Code,
}

pub struct AotKnownShape {
    pub kind: AotKnownShapeKind,
    pub shape_id: AotShapeId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct AotShapeId(pub(crate) u32);

impl AotShapeId {
    pub fn index(self) -> u32 {
        self.0
    }
}

#[derive(Default)]
pub struct AotShapeInterner {
    keys: Vec<AotShapeKey>,
    ids: HashMap<AotShapeKey, AotShapeId>,
}

impl AotShapeInterner {
    pub fn intern(&mut self, key: AotShapeKey) -> AotShapeId {
        if let Some(&id) = self.ids.get(&key) {
            return id;
        }

        let id =
            AotShapeId(u32::try_from(self.keys.len()).expect("too many shapes in AOT shape table"));
        self.keys.push(key.clone());
        self.ids.insert(key, id);
        id
    }

    pub fn get(&self, key: &AotShapeKey) -> AotShapeId {
        *self.ids.get(key).expect("missing AOT shape key")
    }

    pub fn keys(&self) -> &[AotShapeKey] {
        &self.keys
    }
}

pub struct GlobalLayoutEntry {
    pub state_offset: usize,
    pub value_offset: usize,
}

pub struct GlobalLayout {
    pub memory_size: usize,
    pub references: Vec<i32>,
    pub globals: Vec<GlobalLayoutEntry>,
}

pub struct AotCompilation {
    pub strings: AotStringTable,
    pub functions: Vec<AotFunction>,
    pub shapes: Vec<AotShape>,
    pub known_shapes: Vec<AotKnownShape>,
    pub global_layout: GlobalLayout,
    pub collector_name: CollectorName,
    pub test_functions: Vec<AotTestFunction>,
}
