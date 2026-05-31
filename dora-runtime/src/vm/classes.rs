use dora_bytecode::{BytecodeType, BytecodeTypeArray, ClassId, EnumId, FunctionId};

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
