pub mod builder;
pub mod data;
pub mod program;
pub mod reader;
pub mod ty;
pub mod writer;

#[cfg(test)]
mod tests;

pub use builder::*;
pub use data::*;
pub use program::{
    ClassData, ClassField, ClassId, EnumData, EnumId, EnumVariant, FunctionData, FunctionId,
    FunctionKind, GlobalData, GlobalId, ImplData, ImplId, ModuleData, ModuleId, NativeFunction,
    PackageData, PackageId, Program, SourceFileData, SourceFileId, StructData, StructField,
    StructId, TraitData, TraitId, TypeParamBound, TypeParamData,
};
pub use reader::*;
pub use ty::{BytecodeType, BytecodeTypeArray};
pub use writer::*;
