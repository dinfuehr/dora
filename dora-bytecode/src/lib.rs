pub mod builder;
pub mod data;
pub mod dumper;
pub mod program;
pub mod reader;
pub mod ty;
pub mod writer;

#[cfg(test)]
mod tests;

pub use builder::*;
pub use data::*;
pub use dumper::{dump, dump_stdout};
pub use program::{
    AliasData, AliasId, ClassData, ClassField, ClassId, EnumData, EnumId, EnumVariant,
    ExtensionData, ExtensionId, FunctionData, FunctionId, FunctionKind, GlobalData, GlobalId,
    ImplData, ImplId, Intrinsic, ModuleData, ModuleId, PackageData, PackageId, Program,
    SourceFileData, SourceFileId, StructData, StructField, StructId, TraitData, TraitId,
    TypeParamBound, TypeParamData,
};
pub use reader::*;
pub use ty::{BytecodeType, BytecodeTypeArray};
pub use writer::*;
