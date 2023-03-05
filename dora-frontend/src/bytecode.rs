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
pub use dumper::dump;
pub use program::{
    ClassData, ClassId, EnumData, EnumId, FunctionData, FunctionId, GlobalData, GlobalId,
    ModuleData, ModuleId, PackageData, PackageId, Program, StructData, StructField, StructId,
    TraitData, TraitId, TypeParamBound, TypeParamData,
};
pub use reader::*;
pub use ty::{BytecodeType, BytecodeTypeArray};
pub use writer::*;
