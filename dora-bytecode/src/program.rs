use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

use crate::{BytecodeFunction, BytecodeTraitType, BytecodeType, Location};
use bincode::{Decode, Encode, de::Decoder, enc::Encoder};

#[repr(transparent)]
pub struct Id<T>(u32, PhantomData<T>);

impl<T> Id<T> {
    pub fn index(self) -> usize {
        self.0 as usize
    }

    pub fn index_as_u32(self) -> u32 {
        self.0
    }
}

impl<T> From<usize> for Id<T> {
    fn from(value: usize) -> Self {
        Id(value.try_into().expect("overflow"), PhantomData)
    }
}

impl<T> Copy for Id<T> {}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Id<T> {}

impl<T> Hash for Id<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T> fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Id").field(&self.0).finish()
    }
}

impl<T> Encode for Id<T> {
    fn encode<E: Encoder>(&self, encoder: &mut E) -> Result<(), bincode::error::EncodeError> {
        self.0.encode(encoder)
    }
}

impl<Context, T> Decode<Context> for Id<T> {
    fn decode<D: Decoder<Context = Context>>(
        decoder: &mut D,
    ) -> Result<Self, bincode::error::DecodeError> {
        Ok(Id(u32::decode(decoder)?, PhantomData))
    }
}

impl<'de, Context, T> bincode::BorrowDecode<'de, Context> for Id<T> {
    fn borrow_decode<D: bincode::de::BorrowDecoder<'de, Context = Context>>(
        decoder: &mut D,
    ) -> Result<Self, bincode::error::DecodeError> {
        Ok(Id(
            bincode::BorrowDecode::borrow_decode(decoder)?,
            PhantomData,
        ))
    }
}

// Type aliases
pub type PackageId = Id<PackageData>;
pub type ModuleId = Id<ModuleData>;
pub type FunctionId = Id<FunctionData>;
pub type GlobalId = Id<GlobalData>;
pub type ClassId = Id<ClassData>;
pub type StructId = Id<StructData>;
pub type EnumId = Id<EnumData>;
pub type TraitId = Id<TraitData>;
pub type SourceFileId = Id<SourceFileData>;
pub type ExtensionId = Id<ExtensionData>;
pub type ImplId = Id<ImplData>;
pub type AliasId = Id<AliasData>;
pub type ConstId = Id<ConstData>;

#[derive(Debug, Decode, Encode)]
pub struct PackageData {
    pub name: String,
    pub root_module_id: ModuleId,
}

#[derive(Debug, Decode, Encode)]
pub struct ModuleData {
    pub name: String,
    pub parent_id: Option<ModuleId>,
    pub items: Vec<(String, ModuleElementId)>,
}

#[derive(Debug, Decode, Encode)]
pub struct FunctionData {
    pub name: String,
    pub file_id: SourceFileId,
    pub loc: Location,
    pub kind: FunctionKind,
    pub package_id: PackageId,
    pub module_id: ModuleId,
    pub type_params: TypeParamData,
    pub source_file_id: Option<SourceFileId>,
    pub params: Vec<BytecodeType>,
    pub return_type: BytecodeType,
    pub is_internal: bool,
    pub is_test: bool,
    pub is_optimize_immediately: bool,
    pub is_variadic: bool,
    pub is_force_inline: bool,
    pub is_never_inline: bool,
    pub is_trait_object_ignore: bool,
    pub bytecode: Option<BytecodeFunction>,
    pub trait_method_impl: Option<FunctionId>,
}

#[derive(Debug, Decode, Encode)]
pub enum FunctionKind {
    Impl(ImplId),
    Lambda,
    Trait(TraitId),
    Extension(ExtensionId),
    Function,
}

#[derive(Debug, Decode, Encode)]
pub struct GlobalData {
    pub module_id: ModuleId,
    pub ty: BytecodeType,
    pub mutable: bool,
    pub name: String,
    pub initial_value: Option<FunctionId>,
}

#[derive(Debug, Decode, Encode)]
pub struct ClassData {
    pub module_id: ModuleId,
    pub name: String,
    pub type_params: TypeParamData,
    pub fields: Vec<ClassField>,
}

#[derive(Debug, Decode, Encode)]
pub struct ClassField {
    pub ty: BytecodeType,
    pub name: Option<String>,
}

#[derive(Debug, Decode, Encode)]
pub struct StructData {
    pub module_id: ModuleId,
    pub name: String,
    pub type_params: TypeParamData,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Decode, Encode)]
pub struct StructField {
    pub ty: BytecodeType,
    pub name: Option<String>,
}

#[derive(Debug, Decode, Encode)]
pub struct TypeParamData {
    pub names: Vec<String>,
    pub container_count: usize,
    pub bounds: Vec<TypeParamBound>,
}

impl TypeParamData {
    pub fn type_param_count(&self) -> usize {
        self.names.len()
    }
}

#[derive(Debug, Decode, Encode)]
pub struct TypeParamBound {
    pub ty: BytecodeType,
    pub trait_ty: BytecodeTraitType,
}

#[derive(Debug, Decode, Encode)]
pub struct EnumData {
    pub module_id: ModuleId,
    pub name: String,
    pub type_params: TypeParamData,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Decode, Encode)]
pub struct EnumVariant {
    pub name: String,
    pub arguments: Vec<BytecodeType>,
}

#[derive(Debug, Decode, Encode)]
pub struct TraitData {
    pub module_id: ModuleId,
    pub name: String,
    pub type_params: TypeParamData,
    pub methods: Vec<FunctionId>,
    pub virtual_methods: Vec<FunctionId>,
}

#[derive(Debug, Decode, Encode)]
pub struct SourceFileData {
    pub path: String,
}

#[derive(Debug, Decode, Encode)]
pub struct ExtensionData {
    pub module_id: ModuleId,
    pub type_params: TypeParamData,
    pub extended_ty: BytecodeType,
    pub methods: Vec<FunctionId>,
}

#[derive(Debug, Decode, Encode)]
pub struct ImplData {
    pub module_id: ModuleId,
    pub type_params: TypeParamData,
    pub trait_ty: BytecodeTraitType,
    pub extended_ty: BytecodeType,
    pub methods: Vec<FunctionId>,
    pub trait_method_map: Vec<(FunctionId, FunctionId)>,
    pub trait_alias_map: Vec<(AliasId, AliasId)>,
}

#[derive(Debug, Decode, Encode)]
pub struct AliasData {
    pub name: String,
    pub ty: Option<BytecodeType>,
    pub idx_in_trait: Option<usize>,
}

impl AliasData {
    pub fn idx_in_trait(&self) -> usize {
        self.idx_in_trait.expect("missing idx")
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Decode, Encode)]
pub enum ModuleElementId {
    Alias(AliasId),
    Class(ClassId),
    Struct(StructId),
    Enum(EnumId),
    EnumVariant(EnumId, u32),
    Trait(TraitId),
    Module(ModuleId),
    Function(FunctionId),
    Global(GlobalId),
    Const(ConstId),
}

impl ModuleElementId {
    pub fn module_id(&self) -> Option<ModuleId> {
        match self {
            ModuleElementId::Module(id) => Some(*id),
            _ => None,
        }
    }

    pub fn function_id(&self) -> Option<FunctionId> {
        match self {
            ModuleElementId::Function(id) => Some(*id),
            _ => None,
        }
    }

    pub fn class_id(&self) -> Option<ClassId> {
        match self {
            ModuleElementId::Class(id) => Some(*id),
            _ => None,
        }
    }

    pub fn struct_id(&self) -> Option<StructId> {
        match self {
            ModuleElementId::Struct(id) => Some(*id),
            _ => None,
        }
    }

    pub fn trait_id(&self) -> Option<TraitId> {
        match self {
            ModuleElementId::Trait(id) => Some(*id),
            _ => None,
        }
    }
}

#[derive(Debug, Decode, Encode)]
pub struct ConstData {
    pub module_id: ModuleId,
    pub name: String,
    pub ty: BytecodeType,
    pub value: ConstValue,
}

#[derive(Clone, Debug, PartialEq, Decode, Encode)]
pub enum ConstValue {
    None,
    Bool(bool),
    Char(char),
    Int(i64),
    Float(f64),
    String(String),
}

impl ConstValue {
    pub fn to_bool(&self) -> bool {
        match self {
            &ConstValue::Bool(b) => b,
            _ => unreachable!(),
        }
    }

    pub fn to_char(&self) -> char {
        match self {
            &ConstValue::Char(c) => c,
            _ => unreachable!(),
        }
    }

    pub fn to_i64(&self) -> Option<i64> {
        match self {
            &ConstValue::Int(i) => Some(i),
            _ => None,
        }
    }

    pub fn to_f64(&self) -> Option<f64> {
        match self {
            &ConstValue::Float(f) => Some(f),
            _ => None,
        }
    }

    pub fn to_string(&self) -> Option<&String> {
        match self {
            &ConstValue::String(ref v) => Some(v),
            _ => None,
        }
    }
}

#[derive(Debug, Decode, Encode)]
pub struct Program {
    pub packages: Vec<PackageData>,
    pub modules: Vec<ModuleData>,
    pub functions: Vec<FunctionData>,
    pub globals: Vec<GlobalData>,
    pub consts: Vec<ConstData>,
    pub classes: Vec<ClassData>,
    pub structs: Vec<StructData>,
    pub enums: Vec<EnumData>,
    pub traits: Vec<TraitData>,
    pub impls: Vec<ImplData>,
    pub extensions: Vec<ExtensionData>,
    pub aliases: Vec<AliasData>,
    pub source_files: Vec<SourceFileData>,
    pub stdlib_package_id: PackageId,
    pub program_package_id: PackageId,
    pub boots_package_id: Option<PackageId>,
    pub main_fct_id: Option<FunctionId>,
}

impl Program {
    pub fn fct(&self, id: FunctionId) -> &FunctionData {
        &self.functions[id.index()]
    }

    pub fn trait_(&self, id: TraitId) -> &TraitData {
        &self.traits[id.index()]
    }

    pub fn extension(&self, id: ExtensionId) -> &ExtensionData {
        &self.extensions[id.index()]
    }

    pub fn alias(&self, id: AliasId) -> &AliasData {
        &self.aliases[id.index()]
    }

    pub fn struct_(&self, id: StructId) -> &StructData {
        &self.structs[id.index()]
    }

    pub fn class(&self, id: ClassId) -> &ClassData {
        &self.classes[id.index()]
    }

    pub fn enum_(&self, id: EnumId) -> &EnumData {
        &self.enums[id.index()]
    }

    pub fn impl_(&self, id: ImplId) -> &ImplData {
        &self.impls[id.index()]
    }

    pub fn module(&self, id: ModuleId) -> &ModuleData {
        &self.modules[id.index()]
    }

    pub fn global(&self, id: GlobalId) -> &GlobalData {
        &self.globals[id.index()]
    }

    pub fn const_(&self, id: ConstId) -> &ConstData {
        &self.consts[id.index()]
    }

    pub fn file(&self, id: SourceFileId) -> &SourceFileData {
        &self.source_files[id.index()]
    }

    pub fn package(&self, id: PackageId) -> &PackageData {
        &self.packages[id.index()]
    }

    pub fn program_module_id(&self) -> ModuleId {
        self.package(self.program_package_id).root_module_id
    }
}
