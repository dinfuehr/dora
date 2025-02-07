use crate::{BytecodeFunction, BytecodeTraitType, BytecodeType, Location};
use bincode::{Decode, Encode};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
pub struct PackageId(pub u32);

#[derive(Debug, Decode, Encode)]
pub struct PackageData {
    pub name: String,
    pub root_module_id: ModuleId,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Decode, Encode)]
pub struct ModuleId(pub u32);

#[derive(Debug, Decode, Encode)]
pub struct ModuleData {
    pub name: String,
    pub parent_id: Option<ModuleId>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
#[repr(C)]
pub struct FunctionId(pub u32);

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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
#[repr(C)]
pub struct GlobalId(pub u32);

#[derive(Debug, Decode, Encode)]
pub struct GlobalData {
    pub module_id: ModuleId,
    pub ty: BytecodeType,
    pub mutable: bool,
    pub name: String,
    pub initial_value: Option<FunctionId>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
pub struct ClassId(pub u32);

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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
#[repr(C)]
pub struct StructId(pub u32);

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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
#[repr(C)]
pub struct EnumId(pub u32);

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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
pub struct TraitId(pub u32);

#[derive(Debug, Decode, Encode)]
pub struct TraitData {
    pub module_id: ModuleId,
    pub name: String,
    pub type_params: TypeParamData,
    pub methods: Vec<FunctionId>,
    pub virtual_methods: Vec<FunctionId>,
}

#[derive(Copy, Clone, Debug, Decode, Encode)]
pub struct SourceFileId(pub u32);

#[derive(Debug, Decode, Encode)]
pub struct SourceFileData {
    pub path: String,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
pub struct ExtensionId(pub u32);

#[derive(Debug, Decode, Encode)]
pub struct ExtensionData {
    pub module_id: ModuleId,
    pub type_params: TypeParamData,
    pub extended_ty: BytecodeType,
    pub methods: Vec<FunctionId>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
pub struct ImplId(pub u32);

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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
pub struct AliasId(pub u32);

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

#[derive(Debug, Decode, Encode)]
pub struct Program {
    pub packages: Vec<PackageData>,
    pub modules: Vec<ModuleData>,
    pub functions: Vec<FunctionData>,
    pub globals: Vec<GlobalData>,
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
        &self.functions[id.0 as usize]
    }

    pub fn trait_(&self, id: TraitId) -> &TraitData {
        &self.traits[id.0 as usize]
    }

    pub fn extension(&self, id: ExtensionId) -> &ExtensionData {
        &self.extensions[id.0 as usize]
    }

    pub fn alias(&self, id: AliasId) -> &AliasData {
        &self.aliases[id.0 as usize]
    }

    pub fn struct_(&self, id: StructId) -> &StructData {
        &self.structs[id.0 as usize]
    }

    pub fn class(&self, id: ClassId) -> &ClassData {
        &self.classes[id.0 as usize]
    }

    pub fn enum_(&self, id: EnumId) -> &EnumData {
        &self.enums[id.0 as usize]
    }

    pub fn impl_(&self, id: ImplId) -> &ImplData {
        &self.impls[id.0 as usize]
    }

    pub fn module(&self, id: ModuleId) -> &ModuleData {
        &self.modules[id.0 as usize]
    }

    pub fn global(&self, id: GlobalId) -> &GlobalData {
        &self.globals[id.0 as usize]
    }

    pub fn program_module_id(&self) -> ModuleId {
        let pkg_id = self.program_package_id.0 as usize;
        let pkg = &self.packages[pkg_id];
        pkg.root_module_id
    }
}
