use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use parking_lot::{Mutex, RwLock};

use dora_parser::interner::{Interner, Name};

use crate::language::error::diag::Diagnostic;
#[cfg(test)]
use crate::language::sym::ModuleSymTable;
use crate::language::sym::SymTable;
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::{GrowableVec, MutableVec};

pub use self::annotations::{AnnotationDefinition, AnnotationDefinitionId};
pub use self::classes::{
    find_field_in_class, find_methods_in_class, Bound, Candidate, ClassDefinition,
    ClassDefinitionId, Field, FieldId, TypeParamDefinition, TypeParamId, Visibility,
};
pub use self::consts::{ConstDefinition, ConstDefinitionId, ConstValue};
pub use self::enums::{find_methods_in_enum, EnumDefinition, EnumDefinitionId, EnumVariant};
pub use self::extensions::{
    extension_matches, extension_matches_ty, ExtensionDefinition, ExtensionDefinitionId,
};
pub use self::functions::{emit_as_bytecode_operation, FctDefinition, FctDefinitionId, FctParent};
pub use self::globals::{GlobalDefinition, GlobalDefinitionId};
pub use self::impls::{
    find_impl, find_trait_impl, impl_matches, implements_trait, ImplDefinition, ImplDefinitionId,
};
pub use self::known::KnownElements;
pub use self::modules::{module_package, module_path, ModuleDefinition, ModuleDefinitionId};
pub use self::packages::{PackageDefinition, PackageDefinitionId, PackageName};
pub use self::source_files::{compute_line_starts, pos_from_span, SourceFile, SourceFileId};
pub use self::src::{
    AnalysisData, CallType, ContextIdx, ForTypeInfo, IdentType, NestedVarId, NodeMap, Var,
    VarAccess, VarId, VarLocation,
};
pub use self::structs::{
    find_methods_in_struct, StructDefinition, StructDefinitionField, StructDefinitionFieldId,
    StructDefinitionId,
};
pub use self::traits::{TraitDefinition, TraitDefinitionId};
pub use self::tuples::create_tuple;
pub use self::uses::UseDefinition;

mod annotations;
mod classes;
mod consts;
mod enums;
mod extensions;
mod functions;
mod globals;
mod impls;
mod known;
mod modules;
mod packages;
mod source_files;
mod src;
mod structs;
mod traits;
mod tuples;
mod uses;

pub struct SemAnalysisArgs {
    pub packages: Vec<(String, PathBuf)>,
    pub arg_file: Option<String>,
    pub test_file_as_string: Option<&'static str>,
}

impl SemAnalysisArgs {
    pub fn for_test(input: &'static str) -> SemAnalysisArgs {
        SemAnalysisArgs {
            packages: Vec::new(),
            arg_file: None,
            test_file_as_string: Some(input),
        }
    }
}

pub struct SemAnalysis {
    pub args: SemAnalysisArgs,
    pub interner: Interner,
    pub source_files: Vec<SourceFile>,
    pub diag: Mutex<Diagnostic>,
    pub known: KnownElements,
    pub consts: MutableVec<ConstDefinition>, // stores all const definitions
    pub structs: MutableVec<StructDefinition>, // stores all struct source definitions
    pub classes: MutableVec<ClassDefinition>, // stores all class source definitions
    pub extensions: MutableVec<ExtensionDefinition>, // stores all extension definitions
    pub annotations: MutableVec<AnnotationDefinition>, // stores all annotation source definitions
    pub modules: MutableVec<ModuleDefinition>, // stores all module definitions
    pub fcts: GrowableVec<RwLock<FctDefinition>>, // stores all function source definitions
    pub enums: MutableVec<EnumDefinition>,   // stores all enum source definitions
    pub traits: MutableVec<TraitDefinition>, // stores all trait definitions
    pub impls: MutableVec<ImplDefinition>,   // stores all impl definitions
    pub globals: MutableVec<GlobalDefinition>, // stores all global variables
    pub uses: Vec<UseDefinition>,            // stores all uses
    pub packages: MutableVec<PackageDefinition>,
    pub package_names: HashMap<Name, PackageDefinitionId>,
    pub prelude_module_id: Option<ModuleDefinitionId>,
    pub stdlib_module_id: Option<ModuleDefinitionId>,
    pub program_module_id: Option<ModuleDefinitionId>,
    pub boots_module_id: Option<ModuleDefinitionId>,
    pub stdlib_package_id: Option<PackageDefinitionId>,
    pub program_package_id: Option<PackageDefinitionId>,
    pub boots_package_id: Option<PackageDefinitionId>,
}

impl SemAnalysis {
    pub fn new(args: SemAnalysisArgs) -> SemAnalysis {
        SemAnalysis {
            args,
            source_files: Vec::new(),
            consts: MutableVec::new(),
            structs: MutableVec::new(),
            classes: MutableVec::new(),
            extensions: MutableVec::new(),
            annotations: MutableVec::new(),
            modules: MutableVec::new(),
            enums: MutableVec::new(),
            traits: MutableVec::new(),
            impls: MutableVec::new(),
            globals: MutableVec::new(),
            uses: Vec::new(),
            interner: Interner::new(),
            known: KnownElements::new(),
            diag: Mutex::new(Diagnostic::new()),
            fcts: GrowableVec::new(),
            packages: MutableVec::new(),
            package_names: HashMap::new(),
            prelude_module_id: None,
            stdlib_module_id: None,
            program_module_id: None,
            boots_module_id: None,
            stdlib_package_id: None,
            program_package_id: None,
            boots_package_id: None,
        }
    }

    pub fn prelude_module_id(&self) -> ModuleDefinitionId {
        self.prelude_module_id.expect("uninitialized module id")
    }

    pub fn stdlib_module_id(&self) -> ModuleDefinitionId {
        self.stdlib_module_id.expect("uninitialized module id")
    }

    pub fn boots_module_id(&self) -> ModuleDefinitionId {
        self.boots_module_id.expect("uninitialized module id")
    }

    pub fn program_module_id(&self) -> ModuleDefinitionId {
        self.program_module_id.expect("uninitialized module id")
    }

    pub fn stdlib_package_id(&self) -> PackageDefinitionId {
        self.stdlib_package_id.expect("uninitialized package id")
    }

    pub fn boots_package_id(&self) -> PackageDefinitionId {
        self.boots_package_id.expect("uninitialized package id")
    }

    pub fn program_package_id(&self) -> PackageDefinitionId {
        self.program_package_id.expect("uninitialized package id")
    }

    pub fn source_file(&self, idx: SourceFileId) -> &SourceFile {
        &self.source_files[idx.to_usize()]
    }

    pub fn add_fct(&self, mut fct: FctDefinition) -> FctDefinitionId {
        let mut fcts = self.fcts.lock();
        let fctid = FctDefinitionId(fcts.len());

        fct.id = Some(fctid);

        fcts.push(Arc::new(RwLock::new(fct)));

        fctid
    }
}

impl SemAnalysis {
    #[cfg(test)]
    pub fn cls_by_name(&self, name: &'static str) -> ClassDefinitionId {
        let name = self.interner.intern(name);

        ModuleSymTable::new(self, self.program_module_id())
            .get_class(name)
            .expect("class not found")
    }

    #[cfg(test)]
    pub fn struct_by_name(&self, name: &'static str) -> StructDefinitionId {
        let name = self.interner.intern(name);
        ModuleSymTable::new(self, self.program_module_id())
            .get_struct(name)
            .expect("class not found")
    }

    #[cfg(test)]
    pub fn enum_by_name(&self, name: &'static str) -> EnumDefinitionId {
        let name = self.interner.intern(name);
        ModuleSymTable::new(self, self.program_module_id())
            .get_enum(name)
            .expect("class not found")
    }

    #[cfg(test)]
    pub fn const_by_name(&self, name: &'static str) -> ConstDefinitionId {
        let name = self.interner.intern(name);
        ModuleSymTable::new(self, self.program_module_id())
            .get_const(name)
            .expect("class not found")
    }

    #[cfg(test)]
    pub fn cls_method_by_name(
        &self,
        class_name: &'static str,
        function_name: &'static str,
        is_static: bool,
    ) -> Option<FctDefinitionId> {
        let class_name = self.interner.intern(class_name);
        let function_name = self.interner.intern(function_name);

        let cls_id = ModuleSymTable::new(self, self.program_module_id())
            .get_class(class_name)
            .expect("class not found");
        let cls = self.classes.idx(cls_id);
        let cls = cls.read();

        let candidates =
            find_methods_in_class(self, cls.ty(), cls.type_params(), function_name, is_static);
        if candidates.len() == 1 {
            Some(candidates[0].fct_id)
        } else {
            None
        }
    }

    #[cfg(test)]
    pub fn struct_method_by_name(
        &self,
        struct_name: &'static str,
        function_name: &'static str,
        is_static: bool,
    ) -> Option<FctDefinitionId> {
        let struct_name = self.interner.intern(struct_name);
        let function_name = self.interner.intern(function_name);

        let struct_id = ModuleSymTable::new(self, self.program_module_id())
            .get_struct(struct_name)
            .expect("struct not found");
        let struct_ = self.structs.idx(struct_id);
        let struct_ = struct_.read();

        let candidates = find_methods_in_struct(
            self,
            struct_.ty(),
            struct_.type_params(),
            function_name,
            is_static,
        );

        if candidates.len() == 1 {
            Some(candidates[0].fct_id)
        } else {
            None
        }
    }

    #[cfg(test)]
    pub fn field_by_name(
        &self,
        class_name: &'static str,
        field_name: &'static str,
    ) -> (ClassDefinitionId, FieldId) {
        let class_name = self.interner.intern(class_name);
        let field_name = self.interner.intern(field_name);

        let cls_id = ModuleSymTable::new(self, self.program_module_id())
            .get_class(class_name)
            .expect("class not found");
        let cls = self.classes.idx(cls_id);
        let cls = cls.read();
        let field_id = cls.field_by_name(field_name);

        (cls_id, field_id)
    }

    #[cfg(test)]
    pub fn fct_by_name(&self, name: &str) -> Option<FctDefinitionId> {
        let name = self.interner.intern(name);
        ModuleSymTable::new(self, self.program_module_id()).get_fct(name)
    }

    #[cfg(test)]
    pub fn trait_by_name(&self, name: &str) -> TraitDefinitionId {
        let name = self.interner.intern(name);
        let trait_id = ModuleSymTable::new(self, self.program_module_id())
            .get_trait(name)
            .expect("class not found");

        trait_id
    }

    #[cfg(test)]
    pub fn trait_method_by_name(&self, trait_name: &str, method_name: &str) -> FctDefinitionId {
        let trait_id = self.trait_by_name(trait_name);
        let method_name = self.interner.intern(method_name);

        let trait_ = self.traits[trait_id].read();

        trait_
            .instance_names
            .get(&method_name)
            .cloned()
            .expect("method not found")
    }

    #[cfg(test)]
    pub fn global_by_name(&self, name: &str) -> GlobalDefinitionId {
        let name = self.interner.intern(name);
        ModuleSymTable::new(self, self.program_module_id())
            .get_global(name)
            .expect("global not found")
    }

    pub fn cls_with_type_list(
        &self,
        cls_id: ClassDefinitionId,
        type_list: SourceTypeArray,
    ) -> SourceType {
        SourceType::Class(cls_id, type_list)
    }

    pub fn add_source_file(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        path: PathBuf,
        content: Arc<String>,
    ) -> SourceFileId {
        let file_id = SourceFileId(self.source_files.len());
        let line_starts = compute_line_starts(&content);
        self.source_files.push(SourceFile {
            id: file_id,
            package_id,
            path,
            content,
            module_id,
            line_starts,
        });
        file_id
    }

    pub fn module_table(&self, module_id: ModuleDefinitionId) -> Arc<RwLock<SymTable>> {
        self.modules[module_id].read().table.clone()
    }

    pub fn stdlib_module(&self) -> Arc<RwLock<SymTable>> {
        self.modules[self.stdlib_module_id()].read().table.clone()
    }

    pub fn prelude_module(&self) -> Arc<RwLock<SymTable>> {
        self.modules[self.prelude_module_id()].read().table.clone()
    }

    pub fn cls(&self, cls_id: ClassDefinitionId) -> SourceType {
        SourceType::Class(cls_id, SourceTypeArray::empty())
    }

    pub fn set_prelude_module_id(&mut self, module_id: ModuleDefinitionId) {
        assert!(self.prelude_module_id.is_none());
        self.prelude_module_id = Some(module_id);
    }

    pub fn set_stdlib_module_id(&mut self, module_id: ModuleDefinitionId) {
        assert!(self.stdlib_module_id.is_none());
        self.stdlib_module_id = Some(module_id);
    }

    pub fn set_boots_module_id(&mut self, module_id: ModuleDefinitionId) {
        assert!(self.boots_module_id.is_none());
        self.boots_module_id = Some(module_id);
    }

    pub fn set_program_module_id(&mut self, module_id: ModuleDefinitionId) {
        assert!(self.program_module_id.is_none());
        self.program_module_id = Some(module_id);
    }

    pub fn set_stdlib_package_id(&mut self, package_id: PackageDefinitionId) {
        assert!(self.stdlib_package_id.is_none());
        self.stdlib_package_id = Some(package_id);
    }

    pub fn set_program_package_id(&mut self, package_id: PackageDefinitionId) {
        assert!(self.program_package_id.is_none());
        self.program_package_id = Some(package_id);
    }

    pub fn set_boots_package_id(&mut self, package_id: PackageDefinitionId) {
        assert!(self.boots_package_id.is_none());
        self.boots_package_id = Some(package_id);
    }

    pub fn has_boots_package(&mut self) -> bool {
        self.boots_package_id.is_some()
    }

    pub fn add_package(
        &mut self,
        package_name: PackageName,
        module_name: Option<Name>,
    ) -> (PackageDefinitionId, ModuleDefinitionId) {
        let module = ModuleDefinition::new_top_level(module_name);
        let module_id = self.modules.push(module);

        let package = PackageDefinition::new(package_name, module_id);
        let package_id = self.packages.push(package);

        self.modules.idx(module_id).write().package_id = Some(package_id);

        (package_id, module_id)
    }
}
