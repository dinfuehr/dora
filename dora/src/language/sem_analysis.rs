use std::path::PathBuf;
use std::sync::Arc;

use parking_lot::RwLock;

#[cfg(test)]
use crate::language::sym::NestedSymTable;
use crate::language::sym::SymTable;
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::vm::VM;

pub use self::annotations::{AnnotationDefinition, AnnotationDefinitionId};
pub use self::classes::{
    find_field_in_class, find_method_in_class, find_methods_in_class, Candidate, ClassDefinition,
    ClassDefinitionId, Field, FieldId, TypeParam, TypeParamDefinition, TypeParamId,
};
pub use self::consts::{ConstDefinition, ConstDefinitionId, ConstValue};
pub use self::enums::{find_methods_in_enum, EnumDefinition, EnumDefinitionId, EnumVariant};
pub use self::extensions::{
    extension_matches, extension_matches_ty, ExtensionDefinition, ExtensionDefinitionId,
};
pub use self::functions::{FctDefinition, FctDefinitionId, FctParent, Intrinsic};
pub use self::globals::{GlobalDefinition, GlobalDefinitionId};
pub use self::impls::{
    find_impl, find_trait_impl, impl_matches, implements_trait, ImplDefinition, ImplDefinitionId,
};
pub use self::modules::{module_package, module_path, ModuleDefinition, ModuleDefinitionId};
pub use self::source_files::{SourceFile, SourceFileId};
pub use self::src::{
    AnalysisData, CallType, ForTypeInfo, IdentType, NodeMap, Var, VarAccess, VarId,
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
mod modules;
mod source_files;
mod src;
mod structs;
mod traits;
mod tuples;
mod uses;

pub type SemAnalysis = VM;

impl SemAnalysis {
    #[cfg(test)]
    pub fn cls_by_name(&self, name: &'static str) -> ClassDefinitionId {
        let name = self.interner.intern(name);

        NestedSymTable::new(self, self.program_module_id)
            .get_class(name)
            .expect("class not found")
    }

    #[cfg(test)]
    pub fn struct_by_name(&self, name: &'static str) -> StructDefinitionId {
        let name = self.interner.intern(name);
        NestedSymTable::new(self, self.program_module_id)
            .get_struct(name)
            .expect("class not found")
    }

    #[cfg(test)]
    pub fn enum_by_name(&self, name: &'static str) -> EnumDefinitionId {
        let name = self.interner.intern(name);
        NestedSymTable::new(self, self.program_module_id)
            .get_enum(name)
            .expect("class not found")
    }

    #[cfg(test)]
    pub fn const_by_name(&self, name: &'static str) -> ConstDefinitionId {
        let name = self.interner.intern(name);
        NestedSymTable::new(self, self.program_module_id)
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

        let cls_id = NestedSymTable::new(self, self.program_module_id)
            .get_class(class_name)
            .expect("class not found");
        let cls = self.classes.idx(cls_id);
        let cls = cls.read();

        let candidates = find_methods_in_class(
            self,
            cls.ty(),
            &cls.type_params,
            None,
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
    pub fn struct_method_by_name(
        &self,
        struct_name: &'static str,
        function_name: &'static str,
        is_static: bool,
    ) -> Option<FctDefinitionId> {
        let struct_name = self.interner.intern(struct_name);
        let function_name = self.interner.intern(function_name);

        let struct_id = NestedSymTable::new(self, self.program_module_id)
            .get_struct(struct_name)
            .expect("struct not found");
        let xstruct = self.structs.idx(struct_id);
        let xstruct = xstruct.read();

        let candidates = find_methods_in_struct(
            self,
            xstruct.ty(),
            &xstruct.type_params,
            None,
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

        let cls_id = NestedSymTable::new(self, self.program_module_id)
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
        NestedSymTable::new(self, self.program_module_id).get_fct(name)
    }

    #[cfg(test)]
    pub fn ctor_by_name(&self, name: &str) -> FctDefinitionId {
        let name = self.interner.intern(name);
        let cls_id = NestedSymTable::new(self, self.program_module_id)
            .get_class(name)
            .expect("class not found");
        let cls = self.classes.idx(cls_id);
        let cls = cls.read();

        cls.constructor.expect("no ctor found")
    }

    #[cfg(test)]
    pub fn trait_by_name(&self, name: &str) -> TraitDefinitionId {
        let name = self.interner.intern(name);
        let trait_id = NestedSymTable::new(self, self.program_module_id)
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
        NestedSymTable::new(self, self.program_module_id)
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
        path: PathBuf,
        content: Arc<String>,
        module_id: ModuleDefinitionId,
    ) -> SourceFileId {
        let file_id = SourceFileId(self.source_files.len());
        self.source_files.push(SourceFile {
            id: file_id,
            path,
            content,
            module_id,
        });
        file_id
    }

    pub fn module_table(&self, module_id: ModuleDefinitionId) -> Arc<RwLock<SymTable>> {
        self.modules[module_id].read().table.clone()
    }

    pub fn stdlib_module(&self) -> Arc<RwLock<SymTable>> {
        self.modules[self.stdlib_module_id].read().table.clone()
    }

    pub fn prelude_module(&self) -> Arc<RwLock<SymTable>> {
        self.modules[self.prelude_module_id].read().table.clone()
    }

    pub fn cls(&self, cls_id: ClassDefinitionId) -> SourceType {
        SourceType::Class(cls_id, SourceTypeArray::empty())
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
