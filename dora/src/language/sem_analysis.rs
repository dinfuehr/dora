use std::path::PathBuf;
use std::sync::Arc;

use parking_lot::RwLock;

use dora_parser::ast;

#[cfg(test)]
use crate::language::sym::NestedSymTable;
use crate::language::sym::SymTable;
use crate::language::ty::{SourceType, SourceTypeArray};
#[cfg(test)]
use crate::vm::{FieldId, TraitDefinitionId};
use crate::vm::{File, FileId, SemAnalysis};

pub use self::annotations::{AnnotationDefinition, AnnotationDefinitionId};
pub use self::classes::{
    find_field_in_class, find_method_in_class, find_methods_in_class, Candidate, ClassDefinition,
    ClassDefinitionId, TypeParam, TypeParamDefinition, TypeParamId,
};
pub use self::consts::{ConstDefinition, ConstDefinitionId, ConstValue};
pub use self::enums::{find_methods_in_enum, EnumDefinition, EnumDefinitionId, EnumVariant};
pub use self::extensions::{extension_matches, extension_matches_ty, ExtensionData, ExtensionId};
pub use self::functions::{FctDefinition, FctDefinitionId, FctParent, Intrinsic};
pub use self::globals::{GlobalDefinition, GlobalDefinitionId};
pub use self::namespaces::{namespace_package, namespace_path, NamespaceData, NamespaceId};
pub use self::src::{
    AnalysisData, CallType, ConvInfo, ForTypeInfo, IdentType, NodeMap, Var, VarId,
};
pub use self::structs::{
    find_methods_in_struct, StructDefinition, StructDefinitionField, StructDefinitionFieldId,
    StructDefinitionId, StructInstance, StructInstanceField, StructInstanceId,
};

mod annotations;
mod classes;
mod consts;
mod enums;
mod extensions;
mod functions;
mod globals;
mod namespaces;
mod src;
mod structs;

impl SemAnalysis {
    #[cfg(test)]
    pub fn cls_by_name(&self, name: &'static str) -> ClassDefinitionId {
        let name = self.interner.intern(name);

        NestedSymTable::new(self, self.global_namespace_id)
            .get_class(name)
            .expect("class not found")
    }

    #[cfg(test)]
    pub fn struct_by_name(&self, name: &'static str) -> StructDefinitionId {
        let name = self.interner.intern(name);
        NestedSymTable::new(self, self.global_namespace_id)
            .get_struct(name)
            .expect("class not found")
    }

    #[cfg(test)]
    pub fn enum_by_name(&self, name: &'static str) -> EnumDefinitionId {
        let name = self.interner.intern(name);
        NestedSymTable::new(self, self.global_namespace_id)
            .get_enum(name)
            .expect("class not found")
    }

    #[cfg(test)]
    pub fn const_by_name(&self, name: &'static str) -> ConstDefinitionId {
        let name = self.interner.intern(name);
        NestedSymTable::new(self, self.global_namespace_id)
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

        let cls_id = NestedSymTable::new(self, self.global_namespace_id)
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

        let struct_id = NestedSymTable::new(self, self.global_namespace_id)
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

        let cls_id = NestedSymTable::new(self, self.global_namespace_id)
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
        NestedSymTable::new(self, self.global_namespace_id).get_fct(name)
    }

    #[cfg(test)]
    pub fn ctor_by_name(&self, name: &str) -> FctDefinitionId {
        let name = self.interner.intern(name);
        let cls_id = NestedSymTable::new(self, self.global_namespace_id)
            .get_class(name)
            .expect("class not found");
        let cls = self.classes.idx(cls_id);
        let cls = cls.read();

        cls.constructor.expect("no ctor found")
    }

    #[cfg(test)]
    pub fn trait_by_name(&self, name: &str) -> TraitDefinitionId {
        let name = self.interner.intern(name);
        let trait_id = NestedSymTable::new(self, self.global_namespace_id)
            .get_trait(name)
            .expect("class not found");

        trait_id
    }

    #[cfg(test)]
    pub fn trait_method_by_name(&self, trait_name: &str, method_name: &str) -> FctDefinitionId {
        let trait_id = self.trait_by_name(trait_name);
        let method_name = self.interner.intern(method_name);

        let xtrait = self.traits[trait_id].read();

        xtrait
            .instance_names
            .get(&method_name)
            .cloned()
            .expect("method not found")
    }

    #[cfg(test)]
    pub fn global_by_name(&self, name: &str) -> GlobalDefinitionId {
        let name = self.interner.intern(name);
        NestedSymTable::new(self, self.global_namespace_id)
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

    pub fn add_file(&self, path: Option<PathBuf>, namespace_id: NamespaceId, ast: Arc<ast::File>) {
        let mut files = self.files.write();
        let file_id = (files.len() as u32).into();
        files.push(File {
            id: file_id,
            path,
            namespace_id,
            ast,
        });
    }

    pub fn namespace_table(&self, namespace_id: NamespaceId) -> Arc<RwLock<SymTable>> {
        self.namespaces[namespace_id.to_usize()]
            .read()
            .table
            .clone()
    }

    pub fn stdlib_namespace(&self) -> Arc<RwLock<SymTable>> {
        self.namespaces[self.stdlib_namespace_id.to_usize()]
            .read()
            .table
            .clone()
    }

    pub fn prelude_namespace(&self) -> Arc<RwLock<SymTable>> {
        self.namespaces[self.prelude_namespace_id.to_usize()]
            .read()
            .table
            .clone()
    }

    pub fn cls(&self, cls_id: ClassDefinitionId) -> SourceType {
        SourceType::Class(cls_id, SourceTypeArray::empty())
    }

    pub fn file(&self, idx: FileId) -> Arc<ast::File> {
        self.files.read().get(idx.to_usize()).unwrap().ast.clone()
    }

    pub fn add_fct(&self, mut fct: FctDefinition) -> FctDefinitionId {
        let mut fcts = self.fcts.lock();
        let fctid = FctDefinitionId(fcts.len());

        fct.id = fctid;

        fcts.push(Arc::new(RwLock::new(fct)));

        fctid
    }
}
