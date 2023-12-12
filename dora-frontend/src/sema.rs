use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

use id_arena::Arena;

use crate::interner::Interner;
use dora_bytecode::Location;
use dora_parser::{compute_line_column, Span};

use crate::error::diag::Diagnostic;
use crate::error::msg::ErrorMessage;
use crate::sym::SymTable;

pub use self::aliases::{AliasDefinition, AliasDefinitionId, AliasParent};
pub use self::classes::{
    find_field_in_class, find_methods_in_class, Candidate, ClassDefinition, ClassDefinitionId,
    Field, FieldId, Visibility,
};
pub use self::consts::{ConstDefinition, ConstDefinitionId, ConstValue};
pub use self::enums::{find_methods_in_enum, EnumDefinition, EnumDefinitionId, EnumVariant};
pub use self::extensions::{
    extension_matches, extension_matches_ty, ExtensionDefinition, ExtensionDefinitionId,
};
pub use self::functions::{emit_as_bytecode_operation, FctDefinition, FctDefinitionId, FctParent};
pub use self::globals::{GlobalDefinition, GlobalDefinitionId};
pub use self::impls::{
    find_impl, impl_matches, implements_trait, ImplDefinition, ImplDefinitionId,
};
pub use self::known::KnownElements;
pub use self::modules::{module_package, module_path, ModuleDefinition, ModuleDefinitionId};
pub use self::packages::{PackageDefinition, PackageDefinitionId, PackageName};
pub use self::source_files::{SourceFile, SourceFileId};
pub use self::src::{
    AnalysisData, CallType, ContextData, ContextFieldId, ForTypeInfo, IdentType, InnerContextId,
    LazyContextClassCreationData, LazyContextData, LazyLambdaCreationData, LazyLambdaId,
    NestedScopeId, NestedVarId, NodeMap, OuterContextIdx, ScopeId, Var, VarAccess, VarId,
    VarLocation,
};
pub use self::structs::{
    find_methods_in_struct, StructDefinition, StructDefinitionField, StructDefinitionFieldId,
    StructDefinitionId,
};
pub use self::traits::{TraitDefinition, TraitDefinitionId};
pub use self::tuples::create_tuple;
pub use self::type_params::{Bound, TypeParamDefinition, TypeParamId};
pub use self::uses::{UseDefinition, UseDefinitionId};

mod aliases;
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
mod type_params;
mod uses;

pub struct SemaArgs {
    pub packages: Vec<(String, PathBuf)>,
    pub arg_file: Option<String>,
    pub test_file_as_string: Option<String>,
}

impl SemaArgs {
    pub fn for_test(input: &'static str) -> SemaArgs {
        SemaArgs {
            packages: Vec::new(),
            arg_file: None,
            test_file_as_string: Some(input.into()),
        }
    }
}

pub struct Sema {
    pub args: SemaArgs,
    pub interner: Interner,
    pub source_files: Arena<SourceFile>,
    pub diag: RefCell<Diagnostic>,
    pub known: KnownElements,
    pub aliases: Arena<AliasDefinition>, // stores all alias definitions
    pub consts: Arena<ConstDefinition>,  // stores all const definitions
    pub structs: Arena<StructDefinition>, // stores all struct source definitions
    pub classes: Arena<ClassDefinition>, // stores all class source definitions
    pub extensions: Arena<ExtensionDefinition>, // stores all extension definitions
    pub modules: Arena<ModuleDefinition>, // stores all module definitions
    pub fcts: Arena<FctDefinition>,      // stores all function source definitions
    pub enums: Arena<EnumDefinition>,    // stores all enum source definitions
    pub traits: Arena<TraitDefinition>,  // stores all trait definitions
    pub impls: Arena<ImplDefinition>,    // stores all impl definitions
    pub globals: Arena<GlobalDefinition>, // stores all global variables
    pub uses: Arena<UseDefinition>,      // stores all uses
    pub packages: Arena<PackageDefinition>,
    pub package_names: HashMap<String, PackageDefinitionId>,
    pub prelude_module_id: Option<ModuleDefinitionId>,
    pub stdlib_module_id: Option<ModuleDefinitionId>,
    pub program_module_id: Option<ModuleDefinitionId>,
    pub boots_module_id: Option<ModuleDefinitionId>,
    pub stdlib_package_id: Option<PackageDefinitionId>,
    pub program_package_id: Option<PackageDefinitionId>,
    pub boots_package_id: Option<PackageDefinitionId>,
}

impl Sema {
    pub fn new(args: SemaArgs) -> Sema {
        Sema {
            args,
            source_files: Arena::new(),
            aliases: Arena::new(),
            consts: Arena::new(),
            structs: Arena::new(),
            classes: Arena::new(),
            extensions: Arena::new(),
            modules: Arena::new(),
            enums: Arena::new(),
            traits: Arena::new(),
            impls: Arena::new(),
            globals: Arena::new(),
            uses: Arena::new(),
            interner: Interner::new(),
            known: KnownElements::new(),
            diag: RefCell::new(Diagnostic::new()),
            fcts: Arena::new(),
            packages: Arena::new(),
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

    pub fn file(&self, id: SourceFileId) -> &SourceFile {
        &self.source_files[id]
    }

    pub fn alias(&self, id: AliasDefinitionId) -> &AliasDefinition {
        &self.aliases[id]
    }

    pub fn const_(&self, id: ConstDefinitionId) -> &ConstDefinition {
        &self.consts[id]
    }

    pub fn struct_(&self, id: StructDefinitionId) -> &StructDefinition {
        &self.structs[id]
    }

    pub fn class(&self, id: ClassDefinitionId) -> &ClassDefinition {
        &self.classes[id]
    }

    pub fn extension(&self, id: ExtensionDefinitionId) -> &ExtensionDefinition {
        &self.extensions[id]
    }

    pub fn module(&self, id: ModuleDefinitionId) -> &ModuleDefinition {
        &self.modules[id]
    }

    pub fn enum_(&self, id: EnumDefinitionId) -> &EnumDefinition {
        &self.enums[id]
    }

    pub fn trait_(&self, id: TraitDefinitionId) -> &TraitDefinition {
        &self.traits[id]
    }

    pub fn impl_(&self, id: ImplDefinitionId) -> &ImplDefinition {
        &self.impls[id]
    }

    pub fn global(&self, id: GlobalDefinitionId) -> &GlobalDefinition {
        &self.globals[id]
    }

    pub fn fct(&self, id: FctDefinitionId) -> &FctDefinition {
        &self.fcts[id]
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

    pub fn module_table(&self, module_id: ModuleDefinitionId) -> Rc<SymTable> {
        self.module(module_id).table()
    }

    pub fn stdlib_module(&self) -> Rc<SymTable> {
        self.module_table(self.stdlib_module_id())
    }

    pub fn prelude_module(&self) -> Rc<SymTable> {
        self.module_table(self.prelude_module_id())
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

    pub fn compute_loc(&self, file_id: SourceFileId, span: Span) -> Location {
        let file = self.file(file_id);
        let (line, column) = compute_line_column(&file.line_starts, span.start());
        Location::new(line, column)
    }

    pub fn report(&self, file: SourceFileId, span: Span, msg: ErrorMessage) {
        self.diag.borrow_mut().report(file, span, msg);
    }

    pub fn report_without_location(&self, msg: ErrorMessage) {
        self.diag.borrow_mut().report_without_location(msg);
    }
}
