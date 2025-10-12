use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::atomic::{AtomicU32, Ordering};

use id_arena::Arena;

use crate::interner::Interner;
use dora_bytecode::Location;
use dora_parser::ast::{Ast, AstId};
use dora_parser::{Span, compute_line_column};

use crate::error::diag::Diagnostic;
use crate::error::msg::ErrorMessage;
use crate::{Name, SymTable, Vfs};

pub trait ToArcString {
    fn into(self) -> Arc<String>;
}

impl ToArcString for &str {
    fn into(self) -> Arc<String> {
        Arc::new(self.to_string())
    }
}

impl ToArcString for String {
    fn into(self) -> Arc<String> {
        Arc::new(self)
    }
}

impl ToArcString for Arc<String> {
    fn into(self) -> Arc<String> {
        self
    }
}

pub use self::aliases::{AliasBound, AliasDefinition, AliasDefinitionId, AliasParent};
pub use self::classes::{
    Candidate, ClassDefinition, ClassDefinitionId, Visibility, find_field_in_class,
};
pub use self::consts::{ConstDefinition, ConstDefinitionId, ConstValue};
pub use self::elements::{
    Element, ElementAccess, ElementField, ElementId, ElementWithFields, parent_element_or_self,
};
pub use self::enums::{EnumDefinition, EnumDefinitionId, VariantDefinition, VariantDefinitionId};
pub use self::extensions::{ExtensionDefinition, ExtensionDefinitionId};
pub use self::fields::{FieldDefinition, FieldDefinitionId, FieldIndex};
pub use self::functions::{
    FctDefinition, FctDefinitionId, FctParent, Intrinsic, Param, Params, emit_as_bytecode_operation,
};
pub use self::globals::{GlobalDefinition, GlobalDefinitionId};
pub use self::impl_matching::{find_impl, impl_matches, implements_trait, maybe_alias_ty};
pub use self::impls::{ImplDefinition, ImplDefinitionId};
pub use self::known::KnownElements;
pub use self::matching::{block_matches_ty, extension_matches, match_arrays};
pub use self::modules::{ModuleDefinition, ModuleDefinitionId, module_package, module_path};
pub use self::packages::{PackageDefinition, PackageDefinitionId, PackageName};
pub use self::source_files::{SourceFile, SourceFileId};
pub use self::src::{
    AnalysisData, ArrayAssignment, CallType, ContextData, ContextFieldId, ForTypeInfo, IdentType,
    InnerContextId, LazyContextClassCreationData, LazyContextData, LazyLambdaCreationData,
    LazyLambdaId, NestedScopeId, NestedVarId, NodeMap, OuterContextIdx, ScopeId, Var, VarAccess,
    VarId, VarLocation,
};
pub use self::structs::{StructDefinition, StructDefinitionId};
pub use self::traits::{TraitDefinition, TraitDefinitionId, is_trait_object_safe};
pub use self::tuples::create_tuple;
pub use self::type_params::{Bound, TypeParamDefinition, TypeParamId, new_identity_type_params};
pub use self::uses::{UseDefinition, UseDefinitionId};

mod aliases;
mod classes;
mod consts;
mod elements;
mod enums;
mod extensions;
mod fields;
mod functions;
mod globals;
mod impl_matching;
mod impls;
mod known;
mod matching;
mod modules;
mod packages;
mod source_files;
mod src;
mod structs;
mod traits;
mod tuples;
mod type_params;
mod uses;

#[derive(Clone, Debug)]
pub enum FileContent {
    Path(PathBuf),
    Content(Arc<String>),
}

impl FileContent {
    pub fn to_path(&self) -> Option<&PathBuf> {
        match self {
            FileContent::Path(path) => Some(path),
            _ => None,
        }
    }
}

pub struct SemaCreationParams {
    pub packages: Vec<(String, FileContent)>,
    pub program_file: Option<FileContent>,
    pub vfs: Option<Vfs>,
    pub boots: bool,
    pub is_standard_library: bool,
}

impl SemaCreationParams {
    pub fn new() -> SemaCreationParams {
        SemaCreationParams {
            packages: Vec::new(),
            program_file: None,
            vfs: None,
            boots: false,
            is_standard_library: false,
        }
    }

    pub fn for_test(input: &str, packages: &[(&str, &str)]) -> SemaCreationParams {
        SemaCreationParams::new()
            .set_program_content(input)
            .set_package_contents(packages)
    }

    pub fn set_vfs(mut self, vfs: Vfs) -> SemaCreationParams {
        self.vfs = Some(vfs);
        self
    }

    pub fn set_program_content<T>(mut self, content: T) -> SemaCreationParams
    where
        T: ToArcString,
    {
        self.program_file = Some(FileContent::Content(content.into()));
        self
    }

    pub fn set_program_path(mut self, path: PathBuf) -> SemaCreationParams {
        self.program_file = Some(FileContent::Path(path));
        self
    }

    pub fn set_package_contents(mut self, packages: &[(&str, &str)]) -> SemaCreationParams {
        let packages = packages
            .iter()
            .map(|(name, content)| {
                (
                    name.to_string(),
                    FileContent::Content(Arc::new(content.to_string())),
                )
            })
            .collect::<Vec<_>>();

        self.packages = packages;
        self
    }
}

pub struct Sema {
    pub interner: Interner,
    pub source_files: Arena<SourceFile>,
    pub diag: RefCell<Diagnostic>,
    pub known: KnownElements,
    pub aliases: Arena<AliasDefinition>, // stores all alias definitions
    pub consts: Arena<ConstDefinition>,  // stores all const definitions
    pub structs: Arena<StructDefinition>, // stores all struct source definitions
    pub classes: Arena<ClassDefinition>, // stores all class source definitions
    pub fields: Arena<FieldDefinition>,  // stores all field source definitions
    pub extensions: Arena<ExtensionDefinition>, // stores all extension definitions
    pub modules: Arena<ModuleDefinition>, // stores all module definitions
    pub fcts: Arena<FctDefinition>,      // stores all function source definitions
    pub enums: Arena<EnumDefinition>,    // stores all enum source definitions
    pub variants: Arena<VariantDefinition>, // stores all enum variant definitions
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
    pub vfs: Option<Vfs>,
    pub include_boots: bool,
    pub is_standard_library: bool,
    pub program_file: FileContent,
    pub package_contents: Vec<(String, FileContent)>,
    next_context_id: AtomicU32,
    next_lambda_id: AtomicU32,
}

impl Sema {
    pub fn new(args: SemaCreationParams) -> Sema {
        let vfs = args.vfs.clone();
        let include_boots = args.boots;
        let is_standard_library = args.is_standard_library;
        let program_file = args.program_file.expect("missing program");
        let package_contents = args.packages;

        Sema {
            source_files: Arena::new(),
            aliases: Arena::new(),
            consts: Arena::new(),
            structs: Arena::new(),
            classes: Arena::new(),
            fields: Arena::new(),
            extensions: Arena::new(),
            modules: Arena::new(),
            enums: Arena::new(),
            variants: Arena::new(),
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
            vfs,
            include_boots,
            is_standard_library,
            program_file,
            package_contents,
            next_context_id: AtomicU32::new(1),
            next_lambda_id: AtomicU32::new(1),
        }
    }

    pub fn by_id<T: ElementAccess>(&self, id: T::Id) -> &T {
        T::by_id(self, id)
    }

    pub fn file(&self, id: SourceFileId) -> &SourceFile {
        &self.source_files[id]
    }

    pub fn node(&self, id: SourceFileId, ast_id: AstId) -> &Ast {
        self.file(id).node(ast_id)
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

    pub fn field(&self, id: FieldDefinitionId) -> &FieldDefinition {
        &self.fields[id]
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

    pub fn variant(&self, id: VariantDefinitionId) -> &VariantDefinition {
        &self.variants[id]
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

    pub fn element(&self, element_id: ElementId) -> &dyn Element {
        match element_id {
            ElementId::Alias(id) => self.alias(id),
            ElementId::Const(id) => self.const_(id),
            ElementId::Class(id) => self.class(id),
            ElementId::Struct(id) => self.struct_(id),
            ElementId::Global(id) => self.global(id),
            ElementId::Impl(id) => self.impl_(id),
            ElementId::Extension(id) => self.extension(id),
            ElementId::Fct(id) => self.fct(id),
            ElementId::Enum(id) => self.enum_(id),
            ElementId::Trait(id) => self.trait_(id),
            ElementId::Module(id) => self.module(id),
            ElementId::Variant(id) => self.variant(id),
            ElementId::Field(id) => self.field(id),
            ElementId::Use(_) => {
                unreachable!("Use does not implement Element trait")
            }
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

    pub fn debug_loc(&self, element: &dyn Element) -> String {
        let file = self.file(element.file_id());
        let loc = self.compute_loc(element.file_id(), element.span());
        format!("{}:{}", file.path.display(), loc)
    }

    pub fn report(&self, file: SourceFileId, span: Span, msg: ErrorMessage) {
        self.diag.borrow_mut().report(file, span, msg);
    }

    pub fn report_id(&self, file_id: SourceFileId, ast_id: AstId, msg: ErrorMessage) {
        let node = self.node(file_id, ast_id);
        self.diag.borrow_mut().report(file_id, node.span(), msg);
    }

    pub fn report_without_location(&self, msg: ErrorMessage) {
        self.diag.borrow_mut().report_without_location(msg);
    }

    pub fn warn(&self, file: SourceFileId, span: Span, msg: ErrorMessage) {
        self.diag.borrow_mut().warn(file, span, msg);
    }

    pub fn generate_context_name(&self) -> String {
        let id = self.next_context_id.fetch_add(1, Ordering::Relaxed);
        format!("$Context{id}")
    }

    pub fn generate_lambda_name(&self) -> String {
        let id = self.next_lambda_id.fetch_add(1, Ordering::Relaxed);
        format!("$Lambda{id}")
    }

    pub fn name(&self, name: Name) -> String {
        self.interner.str(name).to_string()
    }

    pub fn parse_single_file(&mut self) -> SourceFileId {
        crate::element_collector::collect_elements_for_single_file(self)
    }

    pub fn parse_project(&mut self) {
        crate::element_collector::collect_elements_for_package(self);
    }
}
