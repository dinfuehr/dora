use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::io::{Error, Read};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::error::msg::ErrorMessage;
use crate::interner::Name;
use crate::report_sym_shadow_span;
use crate::sema::{
    ClassDefinition, ConstDefinition, EnumDefinition, ExtensionDefinition, ExtensionDefinitionId,
    FctDefinition, FctParent, Field, FieldId, GlobalDefinition, ImplDefinition, ImplDefinitionId,
    ModuleDefinition, ModuleDefinitionId, PackageDefinitionId, PackageName, Sema, SourceFileId,
    StructDefinition, StructDefinitionField, StructDefinitionFieldId, TraitDefinition,
    TraitDefinitionId, UseDefinition, Visibility,
};
use crate::sym::Sym;
use crate::ty::SourceType;
use crate::STDLIB;
use dora_parser::ast::visit::Visitor;
use dora_parser::ast::{self, visit, Annotation, Modifiers};
use dora_parser::parser::Parser;
use dora_parser::Span;

pub fn parse(sa: &mut Sema) {
    let mut discoverer = ProgramParser::new(sa);
    discoverer.parse_all();
}

#[derive(Copy, Clone)]
enum FileLookup {
    FileSystem,
    Bundle,
}

struct ProgramParser<'a> {
    sa: &'a mut Sema,
    files_to_parse: VecDeque<(SourceFileId, FileLookup, Option<PathBuf>)>,
    packages: HashMap<String, PathBuf>,
}

impl<'a> ProgramParser<'a> {
    fn new(sa: &mut Sema) -> ProgramParser {
        ProgramParser {
            sa,
            files_to_parse: VecDeque::new(),
            packages: HashMap::new(),
        }
    }

    fn parse_all(&mut self) {
        self.prepare_packages();
        self.add_all_packages();

        while let Some((file_id, file_lookup, module_path)) = self.files_to_parse.pop_front() {
            self.parse_file(file_id, file_lookup, module_path);
        }
    }

    fn prepare_packages(&mut self) {
        for (name, file) in &self.sa.args.packages {
            if self.packages.contains_key(name) {
                self.sa
                    .diag
                    .lock()
                    .report_without_location(ErrorMessage::PackageAlreadyExists(name.clone()));
            } else {
                let result = self.packages.insert(name.into(), file.clone());
                assert!(result.is_none());
            }
        }
    }

    fn add_all_packages(&mut self) {
        self.add_stdlib_package();
        self.add_boots_package();
        self.add_program_package();
        self.add_dependency_packages();
    }

    fn add_stdlib_package(&mut self) {
        let stdlib_name = "std";
        let stdlib_iname = self.sa.interner.intern(stdlib_name);
        let (package_id, module_id) = self.sa.add_package(PackageName::Stdlib, Some(stdlib_iname));
        self.sa
            .package_names
            .insert(stdlib_name.to_string(), package_id);
        self.sa.set_stdlib_module_id(module_id);
        self.sa.set_stdlib_package_id(package_id);

        if let Some(stdlib) = self.packages.remove(stdlib_name) {
            self.add_file_from_filesystem(package_id, module_id, PathBuf::from(stdlib));
        } else {
            let stdlib_file = format!("stdlib{}stdlib.dora", std::path::MAIN_SEPARATOR);
            let file_path = PathBuf::from(stdlib_file);
            let module_path = PathBuf::from(file_path.parent().expect("parent missing"));
            self.add_bundled_file(package_id, module_id, file_path, module_path);
        }
    }

    fn add_bundled_file(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_path: PathBuf,
        module_path: PathBuf,
    ) {
        let content = self.get_bundled_file(&file_path);
        self.add_file_from_string(
            package_id,
            module_id,
            file_path,
            content.into(),
            Some(module_path),
            FileLookup::Bundle,
        );
    }

    fn get_bundled_file(&self, path: &Path) -> &'static str {
        for (name, content) in STDLIB {
            if *name == path.to_string_lossy() {
                return *content;
            }
        }

        for (bundled_file_path, _) in STDLIB {
            eprintln!("\t{}", bundled_file_path);
        }
        panic!("can't find file {} in bundle.", path.display())
    }

    fn add_boots_package(&mut self) {
        let boots_name: String = "boots".into();
        if let Some(boots_path) = self.packages.remove(&boots_name) {
            let interned_boots_name = self.sa.interner.intern(&boots_name);
            let (package_id, module_id) = self
                .sa
                .add_package(PackageName::Boots, Some(interned_boots_name));
            self.sa
                .package_names
                .insert(String::from(boots_name), package_id);
            self.sa.set_boots_module_id(module_id);
            self.sa.set_boots_package_id(package_id);
            self.add_file_from_filesystem(package_id, module_id, PathBuf::from(boots_path));
        }
    }

    fn add_program_package(&mut self) {
        let (package_id, module_id) = self.sa.add_package(PackageName::Program, None);
        self.sa.set_program_module_id(module_id);
        self.sa.set_program_package_id(package_id);

        if self.sa.args.arg_file.is_none() {
            if let Some(content) = self.sa.args.test_file_as_string {
                self.add_file_from_string(
                    package_id,
                    module_id,
                    PathBuf::from("<<code>>"),
                    content.to_string(),
                    None,
                    FileLookup::FileSystem,
                );
            } else {
                self.sa
                    .diag
                    .lock()
                    .report_without_location(ErrorMessage::MissingFileArgument);
            }
        } else {
            let arg_file = self.sa.args.arg_file.as_ref().expect("argument expected");
            let arg_file = arg_file.clone();
            let path = PathBuf::from(&arg_file);

            self.add_file_from_filesystem(package_id, module_id, path);
        }
    }

    fn add_dependency_packages(&mut self) {
        let packages = std::mem::replace(&mut self.packages, HashMap::new());

        for (name, path) in packages {
            let iname = self.sa.interner.intern(&name);
            let package_name = PackageName::External(name.clone());
            let (package_id, module_id) = self.sa.add_package(package_name, Some(iname));
            self.sa.package_names.insert(name, package_id);

            self.add_file_from_filesystem(package_id, module_id, path);
        }
    }

    fn scan_file(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        module_path: Option<PathBuf>,
        file_lookup: FileLookup,
        ast: &ast::File,
    ) {
        let mut gdef = TopLevelDeclaration {
            sa: self.sa,
            package_id,
            module_id,
            file_id,
            external_modules: Vec::new(),
        };

        gdef.visit_file(ast);

        if !gdef.external_modules.is_empty() {
            for external_module_id in gdef.external_modules {
                self.add_module_files(
                    package_id,
                    external_module_id,
                    module_path.clone(),
                    file_lookup,
                );
            }
        }
    }

    fn add_module_files(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        module_path: Option<PathBuf>,
        file_lookup: FileLookup,
    ) {
        let module = self.sa.modules[module_id].clone();
        let module = module.read();
        let node = module.ast.clone().unwrap();
        let file_id = module.file_id.expect("missing file_id");

        if let Some(ident) = &node.name {
            match file_lookup {
                FileLookup::FileSystem => {
                    let module_path = module_path.expect("missing module_path");

                    let mut file_path = module_path.clone();
                    file_path.push(format!("{}.dora", ident.name_as_string));

                    let mut module_path = module_path;
                    module_path.push(&ident.name_as_string);

                    self.add_file(
                        package_id,
                        module_id,
                        file_path,
                        Some(module_path),
                        Some((file_id, node.span)),
                        FileLookup::FileSystem,
                    );
                }

                FileLookup::Bundle => {
                    let module_path = module_path.expect("missing module_path");

                    let mut file_path = module_path.clone();
                    file_path.push(format!("{}.dora", ident.name_as_string));

                    let mut module_path = module_path;
                    module_path.push(&ident.name_as_string);

                    self.add_bundled_file(package_id, module_id, file_path, module_path);
                }
            }
        }
    }

    fn add_file(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        path: PathBuf,
        module_path: Option<PathBuf>,
        error_location: Option<(SourceFileId, Span)>,
        file_lookup: FileLookup,
    ) {
        let result = file_as_string(&path);

        match result {
            Ok(content) => {
                self.add_file_from_string(
                    package_id,
                    module_id,
                    path,
                    content,
                    module_path,
                    file_lookup,
                );
            }

            Err(_) => {
                if let Some((file_id, span)) = error_location {
                    self.sa
                        .diag
                        .lock()
                        .report(file_id, span, ErrorMessage::FileNoAccess(path));
                } else {
                    self.sa
                        .diag
                        .lock()
                        .report_without_location(ErrorMessage::FileNoAccess(path));
                }
            }
        }
    }

    fn add_file_from_filesystem(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        path: PathBuf,
    ) {
        if path.is_file() {
            let file_path = PathBuf::from(path);
            let module_path = PathBuf::from(file_path.parent().expect("parent missing"));
            self.add_file(
                package_id,
                module_id,
                file_path,
                Some(module_path),
                None,
                FileLookup::FileSystem,
            );
        } else {
            self.sa
                .diag
                .lock()
                .report_without_location(ErrorMessage::FileDoesNotExist(path));
        }
    }

    fn add_file_from_string(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_path: PathBuf,
        content: String,
        module_path: Option<PathBuf>,
        file_lookup: FileLookup,
    ) {
        let file_id = self
            .sa
            .add_source_file(package_id, module_id, file_path, Arc::new(content));
        self.files_to_parse
            .push_back((file_id, file_lookup, module_path));
    }

    fn parse_file(
        &mut self,
        file_id: SourceFileId,
        file_lookup: FileLookup,
        module_path: Option<PathBuf>,
    ) {
        let file = self.sa.source_file(file_id);
        let package_id = file.package_id;
        let module_id = file.module_id;
        let content = file.content.clone();

        let parser = Parser::from_shared_string(content);

        let (ast, errors) = parser.parse();

        for error in errors {
            self.sa.diag.lock().report(
                file_id,
                error.span,
                ErrorMessage::Custom(error.error.message()),
            );
        }

        self.scan_file(
            package_id,
            module_id,
            file_id,
            module_path,
            file_lookup,
            &ast,
        );
    }
}

fn file_as_string(path: &PathBuf) -> Result<String, Error> {
    let mut content = String::new();
    let mut file = fs::File::open(&path)?;
    file.read_to_string(&mut content)?;
    Ok(content)
}

struct TopLevelDeclaration<'x> {
    sa: &'x mut Sema,
    package_id: PackageDefinitionId,
    file_id: SourceFileId,
    module_id: ModuleDefinitionId,
    external_modules: Vec<ModuleDefinitionId>,
}

impl<'x> visit::Visitor for TopLevelDeclaration<'x> {
    fn visit_extern(&mut self, node: &Arc<ast::ExternPackage>) {
        check_modifiers(self.sa, self.file_id, &node.modifiers, &[]);
        if let Some(name) = &node.name {
            if let Some(package_id) = self.sa.package_names.get(&name.name_as_string).cloned() {
                let top_level_module_id = {
                    let package = self.sa.packages.idx(package_id);
                    let package = package.read();

                    package.top_level_module_id()
                };

                let package = self.sa.packages.idx(package_id);
                let mut package = package.write();
                let iname = self.sa.interner.intern(&name.name_as_string);

                if !package.add_dependency(iname, package_id, top_level_module_id) {
                    let name = name.name_as_string.clone();
                    self.sa.diag.lock().report(
                        self.file_id,
                        node.span,
                        ErrorMessage::PackageAlreadyExists(name),
                    );
                }
            } else {
                self.sa.diag.lock().report(
                    self.file_id,
                    node.span,
                    ErrorMessage::UnknownPackage(name.name_as_string.clone()),
                );
            }
        }
    }

    fn visit_module(&mut self, node: &Arc<ast::Module>) {
        let modifiers = check_modifiers(self.sa, self.file_id, &node.modifiers, &[Annotation::Pub]);
        let name = ensure_name(self.sa, &node.name);
        let module = ModuleDefinition::new_inner(
            self.sa,
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            modifiers,
            name,
        );
        let id = self.sa.modules.push(module);
        let sym = Sym::Module(id);

        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }

        if node.elements.is_none() {
            self.external_modules.push(id);
        } else {
            let saved_module_id = self.module_id;
            self.module_id = id;
            visit::walk_module(self, node);
            self.module_id = saved_module_id;
        }
    }

    fn visit_trait(&mut self, node: &Arc<ast::Trait>) {
        let modifiers = check_modifiers(self.sa, self.file_id, &node.modifiers, &[Annotation::Pub]);

        let trait_ = TraitDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            modifiers,
            ensure_name(self.sa, &node.name),
        );
        let trait_id = self.sa.traits.push(trait_);

        find_methods_in_trait(self.sa, trait_id, node);

        let sym = Sym::Trait(trait_id);

        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }
    }

    fn visit_use(&mut self, node: &Arc<ast::Use>) {
        let modifiers = check_modifiers(self.sa, self.file_id, &node.modifiers, &[Annotation::Pub]);
        let use_def = UseDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            modifiers,
        );
        self.sa.uses.push(use_def);
    }

    fn visit_global(&mut self, node: &Arc<ast::Global>) {
        let modifiers = check_modifiers(self.sa, self.file_id, &node.modifiers, &[Annotation::Pub]);

        let global = GlobalDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            modifiers,
            ensure_name(self.sa, &node.name),
        );
        let global_id = self.sa.globals.push(global);

        let sym = Sym::Global(global_id);
        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }
    }

    fn visit_impl(&mut self, node: &Arc<ast::Impl>) {
        check_modifiers(self.sa, self.file_id, &node.modifiers, &[]);

        if node.trait_type.is_some() {
            let impl_ = ImplDefinition::new(self.package_id, self.module_id, self.file_id, node);
            let impl_id = self.sa.impls.push(impl_);

            find_methods_in_impl(self.sa, impl_id, node);
        } else {
            let extension =
                ExtensionDefinition::new(self.package_id, self.module_id, self.file_id, node);
            let extension_id = self.sa.extensions.push(extension);

            find_methods_in_extension(self.sa, extension_id, node);
        }
    }

    fn visit_const(&mut self, node: &Arc<ast::Const>) {
        let modifiers = check_modifiers(self.sa, self.file_id, &node.modifiers, &[Annotation::Pub]);
        let const_ = ConstDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            modifiers,
            ensure_name(self.sa, &node.name),
        );
        let id = self.sa.consts.push(const_);

        let sym = Sym::Const(id);
        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }
    }

    fn visit_class(&mut self, node: &Arc<ast::Class>) {
        let modifiers = check_modifiers(
            self.sa,
            self.file_id,
            &node.modifiers,
            &[Annotation::Internal, Annotation::Pub],
        );

        let mut fields = Vec::with_capacity(node.fields.len());
        let mut used_names: HashSet<Name> = HashSet::new();

        for (idx, field) in node.fields.iter().enumerate() {
            let modifiers =
                check_modifiers(self.sa, self.file_id, &field.modifiers, &[Annotation::Pub]);

            let name = self
                .sa
                .interner
                .intern(&field.name.as_ref().expect("missing name").name_as_string);

            check_if_symbol_exists(self.sa, self.file_id, &mut used_names, name, field.span);

            fields.push(Field {
                id: FieldId(idx),
                name,
                ty: SourceType::Error,
                mutable: field.mutable,
                visibility: modifiers.visibility(),
            });
        }

        let class = ClassDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            modifiers,
            ensure_name(self.sa, &node.name),
            fields,
        );
        let class_id = self.sa.classes.push(class);

        let sym = Sym::Class(class_id);
        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }

        for field in &node.fields {
            check_modifiers(self.sa, self.file_id, &field.modifiers, &[Annotation::Pub]);
        }
    }

    fn visit_struct(&mut self, node: &Arc<ast::Struct>) {
        let modifiers = check_modifiers(
            self.sa,
            self.file_id,
            &node.modifiers,
            &[Annotation::Pub, Annotation::Internal],
        );

        let mut fields = Vec::with_capacity(node.fields.len());
        let mut used_names: HashSet<Name> = HashSet::new();

        for (idx, field) in node.fields.iter().enumerate() {
            let modifiers =
                check_modifiers(self.sa, self.file_id, &field.modifiers, &[Annotation::Pub]);

            let name = self
                .sa
                .interner
                .intern(&field.name.as_ref().expect("missing name").name_as_string);

            check_if_symbol_exists(self.sa, self.file_id, &mut used_names, name, field.span);

            fields.push(StructDefinitionField {
                id: StructDefinitionFieldId(idx),
                name,
                span: field.span,
                ty: SourceType::Error,
                visibility: modifiers.visibility(),
            });
        }

        let struct_ = StructDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            modifiers,
            ensure_name(self.sa, &node.name),
            fields,
        );
        let id = self.sa.structs.push(struct_);

        let sym = Sym::Struct(id);
        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }
    }

    fn visit_fct(&mut self, node: &Arc<ast::Function>) {
        let modifiers = check_modifiers(
            self.sa,
            self.file_id,
            &node.modifiers,
            &[
                Annotation::Internal,
                Annotation::OptimizeImmediately,
                Annotation::Test,
                Annotation::Pub,
            ],
        );

        let fct = FctDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            modifiers,
            ensure_name(self.sa, &node.name),
            FctParent::None,
        );
        let fctid = self.sa.add_fct(fct);
        let sym = Sym::Fct(fctid);
        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }
    }

    fn visit_enum(&mut self, node: &Arc<ast::Enum>) {
        let modifiers = check_modifiers(self.sa, self.file_id, &node.modifiers, &[Annotation::Pub]);
        let enum_ = EnumDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            modifiers,
            ensure_name(self.sa, &node.name),
        );
        let id = self.sa.enums.push(enum_);

        let sym = Sym::Enum(id);
        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }
    }
}

fn find_methods_in_trait(sa: &mut Sema, trait_id: TraitDefinitionId, node: &Arc<ast::Trait>) {
    let trait_ = sa.traits.idx(trait_id);
    let mut trait_ = trait_.write();

    for child in &node.methods {
        match child.as_ref() {
            ast::ElemData::Function(ref method_node) => {
                let modifiers = check_modifiers(
                    sa,
                    trait_.file_id,
                    &method_node.modifiers,
                    &[Annotation::Static],
                );

                let fct = FctDefinition::new(
                    trait_.package_id,
                    trait_.module_id,
                    trait_.file_id,
                    method_node,
                    modifiers,
                    ensure_name(sa, &method_node.name),
                    FctParent::Trait(trait_id),
                );

                let fct_id = sa.add_fct(fct);
                trait_.methods.push(fct_id);
            }

            ast::ElemData::Error { .. } => {
                // ignore
            }

            _ => sa
                .diag
                .lock()
                .report(trait_.file_id, child.span(), ErrorMessage::ExpectedMethod),
        }
    }
}

fn find_methods_in_impl(sa: &mut Sema, impl_id: ImplDefinitionId, node: &Arc<ast::Impl>) {
    let impl_ = sa.impls.idx(impl_id);
    let mut impl_ = impl_.write();

    for child in &node.methods {
        match child.as_ref() {
            ast::ElemData::Function(ref method_node) => {
                let modifiers = check_modifiers(
                    sa,
                    impl_.file_id,
                    &method_node.modifiers,
                    &[Annotation::Static, Annotation::Internal],
                );

                let fct = FctDefinition::new(
                    impl_.package_id,
                    impl_.module_id,
                    impl_.file_id,
                    method_node,
                    modifiers,
                    ensure_name(sa, &method_node.name),
                    FctParent::Impl(impl_id),
                );

                let fct_id = sa.add_fct(fct);
                impl_.methods.push(fct_id);
            }

            ast::ElemData::Error { .. } => {
                // ignore
            }

            _ => sa
                .diag
                .lock()
                .report(impl_.file_id, child.span(), ErrorMessage::ExpectedMethod),
        }
    }
}

fn find_methods_in_extension(
    sa: &mut Sema,
    extension_id: ExtensionDefinitionId,
    node: &Arc<ast::Impl>,
) {
    let extension = sa.extensions.idx(extension_id);
    let mut extension = extension.write();

    for child in &node.methods {
        match child.as_ref() {
            ast::ElemData::Function(ref method_node) => {
                let modifiers = check_modifiers(
                    sa,
                    extension.file_id,
                    &method_node.modifiers,
                    &[Annotation::Internal, Annotation::Static, Annotation::Pub],
                );

                let fct = FctDefinition::new(
                    extension.package_id,
                    extension.module_id,
                    extension.file_id,
                    method_node,
                    modifiers,
                    ensure_name(sa, &method_node.name),
                    FctParent::Extension(extension_id),
                );

                let fct_id = sa.add_fct(fct);
                extension.methods.push(fct_id);
            }

            ast::ElemData::Error { .. } => {
                // ignore
            }

            _ => sa.diag.lock().report(
                extension.file_id,
                child.span(),
                ErrorMessage::ExpectedMethod,
            ),
        }
    }
}

fn ensure_name(sa: &mut Sema, ident: &Option<ast::Ident>) -> Name {
    if let Some(ident) = ident {
        sa.interner.intern(&ident.name_as_string)
    } else {
        sa.interner.intern("<missing name>")
    }
}

#[derive(Default)]
pub(crate) struct ParsedModifiers {
    pub is_pub: bool,
    pub is_static: bool,
    pub is_test: bool,
    pub is_optimize_immediately: bool,
    pub is_internal: bool,
}

impl ParsedModifiers {
    pub(crate) fn visibility(&self) -> Visibility {
        if self.is_pub {
            Visibility::Public
        } else {
            Visibility::Module
        }
    }
}

fn check_modifiers(
    sa: &Sema,
    file_id: SourceFileId,
    modifiers: &Option<Modifiers>,
    allow_list: &[Annotation],
) -> ParsedModifiers {
    let mut parsed_modifiers = ParsedModifiers::default();

    if let Some(modifiers) = modifiers {
        let mut set: HashSet<Annotation> = HashSet::new();

        for modifier in modifiers.iter() {
            let value = check_modifier(sa, file_id, modifier, &mut parsed_modifiers);

            if value.is_error() {
                continue;
            }

            if !set.insert(value) {
                sa.diag
                    .lock()
                    .report(file_id, modifier.span, ErrorMessage::RedundantAnnotation);
            }

            if !allow_list.contains(&value) {
                sa.diag.lock().report(
                    file_id,
                    modifier.span,
                    ErrorMessage::MisplacedAnnotation(value.name().into()),
                );
            }
        }
    }

    parsed_modifiers
}

fn check_modifier(
    sa: &Sema,
    file_id: SourceFileId,
    modifier: &ast::Modifier,
    parsed_modifiers: &mut ParsedModifiers,
) -> Annotation {
    if modifier.pub_token().is_some() {
        parsed_modifiers.is_pub = true;
        Annotation::Pub
    } else if modifier.static_token().is_some() {
        parsed_modifiers.is_static = true;
        Annotation::Static
    } else {
        assert!(modifier.at_token().is_some());

        if let Some(ident) = modifier.ident_token() {
            match ident.value() {
                "Test" => {
                    parsed_modifiers.is_test = true;
                    Annotation::Test
                }

                "optimizeImmediately" => {
                    parsed_modifiers.is_optimize_immediately = true;
                    Annotation::OptimizeImmediately
                }

                "internal" => {
                    parsed_modifiers.is_internal = true;
                    Annotation::Internal
                }

                _ => {
                    sa.diag.lock().report(
                        file_id,
                        ident.span(),
                        ErrorMessage::UnknownAnnotation(ident.value().into()),
                    );
                    Annotation::Error
                }
            }
        } else {
            Annotation::Error
        }
    }
}

impl<'x> TopLevelDeclaration<'x> {
    fn insert(&mut self, name: Name, sym: Sym) -> Option<Sym> {
        let level = self.sa.module_table(self.module_id);
        let mut level = level.write();
        level.insert(name, sym)
    }

    fn insert_optional(&mut self, ident: &Option<ast::Ident>, sym: Sym) -> Option<(Name, Sym)> {
        if let Some(ident) = ident {
            let name = self.sa.interner.intern(&ident.name_as_string);
            self.insert(name, sym).map(|sym| (name, sym))
        } else {
            None
        }
    }
}

fn check_if_symbol_exists(
    sa: &Sema,
    file_id: SourceFileId,
    used_names: &mut HashSet<Name>,
    name: Name,
    span: Span,
) {
    if !used_names.insert(name) {
        let name = sa.interner.str(name).to_string();
        sa.diag
            .lock()
            .report(file_id, span, ErrorMessage::ShadowField(name));
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;

    #[test]
    fn test_class() {
        err(
            "class Foo class Foo",
            (1, 11),
            ErrorMessage::ShadowClass("Foo".into()),
        );
        err(
            "fn Foo() {} class Foo",
            (1, 13),
            ErrorMessage::ShadowFunction("Foo".into()),
        );
        err(
            "class Foo fn Foo() {}",
            (1, 11),
            ErrorMessage::ShadowClass("Foo".into()),
        );
        err(
            "class Foo let Foo: Int32 = 1;",
            (1, 11),
            ErrorMessage::ShadowClass("Foo".into()),
        );
        err(
            "class Foo let mut Foo: Int32 = 1;",
            (1, 11),
            ErrorMessage::ShadowClass("Foo".into()),
        );
        err(
            "class Foo const Foo: Int32 = 1;",
            (1, 11),
            ErrorMessage::ShadowClass("Foo".into()),
        );
    }

    #[test]
    fn test_struct() {
        ok("struct Foo {}");
        err(
            "struct Foo {} struct Foo {}",
            (1, 15),
            ErrorMessage::ShadowStruct("Foo".into()),
        );
        err(
            "struct Foo {} struct Foo {}",
            (1, 15),
            ErrorMessage::ShadowStruct("Foo".into()),
        );
        err(
            "struct Foo {} class Foo",
            (1, 15),
            ErrorMessage::ShadowStruct("Foo".into()),
        );
        err(
            "fn Foo() {} struct Foo {}",
            (1, 13),
            ErrorMessage::ShadowFunction("Foo".into()),
        );
        err(
            "struct Foo {} fn Foo() {}",
            (1, 15),
            ErrorMessage::ShadowStruct("Foo".into()),
        );
        err(
            "struct Foo {} let Foo: Int32 = 1;",
            (1, 15),
            ErrorMessage::ShadowStruct("Foo".into()),
        );
        err(
            "struct Foo {} let mut Foo: Int32 = 1;",
            (1, 15),
            ErrorMessage::ShadowStruct("Foo".into()),
        );
        err(
            "struct Foo {} const Foo: Int32 = 1;",
            (1, 15),
            ErrorMessage::ShadowStruct("Foo".into()),
        );
    }

    #[test]
    fn test_trait() {
        ok("trait Foo {}");
        err(
            "trait Foo {} struct Foo {}",
            (1, 14),
            ErrorMessage::ShadowTrait("Foo".into()),
        );
        err(
            "trait Foo {} class Foo",
            (1, 14),
            ErrorMessage::ShadowTrait("Foo".into()),
        );
    }

    #[test]
    fn test_const() {
        ok("const foo: Int32 = 0i32;");
        err(
            "const foo: Int32 = 0i32; fn foo() {}",
            (1, 26),
            ErrorMessage::ShadowConst("foo".into()),
        );
        err(
            "const foo: Int32 = 0i32; class foo",
            (1, 26),
            ErrorMessage::ShadowConst("foo".into()),
        );
        err(
            "const foo: Int32 = 0i32; struct foo {}",
            (1, 26),
            ErrorMessage::ShadowConst("foo".into()),
        );
    }

    #[test]
    fn test_enum() {
        ok("enum Foo { A }");

        err(
            "enum Foo { A } class Foo",
            (1, 16),
            ErrorMessage::ShadowEnum("Foo".into()),
        );
    }

    #[test]
    fn test_mod() {
        ok("mod foo {} mod bar {}");
        ok("fn bar() {} mod foo { fn bar() {} }");

        err(
            "mod foo {} mod foo {}",
            (1, 12),
            ErrorMessage::ShadowModule("foo".into()),
        );

        err(
            "mod foo { fn bar() {} fn bar() {} }",
            (1, 23),
            ErrorMessage::ShadowFunction("bar".into()),
        );
    }
}
