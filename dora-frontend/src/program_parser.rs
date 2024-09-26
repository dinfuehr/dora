use std::cell::OnceCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::io::{Error, Read};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::error::msg::ErrorMessage;
use crate::interner::Name;
use crate::report_sym_shadow_span;
use crate::sema::{
    AliasBound, AliasDefinition, AliasParent, ClassDefinition, ConstDefinition, EnumDefinition,
    EnumVariant, ExtensionDefinition, ExtensionDefinitionId, FctDefinition, FctParent, Field,
    FieldId, GlobalDefinition, ImplDefinition, ImplDefinitionId, ModuleDefinition,
    ModuleDefinitionId, PackageDefinition, PackageDefinitionId, PackageName, Param, Sema,
    SourceFile, SourceFileId, StructDefinition, StructDefinitionField, StructDefinitionFieldId,
    TraitDefinition, TraitDefinitionId, TypeParamDefinition, TypeParamId, UseDefinition,
    Visibility,
};
use crate::sym::{SymTable, Symbol, SymbolKind};
use crate::STDLIB;
use dora_parser::ast::visit::Visitor;
use dora_parser::ast::{self, visit, ModifierList};
use dora_parser::parser::Parser;
use dora_parser::{compute_line_starts, Span};

pub fn parse(sa: &mut Sema) -> HashMap<ModuleDefinitionId, SymTable> {
    let mut discoverer = ProgramParser::new(sa);
    discoverer.parse_all();
    discoverer.module_symtables
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
    module_symtables: HashMap<ModuleDefinitionId, SymTable>,
}

impl<'a> ProgramParser<'a> {
    fn new(sa: &mut Sema) -> ProgramParser {
        ProgramParser {
            sa,
            files_to_parse: VecDeque::new(),
            packages: HashMap::new(),
            module_symtables: HashMap::new(),
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
        let (package_id, module_id) = add_package(self.sa, PackageName::Stdlib, Some(stdlib_iname));
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
            let (package_id, module_id) =
                add_package(self.sa, PackageName::Boots, Some(interned_boots_name));
            self.sa
                .package_names
                .insert(String::from(boots_name), package_id);
            self.sa.set_boots_module_id(module_id);
            self.sa.set_boots_package_id(package_id);
            self.add_file_from_filesystem(package_id, module_id, PathBuf::from(boots_path));
        }
    }

    fn add_program_package(&mut self) {
        let (package_id, module_id) = add_package(self.sa, PackageName::Program, None);
        self.sa.set_program_module_id(module_id);
        self.sa.set_program_package_id(package_id);

        if self.sa.args.arg_file.is_none() {
            if let Some(ref content) = self.sa.args.test_file_as_string {
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
            let (package_id, module_id) = add_package(self.sa, package_name, Some(iname));
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
        let module_table = {
            let mut decl_discovery = TopLevelDeclaration {
                sa: self.sa,
                package_id,
                module_id,
                file_id,
                external_modules: Vec::new(),
                module_table: SymTable::new(),
                module_symtables: &mut self.module_symtables,
            };

            decl_discovery.visit_file(ast);

            let module_table = decl_discovery.module_table;

            if !decl_discovery.external_modules.is_empty() {
                for external_module_id in decl_discovery.external_modules {
                    self.add_module_files(
                        package_id,
                        external_module_id,
                        module_path.clone(),
                        file_lookup,
                    );
                }
            }

            module_table
        };

        assert!(self
            .module_symtables
            .insert(module_id, module_table)
            .is_none());
    }

    fn add_module_files(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        module_path: Option<PathBuf>,
        file_lookup: FileLookup,
    ) {
        let module = self.sa.module(module_id);
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
                        .report(file_id, span, ErrorMessage::FileNoAccess(path));
                } else {
                    self.sa
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
        let file_id = add_source_file(self.sa, package_id, module_id, file_path, Arc::new(content));
        self.files_to_parse
            .push_back((file_id, file_lookup, module_path));
    }

    fn parse_file(
        &mut self,
        file_id: SourceFileId,
        file_lookup: FileLookup,
        module_path: Option<PathBuf>,
    ) {
        let file = self.sa.file(file_id);
        let package_id = file.package_id;
        let module_id = file.module_id;
        let content = file.content.clone();

        let parser = Parser::from_shared_string(content);

        let (ast, errors) = parser.parse();

        for error in errors {
            self.sa.report(
                file_id,
                error.span,
                ErrorMessage::Custom(error.error.message()),
            );
        }

        assert!(self.sa.file(file_id).ast.set(ast.clone()).is_ok());

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
    module_table: SymTable,
    module_symtables: &'x mut HashMap<ModuleDefinitionId, SymTable>,
}

impl<'x> visit::Visitor for TopLevelDeclaration<'x> {
    fn visit_extern(&mut self, node: &Arc<ast::ExternPackage>) {
        check_modifiers(self.sa, self.file_id, &node.modifiers, &[]);
        if let Some(name) = &node.name {
            if let Some(package_id) = self.sa.package_names.get(&name.name_as_string).cloned() {
                let top_level_module_id = self.sa.packages[package_id].top_level_module_id();

                let iname = self.sa.interner.intern(&name.name_as_string);

                if !self.sa.packages[package_id].add_dependency(
                    iname,
                    package_id,
                    top_level_module_id,
                ) {
                    let name = name.name_as_string.clone();
                    self.sa.report(
                        self.file_id,
                        node.span,
                        ErrorMessage::PackageAlreadyExists(name),
                    );
                }
            } else {
                self.sa.report(
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
        let id = self.sa.modules.alloc(module);
        self.sa.modules[id].id = Some(id);
        let sym = SymbolKind::Module(id);

        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }

        if node.elements.is_none() {
            self.external_modules.push(id);
        } else {
            let module_table = SymTable::new();
            let saved_module_id = self.module_id;

            let saved_module_table = std::mem::replace(&mut self.module_table, module_table);
            self.module_id = id;
            visit::walk_module(self, node);
            self.module_id = saved_module_id;
            let module_table = std::mem::replace(&mut self.module_table, saved_module_table);

            assert!(self.module_symtables.insert(id, module_table).is_none());
        }
    }

    fn visit_trait(&mut self, node: &Arc<ast::Trait>) {
        let modifiers = check_modifiers(self.sa, self.file_id, &node.modifiers, &[Annotation::Pub]);

        let type_param_definition = parse_type_param_definition(
            self.sa,
            TypeParamDefinition::new(),
            node.type_params.as_ref(),
            node.where_bounds.as_ref(),
            self.file_id,
        );

        let trait_ = TraitDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            modifiers,
            ensure_name(self.sa, &node.name),
            type_param_definition,
        );
        let trait_id = self.sa.traits.alloc(trait_);
        self.sa.traits[trait_id].id = Some(trait_id);

        find_elements_in_trait(
            self.sa,
            self.package_id,
            self.module_id,
            self.file_id,
            trait_id,
            node,
        );

        let sym = SymbolKind::Trait(trait_id);

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
            &node.path,
            modifiers,
        );
        let use_id = self.sa.uses.alloc(use_def);
        assert!(self.sa.uses[use_id].id.set(use_id).is_ok());
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
        let global_id = self.sa.globals.alloc(global);
        self.sa.globals[global_id].id = Some(global_id);

        let sym = SymbolKind::Global(global_id);
        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }
    }

    fn visit_impl(&mut self, node: &Arc<ast::Impl>) {
        check_modifiers(self.sa, self.file_id, &node.modifiers, &[]);

        let type_param_definition = parse_type_param_definition(
            self.sa,
            TypeParamDefinition::new(),
            node.type_params.as_ref(),
            node.where_bounds.as_ref(),
            self.file_id,
        );

        if node.trait_type.is_some() {
            let impl_ = ImplDefinition::new(
                self.package_id,
                self.module_id,
                self.file_id,
                node,
                type_param_definition,
            );
            let impl_id = self.sa.impls.alloc(impl_);
            assert!(self.sa.impls[impl_id].id.set(impl_id).is_ok());

            find_elements_in_impl(
                self.sa,
                self.package_id,
                self.module_id,
                self.file_id,
                impl_id,
                node,
            );
        } else {
            let extension = ExtensionDefinition::new(
                self.package_id,
                self.module_id,
                self.file_id,
                node,
                type_param_definition,
            );
            let extension_id = self.sa.extensions.alloc(extension);
            assert!(self.sa.extensions[extension_id]
                .id
                .set(extension_id)
                .is_ok());

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
        let id = self.sa.consts.alloc(const_);
        self.sa.consts[id].id = Some(id);

        let sym = SymbolKind::Const(id);
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

            let name = ensure_name(self.sa, &field.name);

            check_if_symbol_exists(self.sa, self.file_id, &mut used_names, name, field.span);

            fields.push(Field {
                id: FieldId(idx),
                name,
                ty: OnceCell::new(),
                mutable: field.mutable,
                visibility: modifiers.visibility(),
            });
        }

        let type_param_definition = parse_type_param_definition(
            self.sa,
            TypeParamDefinition::new(),
            node.type_params.as_ref(),
            node.where_bounds.as_ref(),
            self.file_id,
        );

        let class = ClassDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            modifiers,
            ensure_name(self.sa, &node.name),
            type_param_definition,
            fields,
        );
        let class_id = self.sa.classes.alloc(class);
        self.sa.classes[class_id].id = Some(class_id);

        let sym = SymbolKind::Class(class_id);
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

            let name = ensure_name(self.sa, &field.name);

            check_if_symbol_exists(self.sa, self.file_id, &mut used_names, name, field.span);

            fields.push(StructDefinitionField {
                id: StructDefinitionFieldId(idx),
                name,
                span: field.span,
                ty: OnceCell::new(),
                visibility: modifiers.visibility(),
            });
        }

        let type_param_definition = parse_type_param_definition(
            self.sa,
            TypeParamDefinition::new(),
            node.type_params.as_ref(),
            node.where_bounds.as_ref(),
            self.file_id,
        );

        let struct_ = StructDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            modifiers,
            ensure_name(self.sa, &node.name),
            type_param_definition,
            fields,
        );
        let id = self.sa.structs.alloc(struct_);
        self.sa.structs[id].id = Some(id);

        let sym = SymbolKind::Struct(id);
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
                Annotation::ForceInline,
                Annotation::NeverInline,
            ],
        );

        let type_param_definition = parse_type_param_definition(
            self.sa,
            TypeParamDefinition::new(),
            node.type_params.as_ref(),
            node.where_bounds.as_ref(),
            self.file_id,
        );

        let parent = FctParent::None;
        let params = parse_function_params(self.sa, node, parent.clone(), &modifiers);

        let fct = FctDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            modifiers,
            ensure_name(self.sa, &node.name),
            type_param_definition,
            params,
            parent,
        );
        let fct_id = self.sa.fcts.alloc(fct);
        self.sa.fcts[fct_id].id = Some(fct_id);
        let sym = SymbolKind::Fct(fct_id);
        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }
    }

    fn visit_enum(&mut self, node: &Arc<ast::Enum>) {
        let mut next_variant_id: u32 = 0;
        let mut variants = Vec::new();
        let mut name_to_value = HashMap::new();

        for value in &node.variants {
            if value.name.is_none() {
                continue;
            }

            let name = self
                .sa
                .interner
                .intern(&value.name.as_ref().expect("missing name").name_as_string);

            let variant = EnumVariant {
                id: next_variant_id,
                name: name,
                types: OnceCell::new(),
            };

            variants.push(variant);

            if name_to_value.insert(name, next_variant_id).is_some() {
                let name = self.sa.interner.str(name).to_string();
                self.sa.report(
                    self.file_id,
                    value.span,
                    ErrorMessage::ShadowEnumVariant(name),
                );
            }

            next_variant_id += 1;
        }

        if node.variants.is_empty() {
            self.sa
                .report(self.file_id, node.span, ErrorMessage::NoEnumVariant);
        }

        let type_param_definition = parse_type_param_definition(
            self.sa,
            TypeParamDefinition::new(),
            node.type_params.as_ref(),
            node.where_bounds.as_ref(),
            self.file_id,
        );

        let modifiers = check_modifiers(self.sa, self.file_id, &node.modifiers, &[Annotation::Pub]);
        let enum_ = EnumDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            modifiers,
            ensure_name(self.sa, &node.name),
            type_param_definition,
            variants,
            name_to_value,
        );
        let id = self.sa.enums.alloc(enum_);
        self.sa.enums[id].id = Some(id);

        let sym = SymbolKind::Enum(id);
        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }
    }

    fn visit_type_alias(&mut self, node: &Arc<ast::TypeAlias>) {
        let modifiers = check_modifiers(self.sa, self.file_id, &node.modifiers, &[Annotation::Pub]);

        let alias = AliasDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            AliasParent::None,
            node,
            modifiers,
            ensure_name(self.sa, &node.name),
            Vec::new(),
        );
        let id = self.sa.aliases.alloc(alias);
        assert!(self.sa.alias(id).id.set(id).is_ok());

        if node.ty.is_none() {
            self.sa
                .report(self.file_id, node.span, ErrorMessage::TypeAliasMissingType);
        }

        if !node.bounds.is_empty() {
            self.sa
                .report(self.file_id, node.span, ErrorMessage::UnexpectedTypeBounds);
        }

        let sym = SymbolKind::TypeAlias(id);
        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }
    }
}

fn find_elements_in_trait(
    sa: &mut Sema,
    package_id: PackageDefinitionId,
    module_id: ModuleDefinitionId,
    file_id: SourceFileId,
    trait_id: TraitDefinitionId,
    node: &Arc<ast::Trait>,
) {
    let mut methods = Vec::new();
    let mut aliases = Vec::new();

    for child in &node.methods {
        match child.as_ref() {
            ast::ElemData::Function(ref method_node) => {
                let trait_ = sa.trait_(trait_id);

                let modifiers = check_modifiers(
                    sa,
                    trait_.file_id,
                    &method_node.modifiers,
                    &[Annotation::Static, Annotation::OptimizeImmediately],
                );

                let container_type_param_definition = trait_.type_param_definition().clone();
                container_type_param_definition.set_container_type_params();
                let type_param_definition = parse_type_param_definition(
                    sa,
                    container_type_param_definition,
                    method_node.type_params.as_ref(),
                    method_node.where_bounds.as_ref(),
                    file_id,
                );

                let parent = FctParent::Trait(trait_id);
                let params = parse_function_params(sa, method_node, parent.clone(), &modifiers);

                let fct = FctDefinition::new(
                    trait_.package_id,
                    trait_.module_id,
                    trait_.file_id,
                    method_node,
                    modifiers,
                    ensure_name(sa, &method_node.name),
                    type_param_definition,
                    params,
                    parent,
                );

                let fct_id = sa.fcts.alloc(fct);
                sa.fcts[fct_id].id = Some(fct_id);
                methods.push(fct_id);
            }

            ast::ElemData::TypeAlias(ref node) => {
                let modifiers = check_modifiers(sa, file_id, &node.modifiers, &[]);

                let name = ensure_name(sa, &node.name);

                let mut bounds = Vec::with_capacity(node.bounds.len());

                for ast_alias_bound in &node.bounds {
                    bounds.push(AliasBound::new(ast_alias_bound.clone()));
                }

                let alias = AliasDefinition::new(
                    package_id,
                    module_id,
                    file_id,
                    AliasParent::Trait(trait_id),
                    node,
                    modifiers,
                    name,
                    bounds,
                );

                let id = sa.aliases.alloc(alias);
                assert!(sa.alias(id).id.set(id).is_ok());

                if node.ty.is_some() {
                    sa.report(
                        file_id,
                        node.span,
                        ErrorMessage::UnexpectedTypeAliasAssignment,
                    )
                }

                aliases.push(id);
            }

            ast::ElemData::Error { .. } => {
                // ignore
            }

            _ => sa.report(
                sa.trait_(trait_id).file_id,
                child.span(),
                ErrorMessage::ExpectedMethod,
            ),
        }
    }

    let trait_ = sa.trait_(trait_id);
    assert!(trait_.methods.set(methods).is_ok());
    assert!(trait_.aliases.set(aliases).is_ok());
}

fn find_elements_in_impl(
    sa: &mut Sema,
    package_id: PackageDefinitionId,
    module_id: ModuleDefinitionId,
    file_id: SourceFileId,
    impl_id: ImplDefinitionId,
    node: &Arc<ast::Impl>,
) {
    let mut methods = Vec::new();
    let mut aliases = Vec::new();

    for child in &node.methods {
        match child.as_ref() {
            ast::ElemData::Function(ref method_node) => {
                let impl_ = &sa.impl_(impl_id);
                let modifiers = check_modifiers(
                    sa,
                    impl_.file_id,
                    &method_node.modifiers,
                    &[Annotation::Static, Annotation::Internal],
                );

                let container_type_param_definition = impl_.type_param_definition().clone();
                container_type_param_definition.set_container_type_params();
                let type_param_definition = parse_type_param_definition(
                    sa,
                    container_type_param_definition,
                    method_node.type_params.as_ref(),
                    method_node.where_bounds.as_ref(),
                    file_id,
                );

                let parent = FctParent::Impl(impl_id);
                let params = parse_function_params(sa, method_node, parent.clone(), &modifiers);

                let fct = FctDefinition::new(
                    impl_.package_id,
                    impl_.module_id,
                    impl_.file_id,
                    method_node,
                    modifiers,
                    ensure_name(sa, &method_node.name),
                    type_param_definition,
                    params,
                    parent,
                );

                let fct_id = sa.fcts.alloc(fct);
                sa.fcts[fct_id].id = Some(fct_id);
                methods.push(fct_id);
            }

            ast::ElemData::TypeAlias(ref node) => {
                let modifiers = check_modifiers(sa, file_id, &node.modifiers, &[]);

                let name = ensure_name(sa, &node.name);

                let alias = AliasDefinition::new(
                    package_id,
                    module_id,
                    file_id,
                    AliasParent::Impl(impl_id),
                    node,
                    modifiers,
                    name,
                    Vec::new(),
                );

                let id = sa.aliases.alloc(alias);
                assert!(sa.alias(id).id.set(id).is_ok());

                if node.ty.is_none() {
                    sa.report(file_id, node.span, ErrorMessage::TypeAliasMissingType);
                }

                if !node.bounds.is_empty() {
                    sa.report(file_id, node.span, ErrorMessage::UnexpectedTypeBounds);
                }

                aliases.push(id);
            }

            ast::ElemData::Error { .. } => {
                // ignore
            }

            _ => sa.report(
                sa.impl_(impl_id).file_id,
                child.span(),
                ErrorMessage::ExpectedMethod,
            ),
        }
    }

    let impl_ = &sa.impls[impl_id];
    assert!(impl_.methods.set(methods).is_ok());
    assert!(impl_.aliases.set(aliases).is_ok());
}

fn find_methods_in_extension(
    sa: &mut Sema,
    extension_id: ExtensionDefinitionId,
    node: &Arc<ast::Impl>,
) {
    let mut methods = Vec::new();

    for child in &node.methods {
        match child.as_ref() {
            ast::ElemData::Function(ref method_node) => {
                let name = ensure_name(sa, &method_node.name);
                let extension = sa.extension(extension_id);
                let modifiers = check_modifiers(
                    sa,
                    extension.file_id,
                    &method_node.modifiers,
                    &[
                        Annotation::Internal,
                        Annotation::Static,
                        Annotation::Pub,
                        Annotation::OptimizeImmediately,
                    ],
                );

                let container_type_param_definition = extension.type_param_definition().clone();
                container_type_param_definition.set_container_type_params();
                let type_param_definition = parse_type_param_definition(
                    sa,
                    container_type_param_definition,
                    method_node.type_params.as_ref(),
                    method_node.where_bounds.as_ref(),
                    extension.file_id,
                );

                let parent = FctParent::Extension(extension_id);
                let params = parse_function_params(sa, method_node, parent.clone(), &modifiers);

                let fct = FctDefinition::new(
                    extension.package_id,
                    extension.module_id,
                    extension.file_id,
                    method_node,
                    modifiers,
                    name,
                    type_param_definition,
                    params,
                    parent,
                );

                let fct_id = sa.fcts.alloc(fct);
                sa.fcts[fct_id].id = Some(fct_id);
                methods.push(fct_id);
            }

            ast::ElemData::Error { .. } => {
                // ignore
            }

            _ => {
                sa.report(
                    sa.extensions[extension_id].file_id,
                    child.span(),
                    ErrorMessage::ExpectedMethod,
                );
            }
        }
    }

    let extension = sa.extension(extension_id);
    assert!(extension.methods.set(methods).is_ok());
}

fn ensure_name(sa: &Sema, ident: &Option<ast::Ident>) -> Name {
    if let Some(ident) = ident {
        sa.interner.intern(&ident.name_as_string)
    } else {
        sa.interner.intern("<missing name>")
    }
}

#[derive(Default)]
pub struct ParsedModifierList {
    pub is_pub: bool,
    pub is_static: bool,
    pub is_test: bool,
    pub is_optimize_immediately: bool,
    pub is_internal: bool,
    pub is_force_inline: bool,
    pub is_never_inline: bool,
}

impl ParsedModifierList {
    pub(crate) fn visibility(&self) -> Visibility {
        if self.is_pub {
            Visibility::Public
        } else {
            Visibility::Module
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum Annotation {
    Internal,
    Pub,
    Static,
    Test,
    OptimizeImmediately,
    ForceInline,
    NeverInline,
    Error,
}

impl Annotation {
    fn is_error(&self) -> bool {
        *self == Annotation::Error
    }

    fn name(&self) -> &'static str {
        match *self {
            Annotation::Internal => "internal",
            Annotation::Pub => "pub",
            Annotation::Static => "static",
            Annotation::Test => "test",
            Annotation::OptimizeImmediately => "Optimize",
            Annotation::ForceInline => "ForceInline",
            Annotation::NeverInline => "NeverInline",
            Annotation::Error => "<error>",
        }
    }
}

fn check_modifiers(
    sa: &Sema,
    file_id: SourceFileId,
    modifiers: &Option<ModifierList>,
    allow_list: &[Annotation],
) -> ParsedModifierList {
    let mut parsed_modifiers = ParsedModifierList::default();

    if let Some(modifiers) = modifiers {
        let mut set: HashSet<Annotation> = HashSet::new();

        for modifier in modifiers.iter() {
            let value = check_modifier(sa, file_id, modifier, &mut parsed_modifiers);

            if value.is_error() {
                continue;
            }

            if !set.insert(value) {
                sa.report(file_id, modifier.span, ErrorMessage::RedundantAnnotation);
            }

            if !allow_list.contains(&value) {
                sa.report(
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
    parsed_modifiers: &mut ParsedModifierList,
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

                "Optimize" => {
                    parsed_modifiers.is_optimize_immediately = true;
                    Annotation::OptimizeImmediately
                }

                "internal" => {
                    parsed_modifiers.is_internal = true;
                    Annotation::Internal
                }

                "ForceInline" => {
                    parsed_modifiers.is_force_inline = true;
                    Annotation::ForceInline
                }

                "NeverInline" => {
                    parsed_modifiers.is_never_inline = true;
                    Annotation::NeverInline
                }

                _ => {
                    sa.report(
                        file_id,
                        modifier.span,
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
    fn insert(&mut self, name: Name, sym: SymbolKind) -> Option<Symbol> {
        self.module_table.insert(name, sym)
    }

    fn insert_optional(
        &mut self,
        ident: &Option<ast::Ident>,
        sym: SymbolKind,
    ) -> Option<(Name, Symbol)> {
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
        sa.report(file_id, span, ErrorMessage::ShadowField(name));
    }
}

pub fn add_source_file(
    sa: &mut Sema,
    package_id: PackageDefinitionId,
    module_id: ModuleDefinitionId,
    path: PathBuf,
    content: Arc<String>,
) -> SourceFileId {
    let line_starts = compute_line_starts(&content);
    let file_id = sa.source_files.alloc(SourceFile {
        id: OnceCell::new(),
        package_id,
        path,
        content,
        module_id,
        line_starts,
        ast: OnceCell::new(),
    });
    assert!(sa.file(file_id).id.set(file_id).is_ok());
    file_id
}

fn add_package(
    sa: &mut Sema,
    package_name: PackageName,
    module_name: Option<Name>,
) -> (PackageDefinitionId, ModuleDefinitionId) {
    let module = ModuleDefinition::new_top_level(module_name);
    let module_id = sa.modules.alloc(module);

    let package = PackageDefinition::new(package_name, module_id);
    let package_id = sa.packages.alloc(package);

    sa.modules[module_id].package_id = Some(package_id);

    (package_id, module_id)
}

fn parse_type_param_definition(
    sa: &Sema,
    mut type_param_definition: TypeParamDefinition,
    ast_type_params: Option<&ast::TypeParams>,
    where_bounds: Option<&ast::WhereBounds>,
    file_id: SourceFileId,
) -> TypeParamDefinition {
    if ast_type_params.is_none() {
        return type_param_definition;
    }

    let ast_type_params = ast_type_params.expect("type params expected");

    if ast_type_params.params.len() == 0 {
        let msg = ErrorMessage::TypeParamsExpected;
        sa.report(file_id, ast_type_params.span, msg);
    }

    let mut names = HashSet::new();
    let container_type_params = type_param_definition.len();

    for (id, type_param) in ast_type_params.params.iter().enumerate() {
        if let Some(ref ident) = type_param.name {
            let iname = sa.interner.intern(&ident.name_as_string);

            if !names.insert(iname) {
                let name = ident.name_as_string.clone();
                let msg = ErrorMessage::TypeParamNameNotUnique(name);
                sa.report(file_id, type_param.span, msg);
            }

            type_param_definition.add_type_param(iname);
        } else {
            let name = sa.interner.intern("<missing name>");
            type_param_definition.add_type_param(name);
        }

        let id = TypeParamId(container_type_params + id);

        for bound in &type_param.bounds {
            type_param_definition.add_bound2(id, bound.clone());
        }
    }

    if let Some(where_bounds) = where_bounds {
        for clause in where_bounds.clauses.iter() {
            for bound in &clause.bounds {
                type_param_definition.add_where_bound2(clause.ty.clone(), bound.clone());
            }
        }
    }

    type_param_definition
}

fn parse_function_params(
    _sa: &Sema,
    ast: &ast::Function,
    parent: FctParent,
    modifiers: &ParsedModifierList,
) -> Vec<Param> {
    let mut params: Vec<Param> = Vec::new();

    match parent {
        FctParent::Impl(..) | FctParent::Extension(..) | FctParent::Trait(..) => {
            if !modifiers.is_static {
                params.push(Param::new_uninitialized());
            }
        }

        FctParent::None => {}

        FctParent::Function => unreachable!(),
    }

    for p in &ast.params {
        let param = Param::new(p.clone());
        params.push(param);
    }

    params
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
