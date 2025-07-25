use std::cell::OnceCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::io::{Error, Read};
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;

use crate::error::msg::ErrorMessage;
use crate::interner::Name;
use crate::sema::{
    AliasBound, AliasDefinition, AliasDefinitionId, AliasParent, ClassDefinition, ConstDefinition,
    Element, EnumDefinition, EnumField, EnumVariant, ExtensionDefinition, ExtensionDefinitionId,
    FctDefinition, FctDefinitionId, FctParent, Field, FieldId, FileContent, GlobalDefinition,
    ImplDefinition, ImplDefinitionId, ModuleDefinition, ModuleDefinitionId, PackageDefinition,
    PackageDefinitionId, PackageName, Param, Params, Sema, SourceFile, SourceFileId,
    StructDefinition, StructDefinitionField, StructDefinitionFieldId, TraitDefinition,
    TraitDefinitionId, TypeParamDefinition, UseDefinition, Visibility,
};
use crate::sym::{SymTable, Symbol, SymbolKind};
use crate::{report_sym_shadow_span, ty, ParsedType, SourceType};
use dora_parser::ast::visit::Visitor;
use dora_parser::ast::{self, visit, ModifierList};
use dora_parser::parser::Parser;
use dora_parser::{compute_line_starts, Span};

pub fn parse(sa: &mut Sema) -> HashMap<ModuleDefinitionId, SymTable> {
    let mut discoverer = ProgramParser::new(sa);
    discoverer.parse_all();
    discoverer.module_symtables
}

struct ProgramParser<'a> {
    sa: &'a mut Sema,
    worklist: VecDeque<SourceFileId>,
    packages: HashMap<String, FileContent>,
    module_symtables: HashMap<ModuleDefinitionId, SymTable>,
}

impl<'a> ProgramParser<'a> {
    fn new(sa: &mut Sema) -> ProgramParser {
        ProgramParser {
            sa,
            worklist: VecDeque::new(),
            packages: HashMap::new(),
            module_symtables: HashMap::new(),
        }
    }

    fn parse_all(&mut self) {
        self.prepare_packages();
        self.add_all_packages();

        while let Some(file_id) = self.worklist.pop_front() {
            self.parse_and_scan_file(file_id);
        }
    }

    fn prepare_packages(&mut self) {
        for (name, file) in &self.sa.flags.packages {
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
        if !self.sa.flags.is_standard_library {
            self.add_stdlib_package();
            self.add_boots_package();
        }

        self.add_program_package();
        self.add_dependency_packages();
    }

    fn add_stdlib_package(&mut self) {
        let stdlib_name = "std";
        let stdlib_iname = self.sa.interner.intern(stdlib_name);
        let (package_id, module_id) = add_package(self.sa, PackageName::Std, Some(stdlib_iname));
        self.sa
            .package_names
            .insert(stdlib_name.to_string(), package_id);
        self.sa.set_stdlib_module_id(module_id);
        self.sa.set_stdlib_package_id(package_id);

        if let Some(file_path) = self.get_stdlib_path() {
            self.add_file(package_id, module_id, file_path, None);
        } else {
            panic!("Could not find standard library.");
        }
    }

    fn get_stdlib_path(&self) -> Option<PathBuf> {
        if let Some(file_content) = self.packages.get("std") {
            Some(file_content.to_path().cloned().expect("path expected"))
        } else {
            let path = std::env::current_exe().expect("illegal path");
            let path = path.as_path();

            for ancestor in path.ancestors() {
                let stdlib_path = ancestor.join("pkgs/std/std.dora");

                if stdlib_path.exists() {
                    return Some(stdlib_path);
                }
            }

            None
        }
    }

    fn add_boots_package(&mut self) {
        if !self.sa.flags.boots {
            return;
        }

        let boots_name: String = "boots".into();
        let interned_boots_name = self.sa.interner.intern(&boots_name);
        let (package_id, module_id) =
            add_package(self.sa, PackageName::Boots, Some(interned_boots_name));
        self.sa
            .package_names
            .insert(String::from(boots_name), package_id);
        self.sa.set_boots_module_id(module_id);
        self.sa.set_boots_package_id(package_id);

        if let Some(file_path) = self.get_boots_path() {
            self.add_file(package_id, module_id, file_path, None);
        } else {
            panic!("Could not find standard library.");
        }
    }

    fn get_boots_path(&self) -> Option<PathBuf> {
        if let Some(file_content) = self.packages.get("boots") {
            Some(file_content.to_path().cloned().expect("path expected"))
        } else {
            let path = std::env::current_exe().expect("illegal path");
            let path = path.as_path();

            for ancestor in path.ancestors() {
                let stdlib_path = ancestor.join("pkgs/boots/boots.dora");

                if stdlib_path.exists() {
                    return Some(stdlib_path);
                }
            }

            None
        }
    }

    fn add_program_package(&mut self) {
        let (package_id, module_id) = add_package(self.sa, PackageName::Program, None);
        self.sa.set_program_module_id(module_id);
        self.sa.set_program_package_id(package_id);

        if self.sa.flags.is_standard_library {
            self.sa.package_names.insert("std".into(), package_id);

            self.sa.set_stdlib_module_id(module_id);
            self.sa.set_stdlib_package_id(package_id);
        }

        if let Some(ref file_content) = self.sa.flags.program_file {
            match file_content {
                FileContent::Path(path) => {
                    self.add_file(package_id, module_id, path.clone(), None);
                }

                FileContent::Content(ref content) => {
                    self.create_source_file_for_content(
                        package_id,
                        module_id,
                        PathBuf::from("<<code>>"),
                        content.to_string(),
                    );
                }
            }
        } else {
            self.sa
                .report_without_location(ErrorMessage::MissingFileArgument);
        }
    }

    fn add_dependency_packages(&mut self) {
        let packages = std::mem::replace(&mut self.packages, HashMap::new());

        for (name, file_content) in packages {
            let iname = self.sa.interner.intern(&name);
            let package_name = PackageName::External(name.clone());
            let (package_id, module_id) = add_package(self.sa, package_name, Some(iname));
            self.sa.package_names.insert(name, package_id);

            match file_content {
                FileContent::Path(path) => {
                    self.add_file(package_id, module_id, path.clone(), None);
                }

                FileContent::Content(ref content) => {
                    self.create_source_file_for_content(
                        package_id,
                        module_id,
                        PathBuf::from("<<code>>"),
                        content.to_string(),
                    );
                }
            }
        }
    }

    fn scan_file(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
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

            for external_module_id in decl_discovery.external_modules {
                self.add_external_module(package_id, module_id, external_module_id);
            }

            module_table
        };

        assert!(self
            .module_symtables
            .insert(module_id, module_table)
            .is_none());
    }

    fn add_external_module(
        &mut self,
        package_id: PackageDefinitionId,
        parent_module_id: ModuleDefinitionId,
        external_module_id: ModuleDefinitionId,
    ) {
        let external_module = self.sa.module(external_module_id);
        let node = external_module.ast.clone().unwrap();
        let file_id = external_module.file_id.expect("missing file_id");

        if let Some(ident) = &node.name {
            let parent_module = self.sa.module(parent_module_id);
            let is_top_level = parent_module.parent_module_id.is_none();

            let definition_file = self.sa.file(file_id);
            let mut file_path = definition_file.path.clone();
            assert!(file_path.pop());

            if !is_top_level {
                let name = parent_module.name.expect("missing name");
                let name = self.sa.name(name);
                file_path.push(name);
            }

            file_path.push(format!("{}.dora", ident.name_as_string));

            self.add_file(
                package_id,
                external_module_id,
                file_path,
                Some((file_id, node.span)),
            );
        }
    }

    fn add_file(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_path: PathBuf,
        error_location: Option<(SourceFileId, Span)>,
    ) {
        if file_path.exists() {
            let result = file_as_string(&file_path);

            match result {
                Ok(content) => {
                    self.create_source_file_for_content(package_id, module_id, file_path, content);
                }

                Err(_) => {
                    if let Some((file_id, span)) = error_location {
                        self.sa
                            .report(file_id, span, ErrorMessage::FileNoAccess(file_path));
                    } else {
                        self.sa
                            .report_without_location(ErrorMessage::FileNoAccess(file_path));
                    }
                }
            }
        } else {
            if let Some((file_id, span)) = error_location {
                self.sa
                    .report(file_id, span, ErrorMessage::FileDoesNotExist(file_path));
            } else {
                self.sa
                    .report_without_location(ErrorMessage::FileDoesNotExist(file_path));
            }
        }
    }

    fn create_source_file_for_content(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_path: PathBuf,
        content: String,
    ) {
        let file_id = add_source_file(self.sa, package_id, module_id, file_path, Arc::new(content));
        self.worklist.push_back(file_id);
    }

    fn parse_and_scan_file(&mut self, file_id: SourceFileId) {
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

        self.scan_file(package_id, module_id, file_id, &ast);
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
            None,
            node.type_params.as_ref(),
            node.where_bounds.as_ref(),
            Some(&node.bounds),
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
            None,
            node.type_params.as_ref(),
            node.where_bounds.as_ref(),
            None,
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

            find_elements_in_extension(self.sa, self.file_id, extension_id, node);
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

            let name = if node.field_name_style.is_positional() {
                None
            } else {
                let name = ensure_name(self.sa, &field.name);
                check_if_symbol_exists(self.sa, self.file_id, &mut used_names, name, field.span);
                Some(name)
            };

            fields.push(Field {
                id: FieldId(idx),
                name,
                parsed_ty: ParsedType::new_ast(field.data_type.clone()),
                mutable: true,
                visibility: modifiers.visibility(),
            });
        }

        let type_param_definition = parse_type_param_definition(
            self.sa,
            None,
            node.type_params.as_ref(),
            node.where_bounds.as_ref(),
            None,
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

            let name = if node.field_style.is_positional() {
                None
            } else {
                let name = ensure_name(self.sa, &field.name);
                check_if_symbol_exists(self.sa, self.file_id, &mut used_names, name, field.span);
                Some(name)
            };

            fields.push(StructDefinitionField {
                id: StructDefinitionFieldId(idx),
                name,
                span: field.span,
                parsed_ty: ParsedType::new_ast(field.data_type.clone()),
                visibility: modifiers.visibility(),
            });
        }

        let type_param_definition = parse_type_param_definition(
            self.sa,
            None,
            node.type_params.as_ref(),
            node.where_bounds.as_ref(),
            None,
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
                Annotation::Optimize,
                Annotation::Test,
                Annotation::Pub,
                Annotation::ForceInline,
                Annotation::NeverInline,
            ],
        );

        let type_param_definition = parse_type_param_definition(
            self.sa,
            None,
            node.type_params.as_ref(),
            node.where_bounds.as_ref(),
            None,
            self.file_id,
        );

        let parent = FctParent::None;
        let params = parse_function_params(self.sa, self.file_id, node, parent.clone(), &modifiers);

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

        for variant in &node.variants {
            if variant.name.is_none() {
                continue;
            }

            let name = self
                .sa
                .interner
                .intern(&variant.name.as_ref().expect("missing name").name_as_string);

            let mut fields = Vec::new();
            let mut used_names: HashSet<Name> = HashSet::new();

            for field in &variant.fields {
                let name = if variant.field_name_style.is_positional() {
                    None
                } else {
                    let name = ensure_name(self.sa, &field.name);
                    check_if_symbol_exists(
                        self.sa,
                        self.file_id,
                        &mut used_names,
                        name,
                        field.span,
                    );
                    Some(name)
                };

                let field = EnumField {
                    name,
                    parsed_type: ParsedType::new_ast(field.data_type.clone()),
                };

                fields.push(field);
            }

            let enum_variant = EnumVariant {
                id: next_variant_id,
                name: name,
                field_name_style: variant.field_name_style,
                fields,
            };

            variants.push(enum_variant);

            if name_to_value.insert(name, next_variant_id).is_some() {
                let name = self.sa.interner.str(name).to_string();
                self.sa.report(
                    self.file_id,
                    variant.span,
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
            None,
            node.type_params.as_ref(),
            node.where_bounds.as_ref(),
            None,
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

    fn visit_type_alias(&mut self, node: &Arc<ast::Alias>) {
        let modifiers = check_modifiers(self.sa, self.file_id, &node.modifiers, &[Annotation::Pub]);

        let parsed_ty = if let Some(ref ty) = node.ty {
            ParsedType::new_ast(ty.clone())
        } else {
            self.sa
                .report(self.file_id, node.span, ErrorMessage::TypeAliasMissingType);
            ParsedType::new_ty(ty::error())
        };

        let type_param_definition = parse_type_param_definition(
            self.sa,
            None,
            node.type_params.as_ref(),
            node.pre_where_bounds.as_ref(),
            None,
            self.file_id,
        );

        if let Some(ref post_where_bounds) = node.post_where_bounds {
            self.sa.report(
                self.file_id,
                post_where_bounds.span,
                ErrorMessage::UnexpectedWhere,
            );
        }

        let alias = AliasDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            AliasParent::None,
            node,
            modifiers,
            ensure_name(self.sa, &node.name),
            type_param_definition,
            Vec::new(),
            Some(parsed_ty),
            None,
        );
        let id = self.sa.aliases.alloc(alias);
        assert!(self.sa.alias(id).id.set(id).is_ok());

        if !node.bounds.is_empty() {
            self.sa
                .report(self.file_id, node.span, ErrorMessage::UnexpectedTypeBounds);
        }

        let sym = SymbolKind::Alias(id);
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

    let mut instance_names: HashMap<Name, FctDefinitionId> = HashMap::new();
    let mut static_names: HashMap<Name, FctDefinitionId> = HashMap::new();
    let mut alias_names: HashMap<Name, AliasDefinitionId> = HashMap::new();

    let mut alias_idx_in_trait = 0;

    for child in &node.methods {
        match child.as_ref() {
            ast::ElemData::Function(ref method_node) => {
                let trait_ = sa.trait_(trait_id);

                let modifiers = check_modifiers(
                    sa,
                    trait_.file_id,
                    &method_node.modifiers,
                    &[
                        Annotation::Static,
                        Annotation::Optimize,
                        Annotation::TraitObjectIgnore,
                    ],
                );

                let container_type_param_definition = trait_.type_param_definition().clone();
                let type_param_definition = parse_type_param_definition(
                    sa,
                    Some(container_type_param_definition),
                    method_node.type_params.as_ref(),
                    method_node.where_bounds.as_ref(),
                    None,
                    file_id,
                );

                let parent = FctParent::Trait(trait_id);
                let params =
                    parse_function_params(sa, file_id, method_node, parent.clone(), &modifiers);

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

                let fct = sa.fct(fct_id);

                let table = if fct.is_static {
                    &mut static_names
                } else {
                    &mut instance_names
                };

                if let Some(&existing_id) = table.get(&fct.name) {
                    let existing_fct = sa.fct(existing_id);
                    let method_name = sa.interner.str(fct.name).to_string();

                    sa.report(
                        file_id,
                        method_node.span,
                        ErrorMessage::AliasExists(method_name, existing_fct.span),
                    );
                } else {
                    assert!(table.insert(fct.name, fct_id).is_none());
                }
            }

            ast::ElemData::Alias(ref node) => {
                let modifiers = check_modifiers(sa, file_id, &node.modifiers, &[]);

                let name = ensure_name(sa, &node.name);

                let mut bounds = Vec::with_capacity(node.bounds.len());

                for ast_alias_bound in &node.bounds {
                    bounds.push(AliasBound::new(ast_alias_bound.clone()));
                }

                let where_bounds = if let Some(ref node_ty) = node.ty {
                    sa.report(
                        file_id,
                        node_ty.span(),
                        ErrorMessage::UnexpectedTypeAliasAssignment,
                    );

                    if let Some(ref pre_where_bounds) = node.pre_where_bounds {
                        sa.report(
                            file_id,
                            pre_where_bounds.span,
                            ErrorMessage::UnexpectedWhere,
                        );
                    }

                    node.post_where_bounds.as_ref()
                } else {
                    assert!(node.post_where_bounds.is_none());
                    node.pre_where_bounds.as_ref()
                };

                let container_type_param_definition =
                    sa.trait_(trait_id).type_param_definition().clone();
                let type_param_definition = parse_type_param_definition(
                    sa,
                    Some(container_type_param_definition),
                    node.type_params.as_ref(),
                    where_bounds,
                    None,
                    file_id,
                );

                let alias = AliasDefinition::new(
                    package_id,
                    module_id,
                    file_id,
                    AliasParent::Trait(trait_id),
                    node,
                    modifiers,
                    name,
                    type_param_definition,
                    bounds,
                    None,
                    Some(alias_idx_in_trait),
                );

                alias_idx_in_trait += 1;

                let id = sa.aliases.alloc(alias);
                assert!(sa.alias(id).id.set(id).is_ok());

                aliases.push(id);

                if let Some(&existing_id) = alias_names.get(&name) {
                    let existing_alias = sa.alias(existing_id);
                    let method_name = sa.interner.str(name).to_string();

                    sa.report(
                        file_id,
                        node.span,
                        ErrorMessage::TypeExists(method_name, existing_alias.node.span),
                    );
                } else {
                    alias_names.insert(name, id);
                }
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

    assert!(trait_.instance_names.set(instance_names).is_ok());
    assert!(trait_.static_names.set(static_names).is_ok());
    assert!(trait_.alias_names.set(alias_names).is_ok());
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
                let type_param_definition = parse_type_param_definition(
                    sa,
                    Some(container_type_param_definition),
                    method_node.type_params.as_ref(),
                    method_node.where_bounds.as_ref(),
                    None,
                    file_id,
                );

                let parent = FctParent::Impl(impl_id);
                let params =
                    parse_function_params(sa, file_id, method_node, parent.clone(), &modifiers);

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

            ast::ElemData::Alias(ref node) => {
                let modifiers = check_modifiers(sa, file_id, &node.modifiers, &[]);

                let name = ensure_name(sa, &node.name);

                let parsed_ty = if let Some(ref ty) = node.ty {
                    ParsedType::new_ast(ty.clone())
                } else {
                    sa.report(file_id, node.span, ErrorMessage::TypeAliasMissingType);
                    ParsedType::new_ty(ty::error())
                };

                let where_bounds = if node.ty.is_some() {
                    if let Some(ref pre_where_bounds) = node.pre_where_bounds {
                        sa.report(
                            file_id,
                            pre_where_bounds.span,
                            ErrorMessage::UnexpectedWhere,
                        );
                    }

                    node.post_where_bounds.as_ref()
                } else {
                    assert!(node.post_where_bounds.is_none());
                    node.pre_where_bounds.as_ref()
                };

                let container_type_param_definition =
                    sa.impl_(impl_id).type_param_definition().clone();
                let type_param_definition = parse_type_param_definition(
                    sa,
                    Some(container_type_param_definition),
                    node.type_params.as_ref(),
                    where_bounds,
                    None,
                    file_id,
                );

                let alias = AliasDefinition::new(
                    package_id,
                    module_id,
                    file_id,
                    AliasParent::Impl(impl_id),
                    node,
                    modifiers,
                    name,
                    type_param_definition,
                    Vec::new(),
                    Some(parsed_ty),
                    None,
                );

                let id = sa.aliases.alloc(alias);
                assert!(sa.alias(id).id.set(id).is_ok());

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

fn find_elements_in_extension(
    sa: &mut Sema,
    file_id: SourceFileId,
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
                        Annotation::Optimize,
                    ],
                );

                let container_type_param_definition = extension.type_param_definition().clone();
                let type_param_definition = parse_type_param_definition(
                    sa,
                    Some(container_type_param_definition),
                    method_node.type_params.as_ref(),
                    method_node.where_bounds.as_ref(),
                    None,
                    extension.file_id,
                );

                let parent = FctParent::Extension(extension_id);
                let params =
                    parse_function_params(sa, file_id, method_node, parent.clone(), &modifiers);

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
    pub is_trait_object_ignore: bool,
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
    Optimize,
    ForceInline,
    NeverInline,
    Error,
    TraitObjectIgnore,
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
            Annotation::Optimize => "Optimize",
            Annotation::ForceInline => "ForceInline",
            Annotation::NeverInline => "NeverInline",
            Annotation::TraitObjectIgnore => "TraitObjectIgnore",
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
                    Annotation::Optimize
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

                "TraitObjectIgnore" => {
                    parsed_modifiers.is_trait_object_ignore = true;
                    Annotation::TraitObjectIgnore
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
    parent: Option<Rc<TypeParamDefinition>>,
    ast_type_params: Option<&ast::TypeParams>,
    where_bounds: Option<&ast::WhereBounds>,
    trait_bounds: Option<&Vec<ast::Type>>,
    file_id: SourceFileId,
) -> Rc<TypeParamDefinition> {
    let mut type_param_definition = TypeParamDefinition::new(parent);

    if let Some(ast_type_params) = ast_type_params {
        if ast_type_params.params.len() == 0 {
            let msg = ErrorMessage::TypeParamsExpected;
            sa.report(file_id, ast_type_params.span, msg);
        }

        let mut names = HashSet::new();

        for type_param in ast_type_params.params.iter() {
            let id = if let Some(ref ident) = type_param.name {
                let iname = sa.interner.intern(&ident.name_as_string);

                if !names.insert(iname) {
                    let name = ident.name_as_string.clone();
                    let msg = ErrorMessage::TypeParamNameNotUnique(name);
                    sa.report(file_id, type_param.span, msg);
                }

                type_param_definition.add_type_param(iname)
            } else {
                let name = sa.interner.intern("<missing name>");
                type_param_definition.add_type_param(name)
            };

            for bound in &type_param.bounds {
                type_param_definition.add_type_param_bound(id, bound.clone());
            }
        }
    }

    if let Some(where_bounds) = where_bounds {
        for clause in where_bounds.clauses.iter() {
            for bound in &clause.bounds {
                type_param_definition.add_where_bound(clause.ty.clone(), bound.clone());
            }
        }
    }

    if let Some(trait_bounds) = trait_bounds {
        for bound in trait_bounds {
            type_param_definition.add_self_bound(bound.clone());
        }
    }

    Rc::new(type_param_definition)
}

fn parse_function_params(
    sa: &Sema,
    file_id: SourceFileId,
    ast: &ast::Function,
    parent: FctParent,
    modifiers: &ParsedModifierList,
) -> Params {
    let mut params: Vec<Param> = Vec::new();
    let mut has_self = false;

    match parent {
        FctParent::Impl(..) | FctParent::Extension(..) | FctParent::Trait(..) => {
            if !modifiers.is_static {
                has_self = true;
                params.push(Param::new_ty(SourceType::This));
            }
        }

        FctParent::None => {}

        FctParent::Function => unreachable!(),
    }

    let mut is_variadic = false;

    for (idx, ast_param) in ast.params.iter().enumerate() {
        if ast_param.variadic {
            if idx + 1 == ast.params.len() {
                is_variadic = true;
            } else {
                sa.report(
                    file_id,
                    ast_param.span,
                    ErrorMessage::VariadicParameterNeedsToBeLast,
                );
            }
        }

        let param = Param::new(ast_param.clone());
        params.push(param);
    }

    Params::new(params, has_self, is_variadic)
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
