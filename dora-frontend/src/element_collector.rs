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
    Element, ElementId, EnumDefinition, ExtensionDefinition, ExtensionDefinitionId, FctDefinition,
    FctDefinitionId, FctParent, FieldDefinition, FieldIndex, GlobalDefinition, ImplDefinition,
    ImplDefinitionId, ModuleDefinition, ModuleDefinitionId, PackageDefinition, PackageDefinitionId,
    PackageName, Param, Params, Sema, SourceFile, SourceFileId, StructDefinition, ToArcString,
    TraitDefinition, TraitDefinitionId, TypeParamDefinition, UseDefinition, VariantDefinition,
    Visibility,
};
use crate::sym::{SymTable, Symbol, SymbolKind};
use crate::{ParsedType, SourceType, report_sym_shadow_span, ty};
use dora_parser::ast::{self, SyntaxNodeBase};
use dora_parser::parser::Parser;
use dora_parser::{Span, TokenKind, compute_line_starts};

pub fn collect_elements(sa: &mut Sema) -> HashMap<ModuleDefinitionId, SymTable> {
    let mut collector = ElementCollector::new(sa);
    collector.collect_all();
    collector.module_symtables
}

pub fn collect_elements_for_single_file(sa: &mut Sema) -> SourceFileId {
    let mut collector = ElementCollector::new_with_params(sa, true);
    collector.collect_one()
}

pub fn collect_elements_for_package(sa: &mut Sema) {
    let mut collector = ElementCollector::new(sa);
    collector.collect_project()
}

struct ElementCollector<'a> {
    sa: &'a mut Sema,
    worklist: VecDeque<SourceFileId>,
    packages: HashMap<String, PathBuf>,
    module_symtables: HashMap<ModuleDefinitionId, SymTable>,
    is_collect_single_file: bool,
}

impl<'a> ElementCollector<'a> {
    fn new(sa: &mut Sema) -> ElementCollector<'_> {
        ElementCollector::new_with_params(sa, false)
    }

    fn new_with_params(sa: &mut Sema, is_collect_single_file: bool) -> ElementCollector<'_> {
        ElementCollector {
            sa,
            worklist: VecDeque::new(),
            packages: HashMap::new(),
            module_symtables: HashMap::new(),
            is_collect_single_file,
        }
    }

    fn collect_all(&mut self) {
        self.prepare_packages();
        self.add_all_packages();

        while let Some(file_id) = self.worklist.pop_front() {
            self.parse_and_collect_file(file_id);
        }
    }

    fn collect_one(&mut self) -> SourceFileId {
        self.add_program_package();
        let file_id = self.worklist.pop_front().expect("missing file");
        self.parse_and_collect_file(file_id);
        file_id
    }

    fn collect_project(&mut self) {
        self.add_program_package();

        while let Some(file_id) = self.worklist.pop_front() {
            self.parse_and_collect_file(file_id);
        }
    }

    fn prepare_packages(&mut self) {
        for (name, file) in &self.sa.package_contents {
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
        if !self.sa.is_standard_library {
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

        let path = self.get_stdlib_path();
        self.add_file(package_id, module_id, path, None);
    }

    fn get_stdlib_path(&self) -> PathBuf {
        if let Some(file_content) = self.packages.get("std") {
            file_content.clone()
        } else {
            self.sa.pkgs_directory.join("std/std.dora")
        }
    }

    fn add_boots_package(&mut self) {
        if !self.sa.include_boots {
            return;
        }

        let boots_name: String = "boots".to_string();
        let interned_boots_name = self.sa.interner.intern(&boots_name);
        let (package_id, module_id) =
            add_package(self.sa, PackageName::Boots, Some(interned_boots_name));
        self.sa
            .package_names
            .insert(String::from(boots_name), package_id);
        self.sa.set_boots_module_id(module_id);
        self.sa.set_boots_package_id(package_id);

        let file_path = self.get_boots_path();
        self.add_file(package_id, module_id, file_path, None);
    }

    fn get_boots_path(&self) -> PathBuf {
        if let Some(file_content) = self.packages.get("boots") {
            file_content.clone()
        } else {
            self.sa.pkgs_directory.join("boots/boots.dora")
        }
    }

    fn add_program_package(&mut self) {
        let (package_id, module_id) = add_package(self.sa, PackageName::Program, None);
        self.sa.set_program_module_id(module_id);
        self.sa.set_program_package_id(package_id);

        if self.sa.is_standard_library {
            self.sa.package_names.insert("std".to_string(), package_id);

            self.sa.set_stdlib_module_id(module_id);
            self.sa.set_stdlib_package_id(package_id);
        }

        self.add_file(package_id, module_id, self.sa.program_file.clone(), None);
    }

    fn add_dependency_packages(&mut self) {
        let packages = std::mem::replace(&mut self.packages, HashMap::new());

        for (name, path) in packages {
            let iname = self.sa.interner.intern(&name);
            let package_name = PackageName::External(name.clone());
            let (package_id, module_id) = add_package(self.sa, package_name, Some(iname));
            self.sa.package_names.insert(name.clone(), package_id);

            self.add_file(package_id, module_id, path.clone(), None);
        }
    }

    fn collect_file(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast: &ast::File,
    ) {
        let module_table = {
            let mut visitor = ElementVisitor {
                sa: self.sa,
                package_id,
                module_id,
                file_id,
                external_modules: Vec::new(),
                module_table: SymTable::new(),
                module_elements: Vec::new(),
                module_symtables: &mut self.module_symtables,
                is_collect_single_file: self.is_collect_single_file,
            };

            ast::visit_node(&mut visitor, ast.root());

            let module_table = visitor.module_table;
            let module_elements = visitor.module_elements;

            for external_module_id in visitor.external_modules {
                self.add_external_module(package_id, module_id, external_module_id);
            }

            assert!(
                self.sa.modules[module_id]
                    .children
                    .set(module_elements)
                    .is_ok()
            );

            module_table
        };

        assert!(
            self.module_symtables
                .insert(module_id, module_table)
                .is_none()
        );
    }

    fn add_external_module(
        &mut self,
        package_id: PackageDefinitionId,
        parent_module_id: ModuleDefinitionId,
        external_module_id: ModuleDefinitionId,
    ) {
        let external_module = self.sa.module(external_module_id);
        let node = external_module.ast(self.sa);
        let file_id = external_module.file_id();

        if let Some(ident) = node.name() {
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

            file_path.push(format!("{}.dora", ident.name()));

            self.add_file(
                package_id,
                external_module_id,
                file_path,
                Some((file_id, node.span())),
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
        if let Some(content) = self.sa.vfs.get(&file_path) {
            self.create_source_file_for_content(package_id, module_id, file_path, content);
            return;
        }

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

    fn create_source_file_for_content<T: ToArcString>(
        &mut self,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_path: PathBuf,
        content: T,
    ) {
        let file_id = add_source_file(self.sa, package_id, module_id, file_path, content.into());
        let result = self.sa.module(module_id).file_id.set(file_id);
        assert!(result.is_ok() || result.unwrap_err() == file_id);
        self.worklist.push_back(file_id);
    }

    fn parse_and_collect_file(&mut self, file_id: SourceFileId) {
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

        self.collect_file(package_id, module_id, file_id, &ast);
    }
}

fn file_as_string(path: &PathBuf) -> Result<String, Error> {
    let mut content = String::new();
    let mut file = fs::File::open(&path)?;
    file.read_to_string(&mut content)?;
    Ok(content)
}

struct ElementVisitor<'x> {
    sa: &'x mut Sema,
    package_id: PackageDefinitionId,
    file_id: SourceFileId,
    module_id: ModuleDefinitionId,
    external_modules: Vec<ModuleDefinitionId>,
    module_table: SymTable,
    module_elements: Vec<ElementId>,
    module_symtables: &'x mut HashMap<ModuleDefinitionId, SymTable>,
    is_collect_single_file: bool,
}

impl<'x> ast::Visitor for ElementVisitor<'x> {
    fn visit_extern(&mut self, ast_node: ast::AstExtern) {
        check_annotations(self.sa, self.file_id, ast_node.modifier_list(), &[]);
        if let Some(name) = ast_node.name() {
            let name_as_str = name.name();

            if let Some(package_id) = self.sa.package_names.get(name_as_str).cloned() {
                let top_level_module_id = self.sa.packages[package_id].top_level_module_id();

                let iname = self.sa.interner.intern(name_as_str);

                if !self.sa.packages[package_id].add_dependency(
                    iname,
                    package_id,
                    top_level_module_id,
                ) {
                    self.sa.report(
                        self.file_id,
                        ast_node.span(),
                        ErrorMessage::PackageAlreadyExists(name_as_str.clone()),
                    );
                }
            } else {
                self.sa.report(
                    self.file_id,
                    ast_node.span(),
                    ErrorMessage::UnknownPackage(name_as_str.clone()),
                );
            }
        }
    }

    fn visit_module(&mut self, ast_node: ast::AstModule) {
        let syntax_node_ptr = ast_node.syntax_node().as_ptr();
        let modifiers = check_annotations(
            self.sa,
            self.file_id,
            ast_node.modifier_list(),
            &[Annotation::Pub],
        );
        let name = ensure_name(self.sa, ast_node.name());
        let module = ModuleDefinition::new_inner(
            self.sa,
            self.package_id,
            self.module_id,
            self.file_id,
            ast_node.span(),
            syntax_node_ptr,
            modifiers,
            name,
        );
        let id = self.sa.modules.alloc(module);
        self.sa.modules[id].id = Some(id);
        let sym = SymbolKind::Module(id);

        if let Some((name, sym)) = self.insert_optional(ast_node.name(), sym, ElementId::Module(id))
        {
            report_sym_shadow_span(self.sa, name, self.file_id, ast_node.span(), sym);
        }

        if ast_node.element_list().is_none() {
            if self.is_collect_single_file {
                assert!(self.sa.module(id).children.set(Vec::new()).is_ok());
                assert!(self.module_symtables.insert(id, SymTable::new()).is_none());
            } else {
                self.external_modules.push(id);
            }
        } else {
            let module_table = SymTable::new();
            let module_elements = Vec::new();
            let saved_module_id = self.module_id;

            let saved_module_table = std::mem::replace(&mut self.module_table, module_table);
            let saved_module_elements =
                std::mem::replace(&mut self.module_elements, module_elements);
            self.module_id = id;
            ast::walk_children(self, ast_node);
            self.module_id = saved_module_id;
            let module_table = std::mem::replace(&mut self.module_table, saved_module_table);
            let module_elements =
                std::mem::replace(&mut self.module_elements, saved_module_elements);

            assert!(self.sa.module(id).children.set(module_elements).is_ok());
            assert!(self.module_symtables.insert(id, module_table).is_none());
        }
    }

    fn visit_trait(&mut self, ast_node: ast::AstTrait) {
        let modifiers = check_annotations(
            self.sa,
            self.file_id,
            ast_node.modifier_list(),
            &[Annotation::Pub],
        );

        let type_param_definition = build_type_param_definition(
            self.sa,
            None,
            ast_node.type_param_list(),
            ast_node.where_clause(),
            ast_node.bounds(),
            self.file_id,
        );

        let trait_ = TraitDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            ast_node.clone(),
            modifiers,
            ensure_name(self.sa, ast_node.name()),
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
            &ast_node,
        );

        let sym = SymbolKind::Trait(trait_id);

        if let Some((name, sym)) =
            self.insert_optional(ast_node.name(), sym, ElementId::Trait(trait_id))
        {
            report_sym_shadow_span(self.sa, name, self.file_id, ast_node.span(), sym);
        }
    }

    fn visit_use(&mut self, ast_node: ast::AstUse) {
        let modifiers = check_annotations(
            self.sa,
            self.file_id,
            ast_node.modifier_list(),
            &[Annotation::Pub],
        );
        let use_def = UseDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            ast_node,
            modifiers,
        );
        let use_id = self.sa.uses.alloc(use_def);
        assert!(self.sa.uses[use_id].id.set(use_id).is_ok());
    }

    fn visit_global(&mut self, ast_node: ast::AstGlobal) {
        let modifiers = check_annotations(
            self.sa,
            self.file_id,
            ast_node.modifier_list(),
            &[Annotation::Pub],
        );

        let global = GlobalDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            ast_node.clone(),
            modifiers,
            ensure_name(self.sa, ast_node.name()),
        );
        let global_id = self.sa.globals.alloc(global);
        self.sa.globals[global_id].id = Some(global_id);

        let sym = SymbolKind::Global(global_id);
        if let Some((name, sym)) =
            self.insert_optional(ast_node.name(), sym, ElementId::Global(global_id))
        {
            report_sym_shadow_span(self.sa, name, self.file_id, ast_node.span(), sym);
        }
    }

    fn visit_impl(&mut self, ast_node: ast::AstImpl) {
        check_annotations(self.sa, self.file_id, ast_node.modifier_list(), &[]);

        let type_param_definition = build_type_param_definition(
            self.sa,
            None,
            ast_node.type_param_list(),
            ast_node.where_clause(),
            None,
            self.file_id,
        );

        if ast_node.trait_type().is_some() {
            let impl_ = ImplDefinition::new(
                self.package_id,
                self.module_id,
                self.file_id,
                ast_node.clone(),
                type_param_definition,
            );
            let impl_id = self.sa.impls.alloc(impl_);
            assert!(self.sa.impls[impl_id].id.set(impl_id).is_ok());

            self.module_elements.push(ElementId::Impl(impl_id));

            find_elements_in_impl(
                self.sa,
                self.package_id,
                self.module_id,
                self.file_id,
                impl_id,
                ast_node.clone(),
            );
        } else {
            let extension = ExtensionDefinition::new(
                self.package_id,
                self.module_id,
                self.file_id,
                ast_node.clone(),
                type_param_definition,
            );
            let extension_id = self.sa.extensions.alloc(extension);
            assert!(
                self.sa.extensions[extension_id]
                    .id
                    .set(extension_id)
                    .is_ok()
            );

            self.module_elements
                .push(ElementId::Extension(extension_id));

            find_elements_in_extension(self.sa, self.file_id, extension_id, ast_node.clone());
        }
    }

    fn visit_const(&mut self, ast_node: ast::AstConst) {
        let modifiers = check_annotations(
            self.sa,
            self.file_id,
            ast_node.modifier_list(),
            &[Annotation::Pub],
        );
        let const_ = ConstDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            ast_node.clone(),
            modifiers,
            ensure_name(self.sa, ast_node.name()),
        );
        let id = self.sa.consts.alloc(const_);
        self.sa.consts[id].id = Some(id);

        let sym = SymbolKind::Const(id);
        if let Some((name, sym)) = self.insert_optional(ast_node.name(), sym, ElementId::Const(id))
        {
            report_sym_shadow_span(self.sa, name, self.file_id, ast_node.span(), sym);
        }
    }

    fn visit_class(&mut self, ast_node: ast::AstClass) {
        let modifiers = check_annotations(
            self.sa,
            self.file_id,
            ast_node.modifier_list(),
            &[Annotation::Internal, Annotation::Pub],
        );

        let type_param_definition = build_type_param_definition(
            self.sa,
            None,
            ast_node.type_param_list(),
            ast_node.where_clause(),
            None,
            self.file_id,
        );

        let class = ClassDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            ast_node.clone(),
            modifiers,
            ensure_name(self.sa, ast_node.name()),
            type_param_definition,
        );
        let class_id = self.sa.classes.alloc(class);
        self.sa.classes[class_id].id = Some(class_id);

        let mut field_ids = Vec::with_capacity(ast_node.fields_len());
        let mut used_names: HashSet<Name> = HashSet::new();

        for (index, field) in ast_node.fields().enumerate() {
            let modifiers = check_annotations(
                self.sa,
                self.file_id,
                field.modifier_list(),
                &[Annotation::Pub],
            );

            let name = if ast_node.field_name_style().is_positional() {
                None
            } else {
                let name = ensure_name(self.sa, field.name());
                check_if_symbol_exists(self.sa, self.file_id, &mut used_names, name, field.span());
                Some(name)
            };

            let field_def = FieldDefinition {
                id: None,
                name,
                span: Some(field.span()),
                index: FieldIndex(index),
                parsed_ty: ParsedType::new_ast(self.file_id, field.data_type()),
                mutable: true,
                visibility: modifiers.visibility(),
                file_id: Some(self.file_id),
                module_id: self.module_id,
                package_id: self.package_id,
            };
            let field_id = self.sa.fields.alloc(field_def);
            self.sa.fields[field_id].id = Some(field_id);

            field_ids.push(field_id);
        }

        assert!(
            self.sa
                .class(class_id)
                .field_ids
                .set(field_ids.clone())
                .is_ok()
        );

        let children: Vec<ElementId> = field_ids
            .into_iter()
            .map(|id| ElementId::Field(id))
            .collect();
        assert!(self.sa.class(class_id).children.set(children).is_ok());

        let sym = SymbolKind::Class(class_id);
        if let Some((name, sym)) =
            self.insert_optional(ast_node.name(), sym, ElementId::Class(class_id))
        {
            report_sym_shadow_span(self.sa, name, self.file_id, ast_node.span(), sym);
        }
    }

    fn visit_struct(&mut self, ast_node: ast::AstStruct) {
        let modifiers = check_annotations(
            self.sa,
            self.file_id,
            ast_node.modifier_list(),
            &[Annotation::Pub, Annotation::Internal],
        );

        let type_param_definition = build_type_param_definition(
            self.sa,
            None,
            ast_node.type_param_list(),
            ast_node.where_clause(),
            None,
            self.file_id,
        );

        let struct_ = StructDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            ast_node.clone(),
            modifiers,
            ensure_name(self.sa, ast_node.name()),
            type_param_definition,
        );
        let id = self.sa.structs.alloc(struct_);
        self.sa.structs[id].id = Some(id);

        let mut field_ids = Vec::with_capacity(ast_node.fields_len());
        let mut used_names: HashSet<Name> = HashSet::new();

        for (index, field) in ast_node.fields().enumerate() {
            let modifiers = check_annotations(
                self.sa,
                self.file_id,
                field.modifier_list(),
                &[Annotation::Pub],
            );

            let name = if ast_node.field_style().is_positional() {
                None
            } else {
                let name = ensure_name(self.sa, field.name());
                check_if_symbol_exists(self.sa, self.file_id, &mut used_names, name, field.span());
                Some(name)
            };

            let field_def = FieldDefinition {
                id: None,
                name,
                span: Some(field.span()),
                index: FieldIndex(index),
                mutable: false,
                parsed_ty: ParsedType::new_ast(self.file_id, field.data_type()),
                visibility: modifiers.visibility(),
                file_id: Some(self.file_id),
                module_id: self.module_id,
                package_id: self.package_id,
            };
            let field_id = self.sa.fields.alloc(field_def);
            self.sa.fields[field_id].id = Some(field_id);

            field_ids.push(field_id);
        }

        let mut field_names = HashMap::new();

        for &field_id in &field_ids {
            let field = self.sa.field(field_id);
            if let Some(name) = field.name {
                if field_names.contains_key(&name) {
                    continue;
                }

                field_names.insert(name, field.index);
            }
        }

        assert!(self.sa.struct_(id).field_ids.set(field_ids.clone()).is_ok());
        assert!(self.sa.struct_(id).field_names.set(field_names).is_ok());

        let children: Vec<ElementId> = field_ids
            .into_iter()
            .map(|id| ElementId::Field(id))
            .collect();
        assert!(self.sa.struct_(id).children.set(children).is_ok());

        let sym = SymbolKind::Struct(id);
        if let Some((name, sym)) = self.insert_optional(ast_node.name(), sym, ElementId::Struct(id))
        {
            report_sym_shadow_span(self.sa, name, self.file_id, ast_node.span(), sym);
        }
    }

    fn visit_function(&mut self, ast_node: ast::AstFunction) {
        let modifiers = check_annotations(
            self.sa,
            self.file_id,
            ast_node.modifier_list(),
            &[
                Annotation::Internal,
                Annotation::Optimize,
                Annotation::Test,
                Annotation::Pub,
                Annotation::ForceInline,
                Annotation::NeverInline,
            ],
        );

        let type_param_definition = build_type_param_definition(
            self.sa,
            None,
            ast_node.type_param_list(),
            ast_node.where_clause(),
            None,
            self.file_id,
        );

        let parent = FctParent::None;
        let params = build_function_params(
            self.sa,
            self.file_id,
            ast_node.clone(),
            parent.clone(),
            &modifiers,
        );

        let fct = FctDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            ast_node.clone(),
            modifiers,
            ensure_name(self.sa, ast_node.name()),
            type_param_definition,
            params,
            parent,
        );
        let fct_id = self.sa.fcts.alloc(fct);
        self.sa.fcts[fct_id].id = Some(fct_id);
        let sym = SymbolKind::Fct(fct_id);
        if let Some((name, sym)) =
            self.insert_optional(ast_node.name(), sym, ElementId::Fct(fct_id))
        {
            report_sym_shadow_span(self.sa, name, self.file_id, ast_node.span(), sym);
        }
    }

    fn visit_enum(&mut self, ast_node: ast::AstEnum) {
        let type_param_definition = build_type_param_definition(
            self.sa,
            None,
            ast_node.type_param_list(),
            ast_node.where_clause(),
            None,
            self.file_id,
        );

        let modifiers = check_annotations(
            self.sa,
            self.file_id,
            ast_node.modifier_list(),
            &[Annotation::Pub],
        );
        let enum_ = EnumDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            ast_node.clone(),
            modifiers,
            ensure_name(self.sa, ast_node.name()),
            type_param_definition,
        );
        let id = self.sa.enums.alloc(enum_);
        self.sa.enums[id].id = Some(id);

        let mut next_variant_id: u32 = 0;
        let mut variants = Vec::new();
        let mut name_to_value = HashMap::new();

        for variant in ast_node.variants() {
            if variant.name().is_none() {
                continue;
            }

            let variant_name = variant.name().expect("name expected");
            let name = self.sa.interner.intern(variant_name.name());

            let variant_id = self.sa.variants.alloc(VariantDefinition {
                id: OnceCell::new(),
                enum_id: id,
                package_id: self.package_id,
                module_id: self.module_id,
                file_id: self.file_id,
                index: next_variant_id,
                name: name,
                span: variant_name.span(),
                field_name_style: variant.field_name_style(),
                field_ids: OnceCell::new(),
                children: OnceCell::new(),
            });

            let mut field_ids = Vec::new();
            let mut used_names: HashSet<Name> = HashSet::new();

            for (index, field) in variant.fields().enumerate() {
                let name = if variant.field_name_style().is_positional() {
                    None
                } else {
                    let name = ensure_name(self.sa, field.name());
                    check_if_symbol_exists(
                        self.sa,
                        self.file_id,
                        &mut used_names,
                        name,
                        field.span(),
                    );
                    Some(name)
                };

                let field_def = FieldDefinition {
                    id: None,
                    name,
                    span: Some(field.span()),
                    mutable: false,
                    index: FieldIndex(index),
                    parsed_ty: ParsedType::new_ast(self.file_id, field.data_type()),
                    visibility: Visibility::Public,
                    file_id: Some(self.file_id),
                    module_id: self.module_id,
                    package_id: self.package_id,
                };
                let field_id = self.sa.fields.alloc(field_def);
                self.sa.fields[field_id].id = Some(field_id);

                field_ids.push(field_id)
            }

            assert!(
                self.sa
                    .variant(variant_id)
                    .field_ids
                    .set(field_ids.clone())
                    .is_ok()
            );

            let variant_children: Vec<ElementId> = field_ids
                .into_iter()
                .map(|id| ElementId::Field(id))
                .collect();
            assert!(
                self.sa
                    .variant(variant_id)
                    .children
                    .set(variant_children)
                    .is_ok()
            );

            variants.push(variant_id);

            if name_to_value.insert(name, next_variant_id).is_some() {
                let name = self.sa.interner.str(name).to_string();
                self.sa.report(
                    self.file_id,
                    variant.span(),
                    ErrorMessage::ShadowEnumVariant(name),
                );
            }

            next_variant_id += 1;
        }

        if ast_node.variants_len() == 0 {
            self.sa
                .report(self.file_id, ast_node.span(), ErrorMessage::NoEnumVariant);
        }

        assert!(self.sa.enum_(id).variants.set(variants.clone()).is_ok());
        assert!(self.sa.enum_(id).name_to_value.set(name_to_value).is_ok());

        let enum_children: Vec<ElementId> = variants
            .into_iter()
            .map(|id| ElementId::Variant(id))
            .collect();
        assert!(self.sa.enum_(id).children.set(enum_children).is_ok());

        let sym = SymbolKind::Enum(id);
        if let Some((name, sym)) = self.insert_optional(ast_node.name(), sym, ElementId::Enum(id)) {
            report_sym_shadow_span(self.sa, name, self.file_id, ast_node.span(), sym);
        }
    }

    fn visit_alias(&mut self, ast_node: ast::AstAlias) {
        let modifiers = check_annotations(
            self.sa,
            self.file_id,
            ast_node.modifier_list(),
            &[Annotation::Pub],
        );

        let parsed_ty = if let Some(ty) = ast_node.ty() {
            ParsedType::new_ast(self.file_id, ty)
        } else {
            self.sa.report(
                self.file_id,
                ast_node.span(),
                ErrorMessage::TypeAliasMissingType,
            );
            ParsedType::new_ty(ty::error())
        };

        let type_param_definition = build_type_param_definition(
            self.sa,
            None,
            ast_node.type_param_list(),
            ast_node.pre_where_clause(),
            None,
            self.file_id,
        );

        if let Some(post_where_clause) = ast_node.post_where_clause() {
            self.sa.report(
                self.file_id,
                post_where_clause.span(),
                ErrorMessage::UnexpectedWhere,
            );
        }

        let alias = AliasDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            AliasParent::None,
            ast_node.clone(),
            modifiers,
            ensure_name(self.sa, ast_node.name()),
            type_param_definition,
            Vec::new(),
            Some(parsed_ty),
            None,
        );
        let id = self.sa.aliases.alloc(alias);
        assert!(self.sa.alias(id).id.set(id).is_ok());

        if ast_node.bounds().is_some() {
            self.sa.report(
                self.file_id,
                ast_node.span(),
                ErrorMessage::UnexpectedTypeBounds,
            );
        }

        let sym = SymbolKind::Alias(id);
        if let Some((name, sym)) = self.insert_optional(ast_node.name(), sym, ElementId::Alias(id))
        {
            report_sym_shadow_span(self.sa, name, self.file_id, ast_node.span(), sym);
        }
    }
}

fn find_elements_in_trait(
    sa: &mut Sema,
    package_id: PackageDefinitionId,
    module_id: ModuleDefinitionId,
    file_id: SourceFileId,
    trait_id: TraitDefinitionId,
    node: &ast::AstTrait,
) {
    let mut methods = Vec::new();
    let mut aliases = Vec::new();
    let mut children = Vec::new();

    let mut instance_names: HashMap<Name, FctDefinitionId> = HashMap::new();
    let mut static_names: HashMap<Name, FctDefinitionId> = HashMap::new();
    let mut alias_names: HashMap<Name, AliasDefinitionId> = HashMap::new();

    let mut alias_idx_in_trait = 0;

    if let Some(element_list) = node.element_list() {
        for child in element_list.items() {
            match child {
                ast::AstElement::Function(method_node) => {
                    let trait_ = sa.trait_(trait_id);

                    let modifiers = check_annotations(
                        sa,
                        trait_.file_id,
                        method_node.modifier_list(),
                        &[
                            Annotation::Static,
                            Annotation::Optimize,
                            Annotation::TraitObjectIgnore,
                        ],
                    );

                    let container_type_param_definition = trait_.type_param_definition().clone();
                    let type_param_definition = build_type_param_definition(
                        sa,
                        Some(container_type_param_definition),
                        method_node.type_param_list(),
                        method_node.where_clause(),
                        None,
                        file_id,
                    );

                    let parent = FctParent::Trait(trait_id);
                    let params = build_function_params(
                        sa,
                        file_id,
                        method_node.clone(),
                        parent.clone(),
                        &modifiers,
                    );

                    let fct = FctDefinition::new(
                        trait_.package_id,
                        trait_.module_id,
                        trait_.file_id,
                        method_node.clone(),
                        modifiers,
                        ensure_name(sa, method_node.name()),
                        type_param_definition,
                        params,
                        parent,
                    );

                    let fct_id = sa.fcts.alloc(fct);
                    sa.fcts[fct_id].id = Some(fct_id);
                    methods.push(fct_id);
                    children.push(ElementId::Fct(fct_id));

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
                            method_node.span(),
                            ErrorMessage::AliasExists(method_name, existing_fct.span),
                        );
                    } else {
                        assert!(table.insert(fct.name, fct_id).is_none());
                    }
                }

                ast::AstElement::Alias(node) => {
                    let modifiers = check_annotations(sa, file_id, node.modifier_list(), &[]);

                    let name = ensure_name(sa, node.name());

                    let mut bounds = Vec::new();

                    if let Some(ast_bounds) = node.bounds() {
                        for ast_alias_bound in ast_bounds.items() {
                            bounds.push(AliasBound::new(file_id, ast_alias_bound));
                        }
                    }

                    let where_clause = if let Some(node_ty) = node.ty() {
                        sa.report(
                            file_id,
                            node_ty.span(),
                            ErrorMessage::UnexpectedTypeAliasAssignment,
                        );

                        if let Some(pre_where_clause) = node.pre_where_clause() {
                            sa.report(
                                file_id,
                                pre_where_clause.span(),
                                ErrorMessage::UnexpectedWhere,
                            );
                        }

                        node.post_where_clause()
                    } else {
                        assert!(node.post_where_clause().is_none());
                        node.pre_where_clause()
                    };

                    let container_type_param_definition =
                        sa.trait_(trait_id).type_param_definition().clone();
                    let type_param_definition = build_type_param_definition(
                        sa,
                        Some(container_type_param_definition),
                        node.type_param_list(),
                        where_clause,
                        None,
                        file_id,
                    );

                    let alias = AliasDefinition::new(
                        package_id,
                        module_id,
                        file_id,
                        AliasParent::Trait(trait_id),
                        node.clone(),
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
                    children.push(ElementId::Alias(id));

                    if let Some(&existing_id) = alias_names.get(&name) {
                        let existing_alias = sa.alias(existing_id);
                        let method_name = sa.interner.str(name).to_string();

                        sa.report(
                            file_id,
                            node.span(),
                            ErrorMessage::TypeExists(method_name, existing_alias.span),
                        );
                    } else {
                        alias_names.insert(name, id);
                    }
                }

                ast::AstElement::Error(..) => {
                    // ignore
                }

                _ => sa.report(
                    sa.trait_(trait_id).file_id,
                    child.span(),
                    ErrorMessage::ExpectedMethod,
                ),
            }
        }
    }

    let trait_ = sa.trait_(trait_id);
    assert!(trait_.methods.set(methods).is_ok());
    assert!(trait_.aliases.set(aliases).is_ok());
    assert!(trait_.children.set(children).is_ok());

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
    node: ast::AstImpl,
) {
    let mut methods = Vec::new();
    let mut aliases = Vec::new();
    let mut children = Vec::new();

    if let Some(element_list) = node.element_list() {
        for child in element_list.items() {
            match child {
                ast::AstElement::Function(node) => {
                    let impl_ = &sa.impl_(impl_id);
                    let modifiers = check_annotations(
                        sa,
                        impl_.file_id,
                        node.modifier_list(),
                        &[Annotation::Static, Annotation::Internal],
                    );

                    let container_type_param_definition = impl_.type_param_definition().clone();
                    let type_param_definition = build_type_param_definition(
                        sa,
                        Some(container_type_param_definition),
                        node.type_param_list(),
                        node.where_clause(),
                        None,
                        file_id,
                    );

                    let parent = FctParent::Impl(impl_id);
                    let params = build_function_params(
                        sa,
                        file_id,
                        node.clone(),
                        parent.clone(),
                        &modifiers,
                    );

                    let fct = FctDefinition::new(
                        impl_.package_id,
                        impl_.module_id,
                        impl_.file_id,
                        node.clone(),
                        modifiers,
                        ensure_name(sa, node.name()),
                        type_param_definition,
                        params,
                        parent,
                    );

                    let fct_id = sa.fcts.alloc(fct);
                    sa.fcts[fct_id].id = Some(fct_id);
                    methods.push(fct_id);
                    children.push(ElementId::Fct(fct_id));
                }

                ast::AstElement::Alias(node) => {
                    let modifiers = check_annotations(sa, file_id, node.modifier_list(), &[]);

                    let name = ensure_name(sa, node.name());

                    let parsed_ty = if let Some(ty) = node.ty() {
                        ParsedType::new_ast(file_id, ty)
                    } else {
                        sa.report(file_id, node.span(), ErrorMessage::TypeAliasMissingType);
                        ParsedType::new_ty(ty::error())
                    };

                    let where_clause = if node.ty().is_some() {
                        if let Some(pre_where_clause) = node.pre_where_clause() {
                            sa.report(
                                file_id,
                                pre_where_clause.span(),
                                ErrorMessage::UnexpectedWhere,
                            );
                        }

                        node.post_where_clause()
                    } else {
                        assert!(node.post_where_clause().is_none());
                        node.pre_where_clause()
                    };

                    let container_type_param_definition =
                        sa.impl_(impl_id).type_param_definition().clone();
                    let type_param_definition = build_type_param_definition(
                        sa,
                        Some(container_type_param_definition),
                        node.type_param_list(),
                        where_clause,
                        None,
                        file_id,
                    );

                    let alias = AliasDefinition::new(
                        package_id,
                        module_id,
                        file_id,
                        AliasParent::Impl(impl_id),
                        node.clone(),
                        modifiers,
                        name,
                        type_param_definition,
                        Vec::new(),
                        Some(parsed_ty),
                        None,
                    );

                    let id = sa.aliases.alloc(alias);
                    assert!(sa.alias(id).id.set(id).is_ok());

                    if node.bounds().is_some() {
                        sa.report(file_id, node.span(), ErrorMessage::UnexpectedTypeBounds);
                    }

                    aliases.push(id);
                    children.push(ElementId::Alias(id));
                }

                ast::AstElement::Error(..) => {
                    // ignore
                }

                _ => sa.report(
                    sa.impl_(impl_id).file_id,
                    child.span(),
                    ErrorMessage::ExpectedMethod,
                ),
            }
        }
    }

    let impl_ = &sa.impls[impl_id];
    assert!(impl_.methods.set(methods).is_ok());
    assert!(impl_.aliases.set(aliases).is_ok());
    assert!(impl_.children.set(children).is_ok());
}

fn find_elements_in_extension(
    sa: &mut Sema,
    file_id: SourceFileId,
    extension_id: ExtensionDefinitionId,
    node: ast::AstImpl,
) {
    let mut methods = Vec::new();
    let mut children = Vec::new();

    if let Some(element_list) = node.element_list() {
        for child in element_list.items() {
            match child {
                ast::AstElement::Function(method_node) => {
                    let name = ensure_name(sa, method_node.name());
                    let extension = sa.extension(extension_id);
                    let modifiers = check_annotations(
                        sa,
                        extension.file_id,
                        method_node.modifier_list(),
                        &[
                            Annotation::Internal,
                            Annotation::Static,
                            Annotation::Pub,
                            Annotation::Optimize,
                        ],
                    );

                    let container_type_param_definition = extension.type_param_definition().clone();
                    let type_param_definition = build_type_param_definition(
                        sa,
                        Some(container_type_param_definition),
                        method_node.type_param_list(),
                        method_node.where_clause(),
                        None,
                        extension.file_id,
                    );

                    let parent = FctParent::Extension(extension_id);
                    let params = build_function_params(
                        sa,
                        file_id,
                        method_node.clone(),
                        parent.clone(),
                        &modifiers,
                    );

                    let fct = FctDefinition::new(
                        extension.package_id,
                        extension.module_id,
                        extension.file_id,
                        method_node.clone(),
                        modifiers,
                        name,
                        type_param_definition,
                        params,
                        parent,
                    );

                    let fct_id = sa.fcts.alloc(fct);
                    sa.fcts[fct_id].id = Some(fct_id);
                    methods.push(fct_id);
                    children.push(ElementId::Fct(fct_id));
                }

                ast::AstElement::Error(..) => {
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
    }

    let extension = sa.extension(extension_id);
    assert!(extension.methods.set(methods).is_ok());
    assert!(extension.children.set(children).is_ok());
}

fn ensure_name(sa: &Sema, ident: Option<ast::AstName>) -> Name {
    if let Some(ident) = ident {
        sa.interner.intern(ident.name())
    } else {
        sa.interner.intern("<missing name>")
    }
}

#[derive(Default)]
pub struct Annotations {
    pub is_pub: bool,
    pub is_static: bool,
    pub is_test: bool,
    pub is_optimize_immediately: bool,
    pub is_internal: bool,
    pub is_force_inline: bool,
    pub is_never_inline: bool,
    pub is_trait_object_ignore: bool,
}

impl Annotations {
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
    TraitObjectIgnore,
}

impl Annotation {
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
        }
    }
}

fn check_annotations(
    sa: &Sema,
    file_id: SourceFileId,
    modifier_list: Option<ast::AstModifierList>,
    allow_list: &[Annotation],
) -> Annotations {
    let mut annotations = Annotations::default();

    if let Some(modifier_list) = modifier_list {
        let mut set: HashSet<Annotation> = HashSet::new();

        for modifier in modifier_list.items() {
            let annotation = check_annotation(sa, file_id, &modifier, &mut annotations);

            if let Some(annotation) = annotation {
                if !set.insert(annotation) {
                    sa.report(file_id, modifier.span(), ErrorMessage::RedundantAnnotation);
                }

                if !allow_list.contains(&annotation) {
                    sa.report(
                        file_id,
                        modifier.span(),
                        ErrorMessage::MisplacedAnnotation(annotation.name().to_string()),
                    );
                }
            }
        }
    }

    annotations
}

fn check_annotation(
    sa: &Sema,
    file_id: SourceFileId,
    modifier: &ast::AstModifier,
    annotations: &mut Annotations,
) -> Option<Annotation> {
    let mut children = modifier.children_with_tokens().filter(|t| !t.is_trivia());
    let first = children.next()?;

    match first.syntax_kind() {
        TokenKind::PUB_KW => {
            annotations.is_pub = true;
            Some(Annotation::Pub)
        }

        TokenKind::STATIC_KW => {
            annotations.is_static = true;
            Some(Annotation::Static)
        }

        TokenKind::AT => {
            if let Some(ident) = children
                .filter_map(|c| c.to_node())
                .find_map(|x| ast::AstName::cast(x))
            {
                match ident.name().as_str() {
                    "Test" => {
                        annotations.is_test = true;
                        Some(Annotation::Test)
                    }

                    "Optimize" => {
                        annotations.is_optimize_immediately = true;
                        Some(Annotation::Optimize)
                    }

                    "internal" => {
                        annotations.is_internal = true;
                        Some(Annotation::Internal)
                    }

                    "ForceInline" => {
                        annotations.is_force_inline = true;
                        Some(Annotation::ForceInline)
                    }

                    "NeverInline" => {
                        annotations.is_never_inline = true;
                        Some(Annotation::NeverInline)
                    }

                    "TraitObjectIgnore" => {
                        annotations.is_trait_object_ignore = true;
                        Some(Annotation::TraitObjectIgnore)
                    }

                    _ => {
                        sa.report(
                            file_id,
                            modifier.span(),
                            ErrorMessage::UnknownAnnotation(ident.name().clone()),
                        );
                        None
                    }
                }
            } else {
                None
            }
        }

        _ => None,
    }
}

impl<'x> ElementVisitor<'x> {
    fn insert(&mut self, name: Name, sym: SymbolKind, element_id: ElementId) -> Option<Symbol> {
        self.module_elements.push(element_id);
        self.module_table.insert(name, sym)
    }

    fn insert_optional(
        &mut self,
        ident: Option<ast::AstName>,
        sym: SymbolKind,
        element_id: ElementId,
    ) -> Option<(Name, Symbol)> {
        if let Some(ident) = ident {
            let name = self.sa.interner.intern(ident.name());
            self.insert(name, sym, element_id).map(|sym| (name, sym))
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

fn build_type_param_definition(
    sa: &Sema,
    parent: Option<Rc<TypeParamDefinition>>,
    ast_type_params: Option<ast::AstTypeParamList>,
    where_: Option<ast::AstWhereClause>,
    trait_bounds: Option<ast::AstTypeBounds>,
    file_id: SourceFileId,
) -> Rc<TypeParamDefinition> {
    let mut type_param_definition = TypeParamDefinition::new(parent);

    if let Some(ast_type_params) = ast_type_params {
        if ast_type_params.items_len() == 0 {
            let msg = ErrorMessage::TypeParamsExpected;
            sa.report(file_id, ast_type_params.span(), msg);
        }

        let mut names = HashSet::new();

        for type_param in ast_type_params.items() {
            let id = if let Some(ident) = type_param.name() {
                let iname = sa.interner.intern(ident.name());

                if !names.insert(iname) {
                    let name = ident.name().clone();
                    let msg = ErrorMessage::TypeParamNameNotUnique(name);
                    sa.report(file_id, type_param.span(), msg);
                }

                type_param_definition.add_type_param(iname)
            } else {
                let name = sa.interner.intern("<missing name>");
                type_param_definition.add_type_param(name)
            };

            if let Some(ast_bounds) = type_param.bounds() {
                for bound in ast_bounds.items() {
                    type_param_definition.add_type_param_bound(file_id, id, bound);
                }
            }
        }
    }

    if let Some(where_) = where_ {
        for clause in where_.clauses() {
            for bound in clause.bounds() {
                type_param_definition.add_where_bound(file_id, clause.ty(), bound);
            }
        }
    }

    if let Some(trait_bounds) = trait_bounds {
        for bound in trait_bounds.items() {
            type_param_definition.add_self_bound(file_id, bound);
        }
    }

    Rc::new(type_param_definition)
}

fn build_function_params(
    sa: &Sema,
    file_id: SourceFileId,
    ast_node: ast::AstFunction,
    parent: FctParent,
    modifiers: &Annotations,
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

    for (idx, ast_param) in ast_node.params().enumerate() {
        if ast_param.variadic() {
            if idx + 1 == ast_node.params_len() {
                is_variadic = true;
            } else {
                sa.report(
                    file_id,
                    ast_param.span(),
                    ErrorMessage::VariadicParameterNeedsToBeLast,
                );
            }
        }

        let param = Param::new(file_id, ast_param.id(), &ast_param);
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
