use std::collections::{HashMap, VecDeque};
use std::fs;
use std::io::{Error, Read};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::language::error::msg::ErrorMessage;
use crate::language::report_sym_shadow_span;
use crate::language::sem_analysis::{
    ClassDefinition, ConstDefinition, EnumDefinition, ExtensionDefinition, ExtensionDefinitionId,
    FctDefinition, FctParent, GlobalDefinition, GlobalDefinitionId, ImplDefinition,
    ImplDefinitionId, ModuleDefinition, ModuleDefinitionId, PackageDefinitionId, PackageName,
    SemAnalysis, SourceFileId, StructDefinition, TraitDefinition, TraitDefinitionId, UseDefinition,
};
use crate::language::sym::Sym;
use crate::STDLIB;
use dora_parser::ast::visit::Visitor;
use dora_parser::ast::{self, visit};
use dora_parser::builder::Builder;
use dora_parser::interner::Name;
use dora_parser::parser::{NodeIdGenerator, Parser};
use dora_parser::Span;

pub fn parse(sa: &mut SemAnalysis) {
    let mut discoverer = ProgramParser::new(sa);
    discoverer.parse_all();
}

#[derive(Copy, Clone)]
enum FileLookup {
    FileSystem,
    Bundle,
}

struct ProgramParser<'a> {
    sa: &'a mut SemAnalysis,
    files_to_parse: VecDeque<(SourceFileId, FileLookup, Option<PathBuf>)>,
    packages: HashMap<String, PathBuf>,
}

impl<'a> ProgramParser<'a> {
    fn new(sa: &mut SemAnalysis) -> ProgramParser {
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
        self.sa.package_names.insert(stdlib_iname, package_id);
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
        let boots_name = "boots";
        if let Some(boots_path) = self.packages.remove(boots_name) {
            let boots_name = self.sa.interner.intern(boots_name);
            let (package_id, module_id) = self.sa.add_package(PackageName::Boots, Some(boots_name));
            self.sa.package_names.insert(boots_name, package_id);
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
            let package_name = PackageName::External(iname);
            let (package_id, module_id) = self.sa.add_package(package_name, Some(iname));
            self.sa.package_names.insert(iname, package_id);

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
        id_generator: NodeIdGenerator,
    ) {
        let mut gdef = TopLevelDeclaration {
            sa: self.sa,
            package_id,
            module_id,
            file_id,
            id_generator,
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
            let name = self.sa.interner.str(ident.name).to_string();

            match file_lookup {
                FileLookup::FileSystem => {
                    let module_path = module_path.expect("missing module_path");

                    let mut file_path = module_path.clone();
                    file_path.push(format!("{}.dora", name));

                    let mut module_path = module_path;
                    module_path.push(&name);

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
                    file_path.push(format!("{}.dora", name));

                    let mut module_path = module_path;
                    module_path.push(&name);

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

        let parser = Parser::from_shared_string(content, &mut self.sa.interner);

        let (ast, id_generator, errors) = parser.parse();

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
            id_generator,
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
    sa: &'x mut SemAnalysis,
    package_id: PackageDefinitionId,
    file_id: SourceFileId,
    module_id: ModuleDefinitionId,
    external_modules: Vec<ModuleDefinitionId>,
    id_generator: NodeIdGenerator,
}

impl<'x> visit::Visitor for TopLevelDeclaration<'x> {
    fn visit_extern(&mut self, stmt: &Arc<ast::ExternPackage>) {
        if let Some(name) = &stmt.name {
            if let Some(package_id) = self.sa.package_names.get(&name.name).cloned() {
                let top_level_module_id = {
                    let package = self.sa.packages.idx(package_id);
                    let package = package.read();

                    package.top_level_module_id()
                };

                let package = self.sa.packages.idx(package_id);
                let mut package = package.write();

                if !package.add_dependency(name.name, package_id, top_level_module_id) {
                    let name = self.sa.interner.str(name.name).to_string();
                    self.sa.diag.lock().report(
                        self.file_id,
                        stmt.span,
                        ErrorMessage::PackageAlreadyExists(name),
                    );
                }
            } else {
                let name = self.sa.interner.str(name.name).to_string();
                self.sa.diag.lock().report(
                    self.file_id,
                    stmt.span,
                    ErrorMessage::UnknownPackage(name),
                );
            }
        }
    }

    fn visit_module(&mut self, node: &Arc<ast::Module>) {
        let name = ensure_name(self.sa, &node.name);
        let module = ModuleDefinition::new_inner(
            self.sa,
            self.package_id,
            self.module_id,
            self.file_id,
            node,
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
        let trait_ = TraitDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
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
        let use_def = UseDefinition::new(self.package_id, self.module_id, self.file_id, node);
        self.sa.uses.push(use_def);
    }

    fn visit_global(&mut self, node: &Arc<ast::Global>) {
        let global = GlobalDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            ensure_name(self.sa, &node.name),
        );
        let global_id = self.sa.globals.push(global);

        if !self.sa.args.check_global_initializer {
            generate_function_for_initial_value(self.sa, global_id, &self.id_generator, node);
        }

        let sym = Sym::Global(global_id);
        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }
    }

    fn visit_impl(&mut self, node: &Arc<ast::Impl>) {
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
        let const_ = ConstDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            ensure_name(self.sa, &node.name),
        );
        let id = self.sa.consts.push(const_);

        let sym = Sym::Const(id);
        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }
    }

    fn visit_class(&mut self, node: &Arc<ast::Class>) {
        let class = ClassDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            ensure_name(self.sa, &node.name),
        );
        let class_id = self.sa.classes.push(class);

        let sym = Sym::Class(class_id);
        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }
    }

    fn visit_struct(&mut self, node: &Arc<ast::Struct>) {
        let struct_ = StructDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            ensure_name(self.sa, &node.name),
        );
        let id = self.sa.structs.push(struct_);

        let sym = Sym::Struct(id);
        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }
    }

    fn visit_fct(&mut self, node: &Arc<ast::Function>) {
        let fct = FctDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
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
        let enum_ = EnumDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            ensure_name(self.sa, &node.name),
        );
        let id = self.sa.enums.push(enum_);

        let sym = Sym::Enum(id);
        if let Some((name, sym)) = self.insert_optional(&node.name, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, node.span, sym);
        }
    }
}

fn find_methods_in_trait(
    sa: &mut SemAnalysis,
    trait_id: TraitDefinitionId,
    node: &Arc<ast::Trait>,
) {
    let trait_ = sa.traits.idx(trait_id);
    let mut trait_ = trait_.write();

    for method_node in &node.methods {
        let fct = FctDefinition::new(
            trait_.package_id,
            trait_.module_id,
            trait_.file_id,
            method_node,
            ensure_name(sa, &method_node.name),
            FctParent::Trait(trait_id),
        );

        let fct_id = sa.add_fct(fct);
        trait_.methods.push(fct_id);
    }
}

fn find_methods_in_impl(sa: &mut SemAnalysis, impl_id: ImplDefinitionId, node: &Arc<ast::Impl>) {
    let impl_ = sa.impls.idx(impl_id);
    let mut impl_ = impl_.write();

    for method_node in &node.methods {
        let fct = FctDefinition::new(
            impl_.package_id,
            impl_.module_id,
            impl_.file_id,
            method_node,
            ensure_name(sa, &method_node.name),
            FctParent::Impl(impl_id),
        );

        let fct_id = sa.add_fct(fct);
        impl_.methods.push(fct_id);
    }
}

fn find_methods_in_extension(
    sa: &mut SemAnalysis,
    extension_id: ExtensionDefinitionId,
    node: &Arc<ast::Impl>,
) {
    let extension = sa.extensions.idx(extension_id);
    let mut extension = extension.write();

    for method_node in &node.methods {
        let fct = FctDefinition::new(
            extension.package_id,
            extension.module_id,
            extension.file_id,
            method_node,
            ensure_name(sa, &method_node.name),
            FctParent::Extension(extension_id),
        );

        let fct_id = sa.add_fct(fct);
        extension.methods.push(fct_id);
    }
}

fn generate_function_for_initial_value(
    sa: &mut SemAnalysis,
    global_id: GlobalDefinitionId,
    id_generator: &NodeIdGenerator,
    node: &Arc<ast::Global>,
) {
    if let Some(ref initial_value) = node.initial_value {
        let global = sa.globals.idx(global_id);
        let mut global = global.write();
        let span = global.span;

        let function_ast = {
            let builder = Builder::new();
            let mut block = builder.build_block();

            let var = builder.build_ident(id_generator.next(), span, global.name);
            let assignment = builder.build_initializer_assign(
                id_generator.next(),
                span,
                var,
                initial_value.clone(),
            );

            block.add_expr(id_generator.next(), assignment);

            let mut fct = builder.build_fct(global.name);
            fct.block(block.build(id_generator.next(), span));
            Arc::new(fct.build(id_generator.next(), span))
        };

        let fct = FctDefinition::new(
            global.package_id,
            global.module_id,
            global.file_id,
            &function_ast,
            ensure_name(sa, &function_ast.name),
            FctParent::None,
        );

        let fct_id = sa.add_fct(fct);
        global.initializer = Some(fct_id);
    }
}

fn ensure_name(sa: &mut SemAnalysis, ident: &Option<ast::Ident>) -> Name {
    if let Some(ident) = ident {
        ident.name
    } else {
        sa.interner.intern("<missing name>")
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
            self.insert(ident.name, sym).map(|sym| (ident.name, sym))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::ErrorMessage;
    use crate::language::tests::*;

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
