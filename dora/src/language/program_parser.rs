use std::collections::{HashMap, VecDeque};
use std::fs;
use std::io::{Error, Read};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::language::error::msg::SemError;
use crate::language::report_sym_shadow;
use crate::language::sem_analysis::{
    AnnotationDefinition, ClassDefinition, ClassDefinitionId, ConstDefinition, EnumDefinition,
    ExtensionDefinition, ExtensionDefinitionId, FctDefinition, FctParent, GlobalDefinition,
    GlobalDefinitionId, ImplDefinition, ImplDefinitionId, ModuleDefinition, ModuleDefinitionId,
    SemAnalysis, SourceFileId, StructDefinition, TraitDefinition, TraitDefinitionId, UseDefinition,
};
use crate::language::sym::Sym;
use dora_parser::ast::visit::Visitor;
use dora_parser::ast::{self, visit};
use dora_parser::interner::Name;
use dora_parser::parser::Parser;
use dora_parser::Position;

pub fn parse(sa: &mut SemAnalysis) -> Result<(), i32> {
    let mut discoverer = ProgramParser::new(sa);
    discoverer.parse_all()
}

#[derive(Copy, Clone)]
enum FileLookup {
    FileSystem,
    Bundle,
}

type PreparedBundle = HashMap<PathBuf, Vec<(PathBuf, String)>>;

fn prepare_bundle(files: &[(&'static str, &'static str)]) -> PreparedBundle {
    let mut result = HashMap::new();

    for (file, content) in files {
        let file_path = PathBuf::from(file);
        let mut directory = file_path.clone();
        directory.pop();

        let files_in_directory = result.entry(directory).or_insert_with(|| Vec::new());
        files_in_directory.push((file_path, content.to_string()));
    }

    result
}

struct ProgramParser<'a> {
    sa: &'a mut SemAnalysis,
    files_to_parse: VecDeque<(SourceFileId, FileLookup, Option<PathBuf>)>,
    stdlib: PreparedBundle,
}

impl<'a> ProgramParser<'a> {
    fn new(sa: &mut SemAnalysis) -> ProgramParser {
        ProgramParser {
            sa,
            files_to_parse: VecDeque::new(),
            stdlib: HashMap::new(),
        }
    }

    fn parse_all(&mut self) -> Result<(), i32> {
        self.add_initial_files()?;

        while let Some((file_id, file_lookup, module_path)) = self.files_to_parse.pop_front() {
            self.parse_file(file_id, file_lookup, module_path)?;
        }

        Ok(())
    }

    fn add_initial_files(&mut self) -> Result<(), i32> {
        self.add_stdlib_files()?;
        self.add_boots_files()?;
        self.add_program_files()?;
        self.add_test_files()?;

        Ok(())
    }

    fn add_stdlib_files(&mut self) -> Result<(), i32> {
        let stdlib_dir = self.sa.args.flag_stdlib.clone();
        let module_id = self.sa.stdlib_module_id;

        if let Some(stdlib) = stdlib_dir {
            let file_path = PathBuf::from(stdlib);
            let module_path = PathBuf::from(file_path.parent().expect("parent missing"));
            self.add_file(
                file_path,
                module_id,
                Some(module_path),
                None,
                FileLookup::FileSystem,
            )
        } else {
            let stdlib_file = format!("stdlib{}stdlib.dora", std::path::MAIN_SEPARATOR);
            let file_path = PathBuf::from(stdlib_file);
            let module_path = PathBuf::from(file_path.parent().expect("parent missing"));
            self.add_bundled_file(file_path, module_id, module_path)
        }
    }

    fn add_bundled_file(
        &mut self,
        file_path: PathBuf,
        module_id: ModuleDefinitionId,
        module_path: PathBuf,
    ) -> Result<(), i32> {
        let content = self.get_bundled_file(&file_path);
        self.add_file_from_string(
            file_path,
            content.into(),
            module_id,
            Some(module_path),
            FileLookup::Bundle,
        );

        Ok(())
    }

    fn get_bundled_file(&self, path: &Path) -> &'static str {
        let bundle = crate::driver::STDLIB;

        for (name, content) in bundle {
            if *name == path.to_string_lossy() {
                return *content;
            }
        }

        for (bundled_file_path, _) in bundle {
            eprintln!("\t{}", bundled_file_path);
        }
        panic!("can't find file {} in bundle.", path.display())
    }

    fn add_boots_files(&mut self) -> Result<(), i32> {
        let boots_path = self.sa.args.flag_boots.clone();

        if let Some(boots_path) = boots_path {
            let file_path = PathBuf::from(boots_path);
            let module_path = PathBuf::from(file_path.parent().expect("missing parent"));
            self.add_file(
                file_path,
                self.sa.boots_module_id,
                Some(module_path),
                None,
                FileLookup::FileSystem,
            )?;
        }

        Ok(())
    }

    fn add_program_files(&mut self) -> Result<(), i32> {
        if !self.sa.args.arg_file.is_empty() {
            let arg_file = self.sa.args.arg_file.clone();
            let path = PathBuf::from(&arg_file);

            if path.is_file() {
                let file_path = PathBuf::from(path);
                let module_path = PathBuf::from(file_path.parent().expect("parent missing"));
                self.add_file(
                    file_path,
                    self.sa.program_module_id,
                    Some(module_path),
                    None,
                    FileLookup::FileSystem,
                )?;
            } else {
                println!("file `{}` does not exist.", &arg_file);
                return Err(1);
            }
        }

        Ok(())
    }

    fn add_test_files(&mut self) -> Result<(), i32> {
        if let Some(content) = self.sa.test_file_as_string {
            self.add_file_from_string(
                PathBuf::from("<<code>>"),
                content.to_string(),
                self.sa.program_module_id,
                None,
                FileLookup::FileSystem,
            );
        }

        Ok(())
    }

    fn scan_file(
        &mut self,
        file_id: SourceFileId,
        module_id: ModuleDefinitionId,
        module_path: Option<PathBuf>,
        file_lookup: FileLookup,
        ast: &ast::File,
    ) -> Result<(), i32> {
        let mut gdef = GlobalDef {
            sa: self.sa,
            file_id,
            module_id,
            external_modules: Vec::new(),
        };

        gdef.visit_file(ast);

        if !gdef.external_modules.is_empty() {
            for external_module_id in gdef.external_modules {
                self.add_module_files(external_module_id, module_path.clone(), file_lookup)?;
            }
        }

        Ok(())
    }

    fn add_module_files(
        &mut self,
        module_id: ModuleDefinitionId,
        module_path: Option<PathBuf>,
        file_lookup: FileLookup,
    ) -> Result<(), i32> {
        let module = self.sa.modules[module_id].clone();
        let module = module.read();
        let node = module.ast.clone().unwrap();
        let file_id = module.file_id.expect("missing file_id");

        let name = self.sa.interner.str(node.name).to_string();

        match file_lookup {
            FileLookup::FileSystem => {
                let module_path = module_path.expect("missing module_path");

                let mut file_path = module_path.clone();
                file_path.push(format!("{}.dora", name));

                let mut module_path = module_path;
                module_path.push(&name);

                self.add_file(
                    file_path,
                    module_id,
                    Some(module_path),
                    Some((file_id, node.pos)),
                    FileLookup::FileSystem,
                )?;

                Ok(())
            }

            FileLookup::Bundle => {
                let module_path = module_path.expect("missing module_path");

                let mut file_path = module_path.clone();
                file_path.push(format!("{}.dora", name));

                let mut module_path = module_path;
                module_path.push(&name);

                self.add_bundled_file(file_path, module_id, module_path)?;

                Ok(())
            }
        }
    }

    fn add_file(
        &mut self,
        path: PathBuf,
        module_id: ModuleDefinitionId,
        module_path: Option<PathBuf>,
        error_location: Option<(SourceFileId, Position)>,
        file_lookup: FileLookup,
    ) -> Result<(), i32> {
        let result = file_as_string(&path);

        match result {
            Ok(content) => {
                self.add_file_from_string(path, content, module_id, module_path, file_lookup);
                Ok(())
            }

            Err(_) => {
                if let Some((file_id, pos)) = error_location {
                    self.sa
                        .diag
                        .lock()
                        .report(file_id, pos, SemError::FileNoAccess(path));
                    Ok(())
                } else {
                    println!("unable to read file `{:?}`", path);
                    Err(1)
                }
            }
        }
    }

    fn add_file_from_string(
        &mut self,
        file_path: PathBuf,
        content: String,
        module_id: ModuleDefinitionId,
        module_path: Option<PathBuf>,
        file_lookup: FileLookup,
    ) {
        let file_id = self
            .sa
            .add_source_file(file_path, Arc::new(content), module_id);
        self.files_to_parse
            .push_back((file_id, file_lookup, module_path));
    }

    fn parse_file(
        &mut self,
        file_id: SourceFileId,
        file_lookup: FileLookup,
        module_path: Option<PathBuf>,
    ) -> Result<(), i32> {
        let module_id;
        let content;
        let path;

        {
            let file = self.sa.source_file(file_id);
            module_id = file.module_id;
            content = file.content.clone();
            path = file.path.clone();
        }

        let parser = Parser::from_shared_string(content, &mut self.sa.interner);

        match parser.parse() {
            Ok(ast) => {
                self.scan_file(file_id, module_id, module_path, file_lookup, &ast)?;
                Ok(())
            }

            Err(error) => {
                println!(
                    "error in {:?} at {}: {}",
                    path,
                    error.pos,
                    error.error.message()
                );
                println!("error during parsing.");

                Err(1)
            }
        }
    }
}

fn file_as_string(path: &PathBuf) -> Result<String, Error> {
    let mut content = String::new();
    let mut file = fs::File::open(&path)?;
    file.read_to_string(&mut content)?;
    Ok(content)
}

struct GlobalDef<'x> {
    sa: &'x mut SemAnalysis,
    file_id: SourceFileId,
    module_id: ModuleDefinitionId,
    external_modules: Vec<ModuleDefinitionId>,
}

impl<'x> visit::Visitor for GlobalDef<'x> {
    fn visit_module(&mut self, node: &Arc<ast::Module>) {
        let module = ModuleDefinition::new(self.sa, self.module_id, self.file_id, node);
        let id = self.sa.modules.push(module);
        let sym = Sym::Module(id);

        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
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
        let trait_ = TraitDefinition::new(self.file_id, self.module_id, node);
        let trait_id = self.sa.traits.push(trait_);

        find_methods_in_trait(self.sa, trait_id, node);

        let sym = Sym::Trait(trait_id);
        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_use(&mut self, node: &Arc<ast::Use>) {
        let use_def = UseDefinition::new(self.file_id, self.module_id, node);
        self.sa.uses.push(use_def);
    }

    fn visit_global(&mut self, node: &Arc<ast::Global>) {
        let global = GlobalDefinition::new(self.file_id, self.module_id, node);
        let global_id = self.sa.globals.push(global);

        find_methods_in_global(self.sa, global_id, node);

        let sym = Sym::Global(global_id);
        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_impl(&mut self, node: &Arc<ast::Impl>) {
        if node.trait_type.is_some() {
            let impl_ = ImplDefinition::new(self.file_id, self.module_id, node);
            let impl_id = self.sa.impls.push(impl_);

            find_methods_in_impl(self.sa, impl_id, node);
        } else {
            let extension = ExtensionDefinition::new(self.file_id, self.module_id, node);
            let extension_id = self.sa.extensions.push(extension);

            find_methods_in_extension(self.sa, extension_id, node);
        }
    }

    fn visit_const(&mut self, node: &Arc<ast::Const>) {
        let const_ = ConstDefinition::new(self.file_id, self.module_id, node);
        let id = self.sa.consts.push(const_);

        let sym = Sym::Const(id);
        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_class(&mut self, node: &Arc<ast::Class>) {
        let class = ClassDefinition::new(self.file_id, node, self.module_id);
        let class_id = self.sa.classes.push(class);

        find_methods_in_class(self.sa, class_id, node);

        let sym = Sym::Class(class_id);

        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_struct(&mut self, node: &Arc<ast::Struct>) {
        let xstruct = StructDefinition::new(self.file_id, self.module_id, node);
        let id = self.sa.structs.push(xstruct);

        let sym = Sym::Struct(id);
        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_annotation(&mut self, node: &Arc<ast::Annotation>) {
        let annotation =
            AnnotationDefinition::new(self.file_id, node.pos, node.name, self.module_id);
        let id = self.sa.annotations.push(annotation);

        let sym = Sym::Annotation(id);

        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_fct(&mut self, node: &Arc<ast::Function>) {
        let fct = FctDefinition::new(self.file_id, self.module_id, node, FctParent::None);
        let fctid = self.sa.add_fct(fct);
        let sym = Sym::Fct(fctid);

        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_enum(&mut self, node: &Arc<ast::Enum>) {
        let enum_ = EnumDefinition::new(self.file_id, self.module_id, node);
        let id = self.sa.enums.push(enum_);

        let sym = Sym::Enum(id);
        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
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
            trait_.file_id,
            trait_.module_id,
            method_node,
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
            impl_.file_id,
            impl_.module_id,
            method_node,
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
            extension.file_id,
            extension.module_id,
            method_node,
            FctParent::Extension(extension_id),
        );

        let fct_id = sa.add_fct(fct);
        extension.methods.push(fct_id);
    }
}

fn find_methods_in_class(
    sa: &mut SemAnalysis,
    class_id: ClassDefinitionId,
    node: &Arc<ast::Class>,
) {
    let class = sa.classes.idx(class_id);
    let mut class = class.write();

    if let Some(ref ctor_node) = node.constructor {
        let fct = FctDefinition::new(
            class.file_id(),
            class.module_id,
            ctor_node,
            FctParent::Class(class_id),
        );

        let ctor_id = sa.add_fct(fct);
        class.constructor = Some(ctor_id);
    }

    for method_node in &node.methods {
        let fct = FctDefinition::new(
            class.file_id(),
            class.module_id,
            method_node,
            FctParent::Class(class_id),
        );

        let fct_id = sa.add_fct(fct);
        class.methods.push(fct_id);
    }
}

fn find_methods_in_global(
    sa: &mut SemAnalysis,
    global_id: GlobalDefinitionId,
    node: &Arc<ast::Global>,
) {
    if let Some(ref initializer) = node.initializer {
        let global = sa.globals.idx(global_id);
        let mut global = global.write();

        let fct = FctDefinition::new(
            global.file_id,
            global.module_id,
            initializer,
            FctParent::None,
        );

        let fct_id = sa.add_fct(fct);
        global.initializer = Some(fct_id);
    }
}

impl<'x> GlobalDef<'x> {
    fn insert(&mut self, name: Name, sym: Sym) -> Option<Sym> {
        let level = self.sa.module_table(self.module_id);
        let mut level = level.write();
        level.insert(name, sym)
    }
}

pub fn should_file_be_parsed(path: &Path) -> bool {
    if !path.is_file() {
        return false;
    }

    path.to_string_lossy().ends_with(".dora")
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::SemError;
    use crate::language::tests::*;

    #[test]
    fn test_class() {
        err(
            "class Foo class Foo",
            pos(1, 11),
            SemError::ShadowClass("Foo".into()),
        );
        err(
            "fn Foo() {} class Foo",
            pos(1, 13),
            SemError::ShadowFunction("Foo".into()),
        );
        err(
            "class Foo fn Foo() {}",
            pos(1, 11),
            SemError::ShadowClass("Foo".into()),
        );
        err(
            "class Foo let Foo: Int32 = 1;",
            pos(1, 11),
            SemError::ShadowClass("Foo".into()),
        );
        err(
            "class Foo var Foo: Int32 = 1;",
            pos(1, 11),
            SemError::ShadowClass("Foo".into()),
        );
        err(
            "class Foo const Foo: Int32 = 1;",
            pos(1, 11),
            SemError::ShadowClass("Foo".into()),
        );
    }

    #[test]
    fn test_struct() {
        ok("struct Foo {}");
        err(
            "struct Foo {} struct Foo {}",
            pos(1, 15),
            SemError::ShadowStruct("Foo".into()),
        );
        err(
            "struct Foo {} struct Foo {}",
            pos(1, 15),
            SemError::ShadowStruct("Foo".into()),
        );
        err(
            "struct Foo {} class Foo {}",
            pos(1, 15),
            SemError::ShadowStruct("Foo".into()),
        );
        err(
            "fn Foo() {} struct Foo {}",
            pos(1, 13),
            SemError::ShadowFunction("Foo".into()),
        );
        err(
            "struct Foo {} fn Foo() {}",
            pos(1, 15),
            SemError::ShadowStruct("Foo".into()),
        );
        err(
            "struct Foo {} let Foo: Int32 = 1;",
            pos(1, 15),
            SemError::ShadowStruct("Foo".into()),
        );
        err(
            "struct Foo {} var Foo: Int32 = 1;",
            pos(1, 15),
            SemError::ShadowStruct("Foo".into()),
        );
        err(
            "struct Foo {} const Foo: Int32 = 1;",
            pos(1, 15),
            SemError::ShadowStruct("Foo".into()),
        );
    }

    #[test]
    fn test_trait() {
        ok("trait Foo {}");
        err(
            "trait Foo {} struct Foo {}",
            pos(1, 14),
            SemError::ShadowTrait("Foo".into()),
        );
        err(
            "trait Foo {} class Foo {}",
            pos(1, 14),
            SemError::ShadowTrait("Foo".into()),
        );
    }

    #[test]
    fn test_const() {
        ok("const foo: Int32 = 0i32;");
        err(
            "const foo: Int32 = 0i32; fn foo() {}",
            pos(1, 26),
            SemError::ShadowConst("foo".into()),
        );
        err(
            "const foo: Int32 = 0i32; class foo {}",
            pos(1, 26),
            SemError::ShadowConst("foo".into()),
        );
        err(
            "const foo: Int32 = 0i32; struct foo {}",
            pos(1, 26),
            SemError::ShadowConst("foo".into()),
        );
    }

    #[test]
    fn test_enum() {
        ok("enum Foo { A }");

        err(
            "enum Foo { A } class Foo",
            pos(1, 16),
            SemError::ShadowEnum("Foo".into()),
        );
    }

    #[test]
    fn test_mod() {
        ok("mod foo {} mod bar {}");
        ok("fn bar() {} mod foo { fn bar() {} }");

        err(
            "mod foo {} mod foo {}",
            pos(1, 12),
            SemError::ShadowModule("foo".into()),
        );

        err(
            "mod foo { fn bar() {} fn bar() {} }",
            pos(1, 23),
            SemError::ShadowFunction("bar".into()),
        );
    }
}
