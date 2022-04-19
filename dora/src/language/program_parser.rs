use std::collections::{HashMap, VecDeque};
use std::fs;
use std::io::{Error, Read};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::language::error::msg::SemError;
use crate::language::report_sym_shadow;
use crate::language::sem_analysis::{
    AnnotationDefinition, ClassDefinition, ConstDefinition, EnumDefinition, ExtensionDefinition,
    FctDefinition, FctParent, GlobalDefinition, ImplDefinition, ModuleDefinition,
    ModuleDefinitionId, SemAnalysis, SourceFileId, StructDefinition, TraitDefinition,
    UseDefinition,
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
    Disallowed,
    Directory,
    Stdlib,
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
    files_to_parse: VecDeque<(SourceFileId, FileLookup)>,
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

        while let Some((file_id, file_lookup)) = self.files_to_parse.pop_front() {
            self.parse_file(file_id, file_lookup)?;
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
        use crate::driver::start::STDLIB;

        let stdlib_dir = self.sa.args.flag_stdlib.clone();
        let module_id = self.sa.stdlib_module_id;

        if let Some(stdlib) = stdlib_dir {
            self.add_files_in_directory(PathBuf::from(stdlib), module_id, None)?;
        } else {
            self.stdlib = prepare_bundle(STDLIB);
            self.add_bundled_stdlib(module_id)?;
        }

        Ok(())
    }

    fn add_bundled_stdlib(&mut self, module_id: ModuleDefinitionId) -> Result<(), i32> {
        let path = PathBuf::from("stdlib");
        self.add_bundled_directory(path, module_id)
    }

    fn add_bundled_directory(
        &mut self,
        path: PathBuf,
        module_id: ModuleDefinitionId,
    ) -> Result<(), i32> {
        let files = self.stdlib.remove(&path).expect("missing directory");
        for (path, content) in files {
            self.add_file_from_string(path, content, module_id, FileLookup::Stdlib);
        }

        Ok(())
    }

    fn add_boots_files(&mut self) -> Result<(), i32> {
        let boots_dir = self.sa.args.flag_boots.clone();

        if let Some(boots) = boots_dir {
            self.add_files_in_directory(PathBuf::from(boots), self.sa.boots_module_id, None)?;
        }

        Ok(())
    }

    fn add_program_files(&mut self) -> Result<(), i32> {
        if !self.sa.args.arg_file.is_empty() {
            let arg_file = self.sa.args.arg_file.clone();
            let path = PathBuf::from(&arg_file);

            if path.is_file() {
                self.add_file(PathBuf::from(path), self.sa.program_module_id, None)?;
            } else if path.is_dir() {
                self.add_files_in_directory(path, self.sa.program_module_id, None)?;
            } else {
                println!("file or directory `{}` does not exist.", &arg_file);
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
                FileLookup::Disallowed,
            );
        }

        Ok(())
    }

    fn scan_file(
        &mut self,
        file_id: SourceFileId,
        file_path: PathBuf,
        module_id: ModuleDefinitionId,
        file_lookup: FileLookup,
        ast: &ast::File,
    ) -> Result<(), i32> {
        let mut gdef = GlobalDef {
            sa: self.sa,
            file_id,
            module_id,
            unresolved_modules: Vec::new(),
        };

        gdef.visit_file(ast);

        if !gdef.unresolved_modules.is_empty() {
            let mut directory = file_path;
            directory.pop();

            for id in gdef.unresolved_modules {
                self.add_module_files(directory.clone(), id, file_lookup)?;
            }
        }

        Ok(())
    }

    fn add_module_files(
        &mut self,
        directory: PathBuf,
        id: ModuleDefinitionId,
        file_lookup: FileLookup,
    ) -> Result<(), i32> {
        let module = self.sa.modules[id].clone();
        let module = module.read();
        let node = module.ast.clone().unwrap();
        let file_id = module.file_id.expect("missing file_id");

        let mut module_directory = directory;
        let name = self.sa.interner.str(node.name).to_string();
        module_directory.push(&name);

        match file_lookup {
            FileLookup::Directory => {
                self.add_files_in_directory(module_directory, id, Some((file_id, node.pos)))
            }

            FileLookup::Disallowed => panic!("cannot include other files here."),
            FileLookup::Stdlib => self.add_bundled_directory(module_directory, id),
        }
    }

    fn add_files_in_directory(
        &mut self,
        path: PathBuf,
        module_id: ModuleDefinitionId,
        error_location: Option<(SourceFileId, Position)>,
    ) -> Result<(), i32> {
        if path.is_dir() {
            for entry in fs::read_dir(path).unwrap() {
                let path = entry.unwrap().path();

                if should_file_be_parsed(&path) {
                    self.add_file(path.clone(), module_id, error_location)?;
                }
            }

            Ok(())
        } else if let Some((file_id, pos)) = error_location {
            self.sa
                .diag
                .lock()
                .report(file_id, pos, SemError::DirectoryNotFound(path));

            Ok(())
        } else {
            println!("directory `{:?}` does not exist.", path);

            Err(1)
        }
    }

    fn add_file(
        &mut self,
        path: PathBuf,
        module_id: ModuleDefinitionId,
        error_location: Option<(SourceFileId, Position)>,
    ) -> Result<(), i32> {
        let result = file_as_string(&path);

        match result {
            Ok(content) => {
                self.add_file_from_string(path, content, module_id, FileLookup::Directory);
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
        path: PathBuf,
        content: String,
        module_id: ModuleDefinitionId,
        file_lookup: FileLookup,
    ) {
        let file_id = self.sa.add_source_file(path, Arc::new(content), module_id);
        self.files_to_parse.push_back((file_id, file_lookup));
    }

    fn parse_file(&mut self, file_id: SourceFileId, file_lookup: FileLookup) -> Result<(), i32> {
        let module_id;
        let content;
        let path;

        {
            let file = self.sa.source_file(file_id);
            module_id = file.module_id;
            content = file.content.clone();
            path = file.path.clone();
        }

        let parser =
            Parser::from_shared_string(content, &self.sa.id_generator, &mut self.sa.interner);

        match parser.parse() {
            Ok(ast) => {
                self.scan_file(file_id, path, module_id, file_lookup, &ast)?;
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
    unresolved_modules: Vec<ModuleDefinitionId>,
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
            self.unresolved_modules.push(id);
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
        let id = self.sa.globals.push(global);

        let sym = Sym::Global(id);
        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_impl(&mut self, node: &Arc<ast::Impl>) {
        if node.trait_type.is_some() {
            let impl_ = ImplDefinition::new(self.file_id, self.module_id, node);
            self.sa.impls.push(impl_);
        } else {
            let extension = ExtensionDefinition::new(self.file_id, self.module_id, node);
            self.sa.extensions.push(extension);
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
        let cls = ClassDefinition::new(self.file_id, node, self.module_id);
        let id = self.sa.classes.push(cls);

        let sym = Sym::Class(id);

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

    let name = path.to_string_lossy();

    if !name.ends_with(".dora") {
        return false;
    }

    if name.ends_with("_x64.dora") {
        cfg!(target_arch = "x86_64")
    } else if name.ends_with("_arm64.dora") {
        cfg!(target_arch = "aarch64")
    } else {
        true
    }
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
        ok("const foo: Int32 = 0I;");
        err(
            "const foo: Int32 = 0I; fn foo() {}",
            pos(1, 24),
            SemError::ShadowConst("foo".into()),
        );
        err(
            "const foo: Int32 = 0I; class foo {}",
            pos(1, 24),
            SemError::ShadowConst("foo".into()),
        );
        err(
            "const foo: Int32 = 0I; struct foo {}",
            pos(1, 24),
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
