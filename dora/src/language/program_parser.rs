use std::collections::VecDeque;
use std::fs;
use std::io::{Error, Read};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::language::error::msg::SemError;
use crate::language::report_sym_shadow;
use crate::language::sem_analysis::{
    AnnotationDefinition, ClassDefinition, ConstDefinition, EnumDefinition, ExtensionDefinition,
    FctDefinition, FctParent, GlobalDefinition, ImplDefinition, NamespaceDefinition,
    NamespaceDefinitionId, SourceFileId, StructDefinition, TraitDefinition, UseDefinition,
};
use crate::language::sym::Sym;
use crate::vm::SemAnalysis;
use dora_parser::ast::visit::Visitor;
use dora_parser::ast::{self, visit};
use dora_parser::interner::Name;
use dora_parser::parser::Parser;
use dora_parser::Position;

pub fn parse(sa: &mut SemAnalysis) -> Result<(), i32> {
    let mut discoverer = ProgramParser::new(sa);
    discoverer.parse_all()
}

struct ProgramParser<'a> {
    sa: &'a mut SemAnalysis,
    files_to_parse: VecDeque<SourceFileId>,
}

impl<'a> ProgramParser<'a> {
    fn new(sa: &mut SemAnalysis) -> ProgramParser {
        ProgramParser {
            sa,
            files_to_parse: VecDeque::new(),
        }
    }

    fn parse_all(&mut self) -> Result<(), i32> {
        self.add_initial_files()?;

        while let Some(file_id) = self.files_to_parse.pop_front() {
            self.parse_file(file_id)?;
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
        let namespace_id = self.sa.stdlib_namespace_id;

        if let Some(stdlib) = stdlib_dir {
            self.add_files_in_directory(PathBuf::from(stdlib), namespace_id, None)?;
        } else {
            self.add_bundled_stdlib(namespace_id)?;
        }

        Ok(())
    }

    fn add_bundled_stdlib(&mut self, namespace_id: NamespaceDefinitionId) -> Result<(), i32> {
        use crate::driver::start::STDLIB;

        for (filename, content) in STDLIB {
            let path = PathBuf::from(filename);
            self.add_file_from_string(path, content.to_string(), namespace_id);
        }

        Ok(())
    }

    fn add_boots_files(&mut self) -> Result<(), i32> {
        let boots_dir = self.sa.args.flag_boots.clone();

        if let Some(boots) = boots_dir {
            self.add_files_in_directory(PathBuf::from(boots), self.sa.boots_namespace_id, None)?;
        }

        Ok(())
    }

    fn add_program_files(&mut self) -> Result<(), i32> {
        if self.sa.parse_arg_file && !self.sa.args.arg_file.is_empty() {
            let arg_file = self.sa.args.arg_file.clone();
            let path = PathBuf::from(&arg_file);

            if path.is_file() {
                self.add_file(PathBuf::from(path), self.sa.program_namespace_id, None)?;
            } else if path.is_dir() {
                self.add_files_in_directory(path, self.sa.program_namespace_id, None)?;
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
                self.sa.program_namespace_id,
            );
        }

        Ok(())
    }

    fn scan_file(
        &mut self,
        file_id: SourceFileId,
        file_path: PathBuf,
        namespace_id: NamespaceDefinitionId,
        ast: &ast::File,
    ) -> Result<(), i32> {
        let mut gdef = GlobalDef {
            sa: self.sa,
            file_id,
            namespace_id,
            unresolved_namespaces: Vec::new(),
        };

        gdef.visit_file(ast);

        if !gdef.unresolved_namespaces.is_empty() {
            let mut directory = file_path;
            directory.pop();

            for id in gdef.unresolved_namespaces {
                self.add_namespace_files(directory.clone(), id)?;
            }
        }

        Ok(())
    }

    fn add_namespace_files(
        &mut self,
        directory: PathBuf,
        id: NamespaceDefinitionId,
    ) -> Result<(), i32> {
        let namespace = self.sa.namespaces[id].clone();
        let namespace = namespace.read();
        let node = namespace.ast.clone().unwrap();
        let file_id = namespace.file_id.expect("missing file_id");

        let mut namespace_directory = directory;
        let name = self.sa.interner.str(node.name).to_string();
        namespace_directory.push(&name);

        self.add_files_in_directory(namespace_directory, id, Some((file_id, node.pos)))
    }

    fn add_files_in_directory(
        &mut self,
        path: PathBuf,
        namespace_id: NamespaceDefinitionId,
        error_location: Option<(SourceFileId, Position)>,
    ) -> Result<(), i32> {
        if path.is_dir() {
            for entry in fs::read_dir(path).unwrap() {
                let path = entry.unwrap().path();

                if should_file_be_parsed(&path) {
                    self.add_file(path.clone(), namespace_id, error_location)?;
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
        namespace_id: NamespaceDefinitionId,
        error_location: Option<(SourceFileId, Position)>,
    ) -> Result<(), i32> {
        let result = file_as_string(&path);

        match result {
            Ok(content) => {
                self.add_file_from_string(path, content, namespace_id);
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
        namespace_id: NamespaceDefinitionId,
    ) {
        let file_id = self
            .sa
            .add_source_file(path, Arc::new(content), namespace_id);
        self.files_to_parse.push_back(file_id);
    }

    fn parse_file(&mut self, file_id: SourceFileId) -> Result<(), i32> {
        let namespace_id;
        let content;
        let path;

        {
            let file = self.sa.source_file(file_id);
            namespace_id = file.namespace_id;
            content = file.content.clone();
            path = file.path.clone();
        }

        let parser =
            Parser::from_shared_string(content, &self.sa.id_generator, &mut self.sa.interner);

        match parser.parse() {
            Ok(ast) => {
                self.scan_file(file_id, path, namespace_id, &ast)?;
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
    namespace_id: NamespaceDefinitionId,
    unresolved_namespaces: Vec<NamespaceDefinitionId>,
}

impl<'x> visit::Visitor for GlobalDef<'x> {
    fn visit_namespace(&mut self, node: &Arc<ast::Namespace>) {
        let namespace = NamespaceDefinition::new(self.sa, self.namespace_id, self.file_id, node);
        let id = self.sa.namespaces.push(namespace);
        let sym = Sym::Namespace(id);

        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }

        if node.elements.is_none() {
            self.unresolved_namespaces.push(id);
        } else {
            let saved_namespace_id = self.namespace_id;
            self.namespace_id = id;
            visit::walk_namespace(self, node);
            self.namespace_id = saved_namespace_id;
        }
    }

    fn visit_trait(&mut self, node: &Arc<ast::Trait>) {
        let xtrait = TraitDefinition::new(self.file_id, self.namespace_id, node);
        let id = self.sa.traits.push(xtrait);

        let sym = Sym::Trait(id);
        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_use(&mut self, node: &Arc<ast::Use>) {
        let use_def = UseDefinition::new(self.file_id, self.namespace_id, node);
        self.sa.uses.push(use_def);
    }

    fn visit_global(&mut self, node: &Arc<ast::Global>) {
        let global = GlobalDefinition::new(self.file_id, self.namespace_id, node);
        let id = self.sa.globals.push(global);

        let sym = Sym::Global(id);
        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_impl(&mut self, node: &Arc<ast::Impl>) {
        if node.trait_type.is_some() {
            let ximpl = ImplDefinition::new(self.file_id, self.namespace_id, node);
            self.sa.impls.push(ximpl);
        } else {
            let extension = ExtensionDefinition::new(self.file_id, self.namespace_id, node);
            self.sa.extensions.push(extension);
        }
    }

    fn visit_const(&mut self, node: &Arc<ast::Const>) {
        let xconst = ConstDefinition::new(self.file_id, self.namespace_id, node);
        let id = self.sa.consts.push(xconst);

        let sym = Sym::Const(id);
        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_class(&mut self, node: &Arc<ast::Class>) {
        let cls = ClassDefinition::new(self.file_id, node, self.namespace_id);
        let id = self.sa.classes.push(cls);

        let sym = Sym::Class(id);

        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_struct(&mut self, node: &Arc<ast::Struct>) {
        let xstruct = StructDefinition::new(self.file_id, self.namespace_id, node);
        let id = self.sa.structs.push(xstruct);

        let sym = Sym::Struct(id);
        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_annotation(&mut self, node: &Arc<ast::Annotation>) {
        let annotation =
            AnnotationDefinition::new(self.file_id, node.pos, node.name, self.namespace_id);
        let id = self.sa.annotations.push(annotation);

        let sym = Sym::Annotation(id);

        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_fct(&mut self, node: &Arc<ast::Function>) {
        let fct = FctDefinition::new(self.file_id, self.namespace_id, node, FctParent::None);
        let fctid = self.sa.add_fct(fct);
        let sym = Sym::Fct(fctid);

        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_enum(&mut self, node: &Arc<ast::Enum>) {
        let xenum = EnumDefinition::new(self.file_id, self.namespace_id, node);
        let id = self.sa.enums.push(xenum);

        let sym = Sym::Enum(id);
        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }
}

impl<'x> GlobalDef<'x> {
    fn insert(&mut self, name: Name, sym: Sym) -> Option<Sym> {
        let level = self.sa.namespace_table(self.namespace_id);
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
    fn test_namespace() {
        ok("namespace foo {} namespace bar {}");
        ok("fn bar() {} namespace foo { fn bar() {} }");

        err(
            "namespace foo {} namespace foo {}",
            pos(1, 18),
            SemError::ShadowNamespace("foo".into()),
        );

        err(
            "namespace foo { fn bar() {} fn bar() {} }",
            pos(1, 29),
            SemError::ShadowFunction("bar".into()),
        );
    }
}
