use std::collections::VecDeque;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::language::error::msg::SemError;
use crate::language::report_sym_shadow;
use crate::language::sem_analysis::{
    AnnotationDefinition, ClassDefinition, ConstDefinition, EnumDefinition, ExtensionDefinition,
    FctDefinition, FctParent, GlobalDefinition, ImplDefinition, NamespaceDefinition,
    NamespaceDefinitionId, StructDefinition, TraitDefinition, UseDefinition,
};
use crate::language::sym::Sym;
use crate::vm::{FileId, SemAnalysis};
use dora_parser::ast::visit::Visitor;
use dora_parser::ast::{self, visit};
use dora_parser::interner::Name;
use dora_parser::lexer::reader::Reader;
use dora_parser::parser::Parser;

pub fn check(sa: &mut SemAnalysis) -> Result<(), i32> {
    let mut files_to_scan: VecDeque<ScanFile> = VecDeque::new();
    let mut files_to_parse: VecDeque<ParseFile> = VecDeque::new();

    parse_initial_files(sa, &mut files_to_scan)?;

    while !files_to_scan.is_empty() {
        scan_files(sa, &mut files_to_scan, &mut files_to_parse);
        parse_files(sa, &mut files_to_parse, &mut files_to_scan)?;
    }

    Ok(())
}

fn parse_initial_files(
    sa: &mut SemAnalysis,
    files_to_scan: &mut VecDeque<ScanFile>,
) -> Result<(), i32> {
    let stdlib_dir = sa.args.flag_stdlib.clone();
    let namespace_id = sa.stdlib_namespace_id;

    if let Some(stdlib) = stdlib_dir {
        parse_dir(sa, &stdlib, namespace_id, files_to_scan)?;
    } else {
        parse_bundled_stdlib(sa, namespace_id, files_to_scan)?;
    }

    let boots_dir = sa.args.flag_boots.clone();

    if let Some(boots) = boots_dir {
        parse_dir(sa, &boots, sa.boots_namespace_id, files_to_scan)?;
    }

    if sa.parse_arg_file && !sa.args.arg_file.is_empty() {
        let arg_file = sa.args.arg_file.clone();
        let path = Path::new(&arg_file);

        if path.is_file() {
            let file = ParseFile {
                path: PathBuf::from(path),
                namespace_id: sa.global_namespace_id,
            };
            let file = parse_file(sa, &file)?;
            files_to_scan.push_back(file);
        } else if path.is_dir() {
            parse_dir(sa, &arg_file, sa.global_namespace_id, files_to_scan)?;
        } else {
            println!("file or directory `{}` does not exist.", &arg_file);
            return Err(1);
        }
    }

    if let Some(content) = sa.additional_file_to_parse {
        let file = parse_string(sa, sa.global_namespace_id, "<<code>>".into(), content)?;
        files_to_scan.push_back(file);
    }

    Ok(())
}

fn scan_files(
    sa: &mut SemAnalysis,
    files_to_scan: &mut VecDeque<ScanFile>,
    files_to_parse: &mut VecDeque<ParseFile>,
) {
    while !files_to_scan.is_empty() {
        let ScanFile {
            file_id,
            path,
            namespace_id,
            ast,
        } = files_to_scan.pop_front().expect("no element");
        let mut gdef = GlobalDef {
            sa,
            file_id,
            path,
            namespace_id,
            files_to_parse,
        };

        gdef.visit_file(&ast);
    }
}

fn parse_dir(
    sa: &mut SemAnalysis,
    dirname: &str,
    namespace_id: NamespaceDefinitionId,
    files_to_scan: &mut VecDeque<ScanFile>,
) -> Result<(), i32> {
    let path = Path::new(dirname);

    if path.is_dir() {
        for entry in fs::read_dir(path).unwrap() {
            let path = entry.unwrap().path();

            if should_file_be_parsed(&path) {
                let file = ParseFile {
                    path: PathBuf::from(path),
                    namespace_id,
                };
                let file = parse_file(sa, &file)?;
                files_to_scan.push_back(file);
            }
        }

        Ok(())
    } else {
        println!("directory `{}` does not exist.", dirname);

        Err(1)
    }
}

fn parse_files(
    sa: &mut SemAnalysis,
    files_to_parse: &mut VecDeque<ParseFile>,
    files_to_scan: &mut VecDeque<ScanFile>,
) -> Result<(), i32> {
    while !files_to_parse.is_empty() {
        let file = files_to_parse.pop_front().expect("no file");
        let file = parse_file(sa, &file)?;
        files_to_scan.push_back(file);
    }

    Ok(())
}

fn parse_file(sa: &mut SemAnalysis, file: &ParseFile) -> Result<ScanFile, i32> {
    let namespace_id = file.namespace_id;
    let path = file.path.clone();

    let reader = match Reader::from_file(path.to_str().unwrap()) {
        Ok(reader) => reader,

        Err(_) => {
            println!("unable to read file `{}`", path.to_str().unwrap());
            return Err(1);
        }
    };

    let parser = Parser::new(reader, &sa.id_generator, &mut sa.interner);

    match parser.parse() {
        Ok(ast) => {
            let ast = Arc::new(ast);
            let file_id = sa.add_file(Some(path.clone()), namespace_id, ast.clone());
            Ok(ScanFile {
                file_id,
                namespace_id: file.namespace_id,
                path: Some(path.clone()),
                ast,
            })
        }

        Err(error) => {
            println!(
                "error in {} at {}: {}",
                path.to_str().unwrap(),
                error.pos,
                error.error.message()
            );
            println!("error during parsing.");

            Err(1)
        }
    }
}

fn parse_bundled_stdlib(
    sa: &mut SemAnalysis,
    namespace_id: NamespaceDefinitionId,
    files_to_scan: &mut VecDeque<ScanFile>,
) -> Result<(), i32> {
    use crate::driver::start::STDLIB;

    for (filename, content) in STDLIB {
        let file = parse_string(sa, namespace_id, filename, content)?;
        files_to_scan.push_back(file);
    }

    Ok(())
}

fn parse_string(
    sa: &mut SemAnalysis,
    namespace_id: NamespaceDefinitionId,
    filename: &str,
    content: &str,
) -> Result<ScanFile, i32> {
    let reader = Reader::from_string(filename, content);
    let parser = Parser::new(reader, &sa.id_generator, &mut sa.interner);

    match parser.parse() {
        Ok(ast) => {
            let ast = Arc::new(ast);
            let file_id = sa.add_file(None, namespace_id, ast.clone());
            let path = PathBuf::from(filename);
            Ok(ScanFile {
                file_id,
                path: Some(path),
                namespace_id,
                ast,
            })
        }

        Err(error) => {
            println!(
                "error in {} at {}: {}",
                filename,
                error.pos,
                error.error.message()
            );
            println!("error during parsing.");

            Err(1)
        }
    }
}

struct ParseFile {
    path: PathBuf,
    namespace_id: NamespaceDefinitionId,
}

struct ScanFile {
    file_id: FileId,
    path: Option<PathBuf>,
    namespace_id: NamespaceDefinitionId,
    ast: Arc<ast::File>,
}

struct GlobalDef<'x> {
    sa: &'x mut SemAnalysis,
    file_id: FileId,
    path: Option<PathBuf>,
    namespace_id: NamespaceDefinitionId,
    files_to_parse: &'x mut VecDeque<ParseFile>,
}

impl<'x> visit::Visitor for GlobalDef<'x> {
    fn visit_namespace(&mut self, node: &Arc<ast::Namespace>) {
        let namespace = NamespaceDefinition::new(self.sa, self.namespace_id, node);
        let id = self.sa.namespaces.push(namespace);
        let sym = Sym::Namespace(id);

        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }

        if node.elements.is_none() {
            self.parse_directory_into_namespace(node, id);
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
    fn parse_directory_into_namespace(
        &mut self,
        node: &ast::Namespace,
        namespace_id: NamespaceDefinitionId,
    ) {
        let mut dir_path = self.path.as_ref().expect("no path").clone();
        dir_path.pop();
        let name = self.sa.interner.str(node.name).to_string();
        dir_path.push(&name);

        if dir_path.is_dir() {
            for entry in fs::read_dir(dir_path).unwrap() {
                let path = entry.unwrap().path();

                if should_file_be_parsed(&path) {
                    self.files_to_parse.push_back(ParseFile {
                        path,
                        namespace_id: namespace_id,
                    });
                }
            }
        } else {
            self.sa
                .diag
                .lock()
                .report(self.file_id, node.pos, SemError::DirectoryNotFound);
        }
    }

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
