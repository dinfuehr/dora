use parking_lot::RwLock;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::gc::Address;
use crate::language::error::msg::SemError;
use crate::language::report_sym_shadow;
use crate::language::sem_analysis::{
    ConstDefinition, ConstDefinitionId, ConstValue, GlobalDefinition, GlobalDefinitionId,
    NamespaceData, NamespaceId, StructDefinition, StructDefinitionId,
};
use crate::language::sym::Sym;
use crate::language::ty::SourceType;
use crate::vm::{
    self, AnnotationDefinitionId, ClassDefinitionId, EnumDefinition, EnumDefinitionId,
    ExtensionData, ExtensionId, FctDefinition, FctParent, FileId, ImplData, ImplId, ImportData,
    Module, ModuleId, SemAnalysis, TraitDefinition, TraitDefinitionId, TypeParam,
    TypeParamDefinition,
};
use dora_parser::ast::visit::Visitor;
use dora_parser::ast::{self, visit};
use dora_parser::interner::Name;
use dora_parser::lexer::reader::Reader;
use dora_parser::parser::Parser;

pub fn check(sa: &mut SemAnalysis) -> Result<(), i32> {
    parse_initial_files(sa)?;

    let mut next_file = 0;

    loop {
        let (next, files_to_parse) = check_files(sa, next_file);
        next_file = next;

        if files_to_parse.is_empty() {
            break;
        }

        for file in files_to_parse {
            parse_file(sa, file)?;
        }
    }

    Ok(())
}

fn parse_initial_files(sa: &mut SemAnalysis) -> Result<(), i32> {
    let stdlib_dir = sa.args.flag_stdlib.clone();
    let namespace_id = sa.stdlib_namespace_id;

    if let Some(stdlib) = stdlib_dir {
        parse_dir(sa, &stdlib, namespace_id)?;
    } else {
        parse_bundled_stdlib(sa, namespace_id)?;
    }

    let boots_dir = sa.args.flag_boots.clone();

    if let Some(boots) = boots_dir {
        parse_dir(sa, &boots, sa.boots_namespace_id)?;
    }

    if sa.parse_arg_file && !sa.args.arg_file.is_empty() {
        let arg_file = sa.args.arg_file.clone();
        let path = Path::new(&arg_file);

        if path.is_file() {
            let file = ParseFile {
                path: PathBuf::from(path),
                namespace_id: sa.global_namespace_id,
            };
            parse_file(sa, file)?;
        } else if path.is_dir() {
            parse_dir(sa, &arg_file, sa.global_namespace_id)?;
        } else {
            println!("file or directory `{}` does not exist.", &arg_file);
            return Err(1);
        }
    }

    Ok(())
}

fn check_files(sa: &mut SemAnalysis, start: usize) -> (usize, Vec<ParseFile>) {
    let files = sa.files.clone();
    let files = files.read();

    let mut files_to_parse = Vec::new();

    for file in &files[start..] {
        let mut gdef = GlobalDef {
            sa,
            file_id: file.id,
            namespace_id: file.namespace_id,
            files_to_parse: &mut files_to_parse,
        };

        gdef.visit_file(&file.ast);
    }

    (files.len(), files_to_parse)
}

fn parse_dir(sa: &mut SemAnalysis, dirname: &str, namespace_id: NamespaceId) -> Result<(), i32> {
    let path = Path::new(dirname);

    if path.is_dir() {
        for entry in fs::read_dir(path).unwrap() {
            let path = entry.unwrap().path();

            if should_file_be_parsed(&path) {
                let file = ParseFile {
                    path: PathBuf::from(path),
                    namespace_id,
                };
                parse_file(sa, file)?;
            }
        }

        Ok(())
    } else {
        println!("directory `{}` does not exist.", dirname);

        Err(1)
    }
}

fn parse_file(sa: &mut SemAnalysis, file: ParseFile) -> Result<(), i32> {
    let namespace_id = file.namespace_id;
    let path = file.path;

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
            sa.add_file(Some(path), namespace_id, Arc::new(ast));
            Ok(())
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

pub fn parse_bundled_stdlib(sa: &mut SemAnalysis, namespace_id: NamespaceId) -> Result<(), i32> {
    use crate::driver::start::STDLIB;

    for (filename, content) in STDLIB {
        parse_bundled_stdlib_file(sa, namespace_id, filename, content)?;
    }

    Ok(())
}

fn parse_bundled_stdlib_file(
    sa: &mut SemAnalysis,
    namespace_id: NamespaceId,
    filename: &str,
    content: &str,
) -> Result<(), i32> {
    let reader = Reader::from_string(filename, content);
    let parser = Parser::new(reader, &sa.id_generator, &mut sa.interner);

    match parser.parse() {
        Ok(ast) => {
            sa.add_file(None, namespace_id, Arc::new(ast));
            Ok(())
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
    namespace_id: NamespaceId,
}

struct GlobalDef<'x> {
    sa: &'x mut SemAnalysis,
    file_id: FileId,
    namespace_id: NamespaceId,
    files_to_parse: &'x mut Vec<ParseFile>,
}

impl<'x> visit::Visitor for GlobalDef<'x> {
    fn visit_namespace(&mut self, node: &Arc<ast::Namespace>) {
        let namespace = NamespaceData::new(self.sa, self.namespace_id, node);
        let id = namespace.id.clone();
        let sym = Sym::Namespace(id);

        self.sa.namespaces.push(RwLock::new(namespace));

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
        let id: TraitDefinitionId = (self.sa.traits.len() as u32).into();
        let xtrait = TraitDefinition {
            id,
            file_id: self.file_id,
            ast: node.clone(),
            namespace_id: self.namespace_id,
            is_pub: node.is_pub,
            pos: node.pos,
            name: node.name,
            type_params: Vec::new(),
            type_params2: TypeParamDefinition::new(),
            methods: Vec::new(),
            instance_names: HashMap::new(),
            static_names: HashMap::new(),
            vtables: RwLock::new(HashMap::new()),
        };

        self.sa.traits.push(RwLock::new(xtrait));

        let sym = Sym::Trait(id);
        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_import(&mut self, node: &Arc<ast::Import>) {
        let import = ImportData {
            namespace_id: self.namespace_id,
            file_id: self.file_id,
            ast: node.clone(),
        };

        self.sa.imports.push(import);
    }

    fn visit_global(&mut self, node: &Arc<ast::Global>) {
        let id = {
            let mut globals = self.sa.globals.lock();
            let id: GlobalDefinitionId = (globals.len() as u32).into();
            let global = GlobalDefinition {
                id,
                file_id: self.file_id,
                ast: node.clone(),
                namespace_id: self.namespace_id,
                pos: node.pos,
                name: node.name,
                is_pub: node.is_pub,
                ty: SourceType::Unit,
                mutable: node.mutable,
                initializer: None,
                address_init: Address::null(),
                address_value: Address::null(),
            };

            globals.push(Arc::new(RwLock::new(global)));

            id
        };

        let sym = Sym::Global(id);
        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_impl(&mut self, node: &Arc<ast::Impl>) {
        if node.trait_type.is_some() {
            let id: ImplId = (self.sa.impls.len() as u32).into();
            let mut impl_type_params = Vec::new();
            if let Some(ref type_params) = node.type_params {
                for param in type_params {
                    impl_type_params.push(TypeParam::new(param.name));
                }
            }
            let ximpl = ImplData {
                id,
                file_id: self.file_id,
                ast: node.clone(),
                namespace_id: self.namespace_id,
                type_params: impl_type_params,
                pos: node.pos,
                trait_id: None,
                ty: SourceType::Error,
                methods: Vec::new(),
                instance_names: HashMap::new(),
                static_names: HashMap::new(),
                impl_for: HashMap::new(),
            };
            self.sa.impls.push(RwLock::new(ximpl));
        } else {
            let id: ExtensionId = self.sa.extensions.len().into();
            let mut extension_type_params = Vec::new();
            if let Some(ref type_params) = node.type_params {
                for param in type_params {
                    extension_type_params.push(TypeParam::new(param.name));
                }
            }
            let extension = ExtensionData {
                id,
                file_id: self.file_id,
                namespace_id: self.namespace_id,
                ast: node.clone(),
                pos: node.pos,
                type_params: extension_type_params,
                ty: SourceType::Error,
                methods: Vec::new(),
                instance_names: HashMap::new(),
                static_names: HashMap::new(),
            };
            self.sa.extensions.push(RwLock::new(extension));
        }
    }

    fn visit_module(&mut self, node: &Arc<ast::Module>) {
        let id = {
            let mut modules = self.sa.modules.lock();

            let id: ModuleId = modules.len().into();
            let module = Module {
                id: id,
                name: node.name,
                file_id: self.file_id,
                namespace_id: self.namespace_id,
                ast: node.clone(),
                pos: node.pos,
                ty: SourceType::Module(id),
                parent_class: None,
                internal: node.internal,
                internal_resolved: false,
                has_constructor: node.has_constructor,
                is_pub: node.is_pub,

                constructor: None,
                fields: Vec::new(),
                methods: Vec::new(),
                virtual_fcts: Vec::new(),

                traits: Vec::new(),
            };

            modules.push(Arc::new(RwLock::new(module)));

            id
        };

        let sym = Sym::Module(id);
        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_const(&mut self, node: &Arc<ast::Const>) {
        let id = {
            let mut consts = self.sa.consts.lock();
            let id: ConstDefinitionId = consts.len().into();
            let xconst = ConstDefinition {
                id,
                file_id: self.file_id,
                ast: node.clone(),
                namespace_id: self.namespace_id,
                pos: node.pos,
                name: node.name,
                is_pub: node.is_pub,
                ty: SourceType::Error,
                expr: node.expr.clone(),
                value: ConstValue::None,
            };

            consts.push(Arc::new(RwLock::new(xconst)));

            id
        };

        let sym = Sym::Const(id);
        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_class(&mut self, node: &Arc<ast::Class>) {
        let id = {
            let mut classes = self.sa.classes.lock();

            let id: ClassDefinitionId = classes.len().into();
            let cls = vm::ClassDefinition::new(&self.sa, id, self.file_id, node, self.namespace_id);

            classes.push(Arc::new(RwLock::new(cls)));

            id
        };

        let sym = Sym::Class(id);

        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_struct(&mut self, node: &Arc<ast::Struct>) {
        let id = {
            let mut structs = self.sa.structs.lock();
            let id: StructDefinitionId = (structs.len() as u32).into();
            let mut xstruct = StructDefinition {
                id,
                file_id: self.file_id,
                ast: node.clone(),
                namespace_id: self.namespace_id,
                primitive_ty: None,
                is_pub: node.is_pub,
                pos: node.pos,
                name: node.name,
                internal: node.internal,
                internal_resolved: false,
                type_params: Vec::new(),
                type_params2: TypeParamDefinition::new(),
                fields: Vec::new(),
                field_names: HashMap::new(),
                specializations: RwLock::new(HashMap::new()),
                impls: Vec::new(),
                extensions: Vec::new(),
            };

            if let Some(ref type_params) = node.type_params {
                for param in type_params {
                    xstruct.type_params.push(TypeParam::new(param.name));
                }
            }

            structs.push(Arc::new(RwLock::new(xstruct)));

            id
        };

        let sym = Sym::Struct(id);
        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_annotation(&mut self, node: &Arc<ast::Annotation>) {
        let id = {
            let mut annotations = self.sa.annotations.lock();
            let id: AnnotationDefinitionId = annotations.len().into();
            let annotation = vm::AnnotationDefinition::new(
                id,
                self.file_id,
                node.pos,
                node.name,
                self.namespace_id,
            );
            annotations.push(Arc::new(RwLock::new(annotation)));
            id
        };

        let sym = Sym::Annotation(id);

        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_fct(&mut self, node: &Arc<ast::Function>) {
        let fct = FctDefinition::new(
            self.sa,
            self.file_id,
            self.namespace_id,
            node,
            FctParent::None,
        );
        let fctid = self.sa.add_fct(fct);
        let sym = Sym::Fct(fctid);

        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_enum(&mut self, node: &Arc<ast::Enum>) {
        let id: EnumDefinitionId = self.sa.enums.len().into();
        let mut xenum = EnumDefinition {
            id,
            file_id: self.file_id,
            namespace_id: self.namespace_id,
            ast: node.clone(),
            pos: node.pos,
            name: node.name,
            type_params: Vec::new(),
            type_params2: TypeParamDefinition::new(),
            is_pub: node.is_pub,
            variants: Vec::new(),
            name_to_value: HashMap::new(),
            impls: Vec::new(),
            extensions: Vec::new(),
            specializations: RwLock::new(HashMap::new()),
            simple_enumeration: false,
        };

        if let Some(ref type_params) = node.type_params {
            for param in type_params {
                xenum.type_params.push(TypeParam::new(param.name));
            }
        }

        self.sa.enums.push(RwLock::new(xenum));

        let sym = Sym::Enum(id);
        if let Some(sym) = self.insert(node.name, sym) {
            report_sym_shadow(self.sa, node.name, self.file_id, node.pos, sym);
        }
    }
}

impl<'x> GlobalDef<'x> {
    fn parse_directory_into_namespace(&mut self, node: &ast::Namespace, namespace_id: NamespaceId) {
        let files = self.sa.files.clone();
        let files = files.read();
        let file = &files[self.file_id.to_usize()];

        if let Some(mut path) = file.path.clone() {
            path.pop();
            let name = self.sa.interner.str(node.name).to_string();
            path.push(&name);

            if path.is_dir() {
                for entry in fs::read_dir(path).unwrap() {
                    let path = entry.unwrap().path();

                    if should_file_be_parsed(&path) {
                        self.files_to_parse.push(ParseFile {
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
        } else {
            unimplemented!();
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
            "fun Foo() {} class Foo",
            pos(1, 14),
            SemError::ShadowFunction("Foo".into()),
        );
        err(
            "class Foo fun Foo() {}",
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
            "fun Foo() {} struct Foo {}",
            pos(1, 14),
            SemError::ShadowFunction("Foo".into()),
        );
        err(
            "struct Foo {} fun Foo() {}",
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
    fn test_module() {
        ok("module Foo {}");
        err(
            "module Foo {} struct Foo {}",
            pos(1, 15),
            SemError::ShadowModule("Foo".into()),
        );
        err(
            "module Foo {} fun Foo() {}",
            pos(1, 15),
            SemError::ShadowModule("Foo".into()),
        );
    }

    #[test]
    fn test_module_with_fun() {
        ok("module Foo { fun bar(): Int32 = 0I; }");
    }

    #[test]
    fn test_module_with_let() {
        ok("module Foo { let bar: Int32 = 0I; }");
    }

    #[test]
    fn test_const() {
        ok("const foo: Int32 = 0I;");
        err(
            "const foo: Int32 = 0I; fun foo() {}",
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
        ok("fun bar() {} namespace foo { fun bar() {} }");

        err(
            "namespace foo {} namespace foo {}",
            pos(1, 18),
            SemError::ShadowNamespace("foo".into()),
        );

        err(
            "namespace foo { fun bar() {} fun bar() {} }",
            pos(1, 30),
            SemError::ShadowFunction("bar".into()),
        );
    }
}
