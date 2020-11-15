use parking_lot::RwLock;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::error::msg::SemError;
use crate::gc::Address;
use crate::semck::{report_term_shadow, report_type_shadow};
use crate::sym::{SymTable, TermSym, TypeSym};
use crate::ty::SourceType;
use crate::vm::{
    self, ClassId, ConstData, ConstId, ConstValue, EnumData, EnumId, ExtensionData, ExtensionId,
    Fct, FctParent, FileId, GlobalData, GlobalId, ImplData, ImplId, ImportData, Module, ModuleId,
    NamespaceData, NamespaceId, StructData, StructId, TraitData, TraitId, TypeParam, VM,
};
use dora_parser::ast::visit::Visitor;
use dora_parser::ast::{self, visit};
use dora_parser::interner::Name;
use dora_parser::lexer::reader::Reader;
use dora_parser::parser::Parser;

pub fn check(vm: &mut VM) -> Result<(), i32> {
    parse_initial_files(vm)?;

    let mut next_file = 0;

    loop {
        let (next, files_to_parse) = check_files(vm, next_file);
        next_file = next;

        if files_to_parse.is_empty() {
            break;
        }

        for file in files_to_parse {
            parse_file(vm, file)?;
        }
    }

    Ok(())
}

fn parse_initial_files(vm: &mut VM) -> Result<(), i32> {
    let stdlib_dir = vm.args.flag_stdlib.clone();
    let namespace_id = vm.stdlib_namespace_id;

    if let Some(stdlib) = stdlib_dir {
        parse_dir(vm, &stdlib, namespace_id)?;
    } else {
        parse_bundled_stdlib(vm, namespace_id)?;
    }

    let boots_dir = vm.args.flag_boots.clone();

    if let Some(boots) = boots_dir {
        parse_dir(vm, &boots, vm.boots_namespace_id)?;
    }

    if vm.parse_arg_file && !vm.args.arg_file.is_empty() {
        let arg_file = vm.args.arg_file.clone();
        let path = Path::new(&arg_file);

        if path.is_file() {
            let file = ParseFile {
                path: PathBuf::from(path),
                namespace_id: vm.global_namespace_id,
            };
            parse_file(vm, file)?;
        } else if path.is_dir() {
            parse_dir(vm, &arg_file, vm.global_namespace_id)?;
        } else {
            println!("file or directory `{}` does not exist.", &arg_file);
            return Err(1);
        }
    }

    Ok(())
}

fn check_files(vm: &mut VM, start: usize) -> (usize, Vec<ParseFile>) {
    let files = vm.files.clone();
    let files = files.read();

    let mut files_to_parse = Vec::new();

    for file in &files[start..] {
        let mut gdef = GlobalDef {
            vm,
            file_id: file.id,
            namespace_id: file.namespace_id,
            files_to_parse: &mut files_to_parse,
        };

        gdef.visit_file(&file.ast);
    }

    (files.len(), files_to_parse)
}

fn parse_dir(vm: &mut VM, dirname: &str, namespace_id: NamespaceId) -> Result<(), i32> {
    let path = Path::new(dirname);

    if path.is_dir() {
        for entry in fs::read_dir(path).unwrap() {
            let path = entry.unwrap().path();

            if should_file_be_parsed(&path) {
                let file = ParseFile {
                    path: PathBuf::from(path),
                    namespace_id,
                };
                parse_file(vm, file)?;
            }
        }

        Ok(())
    } else {
        println!("directory `{}` does not exist.", dirname);

        Err(1)
    }
}

fn parse_file(vm: &mut VM, file: ParseFile) -> Result<(), i32> {
    let namespace_id = file.namespace_id;
    let path = file.path;

    let reader = match Reader::from_file(path.to_str().unwrap()) {
        Ok(reader) => reader,

        Err(_) => {
            println!("unable to read file `{}`", path.to_str().unwrap());
            return Err(1);
        }
    };

    let parser = Parser::new(reader, &vm.id_generator, &mut vm.interner);

    match parser.parse() {
        Ok(ast) => {
            vm.add_file(Some(path), namespace_id, Arc::new(ast));
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

pub fn parse_bundled_stdlib(vm: &mut VM, namespace_id: NamespaceId) -> Result<(), i32> {
    use crate::driver::start::STDLIB;

    for (filename, content) in STDLIB {
        parse_bundled_stdlib_file(vm, namespace_id, filename, content)?;
    }

    Ok(())
}

fn parse_bundled_stdlib_file(
    vm: &mut VM,
    namespace_id: NamespaceId,
    filename: &str,
    content: &str,
) -> Result<(), i32> {
    let reader = Reader::from_string(filename, content);
    let parser = Parser::new(reader, &vm.id_generator, &mut vm.interner);

    match parser.parse() {
        Ok(ast) => {
            vm.add_file(None, namespace_id, Arc::new(ast));
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
    vm: &'x mut VM,
    file_id: FileId,
    namespace_id: NamespaceId,
    files_to_parse: &'x mut Vec<ParseFile>,
}

impl<'x> visit::Visitor for GlobalDef<'x> {
    fn visit_namespace(&mut self, node: &Arc<ast::Namespace>) {
        let id = NamespaceData::new(self.vm, self.namespace_id, node.name, node.is_pub);

        let sym = TermSym::Namespace(id);
        if let Some(sym) = self.insert_term(node.name, sym) {
            report_term_shadow(self.vm, node.name, self.file_id, node.pos, sym);
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
        let id: TraitId = (self.vm.traits.len() as u32).into();
        let xtrait = TraitData {
            id,
            file_id: self.file_id,
            ast: node.clone(),
            namespace_id: self.namespace_id,
            is_pub: node.is_pub,
            pos: node.pos,
            name: node.name,
            methods: Vec::new(),
        };

        self.vm.traits.push(RwLock::new(xtrait));

        let sym = TypeSym::Trait(id);
        if let Some(sym) = self.insert_type(node.name, sym) {
            report_type_shadow(self.vm, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_import(&mut self, node: &Arc<ast::Import>) {
        let import = ImportData {
            namespace_id: self.namespace_id,
            file_id: self.file_id,
            ast: node.clone(),
        };

        self.vm.imports.push(import);
    }

    fn visit_global(&mut self, node: &Arc<ast::Global>) {
        let id = {
            let mut globals = self.vm.globals.lock();
            let id: GlobalId = (globals.len() as u32).into();
            let global = GlobalData {
                id,
                file_id: self.file_id,
                ast: node.clone(),
                namespace_id: self.namespace_id,
                pos: node.pos,
                name: node.name,
                is_pub: node.is_pub,
                ty: SourceType::Unit,
                reassignable: node.reassignable,
                initializer: None,
                address_init: Address::null(),
                address_value: Address::null(),
            };

            globals.push(Arc::new(RwLock::new(global)));

            id
        };

        let sym = TermSym::Global(id);
        if let Some(sym) = self.insert_term(node.name, sym) {
            report_term_shadow(self.vm, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_impl(&mut self, node: &Arc<ast::Impl>) {
        if node.trait_type.is_some() {
            let id: ImplId = (self.vm.impls.len() as u32).into();
            let ximpl = ImplData {
                id,
                file_id: self.file_id,
                ast: node.clone(),
                namespace_id: self.namespace_id,
                pos: node.pos,
                trait_id: None,
                class_ty: SourceType::Error,
                methods: Vec::new(),
            };
            self.vm.impls.push(RwLock::new(ximpl));
        } else {
            let id: ExtensionId = self.vm.extensions.len().into();
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
            self.vm.extensions.push(RwLock::new(extension));
        }
    }

    fn visit_module(&mut self, node: &Arc<ast::Module>) {
        let id = {
            let mut modules = self.vm.modules.lock();

            let id: ModuleId = modules.len().into();
            let module = Module {
                id: id,
                name: node.name,
                file_id: self.file_id,
                namespace_id: self.namespace_id,
                ast: node.clone(),
                pos: node.pos,
                ty: self.vm.modu(id),
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

        let level = self.vm.namespace_table(self.namespace_id);
        let mut level = level.write();
        match level.get_term(node.name) {
            None => {
                level.insert_term(node.name, TermSym::Module(id));
            }
            Some(TermSym::ClassConstructor(class_id)) => {
                level.insert_term(node.name, TermSym::ClassConstructorAndModule(class_id, id));
            }
            Some(sym) => report_term_shadow(self.vm, node.name, self.file_id, node.pos, sym),
        }
    }

    fn visit_const(&mut self, node: &Arc<ast::Const>) {
        let id = {
            let mut consts = self.vm.consts.lock();
            let id: ConstId = consts.len().into();
            let xconst = ConstData {
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

        let sym = TermSym::Const(id);
        if let Some(sym) = self.insert_term(node.name, sym) {
            report_term_shadow(self.vm, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_class(&mut self, node: &Arc<ast::Class>) {
        let id = {
            let mut classes = self.vm.classes.lock();

            let id: ClassId = classes.len().into();
            let mut cls = vm::Class {
                id,
                name: node.name,
                ast: node.clone(),
                file_id: self.file_id,
                namespace_id: self.namespace_id,
                pos: node.pos,
                ty: self.vm.cls(id),
                parent_class: None,
                has_open: node.has_open,
                is_abstract: node.is_abstract,
                internal: node.internal,
                internal_resolved: false,
                has_constructor: node.has_constructor,
                table: SymTable::new(),
                is_pub: node.is_pub,

                constructor: None,
                fields: Vec::new(),
                methods: Vec::new(),
                virtual_fcts: Vec::new(),

                traits: Vec::new(),
                impls: Vec::new(),
                extensions: Vec::new(),

                type_params: Vec::new(),
                specializations: RwLock::new(HashMap::new()),

                is_array: false,
                is_str: false,
            };

            if let Some(ref type_params) = node.type_params {
                for param in type_params {
                    cls.type_params.push(TypeParam::new(param.name));
                }
            }

            classes.push(Arc::new(RwLock::new(cls)));

            id
        };

        let sym = TypeSym::Class(id);
        if let Some(sym) = self.insert_type(node.name, sym) {
            report_type_shadow(self.vm, node.name, self.file_id, node.pos, sym);
            return;
        }

        let level = self.vm.namespace_table(self.namespace_id);
        let mut level = level.write();
        match level.get_term(node.name) {
            None => {
                level.insert_term(node.name, TermSym::ClassConstructor(id));
            }
            Some(TermSym::Module(module_id)) => {
                level.insert_term(node.name, TermSym::ClassConstructorAndModule(id, module_id));
            }
            Some(sym) => report_term_shadow(self.vm, node.name, self.file_id, node.pos, sym),
        }
    }

    fn visit_struct(&mut self, node: &Arc<ast::Struct>) {
        let id = {
            let mut structs = self.vm.structs.lock();
            let id: StructId = (structs.len() as u32).into();
            let mut xstruct = StructData {
                id,
                file_id: self.file_id,
                ast: node.clone(),
                namespace_id: self.namespace_id,
                is_pub: node.is_pub,
                pos: node.pos,
                name: node.name,
                type_params: Vec::new(),
                fields: Vec::new(),
                specializations: RwLock::new(HashMap::new()),
            };

            if let Some(ref type_params) = node.type_params {
                for param in type_params {
                    xstruct.type_params.push(TypeParam::new(param.name));
                }
            }

            structs.push(Arc::new(RwLock::new(xstruct)));

            id
        };

        let sym = TypeSym::Struct(id);
        if let Some(sym) = self.insert_type(node.name, sym) {
            report_type_shadow(self.vm, node.name, self.file_id, node.pos, sym);
            return;
        }

        let level = self.vm.namespace_table(self.namespace_id);
        let mut level = level.write();
        match level.get_term(node.name) {
            None => {
                level.insert_term(node.name, TermSym::StructConstructor(id));
            }
            Some(TermSym::Module(module_id)) => {
                level.insert_term(
                    node.name,
                    TermSym::StructConstructorAndModule(id, module_id),
                );
            }
            Some(sym) => report_term_shadow(self.vm, node.name, self.file_id, node.pos, sym),
        }
    }

    fn visit_fct(&mut self, node: &Arc<ast::Function>) {
        let fct = Fct::new(self.file_id, self.namespace_id, node, FctParent::None);
        let fctid = self.vm.add_fct(fct);
        let sym = TermSym::Fct(fctid);

        if let Some(sym) = self.insert_term(node.name, sym) {
            report_term_shadow(self.vm, node.name, self.file_id, node.pos, sym);
        }
    }

    fn visit_enum(&mut self, node: &Arc<ast::Enum>) {
        let id: EnumId = self.vm.enums.len().into();
        let mut xenum = EnumData {
            id,
            file_id: self.file_id,
            namespace_id: self.namespace_id,
            ast: node.clone(),
            pos: node.pos,
            name: node.name,
            type_params: Vec::new(),
            is_pub: node.is_pub,
            variants: Vec::new(),
            name_to_value: HashMap::new(),
            extensions: Vec::new(),
            specializations: RwLock::new(HashMap::new()),
            simple_enumeration: false,
        };

        if let Some(ref type_params) = node.type_params {
            for param in type_params {
                xenum.type_params.push(TypeParam::new(param.name));
            }
        }

        self.vm.enums.push(RwLock::new(xenum));

        let sym = TypeSym::Enum(id);
        if let Some(sym) = self.insert_type(node.name, sym) {
            report_type_shadow(self.vm, node.name, self.file_id, node.pos, sym);
        }
    }
}

impl<'x> GlobalDef<'x> {
    fn parse_directory_into_namespace(&mut self, node: &ast::Namespace, namespace_id: NamespaceId) {
        let files = self.vm.files.clone();
        let files = files.read();
        let file = &files[self.file_id.to_usize()];

        if let Some(mut path) = file.path.clone() {
            path.pop();
            let name = self.vm.interner.str(node.name).to_string();
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
                self.vm
                    .diag
                    .lock()
                    .report(self.file_id, node.pos, SemError::DirectoryNotFound);
            }
        } else {
            unimplemented!();
        }
    }

    fn insert_type(&mut self, name: Name, sym: TypeSym) -> Option<TypeSym> {
        let level = self.vm.namespace_table(self.namespace_id);
        let mut level = level.write();
        level.insert_type(name, sym)
    }

    fn insert_term(&mut self, name: Name, sym: TermSym) -> Option<TermSym> {
        let level = self.vm.namespace_table(self.namespace_id);
        let mut level = level.write();
        level.insert_term(name, sym)
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
    use crate::error::msg::SemError;
    use crate::semck::tests::*;

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
            SemError::ShadowClassConstructor("Foo".into()),
        );
        err(
            "class Foo let Foo: Int32 = 1;",
            pos(1, 11),
            SemError::ShadowClassConstructor("Foo".into()),
        );
        err(
            "class Foo var Foo: Int32 = 1;",
            pos(1, 11),
            SemError::ShadowClassConstructor("Foo".into()),
        );
        err(
            "class Foo const Foo: Int32 = 1;",
            pos(1, 11),
            SemError::ShadowClassConstructor("Foo".into()),
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
            SemError::ShadowStructConstructor("Foo".into()),
        );
        err(
            "struct Foo {} let Foo: Int32 = 1;",
            pos(1, 15),
            SemError::ShadowStructConstructor("Foo".into()),
        );
        err(
            "struct Foo {} var Foo: Int32 = 1;",
            pos(1, 15),
            SemError::ShadowStructConstructor("Foo".into()),
        );
        err(
            "struct Foo {} const Foo: Int32 = 1;",
            pos(1, 15),
            SemError::ShadowStructConstructor("Foo".into()),
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
        ok("trait Foo {} fun Foo() {}");
    }

    #[test]
    fn test_module() {
        ok("module Foo {}");
        ok("module Foo {} struct Foo {}");
        ok("module Foo {} class Foo {}");
        err(
            "module Foo {} fun Foo() {}",
            pos(1, 15),
            SemError::ShadowModule("Foo".into()),
        );
    }

    #[test]
    fn test_module_with_fun() {
        ok("module Foo { fun bar(): Int32 = 0; }");
    }

    #[test]
    fn test_module_with_let() {
        ok("module Foo { let bar: Int32 = 0; }");
    }

    #[test]
    fn test_const() {
        ok("const foo: Int32 = 0;");
        err(
            "const foo: Int32 = 0; fun foo() {}",
            pos(1, 23),
            SemError::ShadowConst("foo".into()),
        );
        err(
            "const foo: Int32 = 0; class foo {}",
            pos(1, 23),
            SemError::ShadowConst("foo".into()),
        );
        err(
            "const foo: Int32 = 0; struct foo {}",
            pos(1, 23),
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
