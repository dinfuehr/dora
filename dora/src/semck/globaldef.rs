use parking_lot::RwLock;
use std::collections::HashMap;
use std::sync::Arc;

use crate::gc::Address;
use crate::semck::{report_term_shadow, report_type_shadow};
use crate::sym::{SymTable, TermSym, TypeSym};
use crate::ty::SourceType;
use crate::vm::{
    self, ClassId, ConstData, ConstId, ConstValue, EnumData, EnumId, ExtensionData, ExtensionId,
    Fct, FctId, FctKind, FctParent, FctSrc, GlobalData, GlobalId, ImplData, ImplId, Module,
    ModuleId, NamespaceData, NamespaceId, NodeMap, StructData, StructId, TraitData, TraitId,
    TypeParam, VM,
};
use dora_parser::ast::visit::Visitor;
use dora_parser::ast::{self, visit};
use dora_parser::interner::Name;

pub fn check(
    vm: &mut VM,
    map_cls_defs: &mut NodeMap<ClassId>,
    map_module_defs: &mut NodeMap<ModuleId>,
    map_namespaces: &mut NodeMap<NamespaceId>,
) {
    let files = vm.files.clone();

    let mut gdef = GlobalDef {
        vm,
        file_id: 0,
        namespace_id: None,
        map_cls_defs,
        map_module_defs,
        map_namespaces,
    };

    let files = files.read();
    for file in files.iter() {
        gdef.visit_file(file);
    }
}

struct GlobalDef<'x> {
    vm: &'x mut VM,
    file_id: u32,
    namespace_id: Option<NamespaceId>,
    map_cls_defs: &'x mut NodeMap<ClassId>,
    map_module_defs: &'x mut NodeMap<ModuleId>,
    map_namespaces: &'x mut NodeMap<NamespaceId>,
}

impl<'x> visit::Visitor for GlobalDef<'x> {
    fn visit_file(&mut self, f: &ast::File) {
        visit::walk_file(self, f);
        self.file_id += 1;
    }

    fn visit_namespace(&mut self, n: &Arc<ast::Namespace>) {
        let id: NamespaceId = self.vm.namespaces.len().into();
        let namespace = NamespaceData {
            id,
            file_id: self.file_id.into(),
            pos: n.pos,
            name: n.name,
            namespace_id: self.namespace_id,
            table: Arc::new(RwLock::new(SymTable::new())),
        };

        self.vm.namespaces.push(namespace);
        self.map_namespaces.insert(n.id, id);

        let sym = TermSym::Namespace(id);
        if let Some(sym) = self.insert_term(n.name, sym) {
            report_term_shadow(self.vm, n.name, self.file_id.into(), n.pos, sym);
        }

        let saved_namespace_id = self.namespace_id;
        self.namespace_id = Some(id);
        visit::walk_namespace(self, n);
        self.namespace_id = saved_namespace_id;
    }

    fn visit_trait(&mut self, t: &Arc<ast::Trait>) {
        let id: TraitId = (self.vm.traits.len() as u32).into();
        let xtrait = TraitData {
            id,
            file_id: self.file_id.into(),
            ast: t.clone(),
            namespace_id: self.namespace_id,
            pos: t.pos,
            name: t.name,
            methods: Vec::new(),
        };

        self.vm.traits.push(RwLock::new(xtrait));

        let sym = TypeSym::Trait(id);
        if let Some(sym) = self.insert_type(t.name, sym) {
            report_type_shadow(self.vm, t.name, self.file_id.into(), t.pos, sym);
        }
    }

    fn visit_global(&mut self, node: &Arc<ast::Global>) {
        let id = {
            let mut globals = self.vm.globals.lock();
            let id: GlobalId = (globals.len() as u32).into();
            let global = GlobalData {
                id,
                file_id: self.file_id.into(),
                ast: node.clone(),
                namespace_id: self.namespace_id,
                pos: node.pos,
                name: node.name,
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
            report_term_shadow(self.vm, node.name, self.file_id.into(), node.pos, sym);
        }
    }

    fn visit_impl(&mut self, ast: &Arc<ast::Impl>) {
        if ast.trait_type.is_some() {
            let id: ImplId = (self.vm.impls.len() as u32).into();
            let ximpl = ImplData {
                id,
                file_id: self.file_id.into(),
                ast: ast.clone(),
                namespace_id: self.namespace_id,
                pos: ast.pos,
                trait_id: None,
                class_ty: SourceType::Error,
                methods: Vec::new(),
            };
            self.vm.impls.push(RwLock::new(ximpl));
        } else {
            let id: ExtensionId = self.vm.extensions.len().into();
            let mut extension_type_params = Vec::new();
            if let Some(ref type_params) = ast.type_params {
                for param in type_params {
                    extension_type_params.push(TypeParam::new(param.name));
                }
            }
            let extension = ExtensionData {
                id,
                file_id: self.file_id.into(),
                namespace_id: self.namespace_id,
                ast: ast.clone(),
                pos: ast.pos,
                type_params: extension_type_params,
                ty: SourceType::Error,
                methods: Vec::new(),
                instance_names: HashMap::new(),
                static_names: HashMap::new(),
            };
            self.vm.extensions.push(RwLock::new(extension));
        }
    }

    fn visit_module(&mut self, m: &ast::Module) {
        let id = {
            let mut modules = self.vm.modules.lock();

            let id: ModuleId = modules.len().into();
            let module = Module {
                id: id,
                name: m.name,
                file_id: self.file_id.into(),
                namespace_id: self.namespace_id,
                pos: m.pos,
                ty: self.vm.modu(id),
                parent_class: None,
                internal: m.internal,
                internal_resolved: false,
                has_constructor: m.has_constructor,

                constructor: None,
                fields: Vec::new(),
                methods: Vec::new(),
                virtual_fcts: Vec::new(),

                traits: Vec::new(),
            };

            modules.push(Arc::new(RwLock::new(module)));

            id
        };

        self.map_module_defs.insert(m.id, id);

        let level = self.vm.namespace_table(self.namespace_id);
        let mut level = level.write();
        match level.get_term(m.name) {
            None => {
                level.insert_term(m.name, TermSym::Module(id));
            }
            Some(TermSym::ClassConstructor(class_id)) => {
                level.insert_term(m.name, TermSym::ClassConstructorAndModule(class_id, id));
            }
            Some(sym) => report_term_shadow(self.vm, m.name, self.file_id.into(), m.pos, sym),
        }
    }

    fn visit_const(&mut self, node: &Arc<ast::Const>) {
        let id = {
            let mut consts = self.vm.consts.lock();
            let id: ConstId = consts.len().into();
            let xconst = ConstData {
                id,
                file_id: self.file_id.into(),
                ast: node.clone(),
                namespace_id: self.namespace_id,
                pos: node.pos,
                name: node.name,
                ty: SourceType::Error,
                expr: node.expr.clone(),
                value: ConstValue::None,
            };

            consts.push(Arc::new(RwLock::new(xconst)));

            id
        };

        let sym = TermSym::Const(id);
        if let Some(sym) = self.insert_term(node.name, sym) {
            report_term_shadow(self.vm, node.name, self.file_id.into(), node.pos, sym);
        }
    }

    fn visit_class(&mut self, c: &Arc<ast::Class>) {
        let id = {
            let mut classes = self.vm.classes.lock();

            let id: ClassId = classes.len().into();
            let mut cls = vm::Class {
                id,
                name: c.name,
                ast: c.clone(),
                file_id: self.file_id.into(),
                namespace_id: self.namespace_id,
                pos: c.pos,
                ty: self.vm.cls(id),
                parent_class: None,
                has_open: c.has_open,
                is_abstract: c.is_abstract,
                internal: c.internal,
                internal_resolved: false,
                has_constructor: c.has_constructor,
                table: SymTable::new(),

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

            if let Some(ref type_params) = c.type_params {
                for param in type_params {
                    cls.type_params.push(TypeParam::new(param.name));
                }
            }

            classes.push(Arc::new(RwLock::new(cls)));

            id
        };

        self.map_cls_defs.insert(c.id, id);

        let sym = TypeSym::Class(id);
        if let Some(sym) = self.insert_type(c.name, sym) {
            report_type_shadow(self.vm, c.name, self.file_id.into(), c.pos, sym);
            return;
        }

        let level = self.vm.namespace_table(self.namespace_id);
        let mut level = level.write();
        match level.get_term(c.name) {
            None => {
                level.insert_term(c.name, TermSym::ClassConstructor(id));
            }
            Some(TermSym::Module(module_id)) => {
                level.insert_term(c.name, TermSym::ClassConstructorAndModule(id, module_id));
            }
            Some(sym) => report_term_shadow(self.vm, c.name, self.file_id.into(), c.pos, sym),
        }
    }

    fn visit_struct(&mut self, node: &Arc<ast::Struct>) {
        let id = {
            let mut structs = self.vm.structs.lock();
            let id: StructId = (structs.len() as u32).into();
            let struc = StructData {
                id,
                file_id: self.file_id.into(),
                ast: node.clone(),
                namespace_id: self.namespace_id,
                pos: node.pos,
                name: node.name,
                fields: Vec::new(),
                specializations: RwLock::new(HashMap::new()),
            };

            structs.push(Arc::new(RwLock::new(struc)));

            id
        };

        let sym = TypeSym::Struct(id);
        if let Some(sym) = self.insert_type(node.name, sym) {
            report_type_shadow(self.vm, node.name, self.file_id.into(), node.pos, sym);
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
            Some(sym) => report_term_shadow(self.vm, node.name, self.file_id.into(), node.pos, sym),
        }
    }

    fn visit_fct(&mut self, f: &Arc<ast::Function>) {
        let kind = if f.block.is_some() {
            FctKind::Source(RwLock::new(FctSrc::new()))
        } else {
            FctKind::Definition
        };

        let fct = Fct {
            id: FctId(0),
            file_id: self.file_id.into(),
            pos: f.pos,
            ast: f.clone(),
            name: f.name,
            namespace_id: self.namespace_id,
            param_types: Vec::new(),
            return_type: SourceType::Unit,
            parent: FctParent::None,
            has_override: f.has_override,
            has_open: f.has_open,
            has_final: f.has_final,
            has_optimize_immediately: f.has_optimize_immediately,
            is_pub: true,
            is_static: false,
            is_abstract: false,
            is_test: f.is_test,
            use_cannon: f.use_cannon,
            internal: f.internal,
            internal_resolved: false,
            overrides: None,
            is_constructor: false,
            vtable_index: None,
            initialized: false,
            impl_for: None,
            variadic_arguments: false,

            type_params: Vec::new(),
            kind,
            bytecode: None,
            intrinsic: None,
        };

        let fctid = self.vm.add_fct(fct);
        let sym = TermSym::Fct(fctid);

        if let Some(sym) = self.insert_term(f.name, sym) {
            report_term_shadow(self.vm, f.name, self.file_id.into(), f.pos, sym);
        }
    }

    fn visit_enum(&mut self, e: &Arc<ast::Enum>) {
        let id: EnumId = self.vm.enums.len().into();
        let mut xenum = EnumData {
            id,
            file_id: self.file_id.into(),
            namespace_id: self.namespace_id,
            ast: e.clone(),
            pos: e.pos,
            name: e.name,
            type_params: Vec::new(),
            variants: Vec::new(),
            name_to_value: HashMap::new(),
            extensions: Vec::new(),
            specializations: RwLock::new(HashMap::new()),
            simple_enumeration: false,
        };

        if let Some(ref type_params) = e.type_params {
            for param in type_params {
                xenum.type_params.push(TypeParam::new(param.name));
            }
        }

        self.vm.enums.push(RwLock::new(xenum));

        let sym = TypeSym::Enum(id);
        if let Some(sym) = self.insert_type(e.name, sym) {
            report_type_shadow(self.vm, e.name, self.file_id.into(), e.pos, sym);
        }
    }
}

impl<'x> GlobalDef<'x> {
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
