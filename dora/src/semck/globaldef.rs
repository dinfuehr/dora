use parking_lot::{Mutex, RwLock};
use std::collections::HashMap;
use std::sync::Arc;

use crate::error::msg::SemError;
use crate::gc::Address;
use crate::sym::TermSym::{
    SymClassConstructor, SymClassConstructorAndModule, SymConst, SymFct, SymGlobal, SymModule,
    SymNamespace, SymStructConstructor, SymStructConstructorAndModule, SymVar,
};
use crate::sym::TypeSym::{SymClass, SymEnum, SymStruct, SymTrait};
use crate::sym::{SymTable, TermSym, TypeSym};
use crate::ty::SourceType;
use crate::vm::module::ModuleId;
use crate::vm::{
    class, module, ClassId, ConstData, ConstId, ConstValue, EnumData, EnumId, ExtensionData,
    ExtensionId, Fct, FctId, FctKind, FctParent, FctSrc, FileId, GlobalData, GlobalId, ImplData,
    ImplId, NamespaceData, NamespaceId, NodeMap, StructData, StructId, TraitData, TraitId,
    TypeParam, VM,
};
use dora_parser::ast::visit::*;
use dora_parser::ast::*;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

pub fn check<'ast>(
    vm: &mut VM<'ast>,
    map_cls_defs: &mut NodeMap<ClassId>,
    map_struct_defs: &mut NodeMap<StructId>,
    map_trait_defs: &mut NodeMap<TraitId>,
    map_impl_defs: &mut NodeMap<ImplId>,
    map_module_defs: &mut NodeMap<ModuleId>,
    map_global_defs: &mut NodeMap<GlobalId>,
    map_const_defs: &mut NodeMap<ConstId>,
    map_enum_defs: &mut NodeMap<EnumId>,
    map_extension_defs: &mut NodeMap<ExtensionId>,
    map_namespaces: &mut NodeMap<NamespaceId>,
) {
    let ast = vm.ast;
    let mut gdef = GlobalDef {
        vm,
        file_id: 0,
        namespace_id: None,
        map_cls_defs,
        map_struct_defs,
        map_trait_defs,
        map_impl_defs,
        map_module_defs,
        map_global_defs,
        map_const_defs,
        map_enum_defs,
        map_extension_defs,
        map_namespaces,
    };

    gdef.visit_ast(ast);
}

struct GlobalDef<'x, 'ast: 'x> {
    vm: &'x mut VM<'ast>,
    file_id: u32,
    namespace_id: Option<NamespaceId>,
    map_cls_defs: &'x mut NodeMap<ClassId>,
    map_struct_defs: &'x mut NodeMap<StructId>,
    map_trait_defs: &'x mut NodeMap<TraitId>,
    map_impl_defs: &'x mut NodeMap<ImplId>,
    map_module_defs: &'x mut NodeMap<ModuleId>,
    map_global_defs: &'x mut NodeMap<GlobalId>,
    map_const_defs: &'x mut NodeMap<ConstId>,
    map_enum_defs: &'x mut NodeMap<EnumId>,
    map_extension_defs: &'x mut NodeMap<ExtensionId>,
    map_namespaces: &'x mut NodeMap<NamespaceId>,
}

impl<'x, 'ast> Visitor<'ast> for GlobalDef<'x, 'ast> {
    fn visit_file(&mut self, f: &'ast File) {
        walk_file(self, f);
        self.file_id += 1;
    }

    fn visit_namespace(&mut self, n: &'ast Namespace) {
        let id: NamespaceId = self.vm.namespaces.len().into();
        let namespace = NamespaceData {
            id,
            file: self.file_id.into(),
            pos: n.pos,
            name: n.name,
            namespace_id: self.namespace_id,
            table: Arc::new(RwLock::new(SymTable::new())),
        };

        self.vm.namespaces.push(namespace);
        self.map_namespaces.insert(n.id, id);

        let sym = SymNamespace(id);
        if let Some(sym) = self.insert_term(n.name, sym) {
            report_term_shadow(self.vm, n.name, self.file_id.into(), n.pos, sym);
        }

        let saved_namespace_id = self.namespace_id;
        self.namespace_id = Some(id);
        walk_namespace(self, n);
        self.namespace_id = saved_namespace_id;
    }

    fn visit_trait(&mut self, t: &'ast Trait) {
        let id: TraitId = (self.vm.traits.len() as u32).into();
        let xtrait = TraitData {
            id,
            file: self.file_id.into(),
            namespace_id: self.namespace_id,
            pos: t.pos,
            name: t.name,
            methods: Vec::new(),
        };

        self.vm.traits.push(RwLock::new(xtrait));

        self.map_trait_defs.insert(t.id, id);

        let sym = SymTrait(id);
        if let Some(sym) = self.insert_type(t.name, sym) {
            report_type_shadow(self.vm, t.name, self.file_id.into(), t.pos, sym);
        }
    }

    fn visit_global(&mut self, g: &'ast Global) {
        let id = {
            let mut globals = self.vm.globals.lock();
            let id: GlobalId = (globals.len() as u32).into();
            let global = GlobalData {
                id,
                file: self.file_id.into(),
                namespace_id: self.namespace_id,
                pos: g.pos,
                name: g.name,
                ty: SourceType::Unit,
                reassignable: g.reassignable,
                initializer: None,
                address_init: Address::null(),
                address_value: Address::null(),
            };

            globals.push(Arc::new(RwLock::new(global)));

            id
        };

        self.map_global_defs.insert(g.id, id);

        let sym = SymGlobal(id);
        if let Some(sym) = self.insert_term(g.name, sym) {
            report_term_shadow(self.vm, g.name, self.file_id.into(), g.pos, sym);
        }
    }

    fn visit_impl(&mut self, i: &'ast Impl) {
        if i.trait_type.is_some() {
            let id: ImplId = (self.vm.impls.len() as u32).into();
            let ximpl = ImplData {
                id,
                file: self.file_id.into(),
                pos: i.pos,
                trait_id: None,
                class_ty: SourceType::Error,
                methods: Vec::new(),
            };
            self.vm.impls.push(RwLock::new(ximpl));
            self.map_impl_defs.insert(i.id, id);
        } else {
            let id: ExtensionId = self.vm.extensions.len().into();
            let mut extension_type_params = Vec::new();
            if let Some(ref type_params) = i.type_params {
                for param in type_params {
                    extension_type_params.push(TypeParam::new(param.name));
                }
            }
            let extension = ExtensionData {
                id,
                file: self.file_id.into(),
                pos: i.pos,
                type_params: extension_type_params,
                ty: SourceType::Error,
                methods: Vec::new(),
                instance_names: HashMap::new(),
                static_names: HashMap::new(),
            };
            self.vm.extensions.push(RwLock::new(extension));
            self.map_extension_defs.insert(i.id, id);
        }
    }

    fn visit_module(&mut self, m: &'ast Module) {
        let id = {
            let mut modules = self.vm.modules.lock();

            let id: ModuleId = modules.len().into();
            let module = module::Module {
                id: id,
                name: m.name,
                file: self.file_id.into(),
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

        let level = self.current_level();
        let mut level = level.write();
        match level.get_term(m.name) {
            None => {
                level.insert_term(m.name, SymModule(id));
            }
            Some(SymClassConstructor(class_id)) => {
                level.insert_term(m.name, SymClassConstructorAndModule(class_id, id));
            }
            Some(sym) => report_term_shadow(self.vm, m.name, self.file_id.into(), m.pos, sym),
        }
    }

    fn visit_const(&mut self, c: &'ast Const) {
        let id = {
            let mut consts = self.vm.consts.lock();
            let id: ConstId = consts.len().into();
            let xconst = ConstData {
                id,
                file: self.file_id.into(),
                pos: c.pos,
                name: c.name,
                ty: SourceType::Unit,
                expr: c.expr.clone(),
                value: ConstValue::None,
            };

            consts.push(Arc::new(Mutex::new(xconst)));

            id
        };

        self.map_const_defs.insert(c.id, id);

        let sym = SymConst(id);
        if let Some(sym) = self.insert_term(c.name, sym) {
            report_term_shadow(self.vm, c.name, self.file_id.into(), c.pos, sym);
        }
    }

    fn visit_class(&mut self, c: &'ast Class) {
        let id = {
            let mut classes = self.vm.classes.lock();

            let id: ClassId = classes.len().into();
            let mut cls = class::Class {
                id,
                name: c.name,
                file: self.file_id.into(),
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

        let sym = SymClass(id);
        if let Some(sym) = self.insert_type(c.name, sym) {
            report_type_shadow(self.vm, c.name, self.file_id.into(), c.pos, sym);
            return;
        }

        let level = self.current_level();
        let mut level = level.write();
        match level.get_term(c.name) {
            None => {
                level.insert_term(c.name, SymClassConstructor(id));
            }
            Some(SymModule(module_id)) => {
                level.insert_term(c.name, SymClassConstructorAndModule(id, module_id));
            }
            Some(sym) => report_term_shadow(self.vm, c.name, self.file_id.into(), c.pos, sym),
        }
    }

    fn visit_struct(&mut self, s: &'ast Struct) {
        let id = {
            let mut structs = self.vm.structs.lock();
            let id: StructId = (structs.len() as u32).into();
            let struc = StructData {
                id,
                file: self.file_id.into(),
                pos: s.pos,
                name: s.name,
                fields: Vec::new(),
                specializations: RwLock::new(HashMap::new()),
            };

            structs.push(Arc::new(Mutex::new(struc)));

            id
        };

        self.map_struct_defs.insert(s.id, id);

        let sym = SymStruct(id);
        if let Some(sym) = self.insert_type(s.name, sym) {
            report_type_shadow(self.vm, s.name, self.file_id.into(), s.pos, sym);
            return;
        }

        let level = self.current_level();
        let mut level = level.write();
        match level.get_term(s.name) {
            None => {
                level.insert_term(s.name, SymStructConstructor(id));
            }
            Some(SymModule(module_id)) => {
                level.insert_term(s.name, SymStructConstructorAndModule(id, module_id));
            }
            Some(sym) => report_term_shadow(self.vm, s.name, self.file_id.into(), s.pos, sym),
        }
    }

    fn visit_fct(&mut self, f: &'ast Function) {
        let kind = if f.block.is_some() {
            FctKind::Source(RwLock::new(FctSrc::new()))
        } else {
            FctKind::Definition
        };

        let fct = Fct {
            id: FctId(0),
            file: self.file_id.into(),
            pos: f.pos,
            ast: f,
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
        let sym = SymFct(fctid);

        if let Some(sym) = self.insert_term(f.name, sym) {
            report_term_shadow(self.vm, f.name, self.file_id.into(), f.pos, sym);
        }
    }

    fn visit_enum(&mut self, e: &'ast Enum) {
        let id: EnumId = self.vm.enums.len().into();
        let mut xenum = EnumData {
            id,
            file: self.file_id.into(),
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
        self.map_enum_defs.insert(e.id, id);

        let sym = SymEnum(id);
        if let Some(sym) = self.insert_type(e.name, sym) {
            report_type_shadow(self.vm, e.name, self.file_id.into(), e.pos, sym);
        }
    }
}

impl<'x, 'ast> GlobalDef<'x, 'ast> {
    fn current_level(&self) -> Arc<RwLock<SymTable>> {
        if let Some(namespace_id) = self.namespace_id {
            self.vm.namespaces[namespace_id.to_usize()].table.clone()
        } else {
            self.vm.global_namespace.clone()
        }
    }

    fn insert_type(&mut self, name: Name, sym: TypeSym) -> Option<TypeSym> {
        let level = self.current_level();
        let mut level = level.write();
        level.insert_type(name, sym)
    }

    fn insert_term(&mut self, name: Name, sym: TermSym) -> Option<TermSym> {
        let level = self.current_level();
        let mut level = level.write();
        level.insert_term(name, sym)
    }
}

pub fn report_type_shadow(vm: &VM, name: Name, file: FileId, pos: Position, sym: TypeSym) {
    let name = vm.interner.str(name).to_string();

    let msg = match sym {
        SymClass(_) => SemError::ShadowClass(name),
        SymStruct(_) => SemError::ShadowStruct(name),
        SymTrait(_) => SemError::ShadowTrait(name),
        SymEnum(_) => SemError::ShadowEnum(name),
        _ => unimplemented!(),
    };

    vm.diag.lock().report(file, pos, msg);
}

pub fn report_term_shadow(vm: &VM, name: Name, file: FileId, pos: Position, sym: TermSym) {
    let name = vm.interner.str(name).to_string();

    let msg = match sym {
        SymFct(_) => SemError::ShadowFunction(name),
        SymGlobal(_) => SemError::ShadowGlobal(name),
        SymConst(_) => SemError::ShadowConst(name),
        SymModule(_) => SemError::ShadowModule(name),
        SymVar(_) => SemError::ShadowParam(name),
        SymClassConstructor(_) | SymClassConstructorAndModule(_, _) => {
            SemError::ShadowClassConstructor(name)
        }
        SymStructConstructor(_) | SymStructConstructorAndModule(_, _) => {
            SemError::ShadowStructConstructor(name)
        }
        SymNamespace(_) => SemError::ShadowNamespace(name),
        x => unimplemented!("{:?}", x),
    };

    vm.diag.lock().report(file, pos, msg);
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
