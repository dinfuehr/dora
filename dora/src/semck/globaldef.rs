use parking_lot::{Mutex, RwLock};
use std::collections::HashMap;
use std::sync::Arc;

use crate::error::msg::SemError;
use crate::gc::Address;
use crate::sym::TermSym::{
    SymClassConstructor, SymClassConstructorAndModule, SymConst, SymFct, SymGlobal, SymModule,
    SymStructConstructor, SymStructConstructorAndModule, SymVar,
};
use crate::sym::TypeSym::{SymAnnotation, SymClass, SymEnum, SymStruct, SymTrait};
use crate::sym::{TermSym, TypeSym};
use crate::ty::BuiltinType;
use crate::vm::module::ModuleId;
use crate::vm::{
    annotation, class, module, AnnotationId, ClassId, ConstData, ConstId, ConstValue, EnumData,
    EnumId, ExtensionData, ExtensionId, Fct, FctKind, FctParent, FctSrc, FileId, GlobalData,
    GlobalId, ImplData, ImplId, NodeMap, StructData, StructId, TraitData, TraitId, TypeParam, VM,
};
use dora_parser::ast;
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
    map_annotation_defs: &mut NodeMap<AnnotationId>,
    map_global_defs: &mut NodeMap<GlobalId>,
    map_const_defs: &mut NodeMap<ConstId>,
    map_enum_defs: &mut NodeMap<EnumId>,
    map_extension_defs: &mut NodeMap<ExtensionId>,
) {
    let ast = vm.ast;
    let mut gdef = GlobalDef {
        vm,
        file_id: 0,
        map_cls_defs,
        map_struct_defs,
        map_trait_defs,
        map_impl_defs,
        map_module_defs,
        map_annotation_defs,
        map_global_defs,
        map_const_defs,
        map_enum_defs,
        map_extension_defs,
    };

    gdef.visit_ast(ast);
}

struct GlobalDef<'x, 'ast: 'x> {
    vm: &'x mut VM<'ast>,
    file_id: u32,
    map_cls_defs: &'x mut NodeMap<ClassId>,
    map_struct_defs: &'x mut NodeMap<StructId>,
    map_trait_defs: &'x mut NodeMap<TraitId>,
    map_impl_defs: &'x mut NodeMap<ImplId>,
    map_module_defs: &'x mut NodeMap<ModuleId>,
    map_annotation_defs: &'x mut NodeMap<AnnotationId>,
    map_global_defs: &'x mut NodeMap<GlobalId>,
    map_const_defs: &'x mut NodeMap<ConstId>,
    map_enum_defs: &'x mut NodeMap<EnumId>,
    map_extension_defs: &'x mut NodeMap<ExtensionId>,
}

impl<'x, 'ast> Visitor<'ast> for GlobalDef<'x, 'ast> {
    fn visit_file(&mut self, f: &'ast File) {
        walk_file(self, f);
        self.file_id += 1;
    }

    fn visit_trait(&mut self, t: &'ast Trait) {
        let id: TraitId = (self.vm.traits.len() as u32).into();
        let xtrait = TraitData {
            id,
            file: self.file_id.into(),
            pos: t.pos,
            name: t.name,
            methods: Vec::new(),
        };

        self.vm.traits.push(RwLock::new(xtrait));

        self.map_trait_defs.insert(t.id, id);

        let sym = SymTrait(id);
        if let Some(sym) = self.vm.sym.lock().insert_type(t.name, sym) {
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
                pos: g.pos,
                name: g.name,
                ty: BuiltinType::Unit,
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
        if let Some(sym) = self.vm.sym.lock().insert_term(g.name, sym) {
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
                class_ty: BuiltinType::Error,
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
                ty: BuiltinType::Error,
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

        let mut sym_table = self.vm.sym.lock();
        match sym_table.get_term(m.name) {
            None => {
                sym_table.insert_term(m.name, SymModule(id));
            }
            Some(SymClassConstructor(class_id)) => {
                sym_table.insert_term(m.name, SymClassConstructorAndModule(class_id, id));
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
                ty: BuiltinType::Unit,
                expr: c.expr.clone(),
                value: ConstValue::None,
            };

            consts.push(Arc::new(Mutex::new(xconst)));

            id
        };

        self.map_const_defs.insert(c.id, id);

        let sym = SymConst(id);
        if let Some(sym) = self.vm.sym.lock().insert_term(c.name, sym) {
            report_term_shadow(self.vm, c.name, self.file_id.into(), c.pos, sym);
        }
    }

    fn visit_class(&mut self, c: &'ast Class) {
        let id = {
            let mut classes = self.vm.classes.lock();

            let id: ClassId = classes.len().into();
            let cls = class::Class::new(self.vm, id, c, self.file_id.into(), c.has_constructor);

            classes.push(Arc::new(RwLock::new(cls)));

            id
        };

        self.map_cls_defs.insert(c.id, id);

        let sym = SymClass(id);
        if let Some(sym) = self.vm.sym.lock().insert_type(c.name, sym) {
            report_type_shadow(self.vm, c.name, self.file_id.into(), c.pos, sym);
            return;
        }

        let mut sym_table = self.vm.sym.lock();
        match sym_table.get_term(c.name) {
            None => {
                sym_table.insert_term(c.name, SymClassConstructor(id));
            }
            Some(SymModule(module_id)) => {
                sym_table.insert_term(c.name, SymClassConstructorAndModule(id, module_id));
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
        if let Some(sym) = self.vm.sym.lock().insert_type(s.name, sym) {
            report_type_shadow(self.vm, s.name, self.file_id.into(), s.pos, sym);
            return;
        }

        let mut sym_table = self.vm.sym.lock();
        match sym_table.get_term(s.name) {
            None => {
                sym_table.insert_term(s.name, SymStructConstructor(id));
            }
            Some(SymModule(module_id)) => {
                sym_table.insert_term(s.name, SymStructConstructorAndModule(id, module_id));
            }
            Some(sym) => report_term_shadow(self.vm, s.name, self.file_id.into(), s.pos, sym),
        }
    }

    fn visit_annotation(&mut self, a: &'ast Annotation) {
        if let Some(internal) = a.internal {
            let annotations = self.vm.annotations.lock();
            let annotation = annotations
                .iter()
                .find(|annotation| annotation.read().internal.contains(&internal))
                .expect("expected internal annotation to be present");
            let mut annotation = annotation.write();
            annotation.file = self.file_id.into();
            annotation.pos = a.pos;
            if let Some(ref type_params) = a.type_params {
                annotation.type_params = Some(convert_type_params(type_params));
            }
            return;
        }
        let id = {
            let mut annotations = self.vm.annotations.lock();
            let id: AnnotationId = annotations.len().into();

            let mut annotation = annotation::Annotation {
                id,
                name: a.name,
                file: self.file_id.into(),
                pos: a.pos,
                internal: a.internal,
                type_params: None,
                term_params: None,
                ty: BuiltinType::Error,
            };

            if let Some(ref type_params) = a.type_params {
                annotation.type_params = Some(convert_type_params(type_params));
            }

            annotations.push(Arc::new(RwLock::new(annotation)));

            id
        };

        let sym = SymAnnotation(id);

        self.map_annotation_defs.insert(a.id, id);

        if let Some(sym) = self.vm.sym.lock().insert_type(a.name, sym) {
            report_type_shadow(self.vm, a.name, self.file_id.into(), a.pos, sym);
        }
    }

    fn visit_fct(&mut self, f: &'ast Function) {
        let kind = if f.block.is_some() {
            FctKind::Source(RwLock::new(FctSrc::new()))
        } else {
            FctKind::Definition
        };

        let fct = Fct::new(
            self.vm,
            f,
            self.file_id.into(),
            kind,
            FctParent::None,
            false,
        );

        if let Err(sym) = self.vm.add_fct_to_sym(fct) {
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
        if let Some(sym) = self.vm.sym.lock().insert_type(e.name, sym) {
            report_type_shadow(self.vm, e.name, self.file_id.into(), e.pos, sym);
        }
    }
}

pub fn report_type_shadow(vm: &VM, name: Name, file: FileId, pos: Position, sym: TypeSym) {
    let name = vm.interner.str(name).to_string();

    let msg = match sym {
        SymClass(_) => SemError::ShadowClass(name),
        SymStruct(_) => SemError::ShadowStruct(name),
        SymTrait(_) => SemError::ShadowTrait(name),
        SymAnnotation(_) => SemError::ShadowAnnotation(name),
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
        x => unimplemented!("{:?}", x),
    };

    vm.diag.lock().report(file, pos, msg);
}

fn convert_type_params(type_params: &Vec<ast::TypeParam>) -> Vec<TypeParam> {
    let mut tps = Vec::new();
    for param in type_params {
        tps.push(TypeParam::new(param.name));
    }
    tps
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
    fn class_annotations() {
        ok_with_test(
            "@open @abstract class Foo {}
            class Bar {}",
            |vm| {
                let cls = vm.cls_by_name("Foo");
                let cls = vm.classes.idx(cls);
                let cls = cls.read();
                assert_eq!(true, cls.is_abstract);
                assert_eq!(true, cls.has_open);

                let cls = vm.cls_by_name("Bar");
                let cls = vm.classes.idx(cls);
                let cls = cls.read();
                assert_eq!(false, cls.is_abstract);
                assert_eq!(false, cls.has_open);
            },
        );
    }

    #[test]
    fn class_method_annotations() {
        ok_with_test(
            "@abstract @open class Bar {
              @abstract @open fun zero();
            }
            class Foo extends Bar {
              @final @cannon @optimizeImmediately @override fun zero() {}
              @static fun foo() {}
        }",
            |vm| {
                let met = vm.cls_method_by_name("Bar", "zero", false).unwrap();
                let met = vm.fcts.idx(met);
                let met = met.read();
                assert_eq!(true, met.has_open);
                assert_eq!(true, met.is_abstract);
                assert_eq!(false, met.has_final);
                assert_eq!(false, met.has_optimize_immediately);
                assert_eq!(false, met.has_override);
                assert_eq!(false, met.is_pub);
                assert_eq!(false, met.is_static);
                assert_eq!(false, met.is_test);
                assert_eq!(false, met.use_cannon);

                let met = vm.cls_method_by_name("Foo", "zero", false).unwrap();
                let met = vm.fcts.idx(met);
                let met = met.read();
                assert_eq!(true, met.has_final);
                assert_eq!(true, met.has_optimize_immediately);
                assert_eq!(true, met.has_override);
                assert_eq!(true, met.use_cannon);
                assert_eq!(false, met.has_open);
                assert_eq!(false, met.is_abstract);
                assert_eq!(false, met.is_pub);
                assert_eq!(false, met.is_static);
                assert_eq!(false, met.is_test);

                let met = vm.cls_method_by_name("Foo", "foo", true).unwrap();
                let met = vm.fcts.idx(met);
                let met = met.read();
                assert_eq!(true, met.is_static);
                assert_eq!(false, met.has_final);
                assert_eq!(false, met.has_optimize_immediately);
                assert_eq!(false, met.has_override);
                assert_eq!(false, met.has_open);
                assert_eq!(false, met.is_abstract);
                assert_eq!(false, met.is_pub);
                assert_eq!(false, met.is_test);
                assert_eq!(false, met.use_cannon);
            },
        );
    }
}
