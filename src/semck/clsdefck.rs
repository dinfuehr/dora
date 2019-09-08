use parking_lot::RwLock;
use std::collections::HashSet;

use crate::class::*;
use crate::ctxt::{Fct, FctId, FctKind, FctParent, FctSrc, NodeMap, VM};
use crate::semck;
use crate::sym::Sym;
use crate::ty::BuiltinType;

use dora_parser::ast::visit::{self, Visitor};
use dora_parser::ast::{self, Ast};
use dora_parser::error::msg::Msg;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

pub fn check<'ast>(vm: &mut VM<'ast>, ast: &'ast Ast, map_cls_defs: &NodeMap<ClassId>) {
    let mut clsck = ClsCheck {
        vm: vm,
        ast: ast,
        cls_id: None,
        map_cls_defs: map_cls_defs,
    };

    clsck.check();
}

struct ClsCheck<'x, 'ast: 'x> {
    vm: &'x mut VM<'ast>,
    ast: &'ast ast::Ast,
    map_cls_defs: &'x NodeMap<ClassId>,

    cls_id: Option<ClassId>,
}

impl<'x, 'ast> ClsCheck<'x, 'ast> {
    fn check(&mut self) {
        self.visit_ast(self.ast);
    }

    fn add_field(&mut self, pos: Position, name: Name, ty: BuiltinType, reassignable: bool) {
        let cls = self.vm.classes.idx(self.cls_id.unwrap());
        let mut cls = cls.write();

        for field in &cls.fields {
            if field.name == name {
                let name = self.vm.interner.str(name).to_string();
                report(self.vm, pos, Msg::ShadowField(name));
            }
        }

        let field = Field {
            id: cls.fields.len().into(),
            name: name,
            ty: ty,
            offset: 0,
            reassignable: reassignable,
        };

        cls.fields.push(field);
    }
}

impl<'x, 'ast> Visitor<'ast> for ClsCheck<'x, 'ast> {
    fn visit_class(&mut self, c: &'ast ast::Class) {
        self.cls_id = Some(*self.map_cls_defs.get(c.id).unwrap());

        self.vm.sym.lock().push_level();

        if let Some(ref type_params) = c.type_params {
            if type_params.len() > 0 {
                let mut names = HashSet::new();
                let mut type_param_id = 0;
                let cls = self.vm.classes.idx(self.cls_id.unwrap());
                let mut cls = cls.write();
                let mut params = Vec::new();

                for type_param in type_params {
                    if !names.insert(type_param.name) {
                        let name = self.vm.interner.str(type_param.name).to_string();
                        let msg = Msg::TypeParamNameNotUnique(name);
                        self.vm.diag.lock().report_without_path(type_param.pos, msg);
                    }

                    params.push(BuiltinType::ClassTypeParam(cls.id, type_param_id.into()));

                    for bound in &type_param.bounds {
                        let ty = semck::read_type(self.vm, bound);

                        match ty {
                            Some(BuiltinType::Class(cls_id, _)) => {
                                if let None = cls.type_params[type_param_id].class_bound {
                                    cls.type_params[type_param_id].class_bound = Some(cls_id);
                                } else {
                                    let msg = Msg::MultipleClassBounds;
                                    self.vm.diag.lock().report_without_path(type_param.pos, msg);
                                }
                            }

                            Some(BuiltinType::Trait(trait_id)) => {
                                if !cls.type_params[type_param_id].trait_bounds.insert(trait_id) {
                                    let msg = Msg::DuplicateTraitBound;
                                    self.vm.diag.lock().report_without_path(type_param.pos, msg);
                                }
                            }

                            None => {
                                // unknown type, error is already thrown
                            }

                            _ => {
                                let msg = Msg::BoundExpected;
                                self.vm.diag.lock().report_without_path(bound.pos(), msg);
                            }
                        }
                    }

                    let sym = Sym::SymClassTypeParam(cls.id, type_param_id.into());
                    self.vm.sym.lock().insert(type_param.name, sym);
                    type_param_id += 1;
                }

                let list_id = self.vm.lists.lock().insert(params.into());
                cls.ty = BuiltinType::Class(cls.id, list_id);
            } else {
                let msg = Msg::TypeParamsExpected;
                self.vm.diag.lock().report_without_path(c.pos, msg);
            }
        }

        visit::walk_class(self, c);

        if let Some(ref parent_class) = c.parent_class {
            let name = self.vm.interner.str(parent_class.name).to_string();
            let sym = self.vm.sym.lock().get(parent_class.name);

            match sym {
                Some(Sym::SymClass(clsid)) => {
                    let super_cls = self.vm.classes.idx(clsid);
                    let super_cls = super_cls.read();

                    if super_cls.has_open {
                        let cls = self.vm.classes.idx(self.cls_id.unwrap());
                        let mut cls = cls.write();
                        cls.parent_class = Some(clsid);
                    } else {
                        let msg = Msg::UnderivableType(name);
                        self.vm
                            .diag
                            .lock()
                            .report_without_path(parent_class.pos, msg);
                    }

                    let number_type_params = parent_class
                        .type_params
                        .as_ref()
                        .map(|x| x.len())
                        .unwrap_or(0);

                    if number_type_params != super_cls.type_params.len() {
                        let msg = Msg::WrongNumberTypeParams(
                            super_cls.type_params.len(),
                            number_type_params,
                        );
                        self.vm
                            .diag
                            .lock()
                            .report_without_path(parent_class.pos, msg);
                    }
                }

                _ => {
                    let msg = Msg::UnknownClass(name);
                    self.vm
                        .diag
                        .lock()
                        .report_without_path(parent_class.pos, msg);
                }
            };
        } else {
            let object_cls = self.vm.vips.object_class;
            let cls_id = self.cls_id.unwrap();

            if cls_id != object_cls {
                let cls = self.vm.classes.idx(cls_id);
                let mut cls = cls.write();
                cls.parent_class = Some(object_cls);
            }
        }

        self.cls_id = None;
        self.vm.sym.lock().pop_level();
    }

    fn visit_field(&mut self, f: &'ast ast::Field) {
        let ty = semck::read_type(self.vm, &f.data_type).unwrap_or(BuiltinType::Unit);
        self.add_field(f.pos, f.name, ty, f.reassignable);

        if !f.reassignable && !f.primary_ctor && f.expr.is_none() {
            self.vm
                .diag
                .lock()
                .report_without_path(f.pos, Msg::LetMissingInitialization);
        }
    }

    fn visit_ctor(&mut self, f: &'ast ast::Function) {
        let clsid = self.cls_id.unwrap();

        let kind = if f.block.is_some() {
            FctKind::Source(RwLock::new(FctSrc::new()))
        } else {
            FctKind::Definition
        };

        let fct = Fct {
            id: FctId(0),
            pos: f.pos,
            ast: f,
            name: f.name,
            param_types: Vec::new(),
            return_type: BuiltinType::Unit,
            parent: FctParent::Class(clsid),
            has_override: f.has_override,
            has_open: f.has_open,
            has_final: f.has_final,
            is_pub: true,
            is_static: false,
            is_abstract: false,
            internal: f.internal,
            internal_resolved: false,
            overrides: None,
            throws: f.throws,
            is_constructor: f.is_constructor,
            vtable_index: None,
            initialized: false,
            impl_for: None,

            type_params: Vec::new(),
            kind: kind,
        };

        let fctid = self.vm.add_fct(fct);

        let cls = self.vm.classes.idx(self.cls_id.unwrap());
        let mut cls = cls.write();
        cls.constructor = Some(fctid);
    }

    fn visit_method(&mut self, f: &'ast ast::Function) {
        if self.cls_id.is_none() {
            return;
        }

        let kind = if f.block.is_some() {
            FctKind::Source(RwLock::new(FctSrc::new()))
        } else {
            FctKind::Definition
        };

        let fct = Fct {
            id: FctId(0),
            ast: f,
            pos: f.pos,
            name: f.name,
            param_types: Vec::new(),
            return_type: BuiltinType::Unit,
            parent: FctParent::Class(self.cls_id.unwrap()),
            has_override: f.has_override,

            // abstract for methods also means that method is open to
            // override
            has_open: f.has_open || f.is_abstract,
            has_final: f.has_final,
            is_pub: f.is_pub,
            is_static: f.is_static,
            is_abstract: f.is_abstract,
            internal: f.internal,
            internal_resolved: false,
            overrides: None,
            throws: f.throws,
            is_constructor: false,
            vtable_index: None,
            initialized: false,
            impl_for: None,

            type_params: Vec::new(),
            kind: kind,
        };

        let fctid = self.vm.add_fct(fct);

        let cls = self.vm.classes.idx(self.cls_id.unwrap());
        let mut cls = cls.write();
        cls.methods.push(fctid);
    }
}

fn report(vm: &VM, pos: Position, msg: Msg) {
    vm.diag.lock().report_without_path(pos, msg);
}

#[cfg(test)]
mod tests {
    use crate::semck::tests::*;
    use dora_parser::error::msg::Msg;

    #[test]
    fn test_multiple_definition() {
        err(
            "class Foo class Foo",
            pos(1, 11),
            Msg::ShadowClass("Foo".into()),
        );
    }

    #[test]
    fn test_class_and_function() {
        err(
            "fun Foo() {} class Foo",
            pos(1, 14),
            Msg::ShadowFunction("Foo".into()),
        );
        err(
            "class Foo fun Foo() {}",
            pos(1, 11),
            Msg::ShadowClass("Foo".into()),
        );
    }

    #[test]
    fn test_class_definition() {
        ok("class Foo");
        ok("class Foo()");
        ok("class Foo(let a: Int)");
        ok("class Foo(let a: Int, let b:Int)");
        ok("class Foo(let a: Foo)");
        ok("class Foo(let a: Bar) class Bar");
        err(
            "class Foo(let a: Unknown)",
            pos(1, 18),
            Msg::UnknownType("Unknown".into()),
        );
        err(
            "class Foo(let a: Int, let a: Int)",
            pos(1, 27),
            Msg::ShadowField("a".to_string()),
        );
    }

    #[test]
    fn class_with_unknown_super_class() {
        err("class B : A {}", pos(1, 11), Msg::UnknownClass("A".into()));
        err(
            "open class B : A {}",
            pos(1, 16),
            Msg::UnknownClass("A".into()),
        );
        err(
            "class B : Int {}",
            pos(1, 11),
            Msg::UnderivableType("Int".into()),
        );
    }

    #[test]
    fn class_with_open_modifier() {
        ok("open class A {}");
        ok("open class A {} class B : A {}");
        err(
            "class A {} class B : A {}",
            pos(1, 22),
            Msg::UnderivableType("A".into()),
        );
    }

    #[test]
    fn non_field_ctor_arguments() {
        ok("class Foo(a: Int, b: Int)");
        ok("class Foo(let a: Int, b: Int)");
        ok("class Foo(a: Int, var b: Int)");
        err(
            "class Foo(a: Int, a: Int)",
            pos(1, 1),
            Msg::ShadowParam("a".into()),
        );
        err(
            "class Foo(a: Int, let a: Int)",
            pos(1, 1),
            Msg::ShadowParam("a".into()),
        );
        err(
            "class Foo(let a: Int, a: Int)",
            pos(1, 1),
            Msg::ShadowParam("a".into()),
        );
        err(
            "class Foo(a: Int) fun f(x: Foo) { x.a = 1; }",
            pos(1, 36),
            Msg::UnknownField("a".into(), "Foo".into()),
        );

        ok("class Foo(a: Int) fun foo() -> Foo { return Foo(1); } ");
    }

    #[test]
    fn field_defined_twice() {
        err(
            "class Foo { var a: Int; var a: Int; }",
            pos(1, 25),
            Msg::ShadowField("a".into()),
        );
        err(
            "class Foo(let a: Int) { var a: Int; }",
            pos(1, 25),
            Msg::ShadowField("a".into()),
        );
    }

    #[test]
    fn let_field_without_initialization() {
        err(
            "class Foo { let a: Int; }",
            pos(1, 13),
            Msg::LetMissingInitialization,
        );
    }

    #[test]
    fn field_self_assignment() {
        err(
            "class Foo(a: Int) { var b: Int = b; }",
            pos(1, 34),
            Msg::UnknownIdentifier("b".into()),
        );
    }

    #[test]
    fn test_generic_class() {
        ok("class A[T]");
        ok("class A[X, Y]");
        err(
            "class A[T, T]",
            pos(1, 12),
            Msg::TypeParamNameNotUnique("T".into()),
        );
        err("class A[]", pos(1, 1), Msg::TypeParamsExpected);
    }

    #[test]
    fn test_generic_argument() {
        ok("class A[T](val: T)");
        ok("class A[T](var val: T)");
        ok("class A[T](let val: T)");
    }

    #[test]
    fn test_generic_bound() {
        err(
            "class A[T: Foo]",
            pos(1, 12),
            Msg::UnknownType("Foo".into()),
        );
        ok("class Foo class A[T: Foo]");
        ok("trait Foo {} class A[T: Foo]");
    }

    #[test]
    fn test_generic_multiple_class_bounds() {
        err(
            "class Foo class Bar
            class A[T: Foo + Bar]",
            pos(2, 21),
            Msg::MultipleClassBounds,
        );
    }

    #[test]
    fn test_duplicate_trait_bound() {
        err(
            "trait Foo {}
            class A[T: Foo + Foo]",
            pos(2, 21),
            Msg::DuplicateTraitBound,
        );
    }

    #[test]
    fn test_super_class_with_superfluous_type_params() {
        err(
            "
            open class A
            class B: A[Int] {}",
            pos(3, 22),
            Msg::WrongNumberTypeParams(0, 1),
        );
    }
}
