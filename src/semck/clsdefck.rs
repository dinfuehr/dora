use std::cell::RefCell;
use std::collections::HashSet;

use ast;
use ast::visit::{self, Visitor};
use class::*;
use ctxt::{Context, Fct, FctId, FctKind, FctParent, FctSrc, NodeMap};
use error::msg::Msg;
use interner::Name;
use lexer::position::Position;
use semck;
use sym::Sym;
use ty::BuiltinType;

pub fn check<'ast>(ctxt: &mut Context<'ast>, map_cls_defs: &NodeMap<ClassId>) {
    let mut clsck = ClsCheck {
        ctxt: ctxt,
        ast: ctxt.ast,
        cls_id: None,
        map_cls_defs: map_cls_defs,
    };

    clsck.check();
}

struct ClsCheck<'x, 'ast: 'x> {
    ctxt: &'x mut Context<'ast>,
    ast: &'ast ast::Ast,
    map_cls_defs: &'x NodeMap<ClassId>,

    cls_id: Option<ClassId>,
}

impl<'x, 'ast> ClsCheck<'x, 'ast> {
    fn check(&mut self) {
        self.visit_ast(self.ast);
    }

    fn add_field(&mut self, pos: Position, name: Name, ty: BuiltinType, reassignable: bool) {
        let mut cls = self.ctxt.classes[self.cls_id.unwrap()].borrow_mut();

        for field in &cls.fields {
            if field.name == name {
                let name = self.ctxt.interner.str(name).to_string();
                report(self.ctxt, pos, Msg::ShadowField(name));
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

        self.ctxt.sym.borrow_mut().push_level();

        if let Some(ref type_params) = c.type_params {
            if type_params.len() > 0 {
                let mut names = HashSet::new();
                let mut type_param_id = 0;

                for type_param in type_params {
                    if !names.insert(type_param.name) {
                        let name = self.ctxt.interner.str(type_param.name).to_string();
                        let msg = Msg::TypeParamNameNotUnique(name);
                        self.ctxt.diag.borrow_mut().report(type_param.pos, msg);
                    }

                    let mut cls = self.ctxt.classes[self.cls_id.unwrap()].borrow_mut();
                    cls.type_params.push(type_param.name);

                    let sym = Sym::SymTypeParam(type_param_id.into());
                    self.ctxt.sym.borrow_mut().insert(type_param.name, sym);
                    type_param_id += 1;
                }

            } else {
                let msg = Msg::TypeParamsExpected;
                self.ctxt.diag.borrow_mut().report(c.pos, msg);
            }
        }

        visit::walk_class(self, c);

        if let Some(ref parent_class) = c.parent_class {
            let name = self.ctxt.interner.str(parent_class.name).to_string();
            let sym = self.ctxt.sym.borrow().get(parent_class.name);

            match sym {
                Some(Sym::SymClass(clsid)) => {
                    let has_open = self.ctxt.classes[clsid].borrow().has_open;

                    if has_open {
                        let mut cls = self.ctxt.classes[self.cls_id.unwrap()].borrow_mut();
                        cls.parent_class = Some(clsid);
                    } else {
                        let msg = Msg::UnderivableType(name);
                        self.ctxt
                            .diag
                            .borrow_mut()
                            .report(parent_class.pos, msg);
                    }
                }

                _ => {
                    let msg = Msg::UnknownClass(name);
                    self.ctxt
                        .diag
                        .borrow_mut()
                        .report(parent_class.pos, msg);
                }
            };
        }

        self.cls_id = None;
        self.ctxt.sym.borrow_mut().pop_level();
    }

    fn visit_field(&mut self, f: &'ast ast::Field) {
        let ty = semck::read_type(self.ctxt, &f.data_type).unwrap_or(BuiltinType::Unit);
        self.add_field(f.pos, f.name, ty, f.reassignable);

        if !f.reassignable && !f.primary_ctor && f.expr.is_none() {
            self.ctxt
                .diag
                .borrow_mut()
                .report(f.pos, Msg::LetMissingInitialization);
        }
    }

    fn visit_ctor(&mut self, f: &'ast ast::Function) {
        let clsid = self.cls_id.unwrap();

        let kind = if f.block.is_some() {
            FctKind::Source(RefCell::new(FctSrc::new()))
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
            internal: f.internal,
            internal_resolved: false,
            overrides: None,
            throws: f.throws,
            ctor: f.ctor,
            ctor_allocates: false,
            vtable_index: None,
            initialized: false,
            kind: kind,
        };

        let fctid = self.ctxt.add_fct(fct);

        let mut cls = self.ctxt.classes[self.cls_id.unwrap()].borrow_mut();
        cls.ctors.push(fctid);
    }

    fn visit_method(&mut self, f: &'ast ast::Function) {
        if self.cls_id.is_none() {
            return;
        }

        let kind = if f.block.is_some() {
            FctKind::Source(RefCell::new(FctSrc::new()))
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
            has_open: f.has_open,
            has_final: f.has_final,
            is_pub: f.is_pub,
            is_static: f.is_static,
            internal: f.internal,
            internal_resolved: false,
            overrides: None,
            throws: f.throws,
            ctor: ast::CtorType::None,
            ctor_allocates: false,
            vtable_index: None,
            initialized: false,
            kind: kind,
        };

        let fctid = self.ctxt.add_fct(fct);

        let mut cls = self.ctxt.classes[self.cls_id.unwrap()].borrow_mut();
        cls.methods.push(fctid);
    }
}

fn report(ctxt: &Context, pos: Position, msg: Msg) {
    ctxt.diag.borrow_mut().report(pos, msg);
}

#[cfg(test)]
mod tests {
    use error::msg::Msg;
    use semck::tests::*;

    #[test]
    fn test_multiple_definition() {
        err("class Foo class Foo",
            pos(1, 11),
            Msg::ShadowClass("Foo".into()));
    }

    #[test]
    fn test_class_and_function() {
        err("fun Foo() {} class Foo",
            pos(1, 14),
            Msg::ShadowFunction("Foo".into()));
        err("class Foo fun Foo() {}",
            pos(1, 11),
            Msg::ShadowClass("Foo".into()));
    }

    #[test]
    fn test_class_definition() {
        ok("class Foo");
        ok("class Foo()");
        ok("class Foo(let a: int)");
        ok("class Foo(let a: int, let b:int)");
        ok("class Foo(let a: Foo)");
        ok("class Foo(let a: Bar) class Bar");
        err("class Foo(let a: Unknown)",
            pos(1, 18),
            Msg::UnknownType("Unknown".into()));
        err("class Foo(let a: int, let a: int)",
            pos(1, 27),
            Msg::ShadowField("a".to_string()));
    }

    #[test]
    fn class_with_unknown_super_class() {
        err("class B : A {}", pos(1, 11), Msg::UnknownClass("A".into()));
        err("open class B : A {}",
            pos(1, 16),
            Msg::UnknownClass("A".into()));
        err("class B : int {}",
            pos(1, 11),
            Msg::UnderivableType("int".into()));
    }

    #[test]
    fn class_with_open_modifier() {
        ok("open class A {}");
        ok("open class A {} class B : A {}");
        err("class A {} class B : A {}",
            pos(1, 22),
            Msg::UnderivableType("A".into()));
    }

    #[test]
    fn non_field_ctor_arguments() {
        ok("class Foo(a: int, b: int)");
        ok("class Foo(let a: int, b: int)");
        ok("class Foo(a: int, var b: int)");
        err("class Foo(a: int, a: int)",
            pos(1, 1),
            Msg::ShadowParam("a".into()));
        err("class Foo(a: int, let a: int)",
            pos(1, 1),
            Msg::ShadowParam("a".into()));
        err("class Foo(let a: int, a: int)",
            pos(1, 1),
            Msg::ShadowParam("a".into()));
        err("class Foo(a: int) fun f(x: Foo) { x.a = 1; }",
            pos(1, 36),
            Msg::UnknownField("a".into(), "Foo".into()));

        ok("class Foo(a: int) fun foo() -> Foo { return Foo(1); } ");
    }

    #[test]
    fn field_defined_twice() {
        err("class Foo { var a: int; var a: int; }",
            pos(1, 25),
            Msg::ShadowField("a".into()));
        err("class Foo(let a: int) { var a: int; }",
            pos(1, 25),
            Msg::ShadowField("a".into()));
    }

    #[test]
    fn let_field_without_initialization() {
        err("class Foo { let a: int; }",
            pos(1, 13),
            Msg::LetMissingInitialization);
    }

    #[test]
    fn field_self_assignment() {
        err("class Foo(a: int) { var b: int = b; }",
            pos(1, 34),
            Msg::UnknownIdentifier("b".into()));
    }

    #[test]
    fn test_generic_class() {
        ok("class A<T>");
        ok("class A<X, Y>");
        err("class A<T, T>",
            pos(1, 12),
            Msg::TypeParamNameNotUnique("T".into()));
    }

    #[test]
    fn test_generic_argument() {
        ok("class A<T>(val: T)");
        ok("class A<T>(var val: T)");
        ok("class A<T>(let val: T)");
    }
}
