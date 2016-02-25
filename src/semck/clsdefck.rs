use ast;
use ast::visit::{self, Visitor};
use class::*;
use ctxt::{Context, Fct, FctId, FctKind, FctSrc};
use error::msg::Msg;
use lexer::position::Position;
use mem;
use semck;
use ty::BuiltinType;

pub fn check<'ast>(ctxt: &mut Context<'ast>) {
    let mut clsck = ClsDefCheck {
        ctxt: ctxt,
        ast: ctxt.ast,
        cls_id: None,
    };

    clsck.check();
}

struct ClsDefCheck<'x, 'ast: 'x> {
    ctxt: &'x mut Context<'ast>,
    ast: &'ast ast::Ast,
    cls_id: Option<ClassId>,
}

impl<'x, 'ast> ClsDefCheck<'x, 'ast> {
    fn check(&mut self) {
        self.visit_ast(self.ast);
    }

    fn cls(&self) -> &Class<'ast> {
        self.ctxt.cls_by_id(self.cls_id.unwrap())
    }

    fn cls_mut(&mut self) -> &mut Class<'ast> {
        self.ctxt.cls_by_id_mut(self.cls_id.unwrap())
    }
}

impl<'x, 'ast> Visitor<'ast> for ClsDefCheck<'x, 'ast> {
    fn visit_class(&mut self, c: &'ast ast::Class) {
        self.cls_id = Some(*self.ctxt.cls_defs.get(&c.id).unwrap());

        visit::walk_class(self, c);

        self.cls_id = None;
    }

    fn visit_prop(&mut self, p: &'ast ast::Prop) {
        let ty = semck::read_type(self.ctxt, &p.data_type);

        for prop in &self.cls().props {
            if prop.name == p.name {
                let name = self.ctxt.interner.str(p.name).to_string();
                report(self.ctxt, p.pos, Msg::ShadowProp(name));
            }
        }

        let mut class = self.cls_mut();

        let offset = if ty.size() > 0 {
            mem::align_i32(class.size, ty.size())
        } else {
            class.size
        };

        let prop = Prop {
            name: p.name,
            ty: ty,
            offset: offset,
        };

        class.size = offset + ty.size();
        class.props.push(prop);
    }

    fn visit_ctor(&mut self, f: &'ast ast::Function) {
        let clsid = self.cls_id.unwrap();

        let fct = Fct {
            id: FctId(0),
            name: f.name,
            params_types: Vec::new(),
            return_type: BuiltinType::Unit,
            owner_class: Some(clsid),
            ctor: true,
            initialized: false,
            kind: FctKind::Source(FctSrc::new(f)),
        };

        let fctid = self.ctxt.add_fct(fct);
        self.cls_mut().ctor = fctid;
    }

    fn visit_method(&mut self, f: &'ast ast::Function) {
        let fct = Fct {
            id: FctId(0),
            name: f.name,
            params_types: Vec::new(),
            return_type: BuiltinType::Unit,
            owner_class: Some(self.cls_id.unwrap()),
            ctor: false,
            initialized: false,
            kind: FctKind::Source(FctSrc::new(f)),
        };

        let fctid = self.ctxt.add_fct(fct);

        self.cls_mut().methods.push(fctid);
    }
}

fn report(ctxt: &Context, pos: Position, msg: Msg) {
    ctxt.diag.borrow_mut().report(pos, msg);
}

#[cfg(test)]
mod tests {
    use error::msg::Msg;
    use interner::Name;
    use mem;
    use semck::tests::*;

    fn class_size(code: &'static str) -> i32 {
        ok_with_test(code, |ctxt| {
            ctxt.classes[0].size
        })
    }

    #[test]
    fn test_class_size() {
        assert_eq!(mem::ptr_width(), class_size("class Foo"));
        assert_eq!(mem::ptr_width() + 4, class_size("class Foo(a: int)"));
        assert_eq!(2 * mem::ptr_width(), class_size("class Foo(a: Str)"));
    }

    #[test]
    fn test_multiple_definition() {
        err("class Foo class Foo", pos(1, 11), Msg::ShadowType("Foo".into()));
    }

    #[test]
    fn test_class_and_function() {
        err("fn Foo() {} class Foo", pos(1, 13), Msg::ShadowFunction("Foo".into()));
        err("class Foo fn Foo() {}", pos(1, 11), Msg::ShadowType("Foo".into()));
    }

    #[test]
    fn test_class_definition() {
        ok("class Foo");
        ok("class Foo()");
        ok("class Foo(a: int)");
        ok("class Foo(a: int, b:int)");
        ok("class Foo(a: Foo)");
        ok("class Foo(a: Bar) class Bar");
        err("class Foo(a: Unknown)", pos(1, 14), Msg::UnknownType(Name(2)));
        err("class Foo(a: int, a: int)", pos(1, 19), Msg::ShadowProp("a".to_string()));
    }
}
