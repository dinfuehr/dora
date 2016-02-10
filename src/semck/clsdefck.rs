use ast;
use ast::visit::Visitor;
use class::*;
use ctxt::Context;
use error::msg::Msg;
use lexer::position::Position;
use mem;
use semck;

pub fn check<'a, 'ast>(ctxt: &mut Context<'a, 'ast>) {
    let mut clsck = ClsDefCheck {
        ctxt: ctxt,
        ast: ctxt.ast,
        cls_id: None,
    };

    clsck.check();
}

struct ClsDefCheck<'x, 'a : 'x, 'ast: 'a> {
    ctxt: &'x mut Context<'a, 'ast>,
    ast: &'ast ast::Ast,
    cls_id: Option<ClassId>,
}

impl<'x, 'a, 'ast> ClsDefCheck<'x, 'a, 'ast> {
    fn check(&mut self) {
        self.visit_ast(self.ast);
    }
}

impl<'x, 'a, 'ast> Visitor<'ast> for ClsDefCheck<'x, 'a, 'ast> {
    fn visit_class(&mut self, c: &'ast ast::Class) {
        self.cls_id = Some(*self.ctxt.cls_defs.get(&c.id).unwrap());

        for p in &c.params {
            self.visit_prop(p);
        }
    }

    fn visit_prop(&mut self, p: &'ast ast::Param) {
        let ty = semck::read_type(self.ctxt, &p.data_type);

        {
            let class = self.ctxt.cls_by_id(self.cls_id.unwrap());

            for prop in &class.props {
                if prop.name == p.name {
                    let name = self.ctxt.interner.str(p.name).to_string();
                    report(self.ctxt, p.pos, Msg::ShadowProp(name));
                }
            }
        }

        {
            let mut class = self.ctxt.cls_by_id_mut(self.cls_id.unwrap());

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
    }
}

fn report(ctxt: &Context, pos: Position, msg: Msg) {
    ctxt.diag.borrow_mut().report(pos, msg);
}

#[cfg(test)]
mod tests {
    use error::msg::Msg;
    use semck::tests::*;
    use ty::BuiltinType;

    #[test]
    fn type_prop() {
        ok("class Foo");
        ok("class Foo()");
        ok("class Foo(a: int)");
        ok("class Foo(a: int, b:int)");
        ok("class Foo(a: Foo)");
        ok("class Foo(a: Bar) class Bar");
        err("class Foo(a: Unknown)", pos(1, 14), Msg::UnknownType("Unknown".to_string()));
        err("class Foo(a: int, a: int)", pos(1, 19), Msg::ShadowProp("a".to_string()));
    }
}
