use ast;
use ast::visit::Visitor;
use class::*;
use ctxt::Context;
use semck;

pub fn check<'a, 'ast>(ctxt: &Context<'a, 'ast>) {
    let len = ctxt.classes.len();

    for i in 0..len {
        if let Some(ast) = ctxt.classes[i].ast {
            let mut cls = ClsDefCheck {
                ctxt: ctxt,
                ast: ast,
            };

            cls.check();
        }
    }
}

struct ClsDefCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    ast: &'ast ast::Class,
}

impl<'a, 'ast> ClsDefCheck<'a, 'ast> {
    fn check(&mut self) {
        self.visit_class(self.ast);
    }
}

impl<'a, 'ast> Visitor<'ast> for ClsDefCheck<'a, 'ast> {
    fn visit_class(&mut self, c: &'ast ast::Class) {
        for p in &c.params {
            self.visit_prop(p);
        }
    }

    fn visit_prop(&mut self, p: &'ast ast::Param) {
        semck::read_type(self.ctxt, &p.data_type);

        // TODO: store type in Class
    }
}
