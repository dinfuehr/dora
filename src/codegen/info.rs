use parser::ast::ctxt::Context;

use parser::ast::*;
use parser::ast::Expr::*;
use parser::ast::visit::*;

pub fn generate<'a, 'ast>(ctxt: &'ast Context<'a, 'ast>, fct: &'ast Function) {
    CodeGenInfo::new(ctxt, fct).generate();
}

struct CodeGenInfo<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    fct: &'ast Function,
}

impl<'a, 'ast> CodeGenInfo<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>, fct: &'ast Function) -> CodeGenInfo<'a, 'ast> {
        CodeGenInfo {
            ctxt: ctxt,
            fct: fct,
        }
    }

    fn generate(&mut self) {
        self.ctxt.function(self.fct.id, |fct| {
            fct.stacksize = 0;
            fct.contains_fct_invocation = false;
        });

        self.visit_stmt(&self.fct.block);
    }
}

impl<'a, 'ast> Visitor<'ast> for CodeGenInfo<'a, 'ast> {
    fn visit_expr(&mut self, e: &'ast Expr) {
        if let ExprCall(_) = *e {
            self.ctxt.function(self.fct.id, |fct| {
                fct.contains_fct_invocation = true;
            });
        }

        visit::walk_expr(self, e);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use parser::ast::ctxt::Context;
    use driver::cmd::Args;
    use error::msg::Msg;
    use parser::ast;
    use parser::Parser;
    use semck;

    pub fn parse<F>(code: &'static str, f: F)
            where F: for<'a, 'ast> FnOnce(&'ast Context<'a, 'ast>) -> () {
        let mut parser = Parser::from_str(code);
        let (ast, interner) = parser.parse().unwrap();
        let map = ast::map::build(&ast, &interner);
        let args : Args = Default::default();

        ast::dump::dump(&ast, &interner);

        let ctxt = Context::new(&args, &interner, &map, &ast);

        semck::check(&ctxt);
        assert!(!ctxt.diag.borrow().has_errors());

        f(&ctxt);
    }

    #[test]
    fn test_invocation_flag() {
        parse("fn f() { g(); } fn g() { }", |ctxt| {
            let fct1 = ctxt.ast.elements[0].to_function().unwrap();
            generate(ctxt, fct1);
            assert_eq!(true, ctxt.function(fct1.id, |fct| fct.contains_fct_invocation));

            let fct2 = ctxt.ast.elements[1].to_function().unwrap();
            generate(ctxt, fct2);
            assert_eq!(false, ctxt.function(fct2.id, |fct| fct.contains_fct_invocation));
        });
    }
}
