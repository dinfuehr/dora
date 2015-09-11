use parser::ast::ctxt::Context;

use parser::ast::*;
use parser::ast::Expr::*;
use parser::ast::visit::*;

pub fn generate<'a>(ctxt: &Context<'a, 'a>) {
    CodeGenInfo::new(ctxt).visit_ast(ctxt.ast);
}

struct CodeGenInfo<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    current_fct: Option<NodeId>,
}

impl<'a, 'ast> CodeGenInfo<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>) -> CodeGenInfo<'a, 'ast> {
        CodeGenInfo {
            ctxt: ctxt,
            current_fct: None,
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for CodeGenInfo<'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        self.current_fct = Some(f.id);

        self.ctxt.function(f.id, |fct| {
            fct.stacksize = 0;
            fct.contains_fct_invocation = false;
        });

        visit::walk_stmt(self, &f.block);
    }

    fn visit_expr(&mut self, e: &'ast Expr) {
        if let ExprCall(_) = *e {
            println!("update me");
            self.ctxt.function(self.current_fct.unwrap(), |fct| {
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

    pub fn parse<F>(code: &'static str, f: F) where F: FnOnce(&Context) -> () {
        let mut parser = Parser::from_str(code);
        let (ast, interner) = parser.parse().unwrap();
        let map = ast::map::build(&ast, &interner);
        let args : Args = Default::default();

        ast::dump::dump(&ast, &interner);

        let ctxt = Context::new(&args, &interner, &map, &ast);

        semck::check(&ctxt);
        assert!(!ctxt.diag.borrow().has_errors());

        generate(&ctxt);

        f(&ctxt);
    }

    #[test]
    fn test_invocation_flag() {
        parse("fn f() { g(); } fn g() { }", |ctxt| {
            let fct1 = ctxt.ast.elements[0].to_function().unwrap();
            let fct2 = ctxt.ast.elements[1].to_function().unwrap();

            assert_eq!(true, ctxt.function(fct1.id, |fct| fct.contains_fct_invocation));
            assert_eq!(false, ctxt.function(fct2.id, |fct| fct.contains_fct_invocation));
        });
    }
}
