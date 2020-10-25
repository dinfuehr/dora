use crate::vm::{Fct, FctSrc, VM};

use dora_parser::ast::visit::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;
use dora_parser::lexer::position::Position;

pub fn check<'ast>(vm: &VM<'ast>) {
    for fct in vm.fcts.iter() {
        let fct = fct.read();

        if !fct.is_src() {
            continue;
        }

        let src = fct.src();
        let mut src = src.write();
        let ast = fct.ast;

        let mut returnck = ReturnCheck {
            vm,
            fct: &fct,
            src: &mut src,
            ast,
        };

        returnck.check();
    }
}

struct ReturnCheck<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    fct: &'a Fct<'ast>,
    src: &'a mut FctSrc,
    ast: &'ast Function,
}

impl<'a, 'ast> ReturnCheck<'a, 'ast> {
    fn check(&mut self) {
        self.visit_fct(self.ast);
    }
}

impl<'a, 'ast> Visitor<'ast> for ReturnCheck<'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        let returns = expr_block_returns_value(f.block()).is_ok();

        if returns {
            // otherwise the function is always finished with a return statement
            // save this information for the function, this information is useful
            // for code generation

            self.src.always_returns = true;
        }
    }
}

pub fn returns_value(s: &Stmt) -> Result<(), Position> {
    match *s {
        StmtReturn(_) => Ok(()),
        StmtFor(ref stmt) => Err(stmt.pos),
        StmtWhile(ref stmt) => Err(stmt.pos),
        StmtBreak(ref stmt) => Err(stmt.pos),
        StmtContinue(ref stmt) => Err(stmt.pos),
        StmtLet(ref stmt) => Err(stmt.pos),
        StmtExpr(ref stmt) => expr_returns_value(&stmt.expr),
    }
}

pub fn expr_returns_value(e: &Expr) -> Result<(), Position> {
    match *e {
        Expr::ExprBlock(ref block) => expr_block_returns_value(block),
        Expr::ExprIf(ref expr) => expr_if_returns_value(expr),
        _ => Err(e.pos()),
    }
}

pub fn expr_block_returns_value(e: &ExprBlockType) -> Result<(), Position> {
    let mut pos = e.pos;

    for stmt in &e.stmts {
        match returns_value(stmt) {
            Ok(_) => return Ok(()),
            Err(err_pos) => pos = err_pos,
        }
    }

    if let Some(ref expr) = e.expr {
        expr_returns_value(expr)
    } else {
        Err(pos)
    }
}

fn expr_if_returns_value(e: &ExprIfType) -> Result<(), Position> {
    expr_returns_value(&e.then_block)?;

    match e.else_block {
        Some(ref block) => expr_returns_value(block),
        None => Err(e.pos),
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::SemError;
    use crate::semck::tests::*;
    use crate::test::parse;

    fn test_always_returns(code: &'static str, value: bool) {
        parse(code, |vm| {
            let name = vm.interner.intern("f");
            let fct_id = vm.global_namespace.read().get_fct(name).unwrap();

            let fct = vm.fcts.idx(fct_id);
            let fct = fct.read();
            let src = fct.src();
            let src = src.read();

            assert_eq!(value, src.always_returns);
        });
    }

    #[test]
    fn returns_unit() {
        ok("fun f() {}");
        ok("fun f() { if true { return; } }");
        ok("fun f() { while true { return; } }");
    }

    #[test]
    fn always_returns() {
        test_always_returns("fun f() {}", false);
        test_always_returns("fun f() { return; }", true);
        test_always_returns("fun f(): Int32 { return 1; }", true);
        test_always_returns(
            "fun f(): Int32 { if true { return 1; } else { return 2; } }",
            true,
        );
    }

    #[test]
    fn returns_int() {
        err(
            "fun f(): Int32 { }",
            pos(1, 16),
            SemError::ReturnType("Int32".into(), "()".into()),
        );
        err(
            "fun f(): Int32 { if true { return 1; } }",
            pos(1, 16),
            SemError::ReturnType("Int32".into(), "()".into()),
        );
        err(
            "fun f(): Int32 { if true { } else { return 1; } }",
            pos(1, 16),
            SemError::ReturnType("Int32".into(), "()".into()),
        );
        err(
            "fun f(): Int32 { while true { return 1; } }",
            pos(1, 16),
            SemError::ReturnType("Int32".into(), "()".into()),
        );
        ok("fun f(): Int32 { if true { return 1; } else { return 2; } }");
        ok("fun f(): Int32 { return 1; 1+2; }");
    }
}
