use driver::ctxt::Context;
use error::msg::Msg;

use parser::ast::*;
use parser::ast::Expr::*;
use parser::ast::Stmt::*;
use parser::ast::Type::*;
use parser::ast::visit::Visitor;

use sym::*;
use sym::Sym::*;

pub fn check(ctxt: &Context, ast: &Ast) {
    DefCheck::new(ctxt).visit_ast(ast);

    if ctxt.diag.borrow().has_errors() { return; }

    TypeCheck::new(ctxt).visit_ast(ast);
}

struct DefCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>
}

impl<'a, 'ast> DefCheck<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>) -> DefCheck<'a, 'ast> {
        DefCheck {
            ctxt: ctxt
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for DefCheck<'a, 'ast> {
    fn visit_type(&mut self, t: &'ast Type) {
        match *t {
            TypeBasic(ref basic) => {
                if let Some(builtin) = self.ctxt.sym.borrow().get_type(basic.name) {
                    self.ctxt.types.borrow_mut().insert(basic.id, builtin);
                } else {
                    let tyname = self.ctxt.interner.str(basic.name).to_string();
                    let msg = Msg::UnknownType(tyname);
                    self.ctxt.diag.borrow_mut().report(basic.pos, msg);
                }
            }

            _ => self.ctxt.diag.borrow_mut().report_unimplemented(t.pos())
        }
    }
}

struct TypeCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    expr_type: BuiltinType
}

impl<'a, 'ast> TypeCheck<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>) -> TypeCheck<'a, 'ast> {
        TypeCheck {
            ctxt: ctxt,
            expr_type: BuiltinType::Unit
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for TypeCheck<'a, 'ast> {
    fn visit_expr(&mut self, e: &'ast Expr) {
        match *e {
            ExprLitInt(_) => self.expr_type = BuiltinType::Int,
            ExprLitStr(_) => self.expr_type = BuiltinType::Str,
            ExprLitBool(_) => self.expr_type = BuiltinType::Bool,

            // TODO: rest of possible expressions
            _ => visit::walk_expr(self, e)
        }
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtVar(ref var) => check_var_stmt(self, self.ctxt, var),

            // TODO: handle rest of statements
            _ => visit::walk_stmt(self, s)
        }
    }
}

fn check_var_stmt<'a, 'ast>(ck: &mut TypeCheck<'a, 'ast>, ctxt: &Context, s: &'ast StmtVarType) {
    let expr_type = s.expr.as_ref().map(|expr| {
        ck.visit_expr(&expr);

        ck.expr_type
    });

    let defined_type = if let Some(ref ty) = s.data_type {
        ctxt.types.borrow().get(&ty.id()).map(|bt| *bt)
    } else {
        expr_type
    };

    if defined_type.is_none() {
        let tyname = ctxt.interner.str(s.name).to_string();
        ctxt.diag.borrow_mut().report(s.pos, Msg::VarNeedsTypeInfo(tyname));

        return;
    }

    if expr_type.is_some() && (defined_type.unwrap() != expr_type.unwrap()) {
        let varname = ctxt.interner.str(s.name).to_string();
        let defname = defined_type.unwrap().to_string();
        let exprname = expr_type.unwrap().to_string();
        let msg = Msg::VarTypesIncompatible(varname, defname, exprname);

        ctxt.diag.borrow_mut().report(s.pos, msg);
    }
}

#[cfg(test)]
mod tests {
    use error::msg::Msg;
    use semck::tests::*;

    #[test]
    fn type_def_for_return_type() {
        ok("fn a() -> int {}");
        err("fn a() -> unknown {}", pos(1, 11), Msg::UnknownType("unknown".into()));
    }

    #[test]
    fn type_def_for_param() {
        ok("fn a(b: int) {}");
        err("fn a(b: foo) {}", pos(1, 9), Msg::UnknownType("foo".into()));
    }

    #[test]
    fn type_def_for_var() {
        ok("fn a() { var a : int = 1; }");
        err("fn a() { var a : test = 1; }", pos(1, 18), Msg::UnknownType("test".into()));
    }

    #[test]
    fn type_var_needs_expr_or_definition() {
        err("fn a() { var a; }", pos(1, 10), Msg::VarNeedsTypeInfo("a".into()));
    }

    #[test]
    fn type_var_wrong_type_defined() {
        ok("fn f() { var a : int = 1; }");
        ok("fn f() { var a : bool = false; }");
        ok("fn f() { var a : str = \"f\"; }");

        err("fn f() { var a : int = true; }",
            pos(1, 10), Msg::VarTypesIncompatible("a".into(), "int".into(), "bool".into()));
        err("fn f() { var b : bool = 2; }",
            pos(1, 10), Msg::VarTypesIncompatible("b".into(), "bool".into(), "int".into()));
    }
}
