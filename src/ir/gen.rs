use std::collections::HashMap;
use std::mem;

use ast::{Ast, Expr, Stmt, Param};
use ast::{ExprAssignType, ExprIdentType, ExprBinType, ExprUnType};
use ast::{ExprLitStrType, ExprLitIntType, ExprLitBoolType};
use ast::{StmtVarType, StmtExprType, StmtIfType};
use ast::{StmtLoopType, StmtWhileType, StmtBlockType, Function};
use ast::Expr::*;
use ast::Stmt::*;
use ast::visit::*;

use ctxt::*;

use ir;
use ir::*;
use ir::Instr::*;
use ir::Opnd::*;

pub fn generate<'a, 'ast>(ctxt: &Context<'a, 'ast>, ast: &'ast Ast) {
    Generator::new(ctxt).visit_ast(ast);
}

struct Generator<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    vreg: u32,
    result: Opnd,
    block_id: BlockId,
    fct: Fct,
    var_map: HashMap<VarInfoId, VarId>
}

impl<'a, 'ast> Generator<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>) -> Generator<'a, 'ast> {
        Generator {
            ctxt: ctxt,
            vreg: 0,
            result: OpndInt(0),
            block_id: BlockId(0),
            fct: Fct::new(),
            var_map: HashMap::new(),
        }
    }

    fn add_stmt_var(&mut self, stmt: &'ast StmtVarType) {
        self.ctxt.var(stmt.id, |ctxt_var, ctxt_var_id| {
            let ir_var_id = self.fct.add_var(stmt.name, ctxt_var.data_type, 0);
            self.var_map.insert(ctxt_var_id, ir_var_id);
        });

        if let Some(ref expr) = stmt.expr {
            self.visit_expr(expr);
        }
    }

    fn add_stmt_expr(&mut self, stmt: &'ast StmtExprType) {
        self.visit_expr(&stmt.expr);
    }

    fn add_stmt_if(&mut self, stmt: &'ast StmtIfType) {
        let before_id = self.block_id;
        let then_id = self.fct.add_block();
        let after_id = self.fct.add_block();
        let else_id = if stmt.else_block.is_some() {
            self.fct.add_block()
        } else {
            after_id
        };

        self.visit_expr(&stmt.cond);
        let result = self.result;
        self.add_instr(InstrTest(result));
        self.block_mut().add_successor(then_id);
        self.block_mut().add_successor(else_id);

        self.block_id = then_id;
        self.visit_stmt(&stmt.then_block);
        self.add_instr(InstrGoto(after_id));
        self.block_mut().add_predecessor(before_id);
        self.block_mut().add_successor(after_id);

        if let Some(ref else_block) = stmt.else_block {
            self.block_id = else_id;
            self.visit_stmt(else_block);
            self.add_instr(InstrGoto(after_id));
            self.block_mut().add_predecessor(before_id);
            self.block_mut().add_successor(after_id);
        }

        self.block_id = after_id;
    }

    fn add_stmt_loop(&mut self, stmt: &'ast StmtLoopType) {
        let before_id = self.block_id;
        let loop_id = self.fct.add_block();
        let after_id = self.fct.add_block();

        self.add_instr(InstrGoto(loop_id));
        self.block_mut().add_successor(loop_id);

        self.block_id = loop_id;
        self.visit_stmt(&stmt.block);
        self.add_instr(InstrGoto(loop_id));
        self.block_mut().add_predecessor(before_id);
        self.block_mut().add_successor(loop_id);
        self.block_mut().add_successor(after_id);

        self.block_id = after_id;
    }

    fn add_stmt_while(&mut self, stmt: &'ast StmtWhileType) {

    }

    fn add_stmt_block(&mut self, stmt: &'ast StmtBlockType) {
        for s in &stmt.stmts {
            self.visit_stmt(s);
        }
    }

    fn add_expr_lit_int(&mut self, lit: &'ast ExprLitIntType) {
        self.result = OpndInt(lit.value);
    }

    fn add_expr_lit_bool(&mut self, lit: &'ast ExprLitBoolType) {
        self.result = OpndBool(lit.value);
    }

    fn add_expr_lit_str(&mut self, lit: &'ast ExprLitStrType) {
        let dest = self.next_vreg();
        let instr = InstrStr(dest, lit.value.clone());
        self.add_instr(instr);

        self.result = dest;
    }

    fn add_expr_un(&mut self, expr: &'ast ExprUnType) {
        self.visit_expr(&expr.opnd);
        let src = self.result;

        let dest = self.next_vreg();
        let instr = InstrUn(dest, expr.op, src);
        self.add_instr(instr);
    }

    fn add_expr_bin(&mut self, expr: &'ast ExprBinType) {
        self.visit_expr(&expr.lhs);
        let lhs = self.result;

        self.visit_expr(&expr.rhs);
        let rhs = self.result;

        let dest = self.next_vreg();
        let instr = InstrBin(dest, lhs, expr.op, rhs);
        self.add_instr(instr);
    }

    fn add_expr_ident(&mut self, expr: &'ast ExprIdentType) {
        // self.
    }

    fn add_expr_assign(&mut self, expr: &'ast ExprAssignType) {
        self.visit_expr(&expr.lhs);
        let dest = self.result;

        self.visit_expr(&expr.rhs);
        let src = self.result;

        let instr = InstrAssign(dest, src);
        self.add_instr(instr);
    }

    fn add_instr_assign(&mut self, dest: Opnd, src: Opnd) {
        self.add_instr(InstrAssign(dest, src));
    }

    fn add_instr(&mut self, instr: Instr) {
        self.block_mut().add_instr(instr);
    }

    fn block_mut(&mut self) -> &mut Block {
        self.fct.block_mut(self.block_id)
    }

    fn next_vreg(&mut self) -> Opnd {
        let vreg = self.vreg;

        self.vreg += 1;

        OpndReg(vreg)
    }
}

impl<'a, 'ast> Visitor<'ast> for Generator<'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        self.vreg = 0;
        self.var_map.clear();

        self.block_id = self.fct.add_block();
        self.visit_stmt(&f.block);

        let ir = mem::replace(&mut self.fct, Fct::new());
        ir::dump::dump(self.ctxt, &ir);

        self.ctxt.function(f.id, |fct| fct.ir = Some(ir));
    }

    fn visit_param(&mut self, p: &'ast Param) {
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtExpr(ref stmt) => self.add_stmt_expr(stmt),
            StmtIf(ref stmt) => self.add_stmt_if(stmt),
            StmtLoop(ref stmt) => self.add_stmt_loop(stmt),
            StmtWhile(ref stmt) => self.add_stmt_while(stmt),
            StmtBlock(ref stmt) => self.add_stmt_block(stmt),
            StmtVar(ref stmt) => self.add_stmt_var(stmt),
            _ => panic!("unsupported statement")
        }
    }

    fn visit_expr(&mut self, e: &'ast Expr) {
        match *e {
            ExprLitInt(ref expr) => self.add_expr_lit_int(expr),
            ExprLitBool(ref expr) => self.add_expr_lit_bool(expr),
            ExprLitStr(ref expr) => self.add_expr_lit_str(expr),
            ExprIdent(ref expr) => self.add_expr_ident(expr),
            ExprUn(ref expr) => self.add_expr_un(expr),
            ExprBin(ref expr) => self.add_expr_bin(expr),
            ExprAssign(ref expr) => self.add_expr_assign(expr),
            _ => panic!("unsupported expression")
        }
    }
}

#[cfg(test)]
mod tests {
    use ctxt::*;
    use ir;
    use ir::Fct;
    use test::parse;

    fn check_fct<F, T>(code: &'static str, fname: &'static str, f: F) -> T where F: FnOnce(&Context, &FctInfo) -> T {
        parse(code, |ctxt| {
            ir::gen::generate(ctxt, ctxt.ast);

            let name = ctxt.interner.intern(fname);
            let fct_id = ctxt.sym.borrow().get_function(name).unwrap();

            let fct_infos = ctxt.fct_infos.borrow();
            let fct = &fct_infos[fct_id.0];

            f(ctxt, fct)
        })
    }

    #[test]
    fn assign() {
        check_fct("fn f() { var x = 1; x = x + 2; x = x + 3; }", "f", |ctxt, fct| {
            // FIXME: does not compile
            let ir_fct = fct.ir.as_ref();
            let ir_fct = ir_fct.unwrap();

            ir::dump::dump(ctxt, ir_fct);
        });
    }
}
