use ctxt::Context;

use ast::*;
use ast::Expr::*;
use ast::Stmt::*;
use ast::visit::*;

use ir::*;
use ir::Instr::*;
use ir::Opnd::*;

pub fn generate<'a, 'ast>(ctxt: &Context<'a, 'ast>, ast: &'ast Ast) {
    Builder::new(ctxt).visit_ast(ast);
}

struct Builder<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    vreg: u32,
    result: Opnd,
    block_id: BlockId,
    fct: Fct,
}

impl<'a, 'ast> Builder<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>) -> Builder<'a, 'ast> {
        Builder {
            ctxt: ctxt,
            vreg: 0,
            result: OpndInt(0),
            block_id: BlockId(0),
            fct: Fct::new(),
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

impl<'a, 'ast> Visitor<'ast> for Builder<'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        self.vreg = 0;
        self.block_id = self.fct.add_block();
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
            _ => panic!("unsupported statement")
        }
    }

    fn visit_expr(&mut self, e: &'ast Expr) {
        match *e {
            ExprLitInt(ref expr) => self.add_expr_lit_int(expr),
            ExprLitBool(ref expr) => self.add_expr_lit_bool(expr),
            ExprLitStr(ref expr) => self.add_expr_lit_str(expr),
            // ExprIdent(ref expr) => self.add_expr_ident(expr),
            ExprUn(ref expr) => self.add_expr_un(expr),
            ExprBin(ref expr) => self.add_expr_bin(expr),
            // ExprAssign(ref expr) => self.add_expr_assign(expr),
            _ => panic!("unsupported expression")
        }
    }
}
