use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::once;
use std::iter::repeat;
use std::mem;
use std::rc::Rc;

use ast::{Ast, Expr, Stmt, Param};
use ast::{ExprAssignType, ExprIdentType, ExprBinType, ExprUnType};
use ast::{ExprLitStrType, ExprLitIntType, ExprLitBoolType};
use ast::{StmtVarType, StmtExprType, StmtIfType, StmtReturnType};
use ast::{StmtLoopType, StmtWhileType, StmtBlockType, Function};
use ast::Expr::*;
use ast::Stmt::*;
use ast::visit::*;

use ctxt::*;

use mir;
use mir::*;
use mir::Instr::*;
use mir::Opnd::*;

use ty::BuiltinType;

pub fn generate<'a, 'ast>(ctxt: &Context<'a, 'ast>, fct_id: FctId) {
    ctxt.fct_by_id_mut(fct_id, |fct| {
        Generator::new(ctxt, fct).generate();
    });
}

struct Generator<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    fct: &'a mut Fct<'ast>,
    ast: &'ast Function,
    vreg: u32,
    result: Opnd,
    cur_block: BlockId,
    ir: Mir,
    var_map: HashMap<VarId, VarId>
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum JoinAction {
    If(usize),
    While
}

impl<'a, 'ast> Generator<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>, fct: &'a mut Fct<'ast>) -> Generator<'a, 'ast> {
        let ast = fct.ast.unwrap();

        Generator {
            ctxt: ctxt,
            fct: fct,
            ast: ast,
            vreg: 0,
            result: OpndInt(0),
            cur_block: BlockId(0),
            ir: Mir::new(),
            var_map: HashMap::new(),
        }
    }

    fn generate(&mut self) {
        for p in &self.ast.params {
            self.visit_param(p);
        }

        self.cur_block = self.ir.add_block();
        self.visit_stmt(&self.ast.block);

        self.ensure_return();

        let ir = mem::replace(&mut self.ir, Mir::new());
        mir::dump::dump(self.ctxt, &ir);

        self.fct.ir = Some(ir);
    }

    fn add_stmt_var(&mut self, stmt: &'ast StmtVarType) {
        let ir_var_id = {
            let var = self.fct.var_by_node_id(stmt.id);
            let ir_var_id = self.ir.add_var(stmt.name, var.data_type);
            self.var_map.insert(var.id, ir_var_id);

            ir_var_id
        };

        if let Some(ref expr) = stmt.expr {
            self.visit_expr(expr);

            let src = self.result;
            self.add_instr_assign(OpndVar(ir_var_id), src);
        }
    }

    fn add_stmt_expr(&mut self, stmt: &'ast StmtExprType) {
        self.visit_expr(&stmt.expr);
    }

    fn add_stmt_if(&mut self, stmt: &'ast StmtIfType) {
        let cond_id = self.cur_block;
        let then_id = self.ir.add_block();
        let else_id = stmt.else_block.as_ref().map(|_| self.ir.add_block());
        let join_id = self.ir.add_block();

        // emit condition
        self.cur_block = cond_id;
        self.visit_expr(&stmt.cond);

        let false_block = else_id.unwrap_or(join_id);
        let result = self.result;
        self.add_instr(Instr::test(result, then_id, false_block));

        self.cur_block = then_id;
        self.visit_stmt(&stmt.then_block);
        self.add_instr(Instr::goto(join_id));

        let blk = self.block();
        blk.borrow_mut().add_predecessor(cond_id);

        // emit else block
        if let Some(ref else_block) = stmt.else_block {
            self.cur_block = else_id.unwrap();
            self.visit_stmt(else_block);
            self.add_instr(Instr::goto(join_id));

            let blk = self.block();
            blk.borrow_mut().add_predecessor(cond_id);
        }

        // set join block as current block
        self.cur_block = join_id;
    }

    fn add_stmt_loop(&mut self, stmt: &'ast StmtLoopType) {
        // TODO
    }

    fn add_stmt_while(&mut self, stmt: &'ast StmtWhileType) {
        let before_id = self.cur_block;
        let cond_id = self.ir.add_block();
        let body_id = self.ir.add_block();
        let after_id = self.ir.add_block();

        self.add_instr(Instr::goto(cond_id));

        self.cur_block = cond_id;
        self.visit_expr(&stmt.cond);
        let result = self.result;
        self.add_instr(Instr::test(result, body_id, after_id));

        self.cur_block = body_id;
        self.visit_stmt(&stmt.block);
        self.add_instr(Instr::goto(cond_id));

        self.cur_block = after_id;
    }

    fn add_stmt_block(&mut self, stmt: &'ast StmtBlockType) {
        for s in &stmt.stmts {
            self.visit_stmt(s);
        }
    }

    fn add_stmt_return(&mut self, stmt: &'ast StmtReturnType) {
        if let Some(ref expr) = stmt.expr {
            self.visit_expr(expr);

            let result = self.result;
            self.add_instr(Instr::ret_value(result));
        } else {
            self.add_instr(Instr::ret());
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
        let instr = Instr::str(dest, lit.value.clone());
        self.add_instr(instr);

        self.result = dest;
    }

    fn add_expr_un(&mut self, expr: &'ast ExprUnType) {
        self.visit_expr(&expr.opnd);
        let src = self.result;

        let dest = self.next_vreg();
        let instr = Instr::un(dest, expr.op, src);
        self.add_instr(instr);
        self.result = dest;
    }

    fn add_expr_bin(&mut self, expr: &'ast ExprBinType) {
        self.visit_expr(&expr.lhs);
        let lhs = self.result;

        self.visit_expr(&expr.rhs);
        let rhs = self.result;

        let dest = self.next_vreg();
        let instr = Instr::bin(dest, lhs, expr.op, rhs);
        self.add_instr(instr);
        self.result = dest;
    }

    fn add_expr_ident(&mut self, expr: &'ast ExprIdentType) {
        let var = self.fct.var_by_node_id(expr.id);
        let ir_var_id = *self.var_map.get(&var.id).unwrap();

        self.result = OpndVar(ir_var_id);
    }

    fn add_expr_assign(&mut self, expr: &'ast ExprAssignType) {
        let var_id = self.fct.var_by_node_id(expr.lhs.id()).id;
        let ir_var_id = *self.var_map.get(&var_id).unwrap();

        self.visit_expr(&expr.rhs);
        let src = self.result;

        self.add_instr_assign(OpndVar(ir_var_id), src);
    }

    fn ensure_return(&mut self) {
        let blk = self.block();

        if let Some(&InstrRet(_)) = blk.borrow().last_instr() {
            // already ends with ret: do nothing
            return;
        }

        if self.fct.return_type == BuiltinType::Unit {
            self.add_instr(Instr::ret());
        }
    }

    fn add_instr_assign(&self, dest: Opnd, src: Opnd) {
        self.add_instr(Instr::assign(dest, src));
    }

    fn add_instr(&self, instr: Instr) {
        let blk = self.block();
        blk.borrow_mut().add_instr(instr);
    }

    fn next_vreg(&mut self) -> Opnd {
        let vreg = self.vreg;

        self.vreg += 1;

        OpndReg(vreg)
    }

    fn block(&self) -> Rc<RefCell<Block>> {
        self.ir.block(self.cur_block)
    }
}

impl<'a, 'ast> Visitor<'ast> for Generator<'a, 'ast> {
    fn visit_param(&mut self, p: &'ast Param) {
        let var = self.fct.var_by_node_id(p.id);

        let ir_var_id = self.ir.add_var(p.name, var.data_type);
        self.var_map.insert(var.id, ir_var_id);
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtExpr(ref stmt) => self.add_stmt_expr(stmt),
            StmtIf(ref stmt) => self.add_stmt_if(stmt),
            StmtLoop(ref stmt) => self.add_stmt_loop(stmt),
            StmtWhile(ref stmt) => self.add_stmt_while(stmt),
            StmtBlock(ref stmt) => self.add_stmt_block(stmt),
            StmtVar(ref stmt) => self.add_stmt_var(stmt),
            StmtReturn(ref stmt) => self.add_stmt_return(stmt),
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
    use ast::{BinOp, CmpOp, UnOp};
    use ctxt::*;
    use mir;
    use mir::*;
    use mir::Instr::*;
    use mir::Opnd::*;
    use test::parse;

    fn check_fct<F, T>(code: &'static str, fname: &'static str, f: F) -> T
        where F: FnOnce(&Context, &Fct) -> T
    {
        parse(code, |ctxt| {
            let name = ctxt.interner.intern(fname);
            let fct_id = ctxt.sym.borrow().get_function(name).unwrap();

            mir::gen::generate(ctxt, fct_id);

            ctxt.fct_by_id(fct_id, |fct| f(ctxt, fct))
        })
    }

    #[test]
    fn assign() {
        check_fct("fn f() {
            var x = 1;
            x = x + 2;
            x = x + 3;
        }", "f", |ctxt, fct| {
            let ir_fct = fct.ir.as_ref().unwrap();
            assert_eq!(1, ir_fct.blocks.len());

            let x = VarId(0);

            assert_block(ir_fct, 0, vec![
                Instr::assign(OpndVar(x), OpndInt(1)),
                Instr::bin(OpndReg(0), OpndVar(x), BinOp::Add, OpndInt(2)),
                Instr::assign(OpndVar(x), OpndReg(0)),
                Instr::bin(OpndReg(1), OpndVar(x), BinOp::Add, OpndInt(3)),
                Instr::assign(OpndVar(x), OpndReg(1)),
                Instr::ret()
            ]);
        });
    }

    #[test]
    fn return_value() {
        check_fct("fn f() -> int {
            return 1;
        }", "f", |ctxt, fct| {
            let ir_fct = fct.ir.as_ref().unwrap();
            assert_eq!(1, ir_fct.blocks.len());

            assert_block(ir_fct, 0, vec![
                Instr::ret_value(OpndInt(1))
            ]);
        });
    }

    #[test]
    fn return_without_value() {
        check_fct("fn f() { return; }", "f", |ctxt, fct| {
            let ir_fct = fct.ir.as_ref().unwrap();
            assert_eq!(1, ir_fct.blocks.len());

            assert_block(ir_fct, 0, vec![Instr::ret()]);
        });
    }

    #[test]
    fn implicit_return() {
        check_fct("fn f() { }", "f", |ctxt, fct| {
            let ir_fct = fct.ir.as_ref().unwrap();
            assert_eq!(1, ir_fct.blocks.len());

            assert_block(ir_fct, 0, vec![Instr::ret()]);
        });
    }

    #[test]
    fn param() {
        check_fct("fn f(a: int) -> int {
            return a;
        }", "f", |ctxt, fct| {
            let ir_fct = fct.ir.as_ref().unwrap();
            assert_eq!(1, ir_fct.blocks.len());

            let param = OpndVar(VarId(0));
            assert_block(ir_fct, 0, vec![Instr::ret_value(param)]);
        });
    }

    #[test]
    fn expr() {
        check_fct("fn f() { 1; 2; 3; }", "f", |ctxt, fct| {
            let ir_fct = fct.ir.as_ref().unwrap();
            assert_eq!(1, ir_fct.blocks.len());

            assert_block(ir_fct, 0, vec![Instr::ret()]);
        });
    }

    #[test]
    fn bin() {
        check_fct("fn f(a: int, b: int) -> int {
            return a+b;
        }", "f", |ctxt, fct| {
            let ir_fct = fct.ir.as_ref().unwrap();
            assert_eq!(2, ir_fct.vars.len());
            assert_eq!(1, ir_fct.blocks.len());

            let a = OpndVar(VarId(0));
            let b = OpndVar(VarId(1));
            let temp = OpndReg(0);

            assert_block(ir_fct, 0, vec![
                Instr::bin(temp, a, BinOp::Add, b),
                Instr::ret_value(temp)
            ]);
        });
    }

    #[test]
    fn un() {
        check_fct("fn f(a: int) -> int {
            return -a;
        }", "f", |ctxt, fct| {
            let ir_fct = fct.ir.as_ref().unwrap();
            assert_eq!(1, ir_fct.vars.len());
            assert_eq!(1, ir_fct.blocks.len());

            assert_block(ir_fct, 0, vec![
                Instr::un(OpndReg(0), UnOp::Neg, OpndVar(VarId(0))),
                Instr::ret_value(OpndReg(0))
            ]);
        });
    }

    #[test]
    fn if_then() {
        check_fct("fn f(a: int, b: int, c: int) -> int {
            if (a = b) == 7 {
                a = 1;
                b = a + 1;
            } else {
                a = a + 1 + b;
                c = 2;
            }
            return a + b + c;
        }", "f", |ctxt, fct| {
            let ir_fct = fct.ir.as_ref().unwrap();

            assert_block(ir_fct, 0, vec![
                Instr::assign(OpndVar(VarId(0)), OpndVar(VarId(1))),
                Instr::bin(OpndReg(0), OpndVar(VarId(1)), BinOp::Cmp(CmpOp::Eq), OpndInt(7)),
                Instr::test(OpndReg(0), BlockId(1), BlockId(2))
            ]);

            assert_block(ir_fct, 1, vec![
                Instr::assign(OpndVar(VarId(0)), OpndInt(1)),
                Instr::bin(OpndReg(1), OpndVar(VarId(0)), BinOp::Add, OpndInt(1)),
                Instr::assign(OpndVar(VarId(1)), OpndReg(1)),
                Instr::goto(BlockId(3))
            ]);

            assert_block(ir_fct, 2, vec![
                Instr::bin(OpndReg(2), OpndVar(VarId(0)), BinOp::Add, OpndInt(1)),
                Instr::bin(OpndReg(3), OpndReg(2), BinOp::Add, OpndVar(VarId(1))),
                Instr::assign(OpndVar(VarId(0)), OpndReg(3)),
                Instr::assign(OpndVar(VarId(2)), OpndInt(2)),
                Instr::goto(BlockId(3))
            ]);

            assert_block(ir_fct, 3, vec![
                Instr::bin(OpndReg(4), OpndVar(VarId(0)), BinOp::Add, OpndVar(VarId(1))),
                Instr::bin(OpndReg(5), OpndReg(4), BinOp::Add, OpndVar(VarId(2))),
                Instr::ret_value(OpndReg(5))
            ]);
        })
    }

    #[test]
    fn while_loop() {
        check_fct("fn f(a: int, b: int) -> int {
            a = 1;
            while a < 10 {
                b = a + 1;
                a = a * 2;
            }
            return a + b;
        }", "f", |ctxt, fct| {
            let ir_fct = fct.ir.as_ref().unwrap();

            assert_block(ir_fct, 0, vec![
                Instr::assign(OpndVar(VarId(0)), OpndInt(1)),
                Instr::goto(BlockId(1))
            ]);

            // assert_block(ir_fct, 1, vec![
            //     Instr::phi(VarId(1), 2, vec![0, 1], 0),
            //     Instr::phi(VarId(0), 3, vec![1, 2], 0),
            //     Instr::bin(OpndReg(0), OpndVar(VarId(0), 1), BinOp::Cmp(CmpOp::Lt), OpndInt(10)),
            //     Instr::test(OpndReg(0), BlockId(2), BlockId(3))
            // ]);
            //
            // assert_block(ir_fct, 2, vec![
            //     Instr::bin(OpndReg(1), OpndVar(VarId(0), 3), BinOp::Add, OpndInt(1)),
            //     Instr::assign(OpndVar(VarId(1), 1), OpndReg(1)),
            //
            //     Instr::bin(OpndReg(2), OpndVar(VarId(0), 3), BinOp::Mul, OpndInt(2)),
            //     Instr::assign(OpndVar(VarId(0), 2), OpndReg(2)),
            //
            //     Instr::goto(BlockId(1))
            // ]);
            //
            // assert_block(ir_fct, 3, vec![
            //     Instr::bin(OpndReg(3), OpndVar(VarId(0), 3), BinOp::Add, OpndVar(VarId(1), 2)),
            //     Instr::ret_value(OpndReg(3))
            // ]);
        });
    }

    fn assert_block(fct: &Mir, block_id: usize, expected: Vec<Instr>) {
        let blk = fct.blocks[block_id].borrow();
        let instrs = &blk.instructions;

        assert_eq!(instrs.len(), expected.len());

        for (ind, instr) in instrs.iter().enumerate() {
            assert_eq!(*instr, expected[ind]);
        }
    }
}
