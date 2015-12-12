use std::collections::HashMap;
use std::iter::once;
use std::iter::repeat;
use std::mem;

use ast::{Ast, Expr, Stmt, Param};
use ast::{ExprAssignType, ExprIdentType, ExprBinType, ExprUnType};
use ast::{ExprLitStrType, ExprLitIntType, ExprLitBoolType};
use ast::{StmtVarType, StmtExprType, StmtIfType, StmtReturnType};
use ast::{StmtLoopType, StmtWhileType, StmtBlockType, Function};
use ast::Expr::*;
use ast::Stmt::*;
use ast::visit::*;

use ctxt::*;

use ir;
use ir::*;
use ir::Instr::*;
use ir::Opnd::*;

use sym::BuiltinType;

pub fn generate<'a, 'ast>(ctxt: &Context<'a, 'ast>, fct: &'ast Function) {
    Generator::new(ctxt, fct).generate();
}

struct Generator<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    vreg: u32,
    result: Opnd,
    cur_block: BlockId,
    cur_join_action: JoinAction,
    cur_join: Option<BlockId>,
    ast_fct: &'ast Function,
    ir: Fct,
    var_map: HashMap<VarInfoId, VarId>
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum JoinAction {
    If(usize),
    While
}

impl<'a, 'ast> Generator<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>, fct: &'ast Function) -> Generator<'a, 'ast> {
        Generator {
            ctxt: ctxt,
            vreg: 0,
            result: OpndInt(0),
            cur_block: BlockId(0),
            cur_join_action: JoinAction::If(0),
            cur_join: None,
            ast_fct: fct,
            ir: Fct::new(),
            var_map: HashMap::new(),
        }
    }

    fn generate(&mut self) {
        for p in &self.ast_fct.params {
            self.visit_param(p);
        }

        self.cur_block = self.ir.add_block();
        self.visit_stmt(&self.ast_fct.block);

        self.ensure_return();

        let ir = mem::replace(&mut self.ir, Fct::new());
        ir::dump::dump(self.ctxt, &ir);

        self.ctxt.fct_info_mut(self.ast_fct.id, |fct| fct.ir = Some(ir));
    }

    fn add_stmt_var(&mut self, stmt: &'ast StmtVarType) {
        let var_id = self.ctxt.var(stmt.id, |ctxt_var, ctxt_var_id| {
            let ir_var_id = self.ir.add_var(stmt.name, ctxt_var.data_type);
            self.var_map.insert(ctxt_var_id, ir_var_id);

            ir_var_id
        });

        if let Some(ref expr) = stmt.expr {
            self.visit_expr(expr);

            let src = self.result;
            self.add_instr_assign_var(var_id, src);
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

        // emit then block
        self.use_join_node(join_id, JoinAction::If(1), |gen| {
            gen.cur_block = then_id;
            gen.visit_stmt(&stmt.then_block);
            gen.add_instr(Instr::goto(join_id));
            gen.block_mut().add_predecessor(cond_id);
        });

        // emit else block
        if let Some(ref else_block) = stmt.else_block {
            self.use_join_node(join_id, JoinAction::If(2), |gen| {
                gen.reset_vars_from_phi_backup();
                gen.ensure_phi_opnds_for_else();

                gen.cur_block = else_id.unwrap();
                gen.visit_stmt(else_block);
                gen.add_instr(Instr::goto(join_id));
                gen.block_mut().add_predecessor(cond_id);
            });
        }

        // set join block as current block
        self.cur_block = join_id;
        self.reset_vars_from_phi_dest();
    }

    fn reset_vars_from_phi_backup(&mut self) {
        let resets : Vec<_> = self.join().phi_iter().
            map(|phi| (phi.var_id, phi.backup)).collect();

        for reset in resets {
            self.ir.var_mut(reset.0).cur_ssa = reset.1;
        }
    }

    fn reset_vars_from_phi_dest(&mut self) {
        let resets : Vec<_> = self.block().phi_iter().
            map(|phi| (phi.var_id, phi.dest)).collect();

        for reset in resets {
            self.ir.var_mut(reset.0).cur_ssa = reset.1;
        }
    }

    fn ensure_phi_opnds_for_else(&mut self) {
        for phi in self.join_mut().phi_iter_mut() {
            while phi.opnds.len() < 2 {
                phi.opnds.push(phi.backup);
            }
        }
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

        self.use_join_node(cond_id, JoinAction::While, |gen| {
            gen.cur_block = cond_id;
            gen.visit_expr(&stmt.cond);
            let result = gen.result;
            gen.add_instr(Instr::test(result, body_id, after_id));

            gen.cur_block = body_id;
            gen.visit_stmt(&stmt.block);
            gen.add_instr(Instr::goto(cond_id));
        });

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
        let var_id = self.ctxt.var(expr.id, |_, ctxt_var_id| {
            *self.var_map.get(&ctxt_var_id).unwrap()
        });

        let cur_ssa = self.ir.var(var_id).cur_ssa;
        self.result = OpndVar(var_id, cur_ssa);
    }

    fn add_expr_assign(&mut self, expr: &'ast ExprAssignType) {
        let var_id = self.ctxt.var(expr.lhs.id(), |_, ctxt_var_id| {
            *self.var_map.get(&ctxt_var_id).unwrap()
        });

        self.visit_expr(&expr.rhs);
        let src = self.result;

        self.add_instr_assign_var(var_id, src);
    }

    fn ensure_return(&mut self) {
        if let Some(&InstrRet(_)) = self.block().last_instr() {
            // already ends with ret: do nothing
        } else if self.ctxt.fct_info(self.ast_fct.id,
                |fct| fct.return_type) == BuiltinType::Unit {
            self.add_instr(Instr::ret());
        }
    }

    fn add_instr_assign_var(&mut self, dest: VarId, src: Opnd) {
        let dest_ssa = self.ir.increase_var(dest);
        self.ir.var_mut(dest).cur_ssa = dest_ssa;

        self.add_instr_assign(OpndVar(dest, dest_ssa), src);
        self.phi_for_assign(dest, dest_ssa);
    }

    fn phi_for_assign(&mut self, dest: VarId, dest_ssa: u32) {
        let old_ssa = dest_ssa - 1;

        // if current join node exists
        if let Some(join_id) = self.cur_join {
            match self.cur_join_action {
                JoinAction::If(join_idx) =>
                    self.phi_for_assign_in_if(dest, dest_ssa, old_ssa, join_id, join_idx),
                JoinAction::While => {
                    self.phi_for_assign_in_while(dest, dest_ssa, old_ssa, join_id);
                    self.replace_var_usages(dest, old_ssa, dest_ssa);
                }
            }
        }
    }

    fn phi_for_assign_in_if(&mut self, dest: VarId, dest_ssa: u32, old_ssa: u32,
        join_id: BlockId, join_idx: usize)
    {
        // find existing phi instruction in join node
        if let Some(phi) = self.join_mut().find_phi_mut(dest) {
            while phi.opnds.len() < join_idx {
                phi.opnds.push(old_ssa);
            }

            phi.opnds[join_idx - 1] = dest_ssa;
            return;
        }

        // otherwise add phi instruction into join node
        let join_ssa = self.ir.increase_var(dest);

        let opnds: Vec<u32> =
            repeat(old_ssa).take(join_idx - 1).
            chain(once(dest_ssa)).collect();

        let phi = InstrPhiType {
            var_id: dest,
            dest: join_ssa,
            opnds: opnds,
            backup: old_ssa,
        };

        self.join_mut().add_phi(phi);
    }

    fn phi_for_assign_in_while(&mut self, dest: VarId, dest_ssa: u32, old_ssa: u32,
        join_id: BlockId)
    {
        if let Some(phi) = self.join_mut().find_phi_mut(dest) {
            phi.opnds[1] = dest_ssa;
            return;
        }

        let join_ssa = self.ir.increase_var(dest);

        let phi = InstrPhiType {
            var_id: dest,
            dest: join_ssa,
            opnds: vec![old_ssa, dest_ssa],
            backup: 0
        };

        self.join_mut().add_phi(phi);
    }

    fn replace_var_usages(&mut self, var: VarId, old_ssa: u32, new_ssa: u32) {
        // TODO
    }

    fn add_instr_assign(&mut self, dest: Opnd, src: Opnd) {
        self.add_instr(Instr::assign(dest, src));
    }

    fn add_instr(&mut self, instr: Instr) {
        self.block_mut().add_instr(instr);
    }

    fn use_join_node<F>(&mut self, id: BlockId, action: JoinAction, f: F)
        where F: FnOnce(&mut Generator<'a, 'ast>)
    {
        let old_join = self.cur_join;
        let old_join_action = self.cur_join_action;
        self.cur_join = Some(id);
        self.cur_join_action = action;

        f(self);

        self.cur_join = old_join;
        self.cur_join_action = old_join_action;
    }

    fn join_mut(&mut self) -> &mut Block {
        self.ir.block_mut(self.cur_join.unwrap())
    }

    fn join(&self) -> &Block {
        self.ir.block(self.cur_join.unwrap())
    }

    fn block_mut(&mut self) -> &mut Block {
        self.ir.block_mut(self.cur_block)
    }

    fn block(&self) -> &Block {
        self.ir.block(self.cur_block)
    }

    fn next_vreg(&mut self) -> Opnd {
        let vreg = self.vreg;

        self.vreg += 1;

        OpndReg(vreg)
    }
}

impl<'a, 'ast> Visitor<'ast> for Generator<'a, 'ast> {
    fn visit_param(&mut self, p: &'ast Param) {
        self.ctxt.var(p.id, |ctxt_var, ctxt_var_id| {
            let ir_var_id = self.ir.add_var(p.name, ctxt_var.data_type);
            self.var_map.insert(ctxt_var_id, ir_var_id);
        });
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
    use ir;
    use ir::*;
    use ir::Instr::*;
    use ir::Opnd::*;
    use test::parse;

    fn check_fct<F, T>(code: &'static str, fname: &'static str, f: F) -> T where F: FnOnce(&Context, &FctInfo) -> T {
        parse(code, |ctxt| {
            let name = ctxt.interner.intern(fname);
            let fct_info_id = ctxt.sym.borrow().get_function(name).unwrap();

            let fct = ctxt.fct_info_for_id(fct_info_id, |fct_info| fct_info.ast.unwrap());
            ir::gen::generate(ctxt, fct);

            ctxt.fct_info_for_id(fct_info_id, |fct_info| f(ctxt, fct_info))
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
                Instr::assign(OpndVar(x, 1), OpndInt(1)),
                Instr::bin(OpndReg(0), OpndVar(x, 1), BinOp::Add, OpndInt(2)),
                Instr::assign(OpndVar(x, 2), OpndReg(0)),
                Instr::bin(OpndReg(1), OpndVar(x, 2), BinOp::Add, OpndInt(3)),
                Instr::assign(OpndVar(x, 3), OpndReg(1)),
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

            let param = OpndVar(VarId(0), 0);
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

            let a = OpndVar(VarId(0), 0);
            let b = OpndVar(VarId(1), 0);
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
                Instr::un(OpndReg(0), UnOp::Neg, OpndVar(VarId(0), 0)),
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
                Instr::assign(OpndVar(VarId(0), 1), OpndVar(VarId(1), 0)),
                Instr::bin(OpndReg(0), OpndVar(VarId(1), 0), BinOp::Cmp(CmpOp::Eq), OpndInt(7)),
                Instr::test(OpndReg(0), BlockId(1), BlockId(2))
            ]);

            assert_block(ir_fct, 1, vec![
                Instr::assign(OpndVar(VarId(0), 2), OpndInt(1)),
                Instr::bin(OpndReg(1), OpndVar(VarId(0), 2), BinOp::Add, OpndInt(1)),
                Instr::assign(OpndVar(VarId(1), 1), OpndReg(1)),
                Instr::goto(BlockId(3))
            ]);

            assert_block(ir_fct, 2, vec![
                Instr::bin(OpndReg(2), OpndVar(VarId(0),1), BinOp::Add, OpndInt(1)),
                Instr::bin(OpndReg(3), OpndReg(2), BinOp::Add, OpndVar(VarId(1), 0)),
                Instr::assign(OpndVar(VarId(0), 4), OpndReg(3)),
                Instr::assign(OpndVar(VarId(2), 1), OpndInt(2)),
                Instr::goto(BlockId(3))
            ]);

            assert_block(ir_fct, 3, vec![
                Instr::phi(VarId(0), 3, vec![2, 4], 1),
                Instr::phi(VarId(1), 2, vec![1, 0], 0),
                Instr::phi(VarId(2), 2, vec![0, 1], 0),
                Instr::bin(OpndReg(4), OpndVar(VarId(0), 3), BinOp::Add, OpndVar(VarId(1), 2)),
                Instr::bin(OpndReg(5), OpndReg(4), BinOp::Add, OpndVar(VarId(2), 2)),
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
                Instr::assign(OpndVar(VarId(0), 1), OpndInt(1)),
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

    fn assert_block(fct: &Fct, block_id: usize, expected: Vec<Instr>) {
        let instrs = &fct.blocks[block_id].instructions;

        assert_eq!(instrs.len(), expected.len());

        for (instr, ind) in instrs.iter().zip(0..) {
            assert_eq!(*instr, expected[ind]);
        }
    }
}
