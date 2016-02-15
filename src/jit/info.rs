use std::cmp;

use ast::*;
use ast::Stmt::*;
use ast::Expr::*;
use ast::visit::*;
use cpu;
use ctxt::{Context, Fct};
use jit::expr::is_leaf;
use mem;
use ty::BuiltinType;

pub fn generate<'a, 'ast: 'a>(ctxt: &'a Context<'ast>, fct: &'a mut Fct<'ast>) {
    let ast = fct.ast();

    let mut ig = InfoGenerator {
        ctxt: ctxt,
        fct: fct,
        ast: ast,

        localsize: 0,
        max_tempsize: 0,
        cur_tempsize: 0,

        param_offset: cpu::PARAM_OFFSET,
        leaf: true,
    };

    ig.generate();
}

struct InfoGenerator<'a, 'ast: 'a> {
    ctxt: &'a Context<'ast>,
    fct: &'a mut Fct<'ast>,
    ast: &'ast Function,

    localsize: i32,
    max_tempsize: i32,
    cur_tempsize: i32,

    param_offset: i32,
    leaf: bool,
}

impl<'a, 'ast> InfoGenerator<'a, 'ast> {
    fn generate(&mut self) {
        self.visit_fct(self.ast);

        let src = self.fct.src_mut();
        src.localsize = self.localsize;
        src.tempsize = self.max_tempsize;
        src.leaf = self.leaf;
    }

    fn reserve_stack_for_var(&mut self, id: NodeId) {
        let var = self.fct.var_by_node_id_mut(id);

        let ty_size = var.data_type.size();
        self.localsize = mem::align_i32(self.localsize + ty_size, ty_size);
        var.offset = -self.localsize;
    }
}

impl<'a, 'ast> Visitor<'ast> for InfoGenerator<'a, 'ast> {
    fn visit_param(&mut self, p: &'ast Param) {
        // only some parameters are passed in registers
        // these registers need to be stored into local variables
        if (p.idx as usize) < cpu::REG_PARAMS.len() {
            self.reserve_stack_for_var(p.id);

        // the rest of the parameters are already stored on the stack
        // just use the current offset
        } else {
            let var = self.fct.var_by_node_id_mut(p.id);
            var.offset = self.param_offset;

            // determine next `param_offset`
            self.param_offset = cpu::next_param_offset(self.param_offset, var.data_type);
        }
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        if let StmtVar(ref var) = *s {
            self.reserve_stack_for_var(var.id);
        }

        visit::walk_stmt(self, s);
    }

    fn visit_expr_top(&mut self, e: &'ast Expr) {
        self.cur_tempsize = 0;
        self.visit_expr(e);
        self.max_tempsize = cmp::max(self.cur_tempsize, self.max_tempsize);
    }

    fn visit_expr(&mut self, e: &'ast Expr) {
        match *e {
            ExprCall(ref expr) => {
                // function invokes another function
                self.leaf = false;

                // some function parameters are stored on stack,
                // reservere space for them
                self.cur_tempsize += cpu::reserve_stack_for_call(&expr.args);
            }

            ExprBin(ref expr) => {
                if !is_leaf(&expr.rhs) {
                    self.cur_tempsize += BuiltinType::Int.size();
                }
            }

            _ => {}
        }

        visit::walk_expr(self, e);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use ctxt::*;
    use test;

    fn info<F>(code: &'static str, f: F) where F: FnOnce(&Fct) {
        test::parse(code, |ctxt| {
            let ast = ctxt.ast.elements[0].to_function().unwrap();

            ctxt.fct_by_node_id_mut(ast.id, |fct| {
                generate(ctxt, fct);

                f(fct);
            });
        });
    }

    #[test]
    fn test_tempsize() {
        info("fn f() { 1+2*3; }", |fct| { assert_eq!(4, fct.src().tempsize); });
        info("fn f() { 2*3+4+5; }", |fct| { assert_eq!(0, fct.src().tempsize); });
        info("fn f() { 1+(2+(3+4)); }", |fct| { assert_eq!(8, fct.src().tempsize); })
    }

    #[test]
    fn test_tempsize_for_fct_call() {
        info("fn f() { g(1,2,3,4,5,6); }
              fn g(a:int, b:int, c:int, d:int, e:int, f:int) {}", |fct| {
            assert_eq!(0, fct.src().tempsize);
        });

        info("fn f() { g(1,2,3,4,5,6,7,8); }
              fn g(a:int, b:int, c:int, d:int, e:int, f:int, g:int, h:int) {}", |fct| {
            assert_eq!(16, fct.src().tempsize);
        });

        info("fn f() { g(1,2,3,4,5,6,7,8)+(1+2); }
              fn g(a:int, b:int, c:int, d:int, e:int, f:int, g:int, h:int) -> int {
                  return 0;
              }", |fct| {
            assert_eq!(20, fct.src().tempsize);
        });
    }

    #[test]
    fn test_invocation_flag() {
        info("fn f() { g(); } fn g() { }", |fct| {
            assert!(!fct.src().leaf);
        });

        info("fn f() { }", |fct| {
            assert!(fct.src().leaf);
        });
    }

    #[test]
    fn test_param_offset() {
        info("fn f(a: bool, b: int) { var c = 1; }", |fct| {
            assert_eq!(12, fct.src().localsize);

            for (var, offset) in fct.src().vars.iter().zip(&[-1, -8, -12]) {
                assert_eq!(*offset, var.offset);
            }
        });
    }

    #[test]
    fn test_params_over_6_offset() {
        info("fn f(a: int, b: int, c: int, d: int,
                   e: int, f: int, g: int, h: int) {
                  var i : int = 1;
              }", |fct| {
            assert_eq!(28, fct.src().localsize);
            let offsets = [-4, -8, -12, -16, -20, -24, 16, 24, -28];

            for (var, offset) in fct.src().vars.iter().zip(&offsets) {
                assert_eq!(*offset, var.offset);
            }
        });
    }

    #[test]
    fn test_var_offset() {
        info("fn f() { var a = true; var b = false; var c = 2; var d = \"abc\"; }", |fct| {
            assert_eq!(16, fct.src().localsize);

            for (var, offset) in fct.src().vars.iter().zip(&[-1, -2, -8, -16]) {
                assert_eq!(*offset, var.offset);
            }
        });
    }
}
