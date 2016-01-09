use std::cmp;

use ast::*;
use ast::Stmt::*;
use ast::Expr::*;
use ast::visit::*;
use cpu;
use ctxt::Context;

use jit::expr::is_leaf;

use mem;

use sym::BuiltinType;

pub fn generate<'a, 'ast>(ctxt: &'a Context<'a, 'ast>, fct: &'ast Function) -> Info {
    InfoGenerator::new(ctxt, fct).generate()
}

pub struct Info {
    pub localsize: u32,
    pub tempsize: u32,
    pub fct_call: bool,
}

impl Info {
    pub fn stacksize(&self) -> u32 {
        self.localsize + self.tempsize
    }
}

impl Default for Info {
    fn default() -> Info {
        Info {
            localsize: 0,
            tempsize: 0,
            fct_call: false,
        }
    }
}

struct InfoGenerator<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    fct: &'ast Function,

    localsize: u32,
    max_tempsize: u32,
    cur_tempsize: u32,

    param_offset: i32,
    fct_call: bool,
}

impl<'a, 'ast> InfoGenerator<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>, fct: &'ast Function) -> InfoGenerator<'a, 'ast> {
        InfoGenerator {
            ctxt: ctxt,
            fct: fct,

            localsize: 0,
            max_tempsize: 0,
            cur_tempsize: 0,

            param_offset: cpu::PARAM_OFFSET,

            fct_call: false,
        }
    }

    fn generate(mut self) -> Info {
        self.visit_fct(self.fct);

        Info {
            localsize: self.localsize,
            tempsize: self.max_tempsize,
            fct_call: self.fct_call,
        }
    }

    fn reserve_stack_for_var(&mut self, id: NodeId) {
        self.ctxt.var_mut(id, |v, _| {
            let ty_size = v.data_type.size();
            self.localsize = mem::align(self.localsize + ty_size, ty_size);
            v.offset = -(self.localsize as i32);
        });
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
            let ty = self.ctxt.var_mut(p.id, |v, _| {
                v.offset = self.param_offset;

                v.data_type
            });

            // determine next `param_offset`
            self.param_offset = cpu::next_param_offset(self.param_offset, ty);
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
                self.fct_call = true;

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

    fn info<F>(code: &'static str, f: F) where F: FnOnce(&Context, &FctInfo, Info) {
        test::parse(code, |ctxt| {
            let fct = ctxt.ast.elements[0].to_function().unwrap();
            let info = generate(ctxt, fct);

            ctxt.fct_info(fct.id, |fct| {
                f(ctxt, fct, info);
            });
        });
    }

    #[test]
    fn test_tempsize() {
        info("fn f() { 1+2*3; }", |_, _, info| { assert_eq!(4, info.tempsize); });
        info("fn f() { 2*3+4+5; }", |_, _, info| { assert_eq!(0, info.tempsize); });
        info("fn f() { 1+(2+(3+4)); }", |_, _, info| { assert_eq!(8, info.tempsize); })
    }

    #[test]
    fn test_tempsize_for_fct_call() {
        info("fn f() { g(1,2,3,4,5,6); }
              fn g(a:int, b:int, c:int, d:int, e:int, f:int) {}", |_, _, info| {
            assert_eq!(0, info.tempsize);
        });

        info("fn f() { g(1,2,3,4,5,6,7,8); }
              fn g(a:int, b:int, c:int, d:int, e:int, f:int, g:int, h:int) {}", |_, _, info| {
            assert_eq!(16, info.tempsize);
        });

        info("fn f() { g(1,2,3,4,5,6,7,8)+(1+2); }
              fn g(a:int, b:int, c:int, d:int, e:int, f:int, g:int, h:int) {}", |_, _, info| {
            assert_eq!(20, info.tempsize);
        });
    }

    #[test]
    fn test_invocation_flag() {
        info("fn f() { g(); } fn g() { }", |_, _, info| {
            assert!(info.fct_call);
        });

        info("fn f() { }", |_, _, info| {
            assert!(!info.fct_call);
        });
    }

    #[test]
    fn test_param_offset() {
        info("fn f(a: bool, b: int) { var c = 1; }", |ctxt, fct, info| {
            assert_eq!(12, info.localsize);

            for (varid, offset) in fct.vars.iter().zip(&[-1, -8, -12]) {
                assert_eq!(*offset, ctxt.var_infos.borrow()[varid.0].offset);
            }
        });
    }

    #[test]
    fn test_params_over_6_offset() {
        info("fn f(a: int, b: int, c: int, d: int,
                   e: int, f: int, g: int, h: int) {
                  var i : int = 1;
              }", |ctxt, fct, info| {
            assert_eq!(28, info.localsize);
            let offsets = [-4, -8, -12, -16, -20, -24, 16, 24, -28];

            for (varid, offset) in fct.vars.iter().zip(&offsets) {
                assert_eq!(*offset, ctxt.var_infos.borrow()[varid.0].offset);
            }
        });
    }

    #[test]
    fn test_var_offset() {
        info("fn f() { var a = true; var b = false; var c = 2; var d = \"abc\"; }", |ctxt, fct, info| {
            assert_eq!(16, info.localsize);

            for (varid, offset) in fct.vars.iter().zip(&[-1, -2, -8, -16]) {
                assert_eq!(*offset, ctxt.var_infos.borrow()[varid.0].offset);
            }
        });
    }
}
