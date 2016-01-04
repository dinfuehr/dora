use std::cmp;

use ast::*;
use ast::Stmt::*;
use ast::Expr::*;
use ast::visit::*;

use codegen::expr::is_leaf;
use codegen::x64::reg::*;
use ctxt::Context;

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

            // first param offset to rbp is +16,
            // rbp+0 -> saved rbp
            // rbp+8 -> return address
            param_offset: 16,

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

    fn increase_stack(&mut self, id: NodeId) {
        self.ctxt.var_mut(id, |v, _| {
            let ty_size = v.data_type.size();
            self.localsize = mem::align(self.localsize + ty_size, ty_size);
            v.offset = -(self.localsize as i32);
        });
    }
}

impl<'a, 'ast> Visitor<'ast> for InfoGenerator<'a, 'ast> {
    fn visit_param(&mut self, p: &'ast Param) {
        // on x64 only the first 6 parameters are stored in registers
        if (p.idx as usize) < REG_PARAMS.len() {
            self.increase_stack(p.id);

        // the rest of the parameters need to be stored in the callers stack
        } else {
            self.ctxt.var_mut(p.id, |v, _| { v.offset = self.param_offset; });

            // all params on stack need size of 8
            self.param_offset += 8;
        }
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        if let StmtVar(ref var) = *s {
            self.increase_stack(var.id);
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
            ExprCall(_) => {
                self.fct_call = true;
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
