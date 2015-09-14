use codegen::x64::reg::*;
use mem;
use parser::ast::ctxt::Context;

use parser::ast::*;
use parser::ast::Stmt::*;
use parser::ast::Expr::*;
use parser::ast::visit::*;

pub fn generate<'a, 'ast>(ctxt: &'a Context<'a, 'ast>, fct: &'ast Function) {
    CodeGenInfo::new(ctxt, fct).generate();
}

struct CodeGenInfo<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    fct: &'ast Function,

    stacksize: u32,
    param_offset: i32,
    contains_fct_invocation: bool,
}

impl<'a, 'ast> CodeGenInfo<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>, fct: &'ast Function) -> CodeGenInfo<'a, 'ast> {
        CodeGenInfo {
            ctxt: ctxt,
            fct: fct,

            stacksize: 0,

            // first param offset to rbp is +16,
            // rbp+0 -> saved rbp
            // rbp+8 -> return address
            param_offset: 16,

            contains_fct_invocation: false,
        }
    }

    fn generate(&mut self) {
        self.visit_fct(self.fct);

        self.ctxt.function(self.fct.id, |fct| {
            fct.stacksize = self.stacksize;
            fct.contains_fct_invocation = self.contains_fct_invocation;
        });
    }

    fn increase_stack(&mut self, id: NodeId) {
        self.ctxt.var(id, |v| {
            let ty_size = v.data_type.size();
            self.stacksize = mem::align(self.stacksize + ty_size, ty_size);
            v.offset = -(self.stacksize as i32);
        });
    }
}

impl<'a, 'ast> Visitor<'ast> for CodeGenInfo<'a, 'ast> {
    fn visit_param(&mut self, p: &'ast Param) {
        // on x64 only the first 6 parameters are stored in registers
        if (p.idx as usize) < PARAM_REGS.len() {
            self.increase_stack(p.id);

        // the rest of the parameters need to be stored in the callers stack
        } else {
            self.ctxt.var(p.id, |v| {
                v.offset = self.param_offset;

                // all params on stack need size of 8
                self.param_offset += 8;
            });
        }
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        if let StmtVar(ref var) = *s {
            self.increase_stack(var.id);
        }

        visit::walk_stmt(self, s);
    }

    fn visit_expr(&mut self, e: &'ast Expr) {
        if let ExprCall(_) = *e {
            self.contains_fct_invocation = true;
        }

        visit::walk_expr(self, e);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use parser::ast::ctxt::*;
    use test;

    fn info<F>(code: &'static str, f: F) where F: FnOnce(&Context, &FctInfo) {
        test::parse(code, |ctxt| {
            let fct = ctxt.ast.elements[0].to_function().unwrap();
            generate(ctxt, fct);

            ctxt.function(fct.id, |fct| {
                f(ctxt, fct);
            });
        });
    }

    #[test]
    fn test_invocation_flag() {
        info("fn f() { g(); } fn g() { }", |ctxt, fct| {
            assert!(fct.contains_fct_invocation);
        });

        info("fn f() { }", |ctxt, fct| {
            assert!(!fct.contains_fct_invocation);
        });
    }

    #[test]
    fn test_param_offset() {
        info("fn f(a: bool, b: int) { var c = 1; }", |ctxt, fct| {
            assert_eq!(12, fct.stacksize);

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
              }", |ctxt, fct| {
            assert_eq!(28, fct.stacksize);
            let offsets = [-4, -8, -12, -16, -20, -24, 16, 24, -28];

            for (varid, offset) in fct.vars.iter().zip(&offsets) {
                assert_eq!(*offset, ctxt.var_infos.borrow()[varid.0].offset);
            }
        });
    }

    #[test]
    fn test_var_offset() {
        info("fn f() { var a = true; var b = false; var c = 2; var d = \"abc\"; }", |ctxt, fct| {
            assert_eq!(16, fct.stacksize);

            for (varid, offset) in fct.vars.iter().zip(&[-1, -2, -8, -16]) {
                assert_eq!(*offset, ctxt.var_infos.borrow()[varid.0].offset);
            }
        });
    }
}
