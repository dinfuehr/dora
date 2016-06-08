    use libc::c_void;
use std::cmp;

use ast::*;
use ast::Stmt::*;
use ast::Expr::*;
use ast::visit::*;
use cpu::{self, Reg};
use ctxt::{Arg, Callee, CallSite, Context, Fct, FctKind, FctSrc, Store, Var, VarId};
use jit::expr::is_leaf;
use mem;
use mem::ptr::Ptr;
use stdlib;
use ty::BuiltinType;

pub fn generate<'a, 'ast: 'a>(ctxt: &'a Context<'ast>, fct: &Fct<'ast>,
                              src: &'a mut FctSrc<'ast>) {
    let mut ig = InfoGenerator {
        ctxt: ctxt,
        fct: fct,
        ast: src.ast,
        src: src,

        localsize: 0,
        max_tempsize: 0,
        cur_tempsize: 0,
        argsize: 0,

        param_offset: cpu::PARAM_OFFSET,
        leaf: true,
        eh_return_value: None,
        eh_status: None,
    };

    ig.generate();
}

struct InfoGenerator<'a, 'ast: 'a> {
    ctxt: &'a Context<'ast>,
    fct: &'a Fct<'ast>,
    src: &'a mut FctSrc<'ast>,
    ast: &'ast Function,

    localsize: i32,
    max_tempsize: i32,
    cur_tempsize: i32,
    argsize: i32,

    eh_return_value: Option<i32>,
    eh_status: Option<i32>,
    param_offset: i32,
    leaf: bool,
}

impl<'a, 'ast> Visitor<'ast> for InfoGenerator<'a, 'ast> {
    fn visit_param(&mut self, p: &'ast Param) {
        let idx = (p.idx as usize) + if self.fct.in_class() { 1 } else { 0 };

        // only some parameters are passed in registers
        // these registers need to be stored into local variables
        if idx < cpu::REG_PARAMS.len() {
            self.reserve_stack_for_node(p.var());

        // the rest of the parameters are already stored on the stack
        // just use the current offset
        } else {
            let var = &mut self.src.vars[p.var()];
            var.offset = self.param_offset;

            // determine next `param_offset`
            self.param_offset = cpu::next_param_offset(self.param_offset, var.ty);
        }
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        if let StmtVar(ref var) = *s {
            self.reserve_stack_for_node(var.var());
        }

        if let StmtTry(ref try) = *s {
            let ret = self.fct.return_type;

            if !ret.is_unit() {
                self.eh_return_value = Some(self.eh_return_value.unwrap_or_else(
                    || self.reserve_stack_for_type(ret)));
            }

            // we also need space for catch block parameters
            for catch in &try.catch_blocks {
                self.reserve_stack_for_node(catch.var());
            }

            if let Some(ref finally_block) = try.finally_block {
                let offset = self.reserve_stack_for_type(BuiltinType::Ptr);
                finally_block.set_offset(offset);
            }
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
            ExprCall(ref expr) => self.expr_call(expr),
            ExprSuperCall(ref expr) => self.expr_super_call(expr),
            ExprArray(ref expr) => self.expr_array(expr),
            ExprAssign(ref expr) => self.expr_assign(expr),
            ExprBin(ref expr) => self.expr_bin(expr),

            _ => visit::walk_expr(self, e)
        }
    }
}

impl<'a, 'ast> InfoGenerator<'a, 'ast> {
    fn generate(&mut self) {
        if self.fct.owner_class.is_some() {
            self.reserve_stack_for_self();
        }

        self.visit_fct(self.ast);

        self.src.localsize = self.localsize;
        self.src.tempsize = self.max_tempsize;
        self.src.argsize = self.argsize;
        self.src.leaf = self.leaf;
        self.src.eh_return_value = self.eh_return_value;
    }

    fn reserve_stack_for_self(&mut self) {
        let offset = self.reserve_stack_for_type(BuiltinType::Ptr);
        self.src.var_self_mut().offset = offset;
    }

    fn reserve_stack_for_node(&mut self, id: VarId) {
        let ty = self.src.vars[id].ty;
        let offset = self.reserve_stack_for_type(ty);
        self.src.vars[id].offset = offset;
    }

    fn reserve_stack_for_type(&mut self, ty: BuiltinType) -> i32 {
        let ty_size = ty.size();
        self.localsize = mem::align_i32(self.localsize + ty_size, ty_size);

        -self.localsize
    }

    fn expr_array(&mut self, expr: &'ast ExprArrayType) {
        self.visit_expr(&expr.object);
        self.visit_expr(&expr.index);

        if self.is_intrinsic(expr.id) {
            self.reserve_temp_for_node(&expr.object);

        } else {
            let args = vec![
                Arg::Expr(&expr.object, BuiltinType::Unit, 0),
                Arg::Expr(&expr.index, BuiltinType::Unit, 0)
            ];

            self.universal_call(expr.id, args, true,
                None, None);
        }
    }

    fn is_intrinsic(&self, id: NodeId) -> bool {
        let fid = self.src.calls.get(&id).unwrap().fct_id();

        // the function we compile right now is never an intrinsic
        if self.fct.id == fid { return false; }

        self.ctxt.fct_by_id(fid).kind.is_intrinsic()
    }

    fn expr_call(&mut self, expr: &'ast ExprCallType) {
        if self.is_intrinsic(expr.id) {
            for arg in &expr.args {
                self.visit_expr(arg);
            }

            assert_eq!(1, expr.args.len());

            let id = expr.object.as_ref().unwrap().id();
            self.reserve_temp_for_node_with_type(id, BuiltinType::Ptr);
            return;
        }

        let call_type = *self.src.calls.get(&expr.id).unwrap();

        let mut args = expr.args.iter().map(|arg| {
            Arg::Expr(arg, BuiltinType::Unit, 0)
        }).collect::<Vec<_>>();

        let mut in_class = true;

        if call_type.is_ctor() {
            args.insert(0, Arg::Selfie(call_type.cls_id(), 0));
        } else if call_type.is_method() {
            let object = expr.object.as_ref().unwrap();
            args.insert(0, Arg::Expr(object, BuiltinType::Unit, 0));
        } else if call_type.is_ctor_new() {
            args.insert(0, Arg::SelfieNew(call_type.cls_id(), 0));
        } else {
            in_class = false;
        }

        self.universal_call(expr.id, args, in_class, None, None);
    }

    fn expr_super_call(&mut self, expr: &'ast ExprSuperCallType) {
        let mut args = expr.args.iter().map(|arg| {
            Arg::Expr(arg, BuiltinType::Unit, 0)
        }).collect::<Vec<_>>();

        args.insert(0, Arg::Selfie(expr.class_id(), 0));

        self.universal_call(expr.id, args, true, None, None);
    }

    fn universal_call(&mut self, id: NodeId, args: Vec<Arg<'ast>>, in_class: bool,
                      callee: Option<Callee>, return_type: Option<BuiltinType>) {
        // function invokes another function
        self.leaf = false;
        let mut is_ctor = false;

        for arg in &args {
            match *arg {
                Arg::Expr(ast, _, _) => self.visit_expr(ast),
                Arg::Selfie(_, _)
                | Arg::SelfieNew(_, _) => { is_ctor = true; }
            }
        }

        // reserve stack for arguments
        let no_args = args.len() as i32;
        let argsize = 8 * if no_args <= 6 { 0 } else { no_args - 6 };

        if argsize > self.argsize {
            self.argsize = argsize;
        }

        let fid = if callee.is_none() {
            Some(self.src.calls.get(&id).unwrap().fct_id())
        } else {
            None
        };

        let args = args.iter().enumerate().map(|(ind, arg)| {
            match *arg {
                Arg::Expr(ast, mut ty, _) => {
                    if let Some(fid) = fid {
                        ty = if ind == 0 && in_class {
                            let cid = self.ctxt.fct_by_id(fid).owner_class.unwrap();
                            self.ctxt.cls_by_id(cid).ty

                        } else {
                            let ind = if in_class { ind-1 } else { ind };
                            self.ctxt.fct_by_id(fid).params_types[ind]
                        }
                    }

                    Arg::Expr(ast, ty, self.reserve_temp_for_node_with_type(ast.id(), ty))
                }

                Arg::SelfieNew(cid, _) => Arg::SelfieNew(cid, self.reserve_temp_for_ctor(id)),
                Arg::Selfie(cid, _) => Arg::Selfie(cid, self.reserve_temp_for_ctor(id))
            }
        }).collect::<Vec<_>>();

        let return_type = return_type.unwrap_or_else(|| {
            let fid = fid.unwrap();
            self.ctxt.fct_by_id(fid).return_type
        });

        let callee = callee.unwrap_or_else(|| Callee::Fct(fid.unwrap()));

        let csite = CallSite {
            callee: callee,
            args: args,
            return_type: return_type
        };

        // remember args
        self.src.call_sites.insert(id, csite);
    }

    fn expr_assign(&mut self, e: &'ast ExprAssignType) {
        if e.lhs.is_ident() {
            self.visit_expr(&e.rhs);

            let lhs = e.lhs.to_ident().unwrap();
            let ident_type = lhs.ident_type();

            if ident_type.is_field() {
                self.reserve_temp_for_node_with_type(lhs.id, BuiltinType::Ptr);
            }

        } else if e.lhs.is_field() {
            let lhs = e.lhs.to_field().unwrap();

            self.visit_expr(&lhs.object);
            self.visit_expr(&e.rhs);

            self.reserve_temp_for_node(&lhs.object);

        } else {
            assert!(e.lhs.is_array());
            let array = e.lhs.to_array().unwrap();

            self.visit_expr(&array.object);
            self.visit_expr(&array.index);
            self.visit_expr(&e.rhs);

            if self.is_intrinsic(e.id) {
                self.reserve_temp_for_node_with_type(array.object.id(), BuiltinType::Ptr);
                self.reserve_temp_for_node_with_type(array.index.id(), BuiltinType::Int);
                self.reserve_temp_for_node_with_type(e.rhs.id(), BuiltinType::Int);

            } else {
                let args = vec![
                    Arg::Expr(&array.object, BuiltinType::Unit, 0),
                    Arg::Expr(&array.index, BuiltinType::Unit, 0),
                    Arg::Expr(&e.rhs, BuiltinType::Unit, 0),
                ];

                self.universal_call(e.id, args, true, None, None);
            }
        }
    }

    fn expr_bin(&mut self, expr: &'ast ExprBinType) {
        self.visit_expr(&expr.lhs);
        self.visit_expr(&expr.rhs);

        if expr.op == BinOp::Add && BuiltinType::Str == expr.ty() {
            let args = vec![
                Arg::Expr(&expr.lhs, BuiltinType::Str, 0),
                Arg::Expr(&expr.rhs, BuiltinType::Str, 0)
            ];
            let ptr = Ptr::new(stdlib::strcat as *mut c_void);

            self.universal_call(expr.id, args, false,
                Some(Callee::Ptr(ptr)), Some(BuiltinType::Str));

        } else if expr.op.is_compare()
                  && (expr.lhs.ty().is_str()
                      || expr.rhs.ty().is_str()) {
            let args = vec![
                Arg::Expr(&expr.lhs, BuiltinType::Str, 0),
                Arg::Expr(&expr.rhs, BuiltinType::Str, 0)
            ];
            let ptr = Ptr::new(stdlib::strcmp as *mut c_void);

            self.universal_call(expr.id, args, false,
                Some(Callee::Ptr(ptr)), Some(BuiltinType::Bool));

        } else if !is_leaf(&expr.rhs) {
            self.reserve_temp_for_node(&expr.lhs);
        }
    }

    fn reserve_temp_for_node(&mut self, expr: &Expr) -> i32 {
        self.reserve_temp_for_node_with_type(expr.id(), expr.ty())
    }

    fn reserve_temp_for_ctor(&mut self, id: NodeId) -> i32 {
        self.reserve_temp_for_node_with_type(id, BuiltinType::Ptr)
    }

    fn reserve_temp_for_node_with_type(&mut self, id: NodeId, ty: BuiltinType) -> i32 {
        let ty_size = ty.size();
        self.cur_tempsize = mem::align_i32(self.cur_tempsize + ty_size, ty_size);

        self.src.storage.insert(id, Store::Temp(self.cur_tempsize, ty));
        // println!("temp on {} with type {:?}", self.cur_tempsize, ty);

        self.cur_tempsize
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use ctxt::*;
    use test;

    fn info<F>(code: &'static str, f: F) where F: FnOnce(&FctSrc) {
        test::parse(code, |ctxt| {
            let ast = ctxt.ast.elements[0].to_function().unwrap();

            let fct = ctxt.fct_by_node_id(ast.id);
            let src = fct.src();
            let mut src = src.lock().unwrap();
            generate(ctxt, &fct, &mut src);

            f(&src);
        });
    }

    #[test]
    fn test_tempsize() {
        info("fun f() { 1+2*3; }", |fct| { assert_eq!(4, fct.tempsize); });
        info("fun f() { 2*3+4+5; }", |fct| { assert_eq!(0, fct.tempsize); });
        info("fun f() { 1+(2+(3+4)); }", |fct| { assert_eq!(8, fct.tempsize); })
    }

    #[test]
    fn test_tempsize_for_fct_call() {
        info("fun f() { g(1,2,3,4,5,6); }
              fun g(a:int, b:int, c:int, d:int, e:int, f:int) {}", |fct| {
            assert_eq!(24, fct.tempsize);
        });

        info("fun f() { g(1,2,3,4,5,6,7,8); }
              fun g(a:int, b:int, c:int, d:int, e:int, f:int, g:int, h:int) {}", |fct| {
            assert_eq!(32, fct.tempsize);
        });

        info("fun f() { g(1,2,3,4,5,6,7,8)+(1+2); }
              fun g(a:int, b:int, c:int, d:int, e:int, f:int, g:int, h:int) -> int {
                  return 0;
              }", |fct| {
            assert_eq!(36, fct.tempsize);
        });
    }

    #[test]
    fn test_invocation_flag() {
        info("fun f() { g(); } fun g() { }", |fct| {
            assert!(!fct.leaf);
        });

        info("fun f() { }", |fct| {
            assert!(fct.leaf);
        });
    }

    #[test]
    fn test_param_offset() {
        info("fun f(a: bool, b: int) { let c = 1; }", |fct| {
            assert_eq!(12, fct.localsize);

            for (var, offset) in fct.vars.iter().zip(&[-1, -8, -12]) {
                assert_eq!(*offset, var.offset);
            }
        });
    }

    #[test]
    fn test_params_over_6_offset() {
        info("fun f(a: int, b: int, c: int, d: int,
                   e: int, f: int, g: int, h: int) {
                  let i : int = 1;
              }", |fct| {
            assert_eq!(28, fct.localsize);
            let offsets = [-4, -8, -12, -16, -20, -24, 16, 24, -28];

            for (var, offset) in fct.vars.iter().zip(&offsets) {
                assert_eq!(*offset, var.offset);
            }
        });
    }

    #[test]
    fn test_var_offset() {
        info("fun f() { let a = true; let b = false; let c = 2; let d = \"abc\"; }", |fct| {
            assert_eq!(16, fct.localsize);

            for (var, offset) in fct.vars.iter().zip(&[-1, -2, -8, -16]) {
                assert_eq!(*offset, var.offset);
            }
        });
    }
}
