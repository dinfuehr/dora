use std::cmp::max;
use std::collections::HashMap;
use std::rc::Rc;

use dora_parser::ast::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::Expr::*;
use dora_parser::ast::visit::*;
use class::TypeArgs;
use cpu::*;
use ctxt::{Arg, CallSite, CallType, SemContext, Fct, FctId, FctParent, FctSrc, NodeMap, Store,
           VarId};
use mem;
use ty::BuiltinType;

pub fn generate<'a, 'ast: 'a>(ctxt: &'a SemContext<'ast>,
                              fct: &Fct<'ast>,
                              src: &'a FctSrc,
                              jit_info: &'a mut JitInfo<'ast>,
                              cls_type_params: &[BuiltinType],
                              fct_type_params: &[BuiltinType]) {
    let start = if fct.has_self() { 1 } else { 0 };

    let mut ig = InfoGenerator {
        ctxt: ctxt,
        fct: fct,
        ast: fct.ast,
        src: src,
        jit_info: jit_info,

        localsize: 0,
        max_tempsize: 0,
        cur_tempsize: 0,
        argsize: 0,

        param_offset: PARAM_OFFSET,
        leaf: true,
        eh_return_value: None,
        eh_status: None,

        param_reg_idx: start,
        param_freg_idx: 0,

        cls_type_params: cls_type_params,
        fct_type_params: fct_type_params,
    };

    ig.generate();
}

pub struct JitInfo<'ast> {
    pub tempsize: i32, // size of temporary variables on stack
    pub localsize: i32, // size of local variables on stack
    pub argsize: i32, // size of arguments on stack (need to be on bottom)
    pub leaf: bool, // false if fct calls other functions
    pub eh_return_value: Option<i32>, // stack slot for return value storage

    pub map_stores: NodeMap<Store>,
    pub map_csites: NodeMap<CallSite<'ast>>,
    pub map_offsets: NodeMap<i32>,
    pub map_var_offsets: HashMap<VarId, i32>,
    pub map_var_types: HashMap<VarId, BuiltinType>,
}

impl<'ast> JitInfo<'ast> {
    pub fn get_store(&self, id: NodeId) -> Store {
        match self.map_stores.get(id) {
            Some(store) => *store,
            None => Store::Reg,
        }
    }

    pub fn stacksize(&self) -> i32 {
        mem::align_i32(self.tempsize + self.localsize + self.argsize, 16)
    }

    pub fn offset(&self, var_id: VarId) -> i32 {
        *self.map_var_offsets.get(&var_id).unwrap()
    }

    pub fn ty(&self, var_id: VarId) -> BuiltinType {
        *self.map_var_types.get(&var_id).unwrap()
    }

    pub fn new() -> JitInfo<'ast> {
        JitInfo {
            tempsize: 0,
            localsize: 0,
            argsize: 0,
            leaf: false,
            eh_return_value: None,

            map_stores: NodeMap::new(),
            map_csites: NodeMap::new(),
            map_offsets: NodeMap::new(),
            map_var_offsets: HashMap::new(),
            map_var_types: HashMap::new(),
        }
    }
}

struct InfoGenerator<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    fct: &'a Fct<'ast>,
    src: &'a FctSrc,
    ast: &'ast Function,
    jit_info: &'a mut JitInfo<'ast>,

    localsize: i32,
    max_tempsize: i32,
    cur_tempsize: i32,
    argsize: i32,

    eh_return_value: Option<i32>,
    eh_status: Option<i32>,
    param_offset: i32,
    leaf: bool,

    param_reg_idx: usize,
    param_freg_idx: usize,

    cls_type_params: &'a [BuiltinType],
    fct_type_params: &'a [BuiltinType],
}

impl<'a, 'ast> Visitor<'ast> for InfoGenerator<'a, 'ast> {
    fn visit_param(&mut self, p: &'ast Param) {
        let var = *self.src.map_vars.get(p.id).unwrap();
        let is_float = self.src.vars[var].ty.is_float();

        // only some parameters are passed in registers
        // these registers need to be stored into local variables
        if is_float && self.param_freg_idx < FREG_PARAMS.len() {
            self.reserve_stack_for_node(var);
            self.param_freg_idx += 1;

        } else if !is_float && self.param_reg_idx < REG_PARAMS.len() {
            self.reserve_stack_for_node(var);
            self.param_reg_idx += 1;

            // the rest of the parameters are already stored on the stack
            // just use the current offset
        } else {
            let var = &self.src.vars[var];
            self.jit_info
                .map_var_offsets
                .insert(var.id, self.param_offset);

            // determine next `param_offset`
            self.param_offset = next_param_offset(self.param_offset, var.ty);
        }
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        if let StmtVar(ref var) = *s {
            let var = *self.src.map_vars.get(var.id).unwrap();
            self.reserve_stack_for_node(var);
        }

        if let StmtDo(ref try) = *s {
            let ret = self.fct.return_type;

            if !ret.is_unit() {
                self.eh_return_value =
                    Some(self.eh_return_value
                             .unwrap_or_else(|| self.reserve_stack_for_type(ret)));
            }

            // we also need space for catch block parameters
            for catch in &try.catch_blocks {
                let var = *self.src.map_vars.get(catch.id).unwrap();
                self.reserve_stack_for_node(var);
            }

            if try.finally_block.is_some() {
                let offset = self.reserve_stack_for_type(BuiltinType::Ptr);
                self.jit_info.map_offsets.insert(try.id, offset);
            }
        }

        visit::walk_stmt(self, s);
    }

    fn visit_expr_top(&mut self, e: &'ast Expr) {
        self.cur_tempsize = 0;
        self.visit_expr(e);
        self.max_tempsize = max(self.cur_tempsize, self.max_tempsize);
    }

    fn visit_expr(&mut self, e: &'ast Expr) {
        match *e {
            ExprCall(ref expr) => self.expr_call(expr),
            ExprDelegation(ref expr) => self.expr_delegation(expr),
            ExprArray(ref expr) => self.expr_array(expr),
            ExprAssign(ref expr) => self.expr_assign(expr),
            ExprBin(ref expr) => self.expr_bin(expr),
            ExprUn(ref expr) => self.expr_un(expr),
            ExprConv(ref expr) => self.expr_conv(expr),

            _ => visit::walk_expr(self, e),
        }
    }
}

impl<'a, 'ast> InfoGenerator<'a, 'ast> {
    fn generate(&mut self) {
        if self.fct.has_self() {
            self.reserve_stack_for_self();
        }

        self.visit_fct(self.ast);

        self.jit_info.localsize = self.localsize;
        self.jit_info.tempsize = self.max_tempsize;
        self.jit_info.argsize = self.argsize;
        self.jit_info.leaf = self.leaf;
        self.jit_info.eh_return_value = self.eh_return_value;
    }

    fn reserve_stack_for_self(&mut self) {
        let ty = match self.fct.parent {
            FctParent::Class(clsid) => self.ctxt.classes[clsid].borrow().ty,

            FctParent::Impl(impl_id) => {
                let ximpl = self.ctxt.impls[impl_id].borrow();
                self.ctxt.classes[ximpl.cls_id()].borrow().ty
            }

            _ => unreachable!(),
        };

        let offset = self.reserve_stack_for_type(ty);

        let id = self.src.var_self().id;
        self.jit_info.map_var_offsets.insert(id, offset);
    }

    fn reserve_stack_for_node(&mut self, id: VarId) {
        let ty = self.src.vars[id].ty;
        let ty = self.specialize_type(ty);
        let offset = self.reserve_stack_for_type(ty);

        self.jit_info.map_var_offsets.insert(id, offset);
        self.jit_info.map_var_types.insert(id, ty);
    }

    fn reserve_stack_for_type(&mut self, ty: BuiltinType) -> i32 {
        let ty_size = ty.size(self.ctxt);
        self.localsize = mem::align_i32(self.localsize + ty_size, ty_size);

        -self.localsize
    }

    fn expr_array(&mut self, expr: &'ast ExprArrayType) {
        self.visit_expr(&expr.object);
        self.visit_expr(&expr.index);

        if self.is_intrinsic(expr.id) {
            self.reserve_temp_for_node(&expr.object);

        } else {
            let args = vec![Arg::Expr(&expr.object, BuiltinType::Unit, 0),
                            Arg::Expr(&expr.index, BuiltinType::Unit, 0)];

            self.universal_call(expr.id,
                                args,
                                None,
                                Rc::new(Vec::new()),
                                Rc::new(Vec::new()),
                                None);
        }
    }

    fn expr_conv(&mut self, e: &'ast ExprConvType) {
        self.visit_expr(&e.object);
        let is_valid = self.src.map_convs.get(e.id).unwrap().valid;

        if !e.is && !is_valid {
            self.reserve_temp_for_node(&e.object);
        }
    }

    fn is_intrinsic(&self, id: NodeId) -> bool {
        let fid = self.src.map_calls.get(id).unwrap().fct_id();

        // the function we compile right now is never an intrinsic
        if self.fct.id == fid {
            return false;
        }

        self.ctxt.fcts[fid].borrow().kind.is_intrinsic()
    }

    fn expr_call(&mut self, expr: &'ast ExprCallType) {
        if self.is_intrinsic(expr.id) {
            for arg in &expr.args {
                self.visit_expr(arg);
                self.reserve_temp_for_node(arg);
            }

            if let Some(ref object) = expr.object {
                self.visit_expr(object);
                self.reserve_temp_for_node(object);
            }

            return;
        }

        let call_type = self.src.map_calls.get(expr.id).unwrap().clone();

        let mut args = expr.args
            .iter()
            .map(|arg| Arg::Expr(arg, BuiltinType::Unit, 0))
            .collect::<Vec<_>>();

        let cls_type_params: TypeArgs;
        let fct_type_params: TypeArgs;

        match *call_type {
            CallType::Ctor(_, _, ref type_params) |
            CallType::CtorNew(_, _, ref type_params) => {
                let ty = self.ty(expr.id);
                let arg = if call_type.is_ctor() {
                    Arg::Selfie(ty, 0)
                } else {
                    Arg::SelfieNew(ty, 0)
                };

                args.insert(0, arg);

                cls_type_params = type_params.clone();
                fct_type_params = Rc::new(Vec::new());
            }

            CallType::Method(_, _) => {
                let object = expr.object.as_ref().unwrap();
                self.visit_expr(object);
                args.insert(0, Arg::Expr(object, BuiltinType::Unit, 0));

                cls_type_params = Rc::new(Vec::new());
                fct_type_params = Rc::new(Vec::new());
            }

            CallType::Fct(_, ref cls_tps, ref fct_tps) => {
                cls_type_params = cls_tps.clone();
                fct_type_params = fct_tps.clone();
            }
        }

        self.universal_call(expr.id,
                            args,
                            None,
                            cls_type_params,
                            fct_type_params,
                            None);
    }

    fn expr_delegation(&mut self, expr: &'ast ExprDelegationType) {
        let mut args = expr.args
            .iter()
            .map(|arg| Arg::Expr(arg, BuiltinType::Unit, 0))
            .collect::<Vec<_>>();

        let cls_id = *self.src.map_cls.get(expr.id).unwrap();
        args.insert(0, Arg::Selfie(BuiltinType::Class(cls_id), 0));

        self.universal_call(expr.id,
                            args,
                            None,
                            Rc::new(Vec::new()),
                            Rc::new(Vec::new()),
                            None);
    }

    fn universal_call(&mut self,
                      id: NodeId,
                      args: Vec<Arg<'ast>>,
                      callee: Option<FctId>,
                      cls_type_params: TypeArgs,
                      fct_type_params: TypeArgs,
                      return_type: Option<BuiltinType>) {
        // function invokes another function
        self.leaf = false;

        let mut reg_args: i32 = 0;
        let mut freg_args: i32 = 0;

        for arg in &args {
            match *arg {
                Arg::Expr(ast, ty, _) => {
                    self.visit_expr(ast);

                    if ty.is_float() {
                        freg_args += 1;
                    } else {
                        reg_args += 1;
                    }
                }

                Arg::Selfie(ty, _) |
                Arg::SelfieNew(ty, _) => {
                    if ty.is_float() {
                        freg_args += 1;
                    } else {
                        reg_args += 1;
                    }
                }
            }
        }

        // some register are reserved on stack
        let args_on_stack = max(0, reg_args - REG_PARAMS.len() as i32) +
                            max(0, freg_args - FREG_PARAMS.len() as i32);

        let argsize = 8 * args_on_stack;

        if argsize > self.argsize {
            self.argsize = argsize;
        }

        let fid = if callee.is_none() {
            Some(self.src.map_calls.get(id).unwrap().fct_id())
        } else {
            None
        };

        let mut super_call = false;

        let args = args.iter()
            .enumerate()
            .map(|(ind, arg)| match *arg {
                     Arg::Expr(ast, mut ty, _) => {
                if let Some(fid) = fid {
                    let fct = self.ctxt.fcts[fid].borrow();
                    ty = if ind == 0 && fct.has_self() {
                        if ast.is_super() {
                            super_call = true;
                        }

                        let cid = match fct.parent {
                            FctParent::Class(cid) => cid,
                            FctParent::Impl(impl_id) => {
                                let ximpl = self.ctxt.impls[impl_id].borrow();
                                ximpl.cls_id()
                            }
                            _ => unreachable!(),
                        };

                        let cls = self.ctxt.classes[cid].borrow();
                        cls.ty

                    } else {
                        let ty = fct.params_with_self()[ind];
                        let call_type = self.src.map_calls.get(id).unwrap().clone();

                        match *call_type {
                            CallType::Fct(_, ref cls_type_params, ref fct_type_params) => {
                                specialize_type(self.ctxt, ty, cls_type_params, fct_type_params)
                            }

                            CallType::Ctor(_, _, ref type_params) |
                            CallType::CtorNew(_, _, ref type_params) => {
                                specialize_type(self.ctxt, ty, type_params, &[])
                            }

                            _ => ty,
                        }
                    }
                }

                Arg::Expr(ast, ty, self.reserve_temp_for_node_with_type(ast.id(), ty))
            }

                     Arg::SelfieNew(cid, _) => Arg::SelfieNew(cid, self.reserve_temp_for_ctor(id)),
                     Arg::Selfie(cid, _) => Arg::Selfie(cid, self.reserve_temp_for_ctor(id)),
                 })
            .collect::<Vec<_>>();

        let return_type = return_type.unwrap_or_else(|| {
                                                         let fid = fid.unwrap();
                                                         self.ctxt.fcts[fid].borrow().return_type
                                                     });

        let callee = callee.unwrap_or_else(|| fid.unwrap());

        let csite = CallSite {
            callee: callee,
            args: args,
            cls_type_params: cls_type_params,
            fct_type_params: fct_type_params,
            super_call: super_call,
            return_type: return_type,
        };

        // remember args
        self.jit_info.map_csites.insert_or_replace(id, csite);
    }

    fn expr_assign(&mut self, e: &'ast ExprAssignType) {
        if e.lhs.is_ident() {
            self.visit_expr(&e.rhs);

            let lhs = e.lhs.to_ident().unwrap();
            let field = self.src.map_idents.get(lhs.id).unwrap().is_field();

            if field {
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
                self.reserve_temp_for_node(&array.object);
                self.reserve_temp_for_node(&array.index);
                self.reserve_temp_for_node(&e.rhs);

            } else {
                let args = vec![Arg::Expr(&array.object, BuiltinType::Unit, 0),
                                Arg::Expr(&array.index, BuiltinType::Unit, 0),
                                Arg::Expr(&e.rhs, BuiltinType::Unit, 0)];

                self.universal_call(e.id,
                                    args,
                                    None,
                                    Rc::new(Vec::new()),
                                    Rc::new(Vec::new()),
                                    None);
            }
        }
    }

    fn expr_bin(&mut self, expr: &'ast ExprBinType) {
        self.visit_expr(&expr.lhs);
        self.visit_expr(&expr.rhs);

        let lhs_ty = self.ty(expr.lhs.id());
        let rhs_ty = self.ty(expr.rhs.id());

        if expr.op == BinOp::Cmp(CmpOp::Is) || expr.op == BinOp::Cmp(CmpOp::IsNot) {
            self.reserve_temp_for_node_with_type(expr.lhs.id(), BuiltinType::Ptr);

        } else if expr.op == BinOp::Or || expr.op == BinOp::And {
            // no temporaries needed

        } else if self.is_intrinsic(expr.id) {
            self.reserve_temp_for_node(&expr.lhs);

        } else {
            let args = vec![Arg::Expr(&expr.lhs, lhs_ty, 0), Arg::Expr(&expr.rhs, rhs_ty, 0)];
            let fid = self.src.map_calls.get(expr.id).unwrap().fct_id();

            self.universal_call(expr.id,
                                args,
                                Some(fid),
                                Rc::new(Vec::new()),
                                Rc::new(Vec::new()),
                                Some(BuiltinType::Bool));
        }
    }

    fn expr_un(&mut self, expr: &'ast ExprUnType) {
        self.visit_expr(&expr.opnd);
        let opnd = self.ty(expr.opnd.id());

        if self.is_intrinsic(expr.id) {
            // no temporaries needed

        } else {
            let args = vec![Arg::Expr(&expr.opnd, opnd, 0)];
            let fid = self.src.map_calls.get(expr.id).unwrap().fct_id();

            self.universal_call(expr.id,
                                args,
                                Some(fid),
                                Rc::new(Vec::new()),
                                Rc::new(Vec::new()),
                                Some(BuiltinType::Bool));
        }
    }

    fn reserve_temp_for_node(&mut self, expr: &Expr) -> i32 {
        let ty = self.ty(expr.id());
        self.reserve_temp_for_node_with_type(expr.id(), ty)
    }

    fn reserve_temp_for_ctor(&mut self, id: NodeId) -> i32 {
        self.reserve_temp_for_node_with_type(id, BuiltinType::Ptr)
    }

    fn reserve_temp_for_node_with_type(&mut self, id: NodeId, ty: BuiltinType) -> i32 {
        let ty_size = ty.size(self.ctxt);
        self.cur_tempsize = mem::align_i32(self.cur_tempsize + ty_size, ty_size);

        self.jit_info
            .map_stores
            .insert_or_replace(id, Store::Temp(self.cur_tempsize, ty));

        self.cur_tempsize
    }

    fn ty(&self, id: NodeId) -> BuiltinType {
        let ty = self.src.ty(id);
        self.specialize_type(ty)
    }

    fn specialize_type(&self, ty: BuiltinType) -> BuiltinType {
        match ty {
            BuiltinType::ClassTypeParam(cls_id, id) => {
                debug_assert!(self.fct.parent == FctParent::Class(cls_id));
                self.cls_type_params[id.idx()]
            }

            BuiltinType::FctTypeParam(fct_id, id) => {
                debug_assert!(self.fct.id == fct_id);
                self.fct_type_params[id.idx()]
            }

            BuiltinType::Generic(type_id) => {
                let ty = self.ctxt.types.borrow().get(type_id);

                let params: Vec<_> = ty.params
                    .iter()
                    .map(|&t| self.specialize_type(t))
                    .collect();

                let type_id = self.ctxt
                    .types
                    .borrow_mut()
                    .insert(ty.cls_id, Rc::new(params));

                BuiltinType::Generic(type_id)
            }

            BuiltinType::Lambda(_) => unimplemented!(),

            _ => ty,
        }
    }
}

fn specialize_type(ctxt: &SemContext,
                   ty: BuiltinType,
                   cls_type_params: &[BuiltinType],
                   fct_type_params: &[BuiltinType])
                   -> BuiltinType {
    match ty {
        BuiltinType::ClassTypeParam(_, id) => cls_type_params[id.idx()],

        BuiltinType::FctTypeParam(_, id) => fct_type_params[id.idx()],

        BuiltinType::Generic(type_id) => {
            let ty = ctxt.types.borrow().get(type_id);

            let params: Vec<_> = ty.params
                .iter()
                .map(|&t| specialize_type(ctxt, t, cls_type_params, fct_type_params))
                .collect();

            let type_id = ctxt.types
                .borrow_mut()
                .insert(ty.cls_id, Rc::new(params));

            BuiltinType::Generic(type_id)
        }

        BuiltinType::Lambda(_) => unimplemented!(),

        _ => ty,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use ctxt::*;
    use os;
    use test;

    fn info<F>(code: &'static str, f: F)
        where F: FnOnce(&FctSrc, &JitInfo)
    {
        os::init_page_size();

        test::parse(code, |ctxt| {
            let fid = ctxt.fct_by_name("f").unwrap();
            let fct = ctxt.fcts[fid].borrow();
            let src = fct.src();
            let mut src = src.borrow_mut();
            let mut jit_info = JitInfo::new();

            generate(ctxt, &fct, &mut src, &mut jit_info, &[], &[]);

            f(&src, &jit_info);
        });
    }

    #[test]
    fn test_tempsize() {
        info("fun f() { 1+2*3; }", |_, jit_info| {
            assert_eq!(8, jit_info.tempsize);
        });
        info("fun f() { 2*3+4+5; }", |_, jit_info| {
            assert_eq!(12, jit_info.tempsize);
        });
        info("fun f() { 1+(2+(3+4)); }", |_, jit_info| {
            assert_eq!(12, jit_info.tempsize);
        })
    }

    #[test]
    fn test_tempsize_for_fct_call() {
        info("fun f() { g(1,2,3,4,5,6); }
              fun g(a:int, b:int, c:int, d:int, e:int, f:int) {}",
             |_, jit_info| {
                 assert_eq!(24, jit_info.tempsize);
             });

        info("fun f() { g(1,2,3,4,5,6,7,8); }
              fun g(a:int, b:int, c:int, d:int, e:int, f:int, g:int, h:int) {}",
             |_, jit_info| {
                 assert_eq!(32, jit_info.tempsize);
             });

        info("fun f() { g(1,2,3,4,5,6,7,8)+(1+2); }
              fun g(a:int, b:int, c:int, d:int, e:int, f:int, g:int, h:int) -> int {
                  return 0;
              }",
             |_, jit_info| {
                 assert_eq!(40, jit_info.tempsize);
             });
    }

    #[test]
    fn test_invocation_flag() {
        info("fun f() { g(); } fun g() { }", |_, jit_info| {
            assert!(!jit_info.leaf);
        });

        info("fun f() { }", |_, jit_info| {
            assert!(jit_info.leaf);
        });
    }

    #[test]
    fn test_param_offset() {
        info("fun f(a: bool, b: int) { let c = 1; }", |fct, jit_info| {
            assert_eq!(12, jit_info.localsize);

            for (var, offset) in fct.vars.iter().zip(&[-1, -8, -12]) {
                assert_eq!(*offset, jit_info.offset(var.id));
            }
        });
    }

    #[test]
    #[cfg(target_arch = "x86_64")]
    fn test_params_over_6_offset() {
        info("fun f(a: int, b: int, c: int, d: int,
                   e: int, f: int, g: int, h: int) {
                  let i : int = 1;
              }",
             |fct, jit_info| {
                 assert_eq!(28, jit_info.localsize);
                 let offsets = [-4, -8, -12, -16, -20, -24, 16, 24, -28];

                 for (var, offset) in fct.vars.iter().zip(&offsets) {
                     assert_eq!(*offset, jit_info.offset(var.id));
                 }
             });
    }

    #[test]
    #[cfg(target_arch = "aarch64")]
    fn test_params_over_8_offset() {
        info("fun f(a: int, b: int, c: int, d: int,
                   e: int, f: int, g: int, h: int,
                   i: int, j: int) {
                  let k : int = 1;
              }",
             |fct, jit_info| {
                 assert_eq!(36, jit_info.localsize);
                 let offsets = [-4, -8, -12, -16, -20, -24, -28, -32, 16, 24, -36];

                 for (var, offset) in fct.vars.iter().zip(&offsets) {
                     assert_eq!(*offset, jit_info.offset(var.id));
                 }
             });
    }

    #[test]
    fn test_var_offset() {
        info("fun f() { let a = true; let b = false; let c = 2; let d = \"abc\"; }",
             |fct, jit_info| {
                 assert_eq!(16, jit_info.localsize);

                 for (var, offset) in fct.vars.iter().zip(&[-1, -2, -8, -16]) {
                     assert_eq!(*offset, jit_info.offset(var.id));
                 }
             });
    }
}
