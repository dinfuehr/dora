use std::cmp::max;
use std::collections::HashMap;

use class::TypeParams;
use cpu::*;
use ctxt::{
    Arg, CallSite, CallType, Fct, FctId, FctKind, FctParent, FctSrc, Intrinsic, NodeMap, Store,
    TraitId, VarId,
};
use ctxt::VM;
use dora_parser::ast::visit::*;
use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;
use mem;
use ty::BuiltinType;

pub fn generate<'a, 'ast: 'a>(
    vm: &'a VM<'ast>,
    fct: &Fct<'ast>,
    src: &'a FctSrc,
    jit_info: &'a mut JitInfo<'ast>,
    cls_type_params: &TypeParams,
    fct_type_params: &TypeParams,
) {
    let start = if fct.has_self() { 1 } else { 0 };

    let mut ig = InfoGenerator {
        vm: vm,
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
    pub tempsize: i32,                // size of temporary variables on stack
    pub localsize: i32,               // size of local variables on stack
    pub argsize: i32,                 // size of arguments on stack (need to be on bottom)
    pub leaf: bool,                   // false if fct calls other functions
    pub eh_return_value: Option<i32>, // stack slot for return value storage

    pub map_stores: NodeMap<Store>,
    pub map_csites: NodeMap<CallSite<'ast>>,
    pub map_offsets: NodeMap<i32>,
    pub map_var_offsets: HashMap<VarId, i32>,
    pub map_var_types: HashMap<VarId, BuiltinType>,
    pub map_intrinsics: NodeMap<Intrinsic>,
    pub map_fors: NodeMap<ForInfo<'ast>>,
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
        *self
            .map_var_offsets
            .get(&var_id)
            .expect("no offset found for var")
    }

    pub fn ty(&self, var_id: VarId) -> BuiltinType {
        *self
            .map_var_types
            .get(&var_id)
            .expect("no type found for var")
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
            map_intrinsics: NodeMap::new(),
            map_fors: NodeMap::new(),
        }
    }
}

struct InfoGenerator<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
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

    cls_type_params: &'a TypeParams,
    fct_type_params: &'a TypeParams,
}

impl<'a, 'ast> Visitor<'ast> for InfoGenerator<'a, 'ast> {
    fn visit_param(&mut self, p: &'ast Param) {
        let var = *self.src.map_vars.get(p.id).unwrap();
        let ty = self.src.vars[var].ty;
        let ty = self.specialize_type(ty);
        self.jit_info.map_var_types.insert(var, ty);

        let is_float = ty.is_float();

        // only some parameters are passed in registers
        // these registers need to be stored into local variables
        if is_float && self.param_freg_idx < FREG_PARAMS.len() {
            self.reserve_stack_for_var(var);
            self.param_freg_idx += 1;
        } else if !is_float && self.param_reg_idx < REG_PARAMS.len() {
            self.reserve_stack_for_var(var);
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
        match s {
            &StmtVar(ref var) => {
                let var = *self.src.map_vars.get(var.id).unwrap();
                self.reserve_stack_for_var(var);
            }

            &StmtDo(ref try) => {
                self.reserve_stmt_do(try);
            }

            &StmtFor(ref sfor) => {
                self.reserve_stmt_for(sfor);
            }

            _ => {}
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
            ExprLitStruct(ref expr) => self.expr_lit_struct(expr),

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

    fn reserve_stmt_do(&mut self, try: &'ast StmtDoType) {
        let ret = self.fct.return_type;

        if !ret.is_unit() {
            self.eh_return_value = Some(
                self.eh_return_value
                    .unwrap_or_else(|| self.reserve_stack_for_type(ret)),
            );
        }

        // we also need space for catch block parameters
        for catch in &try.catch_blocks {
            let var = *self.src.map_vars.get(catch.id).unwrap();
            self.reserve_stack_for_var(var);
        }

        if try.finally_block.is_some() {
            let offset = self.reserve_stack_for_type(BuiltinType::Ptr);
            self.jit_info.map_offsets.insert(try.id, offset);
        }
    }

    fn reserve_stmt_for(&mut self, stmt: &'ast StmtForType) {
        let for_type_info = self.src.map_fors.get(stmt.id).unwrap();

        // reserve stack slot for iterated value
        let var = *self.src.map_vars.get(stmt.id).unwrap();
        self.reserve_stack_for_var(var);

        // reserve stack slot for iterator
        let offset = self.reserve_stack_for_type(for_type_info.iterator_type);
        self.jit_info.map_offsets.insert(stmt.id, offset);

        // build makeIterator() call
        let object_type = self.ty(stmt.expr.id());
        let ctype = CallType::Method(
            object_type,
            for_type_info.make_iterator,
            TypeParams::empty(),
        );
        let args = vec![Arg::Expr(&stmt.expr, BuiltinType::Unit, 0)];
        let make_iterator = self.build_call_site(&ctype, for_type_info.make_iterator, args);

        // build hasNext() call
        let ctype = CallType::Method(
            for_type_info.iterator_type,
            for_type_info.has_next,
            TypeParams::empty(),
        );
        let args = vec![Arg::Stack(offset, BuiltinType::Unit, 0)];
        let has_next = self.build_call_site(&ctype, for_type_info.has_next, args);

        // build next() call
        let ctype = CallType::Method(
            for_type_info.iterator_type,
            for_type_info.next,
            TypeParams::empty(),
        );
        let args = vec![Arg::Stack(offset, BuiltinType::Unit, 0)];
        let next = self.build_call_site(&ctype, for_type_info.next, args);

        self.jit_info.map_fors.insert(
            stmt.id,
            ForInfo {
                make_iterator: make_iterator,
                has_next: has_next,
                next: next,
            },
        );
    }

    fn reserve_stack_for_self(&mut self) {
        let ty = match self.fct.parent {
            FctParent::Class(clsid) => self.vm.classes[clsid].borrow().ty,

            FctParent::Impl(impl_id) => {
                let ximpl = self.vm.impls[impl_id].read().unwrap();
                self.vm.classes[ximpl.cls_id()].borrow().ty
            }

            _ => unreachable!(),
        };

        let offset = self.reserve_stack_for_type(ty);

        let id = self.src.var_self().id;
        self.jit_info.map_var_offsets.insert(id, offset);
    }

    fn reserve_stack_for_var(&mut self, id: VarId) -> i32 {
        let ty = self.src.vars[id].ty;
        let ty = self.specialize_type(ty);
        let offset = self.reserve_stack_for_type(ty);

        self.jit_info.map_var_offsets.insert(id, offset);
        self.jit_info.map_var_types.insert(id, ty);

        offset
    }

    fn reserve_stack_for_type(&mut self, ty: BuiltinType) -> i32 {
        let ty_size = ty.size(self.vm);
        self.localsize = mem::align_i32(self.localsize + ty_size, ty_size);

        -self.localsize
    }

    fn expr_array(&mut self, expr: &'ast ExprArrayType) {
        if let Some(intrinsic) = self.get_intrinsic(expr.id) {
            self.visit_expr(&expr.object);
            self.visit_expr(&expr.index);

            self.reserve_temp_for_node(&expr.object);
            self.jit_info.map_intrinsics.insert(expr.id, intrinsic);
        } else {
            let args = vec![
                Arg::Expr(&expr.object, BuiltinType::Unit, 0),
                Arg::Expr(&expr.index, BuiltinType::Unit, 0),
            ];

            self.universal_call(expr.id, args, None);
        }
    }

    fn expr_conv(&mut self, e: &'ast ExprConvType) {
        self.visit_expr(&e.object);
        let is_valid = self.src.map_convs.get(e.id).unwrap().valid;

        if !e.is && !is_valid {
            self.reserve_temp_for_node(&e.object);
        }
    }

    fn expr_lit_struct(&mut self, e: &'ast ExprLitStructType) {
        self.reserve_temp_for_node_id(e.id);

        for arg in &e.args {
            self.visit_expr(&arg.expr);
        }
    }

    fn get_intrinsic(&self, id: NodeId) -> Option<Intrinsic> {
        let fid = self.src.map_calls.get(id).unwrap().fct_id();

        // the function we compile right now is never an intrinsic
        if self.fct.id == fid {
            return None;
        }

        let fct = self.vm.fcts[fid].borrow();

        match fct.kind {
            FctKind::Builtin(intr) => Some(intr),
            _ => None,
        }
    }

    fn expr_call(&mut self, expr: &'ast ExprCallType) {
        if let Some(intrinsic) = self.get_intrinsic(expr.id) {
            self.reserve_args(expr);
            self.jit_info.map_intrinsics.insert(expr.id, intrinsic);
            return;
        }

        let call_type = self.src.map_calls.get(expr.id).unwrap().clone();

        let mut args = expr
            .args
            .iter()
            .map(|arg| Arg::Expr(arg, BuiltinType::Unit, 0))
            .collect::<Vec<_>>();

        let fct_id: FctId;

        match *call_type {
            CallType::Ctor(_, fid, _) | CallType::CtorNew(_, fid, _) => {
                let ty = self.ty(expr.id);
                let arg = if call_type.is_ctor() {
                    Arg::Selfie(ty, 0)
                } else {
                    Arg::SelfieNew(ty, 0)
                };

                args.insert(0, arg);

                fct_id = fid;
            }

            CallType::Method(_, fid, _) => {
                let object = expr.object.as_ref().unwrap();
                args.insert(0, Arg::Expr(object, BuiltinType::Unit, 0));

                fct_id = fid;
            }

            CallType::Fct(fid, _, _) => {
                fct_id = fid;
            }
        }

        let fct = self.vm.fcts[fct_id].borrow();

        let callee_id = if fct.kind.is_definition() {
            let trait_id = fct.trait_id();
            let object_type = match *call_type {
                CallType::Method(ty, _, _) => ty,
                _ => unreachable!(),
            };

            let object_type = self.specialize_type(object_type);

            self.find_trait_impl(fct_id, trait_id, object_type)
        } else {
            fct_id
        };

        let callee = self.vm.fcts[callee_id].borrow();

        if let FctKind::Builtin(intrinsic) = callee.kind {
            self.reserve_args(expr);
            self.jit_info.map_intrinsics.insert(expr.id, intrinsic);
            return;
        }

        self.universal_call(expr.id, args, Some(callee_id));
    }

    fn reserve_args(&mut self, expr: &'ast ExprCallType) {
        for arg in &expr.args {
            self.visit_expr(arg);
            self.reserve_temp_for_node(arg);
        }

        if let Some(ref object) = expr.object {
            self.visit_expr(object);
            self.reserve_temp_for_node(object);
        }
    }

    fn find_trait_impl(&self, fct_id: FctId, trait_id: TraitId, object_type: BuiltinType) -> FctId {
        let cls_id = object_type.cls_id(self.vm).unwrap();
        let cls = self.vm.classes[cls_id].borrow();

        for &impl_id in &cls.impls {
            let ximpl = self.vm.impls[impl_id].read().unwrap();

            if ximpl.trait_id() != trait_id {
                continue;
            }

            for &mtd_id in &ximpl.methods {
                let mtd = self.vm.fcts[mtd_id].borrow();

                if mtd.impl_for == Some(fct_id) {
                    return mtd_id;
                }
            }
        }

        panic!("no impl found for generic trait call")
    }

    fn expr_delegation(&mut self, expr: &'ast ExprDelegationType) {
        let mut args = expr
            .args
            .iter()
            .map(|arg| Arg::Expr(arg, BuiltinType::Unit, 0))
            .collect::<Vec<_>>();

        let cls = self.ty(expr.id);
        args.insert(0, Arg::Selfie(cls, 0));

        self.universal_call(expr.id, args, None);
    }

    fn universal_call(&mut self, id: NodeId, args: Vec<Arg<'ast>>, callee_id: Option<FctId>) {
        let call_type = self.src.map_calls.get(id).unwrap().clone();

        let callee_id = if let Some(callee_id) = callee_id {
            callee_id
        } else {
            call_type.fct_id()
        };

        let csite = self.build_call_site(&*call_type, callee_id, args);

        // remember args
        self.jit_info.map_csites.insert_or_replace(id, csite);
    }

    fn build_call_site(
        &mut self,
        call_type: &CallType,
        callee_id: FctId,
        args: Vec<Arg<'ast>>,
    ) -> CallSite<'ast> {
        // function invokes another function
        self.leaf = false;

        let callee = self.vm.fcts[callee_id].borrow();

        let (args, return_type, super_call) =
            self.determine_call_args_and_types(&*call_type, &*callee, args);
        let (cls_type_params, fct_type_params) = self.determine_call_type_params(&*call_type);

        self.determine_call_stack(&args);

        CallSite {
            callee: callee_id,
            args: args,
            cls_type_params: cls_type_params,
            fct_type_params: fct_type_params,
            super_call: super_call,
            return_type: return_type,
        }
    }

    fn determine_call_args_and_types(
        &mut self,
        call_type: &CallType,
        callee: &Fct<'ast>,
        args: Vec<Arg<'ast>>,
    ) -> (Vec<Arg<'ast>>, BuiltinType, bool) {
        let mut super_call = false;

        assert!(callee.params_with_self().len() == args.len());

        let args = args
            .iter()
            .enumerate()
            .map(|(ind, arg)| {
                let ty = callee.params_with_self()[ind];
                let ty = self.specialize_type_for_call(call_type, ty);
                let offset = self.reserve_temp_for_type(ty);

                match *arg {
                    Arg::Expr(ast, _, _) => {
                        if ind == 0 && ast.is_super() {
                            super_call = true;
                        }

                        Arg::Expr(ast, ty, offset)
                    }

                    Arg::Stack(soffset, _, _) => Arg::Stack(soffset, ty, offset),
                    Arg::SelfieNew(cid, _) => Arg::SelfieNew(cid, offset),
                    Arg::Selfie(cid, _) => Arg::Selfie(cid, offset),
                }
            })
            .collect::<Vec<_>>();

        let return_type = self.specialize_type_for_call(call_type, callee.return_type);

        (args, return_type, super_call)
    }

    fn determine_call_type_params(&mut self, call_type: &CallType) -> (TypeParams, TypeParams) {
        let cls_type_params;
        let fct_type_params;

        match *call_type {
            CallType::Ctor(_, _, ref type_params) | CallType::CtorNew(_, _, ref type_params) => {
                cls_type_params = type_params.clone();
                fct_type_params = TypeParams::empty();
            }

            CallType::Method(ty, _, ref type_params) => {
                let ty = self.specialize_type(ty);

                cls_type_params = ty.type_params(self.vm);
                fct_type_params = type_params.clone();
            }

            CallType::Fct(_, ref cls_tps, ref fct_tps) => {
                cls_type_params = cls_tps.clone();
                fct_type_params = fct_tps.clone();
            }
        }

        (cls_type_params, fct_type_params)
    }

    fn determine_call_stack(&mut self, args: &[Arg<'ast>]) {
        let mut reg_args: i32 = 0;
        let mut freg_args: i32 = 0;

        for arg in args {
            match *arg {
                Arg::Expr(ast, ty, _) => {
                    self.visit_expr(ast);

                    if ty.is_float() {
                        freg_args += 1;
                    } else {
                        reg_args += 1;
                    }
                }

                Arg::Stack(_, ty, _) | Arg::Selfie(ty, _) | Arg::SelfieNew(ty, _) => {
                    if ty.is_float() {
                        freg_args += 1;
                    } else {
                        reg_args += 1;
                    }
                }
            }
        }

        // some register are reserved on stack
        let args_on_stack = max(0, reg_args - REG_PARAMS.len() as i32)
            + max(0, freg_args - FREG_PARAMS.len() as i32);

        let argsize = 8 * args_on_stack;

        if argsize > self.argsize {
            self.argsize = argsize;
        }
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

            if let Some(intrinsic) = self.get_intrinsic(e.id) {
                self.visit_expr(&array.object);
                self.visit_expr(&array.index);
                self.visit_expr(&e.rhs);

                self.reserve_temp_for_node(&array.object);
                self.reserve_temp_for_node(&array.index);

                let element_type = self.ty(array.id);
                self.reserve_temp_for_node_with_type(e.rhs.id(), element_type);

                self.jit_info.map_intrinsics.insert(e.id, intrinsic);
            } else {
                let args = vec![
                    Arg::Expr(&array.object, BuiltinType::Unit, 0),
                    Arg::Expr(&array.index, BuiltinType::Unit, 0),
                    Arg::Expr(&e.rhs, BuiltinType::Unit, 0),
                ];

                self.universal_call(e.id, args, None);
            }
        }
    }

    fn expr_bin(&mut self, expr: &'ast ExprBinType) {
        let lhs_ty = self.ty(expr.lhs.id());
        let rhs_ty = self.ty(expr.rhs.id());

        if expr.op == BinOp::Cmp(CmpOp::Is) || expr.op == BinOp::Cmp(CmpOp::IsNot) {
            self.visit_expr(&expr.lhs);
            self.visit_expr(&expr.rhs);

            self.reserve_temp_for_node_with_type(expr.lhs.id(), BuiltinType::Ptr);
        } else if expr.op == BinOp::Or || expr.op == BinOp::And {
            self.visit_expr(&expr.lhs);
            self.visit_expr(&expr.rhs);

        // no temporaries needed
        } else if let Some(intrinsic) = self.get_intrinsic(expr.id) {
            self.visit_expr(&expr.lhs);
            self.visit_expr(&expr.rhs);

            self.reserve_temp_for_node(&expr.lhs);
            self.jit_info.map_intrinsics.insert(expr.id, intrinsic);
        } else {
            let args = vec![
                Arg::Expr(&expr.lhs, lhs_ty, 0),
                Arg::Expr(&expr.rhs, rhs_ty, 0),
            ];
            let fid = self.src.map_calls.get(expr.id).unwrap().fct_id();

            self.universal_call(expr.id, args, Some(fid));
        }
    }

    fn expr_un(&mut self, expr: &'ast ExprUnType) {
        if let Some(intrinsic) = self.get_intrinsic(expr.id) {
            // no temporaries needed
            self.visit_expr(&expr.opnd);
            self.jit_info.map_intrinsics.insert(expr.id, intrinsic);
        } else {
            let args = vec![Arg::Expr(&expr.opnd, BuiltinType::Unit, 0)];

            self.universal_call(expr.id, args, None);
        }
    }

    fn reserve_temp_for_node_id(&mut self, id: NodeId) -> i32 {
        let ty = self.ty(id);
        self.reserve_temp_for_node_with_type(id, ty)
    }

    fn reserve_temp_for_node(&mut self, expr: &Expr) -> i32 {
        let ty = self.ty(expr.id());
        self.reserve_temp_for_node_with_type(expr.id(), ty)
    }

    fn reserve_temp_for_ctor(&mut self, id: NodeId) -> i32 {
        self.reserve_temp_for_node_with_type(id, BuiltinType::Ptr)
    }

    fn reserve_temp_for_node_with_type(&mut self, id: NodeId, ty: BuiltinType) -> i32 {
        let offset = self.reserve_temp_for_type(ty);

        self.jit_info
            .map_stores
            .insert_or_replace(id, Store::Temp(offset, ty));

        offset
    }

    fn reserve_temp_for_type(&mut self, ty: BuiltinType) -> i32 {
        let ty_size = ty.size(self.vm);
        self.cur_tempsize = mem::align_i32(self.cur_tempsize + ty_size, ty_size);

        self.cur_tempsize
    }

    fn ty(&self, id: NodeId) -> BuiltinType {
        let ty = self.src.ty(id);
        self.specialize_type(ty)
    }

    fn specialize_type_for_call(&self, call_type: &CallType, ty: BuiltinType) -> BuiltinType {
        let ty = match *call_type {
            CallType::Fct(_, ref cls_type_params, ref fct_type_params) => {
                specialize_type(self.vm, ty, cls_type_params, fct_type_params)
            }

            CallType::Method(cls_ty, _, ref type_params) => match cls_ty {
                BuiltinType::Class(_, list_id) => {
                    let params = self.vm.lists.borrow().get(list_id);
                    specialize_type(self.vm, ty, &params, type_params)
                }

                _ => ty,
            },

            CallType::Ctor(_, _, ref type_params) | CallType::CtorNew(_, _, ref type_params) => {
                let empty = TypeParams::empty();
                specialize_type(self.vm, ty, type_params, &empty)
            }
        };

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

            BuiltinType::Class(cls_id, list_id) => {
                let params = self.vm.lists.borrow().get(list_id);
                let params: Vec<_> = params.iter().map(|t| self.specialize_type(t)).collect();

                let list_id = self.vm.lists.borrow_mut().insert(params.into());

                BuiltinType::Class(cls_id, list_id)
            }

            BuiltinType::Lambda(_) => unimplemented!(),

            _ => ty,
        }
    }
}

fn specialize_type(
    vm: &VM,
    ty: BuiltinType,
    cls_type_params: &TypeParams,
    fct_type_params: &TypeParams,
) -> BuiltinType {
    match ty {
        BuiltinType::ClassTypeParam(_, id) => cls_type_params[id.idx()],

        BuiltinType::FctTypeParam(_, id) => fct_type_params[id.idx()],

        BuiltinType::Class(cls_id, list_id) => {
            let params = vm.lists.borrow().get(list_id);

            let params: Vec<_> = params
                .iter()
                .map(|t| specialize_type(vm, t, cls_type_params, fct_type_params))
                .collect();

            let list_id = vm.lists.borrow_mut().insert(params.into());

            BuiltinType::Class(cls_id, list_id)
        }

        BuiltinType::Lambda(_) => unimplemented!(),

        _ => ty,
    }
}

#[derive(Clone)]
pub struct ForInfo<'ast> {
    pub make_iterator: CallSite<'ast>,
    pub has_next: CallSite<'ast>,
    pub next: CallSite<'ast>,
}

#[cfg(test)]
mod tests {
    use super::*;

    use ctxt::*;
    use os;
    use test;

    fn info<F>(code: &'static str, f: F)
    where
        F: FnOnce(&FctSrc, &JitInfo),
    {
        os::init_page_size();

        test::parse(code, |vm| {
            let fid = vm.fct_by_name("f").unwrap();
            let fct = vm.fcts[fid].borrow();
            let src = fct.src();
            let mut src = src.borrow_mut();
            let mut jit_info = JitInfo::new();
            let empty = TypeParams::empty();

            generate(vm, &fct, &mut src, &mut jit_info, &empty, &empty);

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
        info(
            "fun f() { g(1,2,3,4,5,6); }
              fun g(a:int, b:int, c:int, d:int, e:int, f:int) {}",
            |_, jit_info| {
                assert_eq!(24, jit_info.tempsize);
            },
        );

        info(
            "fun f() { g(1,2,3,4,5,6,7,8); }
              fun g(a:int, b:int, c:int, d:int, e:int, f:int, g:int, h:int) {}",
            |_, jit_info| {
                assert_eq!(32, jit_info.tempsize);
            },
        );

        info(
            "fun f() { g(1,2,3,4,5,6,7,8)+(1+2); }
              fun g(a:int, b:int, c:int, d:int, e:int, f:int, g:int, h:int) -> int {
                  return 0;
              }",
            |_, jit_info| {
                assert_eq!(40, jit_info.tempsize);
            },
        );
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
        info(
            "fun f(a: int, b: int, c: int, d: int,
                   e: int, f: int, g: int, h: int) {
                  let i : int = 1;
              }",
            |fct, jit_info| {
                assert_eq!(28, jit_info.localsize);
                let offsets = [-4, -8, -12, -16, -20, -24, 16, 24, -28];

                for (var, offset) in fct.vars.iter().zip(&offsets) {
                    assert_eq!(*offset, jit_info.offset(var.id));
                }
            },
        );
    }

    #[test]
    #[cfg(target_arch = "aarch64")]
    fn test_params_over_8_offset() {
        info(
            "fun f(a: int, b: int, c: int, d: int,
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
            },
        );
    }

    #[test]
    fn test_var_offset() {
        info(
            "fun f() { let a = true; let b = false; let c = 2; let d = \"abc\"; }",
            |fct, jit_info| {
                assert_eq!(16, jit_info.localsize);

                for (var, offset) in fct.vars.iter().zip(&[-1, -2, -8, -16]) {
                    assert_eq!(*offset, jit_info.offset(var.id));
                }
            },
        );
    }
}
