use std::cmp::max;
use std::collections::HashMap;

use dora_parser::ast::visit::*;
use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;
use dora_parser::lexer::position::Position;

use crate::baseline::{
    Arg, CallSite, ExprStore, InternalArg, ManagedStackFrame, ManagedStackSlot,
    VariadicArrayDescriptor,
};
use crate::compiler::asm::BaselineAssembler;
use crate::compiler::codegen::{ensure_native_stub, should_emit_debug, AllocationSize, AnyReg};
use crate::compiler::fct::{Code, GcPoint, JitDescriptor};
use crate::compiler::native_stub::{NativeFct, NativeFctDescriptor};
use crate::cpu::{
    next_param_offset, FReg, Mem, Reg, FREG_PARAMS, FREG_RESULT, FREG_TMP1, PARAM_OFFSET,
    REG_PARAMS, REG_RESULT, REG_SP, REG_TMP1, REG_TMP2,
};
use crate::gc::Address;
use crate::masm::*;
use crate::mem;
use crate::object::{offset_of_array_data, Header, Str};
use crate::semck::specialize::{replace_type_param, specialize_class_ty, specialize_for_call_type};
use crate::size::InstanceSize;
use crate::ty::{BuiltinType, MachineMode, TypeList, TypeParamId};
use crate::vm::{
    CallType, ClassDefId, ConstId, Fct, FctId, FctKind, FctSrc, FieldId, IdentType, Intrinsic,
    TraitId, Trap, TupleId, VarId, VM,
};
use crate::vtable::{VTable, DISPLAY_SIZE};

pub(super) fn generate<'a, 'ast: 'a>(
    vm: &'a VM<'ast>,
    fct: &Fct<'ast>,
    src: &'a FctSrc,
    cls_type_params: &TypeList,
    fct_type_params: &TypeList,
) -> Code {
    AstCodeGen {
        vm,
        fct: &fct,
        ast: fct.ast,
        asm: BaselineAssembler::new(vm),
        src,

        lbl_break: None,
        lbl_continue: None,

        stacksize_offset: 0,
        managed_stack: ManagedStackFrame::new(),
        var_to_slot: HashMap::new(),
        var_to_offset: HashMap::new(),
        return_value: None,

        cls_type_params,
        fct_type_params,
    }
    .generate()
}

struct AstCodeGen<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    fct: &'a Fct<'ast>,
    ast: &'ast Function,
    asm: BaselineAssembler<'a, 'ast>,
    src: &'a FctSrc,

    lbl_break: Option<Label>,
    lbl_continue: Option<Label>,

    managed_stack: ManagedStackFrame,
    stacksize_offset: usize,

    // Used in case return value doesn't fit into single register. Stores
    // address of return value in caller frame.
    return_value: Option<ManagedStackSlot>,

    var_to_slot: HashMap<VarId, ManagedStackSlot>,
    var_to_offset: HashMap<VarId, i32>,

    cls_type_params: &'a TypeList,
    fct_type_params: &'a TypeList,
}

impl<'a, 'ast> AstCodeGen<'a, 'ast>
where
    'ast: 'a,
{
    fn generate(mut self) -> Code {
        if should_emit_debug(self.vm, self.fct) {
            self.asm.debug();
        }

        self.managed_stack.push_scope();
        self.emit_prolog();
        self.store_register_params_on_stack();
        self.emit_stack_guard();

        let always_returns = self.src.always_returns;

        {
            let block = self.ast.block();

            for stmt in &block.stmts {
                self.visit_stmt(stmt);
            }

            if let Some(ref value) = block.expr {
                let return_type = self.specialize_type(self.fct.return_type);
                let dest = self.alloc_expr_store(return_type);
                self.emit_expr(value, dest);

                if !always_returns {
                    if let Some(tuple_id) = return_type.tuple_id() {
                        self.asm.load_mem(
                            MachineMode::Ptr,
                            REG_TMP1.into(),
                            Mem::Local(self.return_value.unwrap().offset),
                        );
                        self.copy_tuple(
                            tuple_id,
                            RegOrOffset::Reg(REG_TMP1),
                            RegOrOffset::Offset(dest.stack_offset()),
                        );
                    }

                    self.emit_epilog();
                }

                self.free_expr_store(dest);
            }
        }

        self.managed_stack.pop_scope(self.vm);
        assert!(self.managed_stack.is_empty());

        if !always_returns {
            self.emit_epilog();
        }

        self.asm
            .patch_stacksize(self.stacksize_offset, self.managed_stack.stacksize());

        let jit_fct = self.asm.jit(
            self.managed_stack.stacksize(),
            JitDescriptor::DoraFct(self.fct.id),
        );

        jit_fct
    }

    fn store_register_params_on_stack(&mut self) {
        let mut reg_idx = 0;
        let mut freg_idx = 0;

        let return_type = self.specialize_type(self.fct.return_type);

        if return_type.is_tuple() {
            let slot = self.managed_stack.add_scope(BuiltinType::Ptr, self.vm);
            self.asm.store_mem(
                MachineMode::Ptr,
                Mem::Local(slot.offset()),
                REG_PARAMS[reg_idx].into(),
            );
            self.return_value = Some(slot);
            reg_idx += 1;
        }

        if self.fct.has_self() {
            let var = self.src.var_self();
            let mode = var.ty.mode();

            self.asm.emit_comment("store param self".into());

            let dest = if mode.is_float() {
                FREG_PARAMS[freg_idx].into()
            } else {
                REG_PARAMS[reg_idx].into()
            };

            let slot_param = self.managed_stack.add_scope(var.ty, self.vm);
            assert!(self.var_to_slot.insert(var.id, slot_param).is_none());

            self.asm
                .store_mem(mode, Mem::Local(slot_param.offset()), dest);

            if mode.is_float() {
                freg_idx += 1;
            } else {
                reg_idx += 1;
            }
        }

        let mut param_offset = PARAM_OFFSET;

        for p in &self.ast.params {
            let varid = *self.src.map_vars.get(p.id).unwrap();
            let ty = self.var_ty(varid);

            if ty.is_unit() {
                // nothing
            } else if let Some(tuple_id) = ty.tuple_id() {
                let slot_param = self.managed_stack.add_scope(ty, self.vm);
                assert!(self.var_to_slot.insert(varid, slot_param).is_none());

                {
                    let var = &self.src.vars[varid];
                    let name = self.vm.interner.str(var.name);
                    self.asm.emit_comment(format!("store tuple param {}", name));
                }

                if reg_idx < REG_PARAMS.len() {
                    self.asm.copy(
                        MachineMode::Ptr,
                        REG_TMP1.into(),
                        REG_PARAMS[reg_idx].into(),
                    );
                    self.copy_tuple(
                        tuple_id,
                        RegOrOffset::Offset(self.var_offset(varid)),
                        RegOrOffset::Reg(REG_TMP1),
                    );
                } else {
                    self.asm
                        .load_mem(MachineMode::Ptr, REG_TMP1.into(), Mem::Local(param_offset));
                    self.copy_tuple(
                        tuple_id,
                        RegOrOffset::Offset(self.var_offset(varid)),
                        RegOrOffset::Reg(REG_TMP1),
                    );
                    param_offset = next_param_offset(param_offset, ty);
                }
            } else if ty.is_float() && freg_idx < FREG_PARAMS.len() {
                let reg = FREG_PARAMS[freg_idx];

                let slot_param = self.managed_stack.add_scope(ty, self.vm);
                assert!(self.var_to_slot.insert(varid, slot_param).is_none());

                {
                    let var = &self.src.vars[varid];
                    let name = self.vm.interner.str(var.name);
                    self.asm.emit_comment(format!("store param {}", name));
                }
                self.asm.var_store(self.var_offset(varid), ty, reg.into());

                freg_idx += 1;
            } else if !ty.is_float() && reg_idx < REG_PARAMS.len() {
                let reg = REG_PARAMS[reg_idx];

                let slot_param = self.managed_stack.add_scope(ty, self.vm);
                assert!(self.var_to_slot.insert(varid, slot_param).is_none());

                {
                    let var = &self.src.vars[varid];
                    let name = self.vm.interner.str(var.name);
                    self.asm.emit_comment(format!("store param {}", name));
                }
                self.asm.var_store(self.var_offset(varid), ty, reg.into());

                reg_idx += 1;
            } else {
                // all other parameters are stored on stack, just use
                // that offset
                self.var_to_offset.insert(varid, param_offset);
                param_offset = next_param_offset(param_offset, ty);
            }
        }
    }

    fn emit_prolog(&mut self) {
        self.stacksize_offset = self.asm.prolog();
    }

    fn emit_stack_guard(&mut self) {
        let gcpoint = self.create_gcpoint();
        self.asm.stack_guard(self.fct.pos(), gcpoint);
    }

    fn emit_epilog(&mut self) {
        self.asm.emit_comment("epilog".into());
        self.asm.epilog();
    }

    fn emit_stmt_return(&mut self, s: &'ast StmtReturnType) {
        let return_type = self.specialize_type(self.fct.return_type);

        let mut temp: Option<ManagedStackSlot> = None;

        if let Some(ref expr) = s.expr {
            let dest = self.emit_expr_result_reg(expr);

            if return_type.is_tuple() {
                temp = Some(dest.stack_slot());
            }
        }

        if let Some(tuple_id) = return_type.tuple_id() {
            self.asm.load_mem(
                MachineMode::Ptr,
                REG_TMP1.into(),
                Mem::Local(self.return_value.unwrap().offset),
            );

            self.copy_tuple(
                tuple_id,
                RegOrOffset::Reg(REG_TMP1),
                RegOrOffset::Offset(temp.unwrap().offset),
            );
        }

        if let Some(temp) = temp {
            self.managed_stack.free_temp(temp, self.vm);
        }

        self.emit_epilog();
    }

    fn emit_stmt_while(&mut self, s: &'ast StmtWhileType) {
        let lbl_start = self.asm.create_label();
        let lbl_end = self.asm.create_label();

        self.asm.bind_label(lbl_start);

        if s.cond.is_lit_true() {
            // always true => no condition evaluation
        } else {
            // execute condition, when condition is false jump to
            // end of while
            self.emit_expr_result_reg(&s.cond);
            self.asm
                .test_and_jump_if(CondCode::Zero, REG_RESULT, lbl_end);
        }

        self.save_label_state(lbl_end, lbl_start, |this| {
            // execute while body, then jump back to condition
            this.visit_stmt(&s.block);

            this.emit_stack_guard();
            this.asm.jump(lbl_start);
        });

        self.asm.bind_label(lbl_end);
    }

    fn emit_stmt_for(&mut self, stmt: &'ast StmtForType) {
        if self.src.map_fors.get(stmt.id).is_some() {
            self.emit_stmt_for_iterator(stmt);
        } else {
            self.emit_stmt_for_array(stmt);
        }
    }

    fn emit_stmt_for_array(&mut self, stmt: &'ast StmtForType) {
        self.managed_stack.push_scope();

        let lbl_start = self.asm.create_label();
        let lbl_end = self.asm.create_label();

        let array_slot = self.managed_stack.add_scope(BuiltinType::Ptr, self.vm);
        let index_slot = self.managed_stack.add_scope(BuiltinType::Int, self.vm);
        let length_slot = self.managed_stack.add_scope(BuiltinType::Int, self.vm);

        let for_var_id = *self.src.map_vars.get(stmt.id).unwrap();
        let var_ty = self.var_ty(for_var_id);
        let var_slot = self.managed_stack.add_scope(var_ty, self.vm);
        assert!(self.var_to_slot.insert(for_var_id, var_slot).is_none());

        // evaluate and store array
        self.emit_expr(&stmt.expr, REG_RESULT.into());

        self.asm
            .test_if_nil_bailout(stmt.expr.pos(), REG_RESULT, Trap::NIL);

        self.asm.store_mem(
            MachineMode::Ptr,
            Mem::Local(array_slot.offset()),
            REG_RESULT.into(),
        );

        // store length
        self.asm.load_mem(
            MachineMode::Ptr,
            REG_RESULT.into(),
            Mem::Base(REG_RESULT, Header::size()),
        );

        self.asm.store_mem(
            MachineMode::Int32,
            Mem::Local(length_slot.offset()),
            REG_RESULT.into(),
        );

        // set current index to 0
        self.asm.load_int_const(MachineMode::Int32, REG_RESULT, 0);
        self.asm.store_mem(
            MachineMode::Int32,
            Mem::Local(index_slot.offset()),
            REG_RESULT.into(),
        );

        // test if current index < array length
        self.asm.bind_label(lbl_start);
        self.asm.load_mem(
            MachineMode::Int32,
            REG_TMP1.into(),
            Mem::Local(index_slot.offset()),
        );

        self.asm.load_mem(
            MachineMode::Int32,
            REG_RESULT.into(),
            Mem::Local(length_slot.offset()),
        );

        self.asm.cmp_reg(MachineMode::Int32, REG_TMP1, REG_RESULT);
        self.asm.jump_if(CondCode::GreaterEq, lbl_end);

        // load current array element into variable
        self.asm.load_mem(
            MachineMode::Ptr,
            REG_RESULT.into(),
            Mem::Local(array_slot.offset()),
        );

        if var_ty.is_unit() {
            // nothing to do
        } else if let Some(tuple_id) = var_ty.tuple_id() {
            let element_size = self.vm.tuples.lock().get_tuple(tuple_id).size();
            self.asm
                .array_address(REG_TMP1, REG_RESULT, REG_TMP1, element_size);

            self.copy_tuple(
                tuple_id,
                RegOrOffset::Offset(var_slot.offset()),
                RegOrOffset::Reg(REG_TMP1),
            );
        } else {
            let dest = result_reg_ty(var_ty).any_reg();
            self.asm
                .load_array_elem(var_ty.mode(), dest, REG_RESULT, REG_TMP1);
            self.asm
                .store_mem(var_ty.mode(), Mem::Local(var_slot.offset()), dest);
        }

        self.save_label_state(lbl_end, lbl_start, |this| {
            // execute while body, then jump back to condition
            this.visit_stmt(&stmt.block);

            // increment index by 1
            this.asm.load_mem(
                MachineMode::Int32,
                REG_RESULT.into(),
                Mem::Local(index_slot.offset()),
            );

            this.asm
                .int_add_imm(MachineMode::Int32, REG_RESULT, REG_RESULT, 1);

            this.asm.store_mem(
                MachineMode::Int32,
                Mem::Local(index_slot.offset()),
                REG_RESULT.into(),
            );

            this.emit_stack_guard();
            this.asm.jump(lbl_start);
        });

        self.managed_stack.pop_scope(&self.vm);

        self.asm.bind_label(lbl_end);
    }

    fn emit_stmt_for_iterator(&mut self, stmt: &'ast StmtForType) {
        let for_type_info = self.src.map_fors.get(stmt.id).unwrap().clone();

        self.managed_stack.push_scope();

        // emit: <iterator> = obj.makeIterator()
        let object_type = self.ty(stmt.expr.id());
        let ctype = CallType::Method(object_type, for_type_info.make_iterator, TypeList::empty());
        let args = vec![Arg::Expr(&stmt.expr)];
        let make_iterator = self.build_call_site(&ctype, for_type_info.make_iterator, args);
        let dest = self.emit_call_site_old(&make_iterator, stmt.pos);

        // offset of iterator storage
        let iterator_slot = self
            .managed_stack
            .add_scope(for_type_info.iterator_type, self.vm);
        self.asm.store_mem(
            MachineMode::Ptr,
            Mem::Local(iterator_slot.offset()),
            dest.any_reg(),
        );

        let lbl_start = self.asm.create_label();
        let lbl_end = self.asm.create_label();

        self.asm.bind_label(lbl_start);

        // emit: iterator.hasNext() & jump to lbl_end if false
        let ctype = CallType::Method(
            for_type_info.iterator_type,
            for_type_info.has_next,
            TypeList::empty(),
        );
        let args = vec![Arg::Stack(iterator_slot.offset())];
        let has_next = self.build_call_site(&ctype, for_type_info.has_next, args);
        let dest = self.emit_call_site_old(&has_next, stmt.pos);
        self.asm
            .test_and_jump_if(CondCode::Zero, dest.reg(), lbl_end);

        // emit: <for_var> = iterator.next()
        let ctype = CallType::Method(
            for_type_info.iterator_type,
            for_type_info.next,
            TypeList::empty(),
        );
        let args = vec![Arg::Stack(iterator_slot.offset())];
        let next = self.build_call_site(&ctype, for_type_info.next, args);
        let dest = self.emit_call_site_old(&next, stmt.pos);

        let for_var_id = *self.src.map_vars.get(stmt.id).unwrap();
        let var_ty = self.var_ty(for_var_id);
        let slot_var = self.managed_stack.add_scope(var_ty, self.vm);
        assert!(self.var_to_slot.insert(for_var_id, slot_var).is_none());

        self.asm
            .var_store(slot_var.offset(), var_ty, dest.any_reg());

        self.save_label_state(lbl_end, lbl_start, |this| {
            // execute while body, then jump back to condition
            this.visit_stmt(&stmt.block);

            this.emit_stack_guard();
            this.asm.jump(lbl_start);
        });

        self.managed_stack.pop_scope(&self.vm);

        self.asm.bind_label(lbl_end);
    }

    fn create_gcpoint(&mut self) -> GcPoint {
        self.managed_stack.gcpoint(self.vm)
    }

    fn save_label_state<F>(&mut self, lbl_break: Label, lbl_continue: Label, f: F)
    where
        F: FnOnce(&mut AstCodeGen<'a, 'ast>),
    {
        let old_lbl_break = self.lbl_break;
        let old_lbl_continue = self.lbl_continue;

        self.lbl_break = Some(lbl_break);
        self.lbl_continue = Some(lbl_continue);

        f(self);

        self.lbl_break = old_lbl_break;
        self.lbl_continue = old_lbl_continue;
    }

    fn emit_stmt_break(&mut self, _: &'ast StmtBreakType) {
        // now jump out of loop
        let lbl_break = self.lbl_break.unwrap();
        self.asm.jump(lbl_break);
    }

    fn emit_stmt_continue(&mut self, _: &'ast StmtContinueType) {
        self.emit_stack_guard();

        // now jump to start of loop
        let lbl_continue = self.lbl_continue.unwrap();
        self.asm.jump(lbl_continue);
    }

    fn emit_stmt_expr(&mut self, s: &'ast StmtExprType) {
        let store = self.emit_expr_result_reg(&s.expr);
        self.free_expr_store(store);
    }

    fn emit_stmt_var(&mut self, s: &'ast StmtVarType) {
        let var = *self.src.map_vars.get(s.id).unwrap();
        let ty = self.var_ty(var);

        let value = if let Some(ref expr) = s.expr {
            Some(self.emit_expr_result_reg(expr))
        } else {
            None
        };

        if ty.is_unit() {
            return;
        }

        let slot_var = self.managed_stack.add_scope(ty, self.vm);
        assert!(self.var_to_slot.insert(var, slot_var).is_none());

        if let Some(value) = value {
            if let Some(tuple_id) = ty.tuple_id() {
                self.copy_tuple(
                    tuple_id,
                    RegOrOffset::Offset(self.var_offset(var)),
                    RegOrOffset::Offset(value.stack_offset()),
                );
            } else if ty.is_unit() {
                // do nothing
            } else {
                self.asm
                    .var_store(self.var_offset(var), ty, value.any_reg());
            }
            self.free_expr_store(value);
        } else if ty.reference_type() {
            // uninitialized variables which reference objects need to be initialized to null
            // otherwise the GC can't know if the stored value is a valid pointer
            self.asm.load_nil(REG_RESULT);
            self.asm
                .var_store(self.var_offset(var), ty, REG_RESULT.into());
        }
    }

    fn copy_tuple(&mut self, tuple_id: TupleId, dest: RegOrOffset, src: RegOrOffset) {
        let subtypes = self.vm.tuples.lock().get(tuple_id);
        let offsets = self
            .vm
            .tuples
            .lock()
            .get_tuple(tuple_id)
            .offsets()
            .to_owned();

        for (&subtype, &subtype_offset) in subtypes.iter().zip(&offsets) {
            if let Some(tuple_id) = subtype.tuple_id() {
                let src = match src {
                    RegOrOffset::Reg(reg) => RegOrOffset::RegWithOffset(reg, subtype_offset),
                    RegOrOffset::RegWithOffset(reg, tuple_offset) => {
                        RegOrOffset::RegWithOffset(reg, tuple_offset + subtype_offset)
                    }
                    RegOrOffset::Offset(tuple_offset) => {
                        RegOrOffset::Offset(tuple_offset + subtype_offset)
                    }
                };
                let dest = match dest {
                    RegOrOffset::Reg(reg) => RegOrOffset::RegWithOffset(reg, subtype_offset),
                    RegOrOffset::RegWithOffset(reg, tuple_offset) => {
                        RegOrOffset::RegWithOffset(reg, tuple_offset + subtype_offset)
                    }
                    RegOrOffset::Offset(tuple_offset) => {
                        RegOrOffset::Offset(tuple_offset + subtype_offset)
                    }
                };
                self.copy_tuple(tuple_id, dest, src);
            } else if subtype.is_unit() {
                // nothing
            } else {
                let tmp = result_reg_ty(subtype);
                let mode = subtype.mode();
                let src = match src {
                    RegOrOffset::Reg(reg) => Mem::Base(reg, subtype_offset),
                    RegOrOffset::RegWithOffset(reg, tuple_offset) => {
                        Mem::Base(reg, tuple_offset + subtype_offset)
                    }
                    RegOrOffset::Offset(tuple_offset) => Mem::Local(tuple_offset + subtype_offset),
                };
                self.asm.load_mem(mode, tmp.any_reg(), src);
                let dest = match dest {
                    RegOrOffset::Reg(reg) => Mem::Base(reg, subtype_offset),
                    RegOrOffset::RegWithOffset(reg, tuple_offset) => {
                        Mem::Base(reg, tuple_offset + subtype_offset)
                    }
                    RegOrOffset::Offset(tuple_offset) => Mem::Local(tuple_offset + subtype_offset),
                };
                self.asm.store_mem(mode, dest, tmp.any_reg());
            }
        }
    }

    fn emit_expr_result_reg(&mut self, e: &'ast Expr) -> ExprStore {
        let ty = self
            .src
            .map_tys
            .get(e.id())
            .map(|ty| *ty)
            .expect("no type found");

        let ty = self.specialize_type(ty);
        let dest = self.alloc_expr_store(ty);
        self.emit_expr(e, dest);

        dest
    }

    fn emit_call_site_old(&mut self, call_site: &CallSite<'ast>, pos: Position) -> ExprStore {
        let callee = self.vm.fcts.idx(call_site.callee);
        let callee = callee.read();
        let return_type = self.specialize_type(callee.return_type);
        let dest = result_reg_ty(return_type);

        self.emit_call_site(call_site, pos, dest);

        dest
    }

    fn emit_expr(&mut self, e: &'ast Expr, dest: ExprStore) {
        match *e {
            ExprLitChar(ref expr) => self.emit_lit_char(expr, dest.reg()),
            ExprLitInt(ref expr) => self.emit_lit_int(expr, dest.reg(), false),
            ExprLitFloat(ref expr) => self.emit_lit_float(expr, dest.freg()),
            ExprLitBool(ref expr) => self.emit_lit_bool(expr, dest.reg()),
            ExprLitStr(ref expr) => self.emit_lit_str(expr, dest.reg()),
            ExprUn(ref expr) => self.emit_unary_operator(expr, dest),
            ExprIdent(ref expr) => self.emit_ident(expr, dest),
            ExprBin(ref expr) => self.emit_bin(expr, dest),
            ExprCall(ref expr) => self.emit_call(expr, dest),
            ExprTypeParam(_) => unreachable!(),
            ExprPath(ref expr) => self.emit_path(expr, dest),
            ExprDelegation(ref expr) => self.emit_delegation(expr, dest),
            ExprDot(ref expr) => self.emit_dot(expr, dest),
            ExprSelf(_) => self.emit_self(dest),
            ExprSuper(_) => self.emit_self(dest),
            ExprNil(_) => self.emit_nil(dest.reg()),
            ExprConv(ref expr) => self.emit_conv(expr, dest.reg()),
            ExprTemplate(ref expr) => self.emit_template(expr, dest.reg()),
            ExprLambda(_) => unimplemented!(),
            ExprBlock(ref expr) => self.emit_block(expr, dest),
            ExprIf(ref expr) => self.emit_if(expr, dest),
            ExprTuple(ref expr) => self.emit_tuple(expr, dest),
        }
    }

    fn emit_tuple(&mut self, e: &'ast ExprTupleType, dest: ExprStore) {
        if e.values.is_empty() {
            assert!(dest.is_none());
            return;
        }

        let ty = self.ty(e.id);
        let mut slots = Vec::new();

        for value in &e.values {
            let ty = self.ty(value.id());
            let dest = self.emit_expr_result_reg(value);

            let slot = self.managed_stack.add_temp(ty, self.vm);
            slots.push(slot);

            if let Some(tuple_id) = ty.tuple_id() {
                self.copy_tuple(
                    tuple_id,
                    RegOrOffset::Offset(slot.offset),
                    RegOrOffset::Offset(dest.stack_offset()),
                );
            } else if ty.is_unit() {
                // nothing to do
            } else {
                self.asm.var_store(slot.offset, ty, dest.any_reg());
            }

            self.free_expr_store(dest);
        }

        let offsets = self
            .vm
            .tuples
            .lock()
            .get_tuple(ty.tuple_id().unwrap())
            .offsets()
            .to_owned();
        let tuple_offset = dest.stack_offset();

        for (value, (slot, &offset)) in e.values.iter().zip(slots.iter().zip(&offsets)) {
            let ty = self.ty(value.id());

            if let Some(tuple_id) = ty.tuple_id() {
                self.copy_tuple(
                    tuple_id,
                    RegOrOffset::Offset(tuple_offset + offset),
                    RegOrOffset::Offset(slot.offset),
                );
            } else if ty.is_unit() {
                // do nothing
            } else {
                let mode = ty.mode();
                let temp = result_reg_ty(ty);
                self.asm
                    .load_mem(mode, temp.any_reg(), Mem::Local(slot.offset));
                self.asm
                    .store_mem(mode, Mem::Local(tuple_offset + offset), temp.any_reg());
            }
        }

        for slot in slots {
            self.managed_stack.free_temp(slot, self.vm);
        }

        let slot = dest.stack_slot();
        self.managed_stack.mark_initialized(slot.var);
    }

    fn emit_if(&mut self, e: &'ast ExprIfType, dest: ExprStore) {
        let lbl_end = self.asm.create_label();
        let lbl_else = if let Some(_) = e.else_block {
            self.asm.create_label()
        } else {
            lbl_end
        };

        self.emit_expr_result_reg(&e.cond);
        self.asm
            .test_and_jump_if(CondCode::Zero, REG_RESULT, lbl_else);

        self.emit_expr(&e.then_block, dest);

        if let Some(ref else_block) = e.else_block {
            self.asm.jump(lbl_end);
            self.asm.bind_label(lbl_else);

            self.emit_expr(else_block, dest);
        }

        self.asm.bind_label(lbl_end);
    }

    fn emit_block(&mut self, block: &'ast ExprBlockType, dest: ExprStore) {
        self.managed_stack.push_scope();

        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }

        if let Some(ref expr) = block.expr {
            self.emit_expr(expr, dest);
        }

        self.managed_stack.pop_scope(self.vm);
    }

    fn emit_template(&mut self, e: &'ast ExprTemplateType, dest: Reg) {
        // build StringBuffer::empty() call
        let fct_id = self.vm.vips.fct.string_buffer_empty;
        let ctype = CallType::Fct(fct_id, TypeList::empty(), TypeList::empty());
        let string_buffer_new = self.build_call_site(&ctype, fct_id, Vec::new());
        self.emit_call_site(&string_buffer_new, e.pos, REG_RESULT.into());
        let buffer_slot = self.managed_stack.add_temp(BuiltinType::Ptr, self.vm);
        self.asm
            .var_store(buffer_slot.offset(), BuiltinType::Ptr, REG_RESULT.into());

        for part in &e.parts {
            if let Some(ref lit_str) = part.to_lit_str() {
                self.emit_lit_str_value(&lit_str.value, REG_RESULT);
            } else {
                let ty = self.ty(part.id());

                let dest = result_reg_ty(ty);
                self.emit_expr(part, dest);

                if ty.cls_id(self.vm) != Some(self.vm.vips.string_class) {
                    let object_slot = self.managed_stack.add_temp(ty, self.vm);
                    self.asm.var_store(object_slot.offset(), ty, dest.any_reg());

                    // build toString() call
                    let cls_id = ty.cls_id(self.vm).expect("no cls_id found for type");
                    let cls = self.vm.classes.idx(cls_id);
                    let cls = cls.read();
                    let name = self.vm.interner.intern("toString");
                    let to_string_id = cls
                        .find_trait_method(self.vm, self.vm.vips.stringable_trait, name, false)
                        .expect("toString() method not found");
                    let ctype = CallType::Method(ty, to_string_id, TypeList::empty());
                    let args = vec![Arg::Stack(object_slot.offset())];
                    let to_string = self.build_call_site(&ctype, to_string_id, args);
                    self.emit_call_site(&to_string, e.pos, REG_RESULT.into());
                    self.managed_stack.free_temp(object_slot, self.vm);
                }
            }

            let part_slot = self.managed_stack.add_temp(BuiltinType::Ptr, self.vm);
            self.asm
                .var_store(part_slot.offset(), BuiltinType::Ptr, REG_RESULT.into());

            // build StringBuffer::append() call
            let fct_id = self.vm.vips.fct.string_buffer_append;
            let ty = BuiltinType::from_cls(self.vm.vips.cls.string_buffer, self.vm);
            let ctype = CallType::Method(ty, fct_id, TypeList::empty());
            let args = vec![
                Arg::Stack(buffer_slot.offset()),
                Arg::Stack(part_slot.offset()),
            ];
            let append = self.build_call_site(&ctype, fct_id, args);
            self.emit_call_site(&append, e.pos, dest.into());
            self.managed_stack.free_temp(part_slot, self.vm);
        }

        // build StringBuffer::toString() call
        let fct_id = self.vm.vips.fct.string_buffer_to_string;
        let ty = BuiltinType::from_cls(self.vm.vips.cls.string_buffer, self.vm);
        let ctype = CallType::Method(ty, fct_id, TypeList::empty());
        let args = vec![Arg::Stack(buffer_slot.offset())];
        let string_buffer_to_string = self.build_call_site(&ctype, fct_id, args);
        self.emit_call_site(&string_buffer_to_string, e.pos, dest.into());
        self.managed_stack.free_temp(buffer_slot, self.vm);
    }

    fn emit_conv(&mut self, e: &'ast ExprConvType, dest: Reg) {
        self.emit_expr(&e.object, dest.into());

        // return false if object is nil
        let lbl_nil = self.asm.test_if_nil(dest);
        let conv = *self.src.map_convs.get(e.id).unwrap();

        if conv.valid {
            if e.is {
                // return true for object is T
                self.asm.load_true(dest);
            } else {
                // do nothing for object as T
            }
        } else {
            let cls_id = specialize_class_ty(self.vm, conv.check_type);
            let cls = self.vm.class_defs.idx(cls_id);
            let cls = cls.read();

            let vtable: &VTable = cls.vtable.as_ref().unwrap();

            let slot = if e.is {
                None
            } else {
                // reserve temp variable for object
                let slot = self.add_temp_node(&e.object);
                self.asm
                    .store_mem(MachineMode::Ptr, Mem::Local(slot.offset()), dest.into());

                Some(slot)
            };

            // object instanceof T

            // tmp1 = <vtable of object>
            self.asm
                .load_mem(MachineMode::Ptr, REG_TMP1.into(), Mem::Base(dest, 0));

            let disp = self.asm.add_addr(vtable as *const _ as *mut u8);
            let pos = self.asm.pos() as i32;

            // tmp2 = <vtable of T>
            self.asm.load_constpool(REG_TMP2, disp + pos);

            if vtable.subtype_depth >= DISPLAY_SIZE {
                // cmp [tmp1 + offset T.vtable.subtype_depth], tmp3
                self.asm.cmp_mem_imm(
                    MachineMode::Int32,
                    Mem::Base(REG_TMP1, VTable::offset_of_depth()),
                    vtable.subtype_depth as i32,
                );

                // jnz lbl_false
                let lbl_false = self.asm.create_label();
                self.asm.jump_if(CondCode::Less, lbl_false);

                // tmp1 = tmp1.subtype_overflow
                self.asm.load_mem(
                    MachineMode::Ptr,
                    REG_TMP1.into(),
                    Mem::Base(REG_TMP1, VTable::offset_of_overflow()),
                );

                let overflow_offset =
                    mem::ptr_width() * (vtable.subtype_depth - DISPLAY_SIZE) as i32;

                // cmp [tmp1 + 8*(vtable.subtype_depth - DISPLAY_SIZE) ], tmp2
                self.asm.cmp_mem(
                    MachineMode::Ptr,
                    Mem::Base(REG_TMP1, overflow_offset),
                    REG_TMP2,
                );

                if e.is {
                    // dest = if zero then true else false
                    self.asm.set(dest, CondCode::Equal);
                } else {
                    // jump to lbl_false if cmp did not succeed
                    self.asm.jump_if(CondCode::NonZero, lbl_false);

                    // otherwise load temp variable again
                    self.asm.load_mem(
                        MachineMode::Ptr,
                        dest.into(),
                        Mem::Local(slot.unwrap().offset()),
                    );
                }

                // jmp lbl_finished
                let lbl_finished = self.asm.create_label();
                self.asm.jump(lbl_finished);

                // lbl_false:
                self.asm.bind_label(lbl_false);

                if e.is {
                    // dest = false
                    self.asm.load_false(dest);
                } else {
                    // bailout
                    self.asm.emit_bailout_inplace(Trap::CAST, e.pos);
                }

                // lbl_finished:
                self.asm.bind_label(lbl_finished);
            } else {
                let display_entry =
                    VTable::offset_of_display() + vtable.subtype_depth as i32 * mem::ptr_width();

                // tmp1 = vtable of object
                // tmp2 = vtable of T
                // cmp [tmp1 + offset], tmp2
                self.asm.cmp_mem(
                    MachineMode::Ptr,
                    Mem::Base(REG_TMP1, display_entry),
                    REG_TMP2,
                );

                if e.is {
                    self.asm.set(dest, CondCode::Equal);
                } else {
                    let lbl_bailout = self.asm.create_label();
                    self.asm.jump_if(CondCode::NotEqual, lbl_bailout);
                    self.asm.emit_bailout(lbl_bailout, Trap::CAST, e.pos);

                    self.asm.load_mem(
                        MachineMode::Ptr,
                        dest.into(),
                        Mem::Local(slot.unwrap().offset()),
                    );
                }
            }

            if let Some(slot) = slot {
                self.managed_stack.free_temp(slot, self.vm);
            }
        }

        // lbl_nil:
        self.asm.bind_label(lbl_nil);

        // for is we are finished: dest is null which is boolean false
        // also for as we are finished: dest is null and stays null
    }

    fn var_offset(&self, id: VarId) -> i32 {
        if let Some(&offset) = self.var_to_offset.get(&id) {
            assert!(!self.var_to_slot.contains_key(&id));
            offset
        } else {
            self.var_to_slot[&id].offset()
        }
    }

    fn add_temp_arg(&mut self, arg: &InternalArg<'ast>) -> ManagedStackSlot {
        let ty = arg.ty();
        self.managed_stack.add_temp(ty, self.vm)
    }

    fn add_temp_node(&mut self, expr: &Expr) -> ManagedStackSlot {
        let id = expr.id();
        let ty = self.ty(id);

        self.managed_stack.add_temp(ty, self.vm)
    }

    fn get_intrinsic(&self, id: NodeId) -> Option<Intrinsic> {
        let call_type = self.src.map_calls.get(id);
        let call_type = if call_type.is_some() {
            call_type.unwrap()
        } else {
            return None;
        };

        if let Some(intrinsic) = call_type.to_intrinsic() {
            return Some(intrinsic);
        }

        let fid = call_type.fct_id().unwrap();

        // the function we compile right now is never an intrinsic
        if self.fct.id == fid {
            return None;
        }

        let fct = self.vm.fcts.idx(fid);
        let fct = fct.read();

        match fct.kind {
            FctKind::Builtin(intr) => Some(intr),
            _ => None,
        }
    }

    fn emit_self(&mut self, dest: ExprStore) {
        let var = self.src.var_self();

        self.asm.emit_comment("load self".into());

        let offset = self.var_offset(var.id);
        self.asm
            .load_mem(var.ty.mode(), dest.any_reg(), Mem::Local(offset));
    }

    fn emit_nil(&mut self, dest: Reg) {
        self.asm.load_nil(dest);
    }

    fn emit_dot(&mut self, expr: &'ast ExprDotType, dest: ExprStore) {
        let object_ty = self.ty(expr.lhs.id());

        if let Some(tuple_id) = object_ty.tuple_id() {
            self.emit_dot_tuple(expr, tuple_id, dest);
            return;
        }

        let (ty, field) = {
            let ident_type = self.src.map_idents.get(expr.id).unwrap();

            match ident_type {
                &IdentType::Field(ty, field) => (ty, field),
                _ => unreachable!(),
            }
        };

        let ty = self.specialize_type(ty);

        self.emit_expr(&expr.lhs, REG_TMP1.into());
        self.emit_field_access(expr.pos, ty, field, REG_TMP1, dest);
    }

    fn emit_dot_tuple(&mut self, expr: &'ast ExprDotType, tuple_id: TupleId, dest: ExprStore) {
        let tuple = self.emit_expr_result_reg(&expr.lhs);

        let idx = expr.rhs.to_lit_int().unwrap().value as usize;
        let (ty, offset) = self.vm.tuples.lock().get_at(tuple_id, idx);

        if let Some(tuple_id) = ty.tuple_id() {
            self.copy_tuple(
                tuple_id,
                RegOrOffset::Offset(dest.stack_offset()),
                RegOrOffset::Offset(tuple.stack_offset() + offset),
            );
        } else if ty.is_unit() {
            assert!(dest.is_none());
        } else {
            self.asm.load_mem(
                ty.mode(),
                dest.any_reg(),
                Mem::Local(tuple.stack_offset() + offset),
            );
        }

        self.free_expr_store(tuple);
    }

    fn emit_field_access(
        &mut self,
        pos: Position,
        cls_ty: BuiltinType,
        fieldid: FieldId,
        src: Reg,
        dest: ExprStore,
    ) {
        let cls_id = specialize_class_ty(self.vm, cls_ty);
        let cls = self.vm.class_defs.idx(cls_id);
        let cls = cls.read();
        let field = &cls.fields[fieldid.idx()];

        {
            let cname = cls.name(self.vm);
            let class_id = cls.cls_id.expect("no corresponding class");
            let class = self.vm.classes.idx(class_id);
            let class = class.read();
            let field = &class.fields[fieldid];
            let fname = self.vm.interner.str(field.name);

            self.asm
                .emit_comment(format!("load field {}.{}", cname, fname));
        }

        self.asm.test_if_nil_bailout(pos, src, Trap::NIL);

        if let Some(tuple_id) = field.ty.tuple_id() {
            self.copy_tuple(
                tuple_id,
                RegOrOffset::Offset(dest.stack_offset()),
                RegOrOffset::RegWithOffset(src, field.offset),
            );
        } else if field.ty.is_unit() {
            assert!(dest.is_none());
        } else {
            self.asm.load_mem(
                field.ty.mode(),
                dest.any_reg(),
                Mem::Base(src, field.offset),
            );
        }
    }

    fn emit_lit_char(&mut self, lit: &'ast ExprLitCharType, dest: Reg) {
        self.asm
            .load_int_const(MachineMode::Int32, dest, lit.value as i64);
    }

    fn emit_lit_int(&mut self, lit: &'ast ExprLitIntType, dest: Reg, negate: bool) {
        let ty = self.src.ty(lit.id);

        let mode = match ty {
            BuiltinType::UInt8 => MachineMode::Int8,
            BuiltinType::Int => MachineMode::Int32,
            BuiltinType::Int64 => MachineMode::Int64,
            _ => unreachable!(),
        };

        let value = if negate {
            (lit.value as i64).wrapping_neg()
        } else {
            lit.value as i64
        };

        self.asm.load_int_const(mode, dest, value);
    }

    fn emit_lit_float(&mut self, lit: &'ast ExprLitFloatType, dest: FReg) {
        let ty = self.src.ty(lit.id);

        let mode = match ty {
            BuiltinType::Float => MachineMode::Float32,
            BuiltinType::Double => MachineMode::Float64,
            _ => unreachable!(),
        };

        self.asm.load_float_const(mode, dest, lit.value);
    }

    fn emit_lit_bool(&mut self, lit: &'ast ExprLitBoolType, dest: Reg) {
        if lit.value {
            self.asm.load_true(dest);
        } else {
            self.asm.load_false(dest);
        };
    }

    fn emit_lit_str(&mut self, lit: &'ast ExprLitStrType, dest: Reg) {
        self.emit_lit_str_value(&lit.value, dest);
    }

    fn emit_lit_str_value(&mut self, lit_value: &str, dest: Reg) {
        let handle = Str::from_buffer_in_perm(self.vm, lit_value.as_bytes());

        let disp = self.asm.add_addr(handle.raw() as *const u8);
        let pos = self.asm.pos() as i32;

        self.asm
            .emit_comment(format!("load string \"{}\"", lit_value));
        self.asm.load_constpool(dest, disp + pos);
    }

    fn emit_ident(&mut self, e: &'ast ExprIdentType, dest: ExprStore) {
        let ident = self.src.map_idents.get(e.id).unwrap();

        match ident {
            &IdentType::Var(varid) => {
                let ty = self.var_ty(varid);

                if ty.is_unit() {
                    assert!(dest.is_none());
                } else {
                    {
                        let var = &self.src.vars[varid];
                        let name = self.vm.interner.str(var.name);
                        self.asm.emit_comment(format!("load var {}", name));
                    }

                    if let Some(tuple_id) = ty.tuple_id() {
                        self.copy_tuple(
                            tuple_id,
                            RegOrOffset::Offset(dest.stack_offset()),
                            RegOrOffset::Offset(self.var_offset(varid)),
                        );
                    } else {
                        self.asm
                            .var_load(self.var_offset(varid), ty, dest.any_reg());
                    }
                }
            }

            &IdentType::Global(gid) => {
                let glob = self.vm.globals.idx(gid);
                let glob = glob.read();

                if glob.ty.is_unit() {
                    assert!(dest.is_none());
                } else {
                    if glob.needs_initialization() {
                        let fid = glob.initializer.unwrap();
                        let ptr = self.ptr_for_fct_id(fid, TypeList::empty(), TypeList::empty());
                        let gcpoint = self.create_gcpoint();
                        self.asm.ensure_global(&*glob, fid, ptr, glob.pos, gcpoint);
                    }

                    let disp = self.asm.add_addr(glob.address_value.to_ptr());
                    let pos = self.asm.pos() as i32;

                    let name = self.vm.interner.str(glob.name);
                    self.asm.emit_comment(format!("load global {}", name));

                    self.asm.load_constpool(REG_TMP1, disp + pos);

                    if let Some(tuple_id) = glob.ty.tuple_id() {
                        self.copy_tuple(
                            tuple_id,
                            RegOrOffset::Offset(dest.stack_offset()),
                            RegOrOffset::Reg(REG_TMP1),
                        );
                    } else {
                        self.asm
                            .load_mem(glob.ty.mode(), dest.any_reg(), Mem::Base(REG_TMP1, 0));
                    }
                }
            }

            &IdentType::Field(_, _) => unreachable!(),

            &IdentType::Struct(_) => {
                unimplemented!();
            }

            &IdentType::Const(const_id) => {
                self.emit_const(const_id, dest.any_reg());
            }

            &IdentType::Enum(_) | &IdentType::EnumValue(_, _) => unreachable!(),
            &IdentType::Fct(_) | &IdentType::FctType(_, _) => unreachable!(),
            &IdentType::Class(_) | &IdentType::ClassType(_, _) => unreachable!(),
            &IdentType::Module(_) => unreachable!(),
            &IdentType::Method(_, _) | &IdentType::MethodType(_, _, _) => unreachable!(),
            &IdentType::TypeParam(_) | &IdentType::TypeParamStaticMethod(_, _) => unreachable!(),
            &IdentType::StaticMethod(_, _) | &IdentType::StaticMethodType(_, _, _) => {
                unreachable!()
            }
        }
    }

    fn emit_const(&mut self, const_id: ConstId, dest: AnyReg) {
        let xconst = self.vm.consts.idx(const_id);
        let xconst = xconst.lock();
        let ty = xconst.ty;

        match ty {
            BuiltinType::Bool => {
                if xconst.value.to_bool() {
                    self.asm.load_true(dest.reg());
                } else {
                    self.asm.load_false(dest.reg());
                }
            }

            BuiltinType::Char => {
                self.asm.load_int_const(
                    MachineMode::Int32,
                    dest.reg(),
                    xconst.value.to_char() as i64,
                );
            }

            BuiltinType::UInt8 | BuiltinType::Int | BuiltinType::Int64 => {
                self.asm
                    .load_int_const(ty.mode(), dest.reg(), xconst.value.to_int());
            }

            BuiltinType::Float | BuiltinType::Double => {
                self.asm
                    .load_float_const(ty.mode(), dest.freg(), xconst.value.to_float());
            }

            _ => unimplemented!(),
        }
    }

    fn emit_intrinsic_unary(&mut self, e: &'ast Expr, dest: ExprStore, intrinsic: Intrinsic) {
        self.emit_expr(&e, dest);

        match intrinsic {
            Intrinsic::Int32Plus
            | Intrinsic::Int64Plus
            | Intrinsic::FloatPlus
            | Intrinsic::DoublePlus => {}

            Intrinsic::Int32Neg | Intrinsic::Int64Neg => {
                let dest = dest.reg();

                let mode = if intrinsic == Intrinsic::Int32Neg {
                    MachineMode::Int32
                } else {
                    MachineMode::Int64
                };

                self.asm.int_neg(mode, dest, dest);
            }

            Intrinsic::FloatNeg | Intrinsic::DoubleNeg => {
                let dest = dest.freg();

                let mode = if intrinsic == Intrinsic::FloatNeg {
                    MachineMode::Float32
                } else {
                    MachineMode::Float64
                };

                self.asm.float_neg(mode, dest, dest);
            }

            Intrinsic::ByteNot => {
                let dest = dest.reg();
                self.asm.int_not(MachineMode::Int8, dest, dest)
            }

            Intrinsic::Int32Not | Intrinsic::Int64Not => {
                let dest = dest.reg();

                let mode = if intrinsic == Intrinsic::Int32Not {
                    MachineMode::Int32
                } else {
                    MachineMode::Int64
                };

                self.asm.int_not(mode, dest, dest);
            }

            Intrinsic::BoolNot => {
                let dest = dest.reg();
                self.asm.bool_not(dest, dest)
            }

            Intrinsic::Int32CountZeroBits => {
                let dest = dest.reg();
                self.asm.count_bits(MachineMode::Int32, dest, dest, false)
            }

            Intrinsic::Int32CountOneBits => {
                let dest = dest.reg();
                self.asm.count_bits(MachineMode::Int32, dest, dest, true)
            }

            Intrinsic::Int32CountZeroBitsLeading => {
                let dest = dest.reg();
                self.asm
                    .count_bits_leading(MachineMode::Int32, dest, dest, false)
            }

            Intrinsic::Int32CountOneBitsLeading => {
                let dest = dest.reg();
                self.asm
                    .count_bits_leading(MachineMode::Int32, dest, dest, true)
            }

            Intrinsic::Int32CountZeroBitsTrailing => {
                let dest = dest.reg();
                self.asm
                    .count_bits_trailing(MachineMode::Int32, dest, dest, false)
            }

            Intrinsic::Int32CountOneBitsTrailing => {
                let dest = dest.reg();
                self.asm
                    .count_bits_trailing(MachineMode::Int32, dest, dest, true)
            }

            Intrinsic::Int64CountZeroBits => {
                let dest = dest.reg();
                self.asm.count_bits(MachineMode::Int64, dest, dest, false)
            }

            Intrinsic::Int64CountOneBits => {
                let dest = dest.reg();
                self.asm.count_bits(MachineMode::Int64, dest, dest, true)
            }

            Intrinsic::Int64CountZeroBitsLeading => {
                let dest = dest.reg();
                self.asm
                    .count_bits_leading(MachineMode::Int64, dest, dest, false)
            }

            Intrinsic::Int64CountOneBitsLeading => {
                let dest = dest.reg();
                self.asm
                    .count_bits_leading(MachineMode::Int64, dest, dest, true)
            }

            Intrinsic::Int64CountZeroBitsTrailing => {
                let dest = dest.reg();
                self.asm
                    .count_bits_trailing(MachineMode::Int64, dest, dest, false)
            }

            Intrinsic::Int64CountOneBitsTrailing => {
                let dest = dest.reg();
                self.asm
                    .count_bits_trailing(MachineMode::Int64, dest, dest, true)
            }

            _ => panic!("unexpected intrinsic {:?}", intrinsic),
        }
    }

    fn emit_unary_operator(&mut self, e: &'ast ExprUnType, dest: ExprStore) {
        if e.op == UnOp::Neg && e.opnd.is_lit_int() {
            self.emit_lit_int(e.opnd.to_lit_int().unwrap(), dest.reg(), true);
        } else if let Some(intrinsic) = self.get_intrinsic(e.id) {
            self.emit_intrinsic_unary(&e.opnd, dest, intrinsic);
        } else {
            let args = vec![Arg::Expr(&e.opnd)];
            let fid = self.src.map_calls.get(e.id).unwrap().fct_id().unwrap();
            let call_site = self.build_call_site_id(e.id, args, Some(fid));
            self.emit_call_site(&call_site, e.pos, dest);
        }
    }

    fn emit_assign(&mut self, e: &'ast ExprBinType) {
        let call_type = self.src.map_calls.get(e.id);

        if call_type.is_some() {
            let call_expr = e.lhs.to_call().unwrap();
            let object = &call_expr.callee;
            let index = &call_expr.args[0];
            let value = &e.rhs;

            if let Some(intrinsic) = self.get_intrinsic(e.id) {
                match intrinsic {
                    Intrinsic::GenericArraySet => {
                        let element_type = self.ty(object.id()).type_params(self.vm)[0];
                        self.emit_array_set(e.pos, element_type, object, index, value)
                    }

                    Intrinsic::StrSet => {
                        self.emit_array_set(e.pos, BuiltinType::UInt8, object, index, value)
                    }

                    _ => panic!("unexpected intrinsic {:?}", intrinsic),
                }
            } else {
                let args = vec![Arg::Expr(object), Arg::Expr(index), Arg::Expr(value)];
                let call_site = self.build_call_site_id(e.id, args, None);
                self.emit_call_site(&call_site, e.pos, REG_RESULT.into());
            }

            return;
        }

        let ident_type = self.src.map_idents.get(e.lhs.id()).unwrap();

        match ident_type {
            &IdentType::Var(varid) => {
                let ty = self.var_ty(varid);
                let value = self.emit_expr_result_reg(&e.rhs);

                if !ty.is_unit() {
                    {
                        let var = &self.src.vars[varid];
                        let name = self.vm.interner.str(var.name);
                        self.asm.emit_comment(format!("store var {}", name));
                    }

                    let offset = self.var_offset(varid);

                    if let Some(tuple_id) = ty.tuple_id() {
                        self.copy_tuple(
                            tuple_id,
                            RegOrOffset::Offset(offset),
                            RegOrOffset::Offset(value.stack_offset()),
                        );
                    } else {
                        self.asm.var_store(offset, ty, value.any_reg());
                    }
                }

                self.free_expr_store(value);
            }

            &IdentType::Global(gid) => {
                let glob = self.vm.globals.idx(gid);
                let glob = glob.read();
                let ty = glob.ty;

                let value = self.emit_expr_result_reg(&e.rhs);

                if !ty.is_unit() {
                    let disp = self.asm.add_addr(glob.address_value.to_ptr());
                    let pos = self.asm.pos() as i32;
                    let name = self.vm.interner.str(glob.name);
                    self.asm.emit_comment(format!("store global {}", name));
                    self.asm.load_constpool(REG_TMP1, disp + pos);

                    if let Some(tuple_id) = ty.tuple_id() {
                        self.copy_tuple(
                            tuple_id,
                            RegOrOffset::Reg(REG_TMP1),
                            RegOrOffset::Offset(value.stack_offset()),
                        );
                    } else {
                        self.asm
                            .store_mem(ty.mode(), Mem::Base(REG_TMP1, 0), value.any_reg());
                    }

                    if glob.needs_initialization() {
                        let disp = self.asm.add_addr(glob.address_init.to_ptr());
                        let pos = self.asm.pos() as i32;
                        self.asm.load_constpool(REG_RESULT, disp + pos);
                        self.asm.load_int_const(MachineMode::Int8, REG_TMP1, 1);
                        self.asm.store_mem(
                            MachineMode::Int8,
                            Mem::Base(REG_RESULT, 0),
                            REG_TMP1.into(),
                        );
                    }
                }

                self.free_expr_store(value);
            }

            &IdentType::Field(ty, fieldid) => {
                let ty = self.specialize_type(ty);
                let cls_id = specialize_class_ty(self.vm, ty);
                let cls = self.vm.class_defs.idx(cls_id);
                let cls = cls.read();
                let field = &cls.fields[fieldid.idx()];

                let temp = if let Some(expr_field) = e.lhs.to_dot() {
                    self.emit_expr(&expr_field.lhs, REG_RESULT.into());

                    &expr_field.lhs
                } else {
                    self.emit_self(REG_RESULT.into());

                    &e.lhs
                };

                let object_slot = self.add_temp_node(temp);
                self.asm.store_mem(
                    MachineMode::Ptr,
                    Mem::Local(object_slot.offset()),
                    REG_RESULT.into(),
                );

                let value = self.emit_expr_result_reg(&e.rhs);
                self.asm.load_mem(
                    MachineMode::Ptr,
                    REG_TMP1.into(),
                    Mem::Local(object_slot.offset()),
                );

                {
                    let cname = cls.name(self.vm);
                    let class_id = cls.cls_id.expect("no corresponding class");
                    let class = self.vm.classes.idx(class_id);
                    let class = class.read();
                    let field = &class.fields[fieldid];
                    let fname = self.vm.interner.str(field.name);
                    self.asm
                        .emit_comment(format!("store field {}.{}", cname, fname));
                }

                self.asm.test_if_nil_bailout(e.pos, REG_TMP1, Trap::NIL);

                let needs_write_barrier = if let Some(tuple_id) = field.ty.tuple_id() {
                    self.copy_tuple(
                        tuple_id,
                        RegOrOffset::RegWithOffset(REG_TMP1, field.offset),
                        RegOrOffset::Offset(value.stack_offset()),
                    );

                    self.vm
                        .tuples
                        .lock()
                        .get_tuple(tuple_id)
                        .contains_references()
                } else if field.ty.is_unit() {
                    false
                } else {
                    self.asm.store_mem(
                        field.ty.mode(),
                        Mem::Base(REG_TMP1, field.offset),
                        value.any_reg(),
                    );

                    field.ty.reference_type()
                };

                if self.vm.gc.needs_write_barrier() && needs_write_barrier {
                    let card_table_offset = self.vm.gc.card_table_offset();
                    self.asm.emit_barrier(REG_TMP1, card_table_offset);
                }

                self.free_expr_store(value);
                self.managed_stack.free_temp(object_slot, self.vm);
            }

            &IdentType::Struct(_) => {
                unimplemented!();
            }

            &IdentType::Const(_) | &IdentType::Enum(_) | &IdentType::EnumValue(_, _) => {
                unreachable!();
            }

            &IdentType::Fct(_) | &IdentType::FctType(_, _) => unreachable!(),
            &IdentType::Class(_) | &IdentType::ClassType(_, _) => unreachable!(),
            &IdentType::Module(_) => unreachable!(),
            &IdentType::Method(_, _) | &IdentType::MethodType(_, _, _) => unreachable!(),
            &IdentType::TypeParam(_) | &IdentType::TypeParamStaticMethod(_, _) => unreachable!(),
            &IdentType::StaticMethod(_, _) | &IdentType::StaticMethodType(_, _, _) => {
                unreachable!()
            }
        }
    }

    fn emit_bin(&mut self, e: &'ast ExprBinType, dest: ExprStore) {
        if e.op.is_any_assign() {
            self.emit_assign(e);
        } else if let Some(intrinsic) = self.get_intrinsic(e.id) {
            self.emit_intrinsic_bin(&e.lhs, &e.rhs, dest, intrinsic, Some(e.op), e.pos);
        } else if e.op == BinOp::Cmp(CmpOp::Is) || e.op == BinOp::Cmp(CmpOp::IsNot) {
            self.emit_bin_is(e, dest.reg());
        } else if e.op == BinOp::Or {
            self.emit_bin_or(e, dest.reg());
        } else if e.op == BinOp::And {
            self.emit_bin_and(e, dest.reg());
        } else {
            let args = vec![Arg::Expr(&e.lhs), Arg::Expr(&e.rhs)];
            let fid = self.src.map_calls.get(e.id).unwrap().fct_id().unwrap();
            let call_site = self.build_call_site_id(e.id, args, Some(fid));
            self.emit_call_site(&call_site, e.pos, dest);

            match e.op {
                BinOp::Cmp(CmpOp::Eq) => {}
                BinOp::Cmp(CmpOp::Ne) => {
                    let dest = dest.reg();
                    self.asm.bool_not(dest, dest);
                }

                BinOp::Cmp(op) => {
                    let dest = dest.reg();

                    let temp = if dest == REG_RESULT {
                        REG_TMP1
                    } else {
                        REG_RESULT
                    };

                    self.asm.load_int_const(MachineMode::Int32, temp, 0);
                    self.asm.cmp_reg(MachineMode::Int32, dest, temp);
                    self.asm.set(dest, to_cond_code(op));
                }
                _ => {}
            }
        }
    }

    fn emit_bin_is(&mut self, e: &'ast ExprBinType, dest: Reg) {
        let builtin_type = self.ty(e.lhs.id());
        let dest_mode = match builtin_type {
            BuiltinType::Nil => MachineMode::Ptr,
            BuiltinType::Float => MachineMode::Int32,
            BuiltinType::Double => MachineMode::Int64,
            _ => builtin_type.mode(),
        };
        let slot = if builtin_type.is_float() {
            let src_mode = builtin_type.mode();

            self.emit_expr(&e.lhs, FREG_RESULT.into());
            self.asm
                .float_as_int(dest_mode, REG_RESULT, src_mode, FREG_RESULT);
            let slot = self.add_temp_node(&e.lhs);
            self.asm
                .store_mem(dest_mode, Mem::Local(slot.offset()), REG_RESULT.into());

            self.emit_expr(&e.rhs, FREG_RESULT.into());
            self.asm
                .float_as_int(dest_mode, REG_TMP1, src_mode, FREG_RESULT);
            slot
        } else {
            self.emit_expr(&e.lhs, REG_RESULT.into());
            let slot = self.add_temp_node(&e.lhs);
            self.asm
                .store_mem(dest_mode, Mem::Local(slot.offset()), REG_RESULT.into());

            self.emit_expr(&e.rhs, REG_TMP1.into());

            slot
        };

        self.asm
            .load_mem(dest_mode, REG_RESULT.into(), Mem::Local(slot.offset()));
        self.asm.cmp_reg(dest_mode, REG_RESULT, REG_TMP1);

        let op = match e.op {
            BinOp::Cmp(CmpOp::Is) => CondCode::Equal,
            _ => CondCode::NotEqual,
        };

        self.asm.set(dest, op);
        self.managed_stack.free_temp(slot, self.vm);
    }

    fn emit_bin_or(&mut self, e: &'ast ExprBinType, dest: Reg) {
        let lbl_true = self.asm.create_label();
        let lbl_false = self.asm.create_label();
        let lbl_end = self.asm.create_label();

        self.emit_expr(&e.lhs, REG_RESULT.into());
        self.asm
            .test_and_jump_if(CondCode::NonZero, REG_RESULT, lbl_true);

        self.emit_expr(&e.rhs, REG_RESULT.into());
        self.asm
            .test_and_jump_if(CondCode::Zero, REG_RESULT, lbl_false);

        self.asm.bind_label(lbl_true);
        self.asm.load_true(dest);
        self.asm.jump(lbl_end);

        self.asm.bind_label(lbl_false);
        self.asm.load_false(dest);

        self.asm.bind_label(lbl_end);
    }

    fn emit_bin_and(&mut self, e: &'ast ExprBinType, dest: Reg) {
        let lbl_true = self.asm.create_label();
        let lbl_false = self.asm.create_label();
        let lbl_end = self.asm.create_label();

        self.emit_expr(&e.lhs, REG_RESULT.into());
        self.asm
            .test_and_jump_if(CondCode::Zero, REG_RESULT, lbl_false);

        self.emit_expr(&e.rhs, REG_RESULT.into());
        self.asm
            .test_and_jump_if(CondCode::Zero, REG_RESULT, lbl_false);

        self.asm.bind_label(lbl_true);
        self.asm.load_true(dest);
        self.asm.jump(lbl_end);

        self.asm.bind_label(lbl_false);
        self.asm.load_false(dest);

        self.asm.bind_label(lbl_end);
    }

    fn ptr_for_fct_id(
        &mut self,
        fid: FctId,
        cls_type_params: TypeList,
        fct_type_params: TypeList,
    ) -> Address {
        if self.fct.id == fid {
            // we want to recursively invoke the function we are compiling right now
            ensure_jit_or_stub_ptr(self.src, self.vm, cls_type_params, fct_type_params)
        } else {
            let fct = self.vm.fcts.idx(fid);
            let fct = fct.read();

            match fct.kind {
                FctKind::Source(_) => {
                    let src = fct.src();
                    let src = src.read();

                    ensure_jit_or_stub_ptr(&src, self.vm, cls_type_params, fct_type_params)
                }

                FctKind::Native(ptr) => {
                    let internal_fct = NativeFct {
                        ptr,
                        args: fct.params_with_self(),
                        return_type: fct.return_type,
                        desc: NativeFctDescriptor::NativeStub(fid),
                    };

                    ensure_native_stub(self.vm, Some(fid), internal_fct)
                }

                FctKind::Definition => panic!("prototype for fct call"),
                FctKind::Builtin(_) => panic!("intrinsic fct call"),
            }
        }
    }

    fn emit_path(&mut self, e: &'ast ExprPathType, dest: ExprStore) {
        let ident_type = self.src.map_idents.get(e.id).unwrap();

        match ident_type {
            &IdentType::EnumValue(_, value) => {
                self.asm
                    .load_int_const(MachineMode::Int32, dest.reg(), value as i64);
            }

            _ => unreachable!(),
        }
    }

    fn emit_call(&mut self, e: &'ast ExprCallType, dest: ExprStore) {
        let call_type = self.src.map_calls.get(e.id).unwrap().clone();

        if let Some(intrinsic) = self.get_intrinsic(e.id) {
            let mut args: Vec<&'ast Expr> = Vec::with_capacity(3);

            if call_type.is_expr() {
                args.push(&e.callee);
            } else if call_type.is_method() {
                args.push(e.object().unwrap());
            }

            for arg in &e.args {
                args.push(arg);
            }

            self.emit_call_intrinsic(e.id, e.pos, &args, intrinsic, dest);
        } else {
            let mut args = e.args.iter().map(|arg| Arg::Expr(arg)).collect::<Vec<_>>();

            let callee_id = match *call_type {
                CallType::Ctor(_, fid) | CallType::CtorNew(_, fid) => {
                    let arg = if call_type.is_ctor() {
                        Arg::Selfie
                    } else {
                        Arg::SelfieNew
                    };

                    args.insert(0, arg);

                    fid
                }

                CallType::Method(_, fct_id, _) => {
                    let object = e.object().unwrap();
                    args.insert(0, Arg::Expr(object));

                    let fct = self.vm.fcts.idx(fct_id);
                    let fct = fct.read();

                    if fct.parent.is_trait() {
                        // This happens for calls like (T: SomeTrait).method()
                        // Find the exact method that is called
                        let trait_id = fct.trait_id();
                        let object_type = match *call_type {
                            CallType::Method(ty, _, _) => ty,
                            _ => unreachable!(),
                        };
                        let object_type = self.specialize_type(object_type);
                        self.find_trait_impl(fct_id, trait_id, object_type)
                    } else {
                        fct_id
                    }
                }

                CallType::Fct(fid, _, _) => fid,

                CallType::Expr(_, fid) => {
                    let object = &e.callee;
                    args.insert(0, Arg::Expr(object));

                    fid
                }

                CallType::TraitStatic(tp_id, trait_id, trait_fct_id) => {
                    let list_id = match tp_id {
                        TypeParamId::Fct(list_id) => list_id,
                        TypeParamId::Class(_) => unimplemented!(),
                    };

                    let ty = self.fct_type_params[list_id.idx()];
                    let cls_id = ty.cls_id(self.vm).expect("no cls_id for type");

                    let cls = self.vm.classes.idx(cls_id);
                    let cls = cls.read();

                    let mut impl_fct_id: Option<FctId> = None;

                    for &impl_id in &cls.impls {
                        let ximpl = self.vm.impls[impl_id].read();

                        if ximpl.trait_id != Some(trait_id) {
                            continue;
                        }

                        for &fid in &ximpl.methods {
                            let method = self.vm.fcts.idx(fid);
                            let method = method.read();

                            if method.impl_for == Some(trait_fct_id) {
                                impl_fct_id = Some(fid);
                                break;
                            }
                        }
                    }

                    impl_fct_id.expect("no impl_fct_id found")
                }

                CallType::Trait(_, _) => unimplemented!(),
                CallType::Intrinsic(_) => unreachable!(),
            };

            let call_site = self.build_call_site_id(e.id, args, Some(callee_id));
            self.emit_call_site(&call_site, e.pos, dest);
        }
    }

    fn find_trait_impl(&self, fct_id: FctId, trait_id: TraitId, object_type: BuiltinType) -> FctId {
        let cls_id = object_type.cls_id(self.vm).unwrap();
        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();

        for &impl_id in &cls.impls {
            let ximpl = self.vm.impls[impl_id].read();

            if ximpl.trait_id() != trait_id {
                continue;
            }

            for &mtd_id in &ximpl.methods {
                let mtd = self.vm.fcts.idx(mtd_id);
                let mtd = mtd.read();

                if mtd.impl_for == Some(fct_id) {
                    return mtd_id;
                }
            }
        }

        panic!("no impl found for generic trait call")
    }

    fn emit_call_intrinsic(
        &mut self,
        id: NodeId,
        pos: Position,
        args: &[&'ast Expr],
        intrinsic: Intrinsic,
        dest: ExprStore,
    ) {
        match intrinsic {
            Intrinsic::GenericArrayLen => self.emit_intrinsic_len(pos, args[0], dest.reg()),
            Intrinsic::GenericArrayGet => {
                let element_type = self.ty(args[0].id()).type_params(self.vm)[0];
                self.emit_array_get(pos, element_type, args[0], args[1], dest)
            }
            Intrinsic::GenericArraySet => {
                let element_type = self.ty(args[0].id()).type_params(self.vm)[0];
                self.emit_array_set(pos, element_type, args[0], args[1], args[2])
            }
            Intrinsic::Assert => {
                assert!(dest.is_none());
                self.emit_intrinsic_assert(pos, args[0])
            }
            Intrinsic::Debug => self.emit_intrinsic_debug(),
            Intrinsic::Shl => self.emit_intrinsic_shl(args[0], args[1], dest.reg()),
            Intrinsic::StrLen => self.emit_intrinsic_len(pos, args[0], dest.reg()),
            Intrinsic::StrGet => {
                self.emit_array_get(pos, BuiltinType::UInt8, args[0], args[1], dest)
            }

            Intrinsic::BoolToInt32 | Intrinsic::ByteToInt32 | Intrinsic::ByteToChar => {
                self.emit_intrinsic_byte_to_int(args[0], dest.reg())
            }
            Intrinsic::BoolToInt64 | Intrinsic::ByteToInt64 => {
                self.emit_intrinsic_byte_to_long(args[0], dest.reg())
            }
            Intrinsic::Int64ToByte => self.emit_intrinsic_long_to_byte(args[0], dest.reg()),
            Intrinsic::Int64ToChar | Intrinsic::Int64ToInt32 => {
                self.emit_intrinsic_long_to_int(args[0], dest.reg())
            }
            Intrinsic::Int64ToFloat => {
                self.emit_intrinsic_int_to_float(args[0], dest.freg(), intrinsic)
            }
            Intrinsic::Int64ToDouble => {
                self.emit_intrinsic_int_to_float(args[0], dest.freg(), intrinsic)
            }

            Intrinsic::ReinterpretInt64AsDouble => {
                self.emit_intrinsic_int_as_float(args[0], dest.freg(), intrinsic)
            }

            Intrinsic::FloatToInt32 => {
                self.emit_intrinsic_float_to_int(args[0], dest.reg(), intrinsic)
            }
            Intrinsic::FloatToInt64 => {
                self.emit_intrinsic_float_to_int(args[0], dest.reg(), intrinsic)
            }
            Intrinsic::PromoteFloatToDouble => {
                self.emit_intrinsic_float_to_double(args[0], dest.freg())
            }
            Intrinsic::ReinterpretFloatAsInt => {
                self.emit_intrinsic_float_as_int(args[0], dest.reg(), intrinsic)
            }

            Intrinsic::DoubleToInt32 => {
                self.emit_intrinsic_float_to_int(args[0], dest.reg(), intrinsic)
            }
            Intrinsic::DoubleToInt64 => {
                self.emit_intrinsic_float_to_int(args[0], dest.reg(), intrinsic)
            }
            Intrinsic::DemoteDoubleToFloat => {
                self.emit_intrinsic_double_to_float(args[0], dest.freg())
            }
            Intrinsic::ReinterpretDoubleAsInt64 => {
                self.emit_intrinsic_float_as_int(args[0], dest.reg(), intrinsic)
            }

            Intrinsic::CharToInt32 | Intrinsic::Int32ToChar => {
                self.emit_expr(args[0], dest);
            }

            Intrinsic::CharToInt64 => self.emit_intrinsic_int_to_long(args[0], dest.reg()),
            Intrinsic::CharEq => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::CharCmp => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }

            Intrinsic::Int32ToByte => self.emit_intrinsic_int_to_byte(args[0], dest.reg()),
            Intrinsic::Int32ToInt64 => self.emit_intrinsic_int_to_long(args[0], dest.reg()),
            Intrinsic::Int32ToFloat => {
                self.emit_intrinsic_int_to_float(args[0], dest.freg(), intrinsic)
            }
            Intrinsic::Int32ToDouble => {
                self.emit_intrinsic_int_to_float(args[0], dest.freg(), intrinsic)
            }

            Intrinsic::ReinterpretInt32AsFloat => {
                self.emit_intrinsic_int_as_float(args[0], dest.freg(), intrinsic)
            }

            Intrinsic::ByteEq => {
                self.emit_intrinsic_bin_call(args[0], args[0], dest, intrinsic, pos)
            }
            Intrinsic::ByteCmp => {
                self.emit_intrinsic_bin_call(args[0], args[0], dest, intrinsic, pos)
            }
            Intrinsic::ByteNot => {
                self.emit_intrinsic_bin_call(args[0], args[0], dest, intrinsic, pos)
            }

            Intrinsic::BoolEq => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::BoolNot => self.emit_intrinsic_unary(args[0], dest, intrinsic),

            Intrinsic::Int32ToInt32 => {
                self.emit_expr(&args[0], dest);
            }

            Intrinsic::Int32Eq => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int32Cmp => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }

            Intrinsic::Int32Add => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int32Sub => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int32Mul => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int32Div => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int32Mod => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int32Neg => self.emit_intrinsic_unary(args[0], dest, intrinsic),
            Intrinsic::Int32Plus => self.emit_intrinsic_unary(args[0], dest, intrinsic),

            Intrinsic::Int32Or => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int32And => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int32Xor => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int32Not => self.emit_intrinsic_unary(args[0], dest, intrinsic),

            Intrinsic::Int32Shl => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int32Sar => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int32Shr => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }

            Intrinsic::Int32RotateLeft => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int32RotateRight => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }

            Intrinsic::Int32CountZeroBits => self.emit_intrinsic_unary(args[0], dest, intrinsic),
            Intrinsic::Int32CountOneBits => self.emit_intrinsic_unary(args[0], dest, intrinsic),
            Intrinsic::Int32CountZeroBitsLeading => {
                self.emit_intrinsic_unary(args[0], dest, intrinsic)
            }
            Intrinsic::Int32CountOneBitsLeading => {
                self.emit_intrinsic_unary(args[0], dest, intrinsic)
            }
            Intrinsic::Int32CountZeroBitsTrailing => {
                self.emit_intrinsic_unary(args[0], dest, intrinsic)
            }
            Intrinsic::Int32CountOneBitsTrailing => {
                self.emit_intrinsic_unary(args[0], dest, intrinsic)
            }

            Intrinsic::Int64Eq => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int64Cmp => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }

            Intrinsic::Int64Add => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int64Sub => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int64Mul => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int64Div => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int64Mod => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int64Neg => self.emit_intrinsic_unary(args[0], dest, intrinsic),
            Intrinsic::Int64Plus => self.emit_intrinsic_unary(args[0], dest, intrinsic),

            Intrinsic::Int64Or => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int64And => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int64Xor => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int64Not => self.emit_intrinsic_unary(args[0], dest, intrinsic),

            Intrinsic::Int64Shl => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int64Sar => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int64Shr => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }

            Intrinsic::Int64RotateLeft => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::Int64RotateRight => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }

            Intrinsic::Int64CountZeroBits => self.emit_intrinsic_unary(args[0], dest, intrinsic),
            Intrinsic::Int64CountOneBits => self.emit_intrinsic_unary(args[0], dest, intrinsic),
            Intrinsic::Int64CountZeroBitsLeading => {
                self.emit_intrinsic_unary(args[0], dest, intrinsic)
            }
            Intrinsic::Int64CountOneBitsLeading => {
                self.emit_intrinsic_unary(args[0], dest, intrinsic)
            }
            Intrinsic::Int64CountZeroBitsTrailing => {
                self.emit_intrinsic_unary(args[0], dest, intrinsic)
            }
            Intrinsic::Int64CountOneBitsTrailing => {
                self.emit_intrinsic_unary(args[0], dest, intrinsic)
            }

            Intrinsic::FloatAdd => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::FloatSub => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::FloatMul => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::FloatDiv => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::FloatNeg => self.emit_intrinsic_unary(args[0], dest, intrinsic),
            Intrinsic::FloatPlus => self.emit_intrinsic_unary(args[0], dest, intrinsic),
            Intrinsic::FloatIsNan => self.emit_intrinsic_is_nan(args[0], dest.reg(), intrinsic),
            Intrinsic::FloatSqrt => self.emit_intrinsic_sqrt(args[0], dest.freg(), intrinsic),
            Intrinsic::FloatEq => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }

            Intrinsic::DoubleAdd => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::DoubleSub => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::DoubleMul => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::DoubleDiv => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }
            Intrinsic::DoubleNeg => self.emit_intrinsic_unary(args[0], dest, intrinsic),
            Intrinsic::DoublePlus => self.emit_intrinsic_unary(args[0], dest, intrinsic),
            Intrinsic::DoubleIsNan => self.emit_intrinsic_is_nan(args[0], dest.reg(), intrinsic),
            Intrinsic::DoubleSqrt => self.emit_intrinsic_sqrt(args[0], dest.freg(), intrinsic),
            Intrinsic::DoubleEq => {
                self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic, pos)
            }

            Intrinsic::DefaultValue => self.emit_intrinsic_default_value(id, dest),

            _ => panic!("unknown intrinsic {:?}", intrinsic),
        }
    }

    fn emit_intrinsic_default_value(&mut self, id: NodeId, dest: ExprStore) {
        let ty = self.ty(id);

        match ty {
            BuiltinType::Unit => {
                assert!(dest.is_none());
            }
            BuiltinType::Bool
            | BuiltinType::UInt8
            | BuiltinType::Int
            | BuiltinType::Int64
            | BuiltinType::Char => self.asm.load_int_const(ty.mode(), dest.reg(), 0),
            BuiltinType::Float | BuiltinType::Double => {
                self.asm.load_float_const(ty.mode(), dest.freg(), 0.0)
            }
            _ => self.asm.load_nil(dest.reg()),
        }
    }

    fn emit_intrinsic_sqrt(&mut self, e: &'ast Expr, dest: FReg, intrinsic: Intrinsic) {
        self.emit_expr(e, dest.into());

        let mode = match intrinsic {
            Intrinsic::FloatSqrt => MachineMode::Float32,
            Intrinsic::DoubleSqrt => MachineMode::Float64,
            _ => unreachable!(),
        };

        self.asm.float_sqrt(mode, dest, dest);
    }

    fn emit_array_set(
        &mut self,
        pos: Position,
        element_type: BuiltinType,
        object: &'ast Expr,
        index: &'ast Expr,
        rhs: &'ast Expr,
    ) {
        self.emit_expr(object, REG_RESULT.into());
        let slot_object = self.add_temp_node(object);
        self.asm.store_mem(
            MachineMode::Ptr,
            Mem::Local(slot_object.offset()),
            REG_RESULT.into(),
        );

        self.emit_expr(index, REG_RESULT.into());
        let slot_index = self.add_temp_node(index);
        self.asm.store_mem(
            MachineMode::Int32,
            Mem::Local(slot_index.offset()),
            REG_RESULT.into(),
        );

        let value = self.emit_expr_result_reg(rhs);

        let slot_value = if element_type.is_unit() {
            None
        } else if element_type.is_tuple() {
            Some(value.stack_slot())
        } else {
            let slot_value = self.add_temp_node(rhs);
            self.asm.store_mem(
                element_type.mode(),
                Mem::Local(slot_value.offset()),
                value.any_reg(),
            );
            Some(slot_value)
        };

        let array = REG_TMP1;
        let index = REG_TMP2;

        self.asm.load_mem(
            MachineMode::Ptr,
            array.into(),
            Mem::Local(slot_object.offset()),
        );
        self.asm.load_mem(
            MachineMode::Int32,
            index.into(),
            Mem::Local(slot_index.offset()),
        );

        self.asm.test_if_nil_bailout(pos, REG_TMP1, Trap::NIL);

        if !self.vm.args.flag_omit_bounds_check {
            self.asm.check_index_out_of_bounds(pos, REG_TMP1, REG_TMP2);
        }

        if element_type.is_unit() {
            // nothing
        } else if let Some(tuple_id) = element_type.tuple_id() {
            let element_size = self.vm.tuples.lock().get_tuple(tuple_id).size();
            let slot_value = slot_value.unwrap();
            self.asm
                .array_address(REG_TMP1, REG_TMP1, REG_TMP2, element_size);

            self.copy_tuple(
                tuple_id,
                RegOrOffset::Reg(REG_TMP1),
                RegOrOffset::Offset(slot_value.offset()),
            );

            let write_barrier = self
                .vm
                .tuples
                .lock()
                .get_tuple(tuple_id)
                .contains_references();

            if self.vm.gc.needs_write_barrier() && write_barrier {
                self.asm.load_mem(
                    MachineMode::Ptr,
                    array.into(),
                    Mem::Local(slot_object.offset()),
                );

                let card_table_offset = self.vm.gc.card_table_offset();
                self.asm.emit_barrier(array, card_table_offset);
            }
        } else {
            let slot_value = slot_value.unwrap();
            let value = result_reg_ty(element_type);
            let mode = element_type.mode();

            self.asm
                .load_mem(mode, value.any_reg(), Mem::Local(slot_value.offset()));

            self.asm.store_mem(
                mode,
                Mem::Index(array, index, mode.size(), offset_of_array_data()),
                value.any_reg(),
            );

            if self.vm.gc.needs_write_barrier() && element_type.reference_type() {
                let card_table_offset = self.vm.gc.card_table_offset();
                let scratch = self.asm.get_scratch();
                self.asm.lea(
                    *scratch,
                    Mem::Index(array, index, mode.size(), offset_of_array_data()),
                );
                self.asm.emit_barrier(*scratch, card_table_offset);
            }
        }

        self.managed_stack.free_temp(slot_object, self.vm);
        self.managed_stack.free_temp(slot_index, self.vm);

        if let Some(slot_value) = slot_value {
            self.managed_stack.free_temp(slot_value, self.vm);
        }
    }

    fn emit_array_get(
        &mut self,
        pos: Position,
        element_type: BuiltinType,
        object: &'ast Expr,
        index: &'ast Expr,
        dest: ExprStore,
    ) {
        self.emit_expr(object, REG_RESULT.into());
        let slot = self.add_temp_node(object);
        self.asm.store_mem(
            MachineMode::Ptr,
            Mem::Local(slot.offset()),
            REG_RESULT.into(),
        );

        self.emit_expr(index, REG_TMP1.into());
        self.asm.load_mem(
            MachineMode::Ptr,
            REG_RESULT.into(),
            Mem::Local(slot.offset()),
        );

        self.asm.test_if_nil_bailout(pos, REG_RESULT, Trap::NIL);

        if !self.vm.args.flag_omit_bounds_check {
            self.asm
                .check_index_out_of_bounds(pos, REG_RESULT, REG_TMP1);
        }

        if element_type.is_unit() {
            assert!(dest.is_none());
        } else if let Some(tuple_id) = element_type.tuple_id() {
            let element_size = self.vm.tuples.lock().get_tuple(tuple_id).size();
            self.asm
                .array_address(REG_TMP1, REG_RESULT, REG_TMP1, element_size);

            self.copy_tuple(
                tuple_id,
                RegOrOffset::Offset(dest.stack_offset()),
                RegOrOffset::Reg(REG_TMP1),
            );
        } else {
            let res = result_reg_ty(element_type).any_reg();
            let dest = dest.any_reg();
            self.asm
                .load_array_elem(element_type.mode(), res, REG_RESULT, REG_TMP1);
            if dest != res {
                self.asm.copy(element_type.mode(), dest, res);
            }
        }

        self.managed_stack.free_temp(slot, self.vm);
    }

    fn emit_intrinsic_is_nan(&mut self, e: &'ast Expr, dest: Reg, intrinsic: Intrinsic) {
        self.emit_expr(e, FREG_RESULT.into());

        let mode = match intrinsic {
            Intrinsic::FloatIsNan => MachineMode::Float32,
            Intrinsic::DoubleIsNan => MachineMode::Float64,
            _ => unreachable!(),
        };

        self.asm.float_cmp_nan(mode, dest, FREG_RESULT);
    }

    fn emit_intrinsic_len(&mut self, pos: Position, e: &'ast Expr, dest: Reg) {
        self.emit_expr(e, REG_RESULT.into());
        self.asm.test_if_nil_bailout(pos, REG_RESULT, Trap::NIL);
        self.asm.load_mem(
            MachineMode::Ptr,
            dest.into(),
            Mem::Base(REG_RESULT, Header::size()),
        );
    }

    fn emit_intrinsic_assert(&mut self, pos: Position, e: &'ast Expr) {
        self.emit_expr(e, REG_RESULT.into());
        self.asm.assert(REG_RESULT, pos);
    }

    fn emit_intrinsic_debug(&mut self) {
        self.asm.debug();
    }

    fn emit_intrinsic_shl(&mut self, lhs: &'ast Expr, rhs: &'ast Expr, dest: Reg) {
        self.emit_expr(lhs, REG_RESULT.into());
        let slot = self.add_temp_node(lhs);
        self.asm.store_mem(
            MachineMode::Int32,
            Mem::Local(slot.offset()),
            REG_RESULT.into(),
        );

        self.emit_expr(rhs, REG_TMP1.into());
        self.asm.load_mem(
            MachineMode::Int32,
            REG_RESULT.into(),
            Mem::Local(slot.offset()),
        );

        self.asm
            .int_shl(MachineMode::Int32, dest, REG_RESULT, REG_TMP1);
        self.managed_stack.free_temp(slot, self.vm);
    }

    fn emit_intrinsic_long_to_int(&mut self, e: &'ast Expr, dest: Reg) {
        self.emit_expr(e, dest.into());
    }

    fn emit_intrinsic_long_to_byte(&mut self, e: &'ast Expr, dest: Reg) {
        self.emit_expr(e, dest.into());
        self.asm.extend_byte(MachineMode::Int32, dest, dest);
    }

    fn emit_intrinsic_int_to_byte(&mut self, e: &'ast Expr, dest: Reg) {
        self.emit_expr(e, dest.into());
        self.asm.extend_byte(MachineMode::Int32, dest, dest);
    }

    fn emit_intrinsic_int_to_long(&mut self, e: &'ast Expr, dest: Reg) {
        self.emit_expr(e, REG_RESULT.into());
        self.asm.extend_int_long(dest, REG_RESULT);
    }

    fn emit_intrinsic_float_to_double(&mut self, e: &'ast Expr, dest: FReg) {
        self.emit_expr(e, FREG_RESULT.into());
        self.asm.float_to_double(dest, FREG_RESULT);
    }

    fn emit_intrinsic_double_to_float(&mut self, e: &'ast Expr, dest: FReg) {
        self.emit_expr(e, FREG_RESULT.into());
        self.asm.double_to_float(dest, FREG_RESULT);
    }

    fn emit_intrinsic_int_to_float(&mut self, e: &'ast Expr, dest: FReg, intrinsic: Intrinsic) {
        self.emit_expr(e, REG_RESULT.into());

        let (src_mode, dest_mode) = match intrinsic {
            Intrinsic::Int32ToFloat => (MachineMode::Int32, MachineMode::Float32),
            Intrinsic::Int32ToDouble => (MachineMode::Int32, MachineMode::Float64),
            Intrinsic::Int64ToFloat => (MachineMode::Int64, MachineMode::Float32),
            Intrinsic::Int64ToDouble => (MachineMode::Int64, MachineMode::Float64),
            _ => unreachable!(),
        };

        self.asm.int_to_float(dest_mode, dest, src_mode, REG_RESULT);
    }

    fn emit_intrinsic_float_to_int(&mut self, e: &'ast Expr, dest: Reg, intrinsic: Intrinsic) {
        self.emit_expr(e, FREG_RESULT.into());

        let (src_mode, dest_mode) = match intrinsic {
            Intrinsic::FloatToInt32 => (MachineMode::Float32, MachineMode::Int32),
            Intrinsic::FloatToInt64 => (MachineMode::Float32, MachineMode::Int64),
            Intrinsic::DoubleToInt32 => (MachineMode::Float64, MachineMode::Int32),
            Intrinsic::DoubleToInt64 => (MachineMode::Float64, MachineMode::Int64),
            _ => unreachable!(),
        };

        self.asm
            .float_to_int(dest_mode, dest, src_mode, FREG_RESULT);
    }

    fn emit_intrinsic_float_as_int(&mut self, e: &'ast Expr, dest: Reg, intrinsic: Intrinsic) {
        self.emit_expr(e, FREG_RESULT.into());

        let (src_mode, dest_mode) = match intrinsic {
            Intrinsic::ReinterpretFloatAsInt => (MachineMode::Float32, MachineMode::Int32),
            Intrinsic::ReinterpretDoubleAsInt64 => (MachineMode::Float64, MachineMode::Int64),
            _ => unreachable!(),
        };

        self.asm
            .float_as_int(dest_mode, dest, src_mode, FREG_RESULT);
    }

    fn emit_intrinsic_int_as_float(&mut self, e: &'ast Expr, dest: FReg, intrinsic: Intrinsic) {
        self.emit_expr(e, REG_RESULT.into());

        let (src_mode, dest_mode) = match intrinsic {
            Intrinsic::ReinterpretInt32AsFloat => (MachineMode::Int32, MachineMode::Float32),
            Intrinsic::ReinterpretInt64AsDouble => (MachineMode::Int64, MachineMode::Float64),
            _ => unreachable!(),
        };

        self.asm.int_as_float(dest_mode, dest, src_mode, REG_RESULT);
    }

    fn emit_intrinsic_byte_to_int(&mut self, e: &'ast Expr, dest: Reg) {
        self.emit_expr(e, dest.into());
        self.asm.extend_byte(MachineMode::Int32, dest, dest);
    }

    fn emit_intrinsic_byte_to_long(&mut self, e: &'ast Expr, dest: Reg) {
        self.emit_expr(e, dest.into());
        self.asm.extend_byte(MachineMode::Int64, dest, dest);
    }

    fn emit_intrinsic_bin_call(
        &mut self,
        lhs: &'ast Expr,
        rhs: &'ast Expr,
        dest: ExprStore,
        intr: Intrinsic,
        pos: Position,
    ) {
        self.emit_intrinsic_bin(lhs, rhs, dest, intr, None, pos);
    }

    fn emit_intrinsic_bin(
        &mut self,
        lhs: &'ast Expr,
        rhs: &'ast Expr,
        dest: ExprStore,
        intr: Intrinsic,
        op: Option<BinOp>,
        pos: Position,
    ) {
        let mode = self.ty(lhs.id()).mode();

        let (lhs_reg, rhs_reg) = if mode.is_float() {
            (FREG_RESULT.into(), FREG_TMP1.into())
        } else {
            (REG_RESULT.into(), REG_TMP1.into())
        };

        self.emit_expr(lhs, lhs_reg);
        let slot = self.add_temp_node(lhs);

        self.asm
            .store_mem(mode, Mem::Local(slot.offset()), lhs_reg.any_reg());

        self.emit_expr(rhs, rhs_reg);

        self.asm
            .load_mem(mode, lhs_reg.any_reg(), Mem::Local(slot.offset()));

        if mode.is_float() {
            let lhs_reg = lhs_reg.freg();
            let rhs_reg = rhs_reg.freg();

            self.emit_intrinsic_float(dest, lhs_reg, rhs_reg, intr, op);
        } else {
            let lhs_reg = lhs_reg.reg();
            let rhs_reg = rhs_reg.reg();

            self.emit_intrinsic_int(dest.reg(), lhs_reg, rhs_reg, intr, op, pos);
        }

        self.managed_stack.free_temp(slot, self.vm);
    }

    fn emit_intrinsic_int(
        &mut self,
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
        intr: Intrinsic,
        op: Option<BinOp>,
        pos: Position,
    ) {
        match intr {
            Intrinsic::ByteEq
            | Intrinsic::BoolEq
            | Intrinsic::CharEq
            | Intrinsic::Int32Eq
            | Intrinsic::Int64Eq => {
                let mode = if intr == Intrinsic::Int64Eq {
                    MachineMode::Int64
                } else {
                    MachineMode::Int32
                };

                let cond_code = match op {
                    Some(BinOp::Cmp(cmp)) => to_cond_code(cmp),
                    _ => CondCode::Equal,
                };

                self.asm.cmp_reg(mode, lhs, rhs);
                self.asm.set(dest, cond_code);
            }

            Intrinsic::EnumEq | Intrinsic::EnumNe => {
                let cond_code = match intr {
                    Intrinsic::EnumEq => CondCode::Equal,
                    Intrinsic::EnumNe => CondCode::NotEqual,
                    _ => unreachable!(),
                };
                self.asm.cmp_reg(MachineMode::Int32, lhs, rhs);
                self.asm.set(dest, cond_code);
            }

            Intrinsic::ByteCmp | Intrinsic::CharCmp | Intrinsic::Int32Cmp | Intrinsic::Int64Cmp => {
                let mode = if intr == Intrinsic::Int64Cmp {
                    MachineMode::Int64
                } else {
                    MachineMode::Int32
                };

                if let Some(BinOp::Cmp(op)) = op {
                    let cond_code = to_cond_code(op);

                    self.asm.cmp_reg(mode, lhs, rhs);
                    self.asm.set(dest, cond_code);
                } else {
                    self.asm.int_sub(mode, dest, lhs, rhs);
                }
            }

            Intrinsic::Int32Add => self.asm.int_add(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::Int32Sub => self.asm.int_sub(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::Int32Mul => self.asm.int_mul(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::Int32Div => self.asm.int_div(MachineMode::Int32, dest, lhs, rhs, pos),
            Intrinsic::Int32Mod => self.asm.int_mod(MachineMode::Int32, dest, lhs, rhs, pos),

            Intrinsic::Int32Or => self.asm.int_or(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::Int32And => self.asm.int_and(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::Int32Xor => self.asm.int_xor(MachineMode::Int32, dest, lhs, rhs),

            Intrinsic::Int32Shl => self.asm.int_shl(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::Int32Sar => self.asm.int_sar(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::Int32Shr => self.asm.int_shr(MachineMode::Int32, dest, lhs, rhs),

            Intrinsic::Int32RotateLeft => self.asm.int_rol(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::Int32RotateRight => self.asm.int_ror(MachineMode::Int32, dest, lhs, rhs),

            Intrinsic::Int64Add => self.asm.int_add(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::Int64Sub => self.asm.int_sub(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::Int64Mul => self.asm.int_mul(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::Int64Div => self.asm.int_div(MachineMode::Int64, dest, lhs, rhs, pos),
            Intrinsic::Int64Mod => self.asm.int_mod(MachineMode::Int64, dest, lhs, rhs, pos),

            Intrinsic::Int64Or => self.asm.int_or(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::Int64And => self.asm.int_and(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::Int64Xor => self.asm.int_xor(MachineMode::Int64, dest, lhs, rhs),

            Intrinsic::Int64Shl => self.asm.int_shl(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::Int64Sar => self.asm.int_sar(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::Int64Shr => self.asm.int_shr(MachineMode::Int64, dest, lhs, rhs),

            Intrinsic::Int64RotateLeft => self.asm.int_rol(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::Int64RotateRight => self.asm.int_ror(MachineMode::Int64, dest, lhs, rhs),

            _ => panic!("unexpected intrinsic {:?}", intr),
        }
    }

    fn emit_intrinsic_float(
        &mut self,
        dest: ExprStore,
        lhs: FReg,
        rhs: FReg,
        intr: Intrinsic,
        op: Option<BinOp>,
    ) {
        use crate::ty::MachineMode::{Float32, Float64};

        match intr {
            Intrinsic::FloatEq | Intrinsic::DoubleEq => {
                let mode = if intr == Intrinsic::DoubleEq {
                    Float64
                } else {
                    Float32
                };

                let cond_code = match op {
                    Some(BinOp::Cmp(cmp)) => to_cond_code(cmp),
                    _ => CondCode::Equal,
                };

                self.asm.float_cmp(mode, dest.reg(), lhs, rhs, cond_code);
            }

            Intrinsic::FloatCmp | Intrinsic::DoubleCmp => {
                let mode = if intr == Intrinsic::DoubleCmp {
                    Float64
                } else {
                    Float32
                };

                if let Some(BinOp::Cmp(op)) = op {
                    let cond_code = to_cond_code(op);

                    self.asm.float_cmp(mode, dest.reg(), lhs, rhs, cond_code);
                } else {
                    unimplemented!();
                }
            }

            Intrinsic::FloatAdd => self.asm.float_add(Float32, dest.freg(), lhs, rhs),
            Intrinsic::FloatSub => self.asm.float_sub(Float32, dest.freg(), lhs, rhs),
            Intrinsic::FloatMul => self.asm.float_mul(Float32, dest.freg(), lhs, rhs),
            Intrinsic::FloatDiv => self.asm.float_div(Float32, dest.freg(), lhs, rhs),

            Intrinsic::DoubleAdd => self.asm.float_add(Float64, dest.freg(), lhs, rhs),
            Intrinsic::DoubleSub => self.asm.float_sub(Float64, dest.freg(), lhs, rhs),
            Intrinsic::DoubleMul => self.asm.float_mul(Float64, dest.freg(), lhs, rhs),
            Intrinsic::DoubleDiv => self.asm.float_div(Float64, dest.freg(), lhs, rhs),

            _ => panic!("unexpected intrinsic {:?}", intr),
        }
    }

    fn emit_delegation(&mut self, e: &'ast ExprDelegationType, dest: ExprStore) {
        let mut args = e.args.iter().map(|arg| Arg::Expr(arg)).collect::<Vec<_>>();
        args.insert(0, Arg::Selfie);

        let call_site = self.build_call_site_id(e.id, args, None);
        self.emit_call_site(&call_site, e.pos, dest);
    }

    pub fn emit_call_site(&mut self, csite: &CallSite<'ast>, pos: Position, dest: ExprStore) {
        let mut temps: Vec<SlotOrOffset> = Vec::new();
        let mut alloc_cls_id: Option<ClassDefId> = None;

        let fid = csite.callee;
        let fct = self.vm.fcts.idx(fid);
        let fct = fct.read();

        for (idx, arg) in csite.args.iter().enumerate() {
            let slot_or_offset = match *arg {
                InternalArg::Expr(ast, ty) => {
                    let ty = self.specialize_type(ty);
                    let dest = self.alloc_expr_store(ty);
                    self.emit_expr(ast, dest);

                    // check first argument for nil for method calls
                    //
                    // no check necessary for:
                    //   super calls (guaranteed to not be nil) and
                    //   dynamic dispatch (implicit check when loading fctptr from vtable)
                    if idx == 0
                        && fct.has_self()
                        && check_for_nil(ty)
                        && !csite.super_call
                        && !fct.is_virtual()
                    {
                        self.asm.test_if_nil_bailout(pos, dest.reg(), Trap::NIL);
                    }

                    if ty.is_tuple() {
                        SlotOrOffset::Slot(dest.stack_slot())
                    } else if ty.is_unit() {
                        SlotOrOffset::Uninitialized
                    } else {
                        let slot = self.add_temp_arg(arg);
                        self.asm.store_mem(
                            arg.ty().mode(),
                            Mem::Local(slot.offset()),
                            dest.any_reg(),
                        );
                        SlotOrOffset::Slot(slot)
                    }
                }

                InternalArg::Array(ast, ty) => {
                    let ty = self.specialize_type(ty);
                    let dest = self.alloc_expr_store(ty);
                    self.emit_expr(ast, dest);

                    if ty.is_tuple() {
                        SlotOrOffset::Slot(dest.stack_slot())
                    } else if ty.is_unit() {
                        SlotOrOffset::Uninitialized
                    } else {
                        let slot = self.add_temp_arg(arg);
                        self.asm.store_mem(
                            arg.ty().mode(),
                            Mem::Local(slot.offset()),
                            dest.any_reg(),
                        );
                        SlotOrOffset::Slot(slot)
                    }
                }

                InternalArg::Stack(offset, _) => SlotOrOffset::Offset(offset),

                InternalArg::Selfie(_) => {
                    let var = self.src.var_self();
                    let offset = self.var_offset(var.id);
                    SlotOrOffset::Offset(offset)
                }

                InternalArg::SelfieNew(ty) => {
                    alloc_cls_id = Some(specialize_class_ty(self.vm, ty));
                    // we have to allocate the object first before we can create
                    // the slot, store uninitialized for now.
                    SlotOrOffset::Uninitialized
                }
            };

            temps.push(slot_or_offset);
        }

        let argsize = self.determine_call_stack(&csite.args, csite.variadic_array.is_some());
        self.asm.increase_stack_frame(argsize);

        let mut sp_offset = 0;
        let mut idx = 0;
        let mut reg_idx = 0;
        let mut freg_idx = 0;

        let skip = if csite.args.len() > 0 && alloc_cls_id.is_some() {
            assert!(csite.args[0].is_selfie_new());
            assert!(temps[idx].is_uninitialized());

            let length: ArrayLength = if temps.len() > 1 {
                ArrayLength::Dynamic(temps[1].clone())
            } else {
                ArrayLength::Fixed(0)
            };

            let slot = self.emit_allocation(pos, length, alloc_cls_id.unwrap());

            self.asm.load_mem(
                MachineMode::Ptr,
                REG_PARAMS[reg_idx].into(),
                Mem::Local(slot.offset()),
            );

            // after object allocation we now have a slot and can
            // store it in our `temps` array.
            temps[idx] = SlotOrOffset::Slot(slot);

            reg_idx += 1;
            idx += 1;

            1
        } else {
            0
        };

        let variadic_slot: Option<ManagedStackSlot> =
            if let Some(ref variadic_array) = csite.variadic_array {
                let length = ArrayLength::Fixed(variadic_array.count as i32);
                let slot = self.emit_allocation(pos, length, variadic_array.cls_def_id);
                Some(slot)
            } else {
                None
            };

        let return_type = self.specialize_type(csite.return_type);

        if return_type.is_tuple() {
            self.asm
                .lea(REG_PARAMS[reg_idx], Mem::Local(dest.stack_offset()));
            reg_idx += 1;
        }

        for arg in csite.args.iter().skip(skip) {
            let ty = arg.ty();

            if ty.is_unit() {
                assert!(temps[idx].is_uninitialized());
                idx += 1;
                continue;
            }

            let offset = temps[idx].offset();

            if csite.variadic_array.is_some() && idx >= csite.variadic_array.as_ref().unwrap().start
            {
                let array_idx = idx - csite.variadic_array.as_ref().unwrap().start;
                self.emit_variadic_argument(
                    ty,
                    array_idx,
                    variadic_slot.unwrap(),
                    temps[idx].clone(),
                );
            } else if ty.is_tuple() {
                if reg_idx < REG_PARAMS.len() {
                    let reg = REG_PARAMS[reg_idx];
                    self.asm.lea(reg, Mem::Local(offset));

                    reg_idx += 1;
                } else {
                    self.asm.lea(REG_TMP1, Mem::Local(offset));
                    self.asm.store_mem(
                        MachineMode::Ptr,
                        Mem::Base(REG_SP, sp_offset),
                        REG_TMP1.into(),
                    );

                    sp_offset += 8;
                }
            } else if ty.is_float() {
                let mode = ty.mode();

                if freg_idx < FREG_PARAMS.len() {
                    let freg = FREG_PARAMS[freg_idx];
                    self.asm.load_mem(mode, freg.into(), Mem::Local(offset));

                    freg_idx += 1;
                } else {
                    self.asm
                        .load_mem(mode, FREG_TMP1.into(), Mem::Local(offset));
                    self.asm
                        .store_mem(mode, Mem::Base(REG_SP, sp_offset), FREG_TMP1.into());

                    sp_offset += 8;
                }
            } else {
                let mode = ty.mode();

                if reg_idx < REG_PARAMS.len() {
                    let reg = REG_PARAMS[reg_idx];
                    self.asm.load_mem(mode, reg.into(), Mem::Local(offset));

                    reg_idx += 1;
                } else {
                    self.asm.load_mem(mode, REG_TMP1.into(), Mem::Local(offset));
                    self.asm
                        .store_mem(mode, Mem::Base(REG_SP, sp_offset), REG_TMP1.into());

                    sp_offset += 8;
                }
            }

            idx += 1;
        }

        if let Some(slot) = variadic_slot {
            if reg_idx < REG_PARAMS.len() {
                let reg = REG_PARAMS[reg_idx];
                self.asm
                    .load_mem(MachineMode::Ptr, reg.into(), Mem::Local(slot.offset()));
            } else {
                self.asm
                    .load_mem(MachineMode::Ptr, REG_TMP1.into(), Mem::Local(slot.offset()));
                self.asm.store_mem(
                    MachineMode::Ptr,
                    Mem::Base(REG_SP, sp_offset),
                    REG_TMP1.into(),
                );
            }
        }

        let cls_type_params = TypeList::with(
            csite
                .cls_type_params
                .iter()
                .map(|ty| self.specialize_type(ty))
                .collect::<Vec<_>>(),
        );
        let fct_type_params = TypeList::with(
            csite
                .fct_type_params
                .iter()
                .map(|ty| self.specialize_type(ty))
                .collect::<Vec<_>>(),
        );

        debug_assert!(cls_type_params
            .iter()
            .all(|ty| !ty.contains_type_param(self.vm)));
        debug_assert!(fct_type_params
            .iter()
            .all(|ty| !ty.contains_type_param(self.vm)));

        let (result, result_type): (AnyReg, BuiltinType) = if return_type.is_tuple() {
            (REG_RESULT.into(), BuiltinType::Unit)
        } else if return_type.is_unit() {
            (REG_RESULT.into(), BuiltinType::Unit)
        } else {
            (dest.any_reg(), return_type)
        };

        if csite.super_call {
            let ptr = self.ptr_for_fct_id(fid, cls_type_params.clone(), fct_type_params.clone());
            let name = fct.full_name(self.vm);
            self.asm.emit_comment(format!("call super {}", name));
            let gcpoint = self.create_gcpoint();
            self.asm.direct_call(
                fid,
                ptr.to_ptr(),
                cls_type_params,
                fct_type_params,
                pos,
                gcpoint,
                result_type,
                result,
            );
        } else if fct.is_virtual() {
            let vtable_index = fct.vtable_index.unwrap();
            let name = fct.full_name(self.vm);
            self.asm.emit_comment(format!("call virtual {}", name));
            let gcpoint = self.create_gcpoint();
            let cls_type_params = csite.args[0].ty().type_params(self.vm);
            self.asm.indirect_call(
                vtable_index,
                pos,
                gcpoint,
                result_type,
                cls_type_params,
                result,
            );
        } else {
            let ptr = self.ptr_for_fct_id(fid, cls_type_params.clone(), fct_type_params.clone());
            let name = fct.full_name(self.vm);
            self.asm.emit_comment(format!("call direct {}", name));
            let gcpoint = self.create_gcpoint();
            self.asm.direct_call(
                fid,
                ptr.to_ptr(),
                cls_type_params,
                fct_type_params,
                pos,
                gcpoint,
                result_type,
                result,
            );
        }

        if csite.args.len() > 0 {
            if let InternalArg::SelfieNew(ty) = csite.args[0] {
                let temp = &temps[0];
                self.asm
                    .load_mem(ty.mode(), dest.any_reg(), Mem::Local(temp.offset()));
            }
        }

        for temp in temps.into_iter() {
            if let SlotOrOffset::Slot(slot) = temp {
                self.managed_stack.free_temp(slot, self.vm);
            }
        }

        if let Some(variadic_slot) = variadic_slot {
            self.managed_stack.free_temp(variadic_slot, self.vm);
        }

        if csite.return_type.is_tuple() {
            self.managed_stack.mark_initialized(dest.stack_slot().var);
        }

        self.asm.decrease_stack_frame(argsize);
    }

    fn emit_variadic_argument(
        &mut self,
        ty: BuiltinType,
        array_idx: usize,
        array: ManagedStackSlot,
        arg: SlotOrOffset,
    ) {
        // load array
        self.asm.load_mem(
            MachineMode::Ptr,
            REG_TMP1.into(),
            Mem::Local(array.offset()),
        );

        let header_size = Header::size() + mem::ptr_width();
        let array_offset = header_size + array_idx as i32 * ty.size(self.vm);

        let needs_write_barrier = if let Some(tuple_id) = ty.tuple_id() {
            self.copy_tuple(
                tuple_id,
                RegOrOffset::RegWithOffset(REG_TMP1, array_offset),
                RegOrOffset::Offset(arg.offset()),
            );

            self.vm
                .tuples
                .lock()
                .get_tuple(tuple_id)
                .contains_references()
        } else if ty.is_unit() {
            false
        } else if ty.is_float() {
            let mode = ty.mode();
            self.asm
                .load_mem(mode, FREG_TMP1.into(), Mem::Local(arg.offset()));
            self.asm
                .store_mem(mode, Mem::Base(REG_TMP1, array_offset), FREG_TMP1.into());
            false
        } else {
            let mode = ty.mode();
            self.asm
                .load_mem(mode, REG_TMP2.into(), Mem::Local(arg.offset()));
            self.asm
                .store_mem(mode, Mem::Base(REG_TMP1, array_offset), REG_TMP2.into());
            ty.reference_type()
        };

        if self.vm.gc.needs_write_barrier() && needs_write_barrier {
            let card_table_offset = self.vm.gc.card_table_offset();
            self.asm.emit_barrier(REG_TMP1, card_table_offset);
        }
    }

    fn emit_allocation(
        &mut self,
        pos: Position,
        length: ArrayLength,
        cls_id: ClassDefId,
    ) -> ManagedStackSlot {
        let cls = self.vm.class_defs.idx(cls_id);
        let cls = cls.read();
        let mut store_length = false;

        // allocate storage for object
        {
            let name = cls.name(self.vm);
            self.asm
                .emit_comment(format!("allocate object of class {}", &name));
        }

        let array_header_size = Header::size() as usize + mem::ptr_width_usize();
        let mut array_size = 0;

        let element_size = cls.size.element_size();

        let alloc_size: AllocationSize = match cls.size {
            InstanceSize::Fixed(size) => {
                self.asm
                    .load_int_const(MachineMode::Int32, REG_PARAMS[0], size as i64);
                AllocationSize::Fixed(size as usize)
            }

            InstanceSize::PrimitiveArray(_)
            | InstanceSize::Str
            | InstanceSize::ObjArray
            | InstanceSize::TupleArray(_) => {
                store_length = true;

                match length.clone() {
                    ArrayLength::Fixed(length) => {
                        let unaligned_array_size =
                            array_header_size + (element_size.unwrap() * length) as usize;
                        array_size = mem::align_usize(unaligned_array_size, mem::ptr_width_usize());
                        AllocationSize::Fixed(array_size)
                    }

                    ArrayLength::Dynamic(slot_or_offset) => {
                        self.asm.load_mem(
                            MachineMode::Int32,
                            REG_TMP1.into(),
                            Mem::Local(slot_or_offset.offset()),
                        );
                        self.asm.determine_array_size(
                            REG_TMP1,
                            REG_TMP1,
                            element_size.unwrap(),
                            true,
                        );
                        AllocationSize::Dynamic(REG_TMP1)
                    }
                }
            }

            InstanceSize::UnitArray => {
                store_length = true;
                AllocationSize::Fixed(array_header_size)
            }

            InstanceSize::FreeArray => unreachable!(),
        };

        let array_ref = match cls.size {
            InstanceSize::ObjArray => true,
            _ => false,
        };

        let dest = REG_TMP1;

        let gcpoint = self.create_gcpoint();
        self.asm.allocate(dest, alloc_size, pos, array_ref, gcpoint);

        // store gc object in temporary storage
        let temp_slot = self.managed_stack.add_temp(BuiltinType::Ptr, self.vm);
        self.asm.store_mem(
            MachineMode::Ptr,
            Mem::Local(temp_slot.offset()),
            dest.into(),
        );

        // store classptr in object
        let cptr = (&**cls.vtable.as_ref().unwrap()) as *const VTable as *const u8;
        let disp = self.asm.add_addr(cptr);
        let pos = self.asm.pos() as i32;

        let temp = REG_TMP2;

        let name = cls.name(self.vm);
        self.asm
            .emit_comment(format!("store vtable ptr for class {} in object", name));
        self.asm.load_constpool(temp, disp + pos);
        self.asm
            .store_mem(MachineMode::Ptr, Mem::Base(dest, 0), temp.into());

        // clear mark/fwdptr word in header
        assert!(Header::size() == 2 * mem::ptr_width());
        self.asm.load_int_const(MachineMode::Ptr, temp, 0);
        self.asm.store_mem(
            MachineMode::Ptr,
            Mem::Base(dest, mem::ptr_width()),
            temp.into(),
        );

        // store length in object
        if store_length {
            match length.clone() {
                ArrayLength::Dynamic(slot_or_offset) => {
                    self.asm.load_mem(
                        MachineMode::Int32,
                        temp.into(),
                        Mem::Local(slot_or_offset.offset()),
                    );
                }

                ArrayLength::Fixed(length) => {
                    self.asm
                        .load_int_const(MachineMode::Ptr, temp, length as i64);
                }
            }

            self.asm.store_mem(
                MachineMode::Ptr,
                Mem::Base(dest, Header::size()),
                temp.into(),
            );
        }

        match cls.size {
            InstanceSize::Fixed(size) => {
                self.asm.fill_zero(dest, false, size as usize);
            }

            InstanceSize::UnitArray => {
                // Array[()] never contains any data, so no zeroing needed.
            }

            InstanceSize::PrimitiveArray(_)
            | InstanceSize::Str
            | InstanceSize::ObjArray
            | InstanceSize::TupleArray(_) => match length.clone() {
                ArrayLength::Fixed(_) => {
                    self.asm.fill_zero(dest, true, array_size);
                }

                ArrayLength::Dynamic(_) => {
                    self.asm
                        .int_add_imm(MachineMode::Ptr, dest, dest, array_header_size as i64);

                    self.asm
                        .determine_array_size(temp, temp, element_size.unwrap(), false);
                    self.asm.int_add(MachineMode::Ptr, temp, temp, dest);
                    self.asm.fill_zero_dynamic(dest, temp);
                }
            },

            InstanceSize::FreeArray => unreachable!(),
        }

        temp_slot
    }

    fn specialize_type(&self, ty: BuiltinType) -> BuiltinType {
        replace_type_param(
            self.vm,
            ty,
            self.cls_type_params,
            self.fct_type_params,
            None,
        )
    }

    fn determine_call_stack(
        &mut self,
        args: &[InternalArg<'ast>],
        variadic_arguments: bool,
    ) -> i32 {
        let mut reg_args: i32 = 0;
        let mut freg_args: i32 = 0;

        for arg in args {
            match *arg {
                InternalArg::Array(_, _) => {}
                InternalArg::Expr(_, ty) => {
                    if ty.is_float() {
                        freg_args += 1;
                    } else {
                        reg_args += 1;
                    }
                }

                InternalArg::Stack(_, ty)
                | InternalArg::Selfie(ty)
                | InternalArg::SelfieNew(ty) => {
                    if ty.is_float() {
                        freg_args += 1;
                    } else {
                        reg_args += 1;
                    }
                }
            }
        }

        if variadic_arguments {
            reg_args += 1;
        }

        // some register are reserved on stack
        let args_on_stack = max(0, reg_args - REG_PARAMS.len() as i32)
            + max(0, freg_args - FREG_PARAMS.len() as i32);

        mem::align_i32(mem::ptr_width() * args_on_stack, 16)
    }

    fn build_call_site(
        &mut self,
        call_type: &CallType,
        callee_id: FctId,
        args: Vec<Arg<'ast>>,
    ) -> CallSite<'ast> {
        let callee = self.vm.fcts.idx(callee_id);
        let callee = callee.read();

        let (args, variadic_array, return_type, super_call) =
            self.determine_call_args_and_types(&*call_type, &*callee, args);
        let (cls_type_params, fct_type_params) = self.determine_call_type_params(&*call_type);

        CallSite {
            callee: callee_id,
            args,
            variadic_array,
            cls_type_params,
            fct_type_params,
            super_call,
            return_type,
        }
    }

    fn build_call_site_id(
        &mut self,
        id: NodeId,
        args: Vec<Arg<'ast>>,
        callee_id: Option<FctId>,
    ) -> CallSite<'ast> {
        let call_type = self.src.map_calls.get(id).unwrap().clone();

        let callee_id = if let Some(callee_id) = callee_id {
            callee_id
        } else {
            call_type.fct_id().unwrap()
        };

        self.build_call_site(&*call_type, callee_id, args)
    }

    fn determine_call_args_and_types(
        &mut self,
        call_type: &CallType,
        callee: &Fct<'ast>,
        args: Vec<Arg<'ast>>,
    ) -> (
        Vec<InternalArg<'ast>>,
        Option<VariadicArrayDescriptor>,
        BuiltinType,
        bool,
    ) {
        let mut super_call = false;

        let callee_params = callee.params_with_self();

        let variadic_argument_start = if callee.variadic_arguments {
            callee_params.len() - 1
        } else {
            args.len()
        };

        let args = args
            .iter()
            .enumerate()
            .map(|(ind, arg)| {
                let ty = if ind >= variadic_argument_start {
                    callee_params.last().cloned().unwrap()
                } else {
                    callee_params[ind]
                };
                let ty = self.specialize_type(specialize_for_call_type(call_type, ty, self.vm));

                match *arg {
                    Arg::Expr(ast) => {
                        if ind == 0 && ast.is_super() {
                            super_call = true;
                        }

                        if ind >= variadic_argument_start {
                            InternalArg::Array(ast, ty)
                        } else {
                            InternalArg::Expr(ast, ty)
                        }
                    }
                    Arg::Stack(offset) => InternalArg::Stack(offset, ty),
                    Arg::SelfieNew => InternalArg::SelfieNew(ty),
                    Arg::Selfie => InternalArg::Selfie(ty),
                }
            })
            .collect::<Vec<_>>();

        let return_type = self.specialize_type(specialize_for_call_type(
            call_type,
            callee.return_type,
            self.vm,
        ));

        let variadic_array: Option<VariadicArrayDescriptor> = if callee.variadic_arguments {
            let ty = callee_params.last().cloned().unwrap();
            let ty = self.specialize_type(specialize_for_call_type(call_type, ty, self.vm));
            let ty = self.vm.vips.array_ty(self.vm, ty);
            let cls_def_id = specialize_class_ty(self.vm, ty);

            Some(VariadicArrayDescriptor {
                cls_def_id,
                start: variadic_argument_start,
                count: args.len() - variadic_argument_start,
            })
        } else {
            None
        };

        (args, variadic_array, return_type, super_call)
    }

    fn determine_call_type_params(&mut self, call_type: &CallType) -> (TypeList, TypeList) {
        let cls_type_params;
        let fct_type_params;

        match *call_type {
            CallType::Ctor(ty, _) | CallType::CtorNew(ty, _) => {
                cls_type_params = ty.type_params(self.vm);
                fct_type_params = TypeList::empty();
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

            CallType::Expr(ty, _) => {
                let ty = self.specialize_type(ty);

                cls_type_params = ty.type_params(self.vm);
                fct_type_params = TypeList::empty();
            }

            CallType::Trait(_, _) => unimplemented!(),

            CallType::TraitStatic(_, _, _) => {
                cls_type_params = TypeList::empty();
                fct_type_params = TypeList::empty();
            }

            CallType::Intrinsic(_) => unreachable!(),
        }

        (cls_type_params, fct_type_params)
    }

    fn ty(&self, id: NodeId) -> BuiltinType {
        let ty = self.src.ty(id);
        self.specialize_type(ty)
    }

    fn var_ty(&self, id: VarId) -> BuiltinType {
        let ty = self.src.vars[id].ty;
        self.specialize_type(ty)
    }

    fn alloc_expr_store(&mut self, ty: BuiltinType) -> ExprStore {
        if ty.is_tuple() {
            let slot = self.managed_stack.add_temp_uninitialized(ty, self.vm);
            ExprStore::Stack(slot)
        } else if ty.is_unit() {
            ExprStore::None
        } else if ty.is_float() {
            ExprStore::FloatReg(FREG_RESULT)
        } else {
            ExprStore::Reg(REG_RESULT)
        }
    }

    fn free_expr_store(&mut self, store: ExprStore) {
        match store {
            ExprStore::Stack(slot) => self.managed_stack.free_temp(slot, self.vm),
            ExprStore::FloatReg(_) => {}
            ExprStore::Reg(_) => {}
            ExprStore::None => {}
        }
    }
}

impl<'a, 'ast> visit::Visitor<'ast> for AstCodeGen<'a, 'ast> {
    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtExpr(ref stmt) => self.emit_stmt_expr(stmt),
            StmtWhile(ref stmt) => self.emit_stmt_while(stmt),
            StmtFor(ref stmt) => self.emit_stmt_for(stmt),
            StmtReturn(ref stmt) => self.emit_stmt_return(stmt),
            StmtBreak(ref stmt) => self.emit_stmt_break(stmt),
            StmtContinue(ref stmt) => self.emit_stmt_continue(stmt),
            StmtVar(ref stmt) => self.emit_stmt_var(stmt),
        }
    }

    fn visit_expr(&mut self, _: &'ast Expr) {
        unreachable!("should not be invoked");
    }
}

#[derive(Clone)]
enum ArrayLength {
    Fixed(i32),
    Dynamic(SlotOrOffset),
}

impl ArrayLength {
    fn is_dynamic(&self) -> bool {
        match self {
            ArrayLength::Dynamic(_) => true,
            ArrayLength::Fixed(_) => false,
        }
    }

    fn to_dynamic(&self) -> Option<SlotOrOffset> {
        match self {
            ArrayLength::Dynamic(slot_or_offset) => Some(slot_or_offset.clone()),
            ArrayLength::Fixed(_) => None,
        }
    }

    fn to_fixed(&self) -> Option<i32> {
        match *self {
            ArrayLength::Fixed(val) => Some(val),
            ArrayLength::Dynamic(_) => None,
        }
    }
}

enum RegOrOffset {
    Reg(Reg),
    RegWithOffset(Reg, i32),
    Offset(i32),
}

fn result_reg_mode(mode: MachineMode) -> ExprStore {
    if mode.is_float() {
        ExprStore::FloatReg(FREG_RESULT)
    } else {
        ExprStore::Reg(REG_RESULT)
    }
}

fn result_reg_ty(ty: BuiltinType) -> ExprStore {
    if ty.is_unit() {
        ExprStore::None
    } else if ty.is_float() {
        ExprStore::FloatReg(FREG_RESULT)
    } else {
        ExprStore::Reg(REG_RESULT)
    }
}

fn check_for_nil(ty: BuiltinType) -> bool {
    match ty {
        BuiltinType::Error => unreachable!(),
        BuiltinType::Any => unreachable!(),
        BuiltinType::Unit => false,
        BuiltinType::UInt8
        | BuiltinType::Char
        | BuiltinType::Int
        | BuiltinType::Int32
        | BuiltinType::Int64
        | BuiltinType::Float
        | BuiltinType::Double
        | BuiltinType::Bool
        | BuiltinType::Enum(_, _) => false,
        BuiltinType::Nil | BuiltinType::Ptr => true,
        BuiltinType::Class(_, _) => true,
        BuiltinType::Struct(_, _) => false,
        BuiltinType::Trait(_) => false,
        BuiltinType::Module(_) => false,
        BuiltinType::This => unreachable!(),
        BuiltinType::ClassTypeParam(_, _) => unreachable!(),
        BuiltinType::FctTypeParam(_, _) => unreachable!(),
        BuiltinType::Lambda(_) => true,
        BuiltinType::Tuple(_) => false,
    }
}

fn ensure_jit_or_stub_ptr<'ast>(
    src: &FctSrc,
    vm: &VM,
    cls_type_params: TypeList,
    fct_type_params: TypeList,
) -> Address {
    let specials = src.specializations.read();
    let key = (cls_type_params, fct_type_params);

    if let Some(&jit_fct_id) = specials.get(&key) {
        let jit_fct = vm.jit_fcts.idx(jit_fct_id);
        return jit_fct.instruction_start();
    }

    vm.compile_stub()
}

fn to_cond_code(cmp: CmpOp) -> CondCode {
    match cmp {
        CmpOp::Eq => CondCode::Equal,
        CmpOp::Ne => CondCode::NotEqual,
        CmpOp::Gt => CondCode::Greater,
        CmpOp::Ge => CondCode::GreaterEq,
        CmpOp::Lt => CondCode::Less,
        CmpOp::Le => CondCode::LessEq,
        CmpOp::Is => CondCode::Equal,
        CmpOp::IsNot => CondCode::NotEqual,
    }
}

#[derive(Clone)]
enum SlotOrOffset {
    Uninitialized,
    Slot(ManagedStackSlot),
    Offset(i32),
}

impl SlotOrOffset {
    fn is_uninitialized(&self) -> bool {
        match self {
            &SlotOrOffset::Uninitialized => true,
            _ => false,
        }
    }

    fn offset(&self) -> i32 {
        match *self {
            SlotOrOffset::Uninitialized => panic!("uninitialized"),
            SlotOrOffset::Slot(slot) => slot.offset(),
            SlotOrOffset::Offset(offset) => offset,
        }
    }
}
