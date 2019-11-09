use crate::baseline::asm::BaselineAssembler;
use crate::baseline::codegen::fct_pattern_match;
use crate::bytecode::generate::BytecodeIdx;
use crate::class::ClassDefId;
use crate::cpu::{Mem, FREG_PARAMS, FREG_RESULT, FREG_TMP1, REG_PARAMS, REG_RESULT, REG_TMP1};
use crate::field::FieldId;
use dora_parser::ast::*;
use std::collections::hash_map::HashMap;

use crate::baseline::codegen::{should_emit_debug, CodeGen, CondCode};
use crate::baseline::fct::{Comment, JitBaselineFct, JitDescriptor};
use crate::masm::*;
use crate::object::Str;
use crate::ty::TypeList;
use crate::vm::VM;
use crate::vm::{Fct, FctSrc, GlobalId};

use crate::bytecode::astgen::generate_fct;
use crate::bytecode::generate::{BytecodeFunction, BytecodeType, Register, StrConstPoolIdx};
use crate::bytecode::opcode::Bytecode;

pub struct ForwardJump {
    label: Label,
    bytecode_idx: BytecodeIdx,
}

pub struct CannonCodeGen<'a, 'ast: 'a> {
    pub vm: &'a VM<'ast>,
    pub fct: &'a Fct<'ast>,
    pub ast: &'ast Function,
    pub asm: BaselineAssembler<'a, 'ast>,
    pub src: &'a mut FctSrc,

    pub lbl_break: Option<Label>,
    pub lbl_continue: Option<Label>,

    // stores all active finally blocks
    pub active_finallys: Vec<&'ast Stmt>,

    // label to jump instead of emitting epilog for return
    // needed for return's in finally blocks
    // return in finally needs to execute to next finally block and not
    // leave the current function
    pub lbl_return: Option<Label>,

    // length of active_finallys in last loop
    // default: 0
    // break/continue need to emit finally blocks up to the last loop
    // see tests/finally/break-while.dora
    pub active_loop: Option<usize>,

    // upper length of active_finallys in emitting finally-blocks for break/continue
    // default: active_finallys.len()
    // break/continue needs to execute finally-blocks in loop, return in these blocks
    // would dump all active_finally-entries from the loop but we need an upper bound.
    // see emit_finallys_within_loop and tests/finally/continue-return.dora
    pub active_upper: Option<usize>,

    pub cls_type_params: &'a TypeList,
    pub fct_type_params: &'a TypeList,

    bytecode_to_address: HashMap<BytecodeIdx, usize>,
    forward_jumps: Vec<ForwardJump>,
    current_pos: Option<BytecodeIdx>,
}

impl<'a, 'ast> CannonCodeGen<'a, 'ast>
where
    'ast: 'a,
{
    pub fn new(
        vm: &'a VM<'ast>,
        fct: &'a Fct<'ast>,
        ast: &'ast Function,
        asm: BaselineAssembler<'a, 'ast>,
        src: &'a mut FctSrc,
        lbl_break: Option<Label>,
        lbl_continue: Option<Label>,
        active_finallys: Vec<&'ast Stmt>,
        lbl_return: Option<Label>,
        active_loop: Option<usize>,
        active_upper: Option<usize>,
        cls_type_params: &'a TypeList,
        fct_type_params: &'a TypeList,
    ) -> CannonCodeGen<'a, 'ast> {
        CannonCodeGen {
            vm,
            fct,
            ast,
            asm,
            src,
            lbl_break,
            lbl_continue,
            active_finallys,
            active_upper,
            active_loop,
            lbl_return,
            cls_type_params,
            fct_type_params,
            bytecode_to_address: HashMap::new(),
            forward_jumps: Vec::new(),
            current_pos: None,
        }
    }

    fn store_params_on_stack(&mut self, bytecode: &BytecodeFunction) {
        let mut reg_idx = 0;
        let mut freg_idx = 0;
        let mut sp_offset = 16;
        let mut idx = 0;

        for param in self.fct.params_with_self() {
            let dest = Register(idx);
            assert_eq!(bytecode.register(dest), (*param).into());

            // let bytecode_type = bytecode.register(dest);
            let offset = bytecode.offset(dest);

            let reg = match param.mode().is_float() {
                true if freg_idx < FREG_PARAMS.len() => {
                    let freg = FREG_PARAMS[freg_idx].into();
                    freg_idx += 1;
                    Some(freg)
                }
                false if reg_idx < REG_PARAMS.len() => {
                    let reg = REG_PARAMS[reg_idx].into();
                    reg_idx += 1;
                    Some(reg)
                }
                _ => None,
            };

            match reg {
                Some(dest) => {
                    self.asm.store_mem(param.mode(), Mem::Local(offset), dest);
                }
                None => {
                    let dest = if param.mode().is_float() {
                        FREG_RESULT.into()
                    } else {
                        REG_RESULT.into()
                    };
                    self.asm.load_mem(param.mode(), dest, Mem::Local(sp_offset));
                    self.asm.store_mem(param.mode(), Mem::Local(offset), dest);
                    sp_offset += 8;
                }
            }

            idx += 1;
        }
    }

    fn emit_prolog(&mut self, bytecode: &BytecodeFunction) {
        self.asm.prolog_size(bytecode.stacksize(), self.fct.ast.pos);
        self.asm.emit_comment(Comment::Lit("prolog end"));
        self.asm.emit_comment(Comment::Newline);
    }

    fn emit_epilog(&mut self) {
        self.asm.emit_comment(Comment::Newline);
        self.asm.emit_comment(Comment::Lit("epilog"));

        let polling_page = self.vm.polling_page.addr();
        self.asm.safepoint(polling_page);
        self.asm.epilog();
    }

    fn emit_add_int(
        &mut self,
        bytecode: &BytecodeFunction,
        dest: Register,
        lhs: Register,
        rhs: Register,
    ) {
        assert_eq!(bytecode.register(lhs), bytecode.register(rhs));
        assert_eq!(bytecode.register(lhs), bytecode.register(dest));

        let bytecode_type = bytecode.register(lhs);
        let offset = bytecode.offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(rhs);
        let offset = bytecode.offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);
        self.asm
            .int_add(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_add_float(
        &mut self,
        bytecode: &BytecodeFunction,
        dest: Register,
        lhs: Register,
        rhs: Register,
    ) {
        assert_eq!(bytecode.register(lhs), bytecode.register(rhs));
        assert_eq!(bytecode.register(lhs), bytecode.register(dest));

        let bytecode_type = bytecode.register(lhs);
        let offset = bytecode.offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), FREG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(rhs);
        let offset = bytecode.offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), FREG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);
        self.asm
            .float_add(bytecode_type.mode(), FREG_RESULT, FREG_RESULT, FREG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_sub_int(
        &mut self,
        bytecode: &BytecodeFunction,
        dest: Register,
        lhs: Register,
        rhs: Register,
    ) {
        assert_eq!(bytecode.register(lhs), bytecode.register(rhs));
        assert_eq!(bytecode.register(lhs), bytecode.register(dest));

        let bytecode_type = bytecode.register(lhs);
        let offset = bytecode.offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(rhs);
        let offset = bytecode.offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);
        self.asm
            .int_sub(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_neg_int(&mut self, bytecode: &BytecodeFunction, dest: Register, src: Register) {
        assert_eq!(bytecode.register(src), bytecode.register(dest));

        let bytecode_type = bytecode.register(src);
        let offset = bytecode.offset(src);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);
        self.asm
            .int_neg(bytecode_type.mode(), REG_RESULT, REG_RESULT);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_mul_int(
        &mut self,
        bytecode: &BytecodeFunction,
        dest: Register,
        lhs: Register,
        rhs: Register,
    ) {
        assert_eq!(bytecode.register(lhs), bytecode.register(rhs));
        assert_eq!(bytecode.register(lhs), bytecode.register(dest));

        let bytecode_type = bytecode.register(lhs);
        let offset = bytecode.offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(rhs);
        let offset = bytecode.offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);
        self.asm
            .int_mul(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_div_int(
        &mut self,
        bytecode: &BytecodeFunction,
        dest: Register,
        lhs: Register,
        rhs: Register,
    ) {
        assert_eq!(bytecode.register(lhs), bytecode.register(rhs));
        assert_eq!(bytecode.register(lhs), bytecode.register(dest));

        let bytecode_type = bytecode.register(lhs);
        let offset = bytecode.offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(rhs);
        let offset = bytecode.offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);
        self.asm
            .int_div(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_mod_int(
        &mut self,
        bytecode: &BytecodeFunction,
        dest: Register,
        lhs: Register,
        rhs: Register,
    ) {
        assert_eq!(bytecode.register(lhs), bytecode.register(rhs));
        assert_eq!(bytecode.register(lhs), bytecode.register(dest));

        let bytecode_type = bytecode.register(lhs);
        let offset = bytecode.offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(rhs);
        let offset = bytecode.offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);
        self.asm
            .int_mod(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_and_int(
        &mut self,
        bytecode: &BytecodeFunction,
        dest: Register,
        lhs: Register,
        rhs: Register,
    ) {
        assert_eq!(bytecode.register(lhs), bytecode.register(rhs));
        assert_eq!(bytecode.register(lhs), bytecode.register(dest));

        let bytecode_type = bytecode.register(lhs);
        let offset = bytecode.offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(rhs);
        let offset = bytecode.offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);
        self.asm
            .int_and(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_or_int(
        &mut self,
        bytecode: &BytecodeFunction,
        dest: Register,
        lhs: Register,
        rhs: Register,
    ) {
        assert_eq!(bytecode.register(lhs), bytecode.register(rhs));
        assert_eq!(bytecode.register(lhs), bytecode.register(dest));

        let bytecode_type = bytecode.register(lhs);
        let offset = bytecode.offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(rhs);
        let offset = bytecode.offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);
        self.asm
            .int_or(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_xor_int(
        &mut self,
        bytecode: &BytecodeFunction,
        dest: Register,
        lhs: Register,
        rhs: Register,
    ) {
        assert_eq!(bytecode.register(lhs), bytecode.register(rhs));
        assert_eq!(bytecode.register(lhs), bytecode.register(dest));

        let bytecode_type = bytecode.register(lhs);
        let offset = bytecode.offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(rhs);
        let offset = bytecode.offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);
        self.asm
            .int_xor(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_not_bool(&mut self, bytecode: &BytecodeFunction, dest: Register, src: Register) {
        assert_eq!(bytecode.register(src), bytecode.register(dest));

        let bytecode_type = bytecode.register(src);
        let offset = bytecode.offset(src);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);
        self.asm.bool_not(REG_RESULT, REG_RESULT);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_shl_int(
        &mut self,
        bytecode: &BytecodeFunction,
        dest: Register,
        lhs: Register,
        rhs: Register,
    ) {
        assert_eq!(bytecode.register(lhs), bytecode.register(rhs));
        assert_eq!(bytecode.register(lhs), bytecode.register(dest));

        let bytecode_type = bytecode.register(lhs);
        let offset = bytecode.offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(rhs);
        let offset = bytecode.offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);
        self.asm
            .int_shl(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_shr_int(
        &mut self,
        bytecode: &BytecodeFunction,
        dest: Register,
        lhs: Register,
        rhs: Register,
    ) {
        assert_eq!(bytecode.register(lhs), bytecode.register(rhs));
        assert_eq!(bytecode.register(lhs), bytecode.register(dest));

        let bytecode_type = bytecode.register(lhs);
        let offset = bytecode.offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(rhs);
        let offset = bytecode.offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);
        self.asm
            .int_shr(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_sar_int(
        &mut self,
        bytecode: &BytecodeFunction,
        dest: Register,
        lhs: Register,
        rhs: Register,
    ) {
        assert_eq!(bytecode.register(lhs), bytecode.register(rhs));
        assert_eq!(bytecode.register(lhs), bytecode.register(dest));

        let bytecode_type = bytecode.register(lhs);
        let offset = bytecode.offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(rhs);
        let offset = bytecode.offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);
        self.asm
            .int_sar(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_mov_generic(&mut self, bytecode: &BytecodeFunction, dest: Register, src: Register) {
        assert_eq!(bytecode.register(src), bytecode.register(dest));

        let bytecode_type = bytecode.register(src);
        let offset = bytecode.offset(src);

        let reg = match bytecode_type {
            BytecodeType::Float | BytecodeType::Double => FREG_RESULT.into(),
            _ => REG_RESULT.into(),
        };

        self.asm
            .load_mem(bytecode_type.mode(), reg, Mem::Local(offset));

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);
        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), reg);
    }

    fn emit_load_field(
        &mut self,
        bytecode: &BytecodeFunction,
        dest: Register,
        obj: Register,
        class_def_id: ClassDefId,
        field_id: FieldId,
    ) {
        assert_eq!(bytecode.register(obj), BytecodeType::Ptr);

        let cls = self.vm.class_defs.idx(class_def_id);
        let cls = cls.read();
        let field = &cls.fields[field_id.idx()];

        assert_eq!(bytecode.register(dest), field.ty.into());

        self.asm
            .emit_comment(Comment::LoadField(class_def_id, field_id));

        let bytecode_type = bytecode.register(obj);
        let offset = bytecode.offset(obj);

        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        self.asm.load_field(
            field.ty.mode(),
            REG_RESULT.into(),
            REG_RESULT,
            field.offset,
            -1,
        );

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_load_global_field(
        &mut self,
        bytecode: &BytecodeFunction,
        dest: Register,
        global_id: GlobalId,
    ) {
        let glob = self.vm.globals.idx(global_id);
        let glob = glob.lock();

        assert_eq!(bytecode.register(dest), glob.ty.into());

        let disp = self.asm.add_addr(glob.address_value.to_ptr());
        let pos = self.asm.pos() as i32;

        self.asm.emit_comment(Comment::LoadGlobal(global_id));
        self.asm.load_constpool(REG_TMP1, disp + pos);

        self.asm
            .load_mem(glob.ty.mode(), REG_RESULT.into(), Mem::Base(REG_TMP1, 0));

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_const_nil(&mut self, bytecode: &BytecodeFunction, dest: Register) {
        assert_eq!(bytecode.register(dest), BytecodeType::Ptr);

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);

        self.asm.load_nil(REG_RESULT);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_const_bool(&mut self, bytecode: &BytecodeFunction, dest: Register, bool_const: bool) {
        assert_eq!(bytecode.register(dest), BytecodeType::Bool);

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);

        if bool_const {
            self.asm.load_true(REG_RESULT);
        } else {
            self.asm.load_false(REG_RESULT);
        }
        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_const_int(&mut self, bytecode: &BytecodeFunction, dest: Register, int_const: i64) {
        assert!(
            bytecode.register(dest) == BytecodeType::Char
                || bytecode.register(dest) == BytecodeType::Byte
                || bytecode.register(dest) == BytecodeType::Int
                || bytecode.register(dest) == BytecodeType::Long
        );

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);

        self.asm
            .load_int_const(bytecode_type.mode(), REG_RESULT, int_const);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_const_float(&mut self, bytecode: &BytecodeFunction, dest: Register, float_const: f64) {
        assert!(
            bytecode.register(dest) == BytecodeType::Float
                || bytecode.register(dest) == BytecodeType::Double
        );

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);

        self.asm
            .load_float_const(bytecode_type.mode(), FREG_RESULT, float_const);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), FREG_RESULT.into());
    }

    fn emit_const_string(
        &mut self,
        bytecode: &BytecodeFunction,
        dest: Register,
        sp: StrConstPoolIdx,
    ) {
        assert_eq!(bytecode.register(dest), BytecodeType::Ptr);

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);

        let lit_value = bytecode.string(sp);

        let handle = Str::from_buffer_in_perm(self.vm, lit_value.as_bytes());
        let disp = self.asm.add_addr(handle.raw() as *const u8);
        let pos = self.asm.pos() as i32;

        self.asm.emit_comment(Comment::LoadString(handle));

        self.asm.load_constpool(REG_RESULT, disp + pos);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_test_generic(
        &mut self,
        bytecode: &BytecodeFunction,
        dest: Register,
        lhs: Register,
        rhs: Register,
        op: CondCode,
    ) {
        assert_eq!(bytecode.register(lhs), bytecode.register(rhs));
        assert_eq!(bytecode.register(dest), BytecodeType::Bool);

        let bytecode_type = bytecode.register(lhs);
        let offset = bytecode.offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));
        let bytecode_type = bytecode.register(rhs);
        let offset = bytecode.offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        self.asm.cmp_reg(bytecode_type.mode(), REG_RESULT, REG_TMP1);
        self.asm.set(REG_RESULT, op);

        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_jump_if(
        &mut self,
        bytecode: &BytecodeFunction,
        src: Register,
        bytecode_idx: BytecodeIdx,
        op: bool,
    ) {
        assert_eq!(bytecode.register(src), BytecodeType::Bool);

        let bytecode_type = bytecode.register(src);
        let offset = bytecode.offset(src);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let op = if op {
            CondCode::NonZero
        } else {
            CondCode::Zero
        };
        let lbl = self.asm.create_label();
        self.asm.test_and_jump_if(op, REG_RESULT, lbl);

        self.resolve_label(bytecode_idx, lbl);
    }

    fn emit_jump(&mut self, _bytecode: &BytecodeFunction, bytecode_idx: BytecodeIdx) {
        let lbl = self.asm.create_label();
        self.asm.jump(lbl);

        self.resolve_label(bytecode_idx, lbl);
    }

    fn resolve_label(&mut self, bytecode_idx: BytecodeIdx, lbl: Label) {
        let BytecodeIdx(current_position) = self.pos();
        let BytecodeIdx(target) = bytecode_idx;

        if target < current_position {
            self.asm.bind_label_to(
                lbl,
                *self
                    .bytecode_to_address
                    .get(&bytecode_idx)
                    .expect("jump with wrong offset"),
            );
        } else {
            self.forward_jumps.push(ForwardJump {
                label: lbl,
                bytecode_idx: bytecode_idx,
            });
        }
    }

    fn emit_return_generic(&mut self, bytecode: &BytecodeFunction, src: Register) {
        let bytecode_type = bytecode.register(src);
        let offset = bytecode.offset(src);

        let reg = match bytecode_type {
            BytecodeType::Float | BytecodeType::Double => FREG_RESULT.into(),
            _ => REG_RESULT.into(),
        };

        self.asm
            .load_mem(bytecode_type.mode(), reg, Mem::Local(offset));

        self.emit_epilog();
    }

    fn resolve_forward_jumps(&mut self) {
        for jump in &self.forward_jumps {
            let ForwardJump {
                label,
                bytecode_idx,
            } = jump;

            match self.bytecode_to_address.get(&bytecode_idx) {
                Some(&address) => {
                    self.asm.bind_label_to(*label, address);
                }
                None => {
                    panic!("address for bytecode index not found");
                }
            }
        }
    }

    fn pos(&self) -> BytecodeIdx {
        self.current_pos.expect("current position is not set")
    }

    fn pos_inc(&mut self) {
        let BytecodeIdx(pos_idx) = self.pos();
        self.current_pos = Some(BytecodeIdx(pos_idx + 1));
    }
}

impl<'a, 'ast> CodeGen<'ast> for CannonCodeGen<'a, 'ast> {
    fn generate(mut self) -> JitBaselineFct {
        let bytecode = generate_fct(
            self.vm,
            self.fct,
            self.src,
            self.cls_type_params,
            self.fct_type_params,
        );

        if should_emit_bytecode(self.vm, self.fct) {
            bytecode.dump();
        }

        if should_emit_debug(self.vm, self.fct) {
            self.asm.debug();
        }

        self.emit_prolog(&bytecode);
        self.store_params_on_stack(&bytecode);

        self.current_pos = Some(BytecodeIdx(0));
        for btcode in bytecode.code() {
            self.bytecode_to_address.insert(self.pos(), self.asm.pos());

            match btcode {
                Bytecode::AddInt(dest, lhs, rhs) | Bytecode::AddLong(dest, lhs, rhs) => {
                    self.emit_add_int(&bytecode, *dest, *lhs, *rhs)
                }
                Bytecode::AddFloat(dest, lhs, rhs) | Bytecode::AddDouble(dest, lhs, rhs) => {
                    self.emit_add_float(&bytecode, *dest, *lhs, *rhs)
                }

                Bytecode::SubInt(dest, lhs, rhs) => self.emit_sub_int(&bytecode, *dest, *lhs, *rhs),
                Bytecode::NegInt(dest, src) | Bytecode::NegLong(dest, src) => {
                    self.emit_neg_int(&bytecode, *dest, *src)
                }
                Bytecode::MulInt(dest, lhs, rhs) => self.emit_mul_int(&bytecode, *dest, *lhs, *rhs),
                Bytecode::DivInt(dest, lhs, rhs) => self.emit_div_int(&bytecode, *dest, *lhs, *rhs),
                Bytecode::ModInt(dest, lhs, rhs) => self.emit_mod_int(&bytecode, *dest, *lhs, *rhs),
                Bytecode::AndInt(dest, lhs, rhs) => self.emit_and_int(&bytecode, *dest, *lhs, *rhs),
                Bytecode::OrInt(dest, lhs, rhs) => self.emit_or_int(&bytecode, *dest, *lhs, *rhs),
                Bytecode::XorInt(dest, lhs, rhs) => self.emit_xor_int(&bytecode, *dest, *lhs, *rhs),
                Bytecode::NotBool(dest, src) => self.emit_not_bool(&bytecode, *dest, *src),
                Bytecode::ShlInt(dest, lhs, rhs) => self.emit_shl_int(&bytecode, *dest, *lhs, *rhs),
                Bytecode::ShrInt(dest, lhs, rhs) => self.emit_shr_int(&bytecode, *dest, *lhs, *rhs),
                Bytecode::SarInt(dest, lhs, rhs) => self.emit_sar_int(&bytecode, *dest, *lhs, *rhs),
                Bytecode::MovBool(dest, src)
                | Bytecode::MovByte(dest, src)
                | Bytecode::MovChar(dest, src)
                | Bytecode::MovInt(dest, src)
                | Bytecode::MovLong(dest, src)
                | Bytecode::MovFloat(dest, src)
                | Bytecode::MovDouble(dest, src)
                | Bytecode::MovPtr(dest, src) => self.emit_mov_generic(&bytecode, *dest, *src),

                Bytecode::LoadFieldBool(dest, obj, class_def_id, field_id)
                | Bytecode::LoadFieldByte(dest, obj, class_def_id, field_id)
                | Bytecode::LoadFieldChar(dest, obj, class_def_id, field_id)
                | Bytecode::LoadFieldInt(dest, obj, class_def_id, field_id)
                | Bytecode::LoadFieldLong(dest, obj, class_def_id, field_id)
                | Bytecode::LoadFieldFloat(dest, obj, class_def_id, field_id)
                | Bytecode::LoadFieldDouble(dest, obj, class_def_id, field_id)
                | Bytecode::LoadFieldPtr(dest, obj, class_def_id, field_id) => {
                    self.emit_load_field(&bytecode, *dest, *obj, *class_def_id, *field_id)
                }

                Bytecode::LoadGlobalBool(dest, global_id)
                | Bytecode::LoadGlobalByte(dest, global_id)
                | Bytecode::LoadGlobalChar(dest, global_id)
                | Bytecode::LoadGlobalInt(dest, global_id)
                | Bytecode::LoadGlobalLong(dest, global_id)
                | Bytecode::LoadGlobalFloat(dest, global_id)
                | Bytecode::LoadGlobalDouble(dest, global_id)
                | Bytecode::LoadGlobalPtr(dest, global_id) => {
                    self.emit_load_global_field(&bytecode, *dest, *global_id);
                }

                Bytecode::ConstNil(dest) => self.emit_const_nil(&bytecode, *dest),
                Bytecode::ConstTrue(dest) => self.emit_const_bool(&bytecode, *dest, true),
                Bytecode::ConstFalse(dest) => self.emit_const_bool(&bytecode, *dest, false),
                Bytecode::ConstZeroByte(dest)
                | Bytecode::ConstZeroInt(dest)
                | Bytecode::ConstZeroLong(dest) => self.emit_const_int(&bytecode, *dest, 0),
                Bytecode::ConstByte(dest, value) => {
                    self.emit_const_int(&bytecode, *dest, *value as i64)
                }
                Bytecode::ConstInt(dest, value) => {
                    self.emit_const_int(&bytecode, *dest, *value as i64)
                }
                Bytecode::ConstLong(dest, value) => {
                    self.emit_const_int(&bytecode, *dest, *value as i64)
                }
                Bytecode::ConstChar(dest, value) => {
                    self.emit_const_int(&bytecode, *dest, *value as i64)
                }
                Bytecode::ConstZeroFloat(dest) | Bytecode::ConstZeroDouble(dest) => {
                    self.emit_const_float(&bytecode, *dest, 0_f64)
                }
                Bytecode::ConstFloat(dest, value) => {
                    self.emit_const_float(&bytecode, *dest, *value as f64)
                }
                Bytecode::ConstDouble(dest, value) => {
                    self.emit_const_float(&bytecode, *dest, *value)
                }
                Bytecode::ConstString(dest, sp) => {
                    self.emit_const_string(&bytecode, *dest, *sp);
                }

                Bytecode::TestEqPtr(dest, lhs, rhs) => {
                    self.emit_test_generic(&bytecode, *dest, *lhs, *rhs, CondCode::Equal);
                }
                Bytecode::TestNePtr(dest, lhs, rhs) => {
                    self.emit_test_generic(&bytecode, *dest, *lhs, *rhs, CondCode::NotEqual);
                }

                Bytecode::TestEqInt(dest, lhs, rhs) => {
                    self.emit_test_generic(&bytecode, *dest, *lhs, *rhs, CondCode::Equal)
                }
                Bytecode::TestNeInt(dest, lhs, rhs) => {
                    self.emit_test_generic(&bytecode, *dest, *lhs, *rhs, CondCode::NotEqual)
                }
                Bytecode::TestGtInt(dest, lhs, rhs) => {
                    self.emit_test_generic(&bytecode, *dest, *lhs, *rhs, CondCode::Greater)
                }
                Bytecode::TestGeInt(dest, lhs, rhs) => {
                    self.emit_test_generic(&bytecode, *dest, *lhs, *rhs, CondCode::GreaterEq)
                }
                Bytecode::TestLtInt(dest, lhs, rhs) => {
                    self.emit_test_generic(&bytecode, *dest, *lhs, *rhs, CondCode::Less)
                }
                Bytecode::TestLeInt(dest, lhs, rhs) => {
                    self.emit_test_generic(&bytecode, *dest, *lhs, *rhs, CondCode::LessEq)
                }

                Bytecode::JumpIfFalse(src, bytecode_idx) => {
                    self.emit_jump_if(&bytecode, *src, *bytecode_idx, false)
                }
                Bytecode::JumpIfTrue(src, bytecode_idx) => {
                    self.emit_jump_if(&bytecode, *src, *bytecode_idx, true)
                }
                Bytecode::Jump(bytecode_idx) => self.emit_jump(&bytecode, *bytecode_idx),

                Bytecode::RetBool(src)
                | Bytecode::RetByte(src)
                | Bytecode::RetChar(src)
                | Bytecode::RetInt(src)
                | Bytecode::RetLong(src)
                | Bytecode::RetFloat(src)
                | Bytecode::RetDouble(src) => self.emit_return_generic(&bytecode, *src),
                Bytecode::RetVoid => self.emit_epilog(),
                _ => panic!("bytecode {:?} not implemented", btcode),
            }

            self.pos_inc();
        }

        self.resolve_forward_jumps();

        let jit_fct = self.asm.jit(
            bytecode.stacksize(),
            JitDescriptor::DoraFct(self.fct.id),
            self.ast.throws,
        );

        jit_fct
    }
}

fn should_emit_bytecode(vm: &VM, fct: &Fct) -> bool {
    if let Some(ref dbg_names) = vm.args.flag_emit_bytecode {
        fct_pattern_match(vm, fct, dbg_names)
    } else {
        false
    }
}
