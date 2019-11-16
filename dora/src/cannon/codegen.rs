use dora_parser::ast::*;
use std::collections::hash_map::HashMap;

use crate::bytecode::{
    self, BytecodeFunction, BytecodeOffset, BytecodeType, BytecodeVisitor, ConstPoolIdx, Register,
};
use crate::compiler::asm::BaselineAssembler;
use crate::compiler::codegen::should_emit_debug;
use crate::compiler::codegen::ExprStore;
use crate::compiler::fct::{Comment, JitBaselineFct, JitDescriptor};
use crate::cpu::{Mem, FREG_PARAMS, FREG_RESULT, FREG_TMP1, REG_PARAMS, REG_RESULT, REG_TMP1};
use crate::masm::*;
use crate::object::Str;
use crate::ty::TypeList;
use crate::vm::{ClassDefId, Fct, FctId, FctSrc, FieldId, GlobalId, VM};

struct ForwardJump {
    label: Label,
    offset: BytecodeOffset,
}

pub struct CannonCodeGen<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    fct: &'a Fct<'ast>,
    ast: &'ast Function,
    asm: BaselineAssembler<'a, 'ast>,
    src: &'a FctSrc,
    bytecode: &'a BytecodeFunction,

    lbl_break: Option<Label>,
    lbl_continue: Option<Label>,

    // stores all active finally blocks
    active_finallys: Vec<&'ast Stmt>,

    // label to jump instead of emitting epilog for return
    // needed for return's in finally blocks
    // return in finally needs to execute to next finally block and not
    // leave the current function
    lbl_return: Option<Label>,

    // length of active_finallys in last loop
    // default: 0
    // break/continue need to emit finally blocks up to the last loop
    // see tests/finally/break-while.dora
    active_loop: Option<usize>,

    // upper length of active_finallys in emitting finally-blocks for break/continue
    // default: active_finallys.len()
    // break/continue needs to execute finally-blocks in loop, return in these blocks
    // would dump all active_finally-entries from the loop but we need an upper bound.
    // see emit_finallys_within_loop and tests/finally/continue-return.dora
    active_upper: Option<usize>,

    cls_type_params: &'a TypeList,
    fct_type_params: &'a TypeList,

    offset_to_address: HashMap<BytecodeOffset, usize>,

    forward_jumps: Vec<ForwardJump>,
    current_offset: BytecodeOffset,
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
        src: &'a FctSrc,
        bytecode: &'a BytecodeFunction,
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
            bytecode,
            lbl_break,
            lbl_continue,
            active_finallys,
            active_upper,
            active_loop,
            lbl_return,
            cls_type_params,
            fct_type_params,
            offset_to_address: HashMap::new(),
            forward_jumps: Vec::new(),
            current_offset: BytecodeOffset(0),
        }
    }

    pub fn generate(mut self) -> JitBaselineFct {
        if should_emit_debug(self.vm, self.fct) {
            self.asm.debug();
        }

        self.emit_prolog();
        self.store_params_on_stack();

        bytecode::read(self.bytecode.code(), &mut self);

        self.resolve_forward_jumps();

        let jit_fct = self.asm.jit(
            self.bytecode.stacksize(),
            JitDescriptor::DoraFct(self.fct.id),
            self.ast.throws,
        );

        jit_fct
    }

    fn store_params_on_stack(&mut self) {
        let mut reg_idx = 0;
        let mut freg_idx = 0;
        let mut sp_offset = 16;
        let mut idx = 0;

        for param in self.fct.params_with_self() {
            let dest = Register(idx);
            assert_eq!(self.bytecode.register_type(dest), (*param).into());

            // let bytecode_type = bytecode.register_type(dest);
            let offset = self.bytecode.register_offset(dest);

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

    fn emit_prolog(&mut self) {
        self.asm
            .prolog_size(self.bytecode.stacksize(), self.fct.ast.pos);
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

    fn emit_add_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.bytecode.register_type(lhs);
        let offset = self.bytecode.register_offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(rhs);
        let offset = self.bytecode.register_offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);
        self.asm
            .int_add(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_add_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.bytecode.register_type(lhs);
        let offset = self.bytecode.register_offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), FREG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(rhs);
        let offset = self.bytecode.register_offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), FREG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);
        self.asm
            .float_add(bytecode_type.mode(), FREG_RESULT, FREG_RESULT, FREG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), FREG_RESULT.into());
    }

    fn emit_sub_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.bytecode.register_type(lhs);
        let offset = self.bytecode.register_offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(rhs);
        let offset = self.bytecode.register_offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);
        self.asm
            .int_sub(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_sub_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.bytecode.register_type(lhs);
        let offset = self.bytecode.register_offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), FREG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(rhs);
        let offset = self.bytecode.register_offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), FREG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);
        self.asm
            .float_sub(bytecode_type.mode(), FREG_RESULT, FREG_RESULT, FREG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), FREG_RESULT.into());
    }

    fn emit_neg_int(&mut self, dest: Register, src: Register) {
        assert_eq!(
            self.bytecode.register_type(src),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.bytecode.register_type(src);
        let offset = self.bytecode.register_offset(src);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);
        self.asm
            .int_neg(bytecode_type.mode(), REG_RESULT, REG_RESULT);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_mul_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.bytecode.register_type(lhs);
        let offset = self.bytecode.register_offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(rhs);
        let offset = self.bytecode.register_offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);
        self.asm
            .int_mul(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_mul_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.bytecode.register_type(lhs);
        let offset = self.bytecode.register_offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), FREG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(rhs);
        let offset = self.bytecode.register_offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), FREG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);
        self.asm
            .float_mul(bytecode_type.mode(), FREG_RESULT, FREG_RESULT, FREG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), FREG_RESULT.into());
    }

    fn emit_div_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.bytecode.register_type(lhs);
        let offset = self.bytecode.register_offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(rhs);
        let offset = self.bytecode.register_offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);
        self.asm
            .int_div(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_div_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.bytecode.register_type(lhs);
        let offset = self.bytecode.register_offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), FREG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(rhs);
        let offset = self.bytecode.register_offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), FREG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);
        self.asm
            .float_div(bytecode_type.mode(), FREG_RESULT, FREG_RESULT, FREG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), FREG_RESULT.into());
    }

    fn emit_mod_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.bytecode.register_type(lhs);
        let offset = self.bytecode.register_offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(rhs);
        let offset = self.bytecode.register_offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);
        self.asm
            .int_mod(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_and_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.bytecode.register_type(lhs);
        let offset = self.bytecode.register_offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(rhs);
        let offset = self.bytecode.register_offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);
        self.asm
            .int_and(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_or_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.bytecode.register_type(lhs);
        let offset = self.bytecode.register_offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(rhs);
        let offset = self.bytecode.register_offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);
        self.asm
            .int_or(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_xor_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.bytecode.register_type(lhs);
        let offset = self.bytecode.register_offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(rhs);
        let offset = self.bytecode.register_offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);
        self.asm
            .int_xor(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_not_bool(&mut self, dest: Register, src: Register) {
        assert_eq!(
            self.bytecode.register_type(src),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.bytecode.register_type(src);
        let offset = self.bytecode.register_offset(src);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);
        self.asm.bool_not(REG_RESULT, REG_RESULT);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_shl_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.bytecode.register_type(lhs);
        let offset = self.bytecode.register_offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(rhs);
        let offset = self.bytecode.register_offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);
        self.asm
            .int_shl(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_shr_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.bytecode.register_type(lhs);
        let offset = self.bytecode.register_offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(rhs);
        let offset = self.bytecode.register_offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);
        self.asm
            .int_shr(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_sar_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.bytecode.register_type(lhs);
        let offset = self.bytecode.register_offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(rhs);
        let offset = self.bytecode.register_offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);
        self.asm
            .int_sar(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_mov_generic(&mut self, dest: Register, src: Register) {
        assert_eq!(
            self.bytecode.register_type(src),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.bytecode.register_type(src);
        let offset = self.bytecode.register_offset(src);

        let reg = result_reg(bytecode_type);

        self.asm
            .load_mem(bytecode_type.mode(), reg, Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);
        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), reg);
    }

    fn emit_load_field(
        &mut self,
        dest: Register,
        obj: Register,
        class_def_id: ClassDefId,
        field_id: FieldId,
    ) {
        assert_eq!(self.bytecode.register_type(obj), BytecodeType::Ptr);

        let cls = self.vm.class_defs.idx(class_def_id);
        let cls = cls.read();
        let field = &cls.fields[field_id.idx()];

        assert_eq!(self.bytecode.register_type(dest), field.ty.into());

        self.asm
            .emit_comment(Comment::LoadField(class_def_id, field_id));

        let bytecode_type = self.bytecode.register_type(obj);
        let offset = self.bytecode.register_offset(obj);

        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);

        let reg = result_reg(bytecode_type);

        self.asm
            .load_field(field.ty.mode(), reg, REG_RESULT, field.offset, -1);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), reg);
    }

    fn emit_load_global_field(&mut self, dest: Register, global_id: GlobalId) {
        let glob = self.vm.globals.idx(global_id);
        let glob = glob.lock();

        assert_eq!(self.bytecode.register_type(dest), glob.ty.into());

        let disp = self.asm.add_addr(glob.address_value.to_ptr());
        let pos = self.asm.pos() as i32;

        self.asm.emit_comment(Comment::LoadGlobal(global_id));
        self.asm.load_constpool(REG_TMP1, disp + pos);

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);

        let reg = result_reg(bytecode_type);

        self.asm
            .load_mem(glob.ty.mode(), reg, Mem::Base(REG_TMP1, 0));

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), reg);
    }

    fn emit_const_nil(&mut self, dest: Register) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Ptr);

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);

        self.asm.load_nil(REG_RESULT);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_const_bool(&mut self, dest: Register, bool_const: bool) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Bool);

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);

        if bool_const {
            self.asm.load_true(REG_RESULT);
        } else {
            self.asm.load_false(REG_RESULT);
        }
        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_const_int(&mut self, dest: Register, int_const: i64) {
        assert!(
            self.bytecode.register_type(dest) == BytecodeType::Char
                || self.bytecode.register_type(dest) == BytecodeType::Byte
                || self.bytecode.register_type(dest) == BytecodeType::Int
                || self.bytecode.register_type(dest) == BytecodeType::Long
        );

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);

        self.asm
            .load_int_const(bytecode_type.mode(), REG_RESULT, int_const);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_const_float(&mut self, dest: Register, float_const: f64) {
        assert!(
            self.bytecode.register_type(dest) == BytecodeType::Float
                || self.bytecode.register_type(dest) == BytecodeType::Double
        );

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);

        self.asm
            .load_float_const(bytecode_type.mode(), FREG_RESULT, float_const);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), FREG_RESULT.into());
    }

    fn emit_const_string(&mut self, dest: Register, lit_value: &str) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Ptr);

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);

        let handle = Str::from_buffer_in_perm(self.vm, lit_value.as_bytes());
        let disp = self.asm.add_addr(handle.raw() as *const u8);
        let pos = self.asm.pos() as i32;

        self.asm.emit_comment(Comment::LoadString(handle));

        self.asm.load_constpool(REG_RESULT, disp + pos);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_test_generic(&mut self, dest: Register, lhs: Register, rhs: Register, op: CondCode) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Bool);

        let bytecode_type = self.bytecode.register_type(lhs);
        let offset = self.bytecode.register_offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_RESULT.into(), Mem::Local(offset));
        let bytecode_type = self.bytecode.register_type(rhs);
        let offset = self.bytecode.register_offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), REG_TMP1.into(), Mem::Local(offset));

        self.asm.cmp_reg(bytecode_type.mode(), REG_RESULT, REG_TMP1);
        self.asm.set(REG_RESULT, op);

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_test_float(&mut self, dest: Register, lhs: Register, rhs: Register, op: CondCode) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Bool);

        let bytecode_type = self.bytecode.register_type(lhs);
        let offset = self.bytecode.register_offset(lhs);
        self.asm
            .load_mem(bytecode_type.mode(), FREG_RESULT.into(), Mem::Local(offset));
        let bytecode_type = self.bytecode.register_type(rhs);
        let offset = self.bytecode.register_offset(rhs);
        self.asm
            .load_mem(bytecode_type.mode(), FREG_TMP1.into(), Mem::Local(offset));

        self.asm
            .float_cmp(bytecode_type.mode(), REG_RESULT, FREG_RESULT, FREG_TMP1, op);

        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.bytecode.register_offset(dest);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_jump_if(&mut self, src: Register, offset: BytecodeOffset, op: bool) {
        assert_eq!(self.bytecode.register_type(src), BytecodeType::Bool);

        let bytecode_type = self.bytecode.register_type(src);
        let src_offset = self.bytecode.register_offset(src);
        self.asm.load_mem(
            bytecode_type.mode(),
            REG_RESULT.into(),
            Mem::Local(src_offset),
        );

        let op = if op {
            CondCode::NonZero
        } else {
            CondCode::Zero
        };
        let lbl = self.asm.create_label();
        self.asm.test_and_jump_if(op, REG_RESULT, lbl);

        self.resolve_label(offset, lbl);
    }

    fn emit_jump(&mut self, offset: BytecodeOffset) {
        let lbl = self.asm.create_label();
        self.asm.jump(lbl);

        self.resolve_label(offset, lbl);
    }

    fn resolve_label(&mut self, target: BytecodeOffset, lbl: Label) {
        if target < self.current_offset {
            self.asm.bind_label_to(
                lbl,
                *self
                    .offset_to_address
                    .get(&target)
                    .expect("jump with wrong offset"),
            );
        } else {
            self.forward_jumps.push(ForwardJump {
                label: lbl,
                offset: target,
            });
        }
    }

    fn emit_return_generic(&mut self, src: Register) {
        let bytecode_type = self.bytecode.register_type(src);
        let offset = self.bytecode.register_offset(src);

        let reg = result_reg(bytecode_type);

        self.asm
            .load_mem(bytecode_type.mode(), reg, Mem::Local(offset));

        self.emit_epilog();
    }

    fn resolve_forward_jumps(&mut self) {
        for jump in &self.forward_jumps {
            let offset = *self
                .offset_to_address
                .get(&jump.offset)
                .expect("offset for bytecode not found");
            self.asm.bind_label_to(jump.label, offset);
        }
    }
}

impl<'a, 'ast: 'a> BytecodeVisitor for CannonCodeGen<'a, 'ast> {
    fn visit_instruction(&mut self, offset: BytecodeOffset) {
        self.offset_to_address.insert(offset, self.asm.pos());
        self.current_offset = offset;
    }

    fn visit_add_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_add_int(dest, lhs, rhs);
    }
    fn visit_add_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_add_int(dest, lhs, rhs);
    }
    fn visit_add_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_add_float(dest, lhs, rhs);
    }
    fn visit_add_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_add_float(dest, lhs, rhs);
    }

    fn visit_sub_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_sub_int(dest, lhs, rhs);
    }
    fn visit_sub_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_sub_int(dest, lhs, rhs);
    }
    fn visit_sub_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_sub_float(dest, lhs, rhs);
    }
    fn visit_sub_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_sub_float(dest, lhs, rhs);
    }

    fn visit_neg_int(&mut self, dest: Register, src: Register) {
        self.emit_neg_int(dest, src);
    }
    fn visit_neg_long(&mut self, dest: Register, src: Register) {
        self.emit_neg_int(dest, src);
    }
    fn visit_neg_float(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_neg_double(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }

    fn visit_mul_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_mul_int(dest, lhs, rhs);
    }
    fn visit_mul_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_mul_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_mul_float(dest, lhs, rhs);
    }
    fn visit_mul_double(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_div_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_div_int(dest, lhs, rhs);
    }
    fn visit_div_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_div_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_div_float(dest, lhs, rhs);
    }
    fn visit_div_double(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_mod_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_mod_int(dest, lhs, rhs);
    }
    fn visit_mod_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_and_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_and_int(dest, lhs, rhs);
    }
    fn visit_and_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_or_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_or_int(dest, lhs, rhs)
    }
    fn visit_or_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_xor_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_xor_int(dest, lhs, rhs);
    }
    fn visit_xor_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_not_bool(&mut self, dest: Register, src: Register) {
        self.emit_not_bool(dest, src);
    }
    fn visit_not_int(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_not_long(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }

    fn visit_shl_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_shl_int(dest, lhs, rhs);
    }
    fn visit_shr_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_shr_int(dest, lhs, rhs);
    }
    fn visit_sar_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_sar_int(dest, lhs, rhs);
    }

    fn visit_shl_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_shr_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_sar_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_mov_bool(&mut self, dest: Register, src: Register) {
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_byte(&mut self, dest: Register, src: Register) {
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_char(&mut self, dest: Register, src: Register) {
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_int(&mut self, dest: Register, src: Register) {
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_long(&mut self, dest: Register, src: Register) {
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_float(&mut self, dest: Register, src: Register) {
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_double(&mut self, dest: Register, src: Register) {
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_ptr(&mut self, dest: Register, src: Register) {
        self.emit_mov_generic(dest, src);
    }

    fn visit_load_field_bool(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(dest, obj, cls, field);
    }
    fn visit_load_field_byte(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(dest, obj, cls, field);
    }
    fn visit_load_field_char(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(dest, obj, cls, field);
    }
    fn visit_load_field_int(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(dest, obj, cls, field);
    }
    fn visit_load_field_long(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(dest, obj, cls, field);
    }
    fn visit_load_field_float(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(dest, obj, cls, field);
    }
    fn visit_load_field_double(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(dest, obj, cls, field);
    }
    fn visit_load_field_ptr(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(dest, obj, cls, field);
    }

    fn visit_store_field_bool(
        &mut self,
        _src: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_store_field_byte(
        &mut self,
        _src: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_store_field_char(
        &mut self,
        _src: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_store_field_int(
        &mut self,
        _src: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_store_field_long(
        &mut self,
        _src: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_store_field_float(
        &mut self,
        _src: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_store_field_double(
        &mut self,
        _src: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_store_field_ptr(
        &mut self,
        _src: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }

    fn visit_load_global_bool(&mut self, dest: Register, glob: GlobalId) {
        self.emit_load_global_field(dest, glob);
    }
    fn visit_load_global_byte(&mut self, dest: Register, glob: GlobalId) {
        self.emit_load_global_field(dest, glob);
    }
    fn visit_load_global_char(&mut self, dest: Register, glob: GlobalId) {
        self.emit_load_global_field(dest, glob);
    }
    fn visit_load_global_int(&mut self, dest: Register, glob: GlobalId) {
        self.emit_load_global_field(dest, glob);
    }
    fn visit_load_global_long(&mut self, dest: Register, glob: GlobalId) {
        self.emit_load_global_field(dest, glob);
    }
    fn visit_load_global_float(&mut self, dest: Register, glob: GlobalId) {
        self.emit_load_global_field(dest, glob);
    }
    fn visit_load_global_double(&mut self, dest: Register, glob: GlobalId) {
        self.emit_load_global_field(dest, glob);
    }
    fn visit_load_global_ptr(&mut self, dest: Register, glob: GlobalId) {
        self.emit_load_global_field(dest, glob);
    }

    fn visit_store_global_bool(&mut self, _src: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_store_global_byte(&mut self, _src: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_store_global_char(&mut self, _src: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_store_global_int(&mut self, _src: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_store_global_long(&mut self, _src: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_store_global_float(&mut self, _src: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_store_global_double(&mut self, _src: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_store_global_ptr(&mut self, _src: Register, _glob: GlobalId) {
        unimplemented!();
    }

    fn visit_const_nil(&mut self, dest: Register) {
        self.emit_const_nil(dest);
    }
    fn visit_const_true(&mut self, dest: Register) {
        self.emit_const_bool(dest, true);
    }
    fn visit_const_false(&mut self, dest: Register) {
        self.emit_const_bool(dest, false);
    }
    fn visit_const_zero_byte(&mut self, dest: Register) {
        self.emit_const_int(dest, 0);
    }
    fn visit_const_zero_char(&mut self, dest: Register) {
        self.emit_const_int(dest, 0);
    }
    fn visit_const_zero_int(&mut self, dest: Register) {
        self.emit_const_int(dest, 0);
    }
    fn visit_const_zero_long(&mut self, dest: Register) {
        self.emit_const_int(dest, 0);
    }
    fn visit_const_zero_float(&mut self, dest: Register) {
        self.emit_const_float(dest, 0_f64);
    }
    fn visit_const_zero_double(&mut self, dest: Register) {
        self.emit_const_float(dest, 0_f64);
    }
    fn visit_const_char(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bytecode
            .const_pool(idx)
            .to_char()
            .expect("unexpected const pool entry");
        self.emit_const_int(dest, value as i64);
    }

    fn visit_const_byte(&mut self, dest: Register, value: u8) {
        self.emit_const_int(dest, value as i64);
    }
    fn visit_const_int(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bytecode
            .const_pool(idx)
            .to_int()
            .expect("unexpected const pool entry");
        self.emit_const_int(dest, value as i64);
    }
    fn visit_const_long(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bytecode
            .const_pool(idx)
            .to_long()
            .expect("unexpected const pool entry");
        self.emit_const_int(dest, value);
    }
    fn visit_const_float(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bytecode
            .const_pool(idx)
            .to_float()
            .expect("unexpected const pool entry");
        self.emit_const_float(dest, value as f64);
    }
    fn visit_const_double(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bytecode
            .const_pool(idx)
            .to_double()
            .expect("unexpected const pool entry");
        self.emit_const_float(dest, value);
    }
    fn visit_const_string(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bytecode
            .const_pool(idx)
            .to_string()
            .expect("unexpected const pool entry");
        self.emit_const_string(dest, value);
    }

    fn visit_test_eq_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Equal);
    }

    fn visit_test_eq_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::NotEqual);
    }
    fn visit_test_gt_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Greater);
    }
    fn visit_test_ge_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::GreaterEq);
    }
    fn visit_test_lt_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Less);
    }
    fn visit_test_le_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::LessEq);
    }

    fn visit_test_eq_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::NotEqual);
    }
    fn visit_test_gt_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Greater);
    }
    fn visit_test_ge_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::GreaterEq);
    }
    fn visit_test_lt_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Less);
    }
    fn visit_test_le_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::LessEq);
    }

    fn visit_test_eq_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::NotEqual);
    }
    fn visit_test_gt_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::Greater);
    }
    fn visit_test_ge_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::GreaterEq);
    }
    fn visit_test_lt_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::Less);
    }
    fn visit_test_le_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::LessEq);
    }

    fn visit_test_eq_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::NotEqual);
    }
    fn visit_test_gt_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::Greater);
    }
    fn visit_test_ge_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::GreaterEq);
    }
    fn visit_test_lt_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::Less);
    }
    fn visit_test_le_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::LessEq);
    }

    fn visit_jump_if_false(&mut self, opnd: Register, offset: u32) {
        let target = BytecodeOffset(self.current_offset.to_u32() + offset);
        self.emit_jump_if(opnd, target, false);
    }
    fn visit_jump_if_false_const(&mut self, opnd: Register, idx: ConstPoolIdx) {
        let offset = self
            .bytecode
            .const_pool(idx)
            .to_int()
            .expect("int expected");
        self.visit_jump_if_false(opnd, offset as u32);
    }
    fn visit_jump_if_true(&mut self, opnd: Register, offset: u32) {
        let target = BytecodeOffset(self.current_offset.to_u32() + offset);
        self.emit_jump_if(opnd, target, true);
    }
    fn visit_jump_if_true_const(&mut self, opnd: Register, idx: ConstPoolIdx) {
        let offset = self
            .bytecode
            .const_pool(idx)
            .to_int()
            .expect("int expected");
        self.visit_jump_if_true(opnd, offset as u32);
    }
    fn visit_jump_loop(&mut self, offset: u32) {
        let target = BytecodeOffset(self.current_offset.to_u32() - offset);
        self.emit_jump(target);
    }
    fn visit_jump(&mut self, offset: u32) {
        let target = BytecodeOffset(self.current_offset.to_u32() + offset);
        self.emit_jump(target);
    }
    fn visit_jump_const(&mut self, idx: ConstPoolIdx) {
        let offset = self
            .bytecode
            .const_pool(idx)
            .to_int()
            .expect("int expected");
        self.visit_jump(offset as u32);
    }

    fn visit_invoke_direct_void(&mut self, _fct: FctId, _start: Register, _count: u32) {
        unimplemented!();
    }
    fn visit_invoke_direct_bool(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_direct_byte(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_direct_char(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_direct_int(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_direct_long(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_direct_float(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_direct_double(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_direct_ptr(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }

    fn visit_invoke_virtual_void(&mut self, _fct: FctId, _start: Register, _count: u32) {
        unimplemented!();
    }
    fn visit_invoke_virtual_bool(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_virtual_byte(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_virtual_char(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_virtual_int(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_virtual_long(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_virtual_float(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_virtual_double(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_virtual_ptr(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }

    fn visit_invoke_static_void(&mut self, _fct: FctId, _start: Register, _count: u32) {
        unimplemented!();
    }
    fn visit_invoke_static_bool(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_static_byte(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_static_char(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_static_int(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_static_long(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_static_float(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_static_double(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_static_ptr(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }

    fn visit_new_object(&mut self, _dest: Register, _cls: ClassDefId) {
        unimplemented!();
    }
    fn visit_throw(&mut self, _opnd: Register) {
        unimplemented!();
    }

    fn visit_ret_void(&mut self) {
        self.emit_epilog();
    }
    fn visit_ret_bool(&mut self, opnd: Register) {
        self.emit_return_generic(opnd);
    }
    fn visit_ret_byte(&mut self, opnd: Register) {
        self.emit_return_generic(opnd);
    }
    fn visit_ret_char(&mut self, opnd: Register) {
        self.emit_return_generic(opnd);
    }
    fn visit_ret_int(&mut self, opnd: Register) {
        self.emit_return_generic(opnd);
    }
    fn visit_ret_long(&mut self, opnd: Register) {
        self.emit_return_generic(opnd);
    }
    fn visit_ret_float(&mut self, opnd: Register) {
        self.emit_return_generic(opnd);
    }
    fn visit_ret_double(&mut self, opnd: Register) {
        self.emit_return_generic(opnd);
    }
    fn visit_ret_ptr(&mut self, opnd: Register) {
        self.emit_return_generic(opnd);
    }
}

fn result_reg(bytecode_type: BytecodeType) -> ExprStore {
    if bytecode_type.mode().is_float() {
        FREG_RESULT.into()
    } else {
        REG_RESULT.into()
    }
}
