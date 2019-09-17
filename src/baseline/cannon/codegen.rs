use crate::baseline::asm::BaselineAssembler;
use crate::baseline::codegen::fct_pattern_match;
use crate::cpu::{Mem, FREG_RESULT, REG_RESULT};
use dora_parser::ast::*;

use crate::baseline::codegen::{should_emit_debug, CodeGen, Scopes};
use crate::baseline::fct::{Comment, JitBaselineFct, JitDescriptor};
use crate::masm::*;
use crate::object::Str;
use crate::typeparams::TypeParams;
use crate::vm::VM;
use crate::vm::{Fct, FctSrc};

use crate::bytecode::astgen::generate_fct;
use crate::bytecode::generate::{BytecodeFunction, Register, StrConstPoolIdx};
use crate::bytecode::opcode::Bytecode;

pub struct CannonCodeGen<'a, 'ast: 'a> {
    pub vm: &'a VM<'ast>,
    pub fct: &'a Fct<'ast>,
    pub ast: &'ast Function,
    pub asm: BaselineAssembler<'a, 'ast>,
    pub scopes: Scopes,
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

    pub cls_type_params: &'a TypeParams,
    pub fct_type_params: &'a TypeParams,
}

impl<'a, 'ast> CannonCodeGen<'a, 'ast>
where
    'ast: 'a,
{
    fn emit_prolog(&mut self, bytecode: &BytecodeFunction) {
        self.asm.prolog(bytecode.stacksize());
        self.asm.emit_comment(Comment::Lit("prolog end"));
        self.asm.emit_comment(Comment::Newline);
    }

    fn emit_epilog(&mut self, bytecode: &BytecodeFunction) {
        self.asm.emit_comment(Comment::Newline);
        self.asm.emit_comment(Comment::Lit("epilog"));

        let polling_page = self.vm.polling_page.addr();
        self.asm
            .epilog_with_polling(bytecode.stacksize(), polling_page);
    }

    fn emit_const_bool(&mut self, bytecode: &BytecodeFunction, dest: Register, bool_const: bool) {
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
        let bytecode_type = bytecode.register(dest);
        let offset = bytecode.offset(dest);

        self.asm
            .load_int_const(bytecode_type.mode(), REG_RESULT, int_const);

        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), REG_RESULT.into());
    }

    fn emit_const_float(&mut self, bytecode: &BytecodeFunction, dest: Register, float_const: f64) {
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
        for btcode in bytecode.code() {
            match btcode {
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
                Bytecode::RetVoid => {}
                _ => panic!("bytecode {:?} not implemented", btcode),
            }
        }

        self.emit_epilog(&bytecode);

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
