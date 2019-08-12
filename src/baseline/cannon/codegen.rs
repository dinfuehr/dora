use crate::baseline::asm::BaselineAssembler;
use crate::baseline::codegen::fct_pattern_match;
use crate::cpu::{Mem, REG_RESULT};
use dora_parser::ast::*;

use crate::baseline::codegen::{should_emit_debug, CodeGen, Scopes};
use crate::baseline::fct::{Comment, JitBaselineFct, JitDescriptor};
use crate::class::TypeParams;
use crate::ctxt::VM;
use crate::ctxt::{Fct, FctSrc};
use crate::masm::*;

use crate::bytecode::astgen::generate_fct;
use crate::bytecode::generate::{BytecodeFunction, Register};
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
        let Register(index) = dest;
        let bytecode_type = bytecode.registers().get(index).expect("register not found");
        let offset = bytecode.offset().get(index).expect("offset not found");

        if bool_const {
            self.asm.load_true(REG_RESULT);
        } else {
            self.asm.load_false(REG_RESULT);
        }
        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(*offset), REG_RESULT.into());
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
                Bytecode::RetVoid => {}
                _ => panic!("bytecode not implemented"),
            }
        }

        let always_returns = self.src.always_returns;

        if !always_returns {
            self.emit_epilog(&bytecode);
        }

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
