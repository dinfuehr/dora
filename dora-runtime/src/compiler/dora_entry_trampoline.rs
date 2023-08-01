use std::sync::Arc;

use crate::cpu::{
    CCALL_REG_PARAMS, REG_FP, REG_PARAMS, REG_THREAD, REG_TMP1, STACK_FRAME_ALIGNMENT,
};
use crate::masm::{CodeDescriptor, MacroAssembler, Mem};
use crate::mem;
use crate::mode::MachineMode;
use crate::vm::{install_code_stub, Code, CodeKind, VM};

pub fn install<'a>(vm: &'a VM) -> Arc<Code> {
    let ngen = DoraEntryGen {
        vm,
        masm: MacroAssembler::new(),
        dbg: vm.flags.emit_debug_entry,
    };

    ngen.install()
}

pub fn generate<'a>(vm: &'a VM) -> CodeDescriptor {
    let ngen = DoraEntryGen {
        vm,
        masm: MacroAssembler::new(),
        dbg: vm.flags.emit_debug_entry,
    };

    ngen.generate()
}

const FP_CALLER_PC_OFFSET: i32 = FP_CALLER_FP_OFFSET + mem::ptr_width();
const FP_CALLER_FP_OFFSET: i32 = 0;

const FRAME_THREAD_SIZE: i32 = mem::ptr_width();
const FP_THREAD_OFFSET: i32 = FP_CALLER_FP_OFFSET - FRAME_THREAD_SIZE;

const UNALIGNED_FRAME_SIZE: i32 = FP_CALLER_FP_OFFSET - FP_THREAD_OFFSET;
const FRAME_SIZE: i32 = mem::align_i32(UNALIGNED_FRAME_SIZE, STACK_FRAME_ALIGNMENT as i32);

struct DoraEntryGen<'a> {
    vm: &'a VM,
    masm: MacroAssembler,
    dbg: bool,
}

impl<'a> DoraEntryGen<'a> {
    pub fn install(self) -> Arc<Code> {
        let vm = self.vm;
        let code_descriptor = DoraEntryGen::generate(self);
        install_code_stub(vm, code_descriptor, CodeKind::DoraEntryTrampoline)
    }

    pub fn generate(mut self) -> CodeDescriptor {
        if self.dbg {
            self.masm.debug();
        }

        self.masm.prolog(FRAME_SIZE);

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_FP, FP_THREAD_OFFSET),
            REG_THREAD.into(),
        );

        self.masm
            .copy_reg(MachineMode::Ptr, REG_THREAD, CCALL_REG_PARAMS[0]);
        self.masm
            .copy_reg(MachineMode::Ptr, REG_TMP1, CCALL_REG_PARAMS[1]);
        self.masm
            .copy_reg(MachineMode::Ptr, REG_PARAMS[0], CCALL_REG_PARAMS[2]);
        self.masm.call_reg(REG_TMP1);

        self.masm.load_mem(
            MachineMode::Ptr,
            REG_THREAD.into(),
            Mem::Base(REG_FP, FP_THREAD_OFFSET),
        );
        self.masm.epilog();

        self.masm.code()
    }
}
