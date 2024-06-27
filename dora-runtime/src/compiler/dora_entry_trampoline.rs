use std::sync::Arc;

use crate::cpu::{
    CALLEE_SAVED_FREGS, CALLEE_SAVED_REGS, CCALL_REG_PARAMS, REG_FP, REG_PARAMS, REG_THREAD,
    REG_TMP1, STACK_FRAME_ALIGNMENT,
};
use crate::masm::{MacroAssembler, Mem};
use crate::mem;
use crate::mode::MachineMode;
use crate::vm::{install_code_stub, Code, CodeDescriptor, CodeKind, VM};

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

const FRAME_CALLEE_REGS_SIZE: i32 = (CALLEE_SAVED_REGS.len() as i32) * mem::ptr_width();
const FP_CALLEE_REGS_OFFSET: i32 = FP_CALLER_FP_OFFSET - FRAME_CALLEE_REGS_SIZE;
const FRAME_CALLEE_FREGS_SIZE: i32 = (CALLEE_SAVED_FREGS.len() as i32) * mem::ptr_width();
const FP_CALLEE_FREGS_OFFSET: i32 = FP_CALLEE_REGS_OFFSET - FRAME_CALLEE_FREGS_SIZE;

const UNALIGNED_FRAME_SIZE: i32 = FP_CALLER_FP_OFFSET - FP_CALLEE_FREGS_OFFSET;
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
        self.save_callee_saved_regs();

        self.masm
            .copy_reg(MachineMode::Ptr, REG_THREAD, CCALL_REG_PARAMS[0]);
        self.masm
            .copy_reg(MachineMode::Ptr, REG_TMP1, CCALL_REG_PARAMS[1]);
        self.masm
            .copy_reg(MachineMode::Ptr, REG_PARAMS[0], CCALL_REG_PARAMS[2]);
        self.masm.call_reg(REG_TMP1);

        self.load_callee_saved_regs();
        self.masm.epilog();

        self.masm.code()
    }

    pub fn save_callee_saved_regs(&mut self) {
        for (idx, &reg) in CALLEE_SAVED_REGS.iter().enumerate() {
            self.masm.store_mem(
                MachineMode::Ptr,
                Mem::Base(
                    REG_FP,
                    FP_CALLEE_REGS_OFFSET + (idx as i32) * mem::ptr_width(),
                ),
                reg.into(),
            );
        }

        for (idx, &reg) in CALLEE_SAVED_FREGS.iter().enumerate() {
            self.masm.store_mem(
                MachineMode::Float64,
                Mem::Base(
                    REG_FP,
                    FP_CALLEE_FREGS_OFFSET + (idx as i32) * mem::ptr_width(),
                ),
                reg.into(),
            );
        }
    }

    pub fn load_callee_saved_regs(&mut self) {
        for (idx, &reg) in CALLEE_SAVED_REGS.iter().enumerate() {
            self.masm.load_mem(
                MachineMode::Ptr,
                reg.into(),
                Mem::Base(
                    REG_FP,
                    FP_CALLEE_REGS_OFFSET + (idx as i32) * mem::ptr_width(),
                ),
            );
        }

        for (idx, &reg) in CALLEE_SAVED_FREGS.iter().enumerate() {
            self.masm.load_mem(
                MachineMode::Float64,
                reg.into(),
                Mem::Base(
                    REG_FP,
                    FP_CALLEE_FREGS_OFFSET + (idx as i32) * mem::ptr_width(),
                ),
            );
        }
    }
}
