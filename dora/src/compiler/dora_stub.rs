use crate::cpu::{CCALL_REG_PARAMS, REG_PARAMS, REG_SP, REG_THREAD, REG_TMP1};
use crate::gc::Address;
use crate::masm::{MacroAssembler, Mem};
use crate::mem;
use crate::mode::MachineMode;
use crate::vm::{Code, CodeKind, VM};

pub fn generate<'a>(vm: &'a VM) -> Address {
    let ngen = DoraEntryGen {
        vm,
        masm: MacroAssembler::new(),
        dbg: vm.args.flag_emit_debug_entry,
    };

    let code = ngen.generate();
    let ptr = code.instruction_start();

    vm.add_code(code);

    ptr
}

struct DoraEntryGen<'a> {
    vm: &'a VM,
    masm: MacroAssembler,
    dbg: bool,
}

impl<'a> DoraEntryGen<'a> {
    pub fn generate(mut self) -> Code {
        let framesize = mem::ptr_width_usize();
        let framesize = mem::align_usize(framesize, 16) as i32;

        let offset_thread = 0;

        if self.dbg {
            self.masm.debug();
        }

        self.masm.prolog_size(framesize);
        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_thread),
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
            Mem::Base(REG_SP, offset_thread),
        );
        self.masm.epilog();

        self.masm.code(self.vm, framesize, CodeKind::DoraStub)
    }
}
