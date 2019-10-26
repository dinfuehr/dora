use std::mem::size_of;

use crate::baseline::dora_native::{finish_native_call, start_native_call};
use crate::baseline::fct::{JitBaselineFct, JitDescriptor, JitFct};
use crate::baseline::map::CodeDescriptor;
use crate::cpu::{Mem, REG_FP, REG_PARAMS, REG_SP, REG_THREAD, REG_TMP1, REG_TMP2};
use crate::exception::throw;
use crate::exception::DoraToNativeInfo;
use crate::gc::Address;
use crate::masm::MacroAssembler;
use crate::mem;
use crate::ty::MachineMode;
use crate::vm::VM;

pub fn generate<'a, 'ast: 'a>(vm: &'a VM<'ast>) -> Address {
    let ngen = DoraThrowGen {
        vm,
        masm: MacroAssembler::new(),
        dbg: vm.args.flag_emit_debug_compile,
    };

    let jit_fct = ngen.generate();
    vm.insert_code_map(
        jit_fct.ptr_start(),
        jit_fct.ptr_end(),
        CodeDescriptor::ThrowThunk,
    );
    let addr = jit_fct.fct_ptr();

    vm.jit_fcts.push(JitFct::Base(jit_fct));

    addr
}

struct DoraThrowGen<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    masm: MacroAssembler,
    dbg: bool,
}

impl<'a, 'ast> DoraThrowGen<'a, 'ast>
where
    'ast: 'a,
{
    pub fn generate(mut self) -> JitBaselineFct {
        let framesize = size_of::<DoraToNativeInfo>() as i32 + 4 * mem::ptr_width();
        let framesize = mem::align_i32(framesize, 16);

        let offset_thread = 0;
        let offset_result = offset_thread + mem::ptr_width();
        let offset_result_pc = offset_result;
        let offset_result_sp = offset_result_pc + mem::ptr_width();
        let offset_result_fp = offset_result_sp + mem::ptr_width();

        if self.dbg {
            self.masm.debug();
        }

        self.masm.prolog_size(framesize);

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_thread),
            REG_THREAD.into(),
        );

        self.masm.copy_reg(MachineMode::Ptr, REG_PARAMS[0], REG_FP);
        self.masm.copy_pc(REG_PARAMS[1]);
        self.masm.raw_call(start_native_call as *const u8);

        self.masm.copy_sp(REG_PARAMS[0]);
        self.masm.int_add_imm(
            MachineMode::Ptr,
            REG_PARAMS[0],
            REG_PARAMS[0],
            offset_result as i64,
        );
        self.masm.raw_call(throw as *const u8);

        self.masm.raw_call(finish_native_call as *const u8);

        self.masm.load_mem(
            MachineMode::Ptr,
            REG_THREAD.into(),
            Mem::Base(REG_SP, offset_thread),
        );

        self.masm.load_mem(
            MachineMode::Ptr,
            REG_TMP1.into(),
            Mem::Base(REG_SP, offset_result_pc),
        );
        self.masm.load_mem(
            MachineMode::Ptr,
            REG_FP.into(),
            Mem::Base(REG_SP, offset_result_fp),
        );
        self.masm.load_mem(
            MachineMode::Ptr,
            REG_TMP2.into(),
            Mem::Base(REG_SP, offset_result_sp),
        );

        self.masm.set_sp(REG_TMP2);
        self.masm.jump_reg(REG_TMP1);

        self.masm
            .jit(self.vm, framesize, JitDescriptor::ThrowThunk, false)
    }
}
