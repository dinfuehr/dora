use std::mem::size_of;

use baseline::dora_native::{finish_native_call, start_native_call};
use baseline::fct::{JitBaselineFct, JitDescriptor, JitFct};
use baseline::map::CodeDescriptor;
use cpu::{Mem, FREG_PARAMS, REG_FP, REG_PARAMS, REG_RESULT, REG_SP, REG_THREAD, REG_TMP1};
use ctxt::SemContext;
use exception::DoraToNativeInfo;
use gc::Address;
use masm::MacroAssembler;
use mem;
use stdlib::throw;
use ty::MachineMode;

pub fn generate<'a, 'ast: 'a>(ctxt: &'a SemContext<'ast>, dbg: bool) -> Address {
    let ngen = DoraThrowGen {
        ctxt: ctxt,
        masm: MacroAssembler::new(),
        dbg: dbg,
    };

    let jit_fct = ngen.generate();
    ctxt.insert_code_map(jit_fct.ptr_start(), jit_fct.ptr_end(), CodeDescriptor::ThrowThunk);
    let addr = Address::from_ptr(jit_fct.fct_ptr());

    ctxt.jit_fcts.push(JitFct::Base(jit_fct));

    addr
}

struct DoraThrowGen<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    masm: MacroAssembler,
    dbg: bool,
}

impl<'a, 'ast> DoraThrowGen<'a, 'ast>
where
    'ast: 'a,
{
    pub fn generate(mut self) -> JitBaselineFct {
        let framesize = size_of::<DoraToNativeInfo>() as i32
            + (REG_PARAMS.len() + FREG_PARAMS.len() + 2) as i32 * mem::ptr_width();
        let framesize = mem::align_i32(framesize, 16) as i32;

        let offset_params = 0;
        let offset_tmp =
            offset_params + (FREG_PARAMS.len() + REG_PARAMS.len()) as i32 * mem::ptr_width();
        let offset_thread = offset_tmp + mem::ptr_width();

        self.masm.copy_ra(REG_TMP1);
        self.masm.prolog(framesize);

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_thread),
            REG_THREAD.into(),
        );

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_tmp),
            REG_TMP1.into(),
        );
        self.store_params(offset_params);

        self.masm.copy_reg(MachineMode::Ptr, REG_PARAMS[0], REG_FP);
        self.masm.copy_pc(REG_PARAMS[1]);
        self.masm
            .direct_call_without_info(start_native_call as *const u8);

        self.masm.load_mem(
            MachineMode::Ptr,
            REG_PARAMS[0].into(),
            Mem::Base(REG_SP, offset_tmp),
        );
        self.masm.load_mem(
            MachineMode::Ptr,
            REG_PARAMS[1].into(),
            Mem::Base(REG_SP, offset_params),
        );
        self.masm
            .direct_call_without_info(throw as *const u8);
        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_tmp),
            REG_RESULT.into(),
        );

        self.masm
            .direct_call_without_info(finish_native_call as *const u8);

        self.masm.load_mem(
            MachineMode::Ptr,
            REG_TMP1.into(),
            Mem::Base(REG_SP, offset_tmp),
        );
        self.masm.load_mem(
            MachineMode::Ptr,
            REG_THREAD.into(),
            Mem::Base(REG_SP, offset_thread),
        );
        self.load_params(offset_params);
        self.masm.epilog_without_return(framesize);
        self.masm.jump_reg(REG_TMP1);

        self.masm
            .jit(self.ctxt, framesize, JitDescriptor::ThrowThunk, false)
    }

    fn store_params(&mut self, mut offset: i32) {
        for reg in &REG_PARAMS {
            self.masm
                .store_mem(MachineMode::Ptr, Mem::Base(REG_SP, offset), (*reg).into());
            offset += mem::ptr_width();
        }

        for reg in &FREG_PARAMS {
            self.masm.store_mem(
                MachineMode::Float64,
                Mem::Base(REG_SP, offset),
                (*reg).into(),
            );
            offset += mem::ptr_width();
        }
    }

    fn load_params(&mut self, mut offset: i32) {
        for reg in &REG_PARAMS {
            self.masm
                .load_mem(MachineMode::Ptr, (*reg).into(), Mem::Base(REG_SP, offset));
            offset += mem::ptr_width();
        }

        for reg in &FREG_PARAMS {
            self.masm.load_mem(
                MachineMode::Float64,
                (*reg).into(),
                Mem::Base(REG_SP, offset),
            );
            offset += mem::ptr_width();
        }
    }
}
