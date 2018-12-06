use baseline::fct::{JitBaselineFct, JitDescriptor, JitFct};
use baseline::map::CodeDescriptor;
use cpu::{Mem, REG_PARAMS, REG_SP, REG_THREAD, REG_TMP1};
use ctxt::VM;
use gc::Address;
use masm::MacroAssembler;
use mem;
use ty::MachineMode;

pub fn generate<'a, 'ast: 'a>(vm: &'a VM<'ast>) -> Address {
    let ngen = DoraEntryGen {
        vm: vm,
        masm: MacroAssembler::new(),
        dbg: vm.args.flag_emit_debug_entry,
    };

    let jit_fct = ngen.generate();
    let ptr = Address::from_ptr(jit_fct.fct_ptr());

    vm.insert_code_map(
        jit_fct.ptr_start(),
        jit_fct.ptr_end(),
        CodeDescriptor::DoraEntry,
    );
    vm.jit_fcts.push(JitFct::Base(jit_fct));

    ptr
}

struct DoraEntryGen<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    masm: MacroAssembler,
    dbg: bool,
}

impl<'a, 'ast> DoraEntryGen<'a, 'ast>
where
    'ast: 'a,
{
    pub fn generate(mut self) -> JitBaselineFct {
        let framesize = mem::ptr_width_usize();
        let framesize = mem::align_usize(framesize, 16) as i32;

        let offset_thread = 0;

        if self.dbg {
            self.masm.debug();
        }

        self.masm.prolog(framesize);
        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_thread),
            REG_THREAD.into(),
        );

        let addr = self.vm.tld.as_ptr() as usize as i64;
        self.masm.load_int_const(MachineMode::Ptr, REG_THREAD, addr);
        self.masm
            .copy_reg(MachineMode::Ptr, REG_TMP1, REG_PARAMS[0]);
        self.masm
            .copy_reg(MachineMode::Ptr, REG_PARAMS[0], REG_PARAMS[1]);
        self.masm.call_reg(REG_TMP1);

        self.masm.load_mem(
            MachineMode::Ptr,
            REG_THREAD.into(),
            Mem::Base(REG_SP, offset_thread),
        );
        self.masm.epilog(framesize);

        self.masm
            .jit(self.vm, framesize, JitDescriptor::DoraEntry, false)
    }
}
