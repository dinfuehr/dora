use std::mem::size_of;

use crate::compiler;
use crate::compiler::fct::{Code, JitDescriptor, JitFct, LazyCompilationSite};
use crate::compiler::map::CodeDescriptor;
use crate::cpu::{
    Mem, CCALL_REG_PARAMS, FREG_PARAMS, REG_FP, REG_PARAMS, REG_RESULT, REG_SP, REG_THREAD,
    REG_TMP1,
};
use crate::exception::DoraToNativeInfo;
use crate::gc::Address;
use crate::masm::MacroAssembler;
use crate::mem;
use crate::object::Obj;
use crate::threads::ThreadLocalData;
use crate::ty::{MachineMode, TypeList};
use crate::vm::FctId;
use crate::vm::{get_vm, VM};

// This code generates the compiler stub, there should only be one instance
// of this function be used in Dora. It is necessary for lazy compilation, where
// functions are only compiled on their first invocation. The compiler can use
// the address of this stub for invocations of functions that have not been compiled
// yet. The stub compiles the function and patches the call site to invoke the
// now-compiled function directly on the next invocation. In the end the function is
// executed.

pub fn generate<'a, 'ast: 'a>(vm: &'a VM<'ast>) -> Address {
    let ngen = DoraCompileGen {
        vm,
        masm: MacroAssembler::new(),
        dbg: vm.args.flag_emit_debug_compile,
    };

    let jit_fct = ngen.generate();
    let addr = jit_fct.instruction_start();
    vm.insert_code_map(
        jit_fct.ptr_start(),
        jit_fct.ptr_end(),
        CodeDescriptor::CompileStub,
    );
    vm.jit_fcts.push(JitFct::Compiled(jit_fct));

    addr
}

struct DoraCompileGen<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    masm: MacroAssembler,
    dbg: bool,
}

impl<'a, 'ast> DoraCompileGen<'a, 'ast>
where
    'ast: 'a,
{
    pub fn generate(mut self) -> Code {
        let offset_shadow_stack = 0;
        let offset_dtn = offset_shadow_stack
            + if cfg!(target_family = "windows") {
                32
            } else {
                0
            };
        let offset_params = offset_dtn + size_of::<DoraToNativeInfo>() as i32;
        let offset_thread =
            offset_params + (FREG_PARAMS.len() + REG_PARAMS.len()) as i32 * mem::ptr_width();
        let framesize = mem::align_i32(offset_thread + mem::ptr_width(), 16) as i32;

        if self.dbg {
            self.masm.debug();
        }

        // the return address is the call-site we need to patch
        self.masm.prolog_size(framesize);

        // store params passed in registers on the stack
        self.store_params(offset_params);

        // prepare the native call
        self.masm.load_mem(
            MachineMode::Ptr,
            REG_TMP1.into(),
            Mem::Base(REG_THREAD, ThreadLocalData::dtn_offset()),
        );

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_dtn + DoraToNativeInfo::last_offset()),
            REG_TMP1.into(),
        );

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_dtn + DoraToNativeInfo::fp_offset()),
            REG_FP.into(),
        );

        self.masm.copy_pc(REG_TMP1);

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_SP, offset_dtn + DoraToNativeInfo::pc_offset()),
            REG_TMP1.into(),
        );

        self.masm.copy_reg(MachineMode::Ptr, REG_TMP1, REG_SP);
        if offset_dtn != 0 {
            self.masm
                .int_add_imm(MachineMode::Ptr, REG_TMP1, REG_TMP1, offset_dtn as i64);
        }

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_THREAD, ThreadLocalData::dtn_offset()),
            REG_TMP1.into(),
        );

        // invoke the compiler for the call site
        self.masm.load_mem(
            MachineMode::Ptr,
            CCALL_REG_PARAMS[0].into(),
            Mem::Base(REG_FP, mem::ptr_width()),
        );
        self.masm.load_mem(
            MachineMode::Ptr,
            CCALL_REG_PARAMS[1].into(),
            Mem::Base(REG_SP, offset_params),
        );
        self.masm.raw_call(compile_request as *const u8);

        self.masm.load_mem(
            MachineMode::Ptr,
            REG_TMP1.into(),
            Mem::Base(REG_SP, offset_dtn + DoraToNativeInfo::last_offset()),
        );

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_THREAD, ThreadLocalData::dtn_offset()),
            REG_TMP1.into(),
        );

        self.masm.copy_reg(MachineMode::Ptr, REG_TMP1, REG_RESULT);

        // restore argument registers from the stack
        self.load_params(offset_params);

        // remove the stack frame
        self.masm.epilog_without_return();

        // jump to compiled function
        self.masm.jump_reg(REG_TMP1);

        self.masm
            .jit(self.vm, framesize, JitDescriptor::CompileStub, false)
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

fn compile_request(ra: usize, receiver: Address) -> Address {
    let vm = get_vm();

    let lazy_compilation_site = {
        let data = {
            let code_map = vm.code_map.lock();
            code_map.get(ra.into()).expect("return address not found")
        };

        let fct_id = match data {
            CodeDescriptor::DoraFct(fct_id) => fct_id,
            _ => panic!("expected function for code"),
        };

        let jit_fct = vm.jit_fcts.idx(fct_id);

        let offset = ra - jit_fct.instruction_start().to_usize();
        jit_fct
            .lazy_for_offset(offset as u32)
            .expect("lazy compilation site not found")
            .clone()
    };

    match lazy_compilation_site {
        LazyCompilationSite::Compile(fct_id, disp, ref cls_tps, ref fct_tps) => {
            patch_fct_call(vm, ra, fct_id, cls_tps, fct_tps, disp)
        }

        LazyCompilationSite::VirtCompile(vtable_index, ref cls_tps, ref fct_tps) => {
            patch_vtable_call(vm, receiver, vtable_index, cls_tps, fct_tps)
        }
    }
}

fn patch_vtable_call(
    vm: &VM,
    receiver: Address,
    vtable_index: u32,
    cls_tps: &TypeList,
    fct_tps: &TypeList,
) -> Address {
    let obj = unsafe { &mut *receiver.to_mut_ptr::<Obj>() };
    let vtable = obj.header().vtbl();
    let cls_id = vtable.class().cls_id.expect("no corresponding class");
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();

    let fct_id = cls.virtual_fcts[vtable_index as usize];
    let fct_ptr = compiler::generate(vm, fct_id, cls_tps, fct_tps);

    let methodtable = vtable.table_mut();
    methodtable[vtable_index as usize] = fct_ptr.to_usize();

    fct_ptr
}

fn patch_fct_call(
    vm: &VM,
    ra: usize,
    fct_id: FctId,
    cls_tps: &TypeList,
    fct_tps: &TypeList,
    disp: i32,
) -> Address {
    let fct_ptr = compiler::generate(vm, fct_id, cls_tps, fct_tps);
    let fct_addr: *mut usize = (ra as isize - disp as isize) as *mut _;

    // update function pointer in data segment
    unsafe {
        *fct_addr = fct_ptr.to_usize();
    }

    fct_ptr
}
