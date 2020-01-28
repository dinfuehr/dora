use std::collections::hash_map::HashMap;
use std::mem::size_of;

use dora_parser::lexer::position::Position;

use crate::compiler::fct::{Code, JitDescriptor, JitFct, JitFctId};
use crate::compiler::map::CodeDescriptor;
use crate::cpu::{Mem, REG_FP, REG_PARAMS, REG_SP, REG_THREAD, REG_TMP1};
use crate::exception::DoraToNativeInfo;
use crate::gc::Address;
use crate::masm::MacroAssembler;
use crate::mem;
use crate::threads::ThreadLocalData;
use crate::ty::{BuiltinType, MachineMode};
use crate::vm::FctId;
use crate::vm::VM;

pub struct NativeStubs {
    map: HashMap<Address, JitFctId>,
}

impl NativeStubs {
    pub fn new() -> NativeStubs {
        NativeStubs {
            map: HashMap::new(),
        }
    }

    pub fn find_fct(&self, ptr: Address) -> Option<JitFctId> {
        self.map.get(&ptr).map(|&jit_fct_id| jit_fct_id)
    }

    pub fn insert_fct(&mut self, ptr: Address, fct: JitFctId) {
        self.map.entry(ptr).or_insert(fct);
    }
}

#[derive(Clone)]
pub enum NativeFctDescriptor {
    NativeStub(FctId),
    AllocStub,
    VerifyStub,
    TrapStub,
}

pub struct NativeFct<'a> {
    pub ptr: Address,
    pub args: &'a [BuiltinType],
    pub return_type: BuiltinType,
    pub throws: bool,
    pub desc: NativeFctDescriptor,
}

pub fn generate<'a, 'ast: 'a>(vm: &'a VM<'ast>, fct: NativeFct, dbg: bool) -> JitFctId {
    let fct_desc = fct.desc.clone();

    let ngen = NativeGen {
        vm,
        masm: MacroAssembler::new(),
        fct,
        dbg,
    };

    let jit_fct = ngen.generate();
    let jit_start = jit_fct.ptr_start();
    let jit_end = jit_fct.ptr_end();
    let jit_fct_id: JitFctId = vm.jit_fcts.push(JitFct::Compiled(jit_fct)).into();

    let code_desc = match fct_desc {
        NativeFctDescriptor::NativeStub(_) => CodeDescriptor::NativeStub(jit_fct_id),
        NativeFctDescriptor::TrapStub => CodeDescriptor::TrapStub,
        NativeFctDescriptor::VerifyStub => CodeDescriptor::VerifyStub,
        NativeFctDescriptor::AllocStub => CodeDescriptor::AllocStub,
    };

    vm.insert_code_map(jit_start, jit_end, code_desc);

    jit_fct_id
}

struct NativeGen<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    masm: MacroAssembler,

    fct: NativeFct<'a>,
    dbg: bool,
}

impl<'a, 'ast> NativeGen<'a, 'ast>
where
    'ast: 'a,
{
    pub fn generate(mut self) -> Code {
        let save_return = self.fct.return_type != BuiltinType::Unit;
        let dtn_size = size_of::<DoraToNativeInfo>() as i32;

        let offset_dtn = 0;
        let offset_return = dtn_size;
        let offset_handles = offset_return + if save_return { mem::ptr_width() } else { 0 };
        let framesize = mem::align_i32(
            offset_handles + needed_handles(self.fct.args) as i32 * mem::ptr_width(),
            16,
        );

        if self.dbg {
            self.masm.debug();
        }

        self.masm.prolog_size(framesize);

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

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_THREAD, ThreadLocalData::dtn_offset()),
            REG_SP.into(),
        );

        store_params(&mut self.masm, self.fct.args, offset_handles);

        self.masm.raw_call(self.fct.ptr.to_ptr());

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

        self.masm.load_mem(
            MachineMode::Ptr,
            REG_TMP1.into(),
            Mem::Base(REG_THREAD, ThreadLocalData::exception_object_offset()),
        );

        let lbl_exception = self.masm.test_if_not_nil(REG_TMP1);

        self.masm.epilog();

        self.masm.bind_label(lbl_exception);
        self.masm.throw(REG_TMP1, Position::new(1, 1));
        self.masm.nop();

        let desc = match self.fct.desc {
            NativeFctDescriptor::NativeStub(fid) => JitDescriptor::NativeStub(fid),
            NativeFctDescriptor::AllocStub => JitDescriptor::AllocStub,
            NativeFctDescriptor::VerifyStub => JitDescriptor::VerifyStub,
            NativeFctDescriptor::TrapStub => JitDescriptor::TrapStub,
        };

        self.masm.jit(self.vm, framesize, desc, self.fct.throws)
    }
}

fn needed_handles(args: &[BuiltinType]) -> u32 {
    let mut reg_idx = 0;
    let mut count = 0;

    for &ty in args {
        if ty.is_float() {
            // ignore floating point arguments
        } else if ty.reference_type() && reg_idx < REG_PARAMS.len() {
            reg_idx += 1;
            count += 1;
        } else {
            reg_idx += 1;
        }
    }

    count
}

fn store_params(masm: &mut MacroAssembler, args: &[BuiltinType], offset_handles: i32) {
    let mut reg_idx = 0;
    let mut count = 0;

    for &ty in args {
        if ty.is_float() {
            // ignore floating point arguments
        } else if ty.reference_type() && reg_idx < REG_PARAMS.len() {
            masm.store_mem(
                MachineMode::Ptr,
                Mem::Base(REG_SP, offset_handles + count * mem::ptr_width()),
                REG_PARAMS[reg_idx].into(),
            );
            reg_idx += 1;
            count += 1;
        } else {
            reg_idx += 1;
        }
    }
}
