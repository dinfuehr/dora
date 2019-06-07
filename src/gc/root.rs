use std::sync::Arc;

use crate::baseline::map::CodeDescriptor;
use crate::ctxt::VM;
use crate::exception::DoraToNativeInfo;
use crate::gc::Address;
use crate::threads::DoraThread;

pub fn get_rootset(vm: &VM, threads: &[Arc<DoraThread>]) -> Vec<Slot> {
    let mut rootset = Vec::new();

    determine_rootset_from_stack(&mut rootset, vm, threads);
    determine_rootset_from_handles(&mut rootset, threads);

    determine_rootset_from_globals(&mut rootset, vm);

    rootset
}

fn determine_rootset_from_handles(rootset: &mut Vec<Slot>, threads: &[Arc<DoraThread>]) {
    for thread in threads {
        for rooted in thread.handles.iter() {
            let slot = Slot::at(Address::from_ptr(rooted.raw()));
            rootset.push(slot);
        }
    }
}

fn determine_rootset_from_globals(rootset: &mut Vec<Slot>, vm: &VM) {
    for glob in vm.globals.iter() {
        let glob = glob.lock();

        if !glob.ty.reference_type() {
            continue;
        }

        let slot = Slot::at(glob.address_value);
        rootset.push(slot);
    }
}

fn determine_rootset_from_stack(rootset: &mut Vec<Slot>, vm: &VM, threads: &[Arc<DoraThread>]) {
    for thread in threads {
        let dtn = Address::from_ptr(thread.dtn());
        determine_rootset_from_stack_for_thread(rootset, vm, dtn);
    }
}

fn determine_rootset_from_stack_for_thread(rootset: &mut Vec<Slot>, vm: &VM, dtn: Address) {
    let mut dtn = dtn.to_ptr::<DoraToNativeInfo>();

    while !dtn.is_null() {
        dtn = from_dora_to_native_info(rootset, vm, dtn);
    }
}

fn from_dora_to_native_info(
    rootset: &mut Vec<Slot>,
    vm: &VM,
    dtn: *const DoraToNativeInfo,
) -> *const DoraToNativeInfo {
    let dtn = unsafe { &*dtn };

    let mut pc: usize = dtn.pc;
    let mut fp: usize = dtn.fp;

    while fp != 0 {
        if !determine_rootset(rootset, vm, fp, pc) {
            break;
        }

        pc = unsafe { *((fp + 8) as *const usize) };
        fp = unsafe { *(fp as *const usize) };
    }

    dtn.last
}

fn determine_rootset(rootset: &mut Vec<Slot>, vm: &VM, fp: usize, pc: usize) -> bool {
    let code_map = vm.code_map.lock();
    let data = code_map.get(pc.into());

    match data {
        Some(CodeDescriptor::DoraFct(fct_id)) => {
            let jit_fct = vm.jit_fcts.idx(fct_id);

            let offset = pc - jit_fct.fct_ptr().to_usize();
            let jit_fct = jit_fct.to_base().expect("baseline expected");
            let gcpoint = jit_fct
                .gcpoint_for_offset(offset as i32)
                .expect("no gcpoint");

            for &offset in &gcpoint.offsets {
                let addr = (fp as isize + offset as isize) as usize;
                rootset.push(Slot::at(addr.into()));
            }

            true
        }

        Some(CodeDescriptor::AllocThunk) => true,
        Some(CodeDescriptor::NativeThunk(_)) => true,
        Some(CodeDescriptor::DoraEntry) => false,

        _ => {
            println!("data = {:?}", data);
            panic!("invalid stack frame");
        }
    }
}

#[derive(Copy, Clone)]
pub struct Slot(Address);

impl Slot {
    pub fn at(addr: Address) -> Slot {
        Slot(addr)
    }

    pub fn address(self) -> Address {
        self.0
    }

    pub fn get(self) -> Address {
        unsafe { *self.0.to_ptr::<Address>() }
    }

    pub fn set(self, obj: Address) {
        unsafe {
            *self.0.to_mut_ptr::<Address>() = obj;
        }
    }
}
