use baseline::map::CodeDescriptor;
use ctxt::VM;
use exception::DoraToNativeInfo;
use gc::Address;

pub fn get_rootset(vm: &VM) -> Vec<Slot> {
    let mut rootset = Vec::new();

    determine_rootset_from_stack(&mut rootset, vm);
    determine_rootset_from_globals(&mut rootset, vm);
    determine_rootset_from_handles(&mut rootset, vm);

    rootset
}

fn determine_rootset_from_handles(rootset: &mut Vec<Slot>, vm: &VM) {
    for rooted in vm.handles.iter() {
        let slot = Slot::at(Address::from_ptr(rooted.raw()));
        rootset.push(slot);
    }
}

fn determine_rootset_from_globals(rootset: &mut Vec<Slot>, vm: &VM) {
    for glob in vm.globals.iter() {
        let glob = glob.lock();

        if !glob.ty.reference_type() {
            continue;
        }

        let slot = Slot::at(Address::from_ptr(glob.address_value));
        rootset.push(slot);
    }
}

fn determine_rootset_from_stack(rootset: &mut Vec<Slot>, vm: &VM) {
    assert!(!vm.dtn.lock().is_null());

    let mut dtn = *vm.dtn.lock();

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
    let data = code_map.get(pc as *const u8);

    match data {
        Some(CodeDescriptor::DoraFct(fct_id)) => {
            let jit_fct = vm.jit_fcts.idx(fct_id);

            let offset = pc - (jit_fct.fct_ptr() as usize);
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
