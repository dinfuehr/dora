use baseline::map::CodeDescriptor;
use exception::DoraToNativeInfo;
use gc::Address;
use vm::VM;

pub fn get_rootset(ctxt: &VM) -> Vec<Slot> {
    let mut rootset = Vec::new();

    determine_rootset_from_stack(&mut rootset, ctxt);
    determine_rootset_from_globals(&mut rootset, ctxt);
    determine_rootset_from_handles(&mut rootset, ctxt);

    rootset
}

fn determine_rootset_from_handles(rootset: &mut Vec<Slot>, ctxt: &VM) {
    for rooted in ctxt.handles.iter() {
        let slot = Slot::at(Address::from_ptr(rooted.raw()));
        rootset.push(slot);
    }
}

fn determine_rootset_from_globals(rootset: &mut Vec<Slot>, ctxt: &VM) {
    for glob in ctxt.globals.iter() {
        let glob = glob.borrow();

        if !glob.ty.reference_type() {
            continue;
        }

        let slot = Slot::at(Address::from_ptr(glob.address_value));
        rootset.push(slot);
    }
}

fn determine_rootset_from_stack(rootset: &mut Vec<Slot>, ctxt: &VM) {
    assert!(!ctxt.dtn.borrow().is_null());

    let mut dtn = *ctxt.dtn.borrow();

    while !dtn.is_null() {
        dtn = from_dora_to_native_info(rootset, ctxt, dtn);
    }
}

fn from_dora_to_native_info(
    rootset: &mut Vec<Slot>,
    ctxt: &VM,
    dtn: *const DoraToNativeInfo,
) -> *const DoraToNativeInfo {
    let dtn = unsafe { &*dtn };

    let mut pc: usize = dtn.pc;
    let mut fp: usize = dtn.fp;

    while fp != 0 {
        if !determine_rootset(rootset, ctxt, fp, pc) {
            break;
        }

        pc = unsafe { *((fp + 8) as *const usize) };
        fp = unsafe { *(fp as *const usize) };
    }

    dtn.last
}

fn determine_rootset(rootset: &mut Vec<Slot>, ctxt: &VM, fp: usize, pc: usize) -> bool {
    let code_map = ctxt.code_map.lock().unwrap();
    let data = code_map.get(pc as *const u8);

    match data {
        Some(CodeDescriptor::DoraFct(fct_id)) => {
            let jit_fct = ctxt.jit_fcts[fct_id].borrow();

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
