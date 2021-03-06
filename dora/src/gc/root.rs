use std::sync::Arc;

use crate::compiler::map::CodeDescriptor;
use crate::gc::Address;
use crate::semck::specialize::{specialize_enum_id_params, specialize_struct_id_params};
use crate::stack::DoraToNativeInfo;
use crate::threads::DoraThread;
use crate::ty::SourceType;
use crate::vm::{EnumLayout, VM};

pub fn get_rootset(vm: &VM, threads: &[Arc<DoraThread>]) -> Vec<Slot> {
    let mut rootset = Vec::new();

    determine_rootset_from_stack(&mut rootset, vm, threads);
    determine_rootset_from_handles(&mut rootset, threads);

    determine_rootset_from_globals(&mut rootset, vm);
    determine_rootset_from_wait_list(&mut rootset, vm);

    rootset
}

fn determine_rootset_from_handles(rootset: &mut Vec<Slot>, threads: &[Arc<DoraThread>]) {
    for thread in threads {
        for rooted in thread.handles.iter() {
            let slot = Slot::at(rooted.location());
            rootset.push(slot);
        }
    }
}

fn determine_rootset_from_globals(rootset: &mut Vec<Slot>, vm: &VM) {
    for glob in vm.globals.iter() {
        let glob = glob.read();

        match glob.ty {
            SourceType::Struct(struct_id, type_params_id) => {
                let type_params = vm.source_type_arrays.lock().get(type_params_id);
                let sdef_id = specialize_struct_id_params(vm, struct_id, type_params);
                let sdef = vm.struct_defs.idx(sdef_id);

                for &offset in &sdef.ref_fields {
                    let slot_address = glob.address_value.offset(offset as usize);
                    let slot = Slot::at(slot_address);
                    rootset.push(slot);
                }
            }

            SourceType::Enum(enum_id, type_params_id) => {
                let type_params = vm.source_type_arrays.lock().get(type_params_id);
                let edef_id = specialize_enum_id_params(vm, enum_id, type_params);
                let edef = vm.enum_defs.idx(edef_id);

                match edef.layout {
                    EnumLayout::Int => {}
                    EnumLayout::Ptr | EnumLayout::Tagged => {
                        let slot = Slot::at(glob.address_value);
                        rootset.push(slot);
                    }
                }
            }

            SourceType::Tuple(tuple_id) => {
                let tuples = vm.tuples.lock();
                let tuple = tuples.get_tuple(tuple_id);

                for &offset in tuple.offsets() {
                    let slot_address = glob.address_value.offset(offset as usize);
                    let slot = Slot::at(slot_address);
                    rootset.push(slot);
                }
            }

            SourceType::Unit
            | SourceType::UInt8
            | SourceType::Bool
            | SourceType::Char
            | SourceType::Int32
            | SourceType::Int64
            | SourceType::Float32
            | SourceType::Float64 => {}

            SourceType::Class(_, _) | SourceType::Trait(_, _) => {
                let slot = Slot::at(glob.address_value);
                rootset.push(slot);
            }

            SourceType::TypeParam(_)
            | SourceType::Error
            | SourceType::Any
            | SourceType::This
            | SourceType::Module(_)
            | SourceType::Lambda(_)
            | SourceType::Ptr => unreachable!(),
        }
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

            let offset = pc - jit_fct.instruction_start().to_usize();
            let gcpoint = jit_fct
                .gcpoint_for_offset(offset as u32)
                .expect("no gcpoint");

            for &offset in &gcpoint.offsets {
                let addr = (fp as isize + offset as isize) as usize;
                rootset.push(Slot::at(addr.into()));
            }

            true
        }

        Some(CodeDescriptor::NativeStub(fct_id)) => {
            let jit_fct = vm.jit_fcts.idx(fct_id);
            let gcpoint = jit_fct.gcpoint_for_offset(0).expect("no gcpoint");

            for &offset in &gcpoint.offsets {
                let addr = (fp as isize + offset as isize) as usize;
                rootset.push(Slot::at(addr.into()));
            }

            true
        }

        Some(CodeDescriptor::AllocStub) => true,
        Some(CodeDescriptor::DoraStub) => false,
        Some(CodeDescriptor::GuardCheckStub) => true,
        Some(CodeDescriptor::SafepointStub) => true,
        Some(CodeDescriptor::CompileStub) => true,

        _ => {
            println!("data = {:?}", data);
            panic!("invalid stack frame");
        }
    }
}

fn determine_rootset_from_wait_list(rootset: &mut Vec<Slot>, vm: &VM) {
    vm.wait_lists.visit_roots(|slot| {
        rootset.push(slot);
    });
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
