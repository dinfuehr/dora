use std::sync::Arc;

use crate::gc::Address;
use crate::language::ty::SourceType;
use crate::stack::DoraToNativeInfo;
use crate::threads::DoraThread;
use crate::vm::{
    get_concrete_tuple_ty, specialize_enum_id_params, specialize_value_id_params, CodeKind,
    EnumLayout, VM,
};

pub fn determine_strong_roots(vm: &VM, threads: &[Arc<DoraThread>]) -> Vec<Slot> {
    let mut rootset = Vec::new();

    iterate_strong_roots(vm, threads, |slot| {
        rootset.push(slot);
    });

    rootset
}

pub fn iterate_strong_roots<F: FnMut(Slot)>(vm: &VM, threads: &[Arc<DoraThread>], mut callback: F) {
    for thread in threads {
        iterate_roots_from_stack(vm, thread, &mut callback);
        iterate_roots_from_handles(thread, &mut callback);
    }

    iterate_roots_from_code_space(vm, &mut callback);

    iterate_roots_from_globals(vm, &mut callback);
    iterate_roots_from_wait_list(vm, &mut callback);
}

fn iterate_roots_from_wait_list<F: FnMut(Slot)>(vm: &VM, callback: &mut F) {
    vm.wait_lists.visit_roots(|slot| {
        callback(slot);
    });
}

fn iterate_roots_from_handles<F: FnMut(Slot)>(thread: &DoraThread, callback: &mut F) {
    for rooted in thread.handles.iter() {
        let slot = Slot::at(rooted.location());
        callback(slot);
    }
}

fn iterate_roots_from_code_space<F: FnMut(Slot)>(vm: &VM, _callback: &mut F) {
    let allocated_region = vm.gc.code_space.allocated_region();
    let mut current = allocated_region.start;

    while current < allocated_region.end {
        let object = current.to_obj();
        current = current.offset(object.size())
    }

    assert_eq!(current, allocated_region.end);
}

fn iterate_roots_from_globals<F: FnMut(Slot)>(vm: &VM, callback: &mut F) {
    for global_var in vm.globals.iter() {
        let global_var = global_var.read();

        match global_var.ty {
            SourceType::Value(value_id, ref type_params) => {
                let sdef_id = specialize_value_id_params(vm, value_id, type_params.clone());
                let sdef = vm.value_instances.idx(sdef_id);

                for &offset in &sdef.ref_fields {
                    let slot_address = global_var.address_value.offset(offset as usize);
                    let slot = Slot::at(slot_address);
                    callback(slot);
                }
            }

            SourceType::Enum(enum_id, ref type_params) => {
                let edef_id = specialize_enum_id_params(vm, enum_id, type_params.clone());
                let edef = vm.enum_instances.idx(edef_id);

                match edef.layout {
                    EnumLayout::Int => {}
                    EnumLayout::Ptr | EnumLayout::Tagged => {
                        let slot = Slot::at(global_var.address_value);
                        callback(slot);
                    }
                }
            }

            SourceType::Tuple(_) => {
                let tuple = get_concrete_tuple_ty(vm, &global_var.ty);

                for &offset in tuple.references() {
                    let slot_address = global_var.address_value.offset(offset as usize);
                    let slot = Slot::at(slot_address);
                    callback(slot);
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
                let slot = Slot::at(global_var.address_value);
                callback(slot);
            }

            SourceType::TypeParam(_)
            | SourceType::Error
            | SourceType::Any
            | SourceType::This
            | SourceType::Lambda(_, _)
            | SourceType::Ptr => unreachable!(),
        }
    }
}

fn iterate_roots_from_stack<F: FnMut(Slot)>(vm: &VM, thread: &DoraThread, callback: &mut F) {
    let mut dtn = thread.dtn();

    while !dtn.is_null() {
        dtn = iterate_roots_from_dora_to_native_info(vm, dtn, callback);
    }
}

fn iterate_roots_from_dora_to_native_info<F: FnMut(Slot)>(
    vm: &VM,
    dtn: *const DoraToNativeInfo,
    callback: &mut F,
) -> *const DoraToNativeInfo {
    let dtn = unsafe { &*dtn };

    let mut pc: usize = dtn.pc;
    let mut fp: usize = dtn.fp;

    while fp != 0 {
        if !iterate_roots_from_stack_frame(vm, fp, pc, callback) {
            break;
        }

        pc = unsafe { *((fp + 8) as *const usize) };
        fp = unsafe { *(fp as *const usize) };
    }

    dtn.last
}

fn iterate_roots_from_stack_frame<F: FnMut(Slot)>(
    vm: &VM,
    fp: usize,
    pc: usize,
    callback: &mut F,
) -> bool {
    let code_id = vm.code_map.get(pc.into());

    if let Some(code_id) = code_id {
        let code = vm.code_objects.get(code_id);

        match code.descriptor() {
            CodeKind::DoraFct(_) => {
                let offset = pc - code.instruction_start().to_usize();
                let gcpoint = code.gcpoint_for_offset(offset as u32).expect("no gcpoint");

                for &offset in &gcpoint.offsets {
                    let addr = (fp as isize + offset as isize) as usize;
                    callback(Slot::at(addr.into()));
                }

                true
            }

            CodeKind::NativeStub(_) => {
                let gcpoint = code.gcpoint_for_offset(0).expect("no gcpoint");

                for &offset in &gcpoint.offsets {
                    let addr = (fp as isize + offset as isize) as usize;
                    callback(Slot::at(addr.into()));
                }

                true
            }

            CodeKind::AllocStub => true,
            CodeKind::DoraStub => false,
            CodeKind::GuardCheckStub => true,
            CodeKind::SafepointStub => true,
            CodeKind::CompileStub => true,

            CodeKind::VerifyStub | CodeKind::TrapStub => unreachable!(),
        }
    } else {
        println!("no code found at pc = {:x}", pc);
        vm.code_map.dump(vm);
        panic!("invalid stack frame");
    }
}

pub fn iterate_weak_roots<F>(vm: &VM, object_updater: F)
where
    F: Fn(Address) -> Option<Address>,
{
    let mut finalizers = vm.gc.finalizers.lock();
    let mut deleted = false;

    for (address, _) in &mut *finalizers {
        *address = if let Some(new_address) = object_updater(*address) {
            new_address
        } else {
            deleted = true;
            Address::null()
        };
    }

    if deleted {
        finalizers.retain(|(address, _)| !address.is_null());
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
