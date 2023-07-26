use std::sync::Arc;

use dora_bytecode::{BytecodeType, BytecodeTypeArray};

use crate::compiler::lazy_compilation_stub;
use crate::gc::Address;
use crate::stack::DoraToNativeInfo;
use crate::threads::DoraThread;
use crate::vm::{specialize_bty, specialize_bty_array, CodeKind, LazyCompilationSite, VM};

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
    for rooted in thread.handles.iterate_for_gc() {
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
    let global_variable_memory = vm
        .global_variable_memory
        .as_ref()
        .expect("uninitialized global memory");
    let address_start = global_variable_memory.start();

    for &slot_offset in global_variable_memory.references() {
        let slot_address = address_start.offset(slot_offset as usize);
        callback(Slot::at(slot_address));
    }
}

fn iterate_roots_from_stack<F: FnMut(Slot)>(vm: &VM, thread: &DoraThread, callback: &mut F) {
    let mut dtn = thread.dtn();

    while !dtn.is_null() {
        dtn = iterate_roots_from_dora_to_native_info(vm, dtn, callback);
    }
}

#[derive(Copy, Clone)]
struct ManagedFrame {
    pc: Address,
    fp: Address,
}

fn iterate_roots_from_dora_to_native_info<F: FnMut(Slot)>(
    vm: &VM,
    dtn: *const DoraToNativeInfo,
    callback: &mut F,
) -> *const DoraToNativeInfo {
    let dtn = unsafe { &*dtn };

    let mut frame = ManagedFrame {
        pc: dtn.pc.into(),
        fp: dtn.fp.into(),
    };

    while frame.fp.is_non_null() {
        if !iterate_roots_from_stack_frame(vm, frame, callback) {
            break;
        }

        frame = read_caller_frame(frame.fp);
    }

    dtn.last
}

fn read_caller_frame(fp: Address) -> ManagedFrame {
    let pc: Address = unsafe { *fp.add_ptr(1).to_ptr::<Address>() };
    let fp: Address = unsafe { *fp.to_ptr::<Address>() };

    ManagedFrame { pc, fp }
}

fn iterate_roots_from_stack_frame<F: FnMut(Slot)>(
    vm: &VM,
    frame: ManagedFrame,
    callback: &mut F,
) -> bool {
    let code_id = vm.code_map.get(frame.pc.into());

    if let Some(code_id) = code_id {
        let code = vm.code_objects.get(code_id);

        match code.descriptor() {
            CodeKind::BaselineFct(_) => {
                let offset = frame.pc.offset_from(code.instruction_start());
                let gcpoint = code.gcpoint_for_offset(offset as u32).expect("no gcpoint");

                for &offset in &gcpoint.offsets {
                    let addr = frame.fp.ioffset(offset as isize);
                    callback(Slot::at(addr.into()));
                }

                true
            }

            CodeKind::RuntimeEntryTrampoline(_) => {
                let gcpoint = code.gcpoint_for_offset(0).expect("no gcpoint");

                for &offset in &gcpoint.offsets {
                    let addr = frame.fp.ioffset(offset as isize);
                    callback(Slot::at(addr.into()));
                }

                true
            }

            CodeKind::LazyCompilationStub => {
                iterate_lazy_compilation_stub_roots(vm, frame, callback);
                true
            }

            CodeKind::AllocationFailureTrampoline => true,
            CodeKind::DoraEntryTrampoline => false,
            CodeKind::StackOverflowTrampoline => true,
            CodeKind::SafepointTrampoline => true,

            CodeKind::TrapTrampoline => unreachable!(),
        }
    } else {
        println!("no code found at pc = {}", frame.pc);
        vm.code_map.dump(vm);
        panic!("invalid stack frame");
    }
}

fn iterate_lazy_compilation_stub_roots<F>(vm: &VM, frame: ManagedFrame, callback: F)
where
    F: FnMut(Slot),
{
    let caller_frame = read_caller_frame(frame.fp);
    let code_id = vm
        .code_map
        .get(caller_frame.pc.into())
        .expect("code not found");
    let code = vm.code_objects.get(code_id);
    let offset: u32 = caller_frame
        .pc
        .offset_from(code.instruction_start())
        .try_into()
        .expect("too large");
    let _lazy_compilation_site = code
        .lazy_for_offset(offset)
        .expect("missing lazy compilation site")
        .clone();

    let (params, is_variadic, return_type) = match _lazy_compilation_site {
        LazyCompilationSite::Direct(fct_id, _, type_params) => {
            let fct = &vm.program.functions[fct_id.0 as usize];
            let params = BytecodeTypeArray::new(fct.params.clone());
            let params = specialize_bty_array(&params, &type_params);
            let return_type = specialize_bty(fct.return_type.clone(), &type_params);
            (params, fct.is_variadic, return_type)
        }

        LazyCompilationSite::Virtual(
            _receiver_is_first,
            trait_object_ty,
            fct_id,
            _,
            type_params,
        ) => {
            let fct = &vm.program.functions[fct_id.0 as usize];
            let mut params = fct.params.clone();
            assert_eq!(params[0], BytecodeType::This);
            params[0] = trait_object_ty;
            let params = BytecodeTypeArray::new(params);
            let params = specialize_bty_array(&params, &type_params);
            let return_type = specialize_bty(fct.return_type.clone(), &type_params);
            (params, fct.is_variadic, return_type)
        }

        LazyCompilationSite::Lambda(_, params, return_type) => {
            debug_assert!(params.is_concrete_type());
            debug_assert!(return_type.is_concrete_type());
            (params, false, return_type)
        }
    };

    lazy_compilation_stub::iterate_roots(vm, frame.fp, &params, is_variadic, return_type, callback)
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
