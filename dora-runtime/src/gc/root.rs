use std::sync::Arc;

use crate::gc::Address;
use crate::runtime::{CodeKind, Runtime};
use crate::stack::DoraToNativeInfo;
use crate::threads::DoraThread;

pub fn determine_strong_roots(rt: &Runtime, threads: &[Arc<DoraThread>]) -> Vec<Slot> {
    let mut rootset = Vec::new();

    iterate_strong_roots(rt, threads, |slot| {
        rootset.push(slot);
    });

    rootset
}

pub fn iterate_strong_roots<F: FnMut(Slot)>(
    rt: &Runtime,
    threads: &[Arc<DoraThread>],
    mut callback: F,
) {
    for thread in threads {
        iterate_roots_from_stack(rt, thread, &mut callback);
        iterate_roots_from_handles(thread, &mut callback);
    }

    iterate_roots_from_globals(rt, &mut callback);
    iterate_roots_from_wait_list(rt, &mut callback);
}

fn iterate_roots_from_wait_list<F: FnMut(Slot)>(rt: &Runtime, callback: &mut F) {
    rt.wait_lists.visit_roots(|slot| {
        callback(slot);
    });
}

fn iterate_roots_from_handles<F: FnMut(Slot)>(thread: &DoraThread, callback: &mut F) {
    for rooted in thread.handles.iterate_for_gc() {
        let slot = Slot::at(rooted.location());
        callback(slot);
    }
}

fn iterate_roots_from_globals<F: FnMut(Slot)>(rt: &Runtime, callback: &mut F) {
    let Some(global_variable_memory) = rt.global_variable_memory.as_ref() else {
        return;
    };
    let address_start = global_variable_memory.start();

    for &slot_offset in global_variable_memory.references() {
        let slot_address = address_start.offset(slot_offset as usize);
        callback(Slot::at(slot_address));
    }
}

fn iterate_roots_from_stack<F: FnMut(Slot)>(rt: &Runtime, thread: &DoraThread, callback: &mut F) {
    let mut dtn = thread.dtn();

    while !dtn.is_null() {
        dtn = iterate_roots_from_dora_to_native_info(rt, dtn, callback);
    }
}

#[derive(Copy, Clone)]
struct ManagedFrame {
    pc: Address,
    fp: Address,
}

fn iterate_roots_from_dora_to_native_info<F: FnMut(Slot)>(
    rt: &Runtime,
    dtn: *const DoraToNativeInfo,
    callback: &mut F,
) -> *const DoraToNativeInfo {
    let dtn = unsafe { &*dtn };

    let mut frame = ManagedFrame {
        pc: dtn.pc.into(),
        fp: dtn.fp.into(),
    };

    while frame.fp.is_non_null() {
        if !iterate_roots_from_stack_frame(rt, frame, callback) {
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
    rt: &Runtime,
    frame: ManagedFrame,
    callback: &mut F,
) -> bool {
    let code_id = rt.code_map.get(frame.pc.into());

    if let Some(code_id) = code_id {
        let code = rt.code_map.get_code(code_id);

        match code.descriptor() {
            CodeKind::OptimizedFct(_) => {
                let offset = frame.pc.offset_from(code.instruction_start());
                let gcpoint = code.gcpoint_for_offset(offset as u32).expect("no gcpoint");

                for &offset in &gcpoint.offsets {
                    let addr = frame.fp.ioffset(offset as isize);
                    callback(Slot::at(addr.into()));
                }

                true
            }

            CodeKind::RuntimeEntryTrampoline(_)
            | CodeKind::UnreachableTrampoline
            | CodeKind::StackOverflowTrampoline
            | CodeKind::FatalErrorTrampoline => {
                let gcpoint = code.gcpoint_for_offset(0).expect("no gcpoint");

                for &offset in &gcpoint.offsets {
                    let addr = frame.fp.ioffset(offset as isize);
                    callback(Slot::at(addr.into()));
                }

                true
            }

            CodeKind::AllocationFailureTrampoline => true,
            CodeKind::DoraEntryTrampoline => false,
            CodeKind::SafepointTrampoline => true,

            CodeKind::TrapTrampoline => unreachable!(),
        }
    } else {
        println!("no code found at pc = {}", frame.pc);
        rt.code_map.dump(rt);
        panic!("invalid stack frame");
    }
}

pub fn iterate_weak_roots<F>(rt: &Runtime, mut object_updater: F)
where
    F: FnMut(Address) -> Option<Address>,
{
    let mut finalizers = rt.gc.finalizers.lock();
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
