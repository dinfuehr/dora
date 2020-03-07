use parking_lot::{Condvar, Mutex};
use std::cell::RefCell;
use std::convert::From;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;

use crate::gc::{Address, Region, K};
use crate::handle::HandleMemory;
use crate::safepoint;
use crate::stack::DoraToNativeInfo;
use crate::vm::{get_vm, VM};

pub const STACK_SIZE: usize = 500 * K;

thread_local! {
    pub static THREAD: RefCell<Arc<DoraThread>> = RefCell::new(DoraThread::main());
}

pub struct Threads {
    pub threads: Mutex<Vec<Arc<DoraThread>>>,
    pub cond_join: Condvar,

    pub stopped: Mutex<usize>,
    pub reached_zero: Condvar,

    pub next_id: AtomicUsize,
    pub barrier: Barrier,
}

impl Threads {
    pub fn new() -> Threads {
        Threads {
            threads: Mutex::new(Vec::new()),
            cond_join: Condvar::new(),
            stopped: Mutex::new(0),
            reached_zero: Condvar::new(),
            next_id: AtomicUsize::new(1),
            barrier: Barrier::new(),
        }
    }

    pub fn attach_current_thread(&self) {
        THREAD.with(|thread| {
            let mut threads = self.threads.lock();
            threads.push(thread.borrow().clone());
        });
    }

    pub fn attach_thread(&self, thread: Arc<DoraThread>) {
        let mut threads = self.threads.lock();
        threads.push(thread);
    }

    pub fn next_id(&self) -> usize {
        self.next_id.fetch_add(1, Ordering::SeqCst)
    }

    pub fn detach_current_thread(&self) {
        THREAD.with(|thread| {
            thread.borrow().park(get_vm());
            let mut threads = self.threads.lock();
            threads.retain(|elem| !Arc::ptr_eq(elem, &*thread.borrow()));
            self.cond_join.notify_all();
            thread.borrow().unpark(get_vm());
        });
    }

    pub fn join_all(&self) {
        let mut threads = self.threads.lock();

        while threads.len() > 0 {
            self.cond_join.wait(&mut threads);
        }
    }

    pub fn each<F>(&self, mut f: F)
    where
        F: FnMut(&Arc<DoraThread>),
    {
        let threads = self.threads.lock();

        for thread in threads.iter() {
            f(thread)
        }
    }
}

pub struct DoraThread {
    pub id: AtomicUsize,
    pub handles: HandleMemory,
    pub tld: ThreadLocalData,
    pub state: AtomicUsize,
    pub saved_pc: AtomicUsize,
    pub saved_fp: AtomicUsize,
}

unsafe impl Sync for DoraThread {}
unsafe impl Send for DoraThread {}

impl DoraThread {
    pub fn new(vm: &VM) -> Arc<DoraThread> {
        DoraThread::with_id(vm.threads.next_id())
    }

    pub fn main() -> Arc<DoraThread> {
        DoraThread::with_id(0)
    }

    fn with_id(id: usize) -> Arc<DoraThread> {
        Arc::new(DoraThread {
            id: AtomicUsize::new(id),
            handles: HandleMemory::new(),
            tld: ThreadLocalData::new(),
            state: AtomicUsize::new(ThreadState::Unparked as usize),
            saved_pc: AtomicUsize::new(0),
            saved_fp: AtomicUsize::new(0),
        })
    }

    pub fn id(&self) -> usize {
        self.id.load(Ordering::Relaxed)
    }

    pub fn park(&self, vm: &VM) {
        let result = self.state.compare_exchange(
            ThreadState::Unparked.to_usize(),
            ThreadState::Parked.to_usize(),
            Ordering::SeqCst,
            Ordering::SeqCst,
        );

        if let Err(value) = result {
            let state: ThreadState = value.into();
            assert!(state.is_gc_requested());
            safepoint::enter_safepoint_from_native_park(vm);
        }
    }

    pub fn unpark(&self, vm: &VM) {
        let result = self.state.compare_exchange(
            ThreadState::Parked.to_usize(),
            ThreadState::Unparked.to_usize(),
            Ordering::SeqCst,
            Ordering::SeqCst,
        );

        if let Err(value) = result {
            let state: ThreadState = value.into();
            assert!(state.is_gc_requested());
            safepoint::enter_safepoint_from_native_unpark(vm);
        }
    }

    pub fn block(&self) -> bool {
        let mut old_state: ThreadState = self.state.load(Ordering::Relaxed).into();

        loop {
            let new_state = match old_state {
                ThreadState::Unparked => ThreadState::GcRequestedInUnparked,
                ThreadState::Parked => ThreadState::GcRequestedInParked,
                ThreadState::GcRequestedInUnparked | ThreadState::GcRequestedInParked => {
                    unreachable!()
                }
            };

            let result = self.state.compare_exchange_weak(
                old_state.to_usize(),
                new_state.to_usize(),
                Ordering::SeqCst,
                Ordering::Relaxed,
            );

            match result {
                Ok(_) => return old_state.is_unparked(),
                Err(x) => old_state = x.into(),
            }
        }
    }

    pub fn unblock(&self) {
        let old_state: ThreadState = self.state.load(Ordering::Relaxed).into();

        let new_state = match old_state {
            ThreadState::GcRequestedInUnparked => ThreadState::Unparked,
            ThreadState::GcRequestedInParked => ThreadState::Parked,
            ThreadState::Unparked => unreachable!(),
            ThreadState::Parked => unreachable!(),
        };

        let result = self.state.compare_exchange_weak(
            old_state.to_usize(),
            new_state.to_usize(),
            Ordering::SeqCst,
            Ordering::Relaxed,
        );

        assert!(result.is_ok());
    }

    pub fn dtn(&self) -> *const DoraToNativeInfo {
        self.tld.dtn.load(Ordering::Relaxed) as *const _
    }

    pub fn set_dtn(&self, ptr: *const DoraToNativeInfo) {
        self.tld.dtn.store(ptr as usize, Ordering::Relaxed);
    }

    pub fn use_dtn<F, R>(&self, dtn: &mut DoraToNativeInfo, fct: F) -> R
    where
        F: FnOnce() -> R,
    {
        dtn.last = self.dtn();

        self.set_dtn(dtn as *const _);

        let ret = fct();

        self.set_dtn(dtn.last);

        ret
    }

    pub fn push_dtn(&self, dtn: &mut DoraToNativeInfo) {
        dtn.last = self.dtn();
        self.set_dtn(dtn as *const _);
    }

    pub fn pop_dtn(&self) {
        let current_dtn = self.dtn();
        assert!(!current_dtn.is_null());
        let dtn = unsafe { &*current_dtn };
        self.set_dtn(dtn.last);
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ThreadState {
    Unparked = 0,
    Parked = 1,
    GcRequestedInUnparked = 2,
    GcRequestedInParked = 3,
}

impl From<usize> for ThreadState {
    fn from(value: usize) -> ThreadState {
        match value {
            0 => ThreadState::Unparked,
            1 => ThreadState::Parked,
            2 => ThreadState::GcRequestedInUnparked,
            3 => ThreadState::GcRequestedInParked,
            _ => unreachable!(),
        }
    }
}

impl ThreadState {
    fn is_unparked(&self) -> bool {
        match *self {
            ThreadState::Unparked => true,
            _ => false,
        }
    }

    fn is_parked(&self) -> bool {
        match *self {
            ThreadState::Parked => true,
            _ => false,
        }
    }

    fn is_gc_requested(&self) -> bool {
        match *self {
            ThreadState::GcRequestedInUnparked | ThreadState::GcRequestedInParked => true,
            _ => false,
        }
    }

    fn to_usize(&self) -> usize {
        *self as usize
    }
}

impl Default for ThreadState {
    fn default() -> ThreadState {
        ThreadState::Unparked
    }
}

pub struct ThreadLocalData {
    tlab_top: AtomicUsize,
    tlab_end: AtomicUsize,
    concurrent_marking: AtomicBool,
    guard_stack_limit: AtomicUsize,
    real_stack_limit: AtomicUsize,
    dtn: AtomicUsize,
}

impl ThreadLocalData {
    pub fn new() -> ThreadLocalData {
        ThreadLocalData {
            tlab_top: AtomicUsize::new(0),
            tlab_end: AtomicUsize::new(0),
            concurrent_marking: AtomicBool::new(false),
            guard_stack_limit: AtomicUsize::new(0),
            real_stack_limit: AtomicUsize::new(0),
            dtn: AtomicUsize::new(0),
        }
    }

    pub fn tlab_initialize(&self, start: Address, end: Address) {
        assert!(start <= end);

        self.tlab_top.store(start.to_usize(), Ordering::Relaxed);
        self.tlab_end.store(end.to_usize(), Ordering::Relaxed);
    }

    pub fn tlab_rest(&self) -> usize {
        let tlab_top = self.tlab_top.load(Ordering::Relaxed);
        let tlab_end = self.tlab_end.load(Ordering::Relaxed);

        tlab_end - tlab_top
    }

    pub fn tlab_region(&self) -> Region {
        let tlab_top = self.tlab_top.load(Ordering::Relaxed);
        let tlab_end = self.tlab_end.load(Ordering::Relaxed);

        Region::new(tlab_top.into(), tlab_end.into())
    }

    pub fn set_stack_limit(&self, stack_limit: Address) {
        self.guard_stack_limit
            .store(stack_limit.to_usize(), Ordering::Relaxed);
        self.real_stack_limit
            .store(stack_limit.to_usize(), Ordering::Relaxed);
    }

    pub fn tlab_top_offset() -> i32 {
        offset_of!(ThreadLocalData, tlab_top) as i32
    }

    pub fn tlab_end_offset() -> i32 {
        offset_of!(ThreadLocalData, tlab_end) as i32
    }

    pub fn concurrent_marking_offset() -> i32 {
        offset_of!(ThreadLocalData, concurrent_marking) as i32
    }

    pub fn guard_stack_limit_offset() -> i32 {
        offset_of!(ThreadLocalData, guard_stack_limit) as i32
    }

    pub fn real_stack_limit(&self) -> Address {
        Address::from(self.real_stack_limit.load(Ordering::Relaxed))
    }

    pub fn real_stack_limit_offset() -> i32 {
        offset_of!(ThreadLocalData, real_stack_limit) as i32
    }

    pub fn dtn_offset() -> i32 {
        offset_of!(ThreadLocalData, dtn) as i32
    }

    pub fn arm_stack_guard(&self) {
        self.guard_stack_limit.store(!0, Ordering::Relaxed);
    }

    pub fn unarm_stack_guard(&self) {
        let limit = self.real_stack_limit.load(Ordering::Relaxed);
        self.guard_stack_limit.store(limit, Ordering::Relaxed);
    }
}

pub struct Barrier {
    active: Mutex<bool>,
    done: Condvar,
}

impl Barrier {
    pub fn new() -> Barrier {
        Barrier {
            active: Mutex::new(false),
            done: Condvar::new(),
        }
    }

    pub fn guard(&self) {
        let mut active = self.active.lock();
        assert_eq!(*active, false);
        *active = true;
    }

    pub fn resume(&self) {
        let mut active = self.active.lock();
        assert_eq!(*active, true);
        *active = false;
        self.done.notify_all();
    }

    pub fn wait(&self) {
        let mut active = self.active.lock();

        while *active {
            self.done.wait(&mut active);
        }
    }
}
