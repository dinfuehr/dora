use parking_lot::{Condvar, Mutex};
use std::cell::RefCell;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;

use crate::exception::DoraToNativeInfo;
use crate::gc::{Address, Region, M};
use crate::handle::HandleMemory;

pub const STACK_SIZE: usize = 1 * M;

thread_local! {
    pub static THREAD: RefCell<Arc<DoraThread>> = RefCell::new(DoraThread::new());
}

pub struct Threads {
    pub threads: Mutex<Vec<Arc<DoraThread>>>,
    pub cond_join: Condvar,
}

impl Threads {
    pub fn new() -> Threads {
        Threads {
            threads: Mutex::new(Vec::new()),
            cond_join: Condvar::new(),
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

    pub fn detach_current_thread(&self) {
        THREAD.with(|thread| {
            let mut threads = self.threads.lock();
            threads.retain(|elem| !Arc::ptr_eq(elem, &*thread.borrow()));
            self.cond_join.notify_all();
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
    pub dtn: AtomicUsize,
    pub handles: HandleMemory,
    pub tld: ThreadLocalData,
    pub state: AtomicUsize,
    pub saved_pc: AtomicUsize,
    pub saved_fp: AtomicUsize,
}

unsafe impl Sync for DoraThread {}
unsafe impl Send for DoraThread {}

impl DoraThread {
    pub fn new() -> Arc<DoraThread> {
        Arc::new(DoraThread {
            dtn: AtomicUsize::new(0),
            handles: HandleMemory::new(),
            tld: ThreadLocalData::new(),
            state: AtomicUsize::new(ThreadState::Uninitialized as usize),
            saved_pc: AtomicUsize::new(0),
            saved_fp: AtomicUsize::new(0),
        })
    }

    pub fn dtn(&self) -> *const DoraToNativeInfo {
        self.dtn.load(Ordering::Relaxed) as *const _
    }

    pub fn set_dtn(&self, ptr: *const DoraToNativeInfo) {
        self.dtn.store(ptr as usize, Ordering::Relaxed);
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

    pub fn state(&self) -> ThreadState {
        let state = self.state.load(Ordering::Relaxed);

        match state {
            0 => ThreadState::Uninitialized,
            1 => ThreadState::Native,
            2 => ThreadState::Dora,
            3 => ThreadState::Blocked,
            _ => unreachable!(),
        }
    }

    pub fn set_state(&self, state: ThreadState) {
        self.state.store(state as usize, Ordering::Relaxed);
    }
}

pub enum ThreadState {
    Uninitialized = 0,
    Native = 1,
    Dora = 2,
    Blocked = 3,
}

impl Default for ThreadState {
    fn default() -> ThreadState {
        ThreadState::Uninitialized
    }
}

pub struct ThreadLocalData {
    tlab_top: AtomicUsize,
    tlab_end: AtomicUsize,
    concurrent_marking: AtomicBool,
    stack_limit: AtomicUsize,
    exception_object: AtomicUsize,
}

impl ThreadLocalData {
    pub fn new() -> ThreadLocalData {
        ThreadLocalData {
            tlab_top: AtomicUsize::new(0),
            tlab_end: AtomicUsize::new(0),
            concurrent_marking: AtomicBool::new(false),
            stack_limit: AtomicUsize::new(0),
            exception_object: AtomicUsize::new(0),
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
        self.stack_limit
            .store(stack_limit.to_usize(), Ordering::Relaxed);
    }

    pub fn set_exception_object(&self, obj: Address) {
        self.exception_object
            .store(obj.to_usize(), Ordering::Relaxed);
    }

    pub fn exception_object(&self) -> Address {
        Address::from(self.exception_object.load(Ordering::Relaxed))
    }

    pub fn tlab_top_offset() -> i32 {
        offset_of!(ThreadLocalData, tlab_top) as i32
    }

    pub fn tlab_end_offset() -> i32 {
        offset_of!(ThreadLocalData, tlab_end) as i32
    }

    pub fn exception_object_offset() -> i32 {
        offset_of!(ThreadLocalData, exception_object) as i32
    }

    pub fn concurrent_marking_offset() -> i32 {
        offset_of!(ThreadLocalData, concurrent_marking) as i32
    }

    pub fn stack_limit_offset() -> i32 {
        offset_of!(ThreadLocalData, stack_limit) as i32
    }
}
