use parking_lot::{Condvar, Mutex};
use std::cell::RefCell;
use std::convert::From;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;

use crate::gc::{tlab, Address, Region, K};
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

    pub next_id: AtomicUsize,
    pub safepoint: Mutex<(usize, usize)>,

    pub barrier: Barrier,
}

impl Threads {
    pub fn new() -> Threads {
        Threads {
            threads: Mutex::new(Vec::new()),
            cond_join: Condvar::new(),
            next_id: AtomicUsize::new(1),
            safepoint: Mutex::new((0, 1)),
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

    pub fn safepoint_id(&self) -> usize {
        let safepoint = self.safepoint.lock();
        safepoint.0
    }

    pub fn safepoint_requested(&self) -> bool {
        let safepoint = self.safepoint.lock();
        safepoint.0 != 0
    }

    pub fn request_safepoint(&self) -> usize {
        let mut safepoint = self.safepoint.lock();
        assert_eq!(safepoint.0, 0);
        safepoint.0 = safepoint.1;
        safepoint.1 += 1;

        safepoint.0
    }

    pub fn clear_safepoint_request(&self) {
        let mut safepoint = self.safepoint.lock();
        assert_ne!(safepoint.0, 0);
        safepoint.0 = 0;
    }

    pub fn detach_current_thread(&self) {
        let vm = get_vm();

        // Other threads might still be running and perform a GC.
        // Fill the TLAB for them.
        tlab::make_iterable_current(vm);

        THREAD.with(|thread| {
            thread.borrow().park(vm);
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
    pub id: AtomicUsize,
    pub handles: HandleMemory,
    pub tld: ThreadLocalData,
    pub saved_pc: AtomicUsize,
    pub saved_fp: AtomicUsize,
    pub state: StateManager,
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
            saved_pc: AtomicUsize::new(0),
            saved_fp: AtomicUsize::new(0),
            state: StateManager::new(),
        })
    }

    pub fn id(&self) -> usize {
        self.id.load(Ordering::Relaxed)
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

    pub fn state(&self) -> ThreadState {
        self.state.state()
    }

    pub fn park(&self, vm: &VM) {
        self.state.park(vm);
    }

    pub fn unpark(&self, vm: &VM) {
        if vm.threads.safepoint_id() != 0 {
            safepoint::block(vm, self);
        }

        self.state.unpark(vm);
    }

    pub fn block(&self, safepoint_id: usize) {
        self.state.block(safepoint_id);
    }

    pub fn unblock(&self) {
        self.state.unblock();
    }

    pub fn in_safepoint(&self, safepoint_id: usize) -> bool {
        self.state.in_safepoint(safepoint_id)
    }
}

pub struct StateManager {
    mtx: Mutex<(ThreadState, usize)>,
}

impl StateManager {
    fn new() -> StateManager {
        StateManager {
            mtx: Mutex::new((ThreadState::Running, 0)),
        }
    }

    fn state(&self) -> ThreadState {
        let mtx = self.mtx.lock();
        mtx.0
    }

    fn park(&self, _vm: &VM) {
        let mut mtx = self.mtx.lock();
        assert!(mtx.0.is_running());
        mtx.0 = ThreadState::Parked;
    }

    fn unpark(&self, _vm: &VM) {
        let mut mtx = self.mtx.lock();
        assert!(mtx.0.is_parked());
        mtx.0 = ThreadState::Running;
    }

    fn block(&self, safepoint_id: usize) {
        let mut mtx = self.mtx.lock();
        assert!(mtx.0.is_running());
        mtx.0 = ThreadState::Blocked;
        mtx.1 = safepoint_id;
    }

    fn unblock(&self) {
        let mut mtx = self.mtx.lock();
        assert!(mtx.0.is_blocked());
        mtx.0 = ThreadState::Running;
        mtx.1 = 0;
    }

    fn in_safepoint(&self, safepoint_id: usize) -> bool {
        assert_ne!(safepoint_id, 0);
        let mtx = self.mtx.lock();

        match mtx.0 {
            ThreadState::Running => false,
            ThreadState::Blocked => mtx.1 == safepoint_id,
            ThreadState::Parked => true,
            ThreadState::RequestedSafepoint | ThreadState::ParkedSafepoint => unreachable!(),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ThreadState {
    Running = 0,
    Parked = 1,
    Blocked = 2,
    RequestedSafepoint = 3,
    ParkedSafepoint = 4,
}

impl From<usize> for ThreadState {
    fn from(value: usize) -> ThreadState {
        match value {
            0 => ThreadState::Running,
            1 => ThreadState::Parked,
            2 => ThreadState::Blocked,
            3 => ThreadState::RequestedSafepoint,
            4 => ThreadState::ParkedSafepoint,
            _ => unreachable!(),
        }
    }
}

impl ThreadState {
    pub fn is_running(&self) -> bool {
        match *self {
            ThreadState::Running => true,
            _ => false,
        }
    }

    pub fn is_parked(&self) -> bool {
        match *self {
            ThreadState::Parked => true,
            _ => false,
        }
    }

    pub fn is_blocked(&self) -> bool {
        match *self {
            ThreadState::Blocked => true,
            _ => false,
        }
    }

    pub fn to_usize(&self) -> usize {
        *self as usize
    }
}

impl Default for ThreadState {
    fn default() -> ThreadState {
        ThreadState::Running
    }
}

pub struct ThreadLocalData {
    tlab_top: AtomicUsize,
    tlab_end: AtomicUsize,
    concurrent_marking: AtomicBool,
    stack_limit: AtomicUsize,
    safepoint_requested: AtomicBool,
    dtn: AtomicUsize,
}

impl ThreadLocalData {
    pub fn new() -> ThreadLocalData {
        ThreadLocalData {
            tlab_top: AtomicUsize::new(0),
            tlab_end: AtomicUsize::new(0),
            concurrent_marking: AtomicBool::new(false),
            stack_limit: AtomicUsize::new(0),
            safepoint_requested: AtomicBool::new(false),
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
        self.stack_limit
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

    pub fn safepoint_requested_offset() -> i32 {
        offset_of!(ThreadLocalData, safepoint_requested) as i32
    }

    pub fn stack_limit(&self) -> Address {
        Address::from(self.stack_limit.load(Ordering::Relaxed))
    }

    pub fn stack_limit_offset() -> i32 {
        offset_of!(ThreadLocalData, stack_limit) as i32
    }

    pub fn dtn_offset() -> i32 {
        offset_of!(ThreadLocalData, dtn) as i32
    }

    pub fn set_safepoint_requested(&self) {
        self.safepoint_requested.store(true, Ordering::Relaxed);
    }

    pub fn clear_safepoint_requested(&self) {
        self.safepoint_requested.store(false, Ordering::Relaxed);
    }
}

pub struct Barrier {
    active: Mutex<usize>,
    done: Condvar,
}

impl Barrier {
    pub fn new() -> Barrier {
        Barrier {
            active: Mutex::new(0),
            done: Condvar::new(),
        }
    }

    pub fn guard(&self, safepoint_id: usize) {
        let mut active = self.active.lock();
        assert_eq!(*active, 0);
        assert_ne!(safepoint_id, 0);
        *active = safepoint_id;
    }

    pub fn resume(&self, safepoint_id: usize) {
        let mut active = self.active.lock();
        assert_eq!(*active, safepoint_id);
        assert_ne!(safepoint_id, 0);
        *active = 0;
        self.done.notify_all();
    }

    pub fn wait(&self, safepoint_id: usize) {
        let mut active = self.active.lock();
        assert_ne!(safepoint_id, 0);

        while *active == safepoint_id {
            self.done.wait(&mut active);
        }
    }
}
