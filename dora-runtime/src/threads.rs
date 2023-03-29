use parking_lot::{Condvar, Mutex};
use std::cell::RefCell;
use std::convert::From;
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;

use crate::gc::{tlab, Address, Region, K};
use crate::handle::HandleMemory;
use crate::object::{alloc, Header, Ref};
use crate::stack::DoraToNativeInfo;
use crate::vm::{get_vm, VM};

pub const STACK_SIZE: usize = 500 * K;

thread_local! {
    static THREAD: RefCell<*const DoraThread> = RefCell::new(ptr::null());
}

pub fn current_thread() -> &'static DoraThread {
    let thread = THREAD.with(|thread| *thread.borrow());
    debug_assert!(!thread.is_null());
    unsafe { &*thread }
}

pub fn init_current_thread(thread: Arc<DoraThread>) -> &'static DoraThread {
    let thread = Arc::into_raw(thread);

    THREAD.with(|thread_local| {
        *thread_local.borrow_mut() = thread;
    });

    unsafe { &*thread }
}

pub fn deinit_current_thread() {
    THREAD.with(|thread| {
        let mut threadptr = thread.borrow_mut();

        {
            let thread = unsafe { Arc::from_raw(*threadptr) };
            std::mem::drop(thread);
        }

        *threadptr = ptr::null();
    });
}

pub struct Threads {
    pub threads: Mutex<Vec<Arc<DoraThread>>>,
    pub cv_join: Condvar,

    pub next_thread_id: AtomicUsize,

    pub barrier: Barrier,
}

impl Threads {
    pub fn new() -> Threads {
        Threads {
            threads: Mutex::new(Vec::new()),
            cv_join: Condvar::new(),
            next_thread_id: AtomicUsize::new(1),
            barrier: Barrier::new(),
        }
    }

    pub fn add_thread(&self, thread: Arc<DoraThread>) {
        assert!(thread.is_parked());
        parked_scope(|| {
            let mut threads = self.threads.lock();
            threads.push(thread);
        });
    }

    pub fn add_main_thread(&self, thread: Arc<DoraThread>) {
        assert!(thread.is_running());
        let mut threads = self.threads.lock();
        assert!(threads.is_empty());
        threads.push(thread);
    }

    pub fn next_thread_id(&self) -> usize {
        self.next_thread_id.fetch_add(1, Ordering::Relaxed)
    }

    pub fn remove_current_thread(&self) {
        let vm = get_vm();

        // Other threads might still be running and perform a GC.
        // Fill the TLAB for them.
        tlab::make_iterable_current(vm);

        let thread = current_thread();

        thread.park(vm);

        let mut threads = self.threads.lock();
        threads.retain(|elem| Arc::as_ptr(elem) != thread as *const _);
        self.cv_join.notify_all();
    }

    pub fn join_all(&self) {
        let mut threads = self.threads.lock();

        while threads.len() > 0 {
            self.cv_join.wait(&mut threads);
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct DoraThreadPtr(Address);

impl DoraThreadPtr {
    pub fn null() -> DoraThreadPtr {
        DoraThreadPtr(Address::null())
    }

    pub fn new(thread: &DoraThread) -> DoraThreadPtr {
        DoraThreadPtr(Address::from_ptr(thread as *const _))
    }

    pub fn to_ref(self) -> &'static DoraThread {
        unsafe { &*self.0.to_ptr::<DoraThread>() }
    }

    pub fn is_null(self) -> bool {
        self.0.is_null()
    }
}

pub struct DoraThread {
    id: AtomicUsize,
    pub handles: HandleMemory,
    pub tld: ThreadLocalData,
    pub state: AtomicUsize,
    join_data: JoinData,
    blocking_data: BlockingData,
}

unsafe impl Sync for DoraThread {}
unsafe impl Send for DoraThread {}

impl DoraThread {
    pub fn new(vm: &VM, initial_state: ThreadState) -> Arc<DoraThread> {
        DoraThread::with_id(vm.threads.next_thread_id(), initial_state)
    }

    fn with_id(id: usize, initial_state: ThreadState) -> Arc<DoraThread> {
        Arc::new(DoraThread {
            id: AtomicUsize::new(id),
            handles: HandleMemory::new(),
            tld: ThreadLocalData::new(),
            state: AtomicUsize::new(initial_state as usize),
            join_data: JoinData::new(),
            blocking_data: BlockingData::new(),
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

    pub fn state_relaxed(&self) -> ThreadState {
        self.state.load(Ordering::Relaxed).into()
    }

    pub fn is_running(&self) -> bool {
        self.state_relaxed().is_running()
    }

    pub fn is_parked(&self) -> bool {
        self.state_relaxed().is_parked()
    }

    pub fn park(&self, vm: &VM) {
        if self
            .state
            .compare_exchange(
                ThreadState::Running as usize,
                ThreadState::Parked as usize,
                Ordering::SeqCst,
                Ordering::SeqCst,
            )
            .is_err()
        {
            self.park_slow(vm);
        }
    }

    fn park_slow(&self, vm: &VM) {
        assert!(self
            .state
            .compare_exchange(
                ThreadState::SafepointRequested as usize,
                ThreadState::ParkedSafepoint as usize,
                Ordering::SeqCst,
                Ordering::SeqCst,
            )
            .is_ok());
        vm.threads.barrier.notify_park();
    }

    pub fn unpark(&self, vm: &VM) {
        if self
            .state
            .compare_exchange(
                ThreadState::Parked as usize,
                ThreadState::Running as usize,
                Ordering::SeqCst,
                Ordering::SeqCst,
            )
            .is_err()
        {
            self.unpark_slow(vm);
        }
    }

    fn unpark_slow(&self, vm: &VM) {
        loop {
            match self.state.compare_exchange(
                ThreadState::Parked as usize,
                ThreadState::Running as usize,
                Ordering::SeqCst,
                Ordering::SeqCst,
            ) {
                Ok(_) => break,
                Err(state) => {
                    assert_eq!(state, ThreadState::ParkedSafepoint as usize);
                    vm.threads.barrier.wait_in_unpark();
                }
            }
        }
    }

    pub fn tld_address(&self) -> Address {
        Address::from_ptr(&self.tld)
    }

    pub fn block(&self) {
        parked_scope(|| {
            let mut data = self.blocking_data.blocking.lock();

            while data.0 {
                self.blocking_data.cv_blocking.wait(&mut data);
            }
        });
    }

    pub fn prepare_for_waitlist(&self) {
        let mut data = self.blocking_data.blocking.lock();
        let (blocking, next) = *data;
        assert!(!blocking && next.is_null());
        *data = (true, DoraThreadPtr::null());
    }

    pub fn set_waitlist_successor(&self, new_tail: DoraThreadPtr) {
        let mut data = self.blocking_data.blocking.lock();
        let (blocking, next) = *data;
        assert!(blocking && next.is_null());
        *data = (true, new_tail);
    }

    pub fn remove_from_waitlist(&self) -> DoraThreadPtr {
        let next = {
            let mut thread_data = self.blocking_data.blocking.lock();
            let (blocking, next) = *thread_data;
            assert!(blocking);
            *thread_data = (false, DoraThreadPtr::null());
            next
        };

        self.blocking_data.cv_blocking.notify_one();
        next
    }

    pub fn stop(&self) {
        let mut running = self.join_data.running.lock();
        *running = false;
        self.join_data.cv_stopped.notify_all();
    }

    pub fn join(&self) {
        parked_scope(|| {
            let mut running = self.join_data.running.lock();

            while *running {
                self.join_data.cv_stopped.wait(&mut running);
            }
        });
    }
}

pub fn parked_scope<F, R>(callback: F) -> R
where
    F: FnOnce() -> R,
{
    let vm = get_vm();
    let thread = current_thread();

    thread.park(vm);
    let result = callback();
    thread.unpark(vm);

    result
}

struct JoinData {
    running: Mutex<bool>,
    cv_stopped: Condvar,
}

impl JoinData {
    fn new() -> JoinData {
        JoinData {
            running: Mutex::new(true),
            cv_stopped: Condvar::new(),
        }
    }
}

struct BlockingData {
    blocking: Mutex<(bool, DoraThreadPtr)>,
    cv_blocking: Condvar,
}

impl BlockingData {
    fn new() -> BlockingData {
        BlockingData {
            blocking: Mutex::new((false, DoraThreadPtr::null())),
            cv_blocking: Condvar::new(),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ThreadState {
    Running = 0,
    Parked = 1,
    SafepointRequested = 2,
    ParkedSafepoint = 3,
    Safepoint = 4,
}

impl From<usize> for ThreadState {
    fn from(value: usize) -> ThreadState {
        match value {
            0 => ThreadState::Running,
            1 => ThreadState::Parked,
            2 => ThreadState::SafepointRequested,
            3 => ThreadState::ParkedSafepoint,
            4 => ThreadState::Safepoint,
            _ => unreachable!(),
        }
    }
}

impl ThreadState {
    pub fn is_running(&self) -> bool {
        match *self {
            ThreadState::Running | ThreadState::SafepointRequested => true,
            _ => false,
        }
    }

    pub fn is_parked(&self) -> bool {
        match *self {
            ThreadState::Parked | ThreadState::ParkedSafepoint => true,
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
    managed_thread_handle: AtomicUsize,
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
            managed_thread_handle: AtomicUsize::new(0),
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

    pub fn set_managed_thread_handle(&self, location: Address) {
        self.managed_thread_handle
            .store(location.to_usize(), Ordering::Relaxed);
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

    pub fn managed_thread_handle_offset() -> i32 {
        offset_of!(ThreadLocalData, managed_thread_handle) as i32
    }

    pub fn set_safepoint_requested(&self) {
        self.safepoint_requested.store(true, Ordering::Relaxed);
    }

    pub fn clear_safepoint_requested(&self) {
        self.safepoint_requested.store(false, Ordering::Relaxed);
    }
}

pub struct Barrier {
    data: Mutex<BarrierData>,
    cv_wakeup: Condvar,
    cv_notify: Condvar,
}

impl Barrier {
    pub fn new() -> Barrier {
        Barrier {
            data: Mutex::new(BarrierData::new()),
            cv_wakeup: Condvar::new(),
            cv_notify: Condvar::new(),
        }
    }

    pub fn arm(&self) {
        let mut data = self.data.lock();
        assert!(!data.is_armed());
        data.arm();
    }

    pub fn disarm(&self) {
        let mut data = self.data.lock();
        assert!(data.is_armed());
        data.disarm();
        self.cv_wakeup.notify_all();
    }

    pub fn notify_park(&self) {
        let mut data = self.data.lock();
        assert!(data.is_armed());
        data.stopped += 1;
        self.cv_notify.notify_one();
    }

    pub fn wait_in_safepoint(&self) {
        let mut data = self.data.lock();
        assert!(data.is_armed());
        data.stopped += 1;
        self.cv_notify.notify_one();

        while data.is_armed() {
            self.cv_wakeup.wait(&mut data);
        }
    }

    pub fn wait_in_unpark(&self) {
        let mut data = self.data.lock();

        while data.is_armed() {
            self.cv_wakeup.wait(&mut data);
        }
    }

    pub fn wait_until_threads_stopped(&self, threads: usize) {
        let mut data = self.data.lock();
        assert!(data.is_armed());
        while data.stopped < threads {
            self.cv_notify.wait(&mut data);
        }
        assert_eq!(data.stopped, threads);
    }
}

struct BarrierData {
    armed: bool,
    stopped: usize,
}

impl BarrierData {
    pub fn new() -> BarrierData {
        BarrierData {
            armed: false,
            stopped: 0,
        }
    }

    pub fn is_armed(&self) -> bool {
        self.armed
    }

    pub fn arm(&mut self) {
        self.stopped = 0;
        self.armed = true;
    }

    pub fn disarm(&mut self) {
        self.armed = false;
    }
}

#[repr(C)]
pub struct ManagedThread {
    header: Header,
    native_thread_ptr: u64,
    id: u64,
}

impl ManagedThread {
    pub fn alloc(vm: &VM) -> Ref<ManagedThread> {
        let cls_id = vm.thread_class_instance();
        let mut managed_thread: Ref<ManagedThread> = alloc(vm, cls_id).cast();
        managed_thread.native_thread_ptr = 0;
        managed_thread.id = 0;
        managed_thread
    }

    pub fn install_native_thread(&mut self, native_thread: &Arc<DoraThread>) {
        self.native_thread_ptr = Arc::as_ptr(native_thread) as usize as u64;
        self.id = native_thread.id() as u64;
    }

    pub fn native_thread(&self) -> &'static DoraThread {
        unsafe { &*(self.native_thread_ptr as *const _) }
    }

    #[allow(dead_code)]
    pub fn id(&self) -> u64 {
        self.id
    }
}
