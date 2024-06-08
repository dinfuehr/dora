use parking_lot::Mutex;

use std::cmp::{Ord, Ordering, PartialOrd};
use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};
use std::sync::Arc;

use crate::gc::allocator::GenerationAllocator;
use crate::gc::code::CodeSpace;
use crate::gc::copy::CopyCollector;
use crate::gc::metaspace::MetaSpace;
use crate::gc::space::{default_readonly_space_config, Space};
use crate::gc::sweep::SweepCollector;
use crate::gc::swiper::{align_page_up, is_page_aligned, Swiper};
use crate::gc::tlab::MAX_TLAB_OBJECT_SIZE;
pub use crate::gc::worklist::{Worklist, WorklistSegment};
use crate::gc::zero::ZeroCollector;
use crate::mem;
use crate::object::{Header, Obj};
use crate::safepoint;
use crate::stdlib;
use crate::threads::DoraThread;
use crate::vm::{CollectorName, Flags, Trap, VM};

pub use crate::gc::root::{iterate_strong_roots, iterate_weak_roots, Slot};

use self::swiper::PAGE_SIZE;

pub mod allocator;
pub mod bump;
pub mod code;
pub mod copy;
pub mod freelist;
pub mod marking;
mod metaspace;
pub mod pmarking;
pub mod root;
pub mod space;
pub mod sweep;
pub mod swiper;
pub mod tlab;
pub mod worklist;
pub mod zero;

pub const K: usize = 1024;
pub const M: usize = K * K;

const CHUNK_SIZE: usize = 8 * K;
pub const DEFAULT_CODE_SPACE_LIMIT: usize = 16 * M;
pub const DEFAULT_READONLY_SPACE_LIMIT: usize = 2 * M;

pub struct Gc {
    collector: Box<dyn Collector + Sync>,
    supports_tlab: bool,

    code_space: CodeSpace,
    meta_space: MetaSpace,
    epoch: AtomicUsize,

    finalizers: Mutex<Vec<(Address, Arc<DoraThread>)>>,
}

impl Gc {
    pub fn new(args: &Flags) -> Gc {
        let collector_name = args.gc.unwrap_or(CollectorName::Swiper);

        let collector: Box<dyn Collector + Sync> = match collector_name {
            CollectorName::Zero => Box::new(ZeroCollector::new(args)),
            CollectorName::Copy => Box::new(CopyCollector::new(args)),
            CollectorName::Sweep => Box::new(SweepCollector::new(args)),
            CollectorName::Swiper => Box::new(Swiper::new(args)),
        };

        let supports_tlab = !args.disable_tlab && collector.supports_tlab();

        let code_size = args.code_size();

        Gc {
            collector,
            supports_tlab,

            code_space: CodeSpace::new(code_size),
            meta_space: MetaSpace::new(),
            epoch: AtomicUsize::new(0),

            finalizers: Mutex::new(Vec::new()),
        }
    }

    pub fn setup(&self, vm: &VM) {
        self.collector.setup(vm);
    }

    pub fn add_finalizer(&self, object: Address, thread: Arc<DoraThread>) {
        let mut finalizers = self.finalizers.lock();
        finalizers.push((object, thread));
    }

    pub fn needs_write_barrier(&self) -> bool {
        self.collector.needs_write_barrier()
    }

    pub fn alloc_code(&self, size: usize) -> Address {
        self.code_space.alloc(size)
    }

    pub fn alloc_meta(&self, align: usize, size: usize) -> Address {
        self.meta_space.alloc(align, size)
    }

    pub fn alloc_readonly(&self, vm: &VM, size: usize) -> Address {
        self.collector.alloc_readonly(vm, size)
    }

    pub fn alloc(&self, vm: &VM, size: usize) -> Address {
        if vm.flags.gc_stress_minor {
            self.force_collect(vm, GcReason::StressMinor);
        }

        if vm.flags.gc_stress {
            self.force_collect(vm, GcReason::Stress);
        }

        let result = self.allocate_raw(vm, size);

        if result.is_non_null() {
            return result;
        }

        for retry in 0..3 {
            let reason = if retry > 0 {
                GcReason::LastResort
            } else {
                GcReason::AllocationFailure
            };
            self.collect_garbage(vm, reason, size);

            let result = self.allocate_raw(vm, size);

            if result.is_non_null() {
                return result;
            }
        }

        stdlib::trap(Trap::OOM as u8 as u32);
        unreachable!()
    }

    fn alloc_in_lab(&self, vm: &VM, size: usize) -> Address {
        // try to allocate in current tlab
        if let Some(addr) = tlab::allocate(size) {
            return addr;
        }

        // if there is not enough space, make heap iterable by filling tlab with unused objects
        tlab::make_iterable_current(vm);

        // allocate new tlab
        if let Some(tlab) = self.collector.alloc_tlab_area(vm, tlab::calculate_size()) {
            let object_start = tlab.start;
            let tlab = Region::new(tlab.start.offset(size), tlab.end);

            // initialize TLAB to new boundaries
            tlab::initialize(tlab);

            // object is allocated before TLAB
            object_start
        } else {
            // fail with OOM if TLAB can't be allocated
            Address::null()
        }
    }

    pub fn epoch(&self) -> usize {
        self.epoch.load(AtomicOrdering::Relaxed)
    }

    pub fn force_collect(&self, vm: &VM, reason: GcReason) {
        assert!(reason.is_forced() || reason.is_stress());
        self.collect_garbage(vm, reason, 0);
    }

    pub fn shutdown(&self, vm: &VM) {
        self.collector.shutdown(vm);
    }

    pub fn dump_summary(&self, runtime: f32) {
        self.collector.dump_summary(runtime);
    }

    pub fn drop_all_native_code_objects(&mut self, meta_space_start: Address) {
        self.code_space
            .drop_all_native_code_objects(meta_space_start);
    }

    pub fn initial_metadata_value(&self, size: usize, is_readonly: bool) -> (bool, bool) {
        self.collector.initial_metadata_value(size, is_readonly)
    }

    pub fn meta_space_start(&self) -> Address {
        self.meta_space.start()
    }

    pub fn meta_space_size(&self) -> usize {
        self.meta_space.size()
    }

    pub fn allocate_raw(&self, vm: &VM, size: usize) -> Address {
        if size < MAX_TLAB_OBJECT_SIZE && self.supports_tlab {
            self.alloc_in_lab(vm, size)
        } else {
            self.collector.alloc_object(vm, size)
        }
    }

    fn collect_garbage(&self, vm: &VM, reason: GcReason, size: usize) {
        safepoint::stop_the_world(vm, |threads| {
            self.epoch.fetch_add(1, AtomicOrdering::Relaxed);
            tlab::make_iterable_all(vm, threads);

            self.collector.collect_garbage(vm, threads, reason, size);
        });
    }
}

trait Collector {
    // Allocate object of given size.
    fn alloc_tlab_area(&self, vm: &VM, size: usize) -> Option<Region>;
    fn alloc_object(&self, vm: &VM, size: usize) -> Address;
    fn alloc_readonly(&self, vm: &VM, size: usize) -> Address;

    // Force garbage collection.
    fn force_collect(&self, vm: &VM, reason: GcReason);

    fn collect_garbage(&self, vm: &VM, threads: &[Arc<DoraThread>], reason: GcReason, size: usize);

    // Decides whether to emit write barriers needed for
    // generational GC.
    fn needs_write_barrier(&self) -> bool {
        false
    }

    fn initial_metadata_value(&self, _size: usize, _is_readonly: bool) -> (bool, bool) {
        (false, false)
    }

    // Gives true when collector supports tlab allocation.
    fn supports_tlab(&self) -> bool;

    // Prints GC summary: minor/full collections, etc.
    fn dump_summary(&self, _runtime: f32);

    // Verify reference
    fn verify_ref(&self, _vm: &VM, _addr: Address) {
        // do nothing
    }

    // Invoked once right after VM was created.
    fn setup(&self, _vm: &VM) {
        // Do nothing.
    }

    fn shutdown(&self, _vm: &VM) {
        // Do nothing.
    }

    fn to_swiper(&self) -> &Swiper {
        unimplemented!()
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct Address(usize);

impl Address {
    #[inline(always)]
    pub fn from(val: usize) -> Address {
        Address(val)
    }

    #[inline(always)]
    pub fn region_start(self, size: usize) -> Region {
        Region::new(self, self.offset(size))
    }

    #[inline(always)]
    pub fn offset_from(self, base: Address) -> usize {
        debug_assert!(self >= base);

        self.to_usize() - base.to_usize()
    }

    #[inline(always)]
    pub fn offset(self, offset: usize) -> Address {
        Address(self.0 + offset)
    }

    #[inline(always)]
    pub fn ioffset(self, offset: isize) -> Address {
        Address((self.0 as isize + offset) as usize)
    }

    #[inline(always)]
    pub fn sub(self, offset: usize) -> Address {
        Address(self.0 - offset)
    }

    #[inline(always)]
    pub fn add_ptr(self, words: usize) -> Address {
        Address(self.0 + words * mem::ptr_width_usize())
    }

    #[inline(always)]
    pub fn sub_ptr(self, words: usize) -> Address {
        Address(self.0 - words * mem::ptr_width_usize())
    }

    #[inline(always)]
    pub fn to_obj(self) -> &'static Obj {
        unsafe { &*self.to_mut_ptr::<Obj>() }
    }

    #[inline(always)]
    pub fn to_usize(self) -> usize {
        self.0
    }

    #[inline(always)]
    pub fn from_ptr<T>(ptr: *const T) -> Address {
        Address(ptr as usize)
    }

    #[inline(always)]
    pub fn to_ptr<T>(&self) -> *const T {
        self.0 as *const T
    }

    #[inline(always)]
    pub fn to_mut_ptr<T>(&self) -> *mut T {
        self.0 as *const T as *mut T
    }

    #[inline(always)]
    pub fn null() -> Address {
        Address(0)
    }

    #[inline(always)]
    pub fn is_null(self) -> bool {
        self.0 == 0
    }

    #[inline(always)]
    pub fn is_non_null(self) -> bool {
        self.0 != 0
    }

    #[inline(always)]
    pub fn is_page_aligned(self) -> bool {
        is_page_aligned(self.to_usize())
    }

    #[inline(always)]
    pub fn align_page_up(self) -> Address {
        align_page_up(self.to_usize()).into()
    }

    #[inline(always)]
    pub fn is_os_page_aligned(self) -> bool {
        mem::is_os_page_aligned(self.to_usize())
    }

    #[inline(always)]
    pub fn is_power_of_2_aligned(self, aligned_bits: usize) -> bool {
        mem::is_power_of_2_aligned(self.to_usize(), aligned_bits)
    }
}

impl fmt::Display for Address {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:x}", self.to_usize())
    }
}

impl fmt::Debug for Address {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:x}", self.to_usize())
    }
}

impl PartialOrd for Address {
    fn partial_cmp(&self, other: &Address) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Address {
    fn cmp(&self, other: &Address) -> Ordering {
        self.to_usize().cmp(&other.to_usize())
    }
}

impl From<usize> for Address {
    fn from(val: usize) -> Address {
        Address(val)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Region {
    pub start: Address,
    pub end: Address,
}

impl Region {
    pub fn new(start: Address, end: Address) -> Region {
        debug_assert!(start <= end);

        Region { start, end }
    }

    #[inline(always)]
    pub fn start(&self) -> Address {
        self.start
    }

    #[inline(always)]
    pub fn end(&self) -> Address {
        self.end
    }

    #[inline(always)]
    pub fn contains(&self, addr: Address) -> bool {
        self.start <= addr && addr < self.end
    }

    #[inline(always)]
    pub fn valid_top(&self, addr: Address) -> bool {
        self.start <= addr && addr <= self.end
    }

    #[inline(always)]
    pub fn size(&self) -> usize {
        self.end.to_usize() - self.start.to_usize()
    }

    #[inline(always)]
    pub fn empty(&self) -> bool {
        self.start == self.end
    }

    #[inline(always)]
    pub fn disjunct(&self, other: &Region) -> bool {
        self.end <= other.start || self.start >= other.end
    }

    #[inline(always)]
    pub fn overlaps(&self, other: &Region) -> bool {
        !self.disjunct(other)
    }

    #[inline(always)]
    pub fn fully_contains(&self, other: &Region) -> bool {
        self.contains(other.start) && self.valid_top(other.end)
    }
}

impl Default for Region {
    fn default() -> Region {
        Region {
            start: Address::null(),
            end: Address::null(),
        }
    }
}

impl fmt::Display for Region {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

struct FormattedSize {
    size: usize,
}

impl fmt::Display for FormattedSize {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ksize = (self.size as f64) / 1024f64;

        if ksize < 1f64 {
            return write!(f, "{}B", self.size);
        }

        let msize = ksize / 1024f64;

        if msize < 1f64 {
            return write!(f, "{:.1}K", ksize);
        }

        let gsize = msize / 1024f64;

        if gsize < 1f64 {
            write!(f, "{:.1}M", msize)
        } else {
            write!(f, "{:.1}G", gsize)
        }
    }
}

fn formatted_size(size: usize) -> FormattedSize {
    FormattedSize { size }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum GcReason {
    AllocationFailure,
    ForceCollect,
    ForceMinorCollect,
    Stress,
    StressMinor,
    LastResort,
}

impl GcReason {
    fn is_forced(&self) -> bool {
        match self {
            GcReason::ForceCollect | GcReason::ForceMinorCollect => true,

            GcReason::AllocationFailure
            | GcReason::LastResort
            | GcReason::Stress
            | GcReason::StressMinor => false,
        }
    }

    fn is_stress(&self) -> bool {
        match self {
            GcReason::Stress | GcReason::StressMinor => true,
            GcReason::ForceCollect
            | GcReason::ForceMinorCollect
            | GcReason::AllocationFailure
            | GcReason::LastResort => false,
        }
    }

    fn message(&self) -> &'static str {
        match self {
            GcReason::AllocationFailure => "alloc failure",
            GcReason::ForceCollect => "force collect",
            GcReason::ForceMinorCollect => "force minor collect",
            GcReason::Stress => "stress",
            GcReason::StressMinor => "stress minor",
            GcReason::LastResort => "last resort",
        }
    }
}

impl fmt::Display for GcReason {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message())
    }
}

pub fn fill_region(vm: &VM, start: Address, end: Address) {
    let size = end.offset_from(start);

    if size == mem::ptr_width_usize() {
        let vtable = vm.known.filler_word_class_address();

        unsafe {
            *start.to_mut_ptr::<usize>() =
                Header::compute_header_word(vtable, vm.meta_space_start(), false, false);
        }
    } else if size > mem::ptr_width_usize() {
        let vtable = vm.known.filler_array_class_address();
        let header_size = Header::size() as usize + mem::ptr_width_usize();
        let length: usize = end.offset_from(start.offset(header_size)) / mem::ptr_width_usize();

        unsafe {
            *start.to_mut_ptr::<usize>() =
                Header::compute_header_word(vtable, vm.meta_space_start(), false, false);
            *start.add_ptr(1).to_mut_ptr::<usize>() = length;
        }
    }
}

pub fn setup_free_space(vm: &VM, start: Address, end: Address, next: Address) {
    assert!(end.offset_from(start) > 2 * mem::ptr_width_usize());

    // fill with FreeArray
    let vtable = vm.known.free_space_class_address();

    // determine of header+length in bytes
    let header_size = Header::size() as usize + mem::ptr_width_usize();

    // calculate array length
    let length: usize = end.offset_from(start.offset(header_size)) / mem::ptr_width_usize();

    unsafe {
        *start.to_mut_ptr::<usize>() =
            Header::compute_header_word(vtable, vm.meta_space_start(), false, false);
        *start.add_ptr(1).to_mut_ptr::<usize>() = length;
        *start.add_ptr(2).to_mut_ptr::<usize>() = next.to_usize();
    }
}

struct CollectionStats {
    collections: usize,
    total_pause: f32,
    pauses: Vec<f32>,
}

impl CollectionStats {
    fn new() -> CollectionStats {
        CollectionStats {
            collections: 0,
            total_pause: 0f32,
            pauses: Vec::new(),
        }
    }

    fn add(&mut self, pause: f32) {
        self.collections += 1;
        self.total_pause += pause;
        self.pauses.push(pause);
    }

    fn pause(&self) -> f32 {
        self.total_pause
    }

    fn pauses(&self) -> AllNumbers {
        AllNumbers(self.pauses.clone())
    }

    fn mutator(&self, runtime: f32) -> f32 {
        runtime - self.total_pause
    }

    fn collections(&self) -> usize {
        self.collections
    }

    fn percentage(&self, runtime: f32) -> (f32, f32) {
        let gc_percentage = ((self.total_pause / runtime) * 100.0).round();
        let mutator_percentage = 100.0 - gc_percentage;

        (mutator_percentage, gc_percentage)
    }
}

pub struct AllNumbers(Vec<f32>);

impl fmt::Display for AllNumbers {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        let mut first = true;
        for num in &self.0 {
            if !first {
                write!(f, ",")?;
            }
            write!(f, "{:.1}", num)?;
            first = false;
        }
        write!(f, "]")
    }
}
