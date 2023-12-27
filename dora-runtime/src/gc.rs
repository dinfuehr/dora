use parking_lot::Mutex;

use std::cmp::{Ord, Ordering, PartialOrd};
use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};
use std::sync::Arc;

use crate::gc::allocator::GenerationAllocator;
use crate::gc::code::CodeSpace;
use crate::gc::compact::MarkCompactCollector;
use crate::gc::copy::CopyCollector;
use crate::gc::space::{Space, SpaceConfig};
use crate::gc::sweep::SweepCollector;
use crate::gc::swiper::{Swiper, CARD_SIZE};
use crate::gc::tlab::TLAB_OBJECT_SIZE;
use crate::gc::zero::ZeroCollector;
use crate::mem;
use crate::object::{Header, Obj};
use crate::threads::DoraThread;
use crate::vm::VM;
use crate::vm::{CollectorName, Flags};
use crate::vtable::VTable;

pub use crate::gc::root::{iterate_strong_roots, iterate_weak_roots, Slot};

use self::swiper::PAGE_SIZE;

pub mod allocator;
pub mod bump;
pub mod code;
pub mod compact;
pub mod copy;
pub mod freelist;
pub mod marking;
pub mod pmarking;
pub mod root;
pub mod space;
pub mod sweep;
pub mod swiper;
pub mod tlab;
pub mod zero;

pub const K: usize = 1024;
pub const M: usize = K * K;

const CHUNK_SIZE: usize = 8 * K;
pub const DEFAULT_CODE_SPACE_LIMIT: usize = 2 * M;
pub const DEFAULT_READONLY_SPACE_LIMIT: usize = PAGE_SIZE;

pub struct Gc {
    collector: Box<dyn Collector + Sync>,
    supports_tlab: bool,

    code_space: CodeSpace,
    readonly_space: Space,
    epoch: AtomicUsize,

    finalizers: Mutex<Vec<(Address, Arc<DoraThread>)>>,
}

impl Gc {
    pub fn new(args: &Flags) -> Gc {
        let readonly_config = SpaceConfig {
            executable: false,
            chunk: PAGE_SIZE,
            limit: args.readonly_size(),
            object_alignment: mem::ptr_width_usize(),
        };

        let collector_name = args.gc.unwrap_or(CollectorName::Swiper);

        let collector: Box<dyn Collector + Sync> = match collector_name {
            CollectorName::Zero => Box::new(ZeroCollector::new(args)),
            CollectorName::Compact => Box::new(MarkCompactCollector::new(args)),
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
            readonly_space: Space::new(readonly_config, "perm"),
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

    pub fn card_table_offset(&self) -> usize {
        self.collector.card_table_offset()
    }

    pub fn alloc_code(&self, size: usize) -> Address {
        self.code_space.alloc(size)
    }

    pub fn alloc_readonly(&self, size: usize) -> Address {
        self.readonly_space.alloc(size)
    }

    pub fn alloc(&self, vm: &VM, size: usize) -> Address {
        if vm.flags.gc_stress_minor {
            self.minor_collect(vm, GcReason::StressMinor);
        }

        if vm.flags.gc_stress {
            self.collect(vm, GcReason::Stress);
        }

        if size < TLAB_OBJECT_SIZE && self.supports_tlab {
            self.alloc_tlab(vm, size)
        } else {
            self.collector.alloc(vm, size)
        }
    }

    fn alloc_tlab(&self, vm: &VM, size: usize) -> Address {
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

    pub fn collect(&self, vm: &VM, reason: GcReason) {
        self.epoch.fetch_add(1, AtomicOrdering::Relaxed);
        self.collector.collect(vm, reason);
    }

    pub fn minor_collect(&self, vm: &VM, reason: GcReason) {
        self.collector.minor_collect(vm, reason);
    }

    pub fn dump_summary(&self, runtime: f32) {
        self.collector.dump_summary(runtime);
    }

    pub fn verify_ref(&self, vm: &VM, reference: Address) {
        if reference.is_null() {
            return;
        }

        self.collector.verify_ref(vm, reference);
    }

    pub fn drop_all_native_code_objects(&mut self) {
        self.code_space.drop_all_native_code_objects();
    }

    pub fn initial_metadata_value(&self) -> usize {
        self.collector.initial_metadata_value()
    }
}

trait Collector {
    // allocate object of given size
    fn alloc_tlab_area(&self, vm: &VM, size: usize) -> Option<Region>;
    fn alloc(&self, vm: &VM, size: usize) -> Address;

    // collect garbage
    fn collect(&self, vm: &VM, reason: GcReason);

    // collect young generation if supported, otherwise
    // collects whole heap
    fn minor_collect(&self, vm: &VM, reason: GcReason);

    // decides whether to emit write barriers needed for
    // generational GC to write into card table
    fn needs_write_barrier(&self) -> bool {
        false
    }

    fn initial_metadata_value(&self) -> usize {
        0
    }

    // gives true when collector supports tlab allocation.
    fn supports_tlab(&self) -> bool;

    // only need if write barriers needed
    fn card_table_offset(&self) -> usize {
        0
    }

    // prints GC summary: minor/full collections, etc.
    fn dump_summary(&self, _runtime: f32);

    // verify reference
    fn verify_ref(&self, _vm: &VM, _addr: Address) {
        // do nothing
    }

    // Invoked once right after VM was created.
    fn setup(&self, _vm: &VM) {
        // Do nothing.
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
    pub fn is_card_aligned(self) -> bool {
        (self.to_usize() & (CARD_SIZE - 1)) == 0
    }

    #[inline(always)]
    pub fn align_card(self) -> Address {
        mem::align_usize_up(self.to_usize(), CARD_SIZE).into()
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

/// round the given value up to the nearest multiple of a generation
pub fn align_page_up(value: usize) -> usize {
    (value + PAGE_SIZE - 1) & !(PAGE_SIZE - 1)
}

pub fn align_page_down(value: usize) -> usize {
    value & !(PAGE_SIZE - 1)
}

/// returns true if given size is gen aligned
pub fn is_page_aligned(size: usize) -> bool {
    (size & (PAGE_SIZE - 1)) == 0
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum GcReason {
    PromotionFailure,
    AllocationFailure,
    ForceCollect,
    ForceMinorCollect,
    Stress,
    StressMinor,
}

impl GcReason {
    fn message(&self) -> &'static str {
        match self {
            GcReason::PromotionFailure => "promo failure",
            GcReason::AllocationFailure => "alloc failure",
            GcReason::ForceCollect => "force collect",
            GcReason::ForceMinorCollect => "force minor collect",
            GcReason::Stress => "stress",
            GcReason::StressMinor => "stress minor",
        }
    }
}

impl fmt::Display for GcReason {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message())
    }
}

pub fn fill_region(vm: &VM, start: Address, end: Address) {
    fill_region_with(vm, start, end, true);
}

pub fn fill_region_with(vm: &VM, start: Address, end: Address, clear: bool) {
    if start == end {
        // nothing to do
    } else if end.offset_from(start) == mem::ptr_width_usize() {
        unsafe {
            *start.to_mut_ptr::<usize>() = 0;
        }
    } else if end.offset_from(start) == Header::size() as usize {
        // fill with object
        let vtable = vm.known.free_object_class_address();

        unsafe {
            *start.to_mut_ptr::<usize>() = vtable.to_usize();
            *start.add_ptr(1).to_mut_ptr::<usize>() = 0;
        }
    } else {
        // fill with free array
        let vtable = vm.known.free_array_class_address();

        // determine of header+length in bytes
        let header_size = Header::size() as usize + mem::ptr_width_usize();

        // calculate array length
        let length: usize = end.offset_from(start.offset(header_size)) / mem::ptr_width_usize();

        unsafe {
            *start.to_mut_ptr::<usize>() = vtable.to_usize();
            *start.offset(mem::ptr_width_usize()).to_mut_ptr::<usize>() = 0;
            *start.offset(Header::size() as usize).to_mut_ptr::<usize>() = length;

            if clear && cfg!(debug_assertions) {
                for idx in 0..length {
                    *start
                        .offset(Header::size() as usize)
                        .add_ptr(idx + 1)
                        .to_mut_ptr::<usize>() = 0xBADDCAFE;
                }
            }
        }
    }
}

pub fn fill_region_with_free(vm: &VM, start: Address, end: Address, next: Address) {
    if start == end || end.offset_from(start) == mem::ptr_width_usize() {
        panic!("region is too small for FreeObject.");
    } else if end.offset_from(start) == Header::size() as usize {
        // fill with FreeObject
        let cls_id = vm.known.free_object_class_instance();
        let cls = vm.class_instances.idx(cls_id);
        let vtable = cls.vtable.read();
        let vtable: &VTable = vtable.as_ref().unwrap();

        unsafe {
            *start.to_mut_ptr::<usize>() = Address::from_ptr(vtable).to_usize();
            *start.add_ptr(1).to_mut_ptr::<usize>() = next.to_usize();
        }
    } else {
        // fill with FreeArray
        let cls_id = vm.known.free_array_class_instance();
        let cls = vm.class_instances.idx(cls_id);
        let vtable = cls.vtable.read();
        let vtable: &VTable = vtable.as_ref().unwrap();

        // determine of header+length in bytes
        let header_size = Header::size() as usize + mem::ptr_width_usize();

        // calculate array length
        let length: usize = end.offset_from(start.offset(header_size)) / mem::ptr_width_usize();

        unsafe {
            *start.to_mut_ptr::<usize>() = Address::from_ptr(vtable).to_usize();
            *start.add_ptr(1).to_mut_ptr::<usize>() = next.to_usize();
            *start.add_ptr(2).to_mut_ptr::<usize>() = length;
        }
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
