use std::cmp::{Ord, Ordering, PartialOrd};
use std::fmt;

use ctxt::VM;
use driver::cmd::{Args, CollectorName};
use gc::compact::MarkCompactCollector;
use gc::copy::CopyCollector;
use gc::space::{Space, SpaceConfig};
use gc::swiper::{Swiper, CARD_SIZE};
use gc::tlab::TLAB_OBJECT_SIZE;
use gc::zero::ZeroCollector;
use mem;
use object::{Header, Obj};
use os;
use vtable::VTable;

pub mod arena;
pub mod bump;
pub mod compact;
pub mod copy;
pub mod root;
pub mod space;
pub mod swiper;
pub mod tlab;
pub mod zero;

pub const K: usize = 1024;
pub const M: usize = K * K;

const CHUNK_SIZE: usize = 8 * K;
pub const DEFAULT_CODE_SPACE_LIMIT: usize = 128 * K;
pub const DEFAULT_PERM_SPACE_LIMIT: usize = 64 * K;

// young/old gen are aligned to at least this size
const GEN_ALIGNMENT_BITS: usize = 17;
const GEN_SIZE: usize = 1 << GEN_ALIGNMENT_BITS;

pub struct Gc {
    collector: Box<Collector + Sync>,
    supports_tlab: bool,

    code_space: Space,
    perm_space: Space,
}

impl Gc {
    pub fn new(args: &Args) -> Gc {
        let code_config = SpaceConfig {
            executable: true,
            chunk: CHUNK_SIZE,
            limit: args.code_size(),
            align: 64,
        };

        let perm_config = SpaceConfig {
            executable: false,
            chunk: CHUNK_SIZE,
            limit: args.perm_size(),
            align: 8,
        };

        let collector_name = args.flag_gc.unwrap_or(CollectorName::Swiper);

        let collector: Box<Collector + Sync> = match collector_name {
            CollectorName::Zero => box ZeroCollector::new(args),
            CollectorName::Compact => box MarkCompactCollector::new(args),
            CollectorName::Copy => box CopyCollector::new(args),
            CollectorName::Swiper => box Swiper::new(args),
        };

        let supports_tlab = !args.flag_disable_tlab && collector.supports_tlab();

        Gc {
            collector: collector,
            supports_tlab: supports_tlab,

            code_space: Space::new(code_config, "code"),
            perm_space: Space::new(perm_config, "perm"),
        }
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

    pub fn alloc_perm(&self, size: usize) -> Address {
        self.perm_space.alloc(size)
    }

    pub fn alloc(&self, vm: &VM, size: usize, array_ref: bool) -> Address {
        if vm.args.flag_gc_stress_minor {
            self.minor_collect(vm, GcReason::StressMinor);
        }

        if vm.args.flag_gc_stress {
            self.collect(vm, GcReason::Stress);
        }

        if size < TLAB_OBJECT_SIZE && self.supports_tlab {
            self.alloc_tlab(vm, size, array_ref)
        } else {
            self.collector.alloc(vm, size, array_ref)
        }
    }

    fn alloc_tlab(&self, vm: &VM, size: usize, array_ref: bool) -> Address {
        // try to allocate in current tlab
        if let Some(addr) = tlab::allocate(size) {
            return addr;
        }

        // if there is not enough space, make heap iterable by filling tlab with unused objects
        tlab::make_iterable_current(vm);

        // allocate new tlab
        if let Some(tlab) = self
            .collector
            .alloc_tlab_area(vm, tlab::calculate_size(size))
        {
            let object_start = tlab.start;
            let tlab = Region::new(tlab.start.offset(size), tlab.end);

            // initialize TLAB to new boundaries
            tlab::initialize(tlab);

            // object is allocated before TLAB
            object_start
        } else {
            // allocate object
            self.collector.alloc(vm, size, array_ref)
        }
    }

    pub fn collect(&self, vm: &VM, reason: GcReason) {
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
}

trait Collector {
    // allocate object of given size
    fn alloc_tlab_area(&self, vm: &VM, size: usize) -> Option<Region>;
    fn alloc(&self, vm: &VM, size: usize, array_ref: bool) -> Address;

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
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
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
    pub fn to_mut_obj(self) -> &'static mut Obj {
        unsafe { &mut *self.to_mut_ptr::<Obj>() }
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
        mem::align_usize(self.to_usize(), CARD_SIZE).into()
    }

    #[inline(always)]
    pub fn align_gen(self) -> Address {
        align_gen(self.to_usize()).into()
    }

    #[inline(always)]
    pub fn is_gen_aligned(self) -> bool {
        gen_aligned(self.to_usize())
    }

    #[inline(always)]
    pub fn align_page(self) -> Address {
        mem::page_align(self.to_usize()).into()
    }

    #[inline(always)]
    pub fn align_page_down(self) -> Address {
        Address(self.0 & !(os::page_size() as usize - 1))
    }

    #[inline(always)]
    pub fn is_page_aligned(self) -> bool {
        mem::is_page_aligned(self.to_usize())
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

#[derive(Copy, Clone)]
pub struct Region {
    pub start: Address,
    pub end: Address,
}

impl Region {
    pub fn new(start: Address, end: Address) -> Region {
        debug_assert!(start <= end);

        Region {
            start: start,
            end: end,
        }
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
pub fn align_gen(value: usize) -> usize {
    let align = GEN_ALIGNMENT_BITS;
    // we know that page size is power of 2, hence
    // we can use shifts instead of expensive division
    ((value + (1 << align) - 1) >> align) << align
}

pub fn align_gen_down(value: usize) -> usize {
    value & !(GEN_SIZE - 1)
}

/// returns true if given size is gen aligned
pub fn gen_aligned(size: usize) -> bool {
    (size & (GEN_SIZE - 1)) == 0
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
    if start == end {
        // nothing to do

    } else if end.offset_from(start) == mem::ptr_width_usize() {
        unsafe {
            *start.to_mut_ptr::<usize>() = 0;
        }
    } else if end.offset_from(start) == Header::size() as usize {
        // fill with object
        let cls_id = vm.vips.obj(vm);
        let cls = vm.class_defs.idx(cls_id);
        let cls = cls.read();
        let vtable: *const VTable = &**cls.vtable.as_ref().unwrap();

        unsafe {
            *start.to_mut_ptr::<usize>() = vtable as usize;
        }
    } else {
        // fill with int array
        let cls_id = vm.vips.int_array(vm);
        let cls = vm.class_defs.idx(cls_id);
        let cls = cls.read();
        let vtable: *const VTable = &**cls.vtable.as_ref().unwrap();

        // determine of header+length in bytes
        let header_size = Header::size() as usize + mem::ptr_width_usize();

        // calculate int array length
        let length: usize = end.offset_from(start.offset(header_size)) / 4;

        unsafe {
            *start.to_mut_ptr::<usize>() = vtable as usize;
            *start.offset(Header::size() as usize).to_mut_ptr::<usize>() = length;
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
