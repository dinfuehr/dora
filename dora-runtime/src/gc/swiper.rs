use parking_lot::{Mutex, RwLock};
use scoped_threadpool::Pool;
use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use threadpool::ThreadPool;

use crate::gc::allocator::GenerationAllocator;
use crate::gc::root::{determine_strong_roots, Slot};
use crate::gc::swiper::controller::{HeapController, SharedHeapConfig};
use crate::gc::swiper::full::FullCollector;
use crate::gc::swiper::heap::Heap;
use crate::gc::swiper::large::LargeSpace;
use crate::gc::swiper::minor::MinorCollector;
use crate::gc::swiper::old::OldGen;
use crate::gc::swiper::readonly::ReadOnlySpace;
use crate::gc::swiper::sweeper::Sweeper;
use crate::gc::swiper::verify::{Verifier, VerifierPhase};
use crate::gc::swiper::young::YoungGen;
use crate::gc::tlab::{MAX_TLAB_SIZE, MIN_TLAB_SIZE};
use crate::gc::{Address, Collector, GcReason, Region, Worklist, WorklistSegment, K, M};
use crate::mem;
use crate::object::Obj;
use crate::os::{self, Reservation};
use crate::threads::DoraThread;
use crate::vm::{get_vm, VmFlags, VM};

mod controller;
mod full;
mod heap;
mod large;
mod minor;
pub mod old;
mod readonly;
mod sweeper;
mod verify;
pub mod young;

// determines size of young generation in heap
// young generation size = heap size / YOUNG_RATIO
const YOUNG_RATIO: usize = 2;

pub const LARGE_OBJECT_SIZE: usize = 32 * K;
pub const PAGE_SIZE: usize = 64 * K;
pub const PAGE_HEADER_SIZE: usize = 512;

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

pub fn get_swiper(vm: &VM) -> &Swiper {
    vm.gc.collector.to_swiper()
}

pub struct Swiper {
    young: YoungGen,
    old: OldGen,
    large: LargeSpace,
    readonly: ReadOnlySpace,
    heap: Heap,

    sweeper: Sweeper,
    remset: RwLock<Vec<Address>>,
    remset2: RwLock<Worklist>,

    // minimum & maximum heap size
    min_heap_size: usize,
    max_heap_size: usize,

    threadpool: Mutex<Pool>,
    concurrent_threadpool: ThreadPool,
    config: SharedHeapConfig,

    reservation: Reservation,
}

impl Swiper {
    pub fn new(args: &VmFlags) -> Swiper {
        let max_heap_size = align_page_up(args.max_heap_size());
        let min_heap_size = align_page_up(args.min_heap_size());

        let mut config = HeapController::new(min_heap_size, max_heap_size);

        controller::init(&mut config, args);

        // Determine full reservation size.
        let reserve_size = max_heap_size * 4;

        // Reserve all memory.
        let reservation = os::reserve_align(reserve_size, PAGE_SIZE, false);
        let heap_region = reservation.start().region_start(reserve_size);

        let config = Arc::new(Mutex::new(config));

        let young = YoungGen::new(args.gc_verify);
        let old = OldGen::new(config.clone());
        let large = LargeSpace::new(config.clone());
        let heap = Heap::new(heap_region, config.clone());

        let nworkers = args.gc_workers();

        let threadpool = Mutex::new(Pool::new(nworkers as u32));
        let concurrent_threadpool = ThreadPool::new(nworkers);

        let readonly = ReadOnlySpace::new(args.readonly_size());

        Swiper {
            reservation,

            young,
            old,
            large,
            readonly,
            heap,

            config,
            sweeper: Sweeper::new(),
            remset: RwLock::new(Vec::new()),
            remset2: RwLock::new(Worklist::new()),

            min_heap_size,
            max_heap_size,

            threadpool,
            concurrent_threadpool,
        }
    }

    fn minor_collect(
        &self,
        vm: &VM,
        reason: GcReason,
        rootset: &[Slot],
        threads: &[Arc<DoraThread>],
    ) {
        self.verify(vm, VerifierPhase::PreMinor, CollectionKind::Minor, &rootset);

        {
            let mut pool = self.threadpool.lock();
            let mut collector = MinorCollector::new(
                vm,
                self,
                &self.young,
                &self.old,
                &self.large,
                rootset,
                threads,
                reason,
                self.min_heap_size,
                self.max_heap_size,
                &mut pool,
                &self.config,
            );

            collector.collect();

            if vm.flags.gc_stats {
                let mut config = self.config.lock();
                config.add_minor(collector.phases());
            }
        }

        self.verify(
            vm,
            VerifierPhase::PostMinor,
            CollectionKind::Minor,
            &rootset,
        );
    }

    fn full_collect(
        &self,
        vm: &VM,
        reason: GcReason,
        threads: &[Arc<DoraThread>],
        rootset: &[Slot],
    ) {
        self.verify(vm, VerifierPhase::PreFull, CollectionKind::Full, &rootset);

        {
            let mut collector = FullCollector::new(
                vm,
                self,
                &self.young,
                &self.old,
                &self.large,
                rootset,
                threads,
                reason,
                self.min_heap_size,
                self.max_heap_size,
            );
            collector.collect();

            if vm.flags.gc_stats {
                let mut config = self.config.lock();
                config.add_full(collector.phases());
            }
        }

        self.verify(vm, VerifierPhase::PostFull, CollectionKind::Full, &rootset);
    }

    fn verify(&self, vm: &VM, phase: VerifierPhase, _kind: CollectionKind, rootset: &[Slot]) {
        if vm.flags.gc_verify {
            let mut verifier = Verifier::new(
                vm,
                self,
                &self.young,
                &self.old,
                rootset,
                &self.large,
                &self.readonly,
                phase,
            );
            verifier.verify();
        }
    }

    fn alloc_normal(&self, vm: &VM, size: usize) -> Option<Address> {
        self.young.allocate(vm, size, size).map(|r| r.start())
    }

    fn alloc_large(&self, _vm: &VM, size: usize) -> Option<Address> {
        self.large.alloc(&self.heap, size)
    }

    pub fn add_remset_segment(&self, segment: WorklistSegment) {
        self.remset2.write().push_segment(segment);
    }
}

impl Collector for Swiper {
    fn alloc_tlab_area(&self, vm: &VM, _size: usize) -> Option<Region> {
        self.young.allocate(vm, MIN_TLAB_SIZE, MAX_TLAB_SIZE)
    }

    fn alloc_object(&self, vm: &VM, size: usize) -> Option<Address> {
        if size < LARGE_OBJECT_SIZE {
            self.alloc_normal(vm, size)
        } else {
            self.alloc_large(vm, size)
        }
    }

    fn alloc_readonly(&self, vm: &VM, size: usize) -> Address {
        self.readonly.alloc(vm, size).unwrap_or(Address::null())
    }

    fn collect_garbage(&self, vm: &VM, threads: &[Arc<DoraThread>], reason: GcReason, size: usize) {
        let kind = choose_collection_kind(&self.heap, reason, size);
        controller::start(&self.config, &self.heap);

        let rootset = determine_strong_roots(vm, threads);

        match kind {
            CollectionKind::Minor => {
                self.minor_collect(vm, reason, &rootset, threads);
            }

            CollectionKind::Full => {
                self.full_collect(vm, reason, threads, &rootset);
            }
        }

        controller::stop(
            vm,
            &self.config,
            kind,
            &self.heap,
            &self.young,
            &vm.flags,
            reason,
        );
    }

    fn needs_write_barrier(&self) -> bool {
        true
    }

    fn initial_metadata_value(&self, size: usize, is_readonly: bool) -> (bool, bool) {
        if is_readonly {
            assert!(size < LARGE_OBJECT_SIZE);
            (true, false)
        } else if size < LARGE_OBJECT_SIZE {
            (false, true)
        } else {
            (false, false)
        }
    }

    fn dump_summary(&self, runtime: f32) {
        let config = self.config.lock();
        let total_gc = config.total_minor_pause + config.total_full_pause;
        let gc_percentage = ((total_gc / runtime) * 100.0).round();
        let mutator = runtime - total_gc;
        let mutator_percentage = 100.0 - gc_percentage;

        println!(
            "GC summary: {:.1}ms minor ({}), {:.1}ms full ({}), {:.1}ms collection, {:.1}ms mutator, {:.1}ms total ({}% mutator, {}% GC)",
            config.total_minor_pause,
            config.total_minor_collections,
            config.total_full_pause,
            config.total_full_collections,
            total_gc,
            mutator,
            runtime,
            mutator_percentage,
            gc_percentage,
        );

        println!("\nMinor:\t\t\t{}", config.minor_total());

        println!("\nFull:");

        println!("\tComplete:\t{}", config.full_complete());
        println!("\tMarking:\t{}", config.full_marking());
        println!("\tSweep:\t\t{}", config.full_sweep());
        println!("\tTotal:\t\t{}", config.full_total());

        println!("");
    }

    fn setup(&self, vm: &VM) {
        let semi_size = self.config.lock().young_size;
        self.young.setup(vm, semi_size);
    }

    fn shutdown(&self, _vm: &VM) {
        self.sweeper.join();
    }

    fn to_swiper(&self) -> &Swiper {
        self
    }
}

fn choose_collection_kind(heap: &Heap, reason: GcReason, size: usize) -> CollectionKind {
    match reason {
        GcReason::ForceCollect | GcReason::Stress | GcReason::LastResort => CollectionKind::Full,
        GcReason::ForceMinorCollect | GcReason::StressMinor => CollectionKind::Minor,

        GcReason::AllocationFailure => {
            if size < LARGE_OBJECT_SIZE {
                let young_size = heap.committed_sizes().young;

                if young_size <= M {
                    CollectionKind::Full
                } else {
                    CollectionKind::Minor
                }
            } else {
                CollectionKind::Full
            }
        }
    }
}

#[derive(Copy, Clone)]
pub enum CollectionKind {
    Minor,
    Full,
}

impl CollectionKind {
    fn is_minor(&self) -> bool {
        match self {
            CollectionKind::Minor => true,
            CollectionKind::Full => false,
        }
    }

    fn is_full(&self) -> bool {
        match self {
            CollectionKind::Minor => false,
            CollectionKind::Full => true,
        }
    }
}

impl fmt::Display for CollectionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            CollectionKind::Minor => "Minor",
            CollectionKind::Full => "Full",
        };

        write!(f, "{}", name)
    }
}

pub fn walk_region<F>(vm: &VM, region: Region, mut fct: F)
where
    F: FnMut(&Obj, Address, usize),
{
    let mut scan = region.start;

    while scan < region.end {
        let object = scan.to_obj();

        if object.is_filler(vm) {
            scan = scan.offset(object.size(vm.meta_space_start()));
        } else {
            let object_size = object.size(vm.meta_space_start());
            fct(object, scan, object_size);
            scan = scan.offset(object_size);
        }
    }

    assert_eq!(scan, region.end);
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BasePage(Address);

impl BasePage {
    pub fn from_address(value: Address) -> BasePage {
        let page_start = value.to_usize() & !(PAGE_SIZE - 1);
        let page = BasePage(page_start.into());
        page
    }

    pub fn is_young(&self) -> bool {
        self.base_page_header().is_young()
    }

    pub fn is_large(&self) -> bool {
        self.base_page_header().is_large()
    }

    pub fn is_readonly(&self) -> bool {
        self.base_page_header().is_readonly()
    }

    pub fn is_survivor(&self) -> bool {
        self.base_page_header().is_survivor()
    }

    fn base_page_header(&self) -> &BasePageHeader {
        unsafe { &*self.0.to_ptr::<BasePageHeader>() }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegularPage(Address);

impl RegularPage {
    pub fn from_address(value: Address) -> RegularPage {
        let page_start = value.to_usize() & !(PAGE_SIZE - 1);
        let page = RegularPage(page_start.into());
        debug_assert!(!page.is_large());
        page
    }

    pub fn setup(address: Address, is_young: bool, is_readonly: bool) -> RegularPage {
        assert!(address.is_page_aligned());

        let page = RegularPage(address);
        page.base_page_header().setup(is_young, false, is_readonly);

        unsafe {
            std::ptr::write(page.object_area_end().to_mut_ptr::<usize>(), 0xDEAD2BAD);
        }

        page
    }

    pub fn promote(&self) {
        assert!(self.is_young());
        self.base_page_header().remove_flag(YOUNG_BIT);
        self.base_page_header().remove_flag(SURVIVOR_BIT);
        assert!(!self.is_young());
        assert!(!self.is_survivor());
    }

    pub fn as_base_page(&self) -> BasePage {
        BasePage(self.address())
    }

    pub fn area(&self) -> Region {
        Region::new(self.address(), self.end())
    }

    pub fn address(&self) -> Address {
        self.0
    }

    pub fn end(&self) -> Address {
        self.address().offset(PAGE_SIZE)
    }

    pub fn size(&self) -> usize {
        PAGE_SIZE
    }

    pub fn object_area(&self) -> Region {
        Region::new(self.object_area_start(), self.object_area_end())
    }

    pub fn object_area_start(&self) -> Address {
        self.address().offset(std::mem::size_of::<BasePageHeader>())
    }

    pub fn object_area_end(&self) -> Address {
        self.end().sub_ptr(1)
    }

    pub fn is_young(&self) -> bool {
        self.base_page_header().is_young()
    }

    pub fn is_large(&self) -> bool {
        self.base_page_header().is_large()
    }

    pub fn is_survivor(&self) -> bool {
        self.base_page_header().is_survivor()
    }

    pub fn is_readonly(&self) -> bool {
        self.base_page_header().is_readonly()
    }

    fn raw_flags(&self) -> usize {
        self.base_page_header().raw_flags()
    }

    fn base_page_header(&self) -> &BasePageHeader {
        unsafe { &*self.0.to_ptr::<BasePageHeader>() }
    }
}

const YOUNG_BIT: usize = 1;
const LARGE_BIT: usize = 1 << 1;
const SURVIVOR_BIT: usize = 1 << 2;
const READONLY_BIT: usize = 1 << 3;

#[repr(C)]
struct BasePageHeader {
    flags: AtomicUsize,
}

impl BasePageHeader {
    fn is_young(&self) -> bool {
        (self.raw_flags() & YOUNG_BIT) != 0
    }

    fn is_large(&self) -> bool {
        (self.raw_flags() & LARGE_BIT) != 0
    }

    fn is_survivor(&self) -> bool {
        (self.raw_flags() & SURVIVOR_BIT) != 0
    }

    fn is_readonly(&self) -> bool {
        (self.raw_flags() & READONLY_BIT) != 0
    }

    fn add_flag(&self, flag: usize) {
        self.set_raw_flags(self.raw_flags() | flag);
    }

    fn remove_flag(&self, flag: usize) {
        self.set_raw_flags(self.raw_flags() & !flag);
    }

    fn raw_flags(&self) -> usize {
        self.flags.load(Ordering::Relaxed)
    }

    fn setup(&self, is_young: bool, is_large: bool, is_readonly: bool) {
        let mut flags = 0;

        if is_young {
            flags |= YOUNG_BIT
        }

        if is_large {
            flags |= LARGE_BIT
        }

        if is_readonly {
            flags |= READONLY_BIT
        }

        self.set_raw_flags(flags);
    }

    fn set_raw_flags(&self, flags: usize) {
        self.flags.store(flags, Ordering::Relaxed)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LargePage(Address);

impl LargePage {
    pub fn compute_sizes(object_size: usize) -> (usize, usize) {
        assert!(object_size >= LARGE_OBJECT_SIZE);

        let committed_size = mem::os_page_align_up(LargePage::object_offset() + object_size);
        let reserved_size = align_page_up(committed_size);

        (committed_size, reserved_size)
    }

    pub fn setup(address: Address, committed_size: usize) -> LargePage {
        assert!(address.is_page_aligned());

        let page = LargePage(address);
        page.base_page_header().setup(false, true, false);
        page.large_page_header().setup(committed_size);

        page
    }

    pub fn as_base_page(&self) -> BasePage {
        BasePage(self.address())
    }

    pub fn address(&self) -> Address {
        self.0
    }

    pub fn is_young(&self) -> bool {
        self.base_page_header().is_young()
    }

    pub fn is_large(&self) -> bool {
        self.base_page_header().is_large()
    }

    pub fn object_address(&self) -> Address {
        self.address().offset(LargePage::object_offset())
    }

    const fn object_offset() -> usize {
        std::mem::size_of::<BasePageHeader>() + std::mem::size_of::<LargePageHeader>()
    }

    pub fn next_page(&self) -> Option<LargePage> {
        let next = self.next_page_address();

        if next.is_null() {
            None
        } else {
            Some(LargePage(next))
        }
    }

    fn next_page_address(&self) -> Address {
        self.large_page_header().next
    }

    pub fn set_next_page(&self, page: Option<LargePage>) {
        self.large_page_header().next = page.map(|p| p.address()).unwrap_or(Address::null())
    }

    pub fn set_prev_page(&self, page: Option<LargePage>) {
        self.large_page_header().prev = page.map(|p| p.address()).unwrap_or(Address::null())
    }

    pub fn committed_size(&self) -> usize {
        self.large_page_header().size
    }

    fn raw_flags(&self) -> usize {
        self.base_page_header().flags.load(Ordering::Relaxed)
    }

    fn base_page_header(&self) -> &BasePageHeader {
        unsafe { &*self.address().to_ptr::<BasePageHeader>() }
    }

    fn large_page_header(&self) -> &mut LargePageHeader {
        unsafe {
            &mut *self
                .address()
                .offset(std::mem::size_of::<BasePageHeader>())
                .to_mut_ptr::<LargePageHeader>()
        }
    }
}

struct LargePageHeader {
    prev: Address,
    next: Address,
    size: usize,
}

impl LargePageHeader {
    fn setup(&mut self, committed_size: usize) {
        self.prev = Address::null();
        self.next = Address::null();
        self.size = committed_size;
    }
}

pub extern "C" fn object_write_barrier_slow_path(object_address: Address, value_address: Address) {
    let vm = get_vm();
    debug_assert!(!BasePage::from_address(object_address).is_young());
    let obj = object_address.to_obj();
    obj.header().set_remembered();

    debug_assert!(value_address.is_non_null());
    let value = value_address.to_obj();
    debug_assert_eq!(value.header().compressed_vtblptr() & 1, 0);
    debug_assert_eq!(value.header().sentinel(), 0xFFFF_FFFC);

    let swiper = get_swiper(vm);
    swiper.remset.write().push(object_address);
    assert!(obj.header().is_remembered());
}
