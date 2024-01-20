use parking_lot::{Mutex, RwLock};
use scoped_threadpool::Pool;
use std::fmt;
use std::mem::size_of;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use crate::gc::allocator::GenerationAllocator;
use crate::gc::root::{determine_strong_roots, Slot};
use crate::gc::swiper::card::CardTable;
use crate::gc::swiper::controller::{HeapController, SharedHeapConfig};
use crate::gc::swiper::crossing::CrossingMap;
use crate::gc::swiper::full::FullCollector;
use crate::gc::swiper::large::LargeSpace;
use crate::gc::swiper::minor::MinorCollector;
use crate::gc::swiper::old::OldGen;
use crate::gc::swiper::readonly::ReadOnlySpace;
use crate::gc::swiper::verify::{Verifier, VerifierPhase};
use crate::gc::swiper::young::YoungGen;
use crate::gc::{tlab, Address, Collector, GcReason, Region, K};
use crate::mem;
use crate::object::Obj;
use crate::os::{self, MemoryPermission, Reservation};
use crate::safepoint;
use crate::threads::DoraThread;
use crate::vm::{get_vm, Flags, VM};

use super::tlab::{MAX_TLAB_SIZE, MIN_TLAB_SIZE};

pub mod card;
mod controller;
mod crossing;
mod full;
mod large;
mod minor;
pub mod old;
mod readonly;
mod verify;
pub mod young;

// determines size of young generation in heap
// young generation size = heap size / YOUNG_RATIO
const YOUNG_RATIO: usize = 2;

// heap is divided into cards of size CARD_SIZE.
// card entry determines whether this part of the heap was modified
// in minor collections those parts of the heap need to be analyzed
pub const CARD_SIZE_BITS: usize = 9;
pub const CARD_SIZE: usize = 1 << CARD_SIZE_BITS;
pub const CARD_REFS: usize = CARD_SIZE / size_of::<usize>();

pub const LARGE_OBJECT_SIZE: usize = 32 * K;
pub const PAGE_SIZE: usize = 64 * K;
pub const PAGE_HEADER_SIZE: usize = CARD_SIZE;

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

pub struct Swiper {
    // contiguous memory for young/old generation and large space
    heap: Region,

    // contains heap and also card table and crossing map
    reserved_area: Region,

    young: YoungGen,
    old: OldGen,
    large: LargeSpace,
    readonly: ReadOnlySpace,

    card_table: CardTable,
    crossing_map: CrossingMap,
    remset: RwLock<Vec<Address>>,

    card_table_offset: usize,

    // minimum & maximum heap size
    min_heap_size: usize,
    max_heap_size: usize,

    threadpool: Mutex<Pool>,
    config: SharedHeapConfig,

    reservation: Reservation,
}

impl Swiper {
    pub fn new(args: &Flags) -> Swiper {
        let max_heap_size = align_page_up(args.max_heap_size());
        let min_heap_size = align_page_up(args.min_heap_size());

        let mut config = HeapController::new(min_heap_size, max_heap_size);

        controller::init(&mut config, args);

        // Determine size for card table.
        let card_size = mem::os_page_align_up((4 * max_heap_size) >> CARD_SIZE_BITS);

        // Determine size for crossing map.
        let crossing_size = mem::os_page_align_up(max_heap_size >> CARD_SIZE_BITS);

        // Determine full reservation size.
        let reserve_size = max_heap_size * 4 + card_size + crossing_size;

        // Reserve all memory.
        let reservation = os::reserve_align(reserve_size, PAGE_SIZE, false);
        let heap_start = reservation.start();

        // Heap is young, old generation & large space.
        let heap_end = heap_start.offset(4 * max_heap_size);

        // Reserved area contains everything.
        let reserved_area = heap_start.region_start(reserve_size);

        // Determine offset to card table (card table starts right after heap).
        // offset = card_table_start - (heap_start >> CARD_SIZE_BITS)
        let card_table_offset = heap_end.to_usize() - (heap_start.to_usize() >> CARD_SIZE_BITS);

        // Determine boundaries for card table.
        let card_start = heap_end;
        let card_end = card_start.offset(card_size);

        os::commit_at(card_start, card_size, MemoryPermission::ReadWrite);

        // Determine boundaries for crossing map.
        let crossing_start = card_end;
        let crossing_end = crossing_start.offset(crossing_size);

        os::commit_at(crossing_start, crossing_size, MemoryPermission::ReadWrite);

        // Determine boundaries of young generation.
        let young_start = heap_start;
        let young_end = young_start.offset(max_heap_size);
        let young = Region::new(young_start, young_end);

        // Determine boundaries of old generation.
        let old_start = young_end;
        let old_end = old_start.offset(max_heap_size);

        let semi_size = config.semi_size;

        // Determine large object space.
        let large_start = old_end;
        let large_end = large_start.offset(2 * max_heap_size);

        let card_table = CardTable::new(
            card_start,
            card_end,
            Region::new(old_start, large_end),
            old_end,
            max_heap_size,
        );
        let crossing_map = CrossingMap::new(crossing_start, crossing_end, max_heap_size);
        let young = YoungGen::new(young, semi_size, args.gc_verify);

        let config = Arc::new(Mutex::new(config));
        let old = OldGen::new(
            old_start,
            old_end,
            crossing_map.clone(),
            card_table.clone(),
            config.clone(),
        );
        let large = LargeSpace::new(large_start, large_end, config.clone());

        let nworkers = args.gc_workers();

        let threadpool = Mutex::new(Pool::new(nworkers as u32));

        let readonly = ReadOnlySpace::new(args.readonly_size());

        Swiper {
            heap: Region::new(heap_start, heap_end),
            reserved_area,
            reservation,

            young,
            old,
            large,
            readonly,

            card_table,
            crossing_map,
            config,
            remset: RwLock::new(Vec::new()),

            card_table_offset,

            min_heap_size,
            max_heap_size,

            threadpool,
        }
    }

    fn perform_collection_and_choose(&self, vm: &VM, reason: GcReason) -> CollectionKind {
        let kind = controller::choose_collection_kind(&self.config, &vm.flags, &self.young);
        self.perform_collection(vm, kind, reason)
    }

    fn perform_collection(
        &self,
        vm: &VM,
        kind: CollectionKind,
        reason: GcReason,
    ) -> CollectionKind {
        safepoint::stop_the_world(vm, |threads| {
            controller::start(&self.config, &self.young, &self.old, &self.large);

            tlab::make_iterable_all(vm, threads);
            let rootset = determine_strong_roots(vm, threads);

            let kind = match kind {
                CollectionKind::Minor => {
                    self.minor_collect(vm, reason, &rootset, threads);
                    CollectionKind::Minor
                }

                CollectionKind::Full => {
                    self.full_collect(vm, reason, threads, &rootset);
                    CollectionKind::Full
                }
            };

            controller::stop(
                vm,
                &self.config,
                kind,
                &self.young,
                &self.old,
                &self.large,
                &vm.flags,
                reason,
            );

            kind
        })
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
                &self.card_table,
                &self.crossing_map,
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
                self.heap.clone(),
                &self.young,
                &self.old,
                &self.large,
                &self.card_table,
                &self.crossing_map,
                &self.readonly,
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
                self.heap,
                &self.young,
                &self.old,
                &self.card_table,
                &self.crossing_map,
                rootset,
                &self.large,
                &self.readonly,
                phase,
            );
            verifier.verify();
        }
    }

    fn alloc_normal(&self, vm: &VM, size: usize) -> Address {
        if let Some(region) = self.young.allocate(vm, size, size) {
            return region.start();
        }

        self.perform_collection_and_choose(vm, GcReason::AllocationFailure);

        self.young
            .allocate(vm, size, size)
            .map(|r| r.start())
            .unwrap_or(Address::null())
    }

    fn alloc_large(&self, vm: &VM, size: usize) -> Address {
        if let Some(address) = self.large.alloc(size) {
            return address;
        }

        self.perform_collection(vm, CollectionKind::Full, GcReason::AllocationFailure);

        self.large.alloc(size).unwrap_or(Address::null())
    }
}

impl Collector for Swiper {
    fn supports_tlab(&self) -> bool {
        true
    }

    fn alloc_tlab_area(&self, vm: &VM, _size: usize) -> Option<Region> {
        if let Some(region) = self.young.allocate(vm, MIN_TLAB_SIZE, MAX_TLAB_SIZE) {
            return Some(region);
        }

        self.perform_collection_and_choose(vm, GcReason::AllocationFailure);

        if let Some(region) = self.young.allocate(vm, MIN_TLAB_SIZE, MAX_TLAB_SIZE) {
            return Some(region);
        }

        self.perform_collection(vm, CollectionKind::Full, GcReason::AllocationFailure);

        if let Some(region) = self.young.allocate(vm, MIN_TLAB_SIZE, MAX_TLAB_SIZE) {
            return Some(region);
        }

        None
    }

    fn alloc(&self, vm: &VM, size: usize) -> Address {
        if size < LARGE_OBJECT_SIZE {
            self.alloc_normal(vm, size)
        } else {
            self.alloc_large(vm, size)
        }
    }

    fn alloc_readonly(&self, vm: &VM, size: usize) -> Address {
        self.readonly.alloc(vm, size).unwrap_or(Address::null())
    }

    fn collect(&self, vm: &VM, reason: GcReason) {
        self.perform_collection(vm, CollectionKind::Full, reason);
    }

    fn minor_collect(&self, vm: &VM, reason: GcReason) {
        self.perform_collection(vm, CollectionKind::Minor, reason);
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

    fn card_table_offset(&self) -> usize {
        self.card_table_offset
    }

    fn dump_summary(&self, runtime: f32) {
        let config = self.config.lock();
        let total_gc = config.total_minor_pause + config.total_full_pause;
        let gc_percentage = ((total_gc / runtime) * 100.0).round();
        let mutator = runtime - total_gc;
        let mutator_percentage = 100.0 - gc_percentage;

        println!("GC stats: total={:.1}", runtime);
        println!("GC stats: mutator={:.1}", mutator);
        println!("GC stats: collection={:.1}", total_gc);
        println!("GC stats: collection-minor={:.1}", config.total_minor_pause);
        println!("GC stats: collection-full={:.1}", config.total_full_pause);

        println!("");
        println!(
            "GC stats: full-collections={:.1}",
            config.total_full_collections
        );
        println!("GC stats: full-total={}", config.full_total_all());
        println!("GC stats: full-marking={}", config.full_marking_all());
        println!("GC stats: full-sweep={}", config.full_sweep_all());
        println!(
            "GC stats: full-update-refs={}",
            config.full_update_refs_all()
        );
        println!("GC stats: full-evacuate={}", config.full_relocate_all());
        println!("");
        println!(
            "GC stats: minor-collections={:.1}",
            config.total_minor_collections
        );
        println!("GC stats: minor-total={}", config.minor_total_all());
        println!("GC stats: minor-roots={}", config.minor_roots_all());
        println!("GC stats: minor-tracing={}", config.minor_tracing_all());
        println!("");

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

        println!("\nMinor:");
        println!("\tRoots:\t\t{}", config.minor_roots());
        println!("\tTracing:\t{}", config.minor_tracing());
        println!("\tTotal:\t\t{}", config.minor_total());

        println!("\nFull:");

        println!("\tMarking:\t{}", config.full_marking());
        println!("\tSweep:\t\t{}", config.full_sweep());
        println!("\tUpdate Refs:\t{}", config.full_update_refs());
        println!("\tEvacuate:\t{}", config.full_relocate());
        println!("\tTotal:\t\t{}", config.full_total());

        println!("");
    }

    fn setup(&self, vm: &VM) {
        self.young.setup(vm);
    }

    fn to_swiper(&self) -> &Swiper {
        self
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct CardIdx(usize);

impl CardIdx {
    pub fn to_usize(self) -> usize {
        self.0
    }

    pub fn offset(self, val: usize) -> CardIdx {
        (self.0 + val).into()
    }
}

impl From<usize> for CardIdx {
    fn from(val: usize) -> CardIdx {
        CardIdx(val)
    }
}

fn on_different_cards(curr: Address, next: Address) -> bool {
    (curr.to_usize() >> CARD_SIZE_BITS) != (next.to_usize() >> CARD_SIZE_BITS)
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

pub trait CommonOldGen {
    fn active_size(&self) -> usize;
    fn committed_size(&self) -> usize;
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

        let uninit_start = page.address().offset(std::mem::size_of::<BasePageHeader>());
        debug_assert_eq!(uninit_start.to_usize() % mem::ptr_width_usize(), 0);
        let uninit_end = page.address().offset(PAGE_HEADER_SIZE);
        debug_assert_eq!(uninit_end.to_usize() % mem::ptr_width_usize(), 0);
        let length = uninit_end.offset_from(uninit_start);
        debug_assert_eq!(length % mem::ptr_width_usize(), 0);

        unsafe {
            let header = std::slice::from_raw_parts_mut(
                uninit_start.to_mut_ptr::<usize>(),
                length / mem::ptr_width_usize(),
            );

            header.fill(0xDEAD2BAD);
        }

        page
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
        self.address().offset(PAGE_HEADER_SIZE)
    }

    pub fn object_area_end(&self) -> Address {
        self.end()
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

pub extern "C" fn object_write_barrier_slow_path(object_address: Address) {
    let vm = get_vm();
    debug_assert!(vm.flags.object_write_barrier);
    debug_assert!(!BasePage::from_address(object_address).is_young());
    let obj = object_address.to_obj();
    obj.header().set_remembered();
    let swiper = vm.gc.collector.to_swiper();
    swiper.remset.write().push(object_address);
    assert!(obj.header().is_remembered());
}
