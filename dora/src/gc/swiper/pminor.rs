use parking_lot::Mutex;
use std::cmp;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Barrier;

use crate::gc::pmarking::Terminator;
use crate::gc::root::Slot;
use crate::gc::swiper::card::{CardEntry, CardTable};
use crate::gc::swiper::controller::{MinorCollectorPhases, SharedHeapConfig};
use crate::gc::swiper::crossing::{CrossingEntry, CrossingMap};
use crate::gc::swiper::large::{LargeAlloc, LargeSpace};
use crate::gc::swiper::old::OldGen;
use crate::gc::swiper::on_different_cards;
use crate::gc::swiper::young::YoungGen;
use crate::gc::swiper::{CardIdx, CARD_SIZE, LARGE_OBJECT_SIZE};
use crate::gc::tlab::{TLAB_OBJECT_SIZE, TLAB_SIZE};
use crate::gc::{fill_region, Address, GcReason, Region};
use crate::object::{offset_of_array_data, Obj};
use crate::timer::Timer;
use crate::vm::VM;
use crate::vtable::VTable;

use crossbeam_deque::{Injector, Steal, Stealer, Worker};
use rand::distributions::{Distribution, Uniform};
use rand::thread_rng;
use scoped_threadpool::Pool;

pub struct ParallelMinorCollector<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,

    young: &'a YoungGen,
    old: &'a OldGen,
    large: &'a LargeSpace,
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,

    rootset: &'a [Slot],
    reason: GcReason,

    young_top: Address,
    young_limit: Address,
    init_old_top: Vec<Address>,

    promotion_failed: bool,
    promoted_size: usize,

    from_active: Region,
    eden_active: Region,

    min_heap_size: usize,
    max_heap_size: usize,

    threadpool: &'a mut Pool,
    number_workers: usize,
    worklist: Vec<Address>,
    config: &'a SharedHeapConfig,
    phases: MinorCollectorPhases,
}

impl<'a, 'ast: 'a> ParallelMinorCollector<'a, 'ast> {
    pub fn new(
        vm: &'a VM<'ast>,
        young: &'a YoungGen,
        old: &'a OldGen,
        large: &'a LargeSpace,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
        rootset: &'a [Slot],
        reason: GcReason,
        min_heap_size: usize,
        max_heap_size: usize,
        threadpool: &'a mut Pool,
        config: &'a SharedHeapConfig,
    ) -> ParallelMinorCollector<'a, 'ast> {
        ParallelMinorCollector {
            vm,
            young,
            old,
            large,
            rootset,
            card_table,
            crossing_map,

            young_top: Address::null(),
            young_limit: Address::null(),
            init_old_top: Vec::new(),

            promotion_failed: false,
            promoted_size: 0,

            from_active: Default::default(),
            eden_active: young.eden_active(),

            reason,

            min_heap_size,
            max_heap_size,

            number_workers: threadpool.thread_count() as usize,
            threadpool,

            worklist: Vec::new(),
            config,
            phases: MinorCollectorPhases::new(),
        }
    }

    pub fn phases(&self) -> MinorCollectorPhases {
        self.phases.clone()
    }

    pub fn collect(&mut self) -> bool {
        self.init_old_top = {
            let protected = self.old.protected();
            protected.regions.iter().map(|r| r.top()).collect()
        };

        self.young.unprotect_from();
        self.young.swap_semi();

        let to_committed = self.young.to_committed();
        self.young_top = to_committed.start;
        self.young_limit = to_committed.end;

        self.from_active = self.young.from_active();

        let dev_verbose = self.vm.args.flag_gc_dev_verbose;

        if dev_verbose {
            println!("Minor GC: Worker threads started");
        }

        self.run_threads();

        if dev_verbose {
            println!("Minor GC: Worker threads finished");
        }

        if self.promotion_failed {
            // oh no: promotion failed, we need a subsequent full GC
            self.remove_forwarding_pointers();
            self.young.minor_fail(self.young_top);

            return true;
        }

        self.young.minor_success(self.young_top);

        assert!(self.young.eden_active().empty());
        assert!(self.young.from_active().empty());

        let mut config = self.config.lock();
        config.minor_promoted = self.promoted_size;
        config.minor_copied = self.young.from_active().size();

        self.promotion_failed
    }

    fn run_threads(&mut self) {
        let mut workers = Vec::with_capacity(self.number_workers);
        let mut stealers = Vec::with_capacity(self.number_workers);
        let injector = Injector::new();

        let stats = self.vm.args.flag_gc_stats;
        let timer = Timer::new(stats);

        for _ in 0..self.number_workers {
            let w = Worker::new_lifo();
            let s = w.stealer();
            workers.push(w);
            stealers.push(s);
        }

        let worklist = std::mem::replace(&mut self.worklist, Vec::new());

        for object in worklist {
            injector.push(object);
        }

        let terminator = Terminator::new(self.number_workers);
        let number_workers = self.number_workers;
        let young_region = self.young.total();
        let vm = self.vm;

        // align old generation to card boundary
        let young_top = Mutex::new(self.young_top);
        let young_limit = self.young_limit;

        let card_table = self.card_table;
        let crossing_map = self.crossing_map;
        let young = self.young;
        let old = self.old;
        let large = self.large;
        let rootset = self.rootset;
        let init_old_top = &self.init_old_top;
        let old_region_start = {
            let protected = self.old.protected();
            protected
                .regions
                .iter()
                .map(|r| r.start())
                .collect::<Vec<_>>()
        };
        let old_region_start = &old_region_start;
        let barrier = Barrier::new(self.number_workers);
        let barrier = &barrier;

        let promoted_size = AtomicUsize::new(self.promoted_size);
        let promoted_size = &promoted_size;

        let promotion_failed = AtomicBool::new(self.promotion_failed);
        let promotion_failed = &promotion_failed;

        let next_root_stride = AtomicUsize::new(0);
        let next_root_stride = &next_root_stride;

        let next_card_stride = AtomicUsize::new(0);
        let next_card_stride = &next_card_stride;
        let strides = 4 * self.number_workers;

        let head = self.large.head();
        let next_large = Mutex::new(head);
        let next_large = &next_large;

        let prot_timer: Option<Mutex<(Timer, f32)>> = if stats {
            Some(Mutex::new((timer, 0.0f32)))
        } else {
            None
        };
        let prot_timer = &prot_timer;

        self.threadpool.scoped(|scoped| {
            for (task_id, worker) in workers.into_iter().enumerate() {
                let injector = &injector;
                let stealers = &stealers;
                let terminator = &terminator;
                let young_region = young_region.clone();
                let young_top = &young_top;
                let promoted_size = promoted_size.clone();
                let promotion_failed = promotion_failed.clone();

                scoped.execute(move || {
                    let mut task = CopyTask {
                        task_id,
                        local: Vec::new(),
                        worker,
                        injector,
                        stealers,
                        terminator,
                        number_workers,

                        vm,
                        young,
                        old,
                        large,
                        young_region,
                        card_table,
                        crossing_map,
                        rootset,
                        init_old_top,
                        old_region_start,
                        barrier,

                        from_active: young.from_active(),
                        eden_active: young.eden_active(),

                        next_card_stride,
                        next_root_stride,
                        strides,
                        next_large,

                        promoted_size: 0,
                        traced: 0,

                        old_lab: Lab::new(),
                        promotion_failed: false,

                        young_lab: Lab::new(),
                        young_top,
                        young_limit,
                        copy_failed: false,

                        timer: prot_timer,
                    };

                    task.run();

                    if task.promoted_size > 0 {
                        promoted_size.fetch_add(task.promoted_size, Ordering::SeqCst);
                    }

                    if task.promotion_failed() {
                        promotion_failed.store(true, Ordering::SeqCst);
                    }
                });
            }
        });

        if let Some(ref mutex) = prot_timer {
            let mut mutex = mutex.lock();
            let (ref mut timer, duration_roots) = *mutex;
            self.phases.roots = duration_roots;
            self.phases.tracing = timer.stop();
        }

        self.young_top = *young_top.lock();

        self.promoted_size = promoted_size.load(Ordering::SeqCst);
        self.promotion_failed = promotion_failed.load(Ordering::SeqCst);
    }

    fn remove_forwarding_pointers(&mut self) {
        let region = self.eden_active.clone();
        self.remove_forwarding_pointers_in_region(region);

        let region = self.from_active.clone();
        self.remove_forwarding_pointers_in_region(region);
    }

    fn remove_forwarding_pointers_in_region(&mut self, region: Region) {
        let mut scan = region.start;

        while scan < region.end {
            let obj = scan.to_mut_obj();

            if obj.header().vtblptr().is_null() {
                scan = scan.add_ptr(1);
                continue;
            }

            obj.header_mut().vtblptr_repair();
            scan = scan.offset(obj.size());
        }

        assert!(scan == region.end);
    }
}

struct Lab {
    top: Address,
    limit: Address,
}

impl Lab {
    fn new() -> Lab {
        Lab {
            top: Address::null(),
            limit: Address::null(),
        }
    }

    fn reset(&mut self, top: Address, limit: Address) {
        self.top = top;
        self.limit = limit;
    }

    fn make_iterable_young(&mut self, vm: &VM) {
        fill_region(vm, self.top, self.limit);

        self.top = Address::null();
        self.limit = Address::null();
    }

    fn make_iterable_old(&mut self, vm: &VM, old: &OldGen) {
        fill_region(vm, self.top, self.limit);
        if self.limit.is_non_null() {
            old.update_crossing(self.top, self.limit, false);
        }

        self.top = Address::null();
        self.limit = Address::null();
    }

    fn alloc(&mut self, size: usize) -> Address {
        let object_start = self.top;
        let object_end = object_start.offset(size);

        if object_end <= self.limit {
            self.top = object_end;
            object_start
        } else {
            Address::null()
        }
    }

    fn undo_alloc(&mut self, size: usize) {
        self.top = (self.top.to_usize() - size).into();
        debug_assert!(self.limit.offset_from(self.top) <= CLAB_SIZE);
    }
}

const CLAB_SIZE: usize = TLAB_SIZE;
const CLAB_OBJECT_SIZE: usize = TLAB_OBJECT_SIZE;

const LOCAL_MAXIMUM: usize = 64;

struct CopyTask<'a, 'ast: 'a> {
    task_id: usize,
    local: Vec<Address>,
    worker: Worker<Address>,
    injector: &'a Injector<Address>,
    stealers: &'a [Stealer<Address>],
    terminator: &'a Terminator,
    number_workers: usize,

    vm: &'a VM<'ast>,
    young: &'a YoungGen,
    old: &'a OldGen,
    large: &'a LargeSpace,
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
    rootset: &'a [Slot],
    init_old_top: &'a [Address],
    old_region_start: &'a [Address],
    barrier: &'a Barrier,

    next_root_stride: &'a AtomicUsize,
    next_card_stride: &'a AtomicUsize,
    strides: usize,
    next_large: &'a Mutex<Address>,

    young_region: Region,
    from_active: Region,
    eden_active: Region,

    promoted_size: usize,
    traced: usize,

    old_lab: Lab,
    promotion_failed: bool,

    young_lab: Lab,
    young_top: &'a Mutex<Address>,
    young_limit: Address,
    copy_failed: bool,

    timer: &'a Option<Mutex<(Timer, f32)>>,
}

impl<'a, 'ast> CopyTask<'a, 'ast>
where
    'ast: 'a,
{
    fn run(&mut self) {
        self.visit_roots();
        self.visit_dirty_cards();

        self.barrier();

        self.trace_gray_objects();
    }

    fn barrier(&mut self) {
        self.barrier.wait();

        if self.task_id == 0 {
            if let Some(ref mutex) = self.timer {
                let mut mutex = mutex.lock();
                let (ref mut timer, ref mut duration_roots) = *mutex;
                *duration_roots = timer.stop();
            }
        }
    }

    fn visit_roots(&mut self) {
        while let Some(stride) = self.next_root_stride() {
            self.visit_roots_in_stride(stride);
        }
    }

    fn next_root_stride(&mut self) -> Option<usize> {
        let stride = self.next_root_stride.fetch_add(1, Ordering::SeqCst);

        if stride < self.strides {
            Some(stride)
        } else {
            None
        }
    }

    fn visit_roots_in_stride(&mut self, stride: usize) {
        let roots_in_stride = self.rootset.iter().skip(stride).step_by(self.strides);

        for root in roots_in_stride {
            let object_address = root.get();

            if self.young.contains(object_address) {
                let dest = self.copy(object_address);
                root.set(dest);
            }
        }
    }

    fn visit_dirty_cards(&mut self) {
        self.visit_dirty_cards_in_old();
        self.visit_dirty_cards_in_large();
    }

    fn visit_dirty_cards_in_old(&mut self) {
        while let Some(stride) = self.next_card_stride() {
            self.visit_dirty_cards_in_stride(stride);
        }
    }

    fn next_card_stride(&mut self) -> Option<usize> {
        let stride = self.next_card_stride.fetch_add(1, Ordering::SeqCst);

        if stride < self.strides {
            Some(stride)
        } else {
            None
        }
    }

    fn visit_dirty_cards_in_stride(&mut self, stride: usize) {
        assert_eq!(self.old_region_start.len(), self.init_old_top.len());

        for (&region_start, &region_end) in self.old_region_start.iter().zip(self.init_old_top) {
            let region = Region::new(region_start, region_end);
            let (start_card_idx, end_card_idx) =
                self.card_table.card_indices(region.start, region.end);
            let cards_in_stride = (start_card_idx..end_card_idx)
                .skip(stride)
                .step_by(self.strides);

            for card_idx in cards_in_stride {
                let card_idx: CardIdx = card_idx.into();

                if self.card_table.get(card_idx).is_dirty() {
                    self.visit_dirty_card(card_idx, region);
                }
            }
        }
    }

    fn visit_dirty_cards_in_large(&mut self) {
        loop {
            if let Some(addr) = self.next_large() {
                let object = addr.to_mut_obj();

                if object.is_array_ref() {
                    self.visit_large_object_array(object, addr);
                } else {
                    self.visit_large_object(object, addr);
                }
            } else {
                break;
            }
        }
    }

    fn next_large(&mut self) -> Option<Address> {
        let mut next_large = self.next_large.lock();

        if next_large.is_null() {
            return None;
        }

        let large_alloc = LargeAlloc::from_address(*next_large);
        let object = large_alloc.object_address();

        *next_large = large_alloc.next;

        Some(object)
    }

    fn visit_large_object_array(&mut self, object: &mut Obj, object_start: Address) {
        let object_end = object_start.offset(object.size() as usize);
        let (start_card_idx, end_card_idx) = self.card_table.card_indices(object_start, object_end);

        for card_idx in start_card_idx..end_card_idx {
            let card_idx = card_idx.into();

            if self.card_table.get(card_idx).is_clean() {
                continue;
            }

            let card_start = self.card_table.to_address(card_idx);
            let card_end = card_start.offset(CARD_SIZE);

            let ref_start = object_start.offset(offset_of_array_data() as usize);
            let ref_start = cmp::max(ref_start, card_start);
            let ref_end = cmp::min(card_end, object_end);

            let mut ref_to_young_gen = false;
            self.copy_refs(ref_start, ref_end, &mut ref_to_young_gen);
            self.clean_card_if_no_young_refs(card_idx, ref_to_young_gen);
        }
    }

    fn visit_large_object(&mut self, object: &mut Obj, object_start: Address) {
        let card_idx = self.card_table.card_idx(object_start);
        let mut ref_to_young_gen = false;

        if self.card_table.get(card_idx).is_clean() {
            return;
        }

        object.visit_reference_fields(|field| {
            let field_ptr = field.get();

            if self.young.contains(field_ptr) {
                let copied_addr = self.copy(field_ptr);
                field.set(copied_addr);

                if self.young.contains(copied_addr) {
                    ref_to_young_gen = true;
                }
            }
        });

        self.clean_card_if_no_young_refs(card_idx, ref_to_young_gen);
    }

    fn visit_dirty_card(&mut self, card_idx: CardIdx, region: Region) {
        if card_idx == self.card_table.card_idx(region.start) {
            let first_object = region.start;
            let ref_to_young_gen = false;
            self.copy_old_card(card_idx, first_object, region, ref_to_young_gen);
            return;
        }

        let crossing_entry = self.crossing_map.get(card_idx);
        let card_start = self.card_table.to_address(card_idx);

        match crossing_entry {
            CrossingEntry::NoRefs => panic!("card dirty without any refs"),
            CrossingEntry::LeadingRefs(refs) => {
                let mut ref_to_young_gen = false;
                let first_object = card_start.add_ptr(refs as usize);

                // copy references at start of card
                let ref_start = cmp::max(card_start, region.start);
                let ref_end = cmp::min(first_object, region.end);
                self.copy_refs(ref_start, ref_end, &mut ref_to_young_gen);

                // copy all objects from this card
                self.copy_old_card(card_idx, first_object, region, ref_to_young_gen);
            }

            CrossingEntry::FirstObject(offset) => {
                let first_object = card_start.add_ptr(offset as usize);

                // copy all objects from this card
                self.copy_old_card(card_idx, first_object, region, false);
            }

            CrossingEntry::PreviousObjectWords(_) => unimplemented!(),
            CrossingEntry::PreviousObjectCards(_) => unimplemented!(),

            CrossingEntry::ArrayStart(offset) => {
                let first_object = card_start.sub_ptr(offset as usize);

                // copy all objects from this card
                self.copy_old_card(card_idx, first_object, region, false);
            }
        }
    }

    fn copy_refs(&mut self, start: Address, end: Address, ref_to_young_gen: &mut bool) {
        let mut ptr = start;

        while ptr < end {
            let slot = Slot::at(ptr);
            let obj = slot.get();

            if self.young.contains(obj) {
                let copied_obj = self.copy(obj);
                slot.set(copied_obj);

                if self.young.contains(copied_obj) {
                    *ref_to_young_gen = true;
                }
            }

            ptr = ptr.add_ptr(1);
        }
    }

    fn copy_old_card(
        &mut self,
        card: CardIdx,
        first_object: Address,
        region: Region,
        mut ref_to_young_gen: bool,
    ) {
        let card_start = self.card_table.to_address(card);
        let card_end = card_start.offset(CARD_SIZE);

        let range_start = cmp::max(first_object, region.start);
        let range_end = cmp::min(card_end, region.end);

        self.copy_range(range_start, range_end, &mut ref_to_young_gen);

        if self.is_card_cleaning_allowed(card, region) {
            self.clean_card_if_no_young_refs(card, ref_to_young_gen);
        }
    }

    fn is_card_cleaning_allowed(&self, card: CardIdx, region: Region) -> bool {
        // no cleaning for first card in region
        if card == self.card_table.card_idx(region.start) {
            // unless the first region is card aligned
            region.start.is_card_aligned()

        // no cleaning for last card in region
        } else if card == self.card_table.card_idx(region.end) {
            // if region.end is card aligned, we shouldn't encounter
            // this card for the current region.
            assert!(!region.end.is_card_aligned());
            false
        } else {
            true
        }
    }

    fn copy_range(
        &mut self,
        mut ptr: Address,
        end: Address,
        ref_to_young_gen: &mut bool,
    ) -> Address {
        while ptr < end {
            let object = ptr.to_mut_obj();

            if object.header().vtblptr().is_null() {
                ptr = ptr.add_ptr(1);
                continue;
            }

            let range = Region::new(ptr, end);
            object.visit_reference_fields_within(range, |field| {
                let field_ptr = field.get();

                if self.young.contains(field_ptr) {
                    let copied_obj = self.copy(field_ptr);
                    field.set(copied_obj);

                    // determine if copied object is still in young generation
                    if self.young.contains(copied_obj) {
                        *ref_to_young_gen = true;
                    }
                }
            });

            ptr = ptr.offset(object.size());
        }

        end
    }

    fn clean_card_if_no_young_refs(&mut self, card_idx: CardIdx, ref_to_young_gen: bool) {
        // if there are no references to the young generation in this card,
        // set the card to clean.
        if !ref_to_young_gen {
            self.card_table.set(card_idx, CardEntry::Clean);
        }
    }

    fn trace_gray_objects(&mut self) {
        loop {
            let object_addr = if let Some(object_addr) = self.pop() {
                object_addr
            } else if self.terminator.try_terminate() {
                break;
            } else {
                continue;
            };

            if self.young_region.contains(object_addr) {
                self.trace_young_object(object_addr);
            } else {
                self.trace_old_object(object_addr);
            }
        }

        self.young_lab.make_iterable_young(self.vm);
        self.old_lab.make_iterable_old(self.vm, self.old);
    }

    fn promotion_failed(&self) -> bool {
        self.promotion_failed
    }

    fn trace_young_object(&mut self, object_addr: Address) {
        let object = object_addr.to_mut_obj();

        object.visit_reference_fields(|slot| {
            let object_addr = slot.get();

            if self.young_region.contains(object_addr) {
                slot.set(self.copy(object_addr));
            }
        });
    }

    fn trace_old_object(&mut self, object_addr: Address) {
        let object = object_addr.to_mut_obj();

        if object.is_array_ref() {
            let mut ref_to_young_gen = false;
            let mut last = object_addr;

            object.visit_reference_fields(|slot| {
                let field_ptr = slot.get();

                if on_different_cards(last, slot.address()) && ref_to_young_gen {
                    let card_idx = self.card_table.card_idx(last);
                    self.card_table.set(card_idx, CardEntry::Dirty);
                    ref_to_young_gen = false;
                }

                if self.young.contains(field_ptr) {
                    let copied_addr = self.copy(field_ptr);
                    slot.set(copied_addr);

                    if self.young.contains(copied_addr) {
                        ref_to_young_gen = true;
                    }
                }

                last = slot.address();
            });

            if ref_to_young_gen {
                let card_idx = self.card_table.card_idx(last);
                self.card_table.set(card_idx, CardEntry::Dirty);
            }
        } else {
            let mut ref_to_young_gen = false;

            object.visit_reference_fields(|slot| {
                let field_ptr = slot.get();

                if self.young.contains(field_ptr) {
                    let copied_addr = self.copy(field_ptr);
                    slot.set(copied_addr);

                    if self.young.contains(copied_addr) {
                        ref_to_young_gen = true;
                    }
                }
            });

            if ref_to_young_gen {
                let card_idx = self.card_table.card_idx(object_addr);
                self.card_table.set(card_idx, CardEntry::Dirty);
            }
        }
    }

    fn alloc_young(&mut self, size: usize) -> Address {
        if size < CLAB_OBJECT_SIZE {
            self.alloc_young_small(size)
        } else {
            self.alloc_young_medium(size)
        }
    }

    fn alloc_young_small(&mut self, size: usize) -> Address {
        debug_assert!(size < CLAB_OBJECT_SIZE);
        let object_start = self.young_lab.alloc(size);

        if object_start.is_non_null() {
            return object_start;
        } else if self.copy_failed {
            return Address::null();
        }

        debug_assert!(size <= CLAB_SIZE);
        self.young_lab.make_iterable_young(self.vm);
        if !self.alloc_young_lab() {
            return Address::null();
        }

        self.young_lab.alloc(size)
    }

    fn alloc_young_medium(&mut self, size: usize) -> Address {
        debug_assert!(CLAB_OBJECT_SIZE <= size && size < LARGE_OBJECT_SIZE);

        if self.copy_failed {
            return Address::null();
        }

        let mut top = self.young_top.lock();

        let lab_start = *top;
        let lab_end = lab_start.offset(size);

        if lab_end <= self.young_limit {
            *top = lab_end;

            lab_start
        } else {
            self.copy_failed = true;

            Address::null()
        }
    }

    fn alloc_young_lab(&mut self) -> bool {
        if self.copy_failed {
            return false;
        }

        let mut top = self.young_top.lock();

        let lab_start = *top;
        let lab_end = lab_start.offset(CLAB_SIZE);

        if lab_end <= self.young_limit {
            *top = lab_end;
            self.young_lab.reset(lab_start, lab_end);

            true
        } else {
            self.copy_failed = true;
            self.young_lab.reset(Address::null(), Address::null());

            false
        }
    }

    fn undo_alloc_young(&mut self, size: usize) {
        if size < CLAB_OBJECT_SIZE {
            self.young_lab.undo_alloc(size)
        } else {
            // can't undo mid-sized objects
        }
    }

    fn alloc_old(&mut self, size: usize, array_ref: bool) -> Address {
        if size < CLAB_OBJECT_SIZE {
            self.alloc_old_small(size, array_ref)
        } else {
            self.alloc_old_medium(size, array_ref)
        }
    }

    fn alloc_old_small(&mut self, size: usize, array_ref: bool) -> Address {
        debug_assert!(size < CLAB_OBJECT_SIZE);
        let object_start = self.alloc_object_in_old_lab(size, array_ref);

        if object_start.is_non_null() {
            return object_start;
        } else if self.promotion_failed {
            return Address::null();
        }

        self.old_lab.make_iterable_old(self.vm, self.old);
        if !self.alloc_old_lab() {
            return Address::null();
        }

        self.alloc_object_in_old_lab(size, array_ref)
    }

    fn alloc_old_medium(&mut self, size: usize, array_ref: bool) -> Address {
        debug_assert!(CLAB_OBJECT_SIZE <= size && size < LARGE_OBJECT_SIZE);

        if self.promotion_failed {
            return Address::null();
        }

        let object_start = self.old.alloc(size);

        if object_start.is_non_null() {
            let old = object_start;
            let new = old.offset(size);
            self.old.update_crossing(old, new, array_ref);
            object_start
        } else {
            self.promotion_failed = true;
            Address::null()
        }
    }

    fn undo_alloc_old(&mut self, size: usize) {
        if size < CLAB_OBJECT_SIZE {
            self.old_lab.undo_alloc(size);
        } else {
            // can't undo mid-sized objects
        }
    }

    fn alloc_old_lab(&mut self) -> bool {
        if self.promotion_failed {
            return false;
        }

        let lab_start = self.old.alloc(CLAB_SIZE);

        if lab_start.is_non_null() {
            let lab_end = lab_start.offset(CLAB_SIZE);
            self.old_lab.reset(lab_start, lab_end);

            true
        } else {
            self.promotion_failed = true;

            false
        }
    }

    fn alloc_object_in_old_lab(&mut self, size: usize, array_ref: bool) -> Address {
        let object_start = self.old_lab.alloc(size);

        if object_start.is_non_null() {
            let old = object_start;
            let new = old.offset(size);
            self.old.update_crossing(old, new, array_ref);
            object_start
        } else {
            Address::null()
        }
    }

    fn copy(&mut self, obj_addr: Address) -> Address {
        let obj = obj_addr.to_mut_obj();

        // Check if object was already copied
        let vtblptr = match obj.header().vtblptr_forwarded_atomic() {
            Ok(fwd_addr) => {
                return fwd_addr;
            }

            Err(vtblptr) => vtblptr,
        };

        // As soon as promotion of an object failed, objects are not copied anymore.
        if self.promotion_failed {
            return obj_addr;
        }

        let obj_size = obj.size_for_vtblptr(vtblptr);

        debug_assert!(
            self.from_active.contains(obj_addr) || self.eden_active.contains(obj_addr),
            "copy objects only from from-space."
        );

        // If object is old enough we copy it into the old generation
        if self.copy_failed || self.young.should_be_promoted(obj_addr) {
            return self.promote_object(vtblptr, obj, obj_size);
        }

        // Try to allocate memory in to-space.
        let copy_addr = self.alloc_young(obj_size);

        // Couldn't allocate object in young generation, try to promote
        // object into old generation instead.
        if copy_addr.is_null() {
            return self.promote_object(vtblptr, obj, obj_size);
        }

        obj.copy_to(copy_addr, obj_size);
        let res = obj.header_mut().vtblptr_forward_atomic(vtblptr, copy_addr);

        match res {
            Ok(()) => {
                self.push(copy_addr);
                copy_addr
            }

            Err(fwdptr) => {
                self.undo_alloc_young(obj_size);
                fwdptr
            }
        }
    }

    fn promote_object(&mut self, vtblptr: Address, obj: &mut Obj, obj_size: usize) -> Address {
        let array_ref = unsafe { &*vtblptr.to_mut_ptr::<VTable>() }.is_array_ref();
        let copy_addr = self.alloc_old(obj_size, array_ref);

        // if there isn't enough space in old gen keep it in the
        // young generation for now. A full collection will be forced later and
        // cleans this up.
        if copy_addr.is_null() {
            let res = obj.header_mut().vtblptr_forward_failure_atomic(vtblptr);

            return match res {
                Ok(()) => obj.address(),
                Err(fwdptr) => fwdptr,
            };
        }

        obj.copy_to(copy_addr, obj_size);
        let res = obj.header_mut().vtblptr_forward_atomic(vtblptr, copy_addr);

        match res {
            Ok(()) => {
                self.promoted_size += obj_size;
                self.push(copy_addr);

                copy_addr
            }

            Err(fwdptr) => {
                self.undo_alloc_old(obj_size);

                fwdptr
            }
        }
    }

    fn push(&mut self, addr: Address) {
        if self.local.len() < LOCAL_MAXIMUM {
            self.local.push(addr);
            self.defensive_push();
        } else {
            self.worker.push(addr);
        }
    }

    fn defensive_push(&mut self) {
        self.traced += 1;

        if self.traced > 256 {
            if self.local.len() > 4 {
                let target_len = self.local.len() / 2;

                while self.local.len() > target_len {
                    let val = self.local.pop().unwrap();
                    self.injector.push(val);
                }
            }

            self.traced = 0;
        }
    }

    fn pop(&mut self) -> Option<Address> {
        self.pop_local()
            .or_else(|| self.pop_worker())
            .or_else(|| self.pop_global())
            .or_else(|| self.steal())
    }

    fn pop_local(&mut self) -> Option<Address> {
        if self.local.is_empty() {
            return None;
        }

        let obj = self.local.pop().expect("should be non-empty");
        Some(obj)
    }

    fn pop_worker(&mut self) -> Option<Address> {
        self.worker.pop()
    }

    fn pop_global(&mut self) -> Option<Address> {
        loop {
            let result = self.injector.steal_batch_and_pop(&mut self.worker);

            match result {
                Steal::Empty => break,
                Steal::Success(value) => return Some(value),
                Steal::Retry => continue,
            }
        }

        None
    }

    fn steal(&self) -> Option<Address> {
        if self.stealers.len() == 1 {
            return None;
        }

        let mut rng = thread_rng();
        let range = Uniform::new(0, self.stealers.len());

        for _ in 0..2 * self.stealers.len() {
            let mut stealer_id = self.task_id;

            while stealer_id == self.task_id {
                stealer_id = range.sample(&mut rng);
            }

            let stealer = &self.stealers[stealer_id];

            loop {
                match stealer.steal_batch_and_pop(&self.worker) {
                    Steal::Empty => break,
                    Steal::Success(address) => return Some(address),
                    Steal::Retry => continue,
                }
            }
        }

        None
    }
}
