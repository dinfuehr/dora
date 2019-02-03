use parking_lot::Mutex;
use std::cmp;
use std::sync::{Arc, Barrier};

use ctxt::VM;
use gc::root::Slot;
use gc::swiper::card::{CardEntry, CardTable};
use gc::swiper::controller::SharedHeapConfig;
use gc::swiper::crossing::{CrossingEntry, CrossingMap};
use gc::swiper::large::{LargeAlloc, LargeSpace};
use gc::swiper::marking::Terminator;
use gc::swiper::old::OldGen;
use gc::swiper::on_different_cards;
use gc::swiper::young::YoungGen;
use gc::swiper::{CardIdx, CARD_SIZE, LARGE_OBJECT_SIZE};
use gc::tlab::{TLAB_OBJECT_SIZE, TLAB_SIZE};
use gc::{fill_region, Address, GcReason, Region};
use mem;
use object::Obj;
use vtable::VTable;

use crossbeam_deque::{self as deque, Pop, Steal, Stealer, Worker};
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
            vm: vm,
            young: young,
            old: old,
            large: large,
            rootset: rootset,
            card_table: card_table,
            crossing_map: crossing_map,

            young_top: Address::null(),
            young_limit: Address::null(),
            init_old_top: Vec::new(),

            promotion_failed: false,
            promoted_size: 0,

            from_active: young.from_active(),
            eden_active: young.eden_active(),

            reason: reason,

            min_heap_size: min_heap_size,
            max_heap_size: max_heap_size,

            number_workers: threadpool.thread_count() as usize,
            threadpool: threadpool,

            worklist: Vec::new(),
            config: config,
        }
    }

    pub fn collect(&mut self) -> bool {
        let to_committed = self.young.to_committed();
        self.young_top = to_committed.start;
        self.young_limit = to_committed.end;

        self.init_old_top = {
            let protected = self.old.protected();
            protected.regions.iter().map(|r| r.top()).collect()
        };

        self.young.unprotect_to();

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
            self.young.swap_semi_and_keep_to_space(self.young_top);

            return true;
        }

        self.young.clear_eden();
        self.young.swap_semi(self.young_top);
        self.young.protect_to();

        assert!(self.young.eden_active().size() == 0);
        assert!(self.young.to_active().size() == 0);

        let mut config = self.config.lock();
        config.minor_promoted = self.promoted_size;
        config.minor_copied = self.young.from_active().size();

        self.promotion_failed
    }

    fn run_threads(&mut self) {
        let mut workers = Vec::with_capacity(self.number_workers);
        let mut stealers = Vec::with_capacity(self.number_workers);

        for _ in 0..self.number_workers {
            let (w, s) = deque::lifo();
            workers.push(w);
            stealers.push(s);
        }

        let worklist = std::mem::replace(&mut self.worklist, Vec::new());

        for (id, object) in worklist.into_iter().enumerate() {
            workers[id % self.number_workers].push(object);
        }

        let terminator = Arc::new(Terminator::new(self.number_workers));
        let number_workers = self.number_workers;
        let young_region = self.young.total();
        let vm = self.vm;

        // align old generation to card boundary
        let young_top = Arc::new(Mutex::new(self.young_top));
        let young_limit = self.young_limit;

        let card_table = self.card_table;
        let crossing_map = self.crossing_map;
        let young = self.young;
        let old = self.old;
        let large = self.large;
        let rootset = self.rootset;
        let init_old_top = &self.init_old_top;
        let barrier = Barrier::new(self.number_workers);
        let barrier = &barrier;

        let promoted_size = Mutex::new(self.promoted_size);
        let promoted_size = &promoted_size;

        let promotion_failed = Mutex::new(self.promotion_failed);
        let promotion_failed = &promotion_failed;

        let next_stride = Mutex::new(0);
        let next_stride = &next_stride;
        let strides = 4 * self.number_workers;

        let head = self.large.head();
        let next_large = Mutex::new(head);
        let next_large = &next_large;

        self.threadpool.scoped(|scoped| {
            for (task_id, worker) in workers.into_iter().enumerate() {
                let stealers = stealers.clone();
                let terminator = terminator.clone();
                let young_region = young_region.clone();
                let young_top = young_top.clone();
                let promoted_size = promoted_size.clone();
                let promotion_failed = promotion_failed.clone();

                scoped.execute(move || {
                    let mut task = CopyTask {
                        task_id: task_id,
                        local: Vec::new(),
                        worker: worker,
                        stealers: stealers,
                        terminator: terminator,
                        number_workers: number_workers,

                        vm: vm,
                        young: young,
                        old: old,
                        large: large,
                        young_region: young_region,
                        card_table: card_table,
                        crossing_map: crossing_map,
                        rootset: rootset,
                        init_old_top: init_old_top,
                        barrier: barrier,

                        from_active: young.from_active(),
                        eden_active: young.eden_active(),

                        next_stride: next_stride,
                        strides: strides,
                        next_large: next_large,

                        promoted_size: 0,
                        traced: 0,

                        old_lab: Lab::new(),
                        old_alloc_failed: false,

                        young_lab: Lab::new(),
                        young_alloc: SpaceAlloc::new(young_top, young_limit),
                    };

                    task.run();

                    *promoted_size.lock() += task.promoted_size;

                    if task.promotion_failed() {
                        let mut promotion_failed = promotion_failed.lock();
                        *promotion_failed = true;
                    }
                });
            }
        });

        self.young_top = *young_top.lock();

        self.promoted_size = *promoted_size.lock();
        self.promotion_failed = *promotion_failed.lock();
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

struct SpaceAlloc {
    top: Arc<Mutex<Address>>,
    limit: Address,
    failed: bool,
}

impl SpaceAlloc {
    fn new(top: Arc<Mutex<Address>>, limit: Address) -> SpaceAlloc {
        SpaceAlloc {
            top: top,
            limit: limit,
            failed: false,
        }
    }

    fn alloc_lab_young(&mut self, lab: &mut Lab) -> bool {
        if self.failed {
            lab.reset(Address::null(), Address::null());
            return false;
        }

        let mut top = self.top.lock();

        let lab_start = *top;
        let lab_end = lab_start.offset(CLAB_SIZE);

        if lab_end <= self.limit {
            *top = lab_end;
            lab.reset(lab_start, lab_end);

            true
        } else {
            self.failed = true;
            lab.reset(Address::null(), Address::null());

            false
        }
    }

    fn alloc_lab_old(&mut self, lab: &mut Lab, old: &OldGen) -> bool {
        if self.failed {
            lab.reset(Address::null(), Address::null());
            return false;
        }

        let lab_start = old.alloc(CLAB_SIZE);

        if lab_start.is_non_null() {
            let lab_end = lab_start.offset(CLAB_SIZE);
            lab.reset(lab_start, lab_end);

            true
        } else {
            self.failed = true;
            lab.reset(Address::null(), Address::null());

            false
        }
    }

    fn alloc_obj_young(&mut self, size: usize) -> Address {
        if self.failed {
            return Address::null();
        }

        let mut top = self.top.lock();

        let obj_start = *top;
        let obj_end = obj_start.offset(size);

        if obj_end <= self.limit {
            *top = obj_end;

            obj_start
        } else {
            self.failed = true;
            Address::null()
        }
    }

    fn alloc_obj_old(&mut self, size: usize, old: &OldGen) -> Address {
        if self.failed {
            return Address::null();
        }

        let obj_start = old.alloc(size);

        if obj_start.is_null() {
            self.failed = true;
        }

        obj_start
    }
}

const CLAB_SIZE: usize = TLAB_SIZE;
const CLAB_OBJECT_SIZE: usize = TLAB_OBJECT_SIZE;

const LOCAL_MAXIMUM: usize = 64;

struct CopyTask<'a, 'ast: 'a> {
    task_id: usize,
    local: Vec<Address>,
    worker: Worker<Address>,
    stealers: Vec<Stealer<Address>>,
    terminator: Arc<Terminator>,
    number_workers: usize,

    vm: &'a VM<'ast>,
    young: &'a YoungGen,
    old: &'a OldGen,
    large: &'a LargeSpace,
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
    rootset: &'a [Slot],
    init_old_top: &'a [Address],
    barrier: &'a Barrier,

    next_stride: &'a Mutex<usize>,
    strides: usize,
    next_large: &'a Mutex<Address>,

    young_region: Region,
    from_active: Region,
    eden_active: Region,

    promoted_size: usize,
    traced: usize,

    old_lab: Lab,
    old_alloc_failed: bool,

    young_lab: Lab,
    young_alloc: SpaceAlloc,
}

impl<'a, 'ast> CopyTask<'a, 'ast>
where
    'ast: 'a,
{
    fn run(&mut self) {
        self.visit_roots();
        self.visit_dirty_cards();
        self.trace_gray_objects();
    }

    fn visit_roots(&mut self) {
        let rootset_for_thread = self
            .rootset
            .iter()
            .skip(self.task_id)
            .step_by(self.number_workers);

        for root in rootset_for_thread {
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
        loop {
            if let Some(stride) = self.next_stride() {
                self.visit_dirty_cards_in_stride(stride);
            } else {
                break;
            }
        }
    }

    fn next_stride(&mut self) -> Option<usize> {
        let mut next_stride = self.next_stride.lock();
        let stride = *next_stride;

        if stride >= self.strides {
            return None;
        }

        *next_stride += 1;
        Some(stride)
    }

    fn visit_dirty_cards_in_stride(&mut self, stride: usize) {
        let init_old_top = *self.init_old_top.first().unwrap();
        let (start_card_idx, end_card_idx) = self
            .card_table
            .card_indices(self.old.total().start, init_old_top);
        let dirty_cards_in_stride = (start_card_idx..=end_card_idx)
            .skip(stride)
            .step_by(self.strides);

        for card_idx in dirty_cards_in_stride {
            let card_idx: CardIdx = card_idx.into();

            if self.card_table.get(card_idx).is_dirty() {
                self.visit_dirty_card(card_idx);
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

        for card_idx in start_card_idx..=end_card_idx {
            let card_idx = card_idx.into();

            if self.card_table.get(card_idx).is_clean() {
                continue;
            }

            let card_start = self.card_table.to_address(card_idx);
            let card_end = card_start.offset(CARD_SIZE);
            let end = cmp::min(card_end, object_end);

            let mut ref_to_young_gen = false;

            if card_idx.to_usize() == start_card_idx {
                self.copy_range(object_start, end, &mut ref_to_young_gen);
            } else {
                // all but the first card are full with references
                let refs = end.offset_from(card_start) / mem::ptr_width_usize();
                self.copy_refs(card_start, refs, &mut ref_to_young_gen);
            }

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

    fn visit_dirty_card(&mut self, card_idx: CardIdx) {
        let crossing_entry = self.crossing_map.get(card_idx);
        let card_start = self.card_table.to_address(card_idx);

        match crossing_entry {
            CrossingEntry::NoRefs => panic!("card dirty without any refs"),
            CrossingEntry::LeadingRefs(refs) => {
                let mut ref_to_young_gen = false;

                // copy references at start of card
                let first_object = self.copy_refs(card_start, refs as usize, &mut ref_to_young_gen);

                // copy all objects from this card
                self.copy_old_card(card_idx, first_object, ref_to_young_gen);
            }

            CrossingEntry::FirstObject(offset) => {
                let ptr = card_start.offset(offset as usize * mem::ptr_width_usize());

                // copy all objects from this card
                self.copy_old_card(card_idx, ptr, false);
            }

            CrossingEntry::ArrayStart(offset) => {
                assert!(offset == 1);
                let ptr = card_start.to_usize() - (offset as usize * mem::ptr_width_usize());

                // copy all objects from this card
                self.copy_old_card(card_idx, ptr.into(), false);
            }
        }
    }

    fn copy_refs(&mut self, mut ptr: Address, refs: usize, ref_to_young_gen: &mut bool) -> Address {
        for _ in 0..refs {
            let slot = Slot::at(ptr);
            let dir_ptr = slot.get();

            if self.young.contains(dir_ptr) {
                let copied_obj = self.copy(dir_ptr);
                slot.set(copied_obj);

                if self.young.contains(copied_obj) {
                    *ref_to_young_gen = true;
                }
            }

            ptr = ptr.offset(mem::ptr_width_usize());
        }

        ptr
    }

    fn copy_old_card(&mut self, card: CardIdx, ptr: Address, mut ref_to_young_gen: bool) {
        let card_start = self.card_table.to_address(card);
        let card_end = card_start.offset(CARD_SIZE);
        let init_old_top = *self.init_old_top.first().unwrap();
        let end = cmp::min(card_end, init_old_top);

        self.copy_range(ptr, end, &mut ref_to_young_gen);
        self.clean_card_if_no_young_refs(card, ref_to_young_gen);
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

            object.visit_reference_fields_within(end, |field| {
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
        self.barrier.wait();

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
        self.old_alloc_failed
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
        } else if self.young_alloc.failed {
            return Address::null();
        }

        debug_assert!(size <= CLAB_SIZE);
        self.young_lab.make_iterable_young(self.vm);
        if !self.young_alloc.alloc_lab_young(&mut self.young_lab) {
            return Address::null();
        }

        self.young_lab.alloc(size)
    }

    fn alloc_young_medium(&mut self, size: usize) -> Address {
        debug_assert!(CLAB_OBJECT_SIZE <= size && size < LARGE_OBJECT_SIZE);
        self.young_alloc.alloc_obj_young(size)
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
        } else if self.old_alloc_failed {
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

        if self.old_alloc_failed {
            return Address::null();
        }

        let object_start = self.old.alloc(size);

        if object_start.is_non_null() {
            let old = object_start;
            let new = old.offset(size);
            self.old.update_crossing(old, new, array_ref);
            object_start
        } else {
            self.old_alloc_failed = true;
            Address::null()
        }
    }

    fn alloc_old_lab(&mut self) -> bool {
        if self.old_alloc_failed {
            return false;
        }

        let lab_start = self.old.alloc(CLAB_SIZE);

        if lab_start.is_non_null() {
            let lab_end = lab_start.offset(CLAB_SIZE);
            self.old_lab.reset(lab_start, lab_end);

            true
        } else {
            self.old_alloc_failed = true;

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
        if self.old_alloc_failed {
            return obj_addr;
        }

        let obj_size = obj.size_for_vtblptr(vtblptr);

        debug_assert!(
            self.from_active.contains(obj_addr) || self.eden_active.contains(obj_addr),
            "copy objects only from from-space."
        );

        // If object is old enough we copy it into the old generation
        if self.young.should_be_promoted(obj_addr) {
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
                self.young_lab.undo_alloc(obj_size);
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
                self.old_lab.undo_alloc(obj_size);

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
                    self.worker.push(val);
                }
            }

            self.traced = 0;
        }
    }

    fn pop(&mut self) -> Option<Address> {
        self.pop_local()
            .or_else(|| self.pop_worker())
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
        loop {
            match self.worker.pop() {
                Pop::Empty => break,
                Pop::Data(address) => return Some(address),
                Pop::Retry => continue,
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
                match stealer.steal() {
                    Steal::Empty => break,
                    Steal::Data(address) => return Some(address),
                    Steal::Retry => continue,
                }
            }
        }

        None
    }
}
