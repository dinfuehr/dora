use parking_lot::Mutex;
use std::cmp;
use std::sync::Arc;

use ctxt::VM;
use gc::root::Slot;
use gc::swiper::card::{CardEntry, CardTable};
use gc::swiper::controller;
use gc::swiper::crossing::{CrossingEntry, CrossingMap};
use gc::swiper::large::LargeSpace;
use gc::swiper::marking::Terminator;
use gc::swiper::old::OldGen;
use gc::swiper::on_different_cards;
use gc::swiper::young::YoungGen;
use gc::swiper::GcStats;
use gc::swiper::{CardIdx, CARD_SIZE};
use gc::{fill_region, formatted_size, Address, GcReason, Region};
use mem;
use object::Obj;
use timer::Timer;

use crossbeam_deque::{self as deque, Pop, Steal, Stealer, Worker};
use rand::distributions::{Distribution, Uniform};
use rand::thread_rng;
use scoped_threadpool::Pool;

pub struct MinorCollector<'a, 'ast: 'a> {
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
    old_end: Address,

    promotion_failed: bool,
    promoted_size: usize,

    from_active: Region,
    eden_active: Region,

    min_heap_size: usize,
    max_heap_size: usize,

    stats: &'a GcStats,

    parallel: bool,
    threadpool: &'a mut Pool,
    number_workers: usize,
    worklist: Vec<Address>,
}

impl<'a, 'ast: 'a> MinorCollector<'a, 'ast> {
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
        stats: &'a GcStats,
        threadpool: &'a mut Pool,
    ) -> MinorCollector<'a, 'ast> {
        MinorCollector {
            vm: vm,
            young: young,
            old: old,
            large: large,
            rootset: rootset,
            card_table: card_table,
            crossing_map: crossing_map,

            young_top: Address::null(),
            young_limit: Address::null(),
            old_end: Address::null(),

            promotion_failed: false,
            promoted_size: 0,

            from_active: young.from_active(),
            eden_active: young.eden_active(),

            reason: reason,

            min_heap_size: min_heap_size,
            max_heap_size: max_heap_size,

            stats: stats,

            parallel: vm.args.flag_gc_parallel_minor,
            number_workers: vm.args.flag_gc_worker,
            threadpool: threadpool,

            worklist: Vec::new(),
        }
    }

    pub fn collect(&mut self) -> bool {
        let active = self.vm.args.flag_gc_verbose;
        let timer = Timer::new(active);

        let init_size = self.heap_size();
        let young_init_size = self.young.active_size();

        let to_committed = self.young.to_committed();
        self.young_top = to_committed.start;
        self.young_limit = to_committed.end;
        self.old_end = self.old.align_to_card(self.vm);

        self.young.unprotect_to();

        let dev_verbose = self.vm.args.flag_gc_dev_verbose;

        if dev_verbose {
            println!("Minor GC: Phase 1 (roots)");
        }
        let time_roots = Timer::ms(active, || {
            self.visit_roots();
        });

        if dev_verbose {
            println!("Minor GC: Phase 2 (dirty cards)");
        }
        let time_dirty_cards = Timer::ms(active, || {
            self.copy_dirty_cards();
            self.visit_large_objects();
        });

        if dev_verbose {
            println!("Minor GC: Phase 3 (traverse)");
        }
        let time_traverse = Timer::ms(active, || {
            self.trace_objects();
        });

        if dev_verbose {
            println!("Minor GC: Phase 3 (traverse) finished.");
        }

        if self.promotion_failed {
            // oh no: promotion failed, we need a subsequent full GC
            self.remove_forwarding_pointers();
            self.young.swap_semi_and_keep_to_space(self.young_top);
        } else {
            self.young.clear_eden();
            self.young.swap_semi(self.young_top);
            self.young.protect_to();

            assert!(self.young.eden_active().size() == 0);
            assert!(self.young.to_active().size() == 0);
        }

        let force_full = if self.vm.args.flag_gc_young_ratio.is_none() {
            controller::resize_gens_after_minor(
                self.min_heap_size,
                self.max_heap_size,
                self.young,
                self.old,
                self.vm.args.flag_gc_verbose,
            )
        } else {
            false
        };

        timer.stop_with(|time_pause| {
            let new_size = self.heap_size();
            let young_new_size = self.young.active_size();
            let garbage = saturating_sub(
                saturating_sub(young_init_size, young_new_size),
                self.promoted_size,
            );
            let garbage_ratio = if young_init_size == 0 {
                0f64
            } else {
                (garbage as f64 / young_init_size as f64) * 100f64
            };

            self.stats.update(|stats| {
                stats.incr_minor_collections();
                stats.incr_minor_runtime(time_pause);
            });

            println!(
                "GC: Minor GC ({:.1} ms, {}->{}, young {}->{}, \
                 {} promoted, {}/{:.0}% garbage); \
                 root={:.1}ms dirty_cards={:.1}ms traverse={:.1}ms; ({})",
                time_pause,
                formatted_size(init_size),
                formatted_size(new_size),
                formatted_size(young_init_size),
                formatted_size(young_new_size),
                formatted_size(self.promoted_size),
                formatted_size(garbage),
                garbage_ratio,
                time_roots,
                time_dirty_cards,
                time_traverse,
                self.reason,
            );
        });

        force_full || self.promotion_failed
    }

    fn heap_size(&self) -> usize {
        self.young.active_size() + self.old.active_size()
    }

    fn visit_roots(&mut self) {
        // detect all references from roots into young generation
        for &root in self.rootset {
            let root_ptr = root.get();

            if self.young.contains(root_ptr) {
                root.set(self.copy(root_ptr));
            }
        }
    }

    fn visit_large_objects(&mut self) {
        self.large.visit_objects(|addr| {
            let object = addr.to_mut_obj();

            if object.is_array_ref() {
                self.visit_large_object_array(object, addr);
            } else {
                self.visit_large_object(object, addr);
            }
        })
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
            let end = cmp::min(card_end, object_end);

            let mut ref_to_young_gen = false;

            if card_idx.to_usize() == start_card_idx {
                self.copy_range(object_start, end, &mut ref_to_young_gen);
            } else {
                // all but the first card are full with references
                let refs = end.offset_from(card_start) / mem::ptr_width_usize();
                self.copy_refs(card_start, refs, &mut ref_to_young_gen);
            }

            self.update_card(card_idx, ref_to_young_gen);
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

        self.update_card(card_idx, ref_to_young_gen);
    }

    fn trace_objects(&mut self) {
        if self.parallel {
            self.trace_gray_objects_par();
        } else {
            self.trace_gray_objects_seq();
        }
    }

    fn trace_gray_objects_seq(&mut self) {
        let mut young_scan = self.young.to_committed().start;
        let mut old_scan = self.old_end;

        // visit all fields in gray (=copied) objects
        // there can be gray objects in old & young gen
        while young_scan < self.young_top || old_scan < self.old.top() {
            while young_scan < self.young_top {
                young_scan = self.trace_young_object(young_scan);
            }

            while old_scan < self.old.top() {
                old_scan = self.trace_old_object(old_scan);
            }
        }

        assert!(young_scan == self.young_top);
        assert!(old_scan == self.old.top());
    }

    fn trace_gray_objects_par(&mut self) {
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
        let young_region = self.young.total();
        let vm = self.vm;

        // align old generation to card boundary
        let old_top = self.old.align_to_card(self.vm);

        let old_top = Arc::new(Mutex::new(old_top));
        let old_limit = self.old.committed().end;

        let young_top = Arc::new(Mutex::new(self.young_top));
        let young_limit = self.young_limit;

        let card_table = self.card_table;
        let crossing_map = self.crossing_map;
        let young = self.young;
        let old = self.old;

        self.threadpool.scoped(|scoped| {
            for (task_id, worker) in workers.into_iter().enumerate() {
                let stealers = stealers.clone();
                let terminator = terminator.clone();
                let young_region = young_region.clone();
                let old_top = old_top.clone();
                let young_top = young_top.clone();

                scoped.execute(move || {
                    let mut task = CopyTask {
                        task_id: task_id,
                        local: Vec::new(),
                        worker: worker,
                        stealers: stealers,
                        terminator: terminator,

                        vm: vm,
                        young: young,
                        old: old,
                        young_region: young_region,
                        card_table: card_table,
                        crossing_map: crossing_map,

                        from_active: young.from_active(),
                        eden_active: young.eden_active(),

                        promoted_size: 0,

                        old_lab: Lab::new(),
                        old_alloc: LabAlloc::new(old_top, old_limit),

                        young_lab: Lab::new(),
                        young_alloc: LabAlloc::new(young_top, young_limit),
                    };

                    task.run();
                });
            }
        });

        self.young_top = *young_top.lock();
        self.old.update_top(*old_top.lock());
    }

    fn trace_young_object(&mut self, addr: Address) -> Address {
        let object = addr.to_mut_obj();

        object.visit_reference_fields(|field| {
            let field_ptr = field.get();

            if self.young.contains(field_ptr) {
                field.set(self.copy(field_ptr));
            }
        });

        addr.offset(object.size())
    }

    fn trace_old_object(&mut self, object_start: Address) -> Address {
        let object = object_start.to_mut_obj();

        if object.is_array_ref() {
            let mut ref_to_young_gen = false;
            let mut last = object_start;

            object.visit_reference_fields(|field| {
                let field_ptr = field.get();

                if on_different_cards(last, field.address()) && ref_to_young_gen {
                    let card_idx = self.card_table.card_idx(last);
                    self.card_table.set(card_idx, CardEntry::Dirty);
                    ref_to_young_gen = false;
                }

                if self.young.contains(field_ptr) {
                    let copied_addr = self.copy(field_ptr);
                    field.set(copied_addr);

                    if self.young.contains(copied_addr) {
                        ref_to_young_gen = true;
                    }
                }

                last = field.address();
            });

            if ref_to_young_gen {
                let card_idx = self.card_table.card_idx(last);
                self.card_table.set(card_idx, CardEntry::Dirty);
            }
        } else {
            let mut ref_to_young_gen = false;

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

            if ref_to_young_gen {
                let card_idx = self.card_table.card_idx(object_start);
                self.card_table.set(card_idx, CardEntry::Dirty);
            }
        }

        object_start.offset(object.size())
    }

    // copy all references from old- into young-generation.
    fn copy_dirty_cards(&mut self) {
        self.card_table
            .visit_dirty_in_old(self.old_end, |card_idx| {
                let crossing_entry = self.crossing_map.get(card_idx);
                let card_start = self.card_table.to_address(card_idx);

                match crossing_entry {
                    CrossingEntry::NoRefs => panic!("card dirty without any refs"),
                    CrossingEntry::LeadingRefs(refs) => {
                        let mut ref_to_young_gen = false;

                        // copy references at start of card
                        let first_object =
                            self.copy_refs(card_start, refs as usize, &mut ref_to_young_gen);

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
                        let ptr =
                            card_start.to_usize() - (offset as usize * mem::ptr_width_usize());

                        // copy all objects from this card
                        self.copy_old_card(card_idx, ptr.into(), false);
                    }
                }
            });
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

    fn copy_old_card(&mut self, card: CardIdx, mut ptr: Address, mut ref_to_young_gen: bool) {
        let card_start = self.card_table.to_address(card);
        let card_end = card_start.offset(CARD_SIZE);

        let old_end: Address = self.old.top();
        let mut end = cmp::min(card_end, old_end);

        loop {
            ptr = self.copy_range(ptr, end, &mut ref_to_young_gen);

            // if we are in the last card of the old generation, promoted objects
            // will increase `end` towards `card_end`. Those newly promoted objects
            // need also be handled.
            if end == card_end {
                break;
            }

            let old_end = self.old.top();
            let next_end = cmp::min(card_end, old_end);

            if end != next_end {
                end = next_end;
            } else {
                break;
            }
        }

        self.update_card(card, ref_to_young_gen);
    }

    fn copy_range(
        &mut self,
        mut ptr: Address,
        end: Address,
        ref_to_young_gen: &mut bool,
    ) -> Address {
        while ptr < end {
            let object = ptr.to_mut_obj();

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

    fn update_card(&mut self, card_idx: CardIdx, ref_to_young_gen: bool) {
        // if there are no references to the young generation in this card,
        // set the card to clean.
        if !ref_to_young_gen {
            self.card_table.set(card_idx, CardEntry::Clean);
        }
    }

    fn copy(&mut self, obj_addr: Address) -> Address {
        let obj = obj_addr.to_mut_obj();

        if let Some(fwd) = obj.header().vtblptr_forwarded() {
            return fwd;
        }

        // As soon as promotion of an object failed, objects are not copied anymore.
        if self.promotion_failed {
            return obj_addr;
        }

        let obj_size = obj.size();
        debug_assert!(
            self.from_active.contains(obj_addr) || self.eden_active.contains(obj_addr),
            "copy objects only from from-space."
        );

        let copy_addr = self.young_top;
        let next_young_top = copy_addr.offset(obj_size);

        // if object is old enough we copy it into the old generation
        if self.young.should_be_promoted(obj_addr) || next_young_top > self.young_limit {
            return self.promote_object(obj, obj_size);
        }

        assert!(next_young_top <= self.young_limit);

        // When doing parallel minor collection, keep track of all objects to process in worklist.
        if self.parallel {
            self.worklist.push(copy_addr);
        }

        self.young_top = next_young_top;
        debug_assert!(self.young.to_committed().valid_top(self.young_top));

        obj.copy_to(copy_addr, obj_size);
        obj.header_mut().vtblptr_forward(copy_addr);

        copy_addr
    }

    fn promote_object(&mut self, obj: &mut Obj, obj_size: usize) -> Address {
        let copy_addr = self.old.bump_alloc(obj_size, obj.is_array_ref());

        // if there isn't enough space in old gen keep it in the
        // young generation for now. A full collection will be forced later and
        // cleans this up.
        if copy_addr.is_null() {
            self.promotion_failed = true;
            return obj.address();
        }

        // When doing parallel minor collection, keep track of all objects to process in worklist.
        if self.parallel {
            self.worklist.push(copy_addr);
        }

        obj.copy_to(copy_addr, obj_size);
        self.promoted_size += obj_size;

        obj.header_mut().vtblptr_forward(copy_addr);

        copy_addr
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

fn saturating_sub(lhs: usize, rhs: usize) -> usize {
    if lhs > rhs {
        lhs - rhs
    } else {
        0
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

    fn make_iterable(&mut self, vm: &VM) {
        fill_region(vm, self.top, self.limit);

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

struct LabAlloc {
    top: Arc<Mutex<Address>>,
    limit: Address,
    failed: bool,
}

impl LabAlloc {
    fn new(top: Arc<Mutex<Address>>, limit: Address) -> LabAlloc {
        LabAlloc {
            top: top,
            limit: limit,
            failed: false,
        }
    }

    fn alloc(&mut self, lab: &mut Lab) -> bool {
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
}

const CLAB_SIZE: usize = 32 * 1024;
const LOCAL_MAXIMUM: usize = 64;

struct CopyTask<'a, 'ast: 'a> {
    task_id: usize,
    local: Vec<Address>,
    worker: Worker<Address>,
    stealers: Vec<Stealer<Address>>,
    terminator: Arc<Terminator>,

    vm: &'a VM<'ast>,
    young: &'a YoungGen,
    old: &'a OldGen,
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,

    young_region: Region,
    from_active: Region,
    eden_active: Region,

    promoted_size: usize,

    old_lab: Lab,
    old_alloc: LabAlloc,

    young_lab: Lab,
    young_alloc: LabAlloc,
}

impl<'a, 'ast> CopyTask<'a, 'ast>
where
    'ast: 'a,
{
    fn run(&mut self) {
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
        let object_start = self.young_lab.alloc(size);

        if object_start.is_non_null() {
            return object_start;
        } else if self.young_alloc.failed {
            return Address::null();
        }

        debug_assert!(size <= CLAB_SIZE);
        self.young_lab.make_iterable(self.vm);
        if !self.young_alloc.alloc(&mut self.young_lab) {
            return Address::null();
        }

        self.young_lab.alloc(size)
    }

    fn alloc_old(&mut self, size: usize, array_ref: bool) -> Address {
        let object_start = self.alloc_object_in_old_lab(size, array_ref);

        if object_start.is_non_null() {
            return object_start;
        } else if self.old_alloc.failed {
            return Address::null();
        }

        debug_assert!(size <= CLAB_SIZE);
        self.old_lab.make_iterable(self.vm);
        if !self.old_alloc.alloc(&mut self.old_lab) {
            return Address::null();
        }

        self.alloc_object_in_old_lab(size, array_ref)
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
        if self.old_alloc.failed {
            return obj_addr;
        }

        let obj_size = obj.size();
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

        let res = obj.header_mut().vtblptr_forward_atomic(vtblptr, copy_addr);

        match res {
            Ok(()) => {
                obj.copy_to(copy_addr, obj_size);
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
        let copy_addr = self.alloc_old(obj_size, obj.is_array_ref());

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

        let res = obj.header_mut().vtblptr_forward_atomic(vtblptr, copy_addr);

        match res {
            Ok(()) => {
                obj.copy_to(copy_addr, obj_size);
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
        } else {
            self.worker.push(addr);
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
