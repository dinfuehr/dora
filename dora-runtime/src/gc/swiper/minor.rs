use parking_lot::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Barrier};

use crate::gc::pmarking::Terminator;
use crate::gc::root::Slot;
use crate::gc::swiper::controller::{MinorCollectorPhases, SharedHeapConfig};
use crate::gc::swiper::large::LargeSpace;
use crate::gc::swiper::old::OldGen;
use crate::gc::swiper::young::YoungGen;
use crate::gc::swiper::{BasePage, LargePage, RegularPage, Swiper, LARGE_OBJECT_SIZE};
use crate::gc::tlab::{MAX_TLAB_OBJECT_SIZE, MAX_TLAB_SIZE, MIN_TLAB_SIZE};
use crate::gc::{
    fill_region, fill_region_with, iterate_weak_roots, Address, GcReason, GenerationAllocator,
    Region,
};
use crate::object::{ForwardResult, Obj, VtblptrWordKind};
use crate::threads::DoraThread;
use crate::vm::VM;

use crossbeam_deque::{Injector, Steal, Stealer, Worker};
use rand::distributions::{Distribution, Uniform};
use rand::thread_rng;
use scoped_threadpool::Pool;

pub struct MinorCollector<'a> {
    vm: &'a VM,

    swiper: &'a Swiper,
    young: &'a YoungGen,
    old: &'a OldGen,
    large: &'a LargeSpace,

    rootset: &'a [Slot],
    _threads: &'a [Arc<DoraThread>],
    _reason: GcReason,
    heap: Region,

    // young_alloc: Option<YoungAlloc>,
    promoted_size: usize,
    copied_size: usize,

    _min_heap_size: usize,
    _max_heap_size: usize,

    threadpool: &'a mut Pool,
    number_workers: usize,
    config: &'a SharedHeapConfig,
    phases: MinorCollectorPhases,
}

impl<'a> MinorCollector<'a> {
    pub fn new(
        vm: &'a VM,
        swiper: &'a Swiper,
        young: &'a YoungGen,
        old: &'a OldGen,
        large: &'a LargeSpace,
        rootset: &'a [Slot],
        threads: &'a [Arc<DoraThread>],
        reason: GcReason,
        min_heap_size: usize,
        max_heap_size: usize,
        threadpool: &'a mut Pool,
        config: &'a SharedHeapConfig,
    ) -> MinorCollector<'a> {
        MinorCollector {
            vm,

            swiper,
            young,
            old,
            large,
            rootset,
            _threads: threads,

            promoted_size: 0,
            copied_size: 0,

            _reason: reason,
            heap: swiper.heap,

            _min_heap_size: min_heap_size,
            _max_heap_size: max_heap_size,

            number_workers: threadpool.thread_count() as usize,
            threadpool,

            config,
            phases: MinorCollectorPhases::new(),
        }
    }

    pub fn phases(&self) -> MinorCollectorPhases {
        self.phases.clone()
    }

    pub fn collect(&mut self) {
        self.young.unprotect_from();
        self.young.swap_semi(self.vm);

        self.run_threads();

        self.iterate_weak_refs();

        self.young.reset_after_minor_gc();
        self.young.protect_from();

        let mut config = self.config.lock();
        config.minor_promoted = self.promoted_size;
        config.minor_copied = self.copied_size;
    }

    fn run_threads(&mut self) {
        let mut workers = Vec::with_capacity(self.number_workers);
        let mut stealers = Vec::with_capacity(self.number_workers);
        let injector: Injector<WorkItem> = Injector::new();

        for _ in 0..self.number_workers {
            let w = Worker::new_lifo();
            let s = w.stealer();
            workers.push(w);
            stealers.push(s);
        }

        let terminator = Terminator::new(self.number_workers);
        let vm = self.vm;

        let heap = self.heap;
        let young = self.young;
        let old = self.old;
        let rootset = self.rootset;
        let barrier = Barrier::new(self.number_workers);
        let barrier = &barrier;

        let promoted_size = AtomicUsize::new(self.promoted_size);
        let promoted_size = &promoted_size;

        let copied_size = AtomicUsize::new(self.copied_size);
        let copied_size = &copied_size;

        let next_root_stride = AtomicUsize::new(0);
        let next_root_stride = &next_root_stride;

        let next_object_stride = AtomicUsize::new(0);
        let next_object_stride = &next_object_stride;

        let strides = 4 * self.number_workers;

        let next_old_page_idx = AtomicUsize::new(0);
        let next_old_page_idx = &next_old_page_idx;
        let old_pages = self.old.protected().pages();
        let old_pages = &old_pages;

        let head = self.large.head();
        let next_large = Mutex::new(head);
        let next_large = &next_large;

        let added_to_remset: Mutex<Vec<Vec<Address>>> = Default::default();

        {
            let remset = self.swiper.remset.read();
            let remset: &[Address] = &*remset;

            let added_to_remset = &added_to_remset;

            self.threadpool.scoped(|scoped| {
                for (task_id, worker) in workers.into_iter().enumerate() {
                    let injector = &injector;
                    let stealers = &stealers;
                    let terminator = &terminator;
                    let meta_space_start = vm.meta_space_start();

                    scoped.execute(move || {
                        let mut task = CopyTask {
                            task_id,
                            local: Vec::new(),
                            worker,
                            injector,
                            stealers,
                            terminator,

                            vm,
                            heap,
                            young,
                            old,
                            rootset,
                            barrier,
                            remset,
                            added_to_remset: Vec::new(),
                            meta_space_start,

                            next_root_stride,
                            next_object_stride,
                            strides,
                            next_large,

                            next_old_page_idx,
                            old_pages,

                            promoted_size: 0,
                            copied_size: 0,
                            traced: 0,

                            old_lab: Lab::new(),

                            young_lab: Lab::new(),
                        };

                        task.run();

                        if task.promoted_size > 0 {
                            promoted_size.fetch_add(task.promoted_size, Ordering::Relaxed);
                        }

                        if task.copied_size > 0 {
                            copied_size.fetch_add(task.copied_size, Ordering::Relaxed);
                        }

                        if !task.added_to_remset.is_empty() {
                            let current = std::mem::replace(&mut task.added_to_remset, Vec::new());
                            added_to_remset.lock().push(current);
                        }
                    });
                }
            });
        }

        let added_to_remset = added_to_remset.into_inner();
        for mut thread_remset in added_to_remset {
            self.swiper.remset.write().append(&mut thread_remset);
        }

        self.promoted_size = promoted_size.load(Ordering::Relaxed);
        self.copied_size = copied_size.load(Ordering::Relaxed);
    }

    fn iterate_weak_refs(&mut self) {
        iterate_weak_roots(self.vm, |object_address| {
            if self.heap.contains(object_address) {
                let page = BasePage::from_address(object_address);
                if page.is_young() {
                    let obj = object_address.to_obj();

                    if let VtblptrWordKind::Fwdptr(fwdptr) =
                        obj.header().vtblptr(self.vm.meta_space_start())
                    {
                        Some(fwdptr)
                    } else {
                        None
                    }
                } else {
                    Some(object_address)
                }
            } else {
                Some(object_address)
            }
        });
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
        fill_region_with(vm, self.top, self.limit, false);

        self.top = Address::null();
        self.limit = Address::null();
    }

    fn make_iterable_old(&mut self, vm: &VM) {
        fill_region_with(vm, self.top, self.limit, false);

        self.top = Address::null();
        self.limit = Address::null();
    }

    fn allocate(&mut self, size: usize) -> Option<Address> {
        let object_start = self.top;
        let object_end = object_start.offset(size);

        if object_end <= self.limit {
            self.top = object_end;
            Some(object_start)
        } else {
            None
        }
    }

    fn undo_alloc(&mut self, size: usize) {
        self.top = (self.top.to_usize() - size).into();
        debug_assert!(self.limit.offset_from(self.top) <= MAX_LAB_SIZE);
    }
}

const MIN_LAB_SIZE: usize = MIN_TLAB_SIZE;
const MAX_LAB_SIZE: usize = MAX_TLAB_SIZE;
const MAX_LAB_OBJECT_SIZE: usize = MAX_TLAB_OBJECT_SIZE;

const LOCAL_MAXIMUM: usize = 64;

struct WorkItem(Address);

impl WorkItem {
    fn slot(slot: Slot) -> WorkItem {
        let value = slot.address().to_usize();
        WorkItem(Address(value | 1))
    }

    fn object(object: Address) -> WorkItem {
        debug_assert_eq!(object.to_usize() % 2, 0);
        WorkItem(object)
    }

    fn to_slot(&self) -> Option<Slot> {
        let value = self.0.to_usize();
        if value & 1 != 0 {
            let address: Address = (value & !1).into();
            Some(Slot::at(address))
        } else {
            None
        }
    }
}

struct CopyTask<'a> {
    task_id: usize,
    local: Vec<WorkItem>,
    worker: Worker<WorkItem>,
    injector: &'a Injector<WorkItem>,
    stealers: &'a [Stealer<WorkItem>],
    terminator: &'a Terminator,

    vm: &'a VM,
    heap: Region,
    young: &'a YoungGen,
    old: &'a OldGen,
    rootset: &'a [Slot],
    barrier: &'a Barrier,
    remset: &'a [Address],
    added_to_remset: Vec<Address>,
    meta_space_start: Address,

    next_root_stride: &'a AtomicUsize,
    next_object_stride: &'a AtomicUsize,
    strides: usize,
    next_large: &'a Mutex<Option<LargePage>>,

    old_pages: &'a [RegularPage],
    next_old_page_idx: &'a AtomicUsize,

    promoted_size: usize,
    copied_size: usize,
    traced: usize,

    old_lab: Lab,

    young_lab: Lab,
}

impl<'a> CopyTask<'a> {
    fn run(&mut self) {
        self.visit_roots();
        self.visit_remembered_objects();

        self.trace_gray_objects();
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

        for &root in roots_in_stride {
            let object_address = root.get();

            if self.is_young(object_address) {
                self.push_slot(root);
            }
        }
    }

    fn visit_remembered_objects(&mut self) {
        while let Some(stride) = self.next_object_stride() {
            self.visit_remembered_objects_in_stride(stride);
        }
    }

    fn next_object_stride(&mut self) -> Option<usize> {
        let stride = self.next_object_stride.fetch_add(1, Ordering::Relaxed);

        if stride < self.strides {
            Some(stride)
        } else {
            None
        }
    }

    fn visit_remembered_objects_in_stride(&mut self, stride: usize) {
        let remset_stride = self.remset.iter().skip(stride).step_by(self.strides);

        for &object_address in remset_stride {
            self.visit_remembered_object(object_address);
        }
    }

    fn visit_remembered_object(&mut self, object_address: Address) {
        let object = object_address.to_obj();

        object.visit_reference_fields(self.vm.meta_space_start(), |slot| {
            let pointer = slot.get();

            if self.is_young(pointer) {
                slot.set(self.evacuate_object(pointer));
            }
        });
    }

    fn next_old_page(&mut self) -> Option<RegularPage> {
        let page_idx = self.next_old_page_idx.fetch_add(1, Ordering::Relaxed);
        self.old_pages.get(page_idx).cloned()
    }

    fn next_large(&mut self) -> Option<LargePage> {
        let mut next_large = self.next_large.lock();

        if let Some(large_page) = *next_large {
            *next_large = large_page.next_page();
            Some(large_page)
        } else {
            None
        }
    }

    fn trace_gray_objects(&mut self) {
        loop {
            if let Some(item) = self.pop() {
                if let Some(slot) = item.to_slot() {
                    let copied_obj = self.evacuate_object(slot.get());
                    slot.set(copied_obj);
                } else {
                    let object = item.0;

                    if self.is_young(object) {
                        self.trace_young_object(object);
                    } else {
                        self.trace_promoted_object(object);
                    }
                }
            } else if self.terminator.try_terminate() {
                break;
            } else {
                continue;
            }
        }

        self.young_lab.make_iterable_young(self.vm);
        self.old_lab.make_iterable_old(self.vm);
    }

    fn trace_young_object(&mut self, object_addr: Address) {
        let object = object_addr.to_obj();

        object.visit_reference_fields(self.vm.meta_space_start(), |slot| {
            let pointer = slot.get();

            if self.is_young(pointer) {
                slot.set(self.evacuate_object(pointer));
            }
        });
    }

    fn trace_promoted_object(&mut self, object_addr: Address) {
        let object = object_addr.to_obj();

        let mut ref_to_young_gen = false;

        object.visit_reference_fields(self.vm.meta_space_start(), |slot| {
            let field_ptr: Address = slot.get();

            if self.is_young(field_ptr) {
                let copied_addr = self.evacuate_object(field_ptr);
                slot.set(copied_addr);

                if self.is_young(copied_addr) {
                    ref_to_young_gen = true;
                }
            }
        });

        if ref_to_young_gen {
            object.header().set_remembered();
            self.added_to_remset.push(object_addr);
        }
    }

    fn alloc_young(&mut self, size: usize) -> Address {
        if size < MAX_LAB_OBJECT_SIZE {
            self.alloc_young_small(size)
        } else {
            self.alloc_young_medium(size)
        }
    }

    fn alloc_young_small(&mut self, size: usize) -> Address {
        debug_assert!(size < MAX_LAB_OBJECT_SIZE);

        if let Some(object_start) = self.young_lab.allocate(size) {
            return object_start;
        }

        debug_assert!(size <= MAX_LAB_SIZE);
        self.young_lab.make_iterable_young(self.vm);
        if !self.alloc_young_lab() {
            return Address::null();
        }

        self.young_lab.allocate(size).unwrap_or(Address::null())
    }

    fn alloc_young_medium(&mut self, size: usize) -> Address {
        debug_assert!(MAX_LAB_OBJECT_SIZE <= size && size < LARGE_OBJECT_SIZE);

        if let Some(region) = self.young.allocate(self.vm, size, size) {
            region.start()
        } else {
            Address::null()
        }
    }

    fn alloc_young_lab(&mut self) -> bool {
        if let Some(lab) = self.young.allocate(self.vm, MIN_LAB_SIZE, MAX_LAB_SIZE) {
            self.young_lab.reset(lab.start(), lab.end());
            true
        } else {
            self.young_lab.reset(Address::null(), Address::null());
            false
        }
    }

    fn undo_alloc_young(&mut self, copy_addr: Address, size: usize) {
        if size < MAX_LAB_OBJECT_SIZE {
            self.young_lab.undo_alloc(size)
        } else {
            // Can't undo mid-sized objects. Need to make the heap iterable.
            fill_region(self.vm, copy_addr, copy_addr.offset(size));
        }
    }

    fn alloc_old(&mut self, size: usize) -> Address {
        if size < MAX_LAB_OBJECT_SIZE {
            self.alloc_old_small(size)
        } else {
            self.alloc_old_medium(size)
        }
    }

    fn alloc_old_small(&mut self, size: usize) -> Address {
        debug_assert!(size < MAX_LAB_OBJECT_SIZE);
        let object_start = self.alloc_object_in_old_lab(size);

        if let Some(object_start) = object_start {
            return object_start;
        }

        self.old_lab.make_iterable_old(self.vm);
        if !self.alloc_old_lab() {
            return Address::null();
        }

        self.alloc_object_in_old_lab(size)
            .unwrap_or(Address::null())
    }

    fn alloc_old_medium(&mut self, size: usize) -> Address {
        debug_assert!(MAX_LAB_OBJECT_SIZE <= size && size < LARGE_OBJECT_SIZE);

        if let Some(new_region) = self.old.allocate(self.vm, size, size) {
            let object_start = new_region.start();
            object_start
        } else {
            Address::null()
        }
    }

    fn undo_alloc_old(&mut self, copy_addr: Address, size: usize) {
        if size < MAX_LAB_OBJECT_SIZE {
            self.old_lab.undo_alloc(size);
        } else {
            // Can't undo mid-sized objects. Need to make the heap iterable.
            fill_region(self.vm, copy_addr, copy_addr.offset(size));
        }
    }

    fn alloc_old_lab(&mut self) -> bool {
        if let Some(lab) = self.old.allocate(self.vm, MIN_LAB_SIZE, MAX_LAB_SIZE) {
            self.old_lab.reset(lab.start(), lab.end());

            true
        } else {
            false
        }
    }

    fn alloc_object_in_old_lab(&mut self, size: usize) -> Option<Address> {
        let object_start = self.old_lab.allocate(size);

        if let Some(object_start) = object_start {
            Some(object_start)
        } else {
            None
        }
    }

    fn evacuate_object(&mut self, obj_addr: Address) -> Address {
        let obj = obj_addr.to_obj();

        // Check if object was already copied
        let vtblptr = match obj.header().vtblptr(self.vm.meta_space_start()) {
            VtblptrWordKind::Fwdptr(fwd_addr) => {
                return fwd_addr;
            }

            VtblptrWordKind::Vtblptr(vtblptr) => vtblptr,
        };

        let obj_size = obj.size_for_vtblptr(vtblptr);

        // If object is old enough we copy it into the old generation
        if self.should_be_promoted(obj_addr) {
            if let Some(address) = self.promote_object(vtblptr, obj, obj_size) {
                return address;
            }
        }

        // Try to allocate memory in to-space.
        let copy_addr = self.alloc_young(obj_size);

        // Couldn't allocate object in young generation, try to promote
        // object into old generation instead.
        if copy_addr.is_null() {
            if let Some(address) = self.promote_object(vtblptr, obj, obj_size) {
                return address;
            } else {
                panic!("FAIL: Not enough space for evacuation during scavenge.");
            }
        }

        obj.copy_to(copy_addr, obj_size);
        let res = obj
            .header()
            .try_install_fwdptr(self.meta_space_start, vtblptr, copy_addr);

        match res {
            ForwardResult::Forwarded => {
                self.copied_size += obj_size;
                self.push(copy_addr);
                copy_addr
            }

            ForwardResult::AlreadyForwarded(actual_new_address) => {
                self.undo_alloc_young(copy_addr, obj_size);
                actual_new_address
            }
        }
    }

    fn should_be_promoted(&self, addr: Address) -> bool {
        let page = BasePage::from_address(addr);
        page.is_survivor()
    }

    fn promote_object(&mut self, vtblptr: Address, obj: &Obj, obj_size: usize) -> Option<Address> {
        let copy_addr = self.alloc_old(obj_size);

        // if there isn't enough space in old gen keep it in the
        // young generation for now. A full collection will be forced later and
        // cleans this up.
        if copy_addr.is_null() {
            return None;
        }

        obj.copy_to(copy_addr, obj_size);
        copy_addr.to_obj().header().set_metadata_raw(false, false);
        let res = obj
            .header()
            .try_install_fwdptr(self.meta_space_start, vtblptr, copy_addr);

        match res {
            ForwardResult::Forwarded => {
                self.promoted_size += obj_size;
                self.push(copy_addr);

                Some(copy_addr)
            }

            ForwardResult::AlreadyForwarded(actual_new_address) => {
                self.undo_alloc_old(copy_addr, obj_size);

                Some(actual_new_address)
            }
        }
    }

    fn push_slot(&mut self, slot: Slot) {
        self.push_item(WorkItem::slot(slot));
    }

    fn push(&mut self, addr: Address) {
        self.push_item(WorkItem::object(addr));
    }

    fn push_item(&mut self, item: WorkItem) {
        if self.local.len() < LOCAL_MAXIMUM {
            self.local.push(item);
            self.defensive_push();
        } else {
            self.worker.push(item);
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

    fn pop(&mut self) -> Option<WorkItem> {
        self.pop_local()
            .or_else(|| self.pop_worker())
            .or_else(|| self.pop_global())
            .or_else(|| self.steal())
    }

    fn pop_local(&mut self) -> Option<WorkItem> {
        self.local.pop()
    }

    fn pop_worker(&mut self) -> Option<WorkItem> {
        self.worker.pop()
    }

    fn pop_global(&mut self) -> Option<WorkItem> {
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

    fn steal(&self) -> Option<WorkItem> {
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

    fn is_young(&self, object_address: Address) -> bool {
        if self.heap.contains(object_address) {
            let page = BasePage::from_address(object_address);
            page.is_young()
        } else {
            false
        }
    }
}
