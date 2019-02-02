use scoped_threadpool::Pool;

use ctxt::VM;
use gc::root::Slot;
use gc::space::Space;
use gc::swiper::card::CardTable;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::large::LargeSpace;
use gc::swiper::marking;
use gc::swiper::old::OldGen;
use gc::swiper::young::YoungGen;
use gc::{Address, GcReason, Region};

pub struct ParallelFullCollector<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    heap: Region,
    young: &'a YoungGen,
    old: &'a OldGen,
    large_space: &'a LargeSpace,
    rootset: &'a [Slot],
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
    perm_space: &'a Space,

    old_top: Address,
    old_limit: Address,
    init_old_top: Address,

    reason: GcReason,
    threadpool: &'a mut Pool,

    min_heap_size: usize,
    max_heap_size: usize,
}

impl<'a, 'ast> ParallelFullCollector<'a, 'ast> {
    pub fn new(
        vm: &'a VM<'ast>,
        heap: Region,
        young: &'a YoungGen,
        old: &'a OldGen,
        large_space: &'a LargeSpace,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
        perm_space: &'a Space,
        rootset: &'a [Slot],
        reason: GcReason,
        threadpool: &'a mut Pool,
        min_heap_size: usize,
        max_heap_size: usize,
    ) -> ParallelFullCollector<'a, 'ast> {
        let old_total = old.total();

        ParallelFullCollector {
            vm: vm,
            heap: heap,
            young: young,
            old: old,
            large_space: large_space,
            rootset: rootset,
            card_table: card_table,
            crossing_map: crossing_map,
            perm_space: perm_space,

            old_top: old_total.start,
            old_limit: old_total.end,
            init_old_top: Address::null(),

            reason: reason,
            threadpool: threadpool,

            min_heap_size: min_heap_size,
            max_heap_size: max_heap_size,
        }
    }

    pub fn collect(&mut self) {
        let dev_verbose = self.vm.args.flag_gc_dev_verbose;
        self.init_old_top = self.old.top();

        if dev_verbose {
            println!("Full GC: Phase 1 (marking)");
        }

        self.mark_live();

        if dev_verbose {
            println!("Full GC: Phase 2 (forward)");
        }

        self.compute_forward();

        if dev_verbose {
            println!("Full GC: Phase 3 (update refs)");
        }

        self.update_references();

        if dev_verbose {
            println!("Full GC: Phase 4 (relocate)");
        }

        self.relocate();

        if dev_verbose {
            println!("Full GC: Phase 5 (large objects)");
        }

        self.update_large_objects();

        if dev_verbose {
            println!("Full GC: Phase 5 (large objects) finished.");
        }
    }

    fn mark_live(&mut self) {
        marking::start(
            self.rootset,
            self.heap.clone(),
            self.perm_space.total(),
            self.threadpool,
        );
    }

    fn compute_forward(&mut self) {
        unimplemented!();
    }

    fn update_references(&mut self) {
        unimplemented!();
    }

    fn relocate(&mut self) {
        unimplemented!();
    }

    fn update_large_objects(&mut self) {
        unimplemented!();
    }
}
