use ctxt::VM;
use gc::marking;
use gc::root::Slot;
use gc::space::Space;
use gc::swiper::card::CardTable;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::large::LargeSpace;
use gc::swiper::old::OldGen;
use gc::swiper::young::YoungGen;
use gc::{GcReason, Region};

pub struct FullSweepCollector<'a, 'ast: 'a> {
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

    min_heap_size: usize,
    max_heap_size: usize,
}

impl<'a, 'ast> FullSweepCollector<'a, 'ast> {
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

        min_heap_size: usize,
        max_heap_size: usize,
    ) -> FullSweepCollector<'a, 'ast> {
        FullSweepCollector {
            vm: vm,
            heap: heap,

            young: young,
            old: old,
            large_space: large_space,
            card_table: card_table,
            crossing_map: crossing_map,
            perm_space: perm_space,

            rootset: rootset,
            reason: reason,

            min_heap_size: min_heap_size,
            max_heap_size: max_heap_size,
        }
    }

    pub fn collect(&mut self) {
        self.mark_live();
        self.evacuate_young();
        self.sweep();
    }

    fn mark_live(&mut self) {
        marking::start(self.rootset, self.heap, self.perm_space.total());
    }

    fn evacuate_young(&mut self) {
        unimplemented!();
    }

    fn sweep(&mut self) {
        unimplemented!();
    }
}
