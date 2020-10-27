use parking_lot::MutexGuard;

use crate::gc::marking;
use crate::gc::root::Slot;
use crate::gc::space::Space;
use crate::gc::swiper::card::CardTable;
use crate::gc::swiper::crossing::CrossingMap;
use crate::gc::swiper::large::LargeSpace;
use crate::gc::swiper::old::{OldGen, OldGenProtected};
use crate::gc::swiper::young::YoungGen;
use crate::gc::{GcReason, Region};
use crate::vm::VM;

pub struct FullSweepCollector<'a> {
    vm: &'a VM,
    heap: Region,

    young: &'a YoungGen,
    old: &'a OldGen,
    old_prot: MutexGuard<'a, OldGenProtected>,
    large_space: &'a LargeSpace,
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
    perm_space: &'a Space,

    rootset: &'a [Slot],
    reason: GcReason,

    min_heap_size: usize,
    max_heap_size: usize,
}

impl<'a> FullSweepCollector<'a> {
    pub fn new(
        vm: &'a VM,
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
    ) -> FullSweepCollector<'a> {
        FullSweepCollector {
            vm,
            heap,

            young,
            old,
            old_prot: old.protected(),
            large_space,
            card_table,
            crossing_map,
            perm_space,

            rootset,
            reason,

            min_heap_size,
            max_heap_size,
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
