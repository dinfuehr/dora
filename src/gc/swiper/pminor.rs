use gc::root::Slot;
use gc::swiper::card::CardTable;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::large::LargeSpace;
use gc::swiper::old::OldGen;
use gc::swiper::young::YoungGen;
use gc::GcReason;
use vm::VM;

use crossbeam_deque::{Stealer, Worker};

pub struct ParMinorCollector<'a, 'ast: 'a> {
    ctxt: &'a VM<'ast>,

    young: &'a YoungGen,
    old: &'a OldGen,
    large: &'a LargeSpace,
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,

    rootset: &'a [Slot],
    reason: GcReason,
    workers: usize,
}

impl<'a, 'ast> ParMinorCollector<'a, 'ast> {
    pub fn new(
        ctxt: &'a VM<'ast>,
        young: &'a YoungGen,
        old: &'a OldGen,
        large: &'a LargeSpace,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
        rootset: &'a [Slot],
        reason: GcReason,
        workers: usize,
    ) -> ParMinorCollector<'a, 'ast> {
        ParMinorCollector {
            ctxt: ctxt,

            young: young,
            old: old,
            large: large,
            card_table: card_table,
            crossing_map: crossing_map,

            rootset: rootset,
            reason: reason,
            workers: workers,
        }
    }

    pub fn collect(&mut self) -> bool {
        // do all of that sequentially for now
        self.visit_roots();
        self.copy_dirty_cards();
        self.visit_large_objects();

        false
    }

    fn visit_roots(&mut self) {
        unimplemented!();
    }

    fn copy_dirty_cards(&mut self) {
        unimplemented!();
    }

    fn visit_large_objects(&mut self) {
        unimplemented!();
    }
}

struct ParMinorCollectorTask {
    local_queue: Vec<Slot>,
    thread_queue: Worker<Slot>,
    rest: Vec<Stealer<Slot>>,
}
