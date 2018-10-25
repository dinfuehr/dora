use ctxt::SemContext;
use gc::root::Slot;
use gc::swiper::card::CardTable;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::large::LargeSpace;
use gc::swiper::old::OldGen;
use gc::swiper::young::YoungGen;
use gc::GcReason;

pub struct ParMinorCollector<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,

    young: &'a YoungGen,
    old: &'a OldGen,
    large: &'a LargeSpace,
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,

    rootset: &'a [Slot],
    reason: GcReason,
}

impl<'a, 'ast> ParMinorCollector<'a, 'ast> {
    pub fn new(
        ctxt: &'a SemContext<'ast>,
        young: &'a YoungGen,
        old: &'a OldGen,
        large: &'a LargeSpace,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
        rootset: &'a [Slot],
        reason: GcReason,
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
        }
    }

    pub fn collect(&mut self) {
        unimplemented!();
    }

    pub fn promotion_failed(&mut self) -> bool {
        false
    }
}