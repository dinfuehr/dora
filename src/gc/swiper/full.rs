use ctxt::SemContext;
use gc::root::IndirectObj;
use gc::swiper::card::CardTable;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::young::YoungGen;
use gc::swiper::old::OldGen;

pub struct FullCollector<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    young: &'a YoungGen,
    old: &'a OldGen,
    rootset: &'a [IndirectObj],
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
}

impl<'a, 'ast> FullCollector<'a, 'ast> {
    pub fn new(
        ctxt: &'a SemContext<'ast>,
        young: &'a YoungGen,
        old: &'a OldGen,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
        rootset: &'a [IndirectObj],
    ) -> FullCollector<'a, 'ast> {
        FullCollector {
            ctxt: ctxt,
            young: young,
            old: old,
            rootset: rootset,
            card_table: card_table,
            crossing_map: crossing_map,
        }
    }

    pub fn collect(&mut self) {
        // TODO
    }
}