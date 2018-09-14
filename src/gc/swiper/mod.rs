use std::fmt;

use ctxt::SemContext;
use driver::cmd::Args;
use gc::arena;
use gc::root::{get_rootset, IndirectObj};
use gc::swiper::card::CardTable;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::full::FullCollector;
use gc::swiper::large::LargeSpace;
use gc::swiper::minor::MinorCollector;
use gc::swiper::old::OldGen;
use gc::swiper::verify::{Verifier, VerifierPhase};
use gc::swiper::young::YoungGen;
use gc::Address;
use gc::Collector;
use gc::{TLAB_OBJECT_SIZE, TLAB_SIZE};
use mem;
use os;
use vtable::VTable;

pub mod card;
mod crossing;
mod full;
mod large;
mod minor;
pub mod old;
mod verify;
pub mod young;

// determines size of young generation in heap
// young generation size = heap size / YOUNG_RATIO
const YOUNG_RATIO: usize = 4;

// heap is divided into cards of size CARD_SIZE.
// card entry determines whether this part of the heap was modified
// in minor collections those parts of the heap need to be analyzed
pub const CARD_SIZE: usize = 512;
pub const CARD_SIZE_BITS: usize = 9;

pub const LARGE_OBJECT_SIZE: usize = 16 * 1024;

pub struct Swiper {
    heap: Region,

    young: YoungGen,
    old: OldGen,
    large: LargeSpace,

    card_table: CardTable,
    crossing_map: CrossingMap,

    card_table_offset: usize,

    // maximum heap size
    heap_size: usize,

    // committed memory size
    committed_size: usize,
}

impl Swiper {
    pub fn new(args: &Args) -> Swiper {
        let heap_size = args.heap_size();

        // set heap size to multiple of page
        let heap_size = mem::page_align(heap_size);

        // determine sizes of young/old-gen
        let alignment = os::page_size() as usize * 2;
        let young_size = mem::align_usize(heap_size / YOUNG_RATIO, alignment);
        let old_size = heap_size - young_size;

        // determine size for card table
        let card_size = mem::page_align((4 * heap_size) >> CARD_SIZE_BITS);

        // determine size for crossing map
        let crossing_size = mem::page_align(heap_size >> CARD_SIZE_BITS);

        let heap_reserve_size = heap_size * 4 + card_size + crossing_size;

        let ptr = arena::reserve(heap_reserve_size);

        let heap_start = ptr;
        let heap_end = ptr.offset(4 * heap_size);

        // determine offset to card table (card table starts right after heap)
        // offset = card_table_start - (heap_start >> CARD_SIZE_BITS)
        let card_table_offset = heap_end.to_usize() - (heap_start.to_usize() >> CARD_SIZE_BITS);

        // determine boundaries for card table
        let card_start = heap_end;
        let card_end = card_start.offset(card_size);

        arena::commit(card_start, card_size, false);

        // determine boundaries for crossing map
        let crossing_start = card_end;
        let crossing_end = crossing_start.offset(crossing_size);

        arena::commit(crossing_start, crossing_size, false);

        // determine boundaries of young generation
        let young_start = heap_start;
        let young_end = young_start.offset(heap_size);
        let young = YoungGen::new(young_start, young_end);

        arena::commit(young_start, heap_size, false);

        // determine boundaries of old generation
        let old_start = young_end;
        let old_end = old_start.offset(heap_size);

        arena::commit(old_start, heap_size, false);

        // determine large object space
        let large_start = old_end;
        let large_end = large_start.offset(2 * heap_size);

        let card_table = CardTable::new(card_start, card_end, Region::new(old_start, large_end), old_end, heap_size);
        let crossing_map = CrossingMap::new(crossing_start, crossing_end);
        let old = OldGen::new(old_start, old_end, crossing_map.clone(), card_table.clone());
        let large = LargeSpace::new(large_start, large_end);

        if args.flag_gc_verbose {
            println!(
                "GC: heap info: {}, old {}, young {}, card {}, crossing {}",
                formatted_size(heap_size),
                formatted_size(old_size),
                formatted_size(young_size),
                formatted_size(card_size),
                formatted_size(crossing_size)
            );
        }

        Swiper {
            heap: Region::new(heap_start, heap_end),

            young: young,
            old: old,
            large: large,

            card_table: card_table,
            crossing_map: crossing_map,

            card_table_offset: card_table_offset,
            heap_size: heap_size,
            committed_size: 0,
        }
    }

    fn minor_collect_inner(&self, ctxt: &SemContext) -> bool {
        // make heap iterable
        self.fill_current_tlab(ctxt);

        let rootset = get_rootset(ctxt);

        self.verify(ctxt, VerifierPhase::PreMinor, "pre-minor", &rootset);

        let mut collector = MinorCollector::new(
            ctxt,
            &self.young,
            &self.old,
            &self.large,
            &self.card_table,
            &self.crossing_map,
            &rootset,
        );
        collector.collect();

        let promotion_failed = collector.promotion_failed();
        self.verify(ctxt, VerifierPhase::PostMinor, "post-minor", &rootset);

        promotion_failed
    }

    fn full_collect(&self, ctxt: &SemContext) {
        // make heap iterable
        self.fill_current_tlab(ctxt);

        let rootset = get_rootset(ctxt);

        self.verify(ctxt, VerifierPhase::PreFull, "pre-full", &rootset);

        let mut collector = FullCollector::new(
            ctxt,
            self.heap.clone(),
            &self.young,
            &self.old,
            &self.large,
            &self.card_table,
            &self.crossing_map,
            &ctxt.gc.perm_space,
            &rootset,
        );
        collector.collect();

        self.verify(ctxt, VerifierPhase::PostFull, "post-full", &rootset);
    }

    fn verify(
        &self,
        ctxt: &SemContext,
        phase: VerifierPhase,
        _name: &str,
        rootset: &[IndirectObj],
    ) {
        if ctxt.args.flag_gc_verify {
            if ctxt.args.flag_gc_verbose {
                println!("VERIFY: {}", _name);
            }

            let perm_space = &ctxt.gc.perm_space;

            let mut verifier = Verifier::new(
                &self.young,
                &self.old,
                &self.card_table,
                &self.crossing_map,
                rootset,
                &self.large,
                &*perm_space,
                phase,
            );
            verifier.verify();
        }
    }
}

impl Collector for Swiper {
    fn alloc(&self, ctxt: &SemContext, size: usize, array_ref: bool) -> Address {
        if ctxt.args.flag_gc_stress_minor {
            self.minor_collect(ctxt);
        }

        if ctxt.args.flag_gc_stress {
            self.full_collect(ctxt);
        }

        if size < TLAB_OBJECT_SIZE {
            self.alloc_tlab(ctxt, size, array_ref)
        } else if size < LARGE_OBJECT_SIZE {
            self.alloc_normal(ctxt, size, array_ref)
        } else {
            self.alloc_large(ctxt, size, array_ref)
        }
    }

    fn collect(&self, ctxt: &SemContext) {
        self.full_collect(ctxt);
    }

    fn minor_collect(&self, ctxt: &SemContext) {
        self.fill_current_tlab(ctxt);
        self.minor_collect_inner(ctxt);
    }

    fn needs_write_barrier(&self) -> bool {
        return true;
    }

    fn card_table_offset(&self) -> usize {
        self.card_table_offset
    }
}

impl Swiper {
    fn alloc_tlab(&self, ctxt: &SemContext, size: usize, array_ref: bool) -> Address {
        let tlab = ctxt.tld.borrow().tlab_region();

        // try to allocate in current tlab
        if size <= tlab.size() {
            ctxt.tld
                .borrow_mut()
                .tlab_initialize(tlab.start.offset(size), tlab.end);
            return tlab.start;
        }

        // if there is not enough space, make heap iterable by filling tlab with unused objects
        self.fill_tlab(ctxt, tlab.start, tlab.end);

        // allocate new tlab
        let tlab_start = self.alloc_tlab_area(ctxt, size);

        if tlab_start.is_null() {
            // allocate object in old generation instead
            self.old.alloc(size, array_ref)
        } else {
            let object_start = tlab_start;
            let tlab_start = tlab_start.offset(size);
            let tlab_end = tlab_start.offset(TLAB_SIZE);

            // initialize TLAB to new boundaries
            ctxt.tld.borrow_mut().tlab_initialize(tlab_start, tlab_end);

            // object is allocated before TLAB
            object_start
        }
    }

    fn alloc_tlab_area(&self, ctxt: &SemContext, extend: usize) -> Address {
        assert!(extend < TLAB_OBJECT_SIZE);
        let size = TLAB_SIZE + extend;
        let ptr = self.young.alloc(size);

        if !ptr.is_null() {
            return ptr;
        }

        let promotion_failed = self.minor_collect_inner(ctxt);

        if promotion_failed {
            self.full_collect(ctxt);
            return self.young.alloc(size);
        }

        let ptr = self.young.alloc(size);

        if !ptr.is_null() {
            return ptr;
        }

        self.full_collect(ctxt);
        self.young.alloc(TLAB_SIZE)
    }

    fn fill_current_tlab(&self, ctxt: &SemContext) {
        let tlab = ctxt.tld.borrow().tlab_region();
        self.fill_tlab(ctxt, tlab.start, tlab.end);
    }

    fn fill_tlab(&self, ctxt: &SemContext, start: Address, end: Address) {
        if start == end {
            // nothing to do

        } else if end.offset_from(start) == mem::ptr_width_usize() {
            // fill with object
            let cls_id = ctxt.vips.obj(ctxt);
            let cls = ctxt.class_defs[cls_id].borrow();
            let vtable: *const VTable = &**cls.vtable.as_ref().unwrap();

            unsafe {
                *start.to_mut_ptr::<usize>() = vtable as usize;
            }
        } else {
            // fill with int array
            let cls_id = ctxt.vips.int_array(ctxt);
            let cls = ctxt.class_defs[cls_id].borrow();
            let vtable: *const VTable = &**cls.vtable.as_ref().unwrap();
            let length: usize = end.offset_from(start.add_ptr(2)) / 4;

            unsafe {
                *start.to_mut_ptr::<usize>() = vtable as usize;
                *start.add_ptr(1).to_mut_ptr::<usize>() = length;
            }
        }

        let n = Address::null();
        ctxt.tld.borrow_mut().tlab_initialize(n, n);
    }

    fn alloc_normal(&self, ctxt: &SemContext, size: usize, array_ref: bool) -> Address {
        let ptr = self.young.alloc(size);

        if !ptr.is_null() {
            return ptr;
        }

        let promotion_failed = self.minor_collect_inner(ctxt);

        if promotion_failed {
            self.full_collect(ctxt);
        }

        let ptr = self.young.alloc(size);

        if !ptr.is_null() {
            return ptr;
        }

        self.old.alloc(size, array_ref)
    }

    fn alloc_large(&self, ctxt: &SemContext, size: usize, _: bool) -> Address {
        let ptr = self.large.alloc(size);

        if !ptr.is_null() {
            return ptr;
        }

        self.full_collect(ctxt);

        self.large.alloc(size)
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct CardIdx(usize);

impl CardIdx {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for CardIdx {
    fn from(val: usize) -> CardIdx {
        CardIdx(val)
    }
}

#[derive(Clone)]
pub struct Region {
    pub start: Address,
    pub end: Address,
}

impl Region {
    pub fn new(start: Address, end: Address) -> Region {
        Region {
            start: start,
            end: end,
        }
    }

    #[inline(always)]
    pub fn contains(&self, addr: Address) -> bool {
        self.start <= addr && addr < self.end
    }

    #[inline(always)]
    pub fn valid_top(&self, addr: Address) -> bool {
        self.start <= addr && addr <= self.end
    }

    #[inline(always)]
    pub fn size(&self) -> usize {
        self.end.to_usize() - self.start.to_usize()
    }
}

impl fmt::Display for Region {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

struct FormattedSize {
    size: usize,
}

impl fmt::Display for FormattedSize {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ksize = (self.size as f64) / 1024f64;

        if ksize < 1f64 {
            return write!(f, "{}B", self.size);
        }

        let msize = ksize / 1024f64;

        if msize < 1f64 {
            return write!(f, "{:.1}K", ksize);
        }

        let gsize = msize / 1024f64;

        if gsize < 1f64 {
            write!(f, "{:.1}M", msize)
        } else {
            write!(f, "{:.1}G", gsize)
        }
    }
}

fn formatted_size(size: usize) -> FormattedSize {
    FormattedSize { size }
}

fn in_kilo(size: usize) -> f64 {
    (size as f64) / 1024.0
}

fn on_different_cards(curr: Address, next: Address) -> bool {
    (curr.to_usize() >> CARD_SIZE_BITS) != (next.to_usize() >> CARD_SIZE_BITS)
}

fn start_of_card(addr: Address) -> bool {
    (addr.to_usize() & (CARD_SIZE - 1)) == addr.to_usize()
}
