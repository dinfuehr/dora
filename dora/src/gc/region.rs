use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};

use fixedbitset::FixedBitSet;
use num_traits::cast::FromPrimitive;
use parking_lot::Mutex;

use crate::driver::cmd::Args;
use crate::gc::{Address, Collector, GcReason, Region};
use crate::os::{self, Reservation};
use crate::vm::VM;

pub const REGION_SIZE_BITS: usize = 20;
pub const REGION_SIZE: usize = 1 << REGION_SIZE_BITS;

pub struct RegionCollector {
    reservation: Reservation,
    regions: Vec<HeapRegion>,
    number_regions: usize,
    regular_alloc_region: Mutex<Option<RegionId>>,
}

impl RegionCollector {
    pub fn new(args: &Args) -> RegionCollector {
        let max_heap_size = align_region(args.max_heap_size());

        let reservation = os::reserve_align(max_heap_size, REGION_SIZE, false);
        let number_regions = max_heap_size / REGION_SIZE;
        let mut regions = Vec::with_capacity(number_regions);

        let mut next_region_start = reservation.start;

        for _ in 0..number_regions {
            let heap_region = HeapRegion {
                area_start: next_region_start,
                area_end: next_region_start.offset(REGION_SIZE - os::page_size()),
                top: AtomicUsize::new(next_region_start.to_usize()),
                state: AtomicRegionState::new(RegionState::Free),
                live_bytes: AtomicUsize::new(0),
            };
            regions.push(heap_region);
            next_region_start = next_region_start.offset(REGION_SIZE);
        }

        RegionCollector {
            reservation,
            regions,
            number_regions,
            regular_alloc_region: Mutex::new(None),
        }
    }
}

impl Collector for RegionCollector {
    fn supports_tlab(&self) -> bool {
        false
    }

    fn alloc_tlab_area(&self, _vm: &VM, _size: usize) -> Option<Region> {
        unimplemented!()
    }

    fn alloc(&self, vm: &VM, size: usize, _array_ref: bool) -> Address {
        if size < REGION_SIZE / 2 {
            self.regular_alloc(vm, size)
        } else {
            unimplemented!()
        }
    }

    fn collect(&self, _vm: &VM, _reason: GcReason) {
        unimplemented!()
    }

    fn minor_collect(&self, _vm: &VM, _reason: GcReason) {
        unimplemented!()
    }

    fn needs_write_barrier(&self) -> bool {
        false
    }

    fn card_table_offset(&self) -> usize {
        unimplemented!()
    }

    fn dump_summary(&self, _runtime: f32) {
        unimplemented!()
    }

    fn verify_ref(&self, _vm: &VM, _reference: Address) {
        unimplemented!()
    }
}

impl RegionCollector {
    fn regular_alloc(&self, _vm: &VM, size: usize) -> Address {
        assert!(size < REGION_SIZE / 2);

        let mut regular_alloc_region = self.regular_alloc_region.lock();

        loop {
            let region_id = match *regular_alloc_region {
                Some(region_id) => region_id,
                None => {
                    let allocated_region_id = self.find_free_region();

                    if allocated_region_id.is_none() {
                        return Address::null();
                    }

                    let allocated_region_id = allocated_region_id.unwrap();
                    self.init_region(allocated_region_id);
                    *regular_alloc_region = Some(allocated_region_id);

                    allocated_region_id
                }
            };

            let region = self.region(region_id);
            let address = self.bump_pointer_alloc(region, size);

            if address.is_non_null() {
                return address;
            } else {
                *regular_alloc_region = None;
            }
        }
    }

    fn bump_pointer_alloc(&self, region: &HeapRegion, size: usize) -> Address {
        let mut top = region.top.load(Ordering::Relaxed);
        let limit = region.area_end.to_usize();

        loop {
            if top + size <= limit {
                let result = region.top.compare_exchange_weak(
                    top,
                    top + size,
                    Ordering::Relaxed,
                    Ordering::Relaxed,
                );

                match result {
                    Ok(_) => return top.into(),
                    Err(actual_top) => {
                        top = actual_top;
                    }
                }
            } else {
                return Address::null();
            }
        }
    }

    fn init_region(&self, region_id: RegionId) {
        let region = self.region(region_id);
        region.state.store(RegionState::Used);

        os::commit_at(
            region.area_start,
            region.area_size(),
            os::MemoryPermission::ReadWrite,
        );
    }

    fn find_free_region(&self) -> Option<RegionId> {
        for region_idx in 0..self.number_regions {
            let region = self.region(region_idx.into());

            if region.state.load() == RegionState::Free {
                return Some(region_idx.into());
            }
        }

        None
    }

    fn region(&self, id: RegionId) -> &HeapRegion {
        &self.regions[id.to_usize()]
    }
}

struct HeapRegion {
    // Object area in region.
    area_start: Address,
    area_end: Address,

    // Separator between used & free bytes in region.
    top: AtomicUsize,

    // Current region state.
    state: AtomicRegionState,

    // Number of live bytes after marking.
    live_bytes: AtomicUsize,
}

impl HeapRegion {
    fn area_size(&self) -> usize {
        self.area_end.to_usize() - self.area_start.to_usize()
    }
}

#[derive(FromPrimitive, ToPrimitive, PartialEq, Eq)]
enum RegionState {
    Free,
    Used,
}

struct AtomicRegionState {
    value: AtomicUsize,
}

impl AtomicRegionState {
    fn new(state: RegionState) -> AtomicRegionState {
        AtomicRegionState {
            value: AtomicUsize::new(state as usize),
        }
    }

    fn load(&self) -> RegionState {
        FromPrimitive::from_usize(self.value.load(Ordering::SeqCst)).unwrap()
    }

    fn store(&self, state: RegionState) {
        self.value.store(state as usize, Ordering::SeqCst);
    }
}

/// round the given value up to the nearest multiple of a generation
pub fn align_region(value: usize) -> usize {
    let align = REGION_SIZE_BITS;
    // we know that region size is power of 2, hence
    // we can use shifts instead of expensive division
    ((value + (1 << align) - 1) >> align) << align
}

pub struct RegionSet {
    bits: FixedBitSet,
}

impl RegionSet {
    fn new(regions: usize) -> RegionSet {
        RegionSet {
            bits: FixedBitSet::with_capacity(regions),
        }
    }

    fn clear(&mut self) {
        self.bits.clear();
    }
}

#[derive(Clone, Copy, Debug)]
pub struct RegionId(usize);

impl RegionId {
    fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for RegionId {
    fn from(val: usize) -> RegionId {
        RegionId(val)
    }
}

impl fmt::Display for RegionId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_usize())
    }
}
