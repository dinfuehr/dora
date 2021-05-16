use crate::gc::Region;
use crate::os;

pub const REGION_SIZE: usize = 1 << 20;

pub struct RegionCollector {
    total: Region,
}

impl RegionCollector {
    fn new() -> RegionCollector {
        let heap_size = 128 * REGION_SIZE;
        let reservation = os::reserve_align(heap_size, REGION_SIZE, false);

        RegionCollector {
            total: reservation.start.region_start(heap_size),
        }
    }
}
