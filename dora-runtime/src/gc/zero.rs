use crate::gc::bump::BumpAllocator;
use crate::gc::{default_readonly_space_config, Address, Collector, GcReason, Region, Space};
use crate::os::{self, MemoryPermission, Reservation};
use crate::vm::{Flags, VM};

pub struct ZeroCollector {
    start: Address,
    end: Address,
    alloc: BumpAllocator,
    reservation: Reservation,
    readonly: Space,
}

impl ZeroCollector {
    pub fn new(args: &Flags) -> ZeroCollector {
        let heap_size: usize = args.max_heap_size();

        let reservation = os::reserve_align(heap_size, 0, false);
        let start = reservation.start();
        let end = start.offset(heap_size);

        os::commit_at(start, heap_size, MemoryPermission::ReadWrite);

        let readonly_space = Space::new(default_readonly_space_config(args), "perm");

        ZeroCollector {
            start,
            end,
            alloc: BumpAllocator::new(start, end),
            reservation,
            readonly: readonly_space,
        }
    }
}

impl Collector for ZeroCollector {
    fn supports_tlab(&self) -> bool {
        true
    }

    fn alloc_tlab_area(&self, _vm: &VM, size: usize) -> Option<Region> {
        let ptr = self.alloc.bump_alloc(size);

        if ptr.is_null() {
            None
        } else {
            Some(ptr.region_start(size))
        }
    }

    fn alloc_object(&self, _vm: &VM, size: usize) -> Address {
        self.alloc.bump_alloc(size)
    }

    fn alloc_readonly(&self, _vm: &VM, size: usize) -> Address {
        self.readonly.alloc(size)
    }

    fn force_collect(&self, _: &VM, _: GcReason) {
        // do nothing
    }

    fn collect_garbage(
        &self,
        _vm: &VM,
        _threads: &[std::sync::Arc<crate::threads::DoraThread>],
        _reason: GcReason,
    ) {
        // Do nothing
    }

    fn dump_summary(&self, runtime: f32) {
        let mutator = runtime;
        let gc = 0.0f32;

        println!("GC stats: total={:.1}", runtime);
        println!("GC stats: mutator={:.1}", mutator);
        println!("GC stats: collection={:.1}", gc);

        println!(
            "GC summary: 0ms collection (0), {:.1}ms mutator, {:.1}ms total (100% mutator, 0% GC)",
            mutator, runtime,
        );
    }
}
