use fixedbitset::FixedBitSet;
use parking_lot::Mutex;

use crossbeam_deque::{Injector, Steal, Stealer, Worker};
use rand::distr::uniform::{UniformSampler, UniformUsize};
use scoped_threadpool::Pool;

use crate::gc::root::Slot;
use crate::gc::swiper::terminator::Terminator;
use crate::gc::{Address, Region};
use crate::runtime::Runtime;

pub struct MarkingResult {
    pub marked_bytes: usize,
    pub live_pages: FixedBitSet,
}

impl MarkingResult {
    fn new(pages: usize) -> MarkingResult {
        MarkingResult {
            marked_bytes: 0,
            live_pages: FixedBitSet::with_capacity(pages),
        }
    }

    fn add(&mut self, other: MarkingResult) {
        self.marked_bytes += other.marked_bytes;
        self.live_pages.union_with(&other.live_pages);
    }
}

pub fn run(
    rt: &Runtime,
    rootset: &[Slot],
    heap: Region,
    perm: Region,
    page_size: usize,
    threadpool: &mut Pool,
) -> MarkingResult {
    assert!(page_size.is_power_of_two());
    assert_eq!(heap.size() % page_size, 0);

    let number_workers = threadpool.thread_count() as usize;
    let page_size_bits = page_size.trailing_zeros();

    let pages = heap.size() >> page_size_bits;
    let mut workers = Vec::with_capacity(number_workers);
    let mut stealers = Vec::with_capacity(number_workers);
    let injector = Injector::new();
    let results = Mutex::new(Vec::with_capacity(number_workers));

    for _ in 0..number_workers {
        let w = Worker::new_lifo();
        let s = w.stealer();
        workers.push(w);
        stealers.push(s);
    }

    for root in rootset {
        let root_ptr = root.get();

        if root_ptr.is_null() {
            continue;
        }

        debug_assert!(heap.contains(root_ptr) || perm.contains(root_ptr));

        if root_ptr.to_obj().header().try_mark() {
            injector.push(root_ptr);
        }
    }

    let terminator = Terminator::new(number_workers);

    threadpool.scoped(|scoped| {
        for (task_id, worker) in workers.into_iter().enumerate() {
            let heap_region = heap.clone();
            let perm_region = perm.clone();

            let injector = &injector;
            let stealers = &stealers;
            let terminator = &terminator;
            let results = &results;
            let shape_base = rt.shape_base();

            scoped.execute(move || {
                let mut task = MarkingTask {
                    task_id,
                    local: Segment::new(),
                    worker,
                    injector,
                    stealers,
                    terminator,
                    heap_region,
                    perm_region,
                    page_size_bits,
                    marked_since_share: 0,
                    shape_base,
                    result: MarkingResult::new(pages),
                };

                task.run();
                results.lock().push(task.result);
            });
        }
    });

    let mut result = MarkingResult::new(pages);
    for worker_result in results.into_inner() {
        result.add(worker_result);
    }
    result
}

struct MarkingTask<'a> {
    task_id: usize,
    local: Segment,
    worker: Worker<Address>,
    injector: &'a Injector<Address>,
    stealers: &'a [Stealer<Address>],
    terminator: &'a Terminator,
    heap_region: Region,
    perm_region: Region,
    page_size_bits: u32,
    marked_since_share: usize,
    shape_base: Address,
    result: MarkingResult,
}

impl<'a> MarkingTask<'a> {
    fn pop(&mut self) -> Option<Address> {
        self.pop_local()
            .or_else(|| self.pop_worker())
            .or_else(|| self.pop_global())
            .or_else(|| self.steal())
    }

    fn pop_local(&mut self) -> Option<Address> {
        if self.local.is_empty() {
            return None;
        }

        let obj = self.local.pop().expect("should be non-empty");
        Some(obj)
    }

    fn pop_worker(&mut self) -> Option<Address> {
        self.worker.pop()
    }

    fn pop_global(&mut self) -> Option<Address> {
        loop {
            let result = self.injector.steal_batch_and_pop(&mut self.worker);

            match result {
                Steal::Empty => break,
                Steal::Success(value) => return Some(value),
                Steal::Retry => continue,
            }
        }

        None
    }

    fn steal(&self) -> Option<Address> {
        if self.stealers.len() == 1 {
            return None;
        }

        let mut rng = rand::rng();
        let range =
            UniformUsize::new(0, self.stealers.len()).expect("failed to create UniformUsize.");

        for _ in 0..2 * self.stealers.len() {
            let mut stealer_id = self.task_id;

            while stealer_id == self.task_id {
                stealer_id = range.sample(&mut rng);
            }

            let stealer = &self.stealers[stealer_id];

            loop {
                match stealer.steal_batch_and_pop(&self.worker) {
                    Steal::Empty => break,
                    Steal::Success(address) => return Some(address),
                    Steal::Retry => continue,
                }
            }
        }

        None
    }

    fn run(&mut self) {
        loop {
            let object_addr = if let Some(object_addr) = self.pop() {
                object_addr
            } else if self.terminator.try_terminate() {
                break;
            } else {
                continue;
            };

            let object = object_addr.to_obj();
            let page_id = object_addr.offset_from(self.heap_region.start()) >> self.page_size_bits;
            self.result.live_pages.set(page_id, true);
            self.result.marked_bytes += object.size(self.shape_base);

            object.visit_reference_fields(self.shape_base, |field| {
                self.trace(field);
            });
        }
    }

    fn trace(&mut self, slot: Slot) {
        let field_addr = slot.get();

        if field_addr.is_null() {
            return;
        }

        debug_assert!(
            self.heap_region.contains(field_addr) || self.perm_region.contains(field_addr)
        );

        let field_obj = field_addr.to_obj();

        if field_obj.header().try_mark() {
            if self.local.has_capacity() {
                self.local.push(field_addr);
                self.defensive_push();
            } else {
                self.worker.push(field_addr);
            }
        }
    }

    fn defensive_push(&mut self) {
        self.marked_since_share += 1;

        if self.marked_since_share > 256 {
            if self.local.len() > 4 {
                let target_len = self.local.len() / 2;

                while self.local.len() > target_len {
                    let val = self.local.pop().unwrap();
                    self.injector.push(val);
                }
            }

            self.marked_since_share = 0;
        }
    }
}

const SEGMENT_SIZE: usize = 64;

struct Segment {
    data: Vec<Address>,
}

impl Segment {
    fn new() -> Segment {
        Segment {
            data: Vec::with_capacity(SEGMENT_SIZE),
        }
    }

    fn empty() -> Segment {
        Segment { data: Vec::new() }
    }

    fn with(addr: Address) -> Segment {
        let mut segment = Segment::new();
        segment.data.push(addr);

        segment
    }

    fn has_capacity(&self) -> bool {
        self.data.len() < SEGMENT_SIZE
    }

    fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    fn push(&mut self, addr: Address) {
        debug_assert!(self.has_capacity());
        self.data.push(addr);
    }

    fn pop(&mut self) -> Option<Address> {
        self.data.pop()
    }

    fn len(&mut self) -> usize {
        self.data.len()
    }
}
