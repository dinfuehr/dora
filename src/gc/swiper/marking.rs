use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::Duration;

use crossbeam_deque::{self as deque, Pop, Steal, Stealer, Worker};
use rand::distributions::{Distribution, Uniform};
use rand::thread_rng;
use threadpool::ThreadPool;

use gc::root::Slot;
use gc::{Address, Region};

pub fn start(
    rootset: &[Slot],
    heap: Region,
    perm: Region,
    number_workers: usize,
    threadpool: &ThreadPool,
) {
    let mut workers = Vec::with_capacity(number_workers);
    let mut stealers = Vec::with_capacity(number_workers);

    for _ in 0..number_workers {
        let (w, s) = deque::lifo();
        workers.push(w);
        stealers.push(s);
    }

    for root in rootset {
        let root_ptr = root.get();

        if heap.contains(root_ptr) {
            let root_obj = root_ptr.to_mut_obj();

            if !root_obj.header().is_marked_non_atomic() {
                root_obj.header_mut().mark_non_atomic();
                workers[0].push(root_ptr);
            }
        } else {
            debug_assert!(root_ptr.is_null() || perm.contains(root_ptr));
        }
    }

    let nworkers_stage = Arc::new(AtomicUsize::new(number_workers));

    for (task_id, worker) in workers.into_iter().enumerate() {
        let heap_region = heap.clone();
        let perm_region = perm.clone();

        let stealers = stealers.clone();
        let nworkers_stage = nworkers_stage.clone();

        threadpool.execute(move || {
            let mut task = MarkingTask {
                task_id: task_id,
                local: Segment::new(),
                worker: worker,
                stealers: stealers,
                nworkers: nworkers_stage,
                heap_region: heap_region,
                perm_region: perm_region,
            };

            task.run();
        });
    }

    threadpool.join();
}

struct MarkingTask {
    task_id: usize,
    local: Segment,
    worker: Worker<Address>,
    stealers: Vec<Stealer<Address>>,
    nworkers: Arc<AtomicUsize>,
    heap_region: Region,
    perm_region: Region,
}

impl MarkingTask {
    fn pop(&mut self) -> Option<Address> {
        self.pop_local()
            .or_else(|| self.pop_worker())
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
        loop {
            match self.worker.pop() {
                Pop::Empty => break,
                Pop::Data(address) => return Some(address),
                Pop::Retry => continue,
            }
        }

        None
    }

    fn steal(&self) -> Option<Address> {
        if self.stealers.len() == 1 {
            return None;
        }

        let mut rng = thread_rng();
        let range = Uniform::new(0, self.stealers.len());

        for _ in 0..2 * self.stealers.len() {
            let mut stealer_id = self.task_id;

            while stealer_id == self.task_id {
                stealer_id = range.sample(&mut rng);
            }

            let stealer = &self.stealers[stealer_id];

            loop {
                match stealer.steal() {
                    Steal::Empty => break,
                    Steal::Data(address) => return Some(address),
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
            } else if self.try_terminate() {
                break;
            } else {
                continue;
            };

            let object = object_addr.to_mut_obj();

            object.visit_reference_fields(|field| {
                self.trace(field);
            });
        }
    }

    fn try_terminate(&mut self) -> bool {
        decrease_workers(self.nworkers.as_ref());
        thread::sleep(Duration::from_micros(1));
        zero_or_increase_workers(self.nworkers.as_ref())
    }

    fn trace(&mut self, slot: Slot) {
        let field_addr = slot.get();

        if self.heap_region.contains(field_addr) {
            let field_obj = field_addr.to_mut_obj();

            if field_obj.header().try_mark_non_atomic() {
                if self.local.has_capacity() {
                    self.local.push(field_addr);
                } else {
                    self.worker.push(field_addr);
                }
            }
        } else {
            debug_assert!(field_addr.is_null() || self.perm_region.contains(field_addr));
        }
    }
}

fn decrease_workers(atomic: &AtomicUsize) -> bool {
    atomic.fetch_sub(1, Ordering::SeqCst) == 1
}

fn zero_or_increase_workers(atomic: &AtomicUsize) -> bool {
    let mut nworkers = atomic.load(Ordering::Relaxed);

    loop {
        if nworkers == 0 {
            return true;
        }

        let prev_nworkers = atomic.compare_and_swap(nworkers, nworkers + 1, Ordering::SeqCst);

        if nworkers == prev_nworkers {
            return false;
        }

        nworkers = prev_nworkers;
    }
}

const SEGMENT_SIZE: usize = 8;

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
}
