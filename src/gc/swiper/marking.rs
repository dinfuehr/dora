use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::Duration;

use crossbeam_deque::{Steal, Stealer, Worker};
use rand::distributions::{Distribution, Uniform};
use rand::thread_rng;
use scoped_threadpool::Pool;

use gc::root::Slot;
use gc::{Address, Region};

pub fn start(rootset: &[Slot], heap: Region, perm: Region, threadpool: &mut Pool) {
    let number_workers = threadpool.thread_count() as usize;
    let mut workers = Vec::with_capacity(number_workers);
    let mut stealers = Vec::with_capacity(number_workers);

    for _ in 0..number_workers {
        let w = Worker::new_lifo();
        let s = w.stealer();
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

    let terminator = Arc::new(Terminator::new(number_workers));

    threadpool.scoped(|scoped| {
        for (task_id, worker) in workers.into_iter().enumerate() {
            let heap_region = heap.clone();
            let perm_region = perm.clone();

            let stealers = stealers.clone();
            let terminator = terminator.clone();

            scoped.execute(move || {
                let mut task = MarkingTask {
                    task_id: task_id,
                    local: Segment::new(),
                    worker: worker,
                    stealers: stealers,
                    terminator: terminator,
                    heap_region: heap_region,
                    perm_region: perm_region,
                    marked: 0,
                };

                task.run();
            });
        }
    });
}

pub struct Terminator {
    nworkers: AtomicUsize,
}

impl Terminator {
    pub fn new(number_workers: usize) -> Terminator {
        Terminator {
            nworkers: AtomicUsize::new(number_workers),
        }
    }

    pub fn try_terminate(&self) -> bool {
        self.decrease_workers();
        thread::sleep(Duration::from_micros(1));
        self.zero_or_increase_workers()
    }

    fn decrease_workers(&self) -> bool {
        self.nworkers.fetch_sub(1, Ordering::SeqCst) == 1
    }

    fn zero_or_increase_workers(&self) -> bool {
        let mut nworkers = self.nworkers.load(Ordering::Relaxed);

        loop {
            if nworkers == 0 {
                return true;
            }

            let prev_nworkers =
                self.nworkers
                    .compare_and_swap(nworkers, nworkers + 1, Ordering::SeqCst);

            if nworkers == prev_nworkers {
                return false;
            }

            nworkers = prev_nworkers;
        }
    }
}

struct MarkingTask {
    task_id: usize,
    local: Segment,
    worker: Worker<Address>,
    stealers: Vec<Stealer<Address>>,
    terminator: Arc<Terminator>,
    heap_region: Region,
    perm_region: Region,
    marked: usize,
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
        self.worker.pop()
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

            let object = object_addr.to_mut_obj();

            object.visit_reference_fields(|field| {
                self.trace(field);
            });
        }
    }

    fn trace(&mut self, slot: Slot) {
        let field_addr = slot.get();

        if self.heap_region.contains(field_addr) {
            let field_obj = field_addr.to_mut_obj();

            if field_obj.header().try_mark_non_atomic() {
                if self.local.has_capacity() {
                    self.local.push(field_addr);
                    self.defensive_push();
                } else {
                    self.worker.push(field_addr);
                }
            }
        } else {
            debug_assert!(field_addr.is_null() || self.perm_region.contains(field_addr));
        }
    }

    fn defensive_push(&mut self) {
        self.marked += 1;

        if self.marked > 256 {
            if self.local.len() > 4 {
                let target_len = self.local.len() / 2;

                while self.local.len() > target_len {
                    let val = self.local.pop().unwrap();
                    self.worker.push(val);
                }
            }

            self.marked = 0;
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
