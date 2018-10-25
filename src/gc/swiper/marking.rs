use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;
use std::thread;

use crossbeam_deque::{self as deque, Pop, Steal, Stealer, Worker};
use threadpool::ThreadPool;

use gc::root::Slot;
use gc::{Address, Region};

pub fn start(rootset: &[Slot], heap: Region, perm: Region, number_workers: usize) {
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

    let pool = ThreadPool::with_name("gc-worker".to_string(), number_workers);
    let nworkers_stage = Arc::new(AtomicUsize::new(number_workers));

    for (task_id, worker) in workers.into_iter().enumerate() {
        let heap_region = heap.clone();
        let perm_region = perm.clone();

        let stealers = stealers.clone();
        let nworkers_stage = nworkers_stage.clone();

        pool.execute(move || {
            let mut local_segment = Segment::new();

            loop {
                let object_addr = if !local_segment.is_empty() {
                    local_segment.pop().expect("should be non-empty")
                } else {
                    if let Some(address) = pop(task_id, &worker, &stealers) {
                        address
                    } else if try_terminate(&nworkers_stage) {
                        break;
                    } else {
                        continue;
                    }
                };

                let object = object_addr.to_mut_obj();

                object.visit_reference_fields(|field| {
                    let field_addr = field.get();

                    if heap_region.contains(field_addr) {
                        let field_obj = field_addr.to_mut_obj();

                        if field_obj.header().try_mark() {
                            if local_segment.has_capacity() {
                                local_segment.push(field_addr);
                            } else {
                                // let new_local_segment = Segment::with(field_addr);
                                // let old_local_segment =
                                    // mem::replace(&mut local_segment, new_local_segment);
                                worker.push(field_addr);
                            }
                        }
                    } else {
                        debug_assert!(field_addr.is_null() || perm_region.contains(field_addr));
                    }
                });
            }
        });
    }

    pool.join();
}

fn pop(_task_id: usize, worker: &Worker<Address>, stealers: &[Stealer<Address>]) -> Option<Address> {
    loop {
        match worker.pop() {
            Pop::Empty => break,
            Pop::Data(address) => {
                return Some(address)
            }
            Pop::Retry => continue,
        }
    }

    for stealer in stealers {
        loop {
            match stealer.steal() {
                Steal::Empty => break,
                Steal::Data(address) => {
                    return Some(address)
                }
                Steal::Retry => continue,
            }
        }
    }

    None
}

fn try_terminate(nworkers_stage: &AtomicUsize) -> bool {
    enter_stage(nworkers_stage);
    thread::sleep(Duration::from_millis(1));
    !try_exit(nworkers_stage)
}

fn enter_stage(atomic: &AtomicUsize) -> bool {
    atomic.fetch_sub(1, Ordering::SeqCst) == 1
}

fn try_exit(atomic: &AtomicUsize) -> bool {
    let mut nworkers = atomic.load(Ordering::Relaxed);

    loop {
        if nworkers == 0 {
            return false;
        }
        let prev_nworkers = atomic.compare_and_swap(nworkers, nworkers + 1, Ordering::SeqCst);

        if nworkers == prev_nworkers {
            return true;
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
