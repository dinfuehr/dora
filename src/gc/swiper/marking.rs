use std::mem;

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
                workers[0].push(Segment::with(root_ptr));
            }
        } else {
            debug_assert!(root_ptr.is_null() || perm.contains(root_ptr));
        }
    }

    // let shared = Arc::new((Mutex::new(shared), Condvar::new()));
    let pool = ThreadPool::with_name("gc-worker".to_string(), number_workers);

    for task_id in 0..number_workers {
        let heap_region = heap.clone();
        let perm_region = perm.clone();

        let worker = workers.pop().unwrap();
        let stealers = stealers.clone();

        pool.execute(move || {
            let mut local_segment = Segment::new();
            let mut traced_objects = 0;

            loop {
                let object_addr = if !local_segment.is_empty() {
                    local_segment.pop().expect("should be non-empty")
                } else {
                    if let Some(segment) = pop(&worker, &stealers) {
                        local_segment = segment;
                        continue;

                    } else {
                        break;
                    }
                };

                let object = object_addr.to_mut_obj();
                traced_objects += 1;

                object.visit_reference_fields(|field| {
                    let field_addr = field.get();

                    if heap_region.contains(field_addr) {
                        let field_obj = field_addr.to_mut_obj();

                        if field_obj.header().try_mark() {
                            if local_segment.has_capacity() {
                                local_segment.push(field_addr);
                            } else {
                                let new_local_segment = Segment::with(field_addr);
                                let old_local_segment =
                                    mem::replace(&mut local_segment, new_local_segment);
                                worker.push(old_local_segment);
                            }
                        }
                    } else {
                        debug_assert!(field_addr.is_null() || perm_region.contains(field_addr));
                    }
                });
            }

            println!("task {}: traced objects: {}", task_id, traced_objects);
        });
    }

    pool.join();
}

fn pop(worker: &Worker<Segment>, stealers: &[Stealer<Segment>]) -> Option<Segment> {
    loop {
        match worker.pop() {
            Pop::Empty => break,
            Pop::Data(segment) => return Some(segment),
            Pop::Retry => continue,
        }
    }

    for stealer in stealers {
        loop {
            match stealer.steal() {
                Steal::Empty => break,
                Steal::Data(segment) => return Some(segment),
                Steal::Retry => continue,
            }
        }
    }

    None
}

const SEGMENT_SIZE: usize = 32;

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
