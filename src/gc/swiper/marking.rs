use std::mem;
use std::sync::{Arc, Condvar, Mutex};

use threadpool::ThreadPool;

use gc::root::Slot;
use gc::{Address, Region};

pub fn start(rootset: &[Slot], heap: Region, perm: Region, number_workers: usize) {
    let mut shared = Shared::new();

    for root in rootset {
        let root_ptr = root.get();

        if heap.contains(root_ptr) {
            let root_obj = root_ptr.to_mut_obj();

            if !root_obj.header().is_marked() {
                root_obj.header_mut().mark();
                shared.queue.push(Segment::with(root_ptr));
            }
        } else {
            debug_assert!(root_ptr.is_null() || perm.contains(root_ptr));
        }
    }

    let shared = Arc::new((Mutex::new(shared), Condvar::new()));
    let pool = ThreadPool::with_name("gc-worker", number_workers);

    for _ in 0..number_workers {
        let heap_region = heap.clone();
        let perm_region = perm.clone();
        let shared = shared.clone();

        pool.execute(move || {
            let mut local_segment = Segment::new();
            let (shared, cvar) = (&shared.0, &shared.1);

            'outer: loop {
                let object_addr = if !local_segment.is_empty() {
                    local_segment.pop().expect("should be non-empty")
                } else {
                    let mut shared = shared.lock().unwrap();

                    loop {
                        if shared.done {
                            break 'outer;
                        }

                        match shared.queue.pop() {
                            Some(segment) => {
                                local_segment = segment;
                                continue 'outer;
                            }

                            None => {
                                shared.blocked_workers += 1;

                                if shared.blocked_workers == number_workers {
                                    shared.done = true;
                                    cvar.notify_all();
                                    break 'outer;
                                }

                                shared = cvar.wait(shared).unwrap();
                                shared.blocked_workers -= 1;
                            }
                        };
                    }
                };

                let object = object_addr.to_mut_obj();

                object.visit_reference_fields(|field| {
                    let field_addr = field.get();

                    if heap_region.contains(field_addr) {
                        let field_obj = field_addr.to_mut_obj();

                        if !field_obj.header().is_marked() {
                            field_obj.header_mut().mark();

                            if local_segment.has_capacity() {
                                local_segment.push(field_addr);
                            } else {
                                let new_local_segment = Segment::with(field_addr);
                                let old_local_segment =
                                    mem::replace(&mut local_segment, new_local_segment);
                                let mut shared = shared.lock().unwrap();
                                shared.queue.push(old_local_segment);
                                cvar.notify_one();
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

struct Shared {
    blocked_workers: usize,
    queue: Vec<Segment>,
    done: bool,
}

impl Shared {
    fn new() -> Shared {
        Shared {
            blocked_workers: 0,
            queue: Vec::new(),
            done: false,
        }
    }
}

const SEGMENT_SIZE: usize = 16;

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
        Segment {
            data: Vec::new(),
        }
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