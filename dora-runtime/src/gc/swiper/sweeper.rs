use parking_lot::{Condvar, Mutex, RwLock};

use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

use crate::gc::swiper::{get_swiper, RegularPage};
use crate::gc::{Address, Region};
use crate::vm::{get_vm, VM};

pub struct Sweeper {
    pages_to_sweep: RwLock<Vec<RegularPage>>,
    next_page_idx: AtomicUsize,
    in_progress: AtomicBool,
    running_workers: Mutex<usize>,
    worker_joined: Condvar,
}

impl Sweeper {
    pub fn new() -> Sweeper {
        Sweeper {
            pages_to_sweep: RwLock::new(Vec::new()),
            next_page_idx: AtomicUsize::new(0),
            in_progress: AtomicBool::new(false),
            running_workers: Mutex::new(0),
            worker_joined: Condvar::new(),
        }
    }

    pub fn in_progress(&self) -> bool {
        self.in_progress.load(Ordering::SeqCst)
    }

    pub fn start(&self, pages: Vec<RegularPage>, vm: &VM) {
        let workers = vm.flags.gc_workers();
        self.reset(pages, workers);

        let swiper = get_swiper(vm);

        for _ in 0..workers {
            swiper.concurrent_threadpool.execute(move || {
                let vm = get_vm();
                let swiper = get_swiper(vm);
                swiper.sweeper.sweep_task(vm);
            });
        }
    }

    fn reset(&self, pages: Vec<RegularPage>, workers: usize) {
        let mut pages_to_sweep = self.pages_to_sweep.try_write().expect("lock failed");
        *pages_to_sweep = pages;
        self.next_page_idx.store(0, Ordering::Relaxed);
        self.in_progress.store(true, Ordering::SeqCst);
        *self.running_workers.lock() = workers;
    }

    pub fn sweep_task(&self, vm: &VM) {
        self.sweep_pages(vm);
        self.decrement_workers();
    }

    pub fn sweep_in_allocation(&self, vm: &VM) {
        let pages_to_sweep = self.pages_to_sweep.try_read().expect("lock failed");

        while let Some(&page) = pages_to_sweep.get(self.next_page_idx()) {
            if !sweep_page(vm, page) {
                return;
            }
        }
    }

    pub fn sweep_to_end(&self, vm: &VM) {
        if !self.in_progress() {
            return;
        }

        self.sweep_pages(vm);
        self.join();
    }

    fn sweep_pages(&self, vm: &VM) {
        let pages_to_sweep = self.pages_to_sweep.try_read().expect("lock failed");

        while let Some(&page) = pages_to_sweep.get(self.next_page_idx()) {
            sweep_page(vm, page);
        }
    }

    fn decrement_workers(&self) {
        let mut running = self.running_workers.lock();
        assert!(*running > 0);
        *running -= 1;

        if *running == 0 {
            self.worker_joined.notify_all();
            self.in_progress.store(false, Ordering::SeqCst);
        }
    }

    pub fn join(&self) {
        let mut running = self.running_workers.lock();

        while *running > 0 {
            self.worker_joined.wait(&mut running);
        }
    }

    fn next_page_idx(&self) -> usize {
        self.next_page_idx.fetch_add(1, Ordering::Relaxed)
    }
}

fn sweep_page(vm: &VM, page: RegularPage) -> bool {
    let swiper = get_swiper(vm);
    let old = &swiper.old;
    let (live, free_regions) = sweep_page_for_free_memory(vm, page);

    if live == 0 {
        old.free_page(vm, page);
        swiper.heap.merge_free_regions();
        true
    } else {
        old.add_to_free_list(vm, free_regions);
        false
    }
}

fn sweep_page_for_free_memory(vm: &VM, page: RegularPage) -> (usize, Vec<Region>) {
    let region = page.object_area();
    let mut scan = region.start;
    let mut free_start = region.start;
    let mut live = 0;
    let mut free_regions = Vec::new();
    let metaspace_start = vm.meta_space_start();

    while scan < region.end {
        let object = scan.to_obj();

        if object.is_filler(vm) {
            scan = scan.offset(object.size(metaspace_start));
        } else {
            let object_size = object.size(metaspace_start);
            let object_end = scan.offset(object_size);

            if object.header().is_marked() {
                handle_free_region(&mut free_regions, free_start, scan);
                object.header().clear_mark();
                assert!(!object.header().is_remembered());
                free_start = object_end;
                live += object_size;
            }

            scan = object_end
        }
    }

    handle_free_region(&mut free_regions, free_start, scan);
    assert_eq!(scan, region.end);

    (live, free_regions)
}

fn handle_free_region(free_regions: &mut Vec<Region>, start: Address, end: Address) {
    assert!(start <= end);

    if start == end {
        return;
    }

    free_regions.push(Region::new(start, end));
}
