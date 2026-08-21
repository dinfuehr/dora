use parking_lot::{Condvar, Mutex};
use std::sync::atomic::{AtomicUsize, Ordering};

pub struct Terminator {
    // Total number of workers participating in termination detection.
    total: usize,
    // Both counters are modified only while holding `lock`; atomics allow
    // `wake_up` to read them without locking on its fast path.
    working: AtomicUsize,
    // Number of notified workers that have not resumed working yet. Incremented
    // before notify and decremented after wait.
    awakening: AtomicUsize,
    lock: Mutex<()>,
    condvar: Condvar,
}

impl Terminator {
    pub fn new(number_workers: usize) -> Terminator {
        assert!(number_workers > 0);

        Terminator {
            total: number_workers,
            working: AtomicUsize::new(number_workers),
            awakening: AtomicUsize::new(0),
            lock: Mutex::new(()),
            condvar: Condvar::new(),
        }
    }

    pub fn try_terminate(&self) -> bool {
        if self.total == 1 {
            return true;
        }

        let mut guard = self.lock.lock();
        let working = self.working.load(Ordering::Relaxed);
        assert!(working > 0);
        let working = working - 1;
        self.working.store(working, Ordering::Relaxed);
        let awakening = self.awakening.load(Ordering::Relaxed);
        debug_assert!(working + awakening <= self.total);

        if working == 0 && awakening == 0 {
            self.condvar.notify_all();
            return true;
        }

        loop {
            self.condvar.wait(&mut guard);

            let working = self.working.load(Ordering::Relaxed);
            let awakening = self.awakening.load(Ordering::Relaxed);
            debug_assert!(working + awakening <= self.total);

            if working == 0 && awakening == 0 {
                return true;
            }

            if awakening > 0 {
                self.awakening.store(awakening - 1, Ordering::Relaxed);
                self.working.store(working + 1, Ordering::Relaxed);
                return false;
            }
        }
    }

    pub fn wake_up(&self) {
        if self.total == 1 {
            return;
        }

        let working = self.working.load(Ordering::Relaxed);
        let awakening = self.awakening.load(Ordering::Relaxed);
        debug_assert!(working > 0);

        if working + awakening == self.total {
            return;
        }

        let _guard = self.lock.lock();
        let working = self.working.load(Ordering::Relaxed);
        let awakening = self.awakening.load(Ordering::Relaxed);
        debug_assert!(working > 0);
        debug_assert!(working + awakening <= self.total);

        if working + awakening != self.total {
            self.awakening.store(awakening + 1, Ordering::Relaxed);
            self.condvar.notify_one();
        }
    }
}
