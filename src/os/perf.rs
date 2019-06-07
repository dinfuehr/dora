use crate::baseline::fct::JitBaselineFct;
use crate::ctxt::VM;
use dora_parser::interner::Name;

#[cfg(target_os = "linux")]
pub fn register_with_perf(jit_fct: &JitBaselineFct, vm: &VM, name: Name) {
    use std::fs::OpenOptions;
    use std::io::prelude::*;

    let pid = unsafe { libc::getpid() };
    let fname = format!("/tmp/perf-{}.map", pid);

    let mut options = OpenOptions::new();
    let mut file = options.create(true).append(true).open(&fname).unwrap();

    let code_start = jit_fct.ptr_start().to_usize();
    let code_end = jit_fct.ptr_end().to_usize();
    let name = vm.interner.str(name);

    let line = format!(
        "{:x} {:x} dora::{}\n",
        code_start,
        code_end - code_start,
        name
    );
    file.write_all(line.as_bytes()).unwrap();
}

#[cfg(not(target_os = "linux"))]
pub fn register_with_perf(_: &JitBaselineFct, _: &VM, _: Name) {
    // nothing to do
}

#[derive(Clone)]
pub struct PerfValues {
    pub l1_misses: u64,
    pub tlb_misses: u64,
}

impl PerfValues {
    pub fn new() -> PerfValues {
        PerfValues {
            l1_misses: 0,
            tlb_misses: 0,
        }
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
pub mod counters {
    use super::PerfValues;
    use parking_lot::Mutex;
    use perfcnt::linux::{CacheId, CacheOpId, CacheOpResultId, PerfCounterBuilderLinux};
    use perfcnt::{AbstractPerfCounter, PerfCounter};

    pub struct PerfCounters {
        counters: Option<Mutex<Counters>>,
    }

    struct Counters {
        total: PerfValues,
        l1_misses: PerfCounter,
        tlb_misses: PerfCounter,
    }

    impl PerfCounters {
        pub fn new(enabled: bool) -> PerfCounters {
            if enabled {
                let l1_misses = PerfCounterBuilderLinux::from_cache_event(
                    CacheId::L1D,
                    CacheOpId::Read,
                    CacheOpResultId::Miss,
                )
                .finish()
                .expect("Could not create the counter");
                let tlb_misses = PerfCounterBuilderLinux::from_cache_event(
                    CacheId::DTLB,
                    CacheOpId::Read,
                    CacheOpResultId::Miss,
                )
                .finish()
                .expect("Could not create the counter");

                let counters = Counters {
                    total: PerfValues::new(),
                    l1_misses: l1_misses,
                    tlb_misses: tlb_misses,
                };

                PerfCounters {
                    counters: Some(Mutex::new(counters)),
                }
            } else {
                PerfCounters { counters: None }
            }
        }

        pub fn start(&self) {
            if let Some(mutex) = &self.counters {
                let counters = mutex.lock();
                counters
                    .l1_misses
                    .start()
                    .expect("Can not start the counter");
                counters
                    .tlb_misses
                    .start()
                    .expect("Can not start the counter");
            }
        }

        pub fn stop(&self) -> PerfValues {
            if let Some(mutex) = &self.counters {
                let mut counters = mutex.lock();
                counters.l1_misses.stop().expect("Can not stop the counter");
                counters
                    .tlb_misses
                    .stop()
                    .expect("Can not stop the counter");

                let l1_misses = counters.l1_misses.read().expect("Can not read the counter");
                counters.total.l1_misses += l1_misses;

                let tlb_misses = counters
                    .tlb_misses
                    .read()
                    .expect("Can not read the counter");
                counters.total.tlb_misses += tlb_misses;

                counters
                    .l1_misses
                    .reset()
                    .expect("Can not reset the counter");
                counters
                    .tlb_misses
                    .reset()
                    .expect("Can not reset the counter");

                counters.total.clone()
            } else {
                PerfValues::new()
            }
        }

        pub fn get(&self) -> PerfValues {
            if let Some(mutex) = &self.counters {
                let counters = mutex.lock();
                counters.total.clone()
            } else {
                PerfValues::new()
            }
        }
    }
}

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
pub mod counters {
    use super::PerfValues;
    pub struct PerfCounters;

    impl PerfCounters {
        pub fn new(_enabled: bool) -> PerfCounters {
            PerfCounters
        }

        pub fn start(&self) {}

        pub fn stop(&self) -> PerfValues {
            PerfValues::new()
        }

        pub fn get(&self) -> PerfValues {
            PerfValues::new()
        }
    }
}
