use parking_lot::Mutex;
use std::cmp::{max, min};
use std::f32;
use std::fmt;
use std::sync::Arc;
use std::time::Instant;

use crate::gc::swiper::young::YoungGen;
use crate::gc::swiper::{CollectionKind, Heap, PAGE_SIZE, align_page_down, align_page_up};
use crate::gc::{AllNumbers, GcReason, formatted_size, report_out_of_memory_error};
use crate::threads::DoraThread;
use crate::vm::{VM, VmFlags};

pub fn init(config: &mut HeapController, args: &VmFlags) {
    assert!(config.min_heap_size <= config.max_heap_size);

    let young_size = if let Some(young_size) = args.young_size() {
        min(align_page_up(young_size), config.max_heap_size - PAGE_SIZE)
    } else if args.young_appel() {
        let max_heap_size = (config.max_heap_size as f64 * 0.9) as usize;
        align_page_down(max_heap_size / 2)
    } else {
        unreachable!();
    };

    let young_size = max(young_size, 2 * PAGE_SIZE);
    let young_size = align_page_up(young_size / 2) * 2;

    config.young_size = young_size;

    let max_old_limit = config.max_heap_size - young_size;
    let min_old_limit = if config.min_heap_size > young_size {
        config.min_heap_size - young_size
    } else {
        0
    };

    let old_limit = align_page_down(config.max_heap_size - young_size);
    let old_limit = min(old_limit, max_old_limit);
    let old_limit = max(old_limit, min_old_limit);

    config.old_size = 0;
    config.old_limit = old_limit;
}

pub fn start(config: &SharedHeapConfig, heap: &Heap) {
    let mut config = config.lock();

    config.gc_start = Some(Instant::now());
    config.start_memory_size = heap.committed_size();
}

pub fn stop(
    vm: &VM,
    config: &SharedHeapConfig,
    kind: CollectionKind,
    heap: &Heap,
    young: &YoungGen,
    args: &VmFlags,
    reason: GcReason,
    threads: &[Arc<DoraThread>],
) {
    let mut config = config.lock();

    let gc_duration = config.gc_start.expect("not started").elapsed();
    let gc_duration_ms = gc_duration.as_secs_f32() * 1000.0f32;

    let committed_sizes = heap.committed_sizes();
    let old_size = committed_sizes.old + committed_sizes.large;
    config.old_size = old_size;

    let max_young_size = if let Some(young_size) = args.young_size() {
        align_page_up(young_size)
    } else {
        std::usize::MAX
    };

    let min_semi_size = young.allocated_size();
    assert_eq!(min_semi_size % PAGE_SIZE, 0);

    let rest = config.max_heap_size - config.old_size;
    let target_young_size = rest / 2;
    let target_young_size = min(target_young_size, max_young_size);
    let target_young_size = max(target_young_size, 2 * min_semi_size);
    let target_young_size = max(target_young_size, 2 * PAGE_SIZE);
    let young_size = align_page_down(target_young_size / 2) * 2;

    if old_size + young_size > config.max_heap_size {
        report_out_of_memory_error(vm, threads);
    }

    young.resize_after_gc(vm, young_size);
    config.old_limit = config.max_heap_size - young_size;
    assert!(config.old_limit >= old_size);

    config.end_memory_size = heap.committed_size();

    assert!(young_size + config.old_limit <= config.max_heap_size);

    match kind {
        CollectionKind::Minor => {
            config.total_minor_collections += 1;
            config.total_minor_pause += gc_duration_ms;

            if args.gc_stats {
                config.minor_phases.last_mut().unwrap().total = gc_duration_ms;
            }
        }

        CollectionKind::Full => {
            config.total_full_collections += 1;
            config.total_full_pause += gc_duration_ms;

            if args.gc_stats {
                config.full_phases.last_mut().unwrap().total = gc_duration_ms;
            }
        }
    }

    if args.gc_verbose {
        print(vm, &*config, kind, gc_duration_ms, reason);
    }
}

fn print(
    vm: &VM,
    config: &HeapController,
    kind: CollectionKind,
    gc_duration: f32,
    reason: GcReason,
) {
    let timestamp = config
        .gc_start
        .expect("missing timestamp")
        .duration_since(vm.startup_time());

    match kind {
        CollectionKind::Minor => {
            println!(
                "[{}.{:03}] Minor GC: {} -> {}; {:.2} ms; {} promoted; {} copied; {}",
                timestamp.as_secs(),
                timestamp.subsec_millis(),
                formatted_size(config.start_memory_size),
                formatted_size(config.end_memory_size),
                gc_duration,
                formatted_size(config.minor_promoted),
                formatted_size(config.minor_copied),
                reason,
            );
        }

        CollectionKind::Full => {
            println!(
                "[{}.{:03}] Full GC: {} -> {}; {:.2} ms; {} marked",
                timestamp.as_secs(),
                timestamp.subsec_millis(),
                formatted_size(config.start_memory_size),
                formatted_size(config.end_memory_size),
                gc_duration,
                formatted_size(config.marked_bytes),
            );
        }
    }
}

pub struct HeapController {
    min_heap_size: usize,
    max_heap_size: usize,

    pub young_size: usize,
    pub old_size: usize,
    pub old_limit: usize,

    gc_start: Option<Instant>,

    start_memory_size: usize,
    end_memory_size: usize,
    pub marked_bytes: usize,

    pub minor_promoted: usize,
    pub minor_copied: usize,

    pub total_minor_collections: usize,
    pub total_minor_pause: f32,
    pub total_full_collections: usize,
    pub total_full_pause: f32,

    full_phases: Vec<FullCollectorPhases>,
    minor_phases: Vec<MinorCollectorPhases>,
}

impl HeapController {
    pub fn new(min_heap_size: usize, max_heap_size: usize) -> HeapController {
        assert!(min_heap_size <= max_heap_size);

        HeapController {
            min_heap_size,
            max_heap_size,

            young_size: 0,
            old_size: 0,
            old_limit: 0,

            gc_start: None,

            start_memory_size: 0,
            end_memory_size: 0,

            marked_bytes: 0,
            minor_promoted: 0,
            minor_copied: 0,

            total_minor_collections: 0,
            total_minor_pause: 0f32,
            total_full_collections: 0,
            total_full_pause: 0f32,

            full_phases: Vec::new(),
            minor_phases: Vec::new(),
        }
    }

    pub fn add_full(&mut self, phases: FullCollectorPhases) {
        self.full_phases.push(phases);
    }

    pub fn add_minor(&mut self, phases: MinorCollectorPhases) {
        self.minor_phases.push(phases);
    }

    pub fn full_complete(&self) -> Numbers {
        let values: Vec<_> = self.full_phases.iter().map(|x| x.complete).collect();
        calculate_numbers(&values)
    }

    pub fn full_marking(&self) -> Numbers {
        let values: Vec<_> = self.full_phases.iter().map(|x| x.marking).collect();
        calculate_numbers(&values)
    }

    pub fn full_marking_all(&self) -> AllNumbers {
        AllNumbers(self.full_phases.iter().map(|x| x.marking).collect())
    }

    pub fn full_sweep(&self) -> Numbers {
        let values: Vec<_> = self.full_phases.iter().map(|x| x.sweep).collect();
        calculate_numbers(&values)
    }

    pub fn full_sweep_all(&self) -> AllNumbers {
        AllNumbers(self.full_phases.iter().map(|x| x.sweep).collect())
    }

    pub fn full_total(&self) -> Numbers {
        let values: Vec<_> = self.full_phases.iter().map(|x| x.total).collect();
        calculate_numbers(&values)
    }

    pub fn full_total_all(&self) -> AllNumbers {
        AllNumbers(self.full_phases.iter().map(|x| x.total).collect())
    }

    pub fn minor_roots(&self) -> Numbers {
        let values: Vec<_> = self.minor_phases.iter().map(|x| x.roots).collect();
        calculate_numbers(&values)
    }

    pub fn minor_roots_all(&self) -> AllNumbers {
        AllNumbers(self.minor_phases.iter().map(|x| x.roots).collect())
    }

    pub fn minor_tracing(&self) -> Numbers {
        let values: Vec<_> = self.minor_phases.iter().map(|x| x.tracing).collect();
        calculate_numbers(&values)
    }

    pub fn minor_tracing_all(&self) -> AllNumbers {
        AllNumbers(self.minor_phases.iter().map(|x| x.tracing).collect())
    }

    pub fn minor_total(&self) -> Numbers {
        let values: Vec<_> = self.minor_phases.iter().map(|x| x.total).collect();
        calculate_numbers(&values)
    }

    pub fn minor_total_all(&self) -> AllNumbers {
        AllNumbers(self.minor_phases.iter().map(|x| x.total).collect())
    }

    pub fn grow_old(&mut self, size: usize) -> bool {
        if self.old_size + size <= self.old_limit {
            self.old_size += size;
            true
        } else {
            false
        }
    }
}

pub struct Numbers {
    pub avg: f32,
    pub sum: f32,
    pub min: f32,
    pub max: f32,
}

impl Numbers {
    fn zero() -> Numbers {
        Numbers {
            avg: 0f32,
            sum: 0f32,
            min: 0f32,
            max: 0f32,
        }
    }
}

impl fmt::Display for Numbers {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "avg={:<8.1} sum={:<8.1} min={:<8.1} max={:<8.1}",
            self.avg, self.sum, self.min, self.max
        )
    }
}

fn calculate_numbers(data: &[f32]) -> Numbers {
    if data.len() == 0 {
        return Numbers::zero();
    }

    let mut sum = 0f32;
    let mut xmin = data[0];
    let mut xmax = data[0];

    for el in data {
        sum += *el;
        xmin = f32::min(xmin, *el);
        xmax = f32::max(xmax, *el);
    }

    let avg = sum / (data.len() as f32);

    Numbers {
        avg,
        sum,
        min: xmin,
        max: xmax,
    }
}

pub type SharedHeapConfig = Arc<Mutex<HeapController>>;

#[derive(Clone)]
pub struct FullCollectorPhases {
    pub complete: f32,
    pub marking: f32,
    pub sweep: f32,
    pub total: f32,
}

impl FullCollectorPhases {
    pub fn new() -> FullCollectorPhases {
        FullCollectorPhases {
            complete: 0f32,
            marking: 0f32,
            sweep: 0f32,
            total: 0f32,
        }
    }
}

#[derive(Clone)]
pub struct MinorCollectorPhases {
    pub roots: f32,
    pub tracing: f32,
    pub total: f32,
}

impl MinorCollectorPhases {
    pub fn new() -> MinorCollectorPhases {
        MinorCollectorPhases {
            roots: 0f32,
            tracing: 0f32,
            total: 0f32,
        }
    }
}
