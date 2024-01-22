use parking_lot::Mutex;
use std::cmp::{max, min};
use std::f32;
use std::fmt;
use std::sync::Arc;
use std::time::Instant;

use crate::gc::swiper::large::LargeSpace;
use crate::gc::swiper::young::YoungGen;
use crate::gc::swiper::{align_page_down, align_page_up, CollectionKind, CommonOldGen, PAGE_SIZE};
use crate::gc::{formatted_size, AllNumbers, GcReason, M};
use crate::stdlib;
use crate::vm::{Flags, Trap, VM};

pub fn init(config: &mut HeapController, args: &Flags) {
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

    config.semi_size = young_size;

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

pub fn choose_collection_kind(
    _config: &SharedHeapConfig,
    _args: &Flags,
    young: &YoungGen,
) -> CollectionKind {
    let young_size = young.committed_size();

    return if young_size <= M {
        CollectionKind::Full
    } else {
        CollectionKind::Minor
    };
}

pub fn start(
    config: &SharedHeapConfig,
    young: &YoungGen,
    old: &dyn CommonOldGen,
    large: &LargeSpace,
) {
    let mut config = config.lock();

    config.gc_start = Some(Instant::now());
    config.start_memory_size = memory_size(young, old, large);
}

pub fn stop(
    vm: &VM,
    config: &SharedHeapConfig,
    kind: CollectionKind,
    young: &YoungGen,
    old: &dyn CommonOldGen,
    large: &LargeSpace,
    args: &Flags,
    reason: GcReason,
) {
    let mut config = config.lock();

    let gc_duration = config.gc_start.expect("not started").elapsed();
    let gc_duration_ms = gc_duration.as_secs_f32() * 1000.0f32;

    let old_size = old.committed_size() + large.committed_size();
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
    let young_size = align_page_down(target_young_size / 2) * 2;

    if old_size + young_size > config.max_heap_size {
        stdlib::trap(Trap::OOM.int());
    }

    young.resize_after_gc(vm, young_size);
    config.old_limit = config.max_heap_size - young_size;
    assert!(config.old_limit >= old_size);

    config.end_memory_size = memory_size(young, old, large);

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
        print(&*config, kind, reason, gc_duration_ms);
    }
}

fn print(config: &HeapController, kind: CollectionKind, reason: GcReason, gc_duration: f32) {
    match kind {
        CollectionKind::Minor => {
            println!(
                "GC: {} ({}) {} -> {}; {:.2} ms; {} promoted; {} copied; {} garbage",
                kind,
                reason,
                formatted_size(config.start_memory_size),
                formatted_size(config.end_memory_size),
                gc_duration,
                formatted_size(config.minor_promoted),
                formatted_size(config.minor_copied),
                formatted_size(config.minor_dead),
            );
        }

        CollectionKind::Full => {
            println!(
                "GC: {} ({}) {} -> {}; {:.2} ms",
                kind,
                reason,
                formatted_size(config.start_memory_size),
                formatted_size(config.end_memory_size),
                gc_duration,
            );
        }
    }
}

fn memory_size(young: &YoungGen, old: &dyn CommonOldGen, large: &LargeSpace) -> usize {
    young.allocated_size() + old.committed_size() + large.committed_size()
}

pub struct HeapController {
    min_heap_size: usize,
    max_heap_size: usize,

    pub semi_size: usize,
    pub old_size: usize,
    pub old_limit: usize,

    gc_start: Option<Instant>,

    start_memory_size: usize,
    end_memory_size: usize,

    pub minor_promoted: usize,
    pub minor_copied: usize,
    pub minor_dead: usize,

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

            semi_size: 0,
            old_size: 0,
            old_limit: 0,

            gc_start: None,

            start_memory_size: 0,
            end_memory_size: 0,

            minor_promoted: 0,
            minor_copied: 0,
            minor_dead: 0,

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

    pub fn full_update_refs(&self) -> Numbers {
        let values: Vec<_> = self.full_phases.iter().map(|x| x.update_refs).collect();
        calculate_numbers(&values)
    }

    pub fn full_update_refs_all(&self) -> AllNumbers {
        AllNumbers(self.full_phases.iter().map(|x| x.update_refs).collect())
    }

    pub fn full_relocate(&self) -> Numbers {
        let values: Vec<_> = self.full_phases.iter().map(|x| x.evacuate).collect();
        calculate_numbers(&values)
    }

    pub fn full_relocate_all(&self) -> AllNumbers {
        AllNumbers(self.full_phases.iter().map(|x| x.evacuate).collect())
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
    pub marking: f32,
    pub sweep: f32,
    pub evacuate: f32,
    pub update_refs: f32,
    pub reset_cards: f32,
    pub total: f32,
}

impl FullCollectorPhases {
    pub fn new() -> FullCollectorPhases {
        FullCollectorPhases {
            marking: 0f32,
            sweep: 0f32,
            update_refs: 0f32,
            evacuate: 0f32,
            reset_cards: 0f32,
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
