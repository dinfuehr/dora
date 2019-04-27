use parking_lot::Mutex;
use std::cmp::{max, min};
use std::f32;
use std::fmt;
use std::sync::Arc;

use driver::cmd::Args;
use gc::swiper::large::LargeSpace;
use gc::swiper::old::OldGen;
use gc::swiper::young::YoungGen;
use gc::swiper::CollectionKind;
use gc::{align_gen, align_gen_down, formatted_size, GcReason, GEN_SIZE, M};
use mem;
use os::signal::Trap;
use stdlib;
use timer;

const MAX_YOUNG_SIZE: usize = 20 * M;

const INIT_HEAP_SIZE_RATIO: usize = 2;
const INIT_YOUNG_RATIO: usize = 4;
const INIT_SEMI_RATIO: usize = 3;

pub fn init(config: &mut HeapConfig, args: &Args) {
    assert!(config.min_heap_size <= config.max_heap_size);

    let young_size = if let Some(young_size) = args.young_size() {
        min(align_gen(young_size), config.max_heap_size - GEN_SIZE)
    } else if args.young_appel() {
        let max_heap_size = (config.max_heap_size as f64 * 0.9) as usize;
        align_gen(max_heap_size / 2)
    } else {
        unreachable!();
    };

    let young_size = max(young_size, GEN_SIZE);
    let (eden_size, semi_size) = calculate_young_size(args, young_size, 0);

    config.eden_size = eden_size;
    config.semi_size = semi_size;

    let max_old_limit = config.max_heap_size - young_size;
    let min_old_limit = if config.min_heap_size > young_size {
        config.min_heap_size - young_size
    } else {
        0
    };

    let old_limit = align_gen(config.max_heap_size / INIT_HEAP_SIZE_RATIO);
    let old_limit = min(old_limit, max_old_limit);
    let old_limit = max(old_limit, min_old_limit);

    config.old_size = 0;
    config.old_limit = old_limit;
}

fn calculate_young_size(args: &Args, young_size: usize, min_semi_size: usize) -> (usize, usize) {
    let semi_ratio = args.flag_gc_semi_ratio.unwrap_or(INIT_SEMI_RATIO);
    let semi_size = if semi_ratio == 0 {
        0
    } else {
        align_gen(young_size / semi_ratio)
    };

    let semi_size = max(semi_size, min_semi_size);
    let semi_size = max(semi_size, GEN_SIZE);
    let eden_size = young_size - semi_size;

    (eden_size, semi_size)
}

pub fn choose_collection_kind(
    _config: &SharedHeapConfig,
    _args: &Args,
    young: &YoungGen,
) -> CollectionKind {
    let (eden_size, semi_size) = young.committed_size();
    let young_size = eden_size + semi_size;

    return if young_size <= M {
        CollectionKind::Full
    } else {
        CollectionKind::Minor
    };
}

pub fn start(config: &SharedHeapConfig, young: &YoungGen, old: &OldGen, large: &LargeSpace) {
    let mut config = config.lock();

    config.gc_start = timer::timestamp();
    config.start_object_size = object_size(young, old, large);
    config.start_memory_size = memory_size(young, old, large);
}

pub fn stop(
    config: &SharedHeapConfig,
    kind: CollectionKind,
    young: &YoungGen,
    old: &OldGen,
    large: &LargeSpace,
    args: &Args,
    reason: GcReason,
) {
    let mut config = config.lock();

    let gc_end = timer::timestamp();
    config.gc_duration = timer::in_ms(gc_end - config.gc_start);

    assert!(young.eden_active().empty());
    assert!(young.from_active().empty());

    let old_size = old.committed_size() + large.committed_size();
    config.old_size = old_size;

    let max_young_size = if let Some(young_size) = args.young_size() {
        align_gen(young_size)
    } else {
        std::usize::MAX
    };

    let rest = config.max_heap_size - config.old_size;
    let target_young_size = align_gen_down(rest / 2);
    let target_young_size = min(target_young_size, max_young_size);
    let target_young_size = max(target_young_size, GEN_SIZE);

    let to_size = young.to_active().size();
    let min_semi_size = align_gen(mem::page_align(to_size) * 2);

    let (eden_size, semi_size) = calculate_young_size(args, target_young_size, min_semi_size);
    let young_size = eden_size + semi_size;

    if old_size + young_size > config.max_heap_size {
        stdlib::trap(Trap::OOM.int());
    }

    young.set_limit(eden_size, semi_size);
    config.old_limit = config.max_heap_size - young_size;
    assert!(config.old_limit >= old_size);

    config.end_object_size = object_size(young, old, large);
    config.end_memory_size = memory_size(young, old, large);

    assert!(young_size + config.old_limit <= config.max_heap_size);

    match kind {
        CollectionKind::Minor => {
            config.total_minor_collections += 1;
            config.total_minor_pause += config.gc_duration;

            if args.flag_gc_stats {
                config.minor_phases.last_mut().unwrap().total = config.gc_duration;
            }
        }

        CollectionKind::Full => {
            config.total_full_collections += 1;
            config.total_full_pause += config.gc_duration;

            if args.flag_gc_stats {
                config.full_phases.last_mut().unwrap().total = config.gc_duration;
            }
        }
    }

    if args.flag_gc_verbose {
        print(&*config, kind, reason);
    }
}

fn print(config: &HeapConfig, kind: CollectionKind, reason: GcReason) {
    match kind {
        CollectionKind::Minor => {
            println!(
                "GC: {} ({}) {}/{} -> {}/{}; {:.2} ms; {} promoted; {} copied",
                kind,
                reason,
                formatted_size(config.start_object_size),
                formatted_size(config.start_memory_size),
                formatted_size(config.end_object_size),
                formatted_size(config.end_memory_size),
                config.gc_duration,
                formatted_size(config.minor_promoted),
                formatted_size(config.minor_copied),
            );
        }

        CollectionKind::Full => {
            println!(
                "GC: {} ({}) {}/{} -> {}/{}; {:.2} ms",
                kind,
                reason,
                formatted_size(config.start_object_size),
                formatted_size(config.start_memory_size),
                formatted_size(config.end_object_size),
                formatted_size(config.end_memory_size),
                config.gc_duration,
            );
        }
    }
}

fn object_size(young: &YoungGen, old: &OldGen, large: &LargeSpace) -> usize {
    young.active_size() + old.active_size() + large.committed_size()
}

fn memory_size(young: &YoungGen, old: &OldGen, large: &LargeSpace) -> usize {
    let (eden_size, semi_size) = young.committed_size();
    let young_size = eden_size + semi_size;

    young_size + old.committed_size() + large.committed_size()
}

pub struct HeapConfig {
    min_heap_size: usize,
    max_heap_size: usize,

    pub eden_size: usize,
    pub semi_size: usize,
    pub old_size: usize,
    pub old_limit: usize,

    gc_start: u64,
    gc_duration: f32,

    start_object_size: usize,
    start_memory_size: usize,
    end_object_size: usize,
    end_memory_size: usize,

    pub minor_promoted: usize,
    pub minor_copied: usize,

    pub total_minor_collections: usize,
    pub total_minor_pause: f32,
    pub total_full_collections: usize,
    pub total_full_pause: f32,

    full_phases: Vec<FullCollectorPhases>,
    minor_phases: Vec<MinorCollectorPhases>,
}

impl HeapConfig {
    pub fn new(min_heap_size: usize, max_heap_size: usize) -> HeapConfig {
        assert!(min_heap_size <= max_heap_size);

        HeapConfig {
            min_heap_size: min_heap_size,
            max_heap_size: max_heap_size,

            eden_size: 0,
            semi_size: 0,
            old_size: 0,
            old_limit: 0,

            gc_start: 0,
            gc_duration: 0f32,

            start_object_size: 0,
            start_memory_size: 0,
            end_object_size: 0,
            end_memory_size: 0,

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

    pub fn full_marking(&self) -> Numbers {
        let values: Vec<_> = self.full_phases.iter().map(|x| x.marking).collect();
        calculate_numbers(&values)
    }

    pub fn full_marking_all(&self) -> AllNumbers {
        AllNumbers(self.full_phases.iter().map(|x| x.marking).collect())
    }

    pub fn full_compute_forward(&self) -> Numbers {
        let values: Vec<_> = self.full_phases.iter().map(|x| x.compute_forward).collect();
        calculate_numbers(&values)
    }

    pub fn full_compute_forward_all(&self) -> AllNumbers {
        AllNumbers(self.full_phases.iter().map(|x| x.compute_forward).collect())
    }

    pub fn full_update_refs(&self) -> Numbers {
        let values: Vec<_> = self.full_phases.iter().map(|x| x.update_refs).collect();
        calculate_numbers(&values)
    }

    pub fn full_update_refs_all(&self) -> AllNumbers {
        AllNumbers(self.full_phases.iter().map(|x| x.update_refs).collect())
    }

    pub fn full_relocate(&self) -> Numbers {
        let values: Vec<_> = self.full_phases.iter().map(|x| x.relocate).collect();
        calculate_numbers(&values)
    }

    pub fn full_relocate_all(&self) -> AllNumbers {
        AllNumbers(self.full_phases.iter().map(|x| x.relocate).collect())
    }

    pub fn full_reset_cards(&self) -> Numbers {
        let values: Vec<_> = self.full_phases.iter().map(|x| x.reset_cards).collect();
        calculate_numbers(&values)
    }

    pub fn full_reset_cards_all(&self) -> AllNumbers {
        AllNumbers(self.full_phases.iter().map(|x| x.reset_cards).collect())
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

pub struct AllNumbers(Vec<f32>);

impl fmt::Display for AllNumbers {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        let mut first = true;
        for num in &self.0 {
            if !first {
                write!(f, ",")?;
            }
            write!(f, "{:.1}", num)?;
            first = false;
        }
        write!(f, "]")
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
        avg: avg,
        sum: sum,
        min: xmin,
        max: xmax,
    }
}

pub type SharedHeapConfig = Arc<Mutex<HeapConfig>>;

#[derive(Clone)]
pub struct FullCollectorPhases {
    pub marking: f32,
    pub compute_forward: f32,
    pub update_refs: f32,
    pub relocate: f32,
    pub reset_cards: f32,
    pub total: f32,
}

impl FullCollectorPhases {
    pub fn new() -> FullCollectorPhases {
        FullCollectorPhases {
            marking: 0f32,
            compute_forward: 0f32,
            update_refs: 0f32,
            relocate: 0f32,
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
