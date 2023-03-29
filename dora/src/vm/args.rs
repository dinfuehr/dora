use crate::gc::M;
use crate::gc::{DEFAULT_CODE_SPACE_LIMIT, DEFAULT_READONLY_SPACE_LIMIT};
use num_cpus;
use std::cmp::{max, min};
use std::fmt;
use std::ops::Deref;

#[derive(Debug)]
pub struct Args {
    pub flag_emit_asm: Option<String>,
    pub flag_emit_asm_file: bool,
    pub flag_emit_compiler: bool,
    pub flag_emit_stubs: bool,
    pub flag_enable_perf: bool,
    pub flag_omit_bounds_check: bool,
    pub flag_emit_debug: Option<String>,
    pub flag_emit_debug_native: bool,
    pub flag_emit_debug_compile: bool,
    pub flag_emit_debug_entry: bool,
    pub flag_gc_events: bool,
    pub flag_gc_stress: bool,
    pub flag_gc_stress_minor: bool,
    pub flag_gc_parallel_full: bool,
    pub flag_gc_parallel_minor: bool,
    pub flag_gc_parallel: bool,
    pub flag_gc_stats: bool,
    pub flag_gc_verbose: bool,
    pub flag_gc_dev_verbose: bool,
    pub flag_gc_verify: bool,
    pub flag_gc_worker: usize,
    pub flag_gc_young_size: Option<MemSize>,
    pub flag_gc_semi_ratio: Option<usize>,
    pub flag_gc: Option<CollectorName>,
    pub flag_compiler: Option<CompilerName>,
    pub flag_min_heap_size: Option<MemSize>,
    pub flag_max_heap_size: Option<MemSize>,
    pub flag_code_size: Option<MemSize>,
    pub flag_readonly_size: Option<MemSize>,
    pub flag_disable_tlab: bool,
    pub flag_disable_barrier: bool,
}

impl Args {
    pub fn min_heap_size(&self) -> usize {
        let min_heap_size = self.flag_min_heap_size.map(|s| *s).unwrap_or(32 * M);
        let max_heap_size = self.max_heap_size();

        min(min_heap_size, max_heap_size)
    }

    pub fn max_heap_size(&self) -> usize {
        let max_heap_size = self.flag_max_heap_size.map(|s| *s).unwrap_or(128 * M);

        max(max_heap_size, 1 * M)
    }

    pub fn code_size(&self) -> usize {
        self.flag_code_size
            .map(|s| *s)
            .unwrap_or(DEFAULT_CODE_SPACE_LIMIT)
    }

    pub fn readonly_size(&self) -> usize {
        self.flag_readonly_size
            .map(|s| *s)
            .unwrap_or(DEFAULT_READONLY_SPACE_LIMIT)
    }

    pub fn gc_workers(&self) -> usize {
        if self.flag_gc_worker > 0 {
            self.flag_gc_worker
        } else {
            min(num_cpus::get(), 8)
        }
    }

    pub fn young_size(&self) -> Option<usize> {
        self.flag_gc_young_size.map(|young_size| *young_size)
    }

    pub fn young_appel(&self) -> bool {
        self.flag_gc_young_size.is_none()
    }

    pub fn parallel_minor(&self) -> bool {
        self.flag_gc_parallel_minor || self.flag_gc_parallel
    }

    pub fn parallel_full(&self) -> bool {
        self.flag_gc_parallel_full || self.flag_gc_parallel
    }

    pub fn compiler(&self) -> CompilerName {
        self.flag_compiler.unwrap_or(CompilerName::Cannon)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum CollectorName {
    Zero,
    Compact,
    Copy,
    Sweep,
    Swiper,
    Region,
}

#[derive(Copy, Clone, Debug)]
pub struct MemSize(pub usize);

impl Deref for MemSize {
    type Target = usize;

    fn deref(&self) -> &usize {
        &self.0
    }
}

#[derive(Copy, Clone, Debug)]
pub enum CompilerName {
    Cannon,
    Boots,
}

impl fmt::Display for CompilerName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let text = match self {
            CompilerName::Cannon => "cannon",
            CompilerName::Boots => "boots",
        };

        f.write_str(text)
    }
}
