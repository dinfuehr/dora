use crate::gc::M;
use crate::gc::{DEFAULT_CODE_SPACE_LIMIT, DEFAULT_READONLY_SPACE_LIMIT};
use num_cpus;
use std::cmp::{max, min};
use std::fmt;
use std::ops::Deref;

#[derive(Debug)]
pub struct Flags {
    pub emit_asm: Option<String>,
    pub emit_asm_file: bool,
    pub emit_asm_boots: bool,
    pub emit_bytecode_compiler: Option<String>,
    pub emit_compiler: bool,
    pub emit_graph: Option<String>,
    pub emit_stubs: bool,
    pub enable_perf: bool,
    pub omit_bounds_check: bool,
    pub emit_debug: Option<String>,
    pub emit_debug_native: bool,
    pub emit_debug_compile: bool,
    pub emit_debug_entry: bool,
    pub gc_events: bool,
    pub gc_stress: bool,
    pub gc_stress_minor: bool,
    pub gc_stress_in_lazy_compile: bool,
    pub gc_stats: bool,
    pub gc_verbose: bool,
    pub gc_verify: bool,
    pub gc_worker: usize,
    pub gc_young_size: Option<MemSize>,
    pub gc_semi_ratio: Option<usize>,
    pub object_write_barrier: bool,
    pub gc: Option<CollectorName>,
    pub compiler: Option<CompilerName>,
    pub min_heap_size: Option<MemSize>,
    pub max_heap_size: Option<MemSize>,
    pub code_size: Option<MemSize>,
    pub readonly_size: Option<MemSize>,
    pub disable_tlab: bool,
    pub disable_barrier: bool,
}

impl Flags {
    pub fn min_heap_size(&self) -> usize {
        let min_heap_size = self.min_heap_size.map(|s| *s).unwrap_or(32 * M);
        let max_heap_size = self.max_heap_size();

        min(min_heap_size, max_heap_size)
    }

    pub fn max_heap_size(&self) -> usize {
        let max_heap_size = self.max_heap_size.map(|s| *s).unwrap_or(128 * M);

        max(max_heap_size, 1 * M)
    }

    pub fn code_size(&self) -> usize {
        self.code_size
            .map(|s| *s)
            .unwrap_or(DEFAULT_CODE_SPACE_LIMIT)
    }

    pub fn readonly_size(&self) -> usize {
        self.readonly_size
            .map(|s| *s)
            .unwrap_or(DEFAULT_READONLY_SPACE_LIMIT)
    }

    pub fn gc_workers(&self) -> usize {
        if self.gc_worker > 0 {
            self.gc_worker
        } else {
            min(num_cpus::get(), 8)
        }
    }

    pub fn young_size(&self) -> Option<usize> {
        self.gc_young_size.map(|young_size| *young_size)
    }

    pub fn young_appel(&self) -> bool {
        self.gc_young_size.is_none()
    }

    pub fn compiler(&self) -> CompilerName {
        self.compiler.unwrap_or(CompilerName::Cannon)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum CollectorName {
    Zero,
    Compact,
    Copy,
    Sweep,
    Swiper,
}

#[derive(Copy, Clone, Debug)]
pub struct MemSize(pub usize);

impl Deref for MemSize {
    type Target = usize;

    fn deref(&self) -> &usize {
        &self.0
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
