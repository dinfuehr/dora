use std::fmt;
use std::ops::Deref;

#[derive(Debug)]
pub struct Args {
    pub flag_emit_asm: Option<String>,
    pub flag_emit_asm_file: bool,
    pub flag_emit_bytecode: Option<String>,
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
