use crate::gc::{DEFAULT_READONLY_SPACE_LIMIT, M};
use num_cpus;
use std::cmp::{max, min};
use std::fmt;
use std::ops::Deref;

pub use dora_compiler::{CollectorName, parse_collector, parse_target_arch};

#[derive(Debug)]
pub struct VmFlags {
    pub gc_stress: bool,
    pub gc_stress_minor: bool,
    pub gc_stats: bool,
    pub gc_verbose: bool,
    pub gc_verify: bool,
    pub gc_worker: usize,
    pub gc_young_size: Option<MemSize>,
    pub gc: Option<CollectorName>,
    pub min_heap_size: Option<MemSize>,
    pub max_heap_size: Option<MemSize>,
    pub code_size: Option<MemSize>,
    pub readonly_size: Option<MemSize>,
    pub disable_tlab: bool,
    pub snapshot_on_oom: Option<String>,
}

impl VmFlags {
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
        self.code_size.map(|s| *s).unwrap_or(32 * M)
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
pub enum Compiler {
    Cannon,
}

impl fmt::Display for Compiler {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Compiler::Cannon => f.write_str("cannon"),
        }
    }
}
