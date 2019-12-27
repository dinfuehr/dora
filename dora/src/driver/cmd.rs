use num_cpus;
use std::cmp::{max, min};
use std::default::Default;
use std::ops::Deref;

use crate::gc::M;
use docopt::Docopt;
use rustc_serialize;

use crate::gc::{DEFAULT_CODE_SPACE_LIMIT, DEFAULT_PERM_SPACE_LIMIT};

pub fn parse() -> Args {
    Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit())
}

// Write the Docopt usage string.
static USAGE: &'static str = "
Usage: dora test [options] <file>
       dora [options] <file> [--] [<argument>...]
       dora (--version | --help)

Options:
    -h, --help              Shows this text.
    --version               Shows version.
    --emit-ast              Emits AST to stdout.
    --emit-llvm             Emits initial LLVM IR to stdout.
    --emit-asm=<fct>        Emits assembly code to stdout.
    --emit-asm-file         Emits assembly code into file `dora-<pid>.asm`.
    --emit-bytecode=<fct>   Emits bytecode to stdout.
    --emit-stubs            Emits generated stubs.
    --emit-debug=<fct>      Emits debug instruction at beginning of functions.
    --emit-debug-compile    Emits debug instruction at beginning of compile stub.
    --emit-debug-throw      Emits debug instruction at beginning of throw stub.
    --emit-debug-entry      Emits debug instruction at beginning of entry stub.
    --omit-bounds-check     Omit array index out of bounds checks.
    --check                 Only type check given program.
    --asm-syntax TYPE       Emits assembly with Intel or AT&T syntax.
                            Allowed values: intel, att.
    --enable-perf           Enable dump for perf.
    --gc-events             Dump GC events.
    --gc-stress             Collect garbage at every allocation.
    --gc-stress-minor       Minor collection at every allocation.
    --gc-parallel-full      Enable parallel full collection.
    --gc-parallel-minor     Enable parallel minor collection.
    --gc-parallel           Enable both parallel minor and full collection.
    --gc-stats              Print GC statistics.
    --gc-verbose            Verbose GC.
    --gc-dev-verbose        Verbose GC for developers.
    --gc-verify             Verify heap before and after collections.
    --gc-verify-write       Verify references when storing in the heap.
    --gc-worker=<num>       Number of GC worker threads.
    --gc=<name>             Switch GC. Possible values: zero, copy, swiper (default).
    --gc-young-size=<SIZE>  Use fixed size for young generation.
    --gc-young-appel        Use Appel dynamic resizing of young generation.
    --gc-semi-ratio=<num>   Use fixed ratio of semi space in young generation.

    --bc=<name>             Switch Baseline Compiler. Possible values: cannon, astcompiler [default: astcompiler].

    --disable-tlab          Disable tlab allocation.
    --disable-barrier       Disable barriers.

    --min-heap-size=<SIZE>  Set minimum heap size.
    --max-heap-size=<SIZE>  Set maximum heap size.
    --code-size=<SIZE>      Set code size limit.
    --perm-size=<SIZE>      Set perm size limit.

    --stdlib=<path>         Load standard library from the given path.
";

#[derive(Debug, RustcDecodable)]
pub struct Args {
    pub arg_argument: Option<Vec<String>>,
    pub arg_file: String,

    pub flag_emit_ast: bool,
    pub flag_emit_asm: Option<String>,
    pub flag_emit_asm_file: bool,
    pub flag_emit_bytecode: Option<String>,
    pub flag_emit_llvm: bool,
    pub flag_emit_stubs: bool,
    pub flag_enable_perf: bool,
    pub flag_omit_bounds_check: bool,
    pub flag_version: bool,
    pub flag_emit_debug: Option<String>,
    pub flag_emit_debug_throw: bool,
    pub flag_emit_debug_compile: bool,
    pub flag_emit_debug_entry: bool,
    pub flag_asm_syntax: Option<AsmSyntax>,
    pub flag_gc_events: bool,
    pub flag_gc_stress: bool,
    pub flag_gc_stress_minor: bool,
    flag_gc_parallel_full: bool,
    flag_gc_parallel_minor: bool,
    flag_gc_parallel: bool,
    pub flag_gc_stats: bool,
    pub flag_gc_verbose: bool,
    pub flag_gc_dev_verbose: bool,
    pub flag_gc_verify: bool,
    pub flag_gc_verify_write: bool,
    pub flag_gc_worker: usize,
    flag_gc_young_size: Option<MemSize>,
    pub flag_gc_semi_ratio: Option<usize>,
    pub flag_gc: Option<CollectorName>,
    pub flag_compiler: Option<CompilerName>,
    pub flag_min_heap_size: Option<MemSize>,
    pub flag_max_heap_size: Option<MemSize>,
    pub flag_code_size: Option<MemSize>,
    pub flag_perm_size: Option<MemSize>,
    pub flag_check: bool,
    pub flag_disable_tlab: bool,
    pub flag_disable_barrier: bool,
    pub flag_stdlib: Option<String>,

    pub cmd_test: bool,
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

    pub fn perm_size(&self) -> usize {
        self.flag_perm_size
            .map(|s| *s)
            .unwrap_or(DEFAULT_PERM_SPACE_LIMIT)
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
        self.flag_compiler.unwrap_or(CompilerName::Baseline)
    }
}

impl Default for Args {
    fn default() -> Args {
        Args {
            arg_argument: None,
            arg_file: "".into(),

            flag_emit_ast: false,
            flag_emit_asm: None,
            flag_emit_asm_file: false,
            flag_emit_bytecode: None,
            flag_emit_llvm: false,
            flag_emit_stubs: false,
            flag_emit_debug: None,
            flag_emit_debug_compile: false,
            flag_emit_debug_throw: false,
            flag_emit_debug_entry: false,
            flag_enable_perf: false,
            flag_omit_bounds_check: false,
            flag_version: false,
            flag_asm_syntax: None,
            flag_gc_events: false,
            flag_gc_stress: false,
            flag_gc_stress_minor: false,
            flag_gc_parallel_full: false,
            flag_gc_parallel_minor: false,
            flag_gc_parallel: false,
            flag_gc_stats: false,
            flag_gc_verbose: false,
            flag_gc_dev_verbose: false,
            flag_gc_verify: false,
            flag_gc_verify_write: false,
            flag_gc_worker: 0,
            flag_gc_young_size: None,
            flag_gc_semi_ratio: None,
            flag_gc: None,
            flag_compiler: None,
            flag_min_heap_size: None,
            flag_max_heap_size: None,
            flag_code_size: None,
            flag_perm_size: None,
            flag_check: false,
            flag_disable_tlab: false,
            flag_disable_barrier: false,
            flag_stdlib: None,

            cmd_test: false,
        }
    }
}

#[derive(Copy, Clone, Debug, RustcDecodable)]
pub enum CollectorName {
    Zero,
    Compact,
    Copy,
    Sweep,
    Swiper,
    SweepSwiper,
}

#[derive(Copy, Clone, Debug, RustcDecodable)]
pub enum CompilerName {
    Cannon,
    Baseline,
    Boots,
}

#[derive(Copy, Clone, Debug, RustcDecodable)]
pub enum AsmSyntax {
    Intel,
    Att,
}

#[derive(Copy, Clone, Debug)]
pub struct MemSize(usize);

impl Deref for MemSize {
    type Target = usize;

    fn deref(&self) -> &usize {
        &self.0
    }
}

impl rustc_serialize::Decodable for MemSize {
    fn decode<D: rustc_serialize::Decoder>(d: &mut D) -> Result<MemSize, D::Error> {
        let mut size = d.read_str()?;
        let suffix = if let Some(ch) = size.chars().last() {
            match ch {
                'k' | 'K' => 1024,
                'm' | 'M' => 1024 * 1024,
                'g' | 'G' => 1024 * 1024 * 1024,
                _ => 1,
            }
        } else {
            1
        };

        if suffix != 1 {
            size.pop();
        }

        match size.parse::<usize>() {
            Ok(size) => Ok(MemSize(size * suffix)),
            Err(_) => Err(d.error("cannot parse mem size")),
        }
    }
}
