use num_cpus;
use std::cmp::{max, min};
use std::default::Default;
use std::ops::Deref;

use crate::gc::M;

use crate::gc::{DEFAULT_CODE_SPACE_LIMIT, DEFAULT_PERM_SPACE_LIMIT};

// Write the Docopt usage string.
static USAGE: &'static str = "
Usage: dora test [options] [<file>]
       dora [options] <file> [--] [<argument>...]
       dora (--version | --help)

Options:
    -h, --help              Shows this text.
    --version               Shows version.
    --emit-ast              Emits AST to stdout.
    --emit-asm=<fct>        Emits assembly code to stdout.
    --emit-asm-file         Emits assembly code into file `dora-<pid>.asm`.
    --emit-bytecode=<fct>   Emits bytecode to stdout.
    --emit-stubs            Emits generated stubs.
    --emit-debug=<fct>      Emits debug instruction at beginning of functions.
    --emit-debug-native     Emits debug instruction at beginning of native stub.
    --emit-debug-compile    Emits debug instruction at beginning of compile stub.
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
    --gc-worker=<num>       Number of GC worker threads.
    --gc=<name>             Switch GC. Possible values: zero, copy, swiper (default).
    --gc-young-size=<SIZE>  Use fixed size for young generation.
    --gc-semi-ratio=<num>   Use fixed ratio of semi space in young generation.

    --compiler=<name>       Switch default compiler. Possible values: cannon [default: cannon].
    --test-filter=<name>    Filter tests.
    --clear-regs            Clear register when freeing.

    --disable-tlab          Disable tlab allocation.
    --disable-barrier       Disable barriers.

    --min-heap-size=<SIZE>  Set minimum heap size.
    --max-heap-size=<SIZE>  Set maximum heap size.
    --code-size=<SIZE>      Set code size limit.
    --perm-size=<SIZE>      Set perm size limit.

    --stdlib=<path>         Load standard library from the given path.
    --boots=<path>          Load boots source from the given path.
    --test-boots            Run unit tests for boots.
";

#[derive(Debug)]
pub struct Args {
    pub arg_argument: Option<Vec<String>>,
    pub arg_file: String,

    pub flag_emit_ast: bool,
    pub flag_emit_asm: Option<String>,
    pub flag_emit_asm_file: bool,
    pub flag_emit_bytecode: Option<String>,
    pub flag_emit_stubs: bool,
    pub flag_enable_perf: bool,
    pub flag_omit_bounds_check: bool,
    pub flag_version: bool,
    pub flag_help: bool,
    pub flag_emit_debug: Option<String>,
    pub flag_emit_debug_native: bool,
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
    pub flag_boots: Option<String>,
    pub flag_test_filter: Option<String>,
    pub flag_clear_regs: bool,

    pub cmd_test: bool,
    pub flag_test_boots: bool,
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
        self.flag_compiler.unwrap_or(CompilerName::Cannon)
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
            flag_emit_stubs: false,
            flag_emit_debug: None,
            flag_emit_debug_compile: false,
            flag_emit_debug_native: false,
            flag_emit_debug_entry: false,
            flag_enable_perf: false,
            flag_omit_bounds_check: false,
            flag_version: false,
            flag_help: false,
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
            flag_boots: None,
            flag_test_filter: None,
            flag_clear_regs: false,

            cmd_test: false,
            flag_test_boots: false,
        }
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
pub enum CompilerName {
    Cannon,
    Boots,
}

#[derive(Copy, Clone, Debug)]
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

pub fn parse_arguments() -> Result<Args, String> {
    let cli_arguments: Vec<String> = std::env::args().collect();
    let mut args: Args = Default::default();
    let mut idx = 1;

    while idx < cli_arguments.len() {
        let arg = &cli_arguments[idx];

        if arg == "test" && idx == 1 {
            args.cmd_test = true;
        } else if arg == "--version" || arg == "-v" {
            args.flag_version = true;
        } else if arg == "--check" {
            args.flag_check = true;
        } else if arg == "-h" || arg == "--help" {
            args.flag_help = true;
        } else if arg == "--emit-ast" {
            args.flag_emit_ast = true;
        } else if arg.starts_with("--emit-asm=") {
            args.flag_emit_asm = Some(argument_value(arg).into());
        } else if arg == "--emit-asm-file" {
            args.flag_emit_asm_file = true;
        } else if arg.starts_with("--emit-bytecode=") {
            args.flag_emit_bytecode = Some(argument_value(arg).into());
        } else if arg == "--emit-stubs" {
            args.flag_emit_stubs = true;
        } else if arg.starts_with("--emit-debug=") {
            args.flag_emit_debug = Some(argument_value(arg).into());
        } else if arg == "--emit-debug-native" {
            args.flag_emit_debug_native = true;
        } else if arg == "--emit-debug-compile" {
            args.flag_emit_debug_compile = true;
        } else if arg == "--emit-debug-entry" {
            args.flag_emit_debug_entry = true;
        } else if arg == "--omit-bounds-check" {
            args.flag_omit_bounds_check = true;
        } else if arg == "--enable-perf" {
            args.flag_enable_perf = true;
        } else if arg == "--gc-events" {
            args.flag_gc_events = true;
        } else if arg == "--gc-stress" {
            args.flag_gc_stress = true;
        } else if arg == "--gc-stress-minor" {
            args.flag_gc_stress_minor = true;
        } else if arg == "--gc-parallel-full" {
            args.flag_gc_parallel_full = true;
        } else if arg == "--gc-parallel-minor" {
            args.flag_gc_parallel_minor = true;
        } else if arg == "--gc-parallel" {
            args.flag_gc_parallel = true;
        } else if arg == "--gc-stats" {
            args.flag_gc_stats = true;
        } else if arg == "--gc-verbose" {
            args.flag_gc_verbose = true;
        } else if arg == "--gc-dev-verbose" {
            args.flag_gc_dev_verbose = true;
        } else if arg == "--gc-verify" {
            args.flag_gc_verify = true;
        } else if arg.starts_with("--gc-worker=") {
            args.flag_gc_worker = argument_usize(arg)?;
        } else if arg.starts_with("--gc=") {
            let value = argument_value(arg);
            let value = match value {
                "zero" => CollectorName::Zero,
                "compact" => CollectorName::Compact,
                "copy" => CollectorName::Copy,
                "sweep" => CollectorName::Sweep,
                "swiper" => CollectorName::Swiper,
                "region" => CollectorName::Region,
                _ => return Err(format!("--gc: unknown collector '{}'", value)),
            };
            args.flag_gc = Some(value);
        } else if arg.starts_with("--gc-young-size=") {
            args.flag_gc_young_size = Some(argument_mem_size(arg)?);
        } else if arg.starts_with("--gc-semi-ratio=") {
            args.flag_gc_semi_ratio = Some(argument_usize(arg)?);
        } else if arg.starts_with("--compiler=") {
            let value = argument_value(arg);
            let value = match value {
                "cannon" => CompilerName::Cannon,
                "boots" => CompilerName::Boots,
                _ => return Err(format!("--compiler: unknown compiler '{}'", value)),
            };
            args.flag_compiler = Some(value);
        } else if arg.starts_with("--test-filter=") {
            args.flag_test_filter = Some(argument_value(arg).into());
        } else if arg.starts_with("--clear-regs") {
            args.flag_clear_regs = true;
        } else if arg == "--disable-tlab" {
            args.flag_disable_tlab = true;
        } else if arg == "--disable-barrier" {
            args.flag_disable_barrier = true;
        } else if arg.starts_with("--min-heap-size=") {
            args.flag_min_heap_size = Some(argument_mem_size(arg)?);
        } else if arg.starts_with("--max-heap-size=") {
            args.flag_max_heap_size = Some(argument_mem_size(arg)?);
        } else if arg.starts_with("--code-size=") {
            args.flag_code_size = Some(argument_mem_size(arg)?);
        } else if arg.starts_with("--perm-size=") {
            args.flag_perm_size = Some(argument_mem_size(arg)?);
        } else if arg.starts_with("--stdlib=") {
            args.flag_stdlib = Some(argument_value(arg).to_string());
        } else if arg.starts_with("--boots=") {
            args.flag_boots = Some(argument_value(arg).to_string());
        } else if arg == "--test-boots" {
            args.flag_test_boots = true;
        } else if arg.starts_with("-") {
            return Err(format!("unknown flag {}", arg));
        } else {
            args.arg_file = arg.clone();

            let count = cli_arguments.len() - idx - 1;
            let mut arguments: Vec<String> = Vec::with_capacity(count);
            for arg in &cli_arguments[idx + 1..] {
                arguments.push(arg.clone());
            }
            args.arg_argument = Some(arguments);
            break;
        }

        idx = idx + 1;
    }

    Ok(args)
}

fn argument_value(arg: &str) -> &str {
    let idx = arg.find("=").expect("missing =");
    let (_, rhs) = arg.split_at(idx);
    &rhs[1..]
}

fn argument_mem_size(arg: &str) -> Result<MemSize, String> {
    let idx = arg.find("=").expect("missing =");
    let (name, value) = arg.split_at(idx);

    match parse_mem_size(&value[1..]) {
        Ok(value) => Ok(value),
        Err(msg) => Err(format!("{}: {}", name, msg)),
    }
}

fn argument_usize(arg: &str) -> Result<usize, String> {
    let idx = arg.find("=").expect("missing =");
    let (name, value) = arg.split_at(idx);
    let value = &value[1..];
    match value.parse::<usize>() {
        Ok(value) => Ok(value),
        Err(_) => Err(format!("{}: invalid value '{}'", name, value)),
    }
}

fn parse_mem_size(value: &str) -> Result<MemSize, String> {
    let suffix = if let Some(ch) = value.chars().last() {
        match ch {
            'k' | 'K' => 1024,
            'm' | 'M' => 1024 * 1024,
            'g' | 'G' => 1024 * 1024 * 1024,
            _ => 1,
        }
    } else {
        1
    };

    let prefix = if suffix != 1 {
        let (left, _) = value.split_at(value.len() - 1);
        left
    } else {
        value
    };

    match prefix.parse::<usize>() {
        Ok(size) => Ok(MemSize(size * suffix)),
        Err(_) => Err(format!("'{}' is not a valid mem size", value)),
    }
}

pub fn print_help() {
    println!("{}", USAGE);
}
