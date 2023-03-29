use std::default::Default;
use std::path::PathBuf;

use dora_runtime::Args as VmArgs;
use dora_runtime::{CollectorName, CompilerName, MemSize};

// Write the Docopt usage string.
static USAGE: &'static str = "
Usage: dora test [options] [<file>]
       dora [options] <file> [--] [<argument>...]
       dora (--version | --help)

Options:
    -h, --help              Shows this text.
    --version               Shows version.
    --emit-ast=<fct>        Emits AST to stdout.
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
";

#[derive(Debug)]
pub struct Args {
    pub arg_argument: Option<Vec<String>>,
    pub arg_file: Option<String>,

    pub flag_emit_ast: Option<String>,
    pub flag_emit_asm: Option<String>,
    pub flag_emit_asm_file: bool,
    pub flag_emit_bytecode: Option<String>,
    pub flag_emit_compiler: bool,
    pub flag_emit_stubs: bool,
    pub flag_enable_perf: bool,
    pub flag_omit_bounds_check: bool,
    pub flag_version: bool,
    pub flag_help: bool,
    pub flag_emit_debug: Option<String>,
    pub flag_emit_debug_native: bool,
    pub flag_emit_debug_compile: bool,
    pub flag_emit_debug_entry: bool,
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
    pub flag_readonly_size: Option<MemSize>,
    pub flag_check: bool,
    pub flag_disable_tlab: bool,
    pub flag_disable_barrier: bool,
    pub flag_test_filter: Option<String>,
    pub packages: Vec<(String, PathBuf)>,

    pub command: Command,
}

impl Default for Args {
    fn default() -> Args {
        Args {
            arg_argument: None,
            arg_file: None,

            flag_emit_ast: None,
            flag_emit_asm: None,
            flag_emit_asm_file: false,
            flag_emit_bytecode: None,
            flag_emit_compiler: false,
            flag_emit_stubs: false,
            flag_emit_debug: None,
            flag_emit_debug_compile: false,
            flag_emit_debug_native: false,
            flag_emit_debug_entry: false,
            flag_enable_perf: false,
            flag_omit_bounds_check: false,
            flag_version: false,
            flag_help: false,
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
            flag_readonly_size: None,
            flag_check: false,
            flag_disable_tlab: false,
            flag_disable_barrier: false,
            flag_test_filter: None,
            packages: Vec::new(),

            command: Command::Run,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Command {
    Run,
    Test,
    Build,
}

impl Command {
    #[allow(dead_code)]
    pub fn is_run(&self) -> bool {
        match self {
            Command::Run => true,
            _ => false,
        }
    }

    pub fn is_test(&self) -> bool {
        match self {
            Command::Test => true,
            _ => false,
        }
    }

    pub fn is_build(&self) -> bool {
        match self {
            Command::Build => true,
            _ => false,
        }
    }
}

pub fn parse_arguments() -> Result<Args, String> {
    let cli_arguments: Vec<String> = std::env::args().collect();
    let mut args: Args = Default::default();
    let mut idx = 1;

    while idx < cli_arguments.len() {
        let arg = &cli_arguments[idx];

        if arg == "test" && idx == 1 {
            args.command = Command::Test;
        } else if arg == "build" && idx == 1 {
            args.command = Command::Build;
        } else if arg == "--version" || arg == "-v" {
            args.flag_version = true;
        } else if arg == "--check" {
            args.flag_check = true;
        } else if arg == "-h" || arg == "--help" {
            args.flag_help = true;
        } else if arg.starts_with("--emit-ast=") {
            args.flag_emit_ast = Some(argument_value(arg).into());
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
        } else if arg == "--emit-compiler" {
            args.flag_emit_compiler = true;
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
        } else if arg.starts_with("--readonly-size=") {
            args.flag_readonly_size = Some(argument_mem_size(arg)?);
        } else if arg == "--package" {
            if idx + 2 >= cli_arguments.len() {
                return Err("--package needs two arguments".into());
            }

            let name = cli_arguments[idx + 1].clone();
            let path = cli_arguments[idx + 2].clone();
            let path = PathBuf::from(path);

            args.packages.push((name, path));
            idx += 2;
        } else if arg.starts_with("-") {
            return Err(format!("unknown flag {}", arg));
        } else {
            args.arg_file = Some(arg.clone());

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

pub fn create_vm_args(args: &Args) -> VmArgs {
    VmArgs {
        flag_emit_asm: args.flag_emit_asm.clone(),
        flag_emit_asm_file: args.flag_emit_asm_file,
        flag_emit_compiler: args.flag_emit_compiler,
        flag_emit_stubs: args.flag_emit_stubs,
        flag_enable_perf: args.flag_enable_perf,
        flag_omit_bounds_check: args.flag_omit_bounds_check,
        flag_emit_debug: args.flag_emit_debug.clone(),
        flag_emit_debug_native: args.flag_emit_debug_native,
        flag_emit_debug_compile: args.flag_emit_debug_compile,
        flag_emit_debug_entry: args.flag_emit_debug_entry,
        flag_gc_events: args.flag_gc_events,
        flag_gc_stress: args.flag_gc_stress,
        flag_gc_stress_minor: args.flag_gc_stress_minor,
        flag_gc_parallel_full: args.flag_gc_parallel_full,
        flag_gc_parallel_minor: args.flag_gc_parallel_minor,
        flag_gc_parallel: args.flag_gc_parallel,
        flag_gc_stats: args.flag_gc_stats,
        flag_gc_verbose: args.flag_gc_verbose,
        flag_gc_dev_verbose: args.flag_gc_dev_verbose,
        flag_gc_verify: args.flag_gc_verify,
        flag_gc_worker: args.flag_gc_worker,
        flag_gc_young_size: args.flag_gc_young_size,
        flag_gc_semi_ratio: args.flag_gc_semi_ratio,
        flag_gc: args.flag_gc,
        flag_compiler: args.flag_compiler,
        flag_min_heap_size: args.flag_min_heap_size,
        flag_max_heap_size: args.flag_max_heap_size,
        flag_code_size: args.flag_code_size,
        flag_readonly_size: args.flag_readonly_size,
        flag_disable_tlab: args.flag_disable_tlab,
        flag_disable_barrier: args.flag_disable_barrier,
    }
}
