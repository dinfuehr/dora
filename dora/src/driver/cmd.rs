use std::default::Default;
use std::path::PathBuf;

use dora_runtime::Flags as VmArgs;
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
    --emit-asm-boots        Emits assembly code for all boots compilations.
    --emit-graph=<fct>      Emits graph for function.
    --emit-bytecode=<fct>   Emits bytecode to stdout.
    --emit-bytecode-compiler=<fct> Emits bytecode before compilation.
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
    --gc-stats              Print GC statistics.
    --gc-verbose            Verbose GC.
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

    pub emit_ast: Option<String>,
    pub output: Option<String>,
    pub emit_asm: Option<String>,
    pub emit_asm_boots: bool,
    pub emit_asm_file: bool,
    pub emit_graph: Option<String>,
    pub emit_bytecode: Option<String>,
    pub emit_bytecode_compiler: Option<String>,
    pub emit_compiler: bool,
    pub emit_stubs: bool,
    pub enable_perf: bool,
    pub omit_bounds_check: bool,
    pub version: bool,
    pub help: bool,
    pub emit_debug: Option<String>,
    pub emit_debug_native: bool,
    pub emit_debug_compile: bool,
    pub emit_debug_entry: bool,
    pub gc_events: bool,
    pub gc_stress: bool,
    pub gc_stress_in_lazy_compile: bool,
    pub gc_stress_minor: bool,
    pub gc_stats: bool,
    pub gc_verbose: bool,
    pub gc_verify: bool,
    pub gc_worker: usize,
    gc_young_size: Option<MemSize>,
    pub gc_semi_ratio: Option<usize>,
    pub gc: Option<CollectorName>,
    pub compiler: Option<CompilerName>,
    pub min_heap_size: Option<MemSize>,
    pub max_heap_size: Option<MemSize>,
    pub code_size: Option<MemSize>,
    pub readonly_size: Option<MemSize>,
    pub check: bool,
    pub disable_tlab: bool,
    pub disable_barrier: bool,
    pub test_filter: Option<String>,
    pub packages: Vec<(String, PathBuf)>,

    pub command: Command,
}

impl Default for Args {
    fn default() -> Args {
        Args {
            arg_argument: None,
            arg_file: None,

            output: None,
            emit_ast: None,
            emit_asm: None,
            emit_asm_boots: false,
            emit_asm_file: false,
            emit_graph: None,
            emit_bytecode: None,
            emit_bytecode_compiler: None,
            emit_compiler: false,
            emit_stubs: false,
            emit_debug: None,
            emit_debug_compile: false,
            emit_debug_native: false,
            emit_debug_entry: false,
            enable_perf: false,
            omit_bounds_check: false,
            version: false,
            help: false,
            gc_events: false,
            gc_stress: false,
            gc_stress_in_lazy_compile: false,
            gc_stress_minor: false,
            gc_stats: false,
            gc_verbose: false,
            gc_verify: false,
            gc_worker: 0,
            gc_young_size: None,
            gc_semi_ratio: None,
            gc: None,
            compiler: None,
            min_heap_size: None,
            max_heap_size: None,
            code_size: None,
            readonly_size: None,
            check: false,
            disable_tlab: false,
            disable_barrier: false,
            test_filter: None,
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
            args.version = true;
        } else if arg == "--check" {
            args.check = true;
        } else if arg == "-h" || arg == "--help" {
            args.help = true;
        } else if arg.starts_with("--emit-ast=") {
            args.emit_ast = Some(argument_value(arg).into());
        } else if arg.starts_with("--emit-asm=") {
            args.emit_asm = Some(argument_value(arg).into());
        } else if arg == "--emit-asm-file" {
            args.emit_asm_file = true;
        } else if arg == "--emit-asm-boots" {
            args.emit_asm_boots = true;
        } else if arg.starts_with("--emit-graph=") {
            args.emit_graph = Some(argument_value(arg).into());
        } else if arg.starts_with("--emit-bytecode=") {
            args.emit_bytecode = Some(argument_value(arg).into());
        } else if arg.starts_with("--emit-bytecode-compiler") {
            args.emit_bytecode_compiler = Some("all".into());
        } else if arg.starts_with("--emit-bytecode-compiler=") {
            args.emit_bytecode_compiler = Some(argument_value(arg).into());
        } else if arg == "--emit-stubs" {
            args.emit_stubs = true;
        } else if arg.starts_with("--emit-debug=") {
            args.emit_debug = Some(argument_value(arg).into());
        } else if arg == "--emit-compiler" {
            args.emit_compiler = true;
        } else if arg == "--emit-debug-native" {
            args.emit_debug_native = true;
        } else if arg == "--emit-debug-compile" {
            args.emit_debug_compile = true;
        } else if arg == "--emit-debug-entry" {
            args.emit_debug_entry = true;
        } else if arg == "--omit-bounds-check" {
            args.omit_bounds_check = true;
        } else if arg == "--enable-perf" {
            args.enable_perf = true;
        } else if arg == "--gc-events" {
            args.gc_events = true;
        } else if arg == "--gc-stress" {
            args.gc_stress = true;
        } else if arg == "--gc-stress-in-lazy-compile" {
            args.gc_stress_in_lazy_compile = true;
        } else if arg == "--gc-stress-minor" {
            args.gc_stress_minor = true;
        } else if arg == "--gc-stats" {
            args.gc_stats = true;
        } else if arg == "--gc-verbose" {
            args.gc_verbose = true;
        } else if arg == "--gc-verify" {
            args.gc_verify = true;
        } else if arg.starts_with("--gc-worker=") {
            args.gc_worker = argument_usize(arg)?;
        } else if arg.starts_with("--gc=") {
            let value = argument_value(arg);
            let value = match value {
                "zero" => CollectorName::Zero,
                "compact" => CollectorName::Compact,
                "copy" => CollectorName::Copy,
                "sweep" => CollectorName::Sweep,
                "swiper" => CollectorName::Swiper,
                _ => return Err(format!("--gc: unknown collector '{}'", value)),
            };
            args.gc = Some(value);
        } else if arg.starts_with("--gc-young-size=") {
            args.gc_young_size = Some(argument_mem_size(arg)?);
        } else if arg.starts_with("--gc-semi-ratio=") {
            args.gc_semi_ratio = Some(argument_usize(arg)?);
        } else if arg.starts_with("--compiler=") {
            let value = argument_value(arg);
            let value = match value {
                "cannon" => CompilerName::Cannon,
                "boots" => CompilerName::Boots,
                _ => return Err(format!("--compiler: unknown compiler '{}'", value)),
            };
            args.compiler = Some(value);
        } else if arg.starts_with("--test-filter=") {
            args.test_filter = Some(argument_value(arg).into());
        } else if arg == "--disable-tlab" {
            args.disable_tlab = true;
        } else if arg == "-o" {
            if idx + 1 >= cli_arguments.len() {
                return Err("-o needs argument".into());
            }
            args.output = Some(cli_arguments[idx + 1].clone());
            idx += 1;
        } else if arg == "--disable-barrier" {
            args.disable_barrier = true;
        } else if arg.starts_with("--min-heap-size=") {
            args.min_heap_size = Some(argument_mem_size(arg)?);
        } else if arg.starts_with("--max-heap-size=") {
            args.max_heap_size = Some(argument_mem_size(arg)?);
        } else if arg.starts_with("--code-size=") {
            args.code_size = Some(argument_mem_size(arg)?);
        } else if arg.starts_with("--readonly-size=") {
            args.readonly_size = Some(argument_mem_size(arg)?);
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
        } else if args.arg_file.is_none() {
            args.arg_file = Some(arg.clone());

            // In `run` mode all arguments after the input file are arguments
            // for the program.
            if args.command.is_run() {
                let count = cli_arguments.len() - idx - 1;
                let mut arguments: Vec<String> = Vec::with_capacity(count);
                for arg in &cli_arguments[idx + 1..] {
                    arguments.push(arg.clone());
                }
                args.arg_argument = Some(arguments);
                break;
            }
        } else {
            return Err(format!("only one input file expected"));
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
        emit_asm: args.emit_asm.clone(),
        emit_asm_boots: args.emit_asm_boots,
        emit_asm_file: args.emit_asm_file,
        emit_bytecode_compiler: args.emit_bytecode_compiler.clone(),
        emit_compiler: args.emit_compiler,
        emit_graph: args.emit_graph.clone(),
        emit_stubs: args.emit_stubs,
        enable_perf: args.enable_perf,
        omit_bounds_check: args.omit_bounds_check,
        emit_debug: args.emit_debug.clone(),
        emit_debug_native: args.emit_debug_native,
        emit_debug_compile: args.emit_debug_compile,
        emit_debug_entry: args.emit_debug_entry,
        gc_events: args.gc_events,
        gc_stress: args.gc_stress,
        gc_stress_in_lazy_compile: args.gc_stress_in_lazy_compile,
        gc_stress_minor: args.gc_stress_minor,
        gc_stats: args.gc_stats,
        gc_verbose: args.gc_verbose,
        gc_verify: args.gc_verify,
        gc_worker: args.gc_worker,
        gc_young_size: args.gc_young_size,
        gc_semi_ratio: args.gc_semi_ratio,
        gc: args.gc,
        compiler: args.compiler,
        min_heap_size: args.min_heap_size,
        max_heap_size: args.max_heap_size,
        code_size: args.code_size,
        readonly_size: args.readonly_size,
        disable_tlab: args.disable_tlab,
        disable_barrier: args.disable_barrier,
    }
}
