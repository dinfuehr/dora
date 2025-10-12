use std::default::Default;
use std::path::PathBuf;

use dora_frontend::sema::{FileContent, SemaCreationParams};
use dora_runtime::VmFlags;
use dora_runtime::{CollectorName, Compiler, MemSize};

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
    --emit-debug-boots      Emits debug instruction at beginning of boots function.
    --always-boots          Uses boots for all functions in the program.
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
    --bootstrap-compiler    Runs bootstrap process for boots compiler.

    --min-heap-size=<SIZE>  Set minimum heap size.
    --max-heap-size=<SIZE>  Set maximum heap size.
    --code-size=<SIZE>      Set code size limit.
    --perm-size=<SIZE>      Set perm size limit.
";

#[derive(Debug)]
pub struct DriverFlags {
    pub arg_argument: Option<Vec<String>>,
    pub arg_file: Option<String>,

    pub emit_ast: Option<String>,
    pub output: Option<String>,
    pub emit_asm: Option<String>,
    pub emit_asm_boots: bool,
    pub emit_asm_file: Option<String>,
    pub emit_graph: Option<String>,
    pub emit_graph_after_each_pass: bool,
    pub emit_bytecode: Option<String>,
    pub emit_bytecode_compiler: Option<String>,
    pub emit_bytecode_boots: bool,
    pub emit_compiler: bool,
    pub emit_stubs: bool,
    pub enable_perf: bool,
    pub boots: bool,
    pub omit_bounds_check: bool,
    pub always_boots: bool,
    pub use_boots: Option<String>,
    pub version: bool,
    pub help: bool,
    pub report_all_warnings: bool,
    pub emit_debug: Option<String>,
    pub emit_debug_native: bool,
    pub emit_debug_compile: bool,
    pub emit_debug_boots: bool,
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
    pub compiler: Option<Compiler>,
    pub min_heap_size: Option<MemSize>,
    pub max_heap_size: Option<MemSize>,
    pub code_size: Option<MemSize>,
    pub readonly_size: Option<MemSize>,
    pub check: bool,
    pub disable_tlab: bool,
    pub disable_barrier: bool,
    pub bootstrap_compiler: bool,
    pub snapshot_on_oom: Option<String>,
    pub test_filter: Option<String>,
    pub test_boots: bool,
    pub packages: Vec<(String, PathBuf)>,

    pub command: Command,

    pub separate_stdlib_check: bool,
}

impl DriverFlags {
    pub fn include_boots(&self) -> bool {
        self.boots || self.always_boots || self.use_boots.is_some()
    }
}

impl Default for DriverFlags {
    fn default() -> DriverFlags {
        DriverFlags {
            arg_argument: None,
            arg_file: None,

            output: None,
            emit_ast: None,
            emit_asm: None,
            emit_asm_boots: false,
            emit_asm_file: None,
            emit_graph: None,
            emit_graph_after_each_pass: false,
            emit_bytecode: None,
            emit_bytecode_compiler: None,
            emit_bytecode_boots: false,
            emit_compiler: false,
            emit_stubs: false,
            emit_debug: None,
            emit_debug_compile: false,
            emit_debug_native: false,
            emit_debug_boots: false,
            emit_debug_entry: false,
            enable_perf: false,
            boots: false,
            omit_bounds_check: false,
            always_boots: false,
            use_boots: None,
            version: false,
            help: false,
            report_all_warnings: false,
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
            bootstrap_compiler: false,
            test_filter: None,
            test_boots: false,
            packages: Vec::new(),
            snapshot_on_oom: None,

            command: Command::Run,

            separate_stdlib_check: true,
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

pub fn parse_arguments() -> Result<DriverFlags, String> {
    let cli_arguments: Vec<String> = std::env::args().collect();
    let mut flags: DriverFlags = Default::default();
    let mut idx = 1;

    while idx < cli_arguments.len() {
        let arg = &cli_arguments[idx];

        if arg == "test" && idx == 1 {
            flags.command = Command::Test;
        } else if arg == "build" && idx == 1 {
            flags.command = Command::Build;
        } else if arg == "--version" || arg == "-v" {
            flags.version = true;
        } else if arg == "--check" {
            flags.check = true;
        } else if arg == "-h" || arg == "--help" {
            flags.help = true;
        } else if arg == "--report-all-warnings" {
            flags.report_all_warnings = true;
        } else if arg.starts_with("--emit-ast=") {
            flags.emit_ast = Some(argument_value(arg).into());
        } else if arg.starts_with("--emit-asm=") {
            flags.emit_asm = Some(argument_value(arg).into());
        } else if arg.starts_with("--emit-asm-file=") {
            flags.emit_asm_file = Some(argument_value(arg).into());
        } else if arg == "--emit-asm-boots" {
            flags.emit_asm_boots = true;
        } else if arg.starts_with("--emit-graph=") {
            flags.emit_graph = Some(argument_value(arg).into());
        } else if arg == "--emit-graph-after-each-pass" {
            flags.emit_graph_after_each_pass = true;
        } else if arg.starts_with("--emit-bytecode=") {
            flags.emit_bytecode = Some(argument_value(arg).into());
        } else if arg.starts_with("--emit-bytecode-compiler") {
            flags.emit_bytecode_compiler = Some("all".into());
        } else if arg.starts_with("--emit-bytecode-compiler=") {
            flags.emit_bytecode_compiler = Some(argument_value(arg).into());
        } else if arg == "--emit-bytecode-boots" {
            flags.emit_bytecode_boots = true;
        } else if arg == "--always-boots" {
            flags.always_boots = true;
        } else if arg.starts_with("--use-boots=") {
            flags.use_boots = Some(argument_value(arg).into());
        } else if arg == "--emit-stubs" {
            flags.emit_stubs = true;
        } else if arg.starts_with("--emit-debug=") {
            flags.emit_debug = Some(argument_value(arg).into());
        } else if arg == "--emit-compiler" {
            flags.emit_compiler = true;
        } else if arg == "--emit-debug-boots" {
            flags.emit_debug_boots = true;
        } else if arg == "--emit-debug-native" {
            flags.emit_debug_native = true;
        } else if arg == "--emit-debug-compile" {
            flags.emit_debug_compile = true;
        } else if arg == "--emit-debug-entry" {
            flags.emit_debug_entry = true;
        } else if arg == "--omit-bounds-check" {
            flags.omit_bounds_check = true;
        } else if arg == "--enable-perf" {
            flags.enable_perf = true;
        } else if arg == "--gc-events" {
            flags.gc_events = true;
        } else if arg == "--gc-stress" {
            flags.gc_stress = true;
        } else if arg == "--gc-stress-in-lazy-compile" {
            flags.gc_stress_in_lazy_compile = true;
        } else if arg == "--gc-stress-minor" {
            flags.gc_stress_minor = true;
        } else if arg == "--gc-stats" {
            flags.gc_stats = true;
        } else if arg == "--gc-verbose" {
            flags.gc_verbose = true;
        } else if arg == "--gc-verify" {
            flags.gc_verify = true;
        } else if arg.starts_with("--gc-worker=") {
            flags.gc_worker = argument_usize(arg)?;
        } else if arg.starts_with("--gc=") {
            let value = argument_value(arg);
            let value = match value {
                "zero" => CollectorName::Zero,
                "copy" => CollectorName::Copy,
                "sweep" => CollectorName::Sweep,
                "swiper" => CollectorName::Swiper,
                _ => return Err(format!("--gc: unknown collector '{}'", value)),
            };
            flags.gc = Some(value);
        } else if arg.starts_with("--gc-young-size=") {
            flags.gc_young_size = Some(argument_mem_size(arg)?);
        } else if arg.starts_with("--gc-semi-ratio=") {
            flags.gc_semi_ratio = Some(argument_usize(arg)?);
        } else if arg.starts_with("--compiler=") {
            let value = argument_value(arg);
            let value = match value {
                "cannon" => Compiler::Cannon,
                "boots" => Compiler::Boots,
                _ => return Err(format!("--compiler: unknown compiler '{}'", value)),
            };
            flags.compiler = Some(value);
        } else if arg.starts_with("--test-filter=") {
            flags.test_filter = Some(argument_value(arg).into());
        } else if arg == "--test-boots" {
            flags.test_boots = true;
        } else if arg == "--disable-tlab" {
            flags.disable_tlab = true;
        } else if arg == "--bootstrap-compiler" {
            flags.bootstrap_compiler = true;
        } else if arg == "-o" {
            if idx + 1 >= cli_arguments.len() {
                return Err("-o needs argument".into());
            }
            flags.output = Some(cli_arguments[idx + 1].clone());
            idx += 1;
        } else if arg == "--disable-barrier" {
            flags.disable_barrier = true;
        } else if arg.starts_with("--min-heap-size=") {
            flags.min_heap_size = Some(argument_mem_size(arg)?);
        } else if arg.starts_with("--max-heap-size=") {
            flags.max_heap_size = Some(argument_mem_size(arg)?);
        } else if arg.starts_with("--code-size=") {
            flags.code_size = Some(argument_mem_size(arg)?);
        } else if arg.starts_with("--readonly-size=") {
            flags.readonly_size = Some(argument_mem_size(arg)?);
        } else if arg == "--boots" {
            flags.boots = true;
        } else if arg == "--package" {
            if idx + 2 >= cli_arguments.len() {
                return Err("--package needs two arguments".into());
            }

            let name = cli_arguments[idx + 1].clone();
            let path = cli_arguments[idx + 2].clone();
            let path = PathBuf::from(path);

            flags.packages.push((name, path));
            idx += 2;
        } else if arg.starts_with("--snapshot-on-oom=") {
            flags.snapshot_on_oom = Some(argument_value(arg).into());
        } else if arg == "--separate-stdlib-check" {
            flags.separate_stdlib_check = true;
        } else if arg.starts_with("-") {
            return Err(format!("unknown flag {}", arg));
        } else if flags.arg_file.is_none() {
            flags.arg_file = Some(arg.clone());

            // In `run` mode all arguments after the input file are arguments
            // for the program.
            if flags.command.is_run() {
                let count = cli_arguments.len() - idx - 1;
                let mut arguments: Vec<String> = Vec::with_capacity(count);
                for arg in &cli_arguments[idx + 1..] {
                    arguments.push(arg.clone());
                }
                flags.arg_argument = Some(arguments);
                break;
            }
        } else {
            return Err(format!("only one input file expected"));
        }

        idx = idx + 1;
    }

    Ok(flags)
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

pub fn create_sema_flags(flags: &DriverFlags, program_file: PathBuf) -> SemaCreationParams {
    let packages = flags
        .packages
        .iter()
        .map(|(name, path)| (name.clone(), FileContent::Path(path.clone())))
        .collect::<Vec<_>>();

    SemaCreationParams {
        program_file: Some(FileContent::Path(program_file)),
        packages,
        vfs: None,
        boots: flags.include_boots(),
        is_standard_library: false,
    }
}

pub fn create_vm_flags(flags: &DriverFlags) -> VmFlags {
    VmFlags {
        emit_asm: flags.emit_asm.clone(),
        emit_asm_boots: flags.emit_asm_boots,
        emit_asm_file: flags.emit_asm_file.clone(),
        emit_bytecode_boots: flags.emit_bytecode_boots,
        emit_bytecode_compiler: flags.emit_bytecode_compiler.clone(),
        emit_compiler: flags.emit_compiler,
        emit_graph: flags.emit_graph.clone(),
        emit_graph_after_each_pass: flags.emit_graph_after_each_pass,
        emit_stubs: flags.emit_stubs,
        always_boots: flags.always_boots,
        use_boots: flags.use_boots.clone(),
        enable_perf: flags.enable_perf,
        omit_bounds_check: flags.omit_bounds_check,
        emit_debug: flags.emit_debug.clone(),
        emit_debug_boots: flags.emit_debug_boots,
        emit_debug_native: flags.emit_debug_native,
        emit_debug_compile: flags.emit_debug_compile,
        emit_debug_entry: flags.emit_debug_entry,
        gc_events: flags.gc_events,
        gc_stress: flags.gc_stress,
        gc_stress_in_lazy_compile: flags.gc_stress_in_lazy_compile,
        gc_stress_minor: flags.gc_stress_minor,
        gc_stats: flags.gc_stats,
        gc_verbose: flags.gc_verbose,
        gc_verify: flags.gc_verify,
        gc_worker: flags.gc_worker,
        gc_young_size: flags.gc_young_size,
        gc_semi_ratio: flags.gc_semi_ratio,
        gc: flags.gc,
        compiler: flags.compiler,
        min_heap_size: flags.min_heap_size,
        max_heap_size: flags.max_heap_size,
        code_size: flags.code_size,
        readonly_size: flags.readonly_size,
        disable_tlab: flags.disable_tlab,
        disable_barrier: flags.disable_barrier,
        bootstrap_compiler: flags.bootstrap_compiler,
        snapshot_on_oom: flags.snapshot_on_oom.clone(),
    }
}
