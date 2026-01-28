use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};

use dora_frontend::sema::SemaCreationParams;
use dora_runtime::VmFlags;
use dora_runtime::{CollectorName, Compiler, MemSize};

fn version_string() -> &'static str {
    const VERSION: &str = env!("CARGO_PKG_VERSION");
    const GIT_HASH: Option<&str> = option_env!("DORA_GIT_HASH");

    match GIT_HASH {
        Some(hash) => {
            // Use Box::leak to create a static string
            Box::leak(format!("{} (commit {})", VERSION, hash).into_boxed_str())
        }
        None => VERSION,
    }
}

#[derive(Parser)]
#[command(name = "dora", version = version_string(), about = "The Dora programming language")]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Command>,

    #[command(flatten)]
    pub run_args: RunArgs,
}

#[derive(Subcommand)]
pub enum Command {
    /// Run a Dora program
    Run(RunArgs),
    /// Run tests
    Test(TestArgs),
    /// Build/compile a program
    Build(BuildArgs),
}

#[derive(Args, Default)]
pub struct RunArgs {
    /// Input file to run
    pub file: Option<String>,

    /// Arguments passed to the program
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    pub arguments: Vec<String>,

    #[command(flatten)]
    pub common: CommonFlags,

    #[command(flatten)]
    pub runtime: RuntimeFlags,
}

#[derive(Args, Default)]
pub struct TestArgs {
    /// Input file to test (optional)
    pub file: Option<String>,

    /// Filter tests by name
    #[arg(long)]
    pub test_filter: Option<String>,

    /// Run boots tests
    #[arg(long)]
    pub test_boots: bool,

    #[command(flatten)]
    pub common: CommonFlags,

    #[command(flatten)]
    pub runtime: RuntimeFlags,
}

#[derive(Args)]
pub struct BuildArgs {
    /// Input file to build
    pub file: String,

    /// Output file (default: <input>.dora-package in current directory)
    #[arg(short, long)]
    pub output: Option<String>,

    /// Build as standard library
    #[arg(long)]
    pub stdlib: bool,

    #[command(flatten)]
    pub common: CommonFlags,
}

#[derive(Args, Default)]
pub struct CommonFlags {
    /// External packages (name path pairs)
    #[arg(long, value_names = ["NAME", "PATH"], num_args = 2)]
    package: Vec<String>,

    /// Emits AST to stdout for function
    #[arg(long, value_name = "FCT")]
    pub emit_ast: Option<String>,

    /// Emits bytecode to stdout for function
    #[arg(long, value_name = "FCT")]
    pub emit_bytecode: Option<String>,

    /// Report all warnings
    #[arg(long)]
    pub report_all_warnings: bool,

    /// Enable boots compiler
    #[arg(long)]
    pub boots: bool,
}

impl CommonFlags {
    pub fn packages(&self) -> Vec<(String, PathBuf)> {
        self.package
            .chunks(2)
            .filter_map(|chunk| {
                if chunk.len() == 2 {
                    Some((chunk[0].clone(), PathBuf::from(&chunk[1])))
                } else {
                    None
                }
            })
            .collect()
    }
}

pub fn include_boots(common: &CommonFlags, runtime: &RuntimeFlags) -> bool {
    common.boots || runtime.needs_boots()
}

#[derive(Args, Default)]
pub struct RuntimeFlags {
    /// Emits assembly code to stdout for function
    #[arg(long, value_name = "FCT")]
    pub emit_asm: Option<String>,

    /// Emits assembly code into file
    #[arg(long, value_name = "FILE")]
    pub emit_asm_file: Option<String>,

    /// Emits assembly code for all boots compilations
    #[arg(long)]
    pub emit_asm_boots: bool,

    /// Emits graph for function
    #[arg(long, value_name = "FCT")]
    pub emit_graph: Option<String>,

    /// Emits graph after each pass
    #[arg(long)]
    pub emit_graph_after_each_pass: bool,

    /// Emits bytecode before compilation
    #[arg(long, value_name = "FCT", default_missing_value = "all", num_args = 0..=1)]
    pub emit_bytecode_compiler: Option<String>,

    /// Emits bytecode for boots
    #[arg(long)]
    pub emit_bytecode_boots: bool,

    /// Emits compiler info
    #[arg(long)]
    pub emit_compiler: bool,

    /// Emits generated stubs
    #[arg(long)]
    pub emit_stubs: bool,

    /// Emits debug instruction at beginning of functions
    #[arg(long, value_name = "FCT")]
    pub emit_debug: Option<String>,

    /// Emits debug instruction at beginning of native stub
    #[arg(long)]
    pub emit_debug_native: bool,

    /// Emits debug instruction at beginning of compile stub
    #[arg(long)]
    pub emit_debug_compile: bool,

    /// Emits debug instruction at beginning of entry stub
    #[arg(long)]
    pub emit_debug_entry: bool,

    /// Emits debug instruction at beginning of boots function
    #[arg(long)]
    pub emit_debug_boots: bool,

    /// Uses boots for all functions in the program
    #[arg(long)]
    pub always_boots: bool,

    /// Use boots for specific function
    #[arg(long, value_name = "FCT")]
    pub use_boots: Option<String>,

    /// Omit array index out of bounds checks
    #[arg(long)]
    pub omit_bounds_check: bool,

    /// Enable dump for perf
    #[arg(long)]
    pub enable_perf: bool,

    /// Dump GC events
    #[arg(long)]
    pub gc_events: bool,

    /// Collect garbage at every allocation
    #[arg(long)]
    pub gc_stress: bool,

    /// GC stress in lazy compile
    #[arg(long)]
    pub gc_stress_in_lazy_compile: bool,

    /// Minor collection at every allocation
    #[arg(long)]
    pub gc_stress_minor: bool,

    /// Print GC statistics
    #[arg(long)]
    pub gc_stats: bool,

    /// Verbose GC
    #[arg(long)]
    pub gc_verbose: bool,

    /// Verify heap before and after collections
    #[arg(long)]
    pub gc_verify: bool,

    /// Number of GC worker threads
    #[arg(long, default_value_t = 0)]
    pub gc_worker: usize,

    /// Switch GC (zero, copy, sweep, swiper)
    #[arg(long, value_parser = parse_collector)]
    pub gc: Option<CollectorName>,

    /// Use fixed size for young generation
    #[arg(long, value_parser = parse_mem_size)]
    pub gc_young_size: Option<MemSize>,

    /// Use fixed ratio of semi space in young generation
    #[arg(long)]
    pub gc_semi_ratio: Option<usize>,

    /// Switch default compiler (cannon, boots)
    #[arg(long, value_parser = parse_compiler)]
    pub compiler: Option<Compiler>,

    /// Set minimum heap size
    #[arg(long, value_parser = parse_mem_size)]
    pub min_heap_size: Option<MemSize>,

    /// Set maximum heap size
    #[arg(long, value_parser = parse_mem_size)]
    pub max_heap_size: Option<MemSize>,

    /// Set code size limit
    #[arg(long, value_parser = parse_mem_size)]
    pub code_size: Option<MemSize>,

    /// Set readonly size limit
    #[arg(long, value_parser = parse_mem_size)]
    pub readonly_size: Option<MemSize>,

    /// Disable tlab allocation
    #[arg(long)]
    pub disable_tlab: bool,

    /// Disable barriers
    #[arg(long)]
    pub disable_barrier: bool,

    /// Runs bootstrap process for boots compiler
    #[arg(long)]
    pub bootstrap_compiler: bool,

    /// Snapshot on OOM
    #[arg(long, value_name = "FILE")]
    pub snapshot_on_oom: Option<String>,
}

impl RuntimeFlags {
    pub fn needs_boots(&self) -> bool {
        self.always_boots || self.use_boots.is_some()
    }

    pub fn to_vm_flags(&self) -> VmFlags {
        VmFlags {
            emit_asm: self.emit_asm.clone(),
            emit_asm_boots: self.emit_asm_boots,
            emit_asm_file: self.emit_asm_file.clone(),
            emit_bytecode_boots: self.emit_bytecode_boots,
            emit_bytecode_compiler: self.emit_bytecode_compiler.clone(),
            emit_compiler: self.emit_compiler,
            emit_graph: self.emit_graph.clone(),
            emit_graph_after_each_pass: self.emit_graph_after_each_pass,
            emit_stubs: self.emit_stubs,
            always_boots: self.always_boots,
            use_boots: self.use_boots.clone(),
            enable_perf: self.enable_perf,
            omit_bounds_check: self.omit_bounds_check,
            emit_debug: self.emit_debug.clone(),
            emit_debug_boots: self.emit_debug_boots,
            emit_debug_native: self.emit_debug_native,
            emit_debug_compile: self.emit_debug_compile,
            emit_debug_entry: self.emit_debug_entry,
            gc_events: self.gc_events,
            gc_stress: self.gc_stress,
            gc_stress_in_lazy_compile: self.gc_stress_in_lazy_compile,
            gc_stress_minor: self.gc_stress_minor,
            gc_stats: self.gc_stats,
            gc_verbose: self.gc_verbose,
            gc_verify: self.gc_verify,
            gc_worker: self.gc_worker,
            gc_young_size: self.gc_young_size,
            gc_semi_ratio: self.gc_semi_ratio,
            gc: self.gc,
            compiler: self.compiler,
            min_heap_size: self.min_heap_size,
            max_heap_size: self.max_heap_size,
            code_size: self.code_size,
            readonly_size: self.readonly_size,
            disable_tlab: self.disable_tlab,
            disable_barrier: self.disable_barrier,
            bootstrap_compiler: self.bootstrap_compiler,
            snapshot_on_oom: self.snapshot_on_oom.clone(),
        }
    }
}

fn parse_collector(s: &str) -> Result<CollectorName, String> {
    match s {
        "zero" => Ok(CollectorName::Zero),
        "copy" => Ok(CollectorName::Copy),
        "sweep" => Ok(CollectorName::Sweep),
        "swiper" => Ok(CollectorName::Swiper),
        _ => Err(format!(
            "unknown collector '{}', expected: zero, copy, sweep, swiper",
            s
        )),
    }
}

fn parse_compiler(s: &str) -> Result<Compiler, String> {
    match s {
        "cannon" => Ok(Compiler::Cannon),
        "boots" => Ok(Compiler::Boots),
        _ => Err(format!("unknown compiler '{}', expected: cannon, boots", s)),
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

pub fn create_sema_params(
    program_file: PathBuf,
    packages: Vec<(String, PathBuf)>,
    include_boots: bool,
) -> SemaCreationParams {
    SemaCreationParams::new()
        .set_program_path(program_file)
        .set_package_paths(packages)
        .set_boots(include_boots)
}
