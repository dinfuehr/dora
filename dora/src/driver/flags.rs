use std::path::PathBuf;

use clap::{ArgGroup, Args, Parser, Subcommand};

use dora_runtime::{CollectorName, TargetArch, parse_collector, parse_target_arch};

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
    pub command: Command,
}

#[derive(Subcommand)]
pub enum Command {
    /// Compile to standalone binary
    Compile(CompileArgs),

    /// Create a new Dora package
    Init(InitArgs),
}

#[derive(Args)]
#[command(group(
    ArgGroup::new("package_kind")
        .args(["bin", "lib"])
        .multiple(false)
))]
pub struct InitArgs {
    /// Create a binary package
    #[arg(long)]
    pub bin: bool,

    /// Create a library package
    #[arg(long)]
    pub lib: bool,

    /// Package name (default: directory name)
    #[arg(long)]
    pub name: Option<String>,

    /// Package directory to create
    pub path: PathBuf,
}

#[derive(Args)]
pub struct CompileArgs {
    /// Input file to compile
    pub file: String,

    /// Output file (default: out, or <input>.dora-package with -c)
    #[arg(short, long)]
    pub output: Option<String>,

    /// Emit .dora-package instead of compiling to a native binary
    #[arg(short = 'c')]
    pub compile_to_package_only: bool,

    /// Emit assembly instead of linking (output to -o path with .s extension)
    #[arg(short = 'S')]
    pub asm_only: bool,

    /// Target architecture (x64, arm64; default: host)
    #[arg(long, value_parser = parse_target_arch)]
    pub target: Option<TargetArch>,

    /// Switch GC (zero, copy, sweep, swiper)
    #[arg(long, value_parser = parse_collector)]
    pub gc: Option<CollectorName>,

    /// Emits graph for function
    #[arg(long, value_name = "FCT")]
    pub emit_graph: Option<String>,

    /// Emits graph after each pass
    #[arg(long)]
    pub emit_graph_after_each_pass: bool,

    /// Compile tests and use the unit test runner as entry point
    #[arg(long)]
    pub test: bool,

    /// Print subcommands run by the compile driver
    #[arg(short, long)]
    pub verbose: bool,

    /// Print AOT compile, assembly, and link timings
    #[arg(long)]
    pub emit_timings: bool,

    /// Internal: compile the standard library package
    #[arg(long, hide = true)]
    pub internal_compile_stdlib: bool,

    /// Compiler binary to use
    #[arg(long, value_name = "PATH")]
    pub compiler: Option<PathBuf>,

    /// Internal: compile the Boots package or Boots compiler image
    #[arg(long, hide = true)]
    pub internal_compile_boots: bool,

    /// Internal: shortcut for --compiler=dora-cannon-compiler
    #[arg(long, hide = true)]
    pub cannon: bool,

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

    /// Include the boots package
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
