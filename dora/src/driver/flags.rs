use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};

use dora_frontend::sema::SemaCreationParams;
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
    /// Build/compile a program
    Build(BuildArgs),
    /// Compile to standalone binary
    Compile(CompileArgs),
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

#[derive(Args)]
pub struct CompileArgs {
    /// Input file to compile
    pub file: String,

    /// Output binary (default: out)
    #[arg(short, long, default_value = "out")]
    pub output: String,

    /// Emit assembly instead of linking (output to -o path with .s extension)
    #[arg(short = 'S')]
    pub emit_asm: bool,

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

    /// Compiler binary to use
    #[arg(long, value_name = "PATH")]
    pub compiler: Option<PathBuf>,

    /// Internal: compile the Boots compiler image
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
