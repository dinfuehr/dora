use std::default::Default;

use docopt::Docopt;

pub fn parse() -> Args {
    Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit())
}

// Write the Docopt usage string.
static USAGE: &'static str = "
Usage: dora [options] <file> [--] [<argument>...]
       dora (--version | --help)

Options:
    -h, --help              Shows this text
    --version               Shows version
    --emit-ast              Emits AST to stdout
    --emit-asm=<fct>        Emits assembly code to stdout
    --emit-stubs            Emits generated stubs
    --emit-debug=<fct>      Emits debug instruction at beginning of functions
    --omit-bounds-check     Omit array index out of bounds checks
    --asm-syntax TYPE       Emits assembly with Intel or AT&T syntax
                            Allowed values: intel, att
    --enable-perf           Enable dump for perf
    --gc-dump               Dump GC actions
    --gc-stress             Collect garbage at every allocation
    --gc-stats              Print GC statistics
";

#[derive(Debug, RustcDecodable)]
pub struct Args {
    pub arg_argument: Option<Vec<String>>,
    pub arg_file: String,
    pub flag_emit_ast: bool,
    pub flag_emit_asm: Option<String>,
    pub flag_emit_stubs: bool,
    pub flag_enable_perf: bool,
    pub flag_omit_bounds_check: bool,
    pub flag_version: bool,
    pub flag_emit_debug: Option<String>,
    pub flag_asm_syntax: Option<AsmSyntax>,
    pub flag_gc_dump: bool,
    pub flag_gc_stress: bool,
    pub flag_gc_stats: bool,
}

impl Default for Args {
    fn default() -> Args {
        Args {
            arg_argument: None,
            arg_file: "".into(),
            flag_emit_ast: false,
            flag_emit_asm: None,
            flag_emit_stubs: false,
            flag_emit_debug: None,
            flag_enable_perf: false,
            flag_omit_bounds_check: false,
            flag_version: false,
            flag_asm_syntax: None,
            flag_gc_dump: false,
            flag_gc_stress: false,
            flag_gc_stats: false,
        }
    }
}

#[derive(Copy, Clone, Debug, RustcDecodable)]
pub enum AsmSyntax {
    Intel, Att
}
