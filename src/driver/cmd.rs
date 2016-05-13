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
    -h, --help          Shows this text
    --version           Shows version
    --emit-ast          Emits AST to stdout
    --emit-asm          Emits assembly code to stdout
    --emit-stubs        Emits generated stubs
    --emit-debug        Emits debug instruction at beginning of functions
    --asm-syntax TYPE   Emits assembly with Intel or AT&T syntax
                        Allowed values: intel, att
    --gc-dump           Dump GC actions
";

#[derive(Debug, RustcDecodable)]
pub struct Args {
    pub arg_argument: Option<Vec<String>>,
    pub arg_file: String,
    pub flag_emit_ast: bool,
    pub flag_emit_asm: bool,
    pub flag_emit_stubs: bool,
    pub flag_version: bool,
    pub flag_emit_debug: bool,
    pub flag_asm_syntax: Option<AsmSyntax>,
    pub flag_gc_dump: bool,
}

impl Default for Args {
    fn default() -> Args {
        Args {
            arg_argument: None,
            arg_file: "".into(),
            flag_emit_ast: false,
            flag_emit_asm: false,
            flag_emit_stubs: false,
            flag_emit_debug: false,
            flag_version: false,
            flag_asm_syntax: None,
            flag_gc_dump: false,
        }
    }
}

#[derive(Copy, Clone, Debug, RustcDecodable)]
pub enum AsmSyntax {
    Intel, Att
}
