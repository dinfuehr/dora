use std::default::Default;
use std::ops::Deref;

use docopt::Docopt;
use rustc_serialize;

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
    --gc-events             Dump GC events
    --gc-stress             Collect garbage at every allocation
    --gc-stats              Print GC statistics
    --gc-copy               Use Copy Collection with Cheney Algorithm
    --gc-copy-size=<SIZE>   Set size of copy collection space
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
    pub flag_gc_events: bool,
    pub flag_gc_stress: bool,
    pub flag_gc_stats: bool,
    pub flag_gc_copy: bool,
    pub flag_gc_copy_size: Option<MemSize>,
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
            flag_gc_events: false,
            flag_gc_stress: false,
            flag_gc_stats: false,
            flag_gc_copy: false,
            flag_gc_copy_size: None,
        }
    }
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
