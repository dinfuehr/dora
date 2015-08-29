use std::default::Default;

use docopt::Docopt;

pub fn parse() -> Args {
    Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit())
}

// Write the Docopt usage string.
static USAGE: &'static str = "
Usage: dora [options] <file>
       dora (--version | --help)

Options:
    -h, --help  Shows this text
    --version   Shows version
    --emit-ast  Emits AST to stdout
";

#[derive(Debug, RustcDecodable)]
pub struct Args {
    pub arg_file: String,
    pub flag_emit_ast: bool,
    pub flag_version: bool
}

impl Default for Args {
    fn default() -> Args {
        Args {
            arg_file: "".into(),
            flag_emit_ast: false,
            flag_version: false
        }
    }
}
