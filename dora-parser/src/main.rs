use std::env;
use std::fs;
use std::sync::Arc;

use dora_parser::Parser;
use dora_parser::ast::printer::dump_file_to_string;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: dora-parser <input>");
        std::process::exit(1);
    }

    let input_path = &args[1];
    let content = Arc::new(fs::read_to_string(input_path)?);
    let (file, errors) = Parser::from_shared_string(content).parse();

    if !errors.is_empty() {
        for error in errors {
            eprintln!("Parse error at {}: {}", error.span, error.error.message());
        }
        return Err("aborting due to parse errors".into());
    }

    print!("{}", dump_file_to_string(&file));

    Ok(())
}
