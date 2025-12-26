use std::env;
use std::fs;

use dora_format::format_source;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: dora-format <input> <output>");
        std::process::exit(1);
    }

    let input_path = &args[1];
    let output_path = &args[2];
    let content = fs::read_to_string(input_path)?;

    let output = match format_source(&content) {
        Ok(output) => output,
        Err(errors) => {
            for error in errors {
                eprintln!("Parse error at {}: {}", error.span, error.error.message());
            }
            return Err("aborting due to parse errors".into());
        }
    };

    fs::write(output_path, output)?;

    Ok(())
}
