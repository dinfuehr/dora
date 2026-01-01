use std::env;
use std::fs;

use dora_format::format_source;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 || args.len() > 4 {
        eprintln!("Usage: dora-format [-i] <input> [output]");
        std::process::exit(1);
    }

    let mut in_place = false;
    let mut paths: Vec<&str> = Vec::new();
    for arg in args.iter().skip(1) {
        if arg == "-i" {
            in_place = true;
        } else {
            paths.push(arg.as_str());
        }
    }

    if paths.is_empty() || paths.len() > 2 || (in_place && paths.len() != 1) {
        eprintln!("Usage: dora-format [-i] <input> [output]");
        std::process::exit(1);
    }

    let input_path = paths[0];
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

    if in_place {
        fs::write(input_path, output.as_str())?;
    } else if paths.len() == 2 {
        let output_path = paths[1];
        fs::write(output_path, output.as_str())?;
    } else {
        print!("{}", output);
    }

    Ok(())
}
