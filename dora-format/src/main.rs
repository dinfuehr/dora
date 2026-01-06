use std::fs;
use std::path::PathBuf;

use clap::Parser;

use dora_format::{format_source, format_source_with_line_length};

#[derive(Parser)]
#[command(about = "Format Dora source files.")]
struct Args {
    #[arg(short = 'i', long = "in-place", conflicts_with = "output")]
    in_place: bool,
    #[arg(long = "line-length", value_parser = clap::value_parser!(u32).range(1..))]
    line_length: Option<u32>,
    #[arg(value_name = "input")]
    input: PathBuf,
    #[arg(value_name = "output")]
    output: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let content = fs::read_to_string(&args.input)?;

    let result = match args.line_length {
        Some(line_length) => format_source_with_line_length(&content, line_length),
        None => format_source(&content),
    };
    let output = match result {
        Ok(output) => output,
        Err(errors) => {
            for error in errors {
                eprintln!("Parse error at {}: {}", error.span, error.error.message());
            }
            return Err("aborting due to parse errors".into());
        }
    };

    if args.in_place {
        fs::write(&args.input, output.as_str())?;
    } else if let Some(output_path) = &args.output {
        fs::write(output_path, output.as_str())?;
    } else {
        print!("{}", output);
    }

    Ok(())
}
