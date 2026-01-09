use std::fs;
use std::sync::Arc;

use clap::Parser as ClapParser;
use dora_parser::Parser;
use dora_parser::ast::json::dump_json;
use dora_parser::ast::printer::dump_file_to_string_with_trivia;

#[derive(ClapParser)]
#[command(name = "dora-parser", about = "Parse Dora source files")]
struct Args {
    input: String,
    #[arg(long, action = clap::ArgAction::SetTrue, default_value_t = true)]
    #[arg(long = "no-trivia", action = clap::ArgAction::SetFalse)]
    trivia: bool,
    #[arg(long, action = clap::ArgAction::SetTrue)]
    json: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    let input_path = &args.input;
    let content = Arc::new(fs::read_to_string(input_path)?);
    let (file, errors) = Parser::from_shared_string(content).parse();

    if !errors.is_empty() {
        for error in errors {
            eprintln!("Parse error at {}: {}", error.span, error.error.message());
        }
        return Err("aborting due to parse errors".into());
    }

    if args.json {
        print!("{}", dump_json(&file, args.trivia));
    } else {
        print!("{}", dump_file_to_string_with_trivia(&file, args.trivia));
    }

    Ok(())
}
