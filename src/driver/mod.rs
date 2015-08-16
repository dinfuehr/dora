mod cmd;

use semck;
use parser::ast;
use parser::Parser;

use std::process::exit;

pub fn compile() {
    let cmd = match cmd::parse() {
        Ok(cmd) => cmd,

        Err(msg) => {
            println!("{}\n", msg);
            cmd::usage();

            exit(1);
        }
    };

    let mut parser = match Parser::from_file(cmd.filename()) {
        Err(_) => {
            println!("unable to read file `{}`", cmd.filename());
            exit(1);
        }

        Ok(parser) => parser
    };

    let (ast, mut interner) = match parser.parse() {
        Ok(ret) => ret,

        Err(error) => {
            error.print();
            exit(1);
        }
    };

    ast::dump::dump(&ast, &interner);

    if let Err(errors) = semck::check(&ast, &mut interner) {
        for err in &errors {
            err.print();
        }

        println!("\n{} errors found", errors.len());
        exit(1);
    }
}
