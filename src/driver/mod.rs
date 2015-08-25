mod cmd;

use parser::ast;
use parser::Parser;

pub fn compile() {
    let args = cmd::parse();

    if args.flag_version {
        println!("dora v0.01b");
        return;
    }

    let mut parser = match Parser::from_file(&args.arg_file) {
        Err(_) => {
            println!("unable to read file `{}`", &args.arg_file);
            panic!();
        }

        Ok(parser) => parser
    };

    let (ast, interner) = match parser.parse() {
        Ok(ret) => ret,

        Err(error) => {
            error.print();
            panic!();
        }
    };

    ast::dump::dump(&ast, &interner);

    let ast_map = ast::map::build(&ast, &interner);
}
