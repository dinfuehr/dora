pub mod cmd;
pub mod ctxt;

use self::ctxt::Context;
use error::diag::Diagnostic;

use parser::ast;
use parser::Parser;
use semck;

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

    let ctxt = Context {
        args: &args,
        interner: &interner,
        map: &ast_map,
        ast: &ast,
        diagnostic: Diagnostic::new()
    };

    semck::check(&ctxt);
}
