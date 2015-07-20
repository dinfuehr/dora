use ast::Ast;

pub struct AstDumper<'a> {
    ast: &'a Ast
}

impl<'a> AstDumper<'a> {
    pub fn new(ast: &Ast) -> AstDumper {
        AstDumper {
            ast: ast
        }
    }

    pub fn dump(&self) {
        println!("dump ast");
    }
}
