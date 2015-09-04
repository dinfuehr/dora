use codegen::buffer::Buffer;
use driver::ctxt::Context;
use parser::ast::*;
use parser::ast::Stmt::*;

pub struct CodeGen<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    fct: &'ast Function,
    buffer: Buffer,
}

impl<'a, 'ast> CodeGen<'a, 'ast> {
    pub fn new(ctxt: &'a Context<'a, 'ast>, fct: &'ast Function) -> CodeGen<'a, 'ast> {
        CodeGen {
            ctxt: ctxt,
            fct: fct,
            buffer: Buffer::new()
        }
    }

    pub fn generate(&mut self) {

    }

    fn emit_stmt_return(&mut self, s: &'ast StmtReturnType) {
        unreachable!()
    }
}

impl<'a, 'ast> visit::Visitor<'ast> for CodeGen<'a, 'ast> {
    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtReturn(ref stmt) => self.emit_stmt_return(stmt),
            _ => unreachable!()
        }
    }
}
