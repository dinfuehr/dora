use std::cell::Cell;

use ast::Ast;
use ast::Elem::{self, ElemFunction};
use ast::Function;
use ast::Stmt::{self, StmtBlock};
use ast::StmtBlockType;
use ast::Type::{self, TypeBasic, TypeUnit};
use interner::Name;

macro_rules! dump {
    ($self_:ident, $($x:expr),*) => {{
        for _ in 0..($self_.indent*2) {
            print!(" ");
        }

        println!($($x,)*);
    }};
}

pub struct AstDumper<'a> {
    ast: &'a Ast,
    indent: u32,
}

impl<'a> AstDumper<'a> {
    pub fn new(ast: &Ast) -> AstDumper {
        AstDumper {
            ast: ast,
            indent: 0
        }
    }

    pub fn dump(&mut self) {
        for el in &self.ast.elements {
            match *el {
                ElemFunction(ref fct) => self.dump_fct(fct),
                _ => unreachable!()
            }
        }
    }

    fn dump_fct(&mut self, fct: &Function) {
        dump!(self, "fct {} @ {}", self.str(fct.name), fct.pos);

        self.indent(|d| {
            if(fct.params.is_empty()) {
                dump!(d, "no params");
            } else {
                for param in &fct.params {
                    dump!(d, "param {} @ {}", d.str(param.name), param.pos);
                    d.indent(|d| d.dump_type(&param.data_type));
                }
            }
        });

        dump!(self, "fct {} returns", self.str(fct.name));
        self.indent(|d| d.dump_type(&fct.return_type));

        self.dump_stmt(&fct.block);
    }

    fn dump_type(&mut self, ty: &Type) {
        match *ty {
            TypeBasic(name) => dump!(self, "type {}", self.str(name)),
            TypeUnit => dump!(self, "type () / void")
        }
    }

    fn dump_stmt(&mut self, stmt: &Stmt) {
        match *stmt {
            StmtBlock(ref block) => self.dump_stmt_block(block),
            _ => unimplemented!()
        }
    }

    fn dump_stmt_block(&mut self, block: &StmtBlockType) {
        dump!(self, "block ({} statements)", block.stmts.len());

        self.indent(|d| {
            if(block.stmts.is_empty()) {
                dump!(d, "no statements");
            } else {
                for stmt in &block.stmts {
                    d.dump_stmt(stmt);
                }
            }
        });

        dump!(self, "block end");
    }

    fn indent<F>(&mut self, fct: F) where F: Fn(&mut AstDumper) -> () {
        let old = self.indent;
        self.indent = old+1;

        fct(self);

        self.indent = old;
    }

    fn str(&self, name: Name) -> &str {
        self.ast.str(name)
    }
}
