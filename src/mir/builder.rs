use ctxt::Fct;
use dora_parser::ast::{Stmt, StmtBlockType, StmtReturnType};
use mir::repr::{BasicBlock, BasicBlockId, Mir};

pub fn generate<'a, 'ast>(fct: &'a Fct<'ast>) -> Result<Mir, ()> {
    let mut builder = Builder::new(fct);

    try!(builder.generate());
    Ok(builder.mir)
}

struct Builder<'a, 'ast: 'a> {
    fct: &'a Fct<'ast>,
    mir: Mir,
    current: BasicBlockId,
}

impl<'a, 'ast> Builder<'a, 'ast> {
    fn new(fct: &'a Fct<'ast>) -> Builder<'a, 'ast> {
        Builder {
            fct: fct,
            mir: Mir::new(fct.name),
            current: BasicBlockId(0),
        }
    }

    fn block_mut(&mut self) -> &mut BasicBlock {
        self.mir.block_mut(self.current)
    }

    fn generate(&mut self) -> Result<(), ()> {
        // let src = self.fct.src();
        // let src = src.lock().unwrap();
        //
        // self.stmt(self.fct.block())

        Ok(())
    }

    fn stmt(&mut self, stmt: &Stmt) -> Result<(), ()> {
        match *stmt {
            Stmt::StmtReturn(ref stmt) => self.stmt_return(stmt),
            Stmt::StmtBlock(ref stmt) => self.stmt_block(stmt),
            _ => Err(()),
        }
    }

    fn stmt_return(&mut self, stmt: &StmtReturnType) -> Result<(), ()> {
        match stmt.expr {
            Some(_) => Err(()),
            None => {
                self.block_mut().add_ret();
                Ok(())
            }
        }
    }

    fn stmt_block(&mut self, stmt: &StmtBlockType) -> Result<(), ()> {
        for stmt in &stmt.stmts {
            try!(self.stmt(stmt));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::generate;
    use test::parse;

    #[test]
    fn generate_empty() {
        parse("fun f() {}", |ctxt| {
            let fid = ctxt.fct_by_name("f").unwrap();
            let fct = ctxt.fcts[fid].borrow();
            let mir = generate(&*fct).unwrap();
            assert_eq!("f", *ctxt.interner.str(mir.name));
        });
    }
}
