use ast::{Stmt,
          StmtBlockType,
          StmtReturnType};
use ctxt::Fct;
use mir::repr::{BasicBlockId, BasicBlock, Mir};

pub fn generate<'a, 'ast>(fct: &'a Fct<'ast>) -> Mir {
    let mut builder = Builder::new(fct);
    builder.generate();

    builder.mir
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

    fn generate(&mut self) {
        //let block_id = self.mir.add_block();
        //self.use_block(block_id);

        let src = self.fct.src();
        let src = src.lock().unwrap();
        self.stmt(&src.ast.block);
    }

    fn stmt(&mut self, stmt: &Stmt) {
        match *stmt {
            Stmt::StmtReturn(ref stmt) => self.stmt_return(stmt),
            Stmt::StmtBlock(ref stmt) => self.stmt_block(stmt),
            _ => unreachable!()
        }
    }

    fn stmt_return(&mut self, stmt: &StmtReturnType) {
        assert!(stmt.expr.is_none());
        self.block_mut().add_ret();
    }

    fn stmt_block(&mut self, stmt: &StmtBlockType) {
        for stmt in &stmt.stmts {
            self.stmt(stmt);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::generate;
    use test::parse;

    #[test]
    fn generate_empty() {
        parse("fun f() {}", |ctxt| {
            let mir = generate(ctxt.fcts.last().unwrap());
            assert_eq!("f", *ctxt.interner.str(mir.name));
        });
    }
}

