use ctxt::Context;
use ir::*;
use ir::Instr::*;
use ir::Opnd::*;

pub fn dump<'a, 'ast>(ctxt: &'a Context<'a, 'ast>, fct: &'a Fct) {
    Dumper::new(ctxt, fct).dump();
}

struct Dumper<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    fct: &'a Fct,
}

impl<'a, 'ast> Dumper<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>, fct: &'a Fct) -> Dumper<'a, 'ast> {
        Dumper {
            ctxt: ctxt,
            fct: fct
        }
    }

    fn dump(&self) {
        println!("fct ({} block(s), {} var(s))", self.fct.blocks.len(), self.fct.vars.len());

        for var in &self.fct.vars {
            let name = self.ctxt.interner.str(var.name);
            println!("\t{}:{} id={}", name, var.data_type.to_string(), var.id.to_string());
        }

        for block in &self.fct.blocks {
            self.dump_block(block);
        }
    }

    fn dump_block(&self, block: &Block) {
        println!("\tblock {}: ({} instruction(s))",
            block.id.to_string(), block.instructions.len());

        for instr in &block.instructions {
            print!("\t\t");
            self.dump_instr(instr);
        }
    }

    fn dump_instr(&self, instr: &Instr) {
        match *instr {
            InstrRet(Some(opnd)) => println!("ret {}", self.opnd(opnd)),
            InstrRet(None) => println!("ret"),
            InstrGoto(block) => println!("goto {}", block.to_string()),
            InstrTest(opnd) => println!("test {}", self.opnd(opnd)),
            InstrAssign(dest, src) => println!("{} = {}", self.opnd(dest), self.opnd(src)),
            InstrUn(dest, op, src) => println!("{} = {} {}",
                self.opnd(dest), op.as_str(), self.opnd(src)),
            InstrBin(dest, lhs, op, rhs) => println!("{} = {} {} {}", self.opnd(dest),
                self.opnd(lhs), op.as_str(), self.opnd(rhs)),
            _ => panic!("unknown instruction")
        }
    }

    fn opnd(&self, opnd: Opnd) -> String {
        match opnd {
            OpndReg(ind) => format!("%reg{}", ind),
            OpndInt(val) => format!("{}", val),
            OpndBool(val) => format!("{}", val),
            OpndVar(id, ssa) => {
                let var = &self.fct.vars[id.0];
                let name = self.ctxt.interner.str(var.name);

                format!("%{}.{}", name, ssa)
            }
        }
    }
}
