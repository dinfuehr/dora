use ctxt::Context;
use mir::*;
use mir::Instr::*;
use mir::Opnd::*;

pub fn dump<'a, 'ast>(ctxt: &'a Context<'a, 'ast>, mir: &'a Mir) {
    Dumper::new(ctxt, mir).dump();
}

struct Dumper<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    mir: &'a Mir,
}

impl<'a, 'ast> Dumper<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>, mir: &'a Mir) -> Dumper<'a, 'ast> {
        Dumper {
            ctxt: ctxt,
            mir: mir
        }
    }

    fn dump(&self) {
        println!("fct ({} block(s), {} var(s))", self.mir.blocks.len(), self.mir.vars.len());

        for var in &self.mir.vars {
            let name = self.ctxt.interner.str(var.name);
            println!("\t%{}:{}", name, var.ty.to_string());
        }

        for block in &self.mir.blocks {
            let block = block.clone();
            self.dump_block(&block.borrow());
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
            InstrGoto(block) => println!("goto -> block {}", block.to_string()),
            InstrTest(ref instr) =>
                println!("test {} -> block {} -> block {}", self.opnd(instr.opnd),
                    instr.true_block.to_string(), instr.false_block.to_string()),
            InstrAssign(ref instr) => println!("{} = {}",
                self.opnd(instr.dest), self.opnd(instr.src)),
            InstrUn(ref instr) => println!("{} = {} {}",
                self.opnd(instr.dest), instr.op.as_str(), self.opnd(instr.src)),
            InstrBin(ref instr) => println!("{} = {} {} {}", self.opnd(instr.dest),
                self.opnd(instr.lhs), instr.op.as_str(), self.opnd(instr.rhs)),
            InstrPhi(ref instr) => {
                let dest = OpndVar(instr.var_id, instr.dest);
                print!("{} = phi(", self.opnd(dest));
                let mut first = true;

                for opnd in &instr.opnds {
                    if !first { print!(", "); }
                    let opnd = OpndVar(instr.var_id, *opnd);
                    print!("{}", self.opnd(opnd));
                    first = false;
                }

                println!(") (backup {})", instr.backup);
            },
            InstrStr(ref instr) => println!("{} = str {:?}",
                self.opnd(instr.dest), instr.value),
            _ => panic!("unknown instruction")
        }
    }

    fn opnd(&self, opnd: Opnd) -> String {
        match opnd {
            OpndReg(ind) => format!("%reg{}", ind),
            OpndInt(val) => format!("{}", val),
            OpndBool(val) => format!("{}", val),
            OpndVar(id, ssa) => {
                let var = &self.mir.vars[id.0];
                let name = self.ctxt.interner.str(var.name);

                format!("%{}.{}", name, ssa)
            }
        }
    }
}
