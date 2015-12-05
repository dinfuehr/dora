use ctxt::Context;
use ir::*;
use ir::Instr::*;
use ir::Opnd::*;

pub fn dump<'a, 'ast>(ctxt: &Context<'a, 'ast>, fct: &Fct) {
    println!("dump ir");

    for block in &fct.blocks {
        dump_block(block);
    }
}

fn dump_block(block: &Block) {
    println!("{}:", block.id.to_string());

    for instr in &block.instructions {
        dump_instr(instr);
    }
}

fn dump_instr(instr: &Instr) {
    match *instr {
        InstrRet(val) => {
            if let Some(opnd) = val {
                println!("\tret {}", dump_opnd(opnd));
            } else {
                println!("\tret");
            }
        }

        InstrGoto(block) => println!("\tgoto {}", block.to_string()),
        InstrTest(opnd) => println!("\ttest {}", dump_opnd(opnd)),
        InstrAssign(dest, src) => println!("\t{} = {}", dump_opnd(dest), dump_opnd(src)),
        InstrUn(dest, op, src) => println!("\t{} = {} {}", dump_opnd(dest), op.as_str(), dump_opnd(src)),
        InstrBin(dest, lhs, op, rhs) => println!("\t{} = {} {} {}", dump_opnd(dest),
            dump_opnd(lhs), op.as_str(), dump_opnd(rhs)),
        _ => panic!("unknown instruction")
    }
}

fn dump_opnd(opnd: Opnd) -> String {
    match opnd {
        OpndReg(val) => format!("r{}", val),
        OpndInt(val) => format!("{}", val),
        OpndBool(val) => format!("{}", val),
        OpndVar(var, ind) => format!("{}_{}", var.to_string(), ind)
    }
}
