use std::fmt;
use std::mem;

use bytecode::opcode::Bytecode;

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Register(pub usize);

#[derive(Copy, Clone, PartialEq, Debug, Eq, Hash)]
pub struct Label(pub usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct BytecodeIdx(pub usize);

impl BytecodeIdx {
    fn invalid() -> BytecodeIdx {
        BytecodeIdx(usize::max_value())
    }

    fn is_invalid(&self) -> bool {
        self.0 == usize::max_value()
    }
}

impl fmt::Display for BytecodeIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "bc#{}", self.0)
    }
}

#[derive(Copy, Clone)]
pub enum BytecodeType {
    Bool,
    Byte,
    Int,
    Long,
    Float,
    Double,
    Ref,
}

pub struct BytecodeGenerator {
    code: Vec<Bytecode>,
    labels: Vec<Option<BytecodeIdx>>,
    unresolved_jumps: Vec<(BytecodeIdx, Label)>,
    registers: Vec<BytecodeType>,
    next_register: usize,
}

impl BytecodeGenerator {
    pub fn new() -> BytecodeGenerator {
        BytecodeGenerator {
            code: Vec::new(),
            labels: Vec::new(),
            unresolved_jumps: Vec::new(),
            registers: Vec::new(),
            next_register: 0,
        }
    }

    pub fn create_label(&mut self) -> Label {
        self.labels.push(None);
        Label(self.labels.len() - 1)
    }

    pub fn define_label(&mut self) -> Label {
        let dest = BytecodeIdx(self.code.len());
        self.labels.push(Some(dest));
        Label(self.labels.len() - 1)
    }

    pub fn bind_label(&mut self, lbl: Label) {
        assert!(self.labels[lbl.0].is_none(), "bind label twice");
        let dest = BytecodeIdx(self.code.len());
        self.labels[lbl.0] = Some(dest);
    }

    fn dest_label(&self, lbl: Label) -> Option<BytecodeIdx> {
        self.labels[lbl.0]
    }

    fn pc(&self) -> BytecodeIdx {
        BytecodeIdx(self.code.len())
    }

    pub fn emit_add(&mut self, src: Register) {
        self.code.push(Bytecode::Add(src));
    }

    pub fn emit_bitwise_and(&mut self, src: Register) {
        self.code.push(Bytecode::BitwiseAnd(src));
    }

    pub fn emit_bitwise_or(&mut self, src: Register) {
        self.code.push(Bytecode::BitwiseOr(src));
    }

    pub fn emit_bitwise_xor(&mut self, src: Register) {
        self.code.push(Bytecode::BitwiseXor(src));
    }

    pub fn emit_div(&mut self, src: Register) {
        self.code.push(Bytecode::Div(src));
    }

    pub fn emit_ldar(&mut self, src: Register) {
        self.code.push(Bytecode::Ldar(src));
    }

    pub fn emit_lda_int(&mut self, value: u64) {
        self.code.push(Bytecode::LdaInt(value));
    }

    pub fn emit_lda_zero(&mut self) {
        self.code.push(Bytecode::LdaZero);
    }

    pub fn emit_lda_true(&mut self) {
        self.code.push(Bytecode::LdaTrue);
    }

    pub fn emit_lda_false(&mut self) {
        self.code.push(Bytecode::LdaFalse);
    }

    pub fn emit_logical_not(&mut self) {
        self.code.push(Bytecode::LogicalNot);
    }

    pub fn emit_star(&mut self, dest: Register) {
        self.code.push(Bytecode::Star(dest));
    }

    pub fn emit_jump_if_false(&mut self, lbl: Label) {
        if let Some(idx) = self.dest_label(lbl) {
            self.code.push(Bytecode::JumpIfFalse(idx));
        } else {
            self.unresolved_jumps.push((self.pc(), lbl));
            self.code
                .push(Bytecode::JumpIfFalse(BytecodeIdx::invalid()));
        }
    }

    pub fn emit_jump(&mut self, lbl: Label) {
        if let Some(idx) = self.dest_label(lbl) {
            self.code.push(Bytecode::Jump(idx));
        } else {
            self.unresolved_jumps.push((self.pc(), lbl));
            self.code.push(Bytecode::Jump(BytecodeIdx::invalid()));
        }
    }

    pub fn emit_mod(&mut self, src: Register) {
        self.code.push(Bytecode::Mod(src));
    }

    pub fn emit_mul(&mut self, src: Register) {
        self.code.push(Bytecode::Mul(src));
    }

    pub fn emit_neg(&mut self) {
        self.code.push(Bytecode::Neg);
    }

    pub fn emit_shift_left(&mut self, src: Register) {
        self.code.push(Bytecode::ShiftLeft(src));
    }

    pub fn emit_shift_right(&mut self, src: Register) {
        self.code.push(Bytecode::ShiftRight(src));
    }

    pub fn emit_arith_shift_right(&mut self, src: Register) {
        self.code.push(Bytecode::ArithShiftRight(src));
    }

    pub fn emit_sub(&mut self, src: Register) {
        self.code.push(Bytecode::Sub(src));
    }

    pub fn emit_ret(&mut self) {
        self.code.push(Bytecode::Return);
    }

    pub fn emit_ret_void(&mut self) {
        self.code.push(Bytecode::ReturnVoid);
    }

    pub fn emit_test_eq(&mut self, src: Register) {
        self.code.push(Bytecode::TestEqual(src));
    }

    pub fn emit_test_ne(&mut self, src: Register) {
        self.code.push(Bytecode::TestNotEqual(src));
    }

    pub fn emit_test_gt(&mut self, src: Register) {
        self.code.push(Bytecode::TestGreatherThan(src));
    }

    pub fn emit_test_ge(&mut self, src: Register) {
        self.code.push(Bytecode::TestGreatherThanOrEqual(src));
    }

    pub fn emit_test_lt(&mut self, src: Register) {
        self.code.push(Bytecode::TestLessThan(src));
    }

    pub fn emit_test_le(&mut self, src: Register) {
        self.code.push(Bytecode::TestLessThanOrEqual(src));
    }

    pub fn generate(mut self) -> BytecodeFunction {
        self.resolve_forward_jumps();

        BytecodeFunction {
            code: self.code,
            registers: self.registers,
        }
    }

    fn resolve_forward_jumps(&mut self) {
        let unresolved_jumps = mem::replace(&mut self.unresolved_jumps, Vec::new());

        for (idx, lbl) in unresolved_jumps {
            let lbl_dest = self.dest_label(lbl).expect("label unresolved");
            let op = &mut self.code[idx.0];

            match op {
                Bytecode::JumpIfFalse(ref mut dest) => {
                    assert!(dest.is_invalid());
                    *dest = lbl_dest;
                }

                Bytecode::Jump(ref mut dest) => {
                    assert!(dest.is_invalid());
                    *dest = lbl_dest;
                }

                _ => unreachable!(),
            }
        }
    }
}

pub struct BytecodeFunction {
    code: Vec<Bytecode>,
    registers: Vec<BytecodeType>,
}

impl BytecodeFunction {
    pub fn code(&self) -> &[Bytecode] {
        &self.code
    }

    pub fn dump(&self) {
        let mut btidx = 0;
        for btcode in self.code.iter() {
            match btcode {
                Bytecode::Add(Register(register)) => println!("{}: Add {}", btidx, register),
                Bytecode::BitwiseAnd(Register(register)) => {
                    println!("{}: BitwiseAnd {}", btidx, register)
                }
                Bytecode::BitwiseOr(Register(register)) => {
                    println!("{}: BitwiseOr {}", btidx, register)
                }
                Bytecode::BitwiseXor(Register(register)) => {
                    println!("{}: BitwiseXor {}", btidx, register)
                }
                Bytecode::Div(Register(register)) => println!("{}: Div {}", btidx, register),
                Bytecode::Ldar(Register(register)) => println!("{}: Ldar {}", btidx, register),
                Bytecode::LdaInt(value) => println!("{}: LdaInt {}", btidx, value),
                Bytecode::LdaZero => println!("{}: LdaZero", btidx),
                Bytecode::LdaTrue => println!("{}: LdaTrue", btidx),
                Bytecode::LdaFalse => println!("{}: LdaFalse", btidx),
                Bytecode::LogicalNot => println!("{}: LogicalNot", btidx),
                Bytecode::Star(Register(register)) => println!("{}: Star {}", btidx, register),
                Bytecode::JumpIfFalse(dest) => println!("{}: JumpIfFalse bc#{}", btidx, dest),
                Bytecode::Jump(dest) => println!("{}: Jump bc#{}", btidx, dest),
                Bytecode::Mod(Register(register)) => println!("{}: Mod {}", btidx, register),
                Bytecode::Mul(Register(register)) => println!("{}: Mul {}", btidx, register),
                Bytecode::Neg => println!("{}: Neg", btidx),
                Bytecode::ShiftLeft(Register(register)) => {
                    println!("{}: ShiftLeft {}", btidx, register)
                }
                Bytecode::ShiftRight(Register(register)) => {
                    println!("{}: ShiftRight {}", btidx, register)
                }
                Bytecode::ArithShiftRight(Register(register)) => {
                    println!("{}: ArithShiftRight {}", btidx, register)
                }
                Bytecode::Sub(Register(register)) => println!("{}: Sub {}", btidx, register),
                Bytecode::Return => println!("{}: Return", btidx),
                Bytecode::ReturnVoid => println!("{}: ReturnVoid", btidx),
                Bytecode::TestEqual(Register(register)) => {
                    println!("{}: TestEqual {}", btidx, register)
                }
                Bytecode::TestGreatherThan(Register(register)) => {
                    println!("{}: TestGreaterThan {}", btidx, register)
                }
                Bytecode::TestGreatherThanOrEqual(Register(register)) => {
                    println!("{}: TestGreatherThanOrEqual {}", btidx, register)
                }
                Bytecode::TestLessThan(Register(register)) => {
                    println!("{}: TestLessThan {}", btidx, register)
                }
                Bytecode::TestLessThanOrEqual(Register(register)) => {
                    println!("{}: TestLessThanOrEqual {}", btidx, register)
                }
                Bytecode::TestNotEqual(Register(register)) => {
                    println!("{}: TestNotEqual {}", btidx, register)
                }
            }
            btidx = btidx + 1;
        }
    }
}
