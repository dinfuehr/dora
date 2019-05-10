use std::fmt;
use std::mem;

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

#[derive(PartialEq, Debug)]
pub enum Bytecode {
    Add(Register),
    BitwiseAnd(Register),
    BitwiseOr(Register),
    BitwiseXor(Register),
    Div(Register),
    Ldar(Register),
    LdaInt(u64),
    LdaZero,
    LogicalNot,
    Star(Register),
    JumpIfFalse(Label),
    Jump(Label),
    JumpIfFalseBytecodeIdx(BytecodeIdx),
    JumpBytecodeIdx(BytecodeIdx),
    Mod(Register),
    Mul(Register),
    Neg,
    ShiftLeft(Register),
    ShiftRight(Register),
    ArithShiftRight(Register),
    Sub(Register),
    Return,
    ReturnVoid,
    TestEqual(Register),
    TestGreatherThan(Register),
    TestGreatherThanOrEqual(Register),
    TestLessThan(Register),
    TestLessThanOrEqual(Register),
    TestNotEqual(Register),
}

pub struct BytecodeGenerator {
    code: Vec<Bytecode>,
    labels: Vec<Option<BytecodeIdx>>,
    unresolved: Vec<(BytecodeIdx, Label)>,
    next_register: usize,
}

impl BytecodeGenerator {
    pub fn new() -> BytecodeGenerator {
        BytecodeGenerator {
            code: Vec::new(),
            labels: Vec::new(),
            unresolved: Vec::new(),
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
        self.code.push(Bytecode::BitwiseAnd(src));
    }

    pub fn emit_bitwise_xor(&mut self, src: Register) {
        self.code.push(Bytecode::BitwiseAnd(src));
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

    pub fn emit_logical_not(&mut self) {
        self.code.push(Bytecode::LogicalNot);
    }

    pub fn emit_star(&mut self, dest: Register) {
        self.code.push(Bytecode::Star(dest));
    }

    pub fn emit_jump_if_false(&mut self, lbl: Label) {
        if let Some(idx) = self.dest_label(lbl) {
            self.code.push(Bytecode::JumpIfFalseBytecodeIdx(idx));
        } else {
            self.unresolved.push((self.pc(), lbl));
            self.code.push(Bytecode::JumpIfFalseBytecodeIdx(BytecodeIdx::invalid()));
        }
    }

    pub fn emit_jump(&mut self, lbl: Label) {
        if let Some(idx) = self.dest_label(lbl) {
            self.code.push(Bytecode::JumpBytecodeIdx(idx));
        } else {
            self.unresolved.push((self.pc(), lbl));
            self.code.push(Bytecode::JumpBytecodeIdx(BytecodeIdx::invalid()));
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

    pub fn emit_ret_nop(&mut self) {
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

    fn resolve_forward_jumps(&mut self) {
        let unresolved_lbls = mem::replace(&mut self.unresolved, Vec::new());

        for (idx, lbl) in unresolved_lbls {
            let lbl_dest = self.dest_label(lbl).expect("label unresolved");
            let op = &mut self.code[idx.0];

            match op {
                Bytecode::JumpIfFalseBytecodeIdx(ref mut dest) => {
                    assert!(dest.is_invalid());
                    *dest = lbl_dest;
                }

                Bytecode::JumpBytecodeIdx(ref mut dest) => {
                    assert!(dest.is_invalid());
                    *dest = lbl_dest;
                }

                _ => unreachable!(),
            }
        }
    }
}
