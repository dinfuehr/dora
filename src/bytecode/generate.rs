use std::convert::From;
use std::fmt;
use std::mem;

use bytecode::opcode::Bytecode;
use ty::BuiltinType;

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Register(pub usize);

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "r{}", self.0)
    }
}

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
    Char,
    Int,
    Long,
    Float,
    Double,
    Ptr,
}

impl From<BuiltinType> for BytecodeType {
    fn from(ty: BuiltinType) -> Self {
        match ty {
            BuiltinType::Bool => BytecodeType::Bool,
            BuiltinType::Byte => BytecodeType::Byte,
            BuiltinType::Char => BytecodeType::Char,
            BuiltinType::Int => BytecodeType::Int,
            BuiltinType::Long => BytecodeType::Long,
            BuiltinType::Float => BytecodeType::Float,
            BuiltinType::Double => BytecodeType::Double,
            BuiltinType::Class(_, _) => BytecodeType::Ptr,
            _ => panic!("BuiltinType cannot converted to BytecodeType"),
        }
    }
}

pub struct BytecodeGenerator {
    code: Vec<Bytecode>,
    labels: Vec<Option<BytecodeIdx>>,
    unresolved_jumps: Vec<(BytecodeIdx, Label)>,
    registers: Vec<BytecodeType>,
}

impl BytecodeGenerator {
    pub fn new() -> BytecodeGenerator {
        BytecodeGenerator {
            code: Vec::new(),
            labels: Vec::new(),
            unresolved_jumps: Vec::new(),
            registers: Vec::new(),
        }
    }

    pub fn add_register(&mut self, ty: BytecodeType) -> Register {
        self.registers.push(ty);
        Register(self.registers.len() - 1)
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

    pub fn emit_add_int(&mut self, src: Register) {
        self.code.push(Bytecode::AddInt(src));
    }

    pub fn emit_add_long(&mut self, src: Register) {
        self.code.push(Bytecode::AddLong(src));
    }

    pub fn emit_add_float(&mut self, src: Register) {
        self.code.push(Bytecode::AddFloat(src));
    }

    pub fn emit_add_double(&mut self, src: Register) {
        self.code.push(Bytecode::AddDouble(src));
    }

    pub fn emit_and_int(&mut self, src: Register) {
        self.code.push(Bytecode::AndInt(src));
    }

    pub fn emit_or_int(&mut self, src: Register) {
        self.code.push(Bytecode::OrInt(src));
    }

    pub fn emit_xor_int(&mut self, src: Register) {
        self.code.push(Bytecode::XorInt(src));
    }

    pub fn emit_div_int(&mut self, src: Register) {
        self.code.push(Bytecode::DivInt(src));
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

    pub fn emit_not_bool(&mut self) {
        self.code.push(Bytecode::NotBool);
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

    pub fn emit_jump_if_true(&mut self, lbl: Label) {
        if let Some(idx) = self.dest_label(lbl) {
            self.code.push(Bytecode::JumpIfTrue(idx));
        } else {
            self.unresolved_jumps.push((self.pc(), lbl));
            self.code.push(Bytecode::JumpIfTrue(BytecodeIdx::invalid()));
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

    pub fn emit_mod_int(&mut self, src: Register) {
        self.code.push(Bytecode::ModInt(src));
    }

    pub fn emit_mul_int(&mut self, src: Register) {
        self.code.push(Bytecode::MulInt(src));
    }

    pub fn emit_neg_int(&mut self) {
        self.code.push(Bytecode::NegInt);
    }

    pub fn emit_shl_int(&mut self, src: Register) {
        self.code.push(Bytecode::ShlInt(src));
    }

    pub fn emit_shr_int(&mut self, src: Register) {
        self.code.push(Bytecode::ShrInt(src));
    }

    pub fn emit_sar_int(&mut self, src: Register) {
        self.code.push(Bytecode::SarInt(src));
    }

    pub fn emit_sub_int(&mut self, src: Register) {
        self.code.push(Bytecode::SubInt(src));
    }

    pub fn emit_ret(&mut self) {
        self.code.push(Bytecode::Ret);
    }

    pub fn emit_ret_void(&mut self) {
        self.code.push(Bytecode::RetVoid);
    }

    pub fn emit_test_eq_int(&mut self, src: Register) {
        self.code.push(Bytecode::TestEqInt(src));
    }

    pub fn emit_test_eq_ptr(&mut self, src: Register) {
        self.code.push(Bytecode::TestEqPtr(src));
    }

    pub fn emit_test_ne_int(&mut self, src: Register) {
        self.code.push(Bytecode::TestNeInt(src));
    }

    pub fn emit_test_ne_ptr(&mut self, src: Register) {
        self.code.push(Bytecode::TestNePtr(src));
    }

    pub fn emit_test_gt_int(&mut self, src: Register) {
        self.code.push(Bytecode::TestGtInt(src));
    }

    pub fn emit_test_ge_int(&mut self, src: Register) {
        self.code.push(Bytecode::TestGeInt(src));
    }

    pub fn emit_test_lt_int(&mut self, src: Register) {
        self.code.push(Bytecode::TestLtInt(src));
    }

    pub fn emit_test_le_int(&mut self, src: Register) {
        self.code.push(Bytecode::TestLeInt(src));
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

                Bytecode::JumpIfTrue(ref mut dest) => {
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
                Bytecode::AddInt(register) => println!("{}: AddInt {}", btidx, register),
                Bytecode::AddLong(register) => println!("{}: AddLong {}", btidx, register),
                Bytecode::AddFloat(register) => println!("{}: AddFloat {}", btidx, register),
                Bytecode::AddDouble(register) => println!("{}: AddDouble {}", btidx, register),
                Bytecode::AndInt(register) => println!("{}: AndInt {}", btidx, register),
                Bytecode::OrInt(register) => println!("{}: OrInt {}", btidx, register),
                Bytecode::XorInt(register) => println!("{}: XorInt {}", btidx, register),
                Bytecode::DivInt(register) => println!("{}: Div {}", btidx, register),
                Bytecode::Ldar(register) => println!("{}: Ldar {}", btidx, register),
                Bytecode::LdaInt(value) => println!("{}: LdaInt {}", btidx, value),
                Bytecode::LdaZero => println!("{}: LdaZero", btidx),
                Bytecode::LdaTrue => println!("{}: LdaTrue", btidx),
                Bytecode::LdaFalse => println!("{}: LdaFalse", btidx),
                Bytecode::NotBool => println!("{}: LogicalNot", btidx),
                Bytecode::Star(register) => println!("{}: Star {}", btidx, register),
                Bytecode::JumpIfFalse(dest) => println!("{}: JumpIfFalse bc#{}", btidx, dest),
                Bytecode::JumpIfTrue(dest) => println!("{}: JumpIfTrue bc#{}", btidx, dest),
                Bytecode::Jump(dest) => println!("{}: Jump bc#{}", btidx, dest),
                Bytecode::ModInt(register) => println!("{}: Mod {}", btidx, register),
                Bytecode::MulInt(register) => println!("{}: Mul {}", btidx, register),
                Bytecode::NegInt => println!("{}: Neg", btidx),
                Bytecode::ShlInt(register) => println!("{}: ShiftLeft {}", btidx, register),
                Bytecode::ShrInt(register) => println!("{}: ShiftRight {}", btidx, register),
                Bytecode::SarInt(register) => println!("{}: ArithShiftRight {}", btidx, register),
                Bytecode::SubInt(register) => println!("{}: Sub {}", btidx, register),
                Bytecode::Ret => println!("{}: Return", btidx),
                Bytecode::RetVoid => println!("{}: ReturnVoid", btidx),
                Bytecode::TestEqInt(register) => println!("{}: TestEqInt {}", btidx, register),
                Bytecode::TestEqPtr(register) => println!("{}: TestEqPtr {}", btidx, register),
                Bytecode::TestNeInt(register) => println!("{}: TestNeInt {}", btidx, register),
                Bytecode::TestNePtr(register) => println!("{}: TestNePtr {}", btidx, register),
                Bytecode::TestGtInt(register) => println!("{}: TestGt {}", btidx, register),
                Bytecode::TestGeInt(register) => println!("{}: TestGe {}", btidx, register),
                Bytecode::TestLtInt(register) => println!("{}: TestLt {}", btidx, register),
                Bytecode::TestLeInt(register) => println!("{}: TestLe {}", btidx, register),
            }
            btidx = btidx + 1;
        }
    }
}
