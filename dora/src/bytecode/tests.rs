use crate::bytecode::{
    read, BytecodeOpcode, BytecodeVisitor, BytecodeWriter, ConstPoolIdx, Register,
};

#[test]
fn test_ret() {
    struct TestVisitor {
        found: bool,
    }
    impl BytecodeVisitor for TestVisitor {
        fn visit_ret(&mut self, _opnd: Register) {
            self.found = true;
        }
    }
    let mut writer = BytecodeWriter::new();
    writer.emit_ret(Register(0));
    let fct = writer.generate();
    assert_eq!(fct.code(), &[BytecodeOpcode::Ret.to_u8(), 0]);
    let mut visitor = TestVisitor { found: false };
    read(fct.code(), &mut visitor);
    assert!(visitor.found);
}

#[test]
fn test_ret_wide() {
    struct TestVisitor {
        found: bool,
    }
    impl BytecodeVisitor for TestVisitor {
        fn visit_ret(&mut self, reg: Register) {
            assert_eq!(Register(256), reg);
            self.found = true;
        }
    }
    let mut writer = BytecodeWriter::new();
    writer.emit_ret(Register(256));
    let fct = writer.generate();
    assert_eq!(
        fct.code(),
        &[
            BytecodeOpcode::Wide.to_u8(),
            BytecodeOpcode::Ret.to_u8(),
            0,
            1,
            0,
            0
        ]
    );
    let mut visitor = TestVisitor { found: false };
    read(fct.code(), &mut visitor);
    assert!(visitor.found);
}

#[test]
fn test_move() {
    struct TestVisitor {
        found: bool,
    }
    impl BytecodeVisitor for TestVisitor {
        fn visit_mov(&mut self, dest: Register, src: Register) {
            assert_eq!(Register(0), dest);
            assert_eq!(Register(1), src);
            self.found = true;
        }
    }
    let mut writer = BytecodeWriter::new();
    writer.emit_mov(Register(0), Register(1));
    let fct = writer.generate();
    assert_eq!(fct.code(), &[BytecodeOpcode::Mov.to_u8(), 0, 1]);
    let mut visitor = TestVisitor { found: false };
    read(fct.code(), &mut visitor);
    assert!(visitor.found);
}

#[test]
fn test_move_wide() {
    struct TestVisitor {
        found: bool,
    }
    impl BytecodeVisitor for TestVisitor {
        fn visit_mov(&mut self, dest: Register, src: Register) {
            assert_eq!(Register(256), dest);
            assert_eq!(Register(255), src);
            self.found = true;
        }
    }
    let mut writer = BytecodeWriter::new();
    writer.emit_mov(Register(256), Register(255));
    let fct = writer.generate();
    assert_eq!(
        fct.code(),
        &[
            BytecodeOpcode::Wide.to_u8(),
            BytecodeOpcode::Mov.to_u8(),
            0,
            1,
            0,
            0,
            255,
            0,
            0,
            0
        ]
    );
    let mut visitor = TestVisitor { found: false };
    read(fct.code(), &mut visitor);
    assert!(visitor.found);
}

#[test]
fn test_const_byte() {
    struct TestVisitor {
        found: bool,
    }
    impl BytecodeVisitor for TestVisitor {
        fn visit_const_uint8(&mut self, dest: Register, value: u8) {
            assert_eq!(Register(255), dest);
            assert_eq!(255, value);
            self.found = true;
        }
    }
    let mut writer = BytecodeWriter::new();
    writer.emit_const_uint8(Register(255), 255);
    let fct = writer.generate();
    assert_eq!(fct.code(), &[BytecodeOpcode::ConstUInt8.to_u8(), 255, 255]);
    let mut visitor = TestVisitor { found: false };
    read(fct.code(), &mut visitor);
    assert!(visitor.found);
}

#[test]
fn test_const_byte_wide() {
    struct TestVisitor {
        found: bool,
    }
    impl BytecodeVisitor for TestVisitor {
        fn visit_const_uint8(&mut self, dest: Register, value: u8) {
            assert_eq!(Register(256), dest);
            assert_eq!(19, value);
            self.found = true;
        }
    }
    let mut writer = BytecodeWriter::new();
    writer.emit_const_uint8(Register(256), 19);
    let fct = writer.generate();
    assert_eq!(
        fct.code(),
        &[
            BytecodeOpcode::Wide.to_u8(),
            BytecodeOpcode::ConstUInt8.to_u8(),
            0,
            1,
            0,
            0,
            19
        ]
    );
    let mut visitor = TestVisitor { found: false };
    read(fct.code(), &mut visitor);
    assert!(visitor.found);
}

#[test]
fn test_const_string() {
    struct TestVisitor {
        found: bool,
    }
    impl BytecodeVisitor for TestVisitor {
        fn visit_const_string(&mut self, dest: Register, idx: ConstPoolIdx) {
            assert_eq!(Register(7), dest);
            assert_eq!(ConstPoolIdx(0), idx);
            self.found = true;
        }
    }
    let mut writer = BytecodeWriter::new();
    writer.emit_const_string(Register(7), "foo".into());
    let fct = writer.generate();
    assert_eq!(fct.code(), &[BytecodeOpcode::ConstString.to_u8(), 7, 0]);
    assert_eq!(fct.const_pool(ConstPoolIdx(0)).to_string(), Some("foo"));
    let mut visitor = TestVisitor { found: false };
    read(fct.code(), &mut visitor);
    assert!(visitor.found);
}

#[test]
fn test_jump_back() {
    struct TestVisitor {
        found: bool,
    }
    impl BytecodeVisitor for TestVisitor {
        fn visit_jump_loop(&mut self, offset: u32) {
            assert_eq!(offset, 0);
            self.found = true;
        }
    }
    let mut writer = BytecodeWriter::new();
    let label = writer.define_label();
    writer.emit_jump_loop(label);
    let fct = writer.generate();
    assert_eq!(fct.code(), &[BytecodeOpcode::JumpLoop.to_u8(), 0]);
    let mut visitor = TestVisitor { found: false };
    read(fct.code(), &mut visitor);
    assert!(visitor.found);
}

#[test]
fn test_jump() {
    struct TestVisitor {
        found: bool,
    }
    impl BytecodeVisitor for TestVisitor {
        fn visit_jump(&mut self, offset: u32) {
            assert_eq!(offset, 2);
            self.found = true;
        }
    }
    let mut writer = BytecodeWriter::new();
    let label = writer.create_label();
    writer.emit_jump(label);
    writer.bind_label(label);
    let fct = writer.generate();
    assert_eq!(fct.code(), &[BytecodeOpcode::Jump.to_u8(), 2]);
    let mut visitor = TestVisitor { found: false };
    read(fct.code(), &mut visitor);
    assert!(visitor.found);
}

#[test]
fn test_cond_jump() {
    struct TestVisitor {
        found: bool,
    }
    impl BytecodeVisitor for TestVisitor {
        fn visit_jump_if_true(&mut self, opnd: Register, offset: u32) {
            assert_eq!(opnd, Register(7));
            assert_eq!(offset, 3);
            self.found = true;
        }
    }
    let mut writer = BytecodeWriter::new();
    let label = writer.create_label();
    writer.emit_jump_if_true(Register(7), label);
    writer.bind_label(label);
    let fct = writer.generate();
    assert_eq!(fct.code(), &[BytecodeOpcode::JumpIfTrue.to_u8(), 7, 3]);
    let mut visitor = TestVisitor { found: false };
    read(fct.code(), &mut visitor);
    assert!(visitor.found);
}

#[test]
fn test_cond_jump_wide() {
    struct TestVisitor {
        found: bool,
    }
    impl BytecodeVisitor for TestVisitor {
        fn visit_jump_if_false(&mut self, opnd: Register, offset: u32) {
            assert_eq!(opnd, Register(256));
            assert_eq!(offset, 10);
            self.found = true;
        }
    }
    let mut writer = BytecodeWriter::new();
    let label = writer.create_label();
    writer.emit_jump_if_false(Register(256), label);
    writer.bind_label(label);
    let fct = writer.generate();
    assert_eq!(
        fct.code(),
        &[
            BytecodeOpcode::Wide.to_u8(),
            BytecodeOpcode::JumpIfFalse.to_u8(),
            0,
            1,
            0,
            0,
            10,
            0,
            0,
            0
        ]
    );
    let mut visitor = TestVisitor { found: false };
    read(fct.code(), &mut visitor);
    assert!(visitor.found);
}
