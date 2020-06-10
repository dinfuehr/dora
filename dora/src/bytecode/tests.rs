use crate::bytecode::{
    read, BytecodeOpcode, BytecodeVisitor, BytecodeWriter, ConstPoolIdx, Register,
};

#[test]
fn test_ret() {
    struct TestVisitor {
        found: bool,
    }
    impl BytecodeVisitor for TestVisitor {
        fn visit_ret_void(&mut self) {
            self.found = true;
        }
    }
    let mut writer = BytecodeWriter::new();
    writer.emit_ret_void();
    let fct = writer.generate();
    assert_eq!(fct.code(), &[BytecodeOpcode::RetVoid as u8]);
    let mut visitor = TestVisitor { found: false };
    read(fct.code(), &mut visitor);
    assert!(visitor.found);
}

#[test]
fn test_ret_int() {
    struct TestVisitor {
        found: bool,
    }
    impl BytecodeVisitor for TestVisitor {
        fn visit_ret_int32(&mut self, reg: Register) {
            assert_eq!(Register(17), reg);
            self.found = true;
        }
    }
    let mut writer = BytecodeWriter::new();
    writer.emit_ret_int32(Register(17));
    let fct = writer.generate();
    assert_eq!(fct.code(), &[BytecodeOpcode::RetInt32 as u8, 17]);
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
        fn visit_ret_int32(&mut self, reg: Register) {
            assert_eq!(Register(256), reg);
            self.found = true;
        }
    }
    let mut writer = BytecodeWriter::new();
    writer.emit_ret_int32(Register(256));
    let fct = writer.generate();
    assert_eq!(
        fct.code(),
        &[
            BytecodeOpcode::Wide as u8,
            BytecodeOpcode::RetInt32 as u8,
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
        fn visit_mov_ptr(&mut self, dest: Register, src: Register) {
            assert_eq!(Register(0), dest);
            assert_eq!(Register(1), src);
            self.found = true;
        }
    }
    let mut writer = BytecodeWriter::new();
    writer.emit_mov_ptr(Register(0), Register(1));
    let fct = writer.generate();
    assert_eq!(fct.code(), &[BytecodeOpcode::MovPtr as u8, 0, 1]);
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
        fn visit_mov_ptr(&mut self, dest: Register, src: Register) {
            assert_eq!(Register(256), dest);
            assert_eq!(Register(255), src);
            self.found = true;
        }
    }
    let mut writer = BytecodeWriter::new();
    writer.emit_mov_ptr(Register(256), Register(255));
    let fct = writer.generate();
    assert_eq!(
        fct.code(),
        &[
            BytecodeOpcode::Wide as u8,
            BytecodeOpcode::MovPtr as u8,
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
    assert_eq!(fct.code(), &[BytecodeOpcode::ConstUInt8 as u8, 255, 255]);
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
            BytecodeOpcode::Wide as u8,
            BytecodeOpcode::ConstUInt8 as u8,
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
    assert_eq!(fct.code(), &[BytecodeOpcode::ConstString as u8, 7, 0]);
    assert_eq!(fct.const_pool(ConstPoolIdx(0)).to_string(), Some("foo"));
    let mut visitor = TestVisitor { found: false };
    read(fct.code(), &mut visitor);
    assert!(visitor.found);
}

#[test]
fn test_sub_float() {
    struct TestVisitor {
        found: bool,
    }
    impl BytecodeVisitor for TestVisitor {
        fn visit_sub_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
            assert_eq!(Register(2), dest);
            assert_eq!(Register(9), lhs);
            assert_eq!(Register(255), rhs);
            self.found = true;
        }
    }
    let mut writer = BytecodeWriter::new();
    writer.emit_sub_float32(Register(2), Register(9), Register(255));
    let fct = writer.generate();
    assert_eq!(fct.code(), &[BytecodeOpcode::SubFloat32 as u8, 2, 9, 255]);
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
    assert_eq!(fct.code(), &[BytecodeOpcode::JumpLoop as u8, 0]);
    let mut visitor = TestVisitor { found: false };
    read(fct.code(), &mut visitor);
    assert!(visitor.found);
}

#[test]
fn test_jump_back_one_inst() {
    struct TestVisitor {
        found: u32,
    }
    impl BytecodeVisitor for TestVisitor {
        fn visit_jump_loop(&mut self, _offset: u32) {
            self.found += 1;
        }

        fn visit_ret_void(&mut self) {
            self.found += 1;
        }
    }
    let mut writer = BytecodeWriter::new();
    let label = writer.define_label();
    writer.emit_ret_void();
    writer.emit_jump_loop(label);
    let fct = writer.generate();
    assert_eq!(
        fct.code(),
        &[
            BytecodeOpcode::RetVoid as u8,
            BytecodeOpcode::JumpLoop as u8,
            1
        ]
    );
    let mut visitor = TestVisitor { found: 0 };
    read(fct.code(), &mut visitor);
    assert_eq!(visitor.found, 2);
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
    assert_eq!(fct.code(), &[BytecodeOpcode::Jump as u8, 2]);
    let mut visitor = TestVisitor { found: false };
    read(fct.code(), &mut visitor);
    assert!(visitor.found);
}

#[test]
fn test_jump_far() {
    struct TestVisitor {
        found: u32,
    }
    impl BytecodeVisitor for TestVisitor {
        fn visit_jump_const(&mut self, idx: ConstPoolIdx) {
            assert_eq!(idx, ConstPoolIdx(0));
            self.found += 1;
        }

        fn visit_ret_void(&mut self) {
            self.found += 1;
        }
    }
    let mut writer = BytecodeWriter::new();
    let label = writer.create_label();
    writer.emit_jump(label);
    for _ in 0..254 {
        writer.emit_ret_void();
    }
    writer.bind_label(label);
    let fct = writer.generate();
    assert_eq!(
        &fct.code()[..3],
        &[
            BytecodeOpcode::JumpConst as u8,
            0,
            BytecodeOpcode::RetVoid as u8
        ]
    );
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0))
            .to_int32()
            .expect("int const expected"),
        256
    );
    let mut visitor = TestVisitor { found: 0 };
    read(fct.code(), &mut visitor);
    assert_eq!(visitor.found, 255);
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
    assert_eq!(fct.code(), &[BytecodeOpcode::JumpIfTrue as u8, 7, 3]);
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
            BytecodeOpcode::Wide as u8,
            BytecodeOpcode::JumpIfFalse as u8,
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

#[test]
fn test_cond_jump_far() {
    struct TestVisitor {
        found: u32,
    }
    impl BytecodeVisitor for TestVisitor {
        fn visit_jump_if_true_const(&mut self, opnd: Register, idx: ConstPoolIdx) {
            assert_eq!(opnd, Register(7));
            assert_eq!(idx, ConstPoolIdx(0));
            self.found += 1;
        }

        fn visit_ret_void(&mut self) {
            self.found += 1;
        }
    }
    let mut writer = BytecodeWriter::new();
    let label = writer.create_label();
    writer.emit_jump_if_true(Register(7), label);
    for _ in 0..253 {
        writer.emit_ret_void();
    }
    writer.bind_label(label);
    let fct = writer.generate();
    assert_eq!(
        &fct.code()[..4],
        &[
            BytecodeOpcode::JumpIfTrueConst as u8,
            7,
            0,
            BytecodeOpcode::RetVoid as u8
        ]
    );
    let mut visitor = TestVisitor { found: 0 };
    read(fct.code(), &mut visitor);
    assert_eq!(visitor.found, 254);
}
