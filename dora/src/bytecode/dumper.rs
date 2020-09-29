use std::io;

use crate::bytecode::{
    read, BytecodeFunction, BytecodeOffset, BytecodeVisitor, ConstPoolEntry, ConstPoolIdx, Register,
};
use crate::ty::BuiltinType;
use crate::vm::{Fct, GlobalId, TupleId, VM};

pub fn dump<'ast>(vm: &VM<'ast>, fct: Option<&Fct<'ast>>, bc: &BytecodeFunction) {
    let mut stdout = io::stdout();
    if let Some(fct) = fct {
        println!("{}", fct.full_name(vm));
    }
    let mut visitor = BytecodeDumper {
        bc,
        pos: BytecodeOffset(0),
        w: &mut stdout,
        vm,
    };
    read(bc.code(), &mut visitor);

    let align = "   ";

    println!();
    println!("  Registers:");

    for (idx, ty) in bc.registers().iter().enumerate() {
        println!("{}{} => {:?}", align, idx, ty);
    }

    println!();
    println!("  Constants:");

    for (idx, entry) in bc.const_pool_entries().iter().enumerate() {
        match entry {
            ConstPoolEntry::String(ref value) => {
                println!("{}{} => String \"{}\"", align, idx, value)
            }
            ConstPoolEntry::Int32(ref value) => println!("{}{} => Int32 {}", align, idx, value),
            ConstPoolEntry::Int64(ref value) => println!("{}{} => Int64 {}", align, idx, value),
            ConstPoolEntry::Float32(ref value) => println!("{}{} => Float32 {}", align, idx, value),
            ConstPoolEntry::Float64(ref value) => println!("{}{} => Float64 {}", align, idx, value),
            ConstPoolEntry::Char(ref value) => println!("{}{} => Char {}", align, idx, value),
            ConstPoolEntry::Class(cls_id, type_params) => {
                let cls = vm.classes.idx(*cls_id);
                let cls = cls.read();
                println!(
                    "{}{} => Class {}",
                    align,
                    idx,
                    cls.name_with_params(vm, type_params)
                )
            }
            ConstPoolEntry::Enum(cls_id, type_params) => {
                let xenum = &vm.enums[*cls_id];
                let xenum = xenum.read();
                println!(
                    "{}{} => Enum {}",
                    align,
                    idx,
                    xenum.name_with_params(vm, type_params)
                )
            }
            ConstPoolEntry::Field(cls_id, type_params, field_id) => {
                let cls = vm.classes.idx(*cls_id);
                let cls = cls.read();
                let field = &cls.fields[field_id.idx()];
                let fname = vm.interner.str(field.name);
                println!(
                    "{}{} => Field {}.{}",
                    align,
                    idx,
                    cls.name_with_params(vm, type_params),
                    fname,
                )
            }
            ConstPoolEntry::Fct(fct_id, type_params) => {
                let fct = vm.fcts.idx(*fct_id);
                let fct = fct.read();

                if type_params.len() > 0 {
                    let type_params = type_params
                        .iter()
                        .map(|n| n.name(vm))
                        .collect::<Vec<_>>()
                        .join(", ");
                    println!(
                        "{}{} => Fct {} with [{}]",
                        align,
                        idx,
                        fct.full_name(vm),
                        type_params
                    );
                } else {
                    println!("{}{} => Fct {}", align, idx, fct.full_name(vm));
                }
            }
            ConstPoolEntry::Generic(id, fct_id, type_params) => {
                let fct = vm.fcts.idx(*fct_id);
                let fct = fct.read();

                if type_params.len() > 0 {
                    let type_params = type_params
                        .iter()
                        .map(|n| n.name(vm))
                        .collect::<Vec<_>>()
                        .join(", ");
                    println!(
                        "{}{} => TypeParam({}) Method {} with [{}]",
                        align,
                        idx,
                        id.to_usize(),
                        fct.full_name(vm),
                        type_params
                    );
                } else {
                    println!(
                        "{}{} => TypeParam({}) Method {}",
                        align,
                        idx,
                        id.to_usize(),
                        fct.full_name(vm)
                    );
                }
            }
        }
    }

    println!();
    println!("  Positions:");
    for (bc_offset, pos) in bc.positions().iter() {
        println!("{}{} => {}", align, bc_offset, pos);
    }
    println!();
}

struct BytecodeDumper<'a, 'ast: 'a> {
    bc: &'a BytecodeFunction,
    pos: BytecodeOffset,
    w: &'a mut dyn io::Write,
    vm: &'a VM<'ast>,
}

impl<'a, 'ast> BytecodeDumper<'a, 'ast> {
    fn emit_inst(&mut self, name: &str) {
        self.emit_start(name);
        writeln!(self.w, "").expect("write! failed");
    }

    fn emit_reg3(&mut self, name: &str, r1: Register, r2: Register, r3: Register) {
        self.emit_start(name);
        writeln!(self.w, " {}, {}, {}", r1, r2, r3).expect("write! failed");
    }

    fn emit_reg3_idx(
        &mut self,
        name: &str,
        r1: Register,
        r2: Register,
        r3: Register,
        idx: ConstPoolIdx,
    ) {
        self.emit_start(name);
        writeln!(
            self.w,
            " {}, {}, {}, ConstPoolIdx({})",
            r1,
            r2,
            r3,
            idx.to_usize()
        )
        .expect("write! failed");
    }

    fn emit_reg2(&mut self, name: &str, r1: Register, r2: Register) {
        self.emit_start(name);
        writeln!(self.w, " {}, {}", r1, r2).expect("write! failed");
    }

    fn emit_reg2_tuple(&mut self, name: &str, r1: Register, r2: Register, tuple_id: TupleId) {
        self.emit_start(name);
        writeln!(self.w, " {}, {}, {}", r1, r2, tuple_id.to_usize()).expect("write! failed");
    }

    fn emit_tuple_load(
        &mut self,
        name: &str,
        r1: Register,
        r2: Register,
        tuple_id: TupleId,
        idx: u32,
    ) {
        self.emit_start(name);
        writeln!(self.w, " {}, {}, {}, {}", r1, r2, tuple_id.to_usize(), idx)
            .expect("write! failed");
    }

    fn emit_reg2_cls(&mut self, name: &str, r1: Register, r2: Register, cls_idx: ConstPoolIdx) {
        self.emit_start(name);
        let (cls_id, type_params) = match self.bc.const_pool(cls_idx) {
            ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params.clone()),
            _ => unreachable!(),
        };
        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();
        let cname = cls.name_with_params(self.vm, &type_params);
        writeln!(
            self.w,
            " {}, {}, ConstPoolIdx({}) # {}",
            r1,
            r2,
            cls_idx.to_usize(),
            cname,
        )
        .expect("write! failed");
    }

    fn emit_reg1(&mut self, name: &str, r1: Register) {
        self.emit_start(name);
        writeln!(self.w, " {}", r1).expect("write! failed");
    }

    fn emit_reg1_cls(&mut self, name: &str, r1: Register, cls_idx: ConstPoolIdx) {
        self.emit_start(name);
        let (cls_id, type_params) = match self.bc.const_pool(cls_idx) {
            ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params.clone()),
            _ => unreachable!(),
        };
        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();
        let cname = cls.name_with_params(self.vm, &type_params);
        writeln!(
            self.w,
            " {}, ConstPoolIdx({}) # {}",
            r1,
            cls_idx.to_usize(),
            cname
        )
        .expect("write! failed");
    }

    fn emit_reg1_idx(&mut self, name: &str, r1: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        writeln!(self.w, " {}, @{}", r1, idx.to_usize()).expect("write! failed");
    }

    fn emit_idx(&mut self, name: &str, idx: ConstPoolIdx) {
        self.emit_start(name);
        writeln!(self.w, " @{}", idx.to_usize()).expect("write! failed");
    }

    fn emit_reg1_u32(&mut self, name: &str, r1: Register, value: u32) {
        self.emit_start(name);
        writeln!(self.w, " {}, 0x{:x}/{}", r1, value, value).expect("write! failed");
    }

    fn emit_cond_jump(&mut self, name: &str, opnd: Register, offset: i32) {
        self.emit_start(name);
        let bc_target = self.pos.to_u32() as i32 + offset;
        writeln!(self.w, " {}, {} # target {}", opnd, offset, bc_target).expect("write! failed");
    }

    fn emit_cond_jump_const(&mut self, name: &str, opnd: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        let offset = self.bc.const_pool(idx).to_int32().expect("int expected");
        let bc_target = self.pos.to_u32() as i32 + offset;
        writeln!(
            self.w,
            " {}, ConstPooldId({}) # offset {}, target {}",
            opnd,
            idx.to_usize(),
            offset,
            bc_target
        )
        .expect("write! failed");
    }

    fn emit_jump(&mut self, name: &str, offset: i32) {
        self.emit_start(name);
        let bc_target = self.pos.to_u32() as i32 + offset;
        writeln!(self.w, " {} # target {}", offset, bc_target).expect("write! failed");
    }

    fn emit_jump_const(&mut self, name: &str, idx: ConstPoolIdx) {
        self.emit_start(name);
        let offset = self.bc.const_pool(idx).to_int32().expect("int expected");
        let bc_target = self.pos.to_u32() as i32 + offset;
        writeln!(
            self.w,
            " ConstPoolId({}) # offset {}, target {}",
            idx.to_usize(),
            offset,
            bc_target
        )
        .expect("write! failed");
    }

    fn emit_field(&mut self, name: &str, r1: Register, r2: Register, field_idx: ConstPoolIdx) {
        self.emit_start(name);
        let (cls_id, type_params, field_id) = match self.bc.const_pool(field_idx) {
            ConstPoolEntry::Field(cls_id, type_params, field_id) => {
                (*cls_id, type_params, *field_id)
            }
            _ => unreachable!(),
        };
        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();
        let cname = cls.name_with_params(self.vm, type_params);

        let field = &cls.fields[field_id.idx()];
        let fname = self.vm.interner.str(field.name);
        writeln!(
            self.w,
            " {}, {}, ConstPoolIdx({}) # {}.{}",
            r1,
            r2,
            field_idx.to_usize(),
            cname,
            fname,
        )
        .expect("write! failed");
    }

    fn emit_global(&mut self, name: &str, r1: Register, gid: GlobalId) {
        self.emit_start(name);
        let glob = self.vm.globals.idx(gid);
        let glob = glob.read();
        let name = self.vm.interner.str(glob.name);
        writeln!(self.w, " {}, GlobalId({}) # {}", r1, gid.to_usize(), name)
            .expect("write! failed");
    }

    fn emit_fct_void(&mut self, name: &str, fid: ConstPoolIdx) {
        self.emit_start(name);
        writeln!(self.w, " ConstPoolIdx({})", fid.to_usize()).expect("write! failed");
    }

    fn emit_fct(&mut self, name: &str, r1: Register, fid: ConstPoolIdx) {
        self.emit_start(name);
        writeln!(self.w, " {}, ConstPoolIdx({})", r1, fid.to_usize()).expect("write! failed");
    }

    fn emit_new_object(&mut self, name: &str, r1: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        let (cls_id, type_params) = match self.bc.const_pool(idx) {
            ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params.clone()),
            _ => unreachable!(),
        };
        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();
        let cname = cls.name_with_params(self.vm, &type_params);
        writeln!(
            self.w,
            " {}, ConstPoolIdx({}) # {}",
            r1,
            idx.to_usize(),
            cname
        )
        .expect("write! failed");
    }

    fn emit_new_array(&mut self, name: &str, r1: Register, idx: ConstPoolIdx, length: Register) {
        self.emit_start(name);
        let (cls_id, type_params) = match self.bc.const_pool(idx) {
            ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params),
            _ => unreachable!(),
        };
        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();
        let cname = cls.name_with_params(self.vm, type_params);
        writeln!(
            self.w,
            " {}, ConstPoolIdx({}), {} # {}",
            r1,
            idx.to_usize(),
            length,
            cname,
        )
        .expect("write! failed");
    }

    fn emit_new_tuple(&mut self, name: &str, r1: Register, tuple_id: TupleId) {
        self.emit_start(name);
        let tuple_name = BuiltinType::Tuple(tuple_id).name(self.vm);
        writeln!(
            self.w,
            " {}, TupleId({}) # {}",
            r1,
            tuple_id.to_usize(),
            tuple_name
        )
        .expect("write! failed");
    }

    fn emit_start(&mut self, name: &str) {
        write!(self.w, "{:3}: {}", self.pos.to_usize(), name).expect("write! failed");
    }
}

impl<'a, 'ast> BytecodeVisitor for BytecodeDumper<'a, 'ast> {
    fn visit_instruction(&mut self, offset: BytecodeOffset) {
        self.pos = offset;
    }

    fn visit_add_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("AddInt32", dest, lhs, rhs);
    }
    fn visit_add_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("AddInt64", dest, lhs, rhs);
    }
    fn visit_add_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("AddFloat32", dest, lhs, rhs);
    }
    fn visit_add_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("AddFloat64", dest, lhs, rhs);
    }

    fn visit_sub_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("SubInt32", dest, lhs, rhs);
    }
    fn visit_sub_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("SubInt64", dest, lhs, rhs);
    }
    fn visit_sub_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("SubFloat32", dest, lhs, rhs);
    }
    fn visit_sub_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("SubFloat64", dest, lhs, rhs);
    }

    fn visit_neg_int32(&mut self, dest: Register, src: Register) {
        self.emit_reg2("NegInt32", dest, src);
    }
    fn visit_neg_int64(&mut self, dest: Register, src: Register) {
        self.emit_reg2("NegInt64", dest, src);
    }
    fn visit_neg_float32(&mut self, dest: Register, src: Register) {
        self.emit_reg2("NegFloat32", dest, src);
    }
    fn visit_neg_float64(&mut self, dest: Register, src: Register) {
        self.emit_reg2("NegFloat64", dest, src);
    }

    fn visit_mul_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("MulInt32", dest, lhs, rhs);
    }
    fn visit_mul_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("MulInt64", dest, lhs, rhs);
    }
    fn visit_mul_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("MulFloat32", dest, lhs, rhs);
    }
    fn visit_mul_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("MulFloat64", dest, lhs, rhs);
    }

    fn visit_div_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("DivInt32", dest, lhs, rhs);
    }
    fn visit_div_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("DivInt64", dest, lhs, rhs);
    }
    fn visit_div_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("DivFloat32", dest, lhs, rhs);
    }
    fn visit_div_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("DivFloat64", dest, lhs, rhs);
    }

    fn visit_mod_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("ModInt32", dest, lhs, rhs);
    }
    fn visit_mod_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("ModInt64", dest, lhs, rhs);
    }

    fn visit_and_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("AndInt32", dest, lhs, rhs);
    }
    fn visit_and_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("AndInt64", dest, lhs, rhs);
    }

    fn visit_or_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("OrInt32", dest, lhs, rhs);
    }
    fn visit_or_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("OrInt64", dest, lhs, rhs);
    }

    fn visit_xor_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("XorInt32", dest, lhs, rhs);
    }
    fn visit_xor_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("XorInt64", dest, lhs, rhs);
    }

    fn visit_not_bool(&mut self, dest: Register, src: Register) {
        self.emit_reg2("NotBool", dest, src);
    }
    fn visit_not_int32(&mut self, dest: Register, src: Register) {
        self.emit_reg2("NotInt32", dest, src);
    }
    fn visit_not_int64(&mut self, dest: Register, src: Register) {
        self.emit_reg2("NotInt64", dest, src);
    }

    fn visit_shl_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("ShlInt32", dest, lhs, rhs);
    }
    fn visit_shr_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("ShrInt32", dest, lhs, rhs);
    }
    fn visit_sar_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("SarInt32", dest, lhs, rhs);
    }

    fn visit_shl_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("ShlInt64", dest, lhs, rhs);
    }
    fn visit_shr_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("ShrInt64", dest, lhs, rhs);
    }
    fn visit_sar_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("SarInt64", dest, lhs, rhs);
    }

    fn visit_rol_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("RolInt32", dest, lhs, rhs);
    }
    fn visit_ror_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("RorInt32", dest, lhs, rhs);
    }

    fn visit_rol_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("RolInt64", dest, lhs, rhs);
    }
    fn visit_ror_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("RorInt64", dest, lhs, rhs);
    }

    fn visit_reinterpret_int32_as_float32(&mut self, dest: Register, src: Register) {
        self.emit_reg2("ReinterpretInt32AsFloat32", dest, src);
    }
    fn visit_reinterpret_float32_as_int32(&mut self, dest: Register, src: Register) {
        self.emit_reg2("ReinterpretFloat32AsInt32", dest, src);
    }
    fn visit_reinterpret_float64_as_int64(&mut self, dest: Register, src: Register) {
        self.emit_reg2("ReinterpretFloat64AsInt64", dest, src);
    }
    fn visit_reinterpret_int64_as_float64(&mut self, dest: Register, src: Register) {
        self.emit_reg2("ReinterpretInt64AsFloat64", dest, src);
    }

    fn visit_extend_byte_to_char(&mut self, dest: Register, src: Register) {
        self.emit_reg2("ExtendByteToChar", dest, src);
    }
    fn visit_extend_byte_to_int32(&mut self, dest: Register, src: Register) {
        self.emit_reg2("ExtendByteToInt32", dest, src);
    }
    fn visit_extend_byte_to_int64(&mut self, dest: Register, src: Register) {
        self.emit_reg2("ExtendByteToInt64", dest, src);
    }
    fn visit_extend_int32_to_int64(&mut self, dest: Register, src: Register) {
        self.emit_reg2("ExtendInt32ToInt64", dest, src);
    }
    fn visit_extend_char_to_int64(&mut self, dest: Register, src: Register) {
        self.emit_reg2("ExtendCharToInt64", dest, src);
    }
    fn visit_cast_char_to_int32(&mut self, dest: Register, src: Register) {
        self.emit_reg2("CastCharToInt32", dest, src);
    }
    fn visit_cast_int32_to_uint8(&mut self, dest: Register, src: Register) {
        self.emit_reg2("CastInt32ToUInt8", dest, src);
    }
    fn visit_cast_int32_to_char(&mut self, dest: Register, src: Register) {
        self.emit_reg2("CastInt32ToChar", dest, src);
    }
    fn visit_cast_int64_to_uint8(&mut self, dest: Register, src: Register) {
        self.emit_reg2("CastInt64ToUInt8", dest, src);
    }
    fn visit_cast_int64_to_char(&mut self, dest: Register, src: Register) {
        self.emit_reg2("CastInt64ToChar", dest, src);
    }
    fn visit_cast_int64_to_int32(&mut self, dest: Register, src: Register) {
        self.emit_reg2("CastInt64ToInt32", dest, src);
    }

    fn visit_convert_int32_to_float32(&mut self, dest: Register, src: Register) {
        self.emit_reg2("ConvertInt32ToFloat32", dest, src);
    }
    fn visit_convert_int32_to_float64(&mut self, dest: Register, src: Register) {
        self.emit_reg2("ConvertInt32ToFloat64", dest, src);
    }
    fn visit_convert_int64_to_float32(&mut self, dest: Register, src: Register) {
        self.emit_reg2("ConvertInt64ToFloat32", dest, src);
    }
    fn visit_convert_int64_to_float64(&mut self, dest: Register, src: Register) {
        self.emit_reg2("ConvertInt64ToFloat64", dest, src);
    }

    fn visit_instance_of(&mut self, dest: Register, src: Register, cls_id: ConstPoolIdx) {
        self.emit_reg2_cls("InstanceOf", dest, src, cls_id);
    }
    fn visit_checked_cast(&mut self, src: Register, cls_id: ConstPoolIdx) {
        self.emit_reg1_cls("CheckedCast", src, cls_id);
    }

    fn visit_mov_bool(&mut self, dest: Register, src: Register) {
        self.emit_reg2("MovBool", dest, src);
    }
    fn visit_mov_uint8(&mut self, dest: Register, src: Register) {
        self.emit_reg2("MovUInt8", dest, src);
    }
    fn visit_mov_char(&mut self, dest: Register, src: Register) {
        self.emit_reg2("MovChar", dest, src);
    }
    fn visit_mov_int32(&mut self, dest: Register, src: Register) {
        self.emit_reg2("MovInt32", dest, src);
    }
    fn visit_mov_int64(&mut self, dest: Register, src: Register) {
        self.emit_reg2("MovInt64", dest, src);
    }
    fn visit_mov_float32(&mut self, dest: Register, src: Register) {
        self.emit_reg2("MovFloat32", dest, src);
    }
    fn visit_mov_float64(&mut self, dest: Register, src: Register) {
        self.emit_reg2("MovFloat64", dest, src);
    }
    fn visit_mov_ptr(&mut self, dest: Register, src: Register) {
        self.emit_reg2("MovPtr", dest, src);
    }
    fn visit_mov_tuple(&mut self, dest: Register, src: Register, tuple_id: TupleId) {
        self.emit_reg2_tuple("MovTuple", dest, src, tuple_id);
    }
    fn visit_mov_generic(&mut self, dest: Register, src: Register) {
        self.emit_reg2("MovGeneric", dest, src);
    }

    fn visit_load_tuple_element(
        &mut self,
        dest: Register,
        src: Register,
        tuple_id: TupleId,
        element: u32,
    ) {
        self.emit_tuple_load("LoadTupleElement", dest, src, tuple_id, element);
    }

    fn visit_load_field(&mut self, dest: Register, obj: Register, field_idx: ConstPoolIdx) {
        self.emit_field("LoadField", dest, obj, field_idx);
    }

    fn visit_store_field(&mut self, src: Register, obj: Register, field_idx: ConstPoolIdx) {
        self.emit_field("StoreField", src, obj, field_idx);
    }

    fn visit_load_global(&mut self, dest: Register, glob: GlobalId) {
        self.emit_global("LoadGlobal", dest, glob);
    }

    fn visit_store_global(&mut self, src: Register, glob: GlobalId) {
        self.emit_global("StoreGlobal", src, glob);
    }

    fn visit_push_register(&mut self, src: Register) {
        self.emit_reg1("PushRegister", src)
    }

    fn visit_const_nil(&mut self, dest: Register) {
        self.emit_reg1("ConstNil", dest);
    }
    fn visit_const_true(&mut self, dest: Register) {
        self.emit_reg1("ConstTrue", dest);
    }
    fn visit_const_false(&mut self, dest: Register) {
        self.emit_reg1("ConstFalse", dest);
    }
    fn visit_const_zero_uint8(&mut self, dest: Register) {
        self.emit_reg1("ConstZeroUInt8", dest);
    }
    fn visit_const_zero_char(&mut self, dest: Register) {
        self.emit_reg1("ConstZeroChar", dest);
    }
    fn visit_const_zero_int32(&mut self, dest: Register) {
        self.emit_reg1("ConstZeroInt32", dest);
    }
    fn visit_const_zero_int64(&mut self, dest: Register) {
        self.emit_reg1("ConstZeroInt64", dest);
    }
    fn visit_const_zero_float32(&mut self, dest: Register) {
        self.emit_reg1("ConstZeroFloat32", dest);
    }
    fn visit_const_zero_float64(&mut self, dest: Register) {
        self.emit_reg1("ConstZeroFloat64", dest);
    }
    fn visit_const_char(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_start("ConstChar");
        let value = self.bc.const_pool(idx).to_char().expect("char expected");
        writeln!(
            self.w,
            " {}, @{} # '{}' 0x{:x}",
            dest,
            idx.to_usize(),
            value,
            value as u32
        )
        .expect("write! failed");
    }
    fn visit_const_uint8(&mut self, dest: Register, value: u8) {
        self.emit_start("ConstUInt8");
        writeln!(self.w, " {}, {}", dest, value).expect("write! failed");
    }
    fn visit_const_int32(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_start("ConstInt32");
        let value = self.bc.const_pool(idx).to_int32().expect("int32 expected");
        writeln!(self.w, " {}, @{} # {}", dest, idx.to_usize(), value).expect("write! failed");
    }
    fn visit_const_int64(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_start("ConstInt64");
        let value = self.bc.const_pool(idx).to_int64().expect("int64 expected");
        writeln!(self.w, " {}, @{} # {}", dest, idx.to_usize(), value).expect("write! failed");
    }
    fn visit_const_float32(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_start("ConstFloat32");
        let value = self
            .bc
            .const_pool(idx)
            .to_float32()
            .expect("float32 expected");
        writeln!(self.w, " {}, @{} # {}", dest, idx.to_usize(), value).expect("write! failed");
    }
    fn visit_const_float64(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_start("ConstFloat64");
        let value = self
            .bc
            .const_pool(idx)
            .to_float64()
            .expect("float64 expected");
        writeln!(self.w, " {}, @{} # {}", dest, idx.to_usize(), value).expect("write! failed");
    }
    fn visit_const_string(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_start("ConstString");
        let value = self
            .bc
            .const_pool(idx)
            .to_string()
            .expect("string expected");
        writeln!(self.w, " {}, @{} # \"{}\"", dest, idx.to_usize(), value).expect("write! failed");
    }

    fn visit_test_eq_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqPtr", dest, lhs, rhs);
    }
    fn visit_test_ne_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNePtr", dest, lhs, rhs);
    }

    fn visit_test_eq_generic(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqGeneric", dest, lhs, rhs);
    }
    fn visit_test_ne_generic(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNeGeneric", dest, lhs, rhs);
    }

    fn visit_test_eq_bool(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqBool", dest, lhs, rhs);
    }
    fn visit_test_ne_bool(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNeBool", dest, lhs, rhs);
    }

    fn visit_test_eq_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqUInt8", dest, lhs, rhs);
    }
    fn visit_test_ne_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNeUInt8", dest, lhs, rhs);
    }
    fn visit_test_gt_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGtUInt8", dest, lhs, rhs);
    }
    fn visit_test_ge_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGeUInt8", dest, lhs, rhs);
    }
    fn visit_test_lt_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLtUInt8", dest, lhs, rhs);
    }
    fn visit_test_le_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLeUInt8", dest, lhs, rhs);
    }

    fn visit_test_eq_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqChar", dest, lhs, rhs);
    }
    fn visit_test_ne_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNeChar", dest, lhs, rhs);
    }
    fn visit_test_gt_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGtChar", dest, lhs, rhs);
    }
    fn visit_test_ge_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGeChar", dest, lhs, rhs);
    }
    fn visit_test_lt_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLtChar", dest, lhs, rhs);
    }
    fn visit_test_le_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLeChar", dest, lhs, rhs);
    }

    fn visit_test_eq_enum(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqEnum", dest, lhs, rhs);
    }
    fn visit_test_ne_enum(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNeEnum", dest, lhs, rhs);
    }

    fn visit_test_eq_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqInt32", dest, lhs, rhs);
    }
    fn visit_test_ne_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNeInt32", dest, lhs, rhs);
    }
    fn visit_test_gt_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGtInt32", dest, lhs, rhs);
    }
    fn visit_test_ge_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGeInt32", dest, lhs, rhs);
    }
    fn visit_test_lt_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLtInt32", dest, lhs, rhs);
    }
    fn visit_test_le_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLeInt32", dest, lhs, rhs);
    }

    fn visit_test_eq_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqInt64", dest, lhs, rhs);
    }
    fn visit_test_ne_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNeInt64", dest, lhs, rhs);
    }
    fn visit_test_gt_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGtInt64", dest, lhs, rhs);
    }
    fn visit_test_ge_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGeInt64", dest, lhs, rhs);
    }
    fn visit_test_lt_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLtInt64", dest, lhs, rhs);
    }
    fn visit_test_le_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLeInt64", dest, lhs, rhs);
    }

    fn visit_test_eq_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqFloat32", dest, lhs, rhs);
    }
    fn visit_test_ne_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNeFloat32", dest, lhs, rhs);
    }
    fn visit_test_gt_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGtFloat32", dest, lhs, rhs);
    }
    fn visit_test_ge_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGeFloat32", dest, lhs, rhs);
    }
    fn visit_test_lt_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLtFloat32", dest, lhs, rhs);
    }
    fn visit_test_le_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLeFloat32", dest, lhs, rhs);
    }

    fn visit_test_eq_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqFloat64", dest, lhs, rhs);
    }
    fn visit_test_ne_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNeFloat64", dest, lhs, rhs);
    }
    fn visit_test_gt_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGtFloat64", dest, lhs, rhs);
    }
    fn visit_test_ge_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGeFloat64", dest, lhs, rhs);
    }
    fn visit_test_lt_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLtFloat64", dest, lhs, rhs);
    }
    fn visit_test_le_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLeFloat64", dest, lhs, rhs);
    }

    fn visit_assert(&mut self, value: Register) {
        self.emit_reg1("Assert", value);
    }

    fn visit_jump_if_false(&mut self, opnd: Register, offset: u32) {
        self.emit_cond_jump("JumpIfFalse", opnd, offset as i32);
    }
    fn visit_jump_if_false_const(&mut self, opnd: Register, idx: ConstPoolIdx) {
        self.emit_cond_jump_const("JumpIfFalseConst", opnd, idx);
    }
    fn visit_jump_if_true(&mut self, opnd: Register, offset: u32) {
        self.emit_cond_jump("JumpIfTrue", opnd, offset as i32);
    }
    fn visit_jump_if_true_const(&mut self, opnd: Register, idx: ConstPoolIdx) {
        self.emit_cond_jump_const("JumpIfTrueConst", opnd, idx);
    }
    fn visit_jump_loop(&mut self, offset: u32) {
        self.emit_jump("JumpLoop", -(offset as i32));
    }
    fn visit_loop_start(&mut self) {
        self.emit_inst("LoopStart");
    }
    fn visit_jump(&mut self, offset: u32) {
        self.emit_jump("Jump", offset as i32);
    }
    fn visit_jump_const(&mut self, idx: ConstPoolIdx) {
        self.emit_jump_const("JumpConst", idx);
    }

    fn visit_invoke_direct_void(&mut self, fctdef: ConstPoolIdx) {
        self.emit_fct_void("InvokeDirectVoid", fctdef);
    }
    fn visit_invoke_direct(&mut self, dest: Register, fctdef: ConstPoolIdx) {
        self.emit_fct("InvokeDirect", dest, fctdef);
    }

    fn visit_invoke_virtual_void(&mut self, fct: ConstPoolIdx) {
        self.emit_fct_void("InvokeVirtualVoid", fct);
    }
    fn visit_invoke_virtual(&mut self, dest: Register, fct: ConstPoolIdx) {
        self.emit_fct("InvokeVirtual", dest, fct);
    }

    fn visit_invoke_static_void(&mut self, fctdef: ConstPoolIdx) {
        self.emit_fct_void("InvokeStaticVoid", fctdef);
    }
    fn visit_invoke_static(&mut self, dest: Register, fctdef: ConstPoolIdx) {
        self.emit_fct("InvokeStatic", dest, fctdef);
    }

    fn visit_invoke_generic_static_void(&mut self, fct: ConstPoolIdx) {
        self.emit_fct_void("InvokeGenericStaticVoid", fct);
    }
    fn visit_invoke_generic_static(&mut self, dest: Register, fct: ConstPoolIdx) {
        self.emit_fct("InvokeGenericStatic", dest, fct);
    }

    fn visit_invoke_generic_direct_void(&mut self, fct: ConstPoolIdx) {
        self.emit_fct_void("InvokeGenericDirectVoid", fct);
    }
    fn visit_invoke_generic_direct(&mut self, dest: Register, fct: ConstPoolIdx) {
        self.emit_fct("InvokeGenericDirect", dest, fct);
    }

    fn visit_new_object(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_new_object("NewObject", dest, idx);
    }
    fn visit_new_array(&mut self, dest: Register, idx: ConstPoolIdx, length: Register) {
        self.emit_new_array("NewArray", dest, idx, length);
    }
    fn visit_new_tuple(&mut self, dest: Register, tuple_id: TupleId) {
        self.emit_new_tuple("NewTuple", dest, tuple_id);
    }

    fn visit_nil_check(&mut self, obj: Register) {
        self.emit_reg1("NilCheck", obj);
    }

    fn visit_load_array_bool(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_reg3("LoadArrayBool", dest, arr, idx);
    }
    fn visit_load_array_uint8(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_reg3("LoadArrayUInt8", dest, arr, idx);
    }
    fn visit_load_array_char(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_reg3("LoadArrayChar", dest, arr, idx);
    }
    fn visit_load_array_int32(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_reg3("LoadArrayInt32", dest, arr, idx);
    }
    fn visit_load_array_int64(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_reg3("LoadArrayInt64", dest, arr, idx);
    }
    fn visit_load_array_float32(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_reg3("LoadArrayFloat32", dest, arr, idx);
    }
    fn visit_load_array_float64(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_reg3("LoadArrayFloat64", dest, arr, idx);
    }
    fn visit_load_array_ptr(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_reg3("LoadArrayPtr", dest, arr, idx);
    }
    fn visit_load_array_tuple(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_reg3("LoadArrayTuple", dest, arr, idx);
    }
    fn visit_load_array_generic(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_reg3("LoadArrayGeneric", dest, arr, idx);
    }
    fn visit_load_array_enum(
        &mut self,
        dest: Register,
        arr: Register,
        idx: Register,
        enum_idx: ConstPoolIdx,
    ) {
        self.emit_reg3_idx("LoadArrayEnum", dest, arr, idx, enum_idx);
    }

    fn visit_store_array_bool(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_reg3("StoreArrayBool", src, arr, idx);
    }
    fn visit_store_array_uint8(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_reg3("StoreArrayUInt8", src, arr, idx);
    }
    fn visit_store_array_char(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_reg3("StoreArrayChar", src, arr, idx);
    }
    fn visit_store_array_int32(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_reg3("StoreArrayInt32", src, arr, idx);
    }
    fn visit_store_array_int64(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_reg3("StoreArrayInt64", src, arr, idx);
    }
    fn visit_store_array_float32(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_reg3("StoreArrayFloat32", src, arr, idx);
    }
    fn visit_store_array_float64(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_reg3("StoreArrayFloat64", src, arr, idx);
    }
    fn visit_store_array_ptr(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_reg3("StoreArrayPtr", src, arr, idx);
    }
    fn visit_store_array_tuple(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_reg3("StoreArrayTuple", src, arr, idx);
    }
    fn visit_store_array_generic(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_reg3("StoreArrayGeneric", src, arr, idx);
    }
    fn visit_store_array_enum(
        &mut self,
        src: Register,
        arr: Register,
        idx: Register,
        enum_idx: ConstPoolIdx,
    ) {
        self.emit_reg3_idx("StoreArrayEnum", src, arr, idx, enum_idx);
    }

    fn visit_array_length(&mut self, dest: Register, arr: Register) {
        self.emit_reg2("ArrayLength", dest, arr);
    }
    fn visit_array_bound_check(&mut self, arr: Register, idx: Register) {
        self.emit_reg2("ArrayBoundCheck", arr, idx);
    }

    fn visit_ret_void(&mut self) {
        self.emit_inst("RetVoid");
    }
    fn visit_ret(&mut self, opnd: Register) {
        self.emit_reg1("Ret", opnd);
    }
}
