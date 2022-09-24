use std::io;

use crate::bytecode::{
    read, BytecodeFunction, BytecodeOffset, BytecodeVisitor, ConstPoolEntry, ConstPoolIdx, Register,
};
use crate::language::sem_analysis::{FctDefinition, GlobalDefinitionId, SemAnalysis};

pub fn dump(vm: &SemAnalysis, fct: Option<&FctDefinition>, bc: &BytecodeFunction) {
    let mut stdout = io::stdout();
    if let Some(fct) = fct {
        println!("{}", fct.display_name(vm));
    }
    let mut visitor = BytecodeDumper {
        bc,
        pos: BytecodeOffset(0),
        w: &mut stdout,
        sa: vm,
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
            ConstPoolEntry::Struct(struct_id, type_params) => {
                let struct_ = vm.structs.idx(*struct_id);
                let struct_ = struct_.read();
                println!(
                    "{}{} => Struct {}",
                    align,
                    idx,
                    struct_.name_with_params(vm, type_params)
                )
            }
            ConstPoolEntry::StructField(struct_id, type_params, field_idx) => {
                let struct_ = vm.structs.idx(*struct_id);
                let struct_ = struct_.read();
                let field = &struct_.fields[field_idx.to_usize()];
                let fname = vm.interner.str(field.name);
                println!(
                    "{}{} => StructField {}.{}",
                    align,
                    idx,
                    struct_.name_with_params(vm, type_params),
                    fname
                )
            }
            ConstPoolEntry::Enum(enum_id, type_params) => {
                let enum_ = &vm.enums[*enum_id];
                let enum_ = enum_.read();
                println!(
                    "{}{} => Enum {}",
                    align,
                    idx,
                    enum_.name_with_params(vm, type_params)
                )
            }
            ConstPoolEntry::EnumVariant(cls_id, type_params, variant_idx) => {
                let enum_ = &vm.enums[*cls_id];
                let enum_ = enum_.read();
                let variant = &enum_.variants[*variant_idx];
                let variant_name = vm.interner.str(variant.name);
                println!(
                    "{}{} => EnumVariant {}::{}",
                    align,
                    idx,
                    enum_.name_with_params(vm, type_params),
                    variant_name,
                )
            }
            ConstPoolEntry::EnumElement(cls_id, type_params, variant_idx, element_idx) => {
                let enum_ = &vm.enums[*cls_id];
                let enum_ = enum_.read();
                let variant = &enum_.variants[*variant_idx];
                let variant_name = vm.interner.str(variant.name);
                println!(
                    "{}{} => EnumVariantElement {}::{}::{}",
                    align,
                    idx,
                    enum_.name_with_params(vm, type_params),
                    variant_name,
                    element_idx,
                )
            }
            ConstPoolEntry::Field(cls_id, type_params, field_id) => {
                let cls = vm.classes.idx(*cls_id);
                let cls = cls.read();
                let field = &cls.fields[field_id.to_usize()];
                let fname = vm.interner.str(field.name);
                println!(
                    "{}{} => Field {}.{}",
                    align,
                    idx,
                    cls.name_with_params(vm, type_params),
                    fname,
                )
            }
            ConstPoolEntry::FieldFixed(_, field_id) => {
                println!("{}{} => FieldFixed {}", align, idx, field_id.to_usize())
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
                        fct.display_name(vm),
                        type_params
                    );
                } else {
                    println!("{}{} => Fct {}", align, idx, fct.display_name(vm));
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
                        fct.display_name(vm),
                        type_params
                    );
                } else {
                    println!(
                        "{}{} => TypeParam({}) Method {}",
                        align,
                        idx,
                        id.to_usize(),
                        fct.display_name(vm)
                    );
                }
            }
            ConstPoolEntry::Trait(trait_id, type_params, object_ty) => {
                let trait_ = vm.traits[*trait_id].read();
                println!(
                    "{}{} => Trait {} from {}",
                    align,
                    idx,
                    trait_.name_with_params(vm, type_params),
                    object_ty.name(vm),
                )
            }
            ConstPoolEntry::TupleElement(_tuple_id, _idx) => {
                println!("{}{} => TupleElement {}.{}", align, idx, "subtypes", idx)
            }
            ConstPoolEntry::Tuple(ref source_type_array) => {
                let tuple_name = source_type_array.tuple_name(vm);
                println!("{}{} => Tuple {}", align, idx, tuple_name)
            }
            ConstPoolEntry::Lambda(ref params, ref return_type) => {
                let params = params.tuple_name(vm);
                let return_type = return_type.name(vm);
                println!("{}{} => Lambda {}: {}", align, idx, params, return_type)
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

struct BytecodeDumper<'a> {
    bc: &'a BytecodeFunction,
    pos: BytecodeOffset,
    w: &'a mut dyn io::Write,
    sa: &'a SemAnalysis,
}

impl<'a> BytecodeDumper<'a> {
    fn emit_inst(&mut self, name: &str) {
        self.emit_start(name);
        writeln!(self.w, "").expect("write! failed");
    }

    fn emit_reg3(&mut self, name: &str, r1: Register, r2: Register, r3: Register) {
        self.emit_start(name);
        writeln!(self.w, " {}, {}, {}", r1, r2, r3).expect("write! failed");
    }

    fn emit_reg2(&mut self, name: &str, r1: Register, r2: Register) {
        self.emit_start(name);
        writeln!(self.w, " {}, {}", r1, r2).expect("write! failed");
    }

    fn emit_tuple_load(&mut self, name: &str, r1: Register, r2: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        let (tuple_ty, subtype_idx) = match self.bc.const_pool(idx) {
            ConstPoolEntry::TupleElement(tuple_ty, subtype_idx) => (tuple_ty, *subtype_idx),
            _ => unreachable!(),
        };
        writeln!(
            self.w,
            " {}, {}, {}, {}",
            r1,
            r2,
            tuple_ty.name(self.sa),
            subtype_idx
        )
        .expect("write! failed");
    }

    fn emit_enum_load(&mut self, name: &str, r1: Register, r2: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        let (enum_id, type_params, variant_idx, element_idx) = match self.bc.const_pool(idx) {
            ConstPoolEntry::EnumElement(enum_id, type_params, variant_idx, element_idx) => {
                (*enum_id, type_params, *variant_idx, *element_idx)
            }
            _ => unreachable!(),
        };
        let enum_ = &self.sa.enums[enum_id];
        let enum_ = enum_.read();
        let enum_name = enum_.name_with_params(self.sa, type_params);
        let variant_name = self.sa.interner.str(enum_.variants[variant_idx].name);
        writeln!(
            self.w,
            " {}, {}, ConstPoolIdx({}), {} # {}::{}.{}",
            r1,
            r2,
            idx.to_usize(),
            element_idx,
            enum_name,
            variant_name,
            element_idx
        )
        .expect("write! failed");
    }

    fn emit_enum_variant(&mut self, name: &str, r1: Register, r2: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        let (enum_id, type_params) = match self.bc.const_pool(idx) {
            ConstPoolEntry::Enum(enum_id, type_params) => (*enum_id, type_params),
            _ => unreachable!(),
        };
        let enum_ = &self.sa.enums[enum_id];
        let enum_ = enum_.read();
        let enum_name = enum_.name_with_params(self.sa, type_params);
        writeln!(
            self.w,
            " {}, {}, ConstPoolIdx({}) # {}",
            r1,
            r2,
            idx.to_usize(),
            enum_name,
        )
        .expect("write! failed");
    }

    fn emit_reg1(&mut self, name: &str, r1: Register) {
        self.emit_start(name);
        writeln!(self.w, " {}", r1).expect("write! failed");
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
        let (cname, fname) = match self.bc.const_pool(field_idx) {
            ConstPoolEntry::Field(cls_id, type_params, field_id) => {
                let cls = self.sa.classes.idx(*cls_id);
                let cls = cls.read();
                let cname = cls.name_with_params(self.sa, type_params);

                let field = &cls.fields[field_id.to_usize()];
                let fname = self.sa.interner.str(field.name).to_string();

                (cname, fname)
            }
            ConstPoolEntry::StructField(struct_id, type_params, field_id) => {
                let struct_ = self.sa.structs.idx(*struct_id);
                let struct_ = struct_.read();
                let struct_name = struct_.name_with_params(self.sa, type_params);

                let field = &struct_.fields[field_id.to_usize()];
                let fname = self.sa.interner.str(field.name).to_string();

                (struct_name, fname)
            }
            ConstPoolEntry::FieldFixed(class_instance_id, field_id) => {
                let cls = self.sa.class_instances.idx(*class_instance_id);
                let cname = cls
                    .trait_object()
                    .expect("trait object expected")
                    .name(self.sa);

                let fname = format!("{}", field_id.to_usize());

                (cname, fname)
            }
            _ => unreachable!(),
        };

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

    fn emit_global(&mut self, name: &str, r1: Register, gid: GlobalDefinitionId) {
        self.emit_start(name);
        let global_var = self.sa.globals.idx(gid);
        let global_var = global_var.read();
        let name = self.sa.interner.str(global_var.name);
        writeln!(self.w, " {}, GlobalId({}) # {}", r1, gid.to_usize(), name)
            .expect("write! failed");
    }

    fn emit_fct(&mut self, name: &str, r1: Register, fid: ConstPoolIdx) {
        self.emit_start(name);
        let fname = self.get_fct_name(fid);
        writeln!(
            self.w,
            " {}, ConstPoolIdx({}) # {}",
            r1,
            fid.to_usize(),
            fname
        )
        .expect("write! failed");
    }

    fn get_fct_name(&mut self, idx: ConstPoolIdx) -> String {
        let fct_id = match self.bc.const_pool(idx) {
            ConstPoolEntry::Fct(fct_id, _) => fct_id,
            ConstPoolEntry::Generic(_, fct_id, _) => fct_id,
            ConstPoolEntry::Lambda(_, _) => return "lambda".into(),
            _ => unreachable!(),
        };

        let fct = self.sa.fcts.idx(*fct_id);
        let fct = fct.read();

        fct.display_name(self.sa)
    }

    fn emit_new_lambda(&mut self, name: &str, r1: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        let (fct_id, _type_params) = match self.bc.const_pool(idx) {
            ConstPoolEntry::Fct(fct_id, type_params) => (*fct_id, type_params.clone()),
            _ => unreachable!(),
        };
        let fct = self.sa.fcts.idx(fct_id);
        let fct = fct.read();
        let fname = fct.display_name(self.sa);
        writeln!(
            self.w,
            " {}, ConstPoolIdx({}) # {}",
            r1,
            idx.to_usize(),
            fname
        )
        .expect("write! failed");
    }

    fn emit_new_object(&mut self, name: &str, r1: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        let (cls_id, type_params) = match self.bc.const_pool(idx) {
            ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params.clone()),
            _ => unreachable!(),
        };
        let cls = self.sa.classes.idx(cls_id);
        let cls = cls.read();
        let cname = cls.name_with_params(self.sa, &type_params);
        writeln!(
            self.w,
            " {}, ConstPoolIdx({}) # {}",
            r1,
            idx.to_usize(),
            cname
        )
        .expect("write! failed");
    }

    fn emit_new_trait_object(&mut self, name: &str, r1: Register, idx: ConstPoolIdx, r2: Register) {
        self.emit_start(name);
        let (trait_id, type_params, actual_ty) = match self.bc.const_pool(idx) {
            ConstPoolEntry::Trait(trait_id, type_params, ty) => {
                (*trait_id, type_params.clone(), ty.clone())
            }
            _ => unreachable!(),
        };
        let trait_ = self.sa.traits.idx(trait_id);
        let trait_ = trait_.read();
        let trait_name = trait_.name_with_params(self.sa, &type_params);
        writeln!(
            self.w,
            " {}, ConstPoolIdx({}), {} # {} wrapping {}",
            r1,
            idx.to_usize(),
            r2,
            trait_name,
            actual_ty.name(self.sa),
        )
        .expect("write! failed");
    }

    fn emit_new_array(&mut self, name: &str, r1: Register, idx: ConstPoolIdx, length: Register) {
        self.emit_start(name);
        let (cls_id, type_params) = match self.bc.const_pool(idx) {
            ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params),
            _ => unreachable!(),
        };
        let cls = self.sa.classes.idx(cls_id);
        let cls = cls.read();
        let cname = cls.name_with_params(self.sa, type_params);
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

    fn emit_new_tuple(&mut self, name: &str, r1: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        let source_type_array = match self.bc.const_pool(idx) {
            ConstPoolEntry::Tuple(ref source_type_array) => source_type_array.clone(),
            _ => unreachable!(),
        };
        let tuple_name = source_type_array.tuple_name(self.sa);
        writeln!(self.w, " {}, {}", r1, tuple_name).expect("write! failed");
    }

    fn emit_new_enum(&mut self, name: &str, r1: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        let (enum_id, type_params, variant_idx) = match self.bc.const_pool(idx) {
            ConstPoolEntry::EnumVariant(enum_id, type_params, variant_idx) => {
                (*enum_id, type_params, *variant_idx)
            }
            _ => unreachable!(),
        };
        let enum_ = &self.sa.enums[enum_id];
        let enum_ = enum_.read();
        let enum_name = enum_.name_with_params(self.sa, type_params);
        let variant_name = self.sa.interner.str(enum_.variants[variant_idx].name);
        writeln!(
            self.w,
            " {}, ConstPoolIdx({}) # {}::{}",
            r1,
            idx.to_usize(),
            enum_name,
            variant_name,
        )
        .expect("write! failed");
    }

    fn emit_new_struct(&mut self, name: &str, r1: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        let (struct_id, type_params) = match self.bc.const_pool(idx) {
            ConstPoolEntry::Struct(struct_id, type_params) => (*struct_id, type_params),
            _ => unreachable!(),
        };
        let struct_ = self.sa.structs.idx(struct_id);
        let struct_ = struct_.read();
        let struct_name = struct_.name_with_params(self.sa, type_params);
        writeln!(
            self.w,
            " {}, ConstPoolIdx({}) # {}",
            r1,
            idx.to_usize(),
            struct_name,
        )
        .expect("write! failed");
    }

    fn emit_start(&mut self, name: &str) {
        write!(self.w, "{:3}: {}", self.pos.to_usize(), name).expect("write! failed");
    }
}

impl<'a> BytecodeVisitor for BytecodeDumper<'a> {
    fn visit_instruction(&mut self, offset: BytecodeOffset) {
        self.pos = offset;
    }

    fn visit_add(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("Add", dest, lhs, rhs);
    }

    fn visit_sub(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("Sub", dest, lhs, rhs);
    }

    fn visit_neg(&mut self, dest: Register, src: Register) {
        self.emit_reg2("NegInt32", dest, src);
    }

    fn visit_mul(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("Mul", dest, lhs, rhs);
    }

    fn visit_div(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("Div", dest, lhs, rhs);
    }

    fn visit_mod(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("Mod", dest, lhs, rhs);
    }

    fn visit_and(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("And", dest, lhs, rhs);
    }

    fn visit_or(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("Or", dest, lhs, rhs);
    }

    fn visit_xor(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("Xor", dest, lhs, rhs);
    }

    fn visit_not(&mut self, dest: Register, src: Register) {
        self.emit_reg2("Not", dest, src);
    }

    fn visit_shl(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("Shl", dest, lhs, rhs);
    }
    fn visit_shr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("Shr", dest, lhs, rhs);
    }
    fn visit_sar(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("Sar", dest, lhs, rhs);
    }

    fn visit_mov(&mut self, dest: Register, src: Register) {
        self.emit_reg2("Mov", dest, src);
    }

    fn visit_load_tuple_element(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
        self.emit_tuple_load("LoadTupleElement", dest, src, idx);
    }

    fn visit_load_enum_element(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
        self.emit_enum_load("LoadEnumElement", dest, src, idx);
    }

    fn visit_load_enum_variant(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
        self.emit_enum_variant("LoadEnumVariant", dest, src, idx);
    }

    fn visit_load_field(&mut self, dest: Register, obj: Register, field_idx: ConstPoolIdx) {
        self.emit_field("LoadField", dest, obj, field_idx);
    }

    fn visit_load_struct_field(&mut self, dest: Register, obj: Register, field_idx: ConstPoolIdx) {
        self.emit_field("LoadStructField", dest, obj, field_idx);
    }

    fn visit_store_field(&mut self, src: Register, obj: Register, field_idx: ConstPoolIdx) {
        self.emit_field("StoreField", src, obj, field_idx);
    }

    fn visit_load_global(&mut self, dest: Register, global_id: GlobalDefinitionId) {
        self.emit_global("LoadGlobal", dest, global_id);
    }

    fn visit_store_global(&mut self, src: Register, global_id: GlobalDefinitionId) {
        self.emit_global("StoreGlobal", src, global_id);
    }

    fn visit_push_register(&mut self, src: Register) {
        self.emit_reg1("PushRegister", src)
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
            " {}, ConstPoolIdx({}) # '{}' 0x{:x}",
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
        writeln!(
            self.w,
            " {}, ConstPoolIdx({}) # {}",
            dest,
            idx.to_usize(),
            value
        )
        .expect("write! failed");
    }
    fn visit_const_int64(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_start("ConstInt64");
        let value = self.bc.const_pool(idx).to_int64().expect("int64 expected");
        writeln!(
            self.w,
            " {}, ConstPoolIdx({}) # {}",
            dest,
            idx.to_usize(),
            value
        )
        .expect("write! failed");
    }
    fn visit_const_float32(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_start("ConstFloat32");
        let value = self
            .bc
            .const_pool(idx)
            .to_float32()
            .expect("float32 expected");
        writeln!(
            self.w,
            " {}, ConstPoolIdx({}) # {}",
            dest,
            idx.to_usize(),
            value
        )
        .expect("write! failed");
    }
    fn visit_const_float64(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_start("ConstFloat64");
        let value = self
            .bc
            .const_pool(idx)
            .to_float64()
            .expect("float64 expected");
        writeln!(
            self.w,
            " {}, ConstPoolIdx({}) # {}",
            dest,
            idx.to_usize(),
            value
        )
        .expect("write! failed");
    }
    fn visit_const_string(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_start("ConstString");
        let value = self
            .bc
            .const_pool(idx)
            .to_string()
            .expect("string expected");
        writeln!(
            self.w,
            " {}, ConstPoolIdx({}) # \"{}\"",
            dest,
            idx.to_usize(),
            value
        )
        .expect("write! failed");
    }

    fn visit_test_eq(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEq", dest, lhs, rhs);
    }
    fn visit_test_ne(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNe", dest, lhs, rhs);
    }
    fn visit_test_gt(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGt", dest, lhs, rhs);
    }
    fn visit_test_ge(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGe", dest, lhs, rhs);
    }
    fn visit_test_lt(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLt", dest, lhs, rhs);
    }
    fn visit_test_le(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLe", dest, lhs, rhs);
    }
    fn visit_test_identity(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestIdentity", dest, lhs, rhs);
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

    fn visit_invoke_direct(&mut self, dest: Register, fctdef: ConstPoolIdx) {
        self.emit_fct("InvokeDirect", dest, fctdef);
    }

    fn visit_invoke_virtual(&mut self, dest: Register, fct: ConstPoolIdx) {
        self.emit_fct("InvokeVirtual", dest, fct);
    }

    fn visit_invoke_static(&mut self, dest: Register, fctdef: ConstPoolIdx) {
        self.emit_fct("InvokeStatic", dest, fctdef);
    }

    fn visit_invoke_lambda(&mut self, dest: Register, fct: ConstPoolIdx) {
        self.emit_fct("InvokeLambda", dest, fct);
    }

    fn visit_invoke_generic_static(&mut self, dest: Register, fct: ConstPoolIdx) {
        self.emit_fct("InvokeGenericStatic", dest, fct);
    }

    fn visit_invoke_generic_direct(&mut self, dest: Register, fct: ConstPoolIdx) {
        self.emit_fct("InvokeGenericDirect", dest, fct);
    }

    fn visit_new_object(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_new_object("NewObject", dest, idx);
    }
    fn visit_new_object_initialized(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_new_object("NewObjectInitialized", dest, idx);
    }
    fn visit_new_trait_object(&mut self, dest: Register, idx: ConstPoolIdx, src: Register) {
        self.emit_new_trait_object("NewTraitObject", dest, idx, src);
    }
    fn visit_new_lambda(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_new_lambda("NewLambda", dest, idx);
    }
    fn visit_new_array(&mut self, dest: Register, idx: ConstPoolIdx, length: Register) {
        self.emit_new_array("NewArray", dest, idx, length);
    }
    fn visit_new_tuple(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_new_tuple("NewTuple", dest, idx);
    }
    fn visit_new_enum(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_new_enum("NewEnum", dest, idx);
    }
    fn visit_new_struct(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_new_struct("NewStruct", dest, idx);
    }

    fn visit_array_length(&mut self, dest: Register, arr: Register) {
        self.emit_reg2("ArrayLength", dest, arr);
    }

    fn visit_load_array(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_reg3("LoadArray", dest, arr, idx);
    }

    fn visit_store_array(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_reg3("StoreArray", src, arr, idx);
    }

    fn visit_load_string_uint8(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_reg3("LoadStringUInt8", dest, arr, idx);
    }

    fn visit_string_length(&mut self, dest: Register, arr: Register) {
        self.emit_reg2("StringLength", dest, arr);
    }

    fn visit_ret(&mut self, opnd: Register) {
        self.emit_reg1("Ret", opnd);
    }
}
