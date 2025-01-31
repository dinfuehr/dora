use std::fmt::Display;
use std::io;

use crate::{
    display_fct, module_path_name, read, BytecodeFunction, BytecodeOffset, BytecodeType,
    BytecodeTypeArray, BytecodeVisitor, ConstPoolEntry, ConstPoolIdx, GlobalId, Program, Register,
};

pub fn dump_stdout(prog: &Program, bc: &BytecodeFunction) {
    let mut stdout = io::stdout();
    dump(&mut stdout, prog, bc).expect("I/O failure");
}

pub fn dump(w: &mut dyn io::Write, prog: &Program, bc: &BytecodeFunction) -> std::io::Result<()> {
    let mut visitor = BytecodeDumper {
        bc,
        pos: BytecodeOffset(0),
        w,
        prog,
    };
    read(bc.code(), &mut visitor);

    let align = "   ";

    writeln!(w)?;
    writeln!(w, "  Registers:")?;

    for (idx, ty) in bc.registers().iter().enumerate() {
        writeln!(w, "{}{} => {}", align, idx, fmt_ty(prog, ty))?;
    }

    writeln!(w)?;
    writeln!(w, "  Constants:")?;

    for (idx, entry) in bc.const_pool_entries().iter().enumerate() {
        match entry {
            ConstPoolEntry::String(ref value) => {
                writeln!(w, "{}{} => String \"{}\"", align, idx, value)?
            }
            ConstPoolEntry::Int32(ref value) => writeln!(w, "{}{} => Int32 {}", align, idx, value)?,
            ConstPoolEntry::Int64(ref value) => writeln!(w, "{}{} => Int64 {}", align, idx, value)?,
            ConstPoolEntry::Float32(ref value) => {
                writeln!(w, "{}{} => Float32 {}", align, idx, value)?;
            }
            ConstPoolEntry::Float64(ref value) => {
                writeln!(w, "{}{} => Float64 {}", align, idx, value)?;
            }
            ConstPoolEntry::Char(ref value) => writeln!(w, "{}{} => Char {}", align, idx, value)?,
            ConstPoolEntry::Class(cls_id, type_params) => {
                let cls = &prog.class(*cls_id);
                let cls_name = module_path_name(prog, cls.module_id, &cls.name);
                writeln!(
                    w,
                    "{}{} => Class {}{}",
                    align,
                    idx,
                    cls.name,
                    fmt_name(prog, &cls_name, &type_params)
                )?;
            }
            ConstPoolEntry::Struct(struct_id, type_params) => {
                let struct_ = prog.struct_(*struct_id);
                let struct_name = module_path_name(prog, struct_.module_id, &struct_.name);
                writeln!(
                    w,
                    "{}{} => Struct {}",
                    align,
                    idx,
                    fmt_name(prog, &struct_name, &type_params)
                )?;
            }
            ConstPoolEntry::StructField(struct_id, type_params, field_idx) => {
                let struct_ = prog.struct_(*struct_id);
                let struct_name = module_path_name(prog, struct_.module_id, &struct_.name);
                let field = &struct_.fields[*field_idx as usize];
                let name = if let Some(ref name) = field.name {
                    name
                } else {
                    &field_idx.to_string()
                };
                writeln!(
                    w,
                    "{}{} => StructField {}.{}",
                    align,
                    idx,
                    fmt_name(prog, &struct_name, &type_params),
                    name
                )?;
            }
            ConstPoolEntry::Enum(enum_id, type_params) => {
                let enum_ = prog.enum_(*enum_id);
                let enum_name = module_path_name(prog, enum_.module_id, &enum_.name);
                writeln!(
                    w,
                    "{}{} => Enum {}",
                    align,
                    idx,
                    fmt_name(prog, &enum_name, &type_params)
                )?;
            }
            ConstPoolEntry::EnumVariant(enum_id, type_params, variant_idx) => {
                let enum_ = prog.enum_(*enum_id);
                let enum_name = module_path_name(prog, enum_.module_id, &enum_.name);
                let variant = &enum_.variants[*variant_idx as usize];
                writeln!(
                    w,
                    "{}{} => EnumVariant {}::{}",
                    align,
                    idx,
                    fmt_name(prog, &enum_name, &type_params),
                    variant.name,
                )?;
            }
            ConstPoolEntry::EnumElement(enum_id, type_params, variant_idx, element_idx) => {
                let enum_ = &prog.enum_(*enum_id);
                let enum_name = module_path_name(prog, enum_.module_id, &enum_.name);
                let variant = &enum_.variants[*variant_idx as usize];
                writeln!(
                    w,
                    "{}{} => EnumVariantElement {}::{}::{}",
                    align,
                    idx,
                    fmt_name(prog, &enum_name, &type_params),
                    variant.name,
                    element_idx,
                )?;
            }
            ConstPoolEntry::Field(cls_id, type_params, field_id) => {
                let cls = prog.class(*cls_id);
                let cls_name = module_path_name(prog, cls.module_id, &cls.name);
                let field = &cls.fields[*field_id as usize];
                let name = if let Some(ref name) = field.name {
                    name
                } else {
                    &field_id.to_string()
                };
                writeln!(
                    w,
                    "{}{} => Field {}.{}",
                    align,
                    idx,
                    fmt_name(prog, &cls_name, &type_params),
                    name
                )?;
            }
            ConstPoolEntry::Fct(fct_id, type_params) => {
                writeln!(
                    w,
                    "{}{} => Fct {}",
                    align,
                    idx,
                    fmt_name(prog, &display_fct(prog, *fct_id), &type_params)
                )?;
            }
            ConstPoolEntry::TraitObjectMethod(trait_object_ty, fct_id) => {
                writeln!(
                    w,
                    "{}{} => TraitObjectMethod {}.{}",
                    align,
                    idx,
                    fmt_ty(prog, &trait_object_ty),
                    &display_fct(prog, *fct_id)
                )?;
            }
            ConstPoolEntry::Generic(id, fct_id, type_params) => {
                writeln!(
                    w,
                    "{}{} => TypeParam({}) Method {}",
                    align,
                    idx,
                    id,
                    fmt_name(prog, &display_fct(prog, *fct_id), &type_params)
                )?;
            }
            ConstPoolEntry::TraitObject {
                trait_ty,
                actual_object_ty,
            } => writeln!(
                w,
                "{}{} => Trait {} from {}",
                align,
                idx,
                fmt_ty(prog, &trait_ty),
                fmt_ty(prog, &actual_object_ty),
            )?,
            ConstPoolEntry::TupleElement(_tuple_id, _idx) => {
                writeln!(w, "{}{} => TupleElement {}.{}", align, idx, "subtypes", idx)?
            }
            ConstPoolEntry::Tuple(ref subtypes) => {
                writeln!(w, "{}{} => Tuple {}", align, idx, fmt_tuple(prog, subtypes))?
            }
            ConstPoolEntry::Lambda(ref params, ref return_type) => writeln!(
                w,
                "{}{} => Lambda {}: {}",
                align,
                idx,
                fmt_tuple(prog, params),
                fmt_ty(prog, return_type)
            )?,
            ConstPoolEntry::JumpTable {
                targets,
                default_target,
            } => {
                writeln!(w, "{}{} => JumpTable ", align, idx)?;

                for (idx, target) in targets.iter().enumerate() {
                    if idx > 0 {
                        write!(w, ", ")?;
                    }

                    write!(w, "{}", *target)?;
                }

                write!(w, ", default {}", default_target)?
            }
        }
    }

    writeln!(w,)?;
    writeln!(w, "  Locations:")?;
    for (bc_offset, line) in bc.locations().iter() {
        writeln!(w, "{}{} => {}", align, bc_offset.0, line)?;
    }
    writeln!(w,)?;

    Ok(())
}

fn fmt_tuple<'a>(prog: &'a Program, types: &'a BytecodeTypeArray) -> TuplePrinter<'a> {
    TuplePrinter { prog, types }
}

struct TuplePrinter<'a> {
    prog: &'a Program,
    types: &'a BytecodeTypeArray,
}

impl<'a> Display for TuplePrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        let mut first = true;
        for ty in self.types.iter() {
            if !first {
                write!(f, ", ")?;
            }
            write!(f, "{}", fmt_ty(self.prog, &ty))?;
            first = false;
        }
        write!(f, ")")
    }
}

fn fmt_name<'a>(
    prog: &'a Program,
    name: &'a str,
    type_params: &'a BytecodeTypeArray,
) -> NameWithTypeParamsPrinter<'a> {
    NameWithTypeParamsPrinter {
        prog,
        name,
        type_params,
    }
}

struct NameWithTypeParamsPrinter<'a> {
    prog: &'a Program,
    name: &'a str,
    type_params: &'a BytecodeTypeArray,
}

impl<'a> Display for NameWithTypeParamsPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;

        if !self.type_params.is_empty() {
            write!(f, "[")?;
            let mut first = true;
            for tp in self.type_params.iter() {
                if !first {
                    write!(f, ", ")?;
                }
                write!(f, "{}", fmt_ty(self.prog, &tp))?;
                first = false;
            }
            write!(f, "]")
        } else {
            Ok(())
        }
    }
}

fn fmt_ty<'a>(prog: &'a Program, ty: &'a BytecodeType) -> BytecodeTypePrinter<'a> {
    BytecodeTypePrinter { prog, ty }
}

struct BytecodeTypePrinter<'a> {
    prog: &'a Program,
    ty: &'a BytecodeType,
}

impl<'a> Display for BytecodeTypePrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ty {
            BytecodeType::Bool => write!(f, "Bool"),
            BytecodeType::Unit => write!(f, "()"),
            BytecodeType::UInt8 => write!(f, "UInt8"),
            BytecodeType::Char => write!(f, "Char"),
            BytecodeType::Int32 => write!(f, "Int32"),
            BytecodeType::Int64 => write!(f, "Int64"),
            BytecodeType::Float32 => write!(f, "Float32"),
            BytecodeType::Float64 => write!(f, "Float64"),
            BytecodeType::Ptr => write!(f, "Ptr"),
            BytecodeType::This => write!(f, "This"),
            BytecodeType::Tuple(ref types) => write!(f, "{}", fmt_tuple(self.prog, types)),
            BytecodeType::TypeParam(idx) => write!(f, "T#{}", idx),
            BytecodeType::Enum(enum_id, type_params) => {
                let enum_ = self.prog.enum_(*enum_id);
                write!(f, "{}", fmt_name(self.prog, &enum_.name, type_params))
            }
            BytecodeType::Struct(struct_id, type_params) => {
                let struct_ = self.prog.struct_(*struct_id);
                write!(f, "{}", fmt_name(self.prog, &struct_.name, type_params))
            }
            BytecodeType::Class(class_id, type_params) => {
                let class = self.prog.class(*class_id);
                write!(f, "{}", fmt_name(self.prog, &class.name, type_params))
            }
            BytecodeType::TraitObject(trait_id, type_params, _assoc_types) => {
                let trait_ = self.prog.trait_(*trait_id);
                write!(f, "{}", fmt_name(self.prog, &trait_.name, type_params))
            }
            BytecodeType::Lambda(params, return_ty) => {
                write!(
                    f,
                    "{} -> {}",
                    fmt_tuple(self.prog, params),
                    fmt_ty(self.prog, return_ty)
                )
            }
            BytecodeType::TypeAlias(alias_id) | BytecodeType::Assoc(alias_id, ..) => {
                let alias = self.prog.alias(*alias_id);
                write!(f, "{}", alias.name)?;
                if let Some(ref ty) = alias.ty {
                    write!(f, "{}", fmt_ty(self.prog, ty))
                } else {
                    Ok(())
                }
            }
            BytecodeType::GenericAssoc {
                type_param_id,
                assoc_id,
                ..
            } => {
                let assoc = self.prog.alias(*assoc_id);
                write!(f, "T#{}::{}", &type_param_id, assoc.name)
            }
        }
    }
}

struct BytecodeDumper<'a> {
    bc: &'a BytecodeFunction,
    pos: BytecodeOffset,
    w: &'a mut dyn io::Write,
    prog: &'a Program,
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
            fmt_ty(self.prog, tuple_ty),
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
        let enum_ = self.prog.enum_(enum_id);
        let variant = &enum_.variants[variant_idx as usize];
        writeln!(
            self.w,
            " {}, {}, ConstPoolIdx({}), {} # {}::{}.{}",
            r1,
            r2,
            idx.0,
            element_idx,
            fmt_name(self.prog, &enum_.name, type_params),
            variant.name,
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
        let enum_ = self.prog.enum_(enum_id);
        writeln!(
            self.w,
            " {}, {}, ConstPoolIdx({}) # {}",
            r1,
            r2,
            idx.0,
            fmt_name(self.prog, &enum_.name, type_params),
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

    fn emit_jump(&mut self, name: &str, offset: i32) {
        self.emit_start(name);
        let bc_target = self.pos.to_u32() as i32 + offset;
        writeln!(self.w, " {} # target {}", offset, bc_target).expect("write! failed");
    }

    fn emit_field(&mut self, name: &str, r1: Register, r2: Register, field_idx: ConstPoolIdx) {
        self.emit_start(name);
        match self.bc.const_pool(field_idx) {
            ConstPoolEntry::Field(cls_id, type_params, field_id) => {
                let cls = self.prog.class(*cls_id);
                let field = &cls.fields[*field_id as usize];
                let name = if let Some(ref name) = field.name {
                    name
                } else {
                    &field_id.to_string()
                };
                writeln!(
                    self.w,
                    " {}, {}, ConstPoolIdx({}) # {}.{}",
                    r1,
                    r2,
                    field_idx.0,
                    fmt_name(self.prog, &cls.name, type_params),
                    name,
                )
                .expect("write! failed");
            }
            ConstPoolEntry::StructField(struct_id, type_params, field_id) => {
                let struct_ = self.prog.struct_(*struct_id);
                let field = &struct_.fields[*field_id as usize];
                let name = if let Some(ref name) = field.name {
                    name
                } else {
                    &field_id.to_string()
                };

                writeln!(
                    self.w,
                    " {}, {}, ConstPoolIdx({}) # {}.{}",
                    r1,
                    r2,
                    field_idx.0,
                    fmt_name(self.prog, &struct_.name, type_params),
                    name,
                )
                .expect("write! failed");
            }
            _ => unreachable!(),
        }
    }

    fn emit_global(&mut self, name: &str, r1: Register, gid: GlobalId) {
        self.emit_start(name);
        let global_var = self.prog.global(gid);
        writeln!(self.w, " {}, GlobalId({}) # {}", r1, gid.0, global_var.name)
            .expect("write! failed");
    }

    fn emit_fct(&mut self, name: &str, r1: Register, fid: ConstPoolIdx) {
        self.emit_start(name);
        let fname = self.get_fct_name(fid);
        writeln!(self.w, " {}, ConstPoolIdx({}) # {}", r1, fid.0, fname).expect("write! failed");
    }

    fn get_fct_name(&mut self, idx: ConstPoolIdx) -> String {
        let fct_id = match self.bc.const_pool(idx) {
            ConstPoolEntry::Fct(fct_id, _) => fct_id,
            ConstPoolEntry::Generic(_, fct_id, _) => fct_id,
            ConstPoolEntry::Lambda(_, _) => return "lambda".into(),
            ConstPoolEntry::TraitObjectMethod(_, trait_fct_id) => trait_fct_id,
            _ => unreachable!(),
        };

        display_fct(&self.prog, *fct_id)
    }

    fn emit_new_lambda(&mut self, name: &str, r1: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        let (fct_id, _type_params) = match self.bc.const_pool(idx) {
            ConstPoolEntry::Fct(fct_id, type_params) => (*fct_id, type_params.clone()),
            _ => unreachable!(),
        };
        let fct = self.prog.fct(fct_id);
        writeln!(self.w, " {}, ConstPoolIdx({}) # {}", r1, idx.0, fct.name).expect("write! failed");
    }

    fn emit_new_object(&mut self, name: &str, r1: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        let (cls_id, type_params) = match self.bc.const_pool(idx) {
            ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params),
            _ => unreachable!(),
        };
        let cls = self.prog.class(cls_id);
        writeln!(
            self.w,
            " {}, ConstPoolIdx({}) # {}",
            r1,
            idx.0,
            fmt_name(self.prog, &cls.name, type_params)
        )
        .expect("write! failed");
    }

    fn emit_new_trait_object(&mut self, name: &str, r1: Register, r2: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        let (trait_ty, actual_object_ty) = match self.bc.const_pool(idx) {
            ConstPoolEntry::TraitObject {
                trait_ty,
                actual_object_ty,
            } => (trait_ty, actual_object_ty),
            _ => unreachable!(),
        };
        writeln!(
            self.w,
            " {}, {}, ConstPoolIdx({}) # {} wrapping {}",
            r1,
            r2,
            idx.0,
            fmt_ty(self.prog, &trait_ty),
            fmt_ty(self.prog, &actual_object_ty),
        )
        .expect("write! failed");
    }

    fn emit_new_array(&mut self, name: &str, r1: Register, length: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        let (cls_id, type_params) = match self.bc.const_pool(idx) {
            ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params),
            _ => unreachable!(),
        };
        let cls = self.prog.class(cls_id);
        writeln!(
            self.w,
            " {}, {}, ConstPoolIdx({}) # {}",
            r1,
            length,
            idx.0,
            fmt_name(self.prog, &cls.name, type_params),
        )
        .expect("write! failed");
    }

    fn emit_new_tuple(&mut self, name: &str, r1: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        let types = match self.bc.const_pool(idx) {
            ConstPoolEntry::Tuple(ref subtypes) => subtypes,
            _ => unreachable!(),
        };
        writeln!(self.w, " {}, {}", r1, fmt_tuple(self.prog, types)).expect("write! failed");
    }

    fn emit_new_enum(&mut self, name: &str, r1: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        let (enum_id, type_params, variant_idx) = match self.bc.const_pool(idx) {
            ConstPoolEntry::EnumVariant(enum_id, type_params, variant_idx) => {
                (*enum_id, type_params, *variant_idx)
            }
            _ => unreachable!(),
        };
        let enum_ = self.prog.enum_(enum_id);
        let variant = &enum_.variants[variant_idx as usize];
        writeln!(
            self.w,
            " {}, ConstPoolIdx({}) # {}::{}",
            r1,
            idx.0,
            fmt_name(self.prog, &enum_.name, type_params),
            variant.name,
        )
        .expect("write! failed");
    }

    fn emit_new_struct(&mut self, name: &str, r1: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        let (struct_id, type_params) = match self.bc.const_pool(idx) {
            ConstPoolEntry::Struct(struct_id, type_params) => (*struct_id, type_params),
            _ => unreachable!(),
        };
        let struct_ = self.prog.struct_(struct_id);
        writeln!(
            self.w,
            " {}, ConstPoolIdx({}) # {}",
            r1,
            idx.0,
            fmt_name(self.prog, &struct_.name, type_params),
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

    fn visit_load_global(&mut self, dest: Register, global_id: GlobalId) {
        self.emit_global("LoadGlobal", dest, global_id);
    }

    fn visit_store_global(&mut self, src: Register, global_id: GlobalId) {
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
            dest, idx.0, value, value as u32
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
        writeln!(self.w, " {}, ConstPoolIdx({}) # {}", dest, idx.0, value).expect("write! failed");
    }
    fn visit_const_int64(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_start("ConstInt64");
        let value = self.bc.const_pool(idx).to_int64().expect("int64 expected");
        writeln!(self.w, " {}, ConstPoolIdx({}) # {}", dest, idx.0, value).expect("write! failed");
    }
    fn visit_const_float32(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_start("ConstFloat32");
        let value = self
            .bc
            .const_pool(idx)
            .to_float32()
            .expect("float32 expected");
        writeln!(self.w, " {}, ConstPoolIdx({}) # {}", dest, idx.0, value).expect("write! failed");
    }
    fn visit_const_float64(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_start("ConstFloat64");
        let value = self
            .bc
            .const_pool(idx)
            .to_float64()
            .expect("float64 expected");
        writeln!(self.w, " {}, ConstPoolIdx({}) # {}", dest, idx.0, value).expect("write! failed");
    }
    fn visit_const_string(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_start("ConstString");
        let value = self
            .bc
            .const_pool(idx)
            .to_string()
            .expect("string expected");
        writeln!(self.w, " {}, ConstPoolIdx({}) # \"{}\"", dest, idx.0, value)
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
    fn visit_jump_if_true(&mut self, opnd: Register, offset: u32) {
        self.emit_cond_jump("JumpIfTrue", opnd, offset as i32);
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
    fn visit_new_trait_object(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
        self.emit_new_trait_object("NewTraitObject", dest, src, idx);
    }
    fn visit_new_lambda(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_new_lambda("NewLambda", dest, idx);
    }
    fn visit_new_array(&mut self, dest: Register, length: Register, idx: ConstPoolIdx) {
        self.emit_new_array("NewArray", dest, length, idx);
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

    fn visit_load_array(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_reg3("LoadArray", dest, arr, idx);
    }

    fn visit_store_array(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_reg3("StoreArray", src, arr, idx);
    }

    fn visit_array_length(&mut self, dest: Register, arr: Register) {
        self.emit_reg2("ArrayLength", dest, arr);
    }

    fn visit_load_trait_object_value(&mut self, dest: Register, object: Register) {
        self.emit_reg2("LoadTraitObjectValue", dest, object);
    }

    fn visit_ret(&mut self, opnd: Register) {
        self.emit_reg1("Ret", opnd);
    }
}
