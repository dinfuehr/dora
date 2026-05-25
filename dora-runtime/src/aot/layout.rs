use std::cell::RefCell;
use std::cmp::max;
use std::collections::HashMap;

use crate::mem;
use crate::mode::MachineMode;
use crate::size::InstanceSize;
use crate::vm::{specialize_bty, specialize_ty_in_program};
use dora_bytecode::{BytecodeType, BytecodeTypeArray, EnumData, EnumId, Program, StructId};

pub(crate) struct AotLayout<'a> {
    program: &'a Program,
    tuples: RefCell<HashMap<BytecodeTypeArray, AotRecordLayout>>,
    structs: RefCell<HashMap<(StructId, BytecodeTypeArray), AotRecordLayout>>,
    enums: RefCell<HashMap<(EnumId, BytecodeTypeArray), AotEnumLayout>>,
}

#[derive(Clone)]
struct AotRecordLayout {
    size: i32,
    align: i32,
    refs: Vec<i32>,
}

#[derive(Clone, Copy)]
enum AotEnumLayout {
    Int,
    Ptr,
    Tagged,
}

impl<'a> AotLayout<'a> {
    pub(crate) fn new(program: &'a Program) -> AotLayout<'a> {
        AotLayout {
            program,
            tuples: RefCell::new(HashMap::new()),
            structs: RefCell::new(HashMap::new()),
            enums: RefCell::new(HashMap::new()),
        }
    }

    pub(crate) fn size(&self, ty: BytecodeType) -> i32 {
        match ty {
            BytecodeType::Unit => 0,
            BytecodeType::Bool => 1,
            BytecodeType::UInt8 => 1,
            BytecodeType::Char => 4,
            BytecodeType::Int32 => 4,
            BytecodeType::Int64 => 8,
            BytecodeType::Float32 => 4,
            BytecodeType::Float64 => 8,
            BytecodeType::Ptr
            | BytecodeType::Address
            | BytecodeType::TraitObject(..)
            | BytecodeType::Class(..)
            | BytecodeType::Lambda(..)
            | BytecodeType::Ref(..) => mem::ptr_width(),
            BytecodeType::Tuple(subtypes) => self.tuple_layout(subtypes).size,
            BytecodeType::Enum(enum_id, type_params) => {
                match self.enum_layout(enum_id, &type_params) {
                    AotEnumLayout::Int => 4,
                    AotEnumLayout::Ptr | AotEnumLayout::Tagged => mem::ptr_width(),
                }
            }
            BytecodeType::Struct(struct_id, type_params) => {
                self.struct_layout(struct_id, &type_params).size
            }
            BytecodeType::TypeAlias(..)
            | BytecodeType::Assoc { .. }
            | BytecodeType::TypeParam(_)
            | BytecodeType::This => {
                unreachable!()
            }
        }
    }

    pub(crate) fn mode(&self, ty: BytecodeType) -> MachineMode {
        match ty {
            BytecodeType::Bool => MachineMode::Int8,
            BytecodeType::UInt8 => MachineMode::Int8,
            BytecodeType::Char => MachineMode::Int32,
            BytecodeType::Int32 => MachineMode::Int32,
            BytecodeType::Int64 => MachineMode::Int64,
            BytecodeType::Float32 => MachineMode::Float32,
            BytecodeType::Float64 => MachineMode::Float64,
            BytecodeType::Ptr
            | BytecodeType::Address
            | BytecodeType::TraitObject(..)
            | BytecodeType::Class(..)
            | BytecodeType::Lambda(..)
            | BytecodeType::Ref(..) => MachineMode::Ptr,
            BytecodeType::Enum(enum_id, type_params) => {
                match self.enum_layout(enum_id, &type_params) {
                    AotEnumLayout::Int => MachineMode::Int32,
                    AotEnumLayout::Ptr | AotEnumLayout::Tagged => MachineMode::Ptr,
                }
            }
            BytecodeType::TypeAlias(..)
            | BytecodeType::Assoc { .. }
            | BytecodeType::Tuple(_)
            | BytecodeType::TypeParam(_)
            | BytecodeType::This
            | BytecodeType::Struct(_, _)
            | BytecodeType::Unit => {
                panic!("unexpected type {:?}", ty)
            }
        }
    }

    pub(crate) fn align(&self, ty: BytecodeType) -> i32 {
        match ty {
            BytecodeType::Unit => 0,
            BytecodeType::Bool => 1,
            BytecodeType::UInt8 => 1,
            BytecodeType::Char => 4,
            BytecodeType::Int32 => 4,
            BytecodeType::Int64 => 8,
            BytecodeType::Float32 => 4,
            BytecodeType::Float64 => 8,
            BytecodeType::Ptr
            | BytecodeType::Address
            | BytecodeType::TraitObject(..)
            | BytecodeType::Class(..)
            | BytecodeType::Lambda(..)
            | BytecodeType::Ref(..) => mem::ptr_width(),
            BytecodeType::Tuple(subtypes) => self.tuple_layout(subtypes).align,
            BytecodeType::Enum(enum_id, type_params) => {
                match self.enum_layout(enum_id, &type_params) {
                    AotEnumLayout::Int => 4,
                    AotEnumLayout::Ptr | AotEnumLayout::Tagged => mem::ptr_width(),
                }
            }
            BytecodeType::Struct(struct_id, type_params) => {
                self.struct_layout(struct_id, &type_params).align
            }
            BytecodeType::TypeAlias(..)
            | BytecodeType::Assoc { .. }
            | BytecodeType::TypeParam(_)
            | BytecodeType::This => {
                unreachable!()
            }
        }
    }

    pub(crate) fn add_ref_fields(&self, refs: &mut Vec<i32>, offset: i32, ty: BytecodeType) {
        assert!(ty.is_concrete_type());

        match ty {
            BytecodeType::Tuple(subtypes) => {
                let tuple = self.tuple_layout(subtypes);

                for &ref_offset in &tuple.refs {
                    refs.push(offset + ref_offset);
                }
            }

            BytecodeType::Enum(enum_id, type_params) => {
                match self.enum_layout(enum_id, &type_params) {
                    AotEnumLayout::Int => {}
                    AotEnumLayout::Ptr | AotEnumLayout::Tagged => refs.push(offset),
                }
            }

            BytecodeType::Struct(struct_id, type_params) => {
                let struct_ = self.struct_layout(struct_id, &type_params);

                for &ref_offset in &struct_.refs {
                    refs.push(offset + ref_offset);
                }
            }

            BytecodeType::Bool
            | BytecodeType::Char
            | BytecodeType::UInt8
            | BytecodeType::Int32
            | BytecodeType::Int64
            | BytecodeType::Float32
            | BytecodeType::Float64
            | BytecodeType::Address
            | BytecodeType::Unit => {}

            BytecodeType::Ptr
            | BytecodeType::Class(..)
            | BytecodeType::Lambda(..)
            | BytecodeType::TraitObject(..) => refs.push(offset),

            BytecodeType::Ref(..) => {
                // Ref points to local stack memory, not GC-managed heap objects.
            }

            BytecodeType::TypeAlias(..)
            | BytecodeType::Assoc { .. }
            | BytecodeType::TypeParam(..)
            | BytecodeType::This => {
                unreachable!()
            }
        }
    }

    pub(crate) fn array_shape_size(&self, element_ty: &BytecodeType) -> InstanceSize {
        match element_ty {
            BytecodeType::Unit => InstanceSize::UnitArray,
            BytecodeType::Ptr
            | BytecodeType::Class(..)
            | BytecodeType::TraitObject(..)
            | BytecodeType::Lambda(..) => InstanceSize::ObjArray,

            BytecodeType::Tuple(subtypes) => {
                let tuple = self.tuple_layout(subtypes.clone());
                InstanceSize::StructArray(tuple.size)
            }

            BytecodeType::Struct(struct_id, type_params) => {
                let struct_ = self.struct_layout(*struct_id, type_params);
                InstanceSize::StructArray(struct_.size)
            }

            BytecodeType::Enum(enum_id, type_params) => {
                match self.enum_layout(*enum_id, type_params) {
                    AotEnumLayout::Int => InstanceSize::PrimitiveArray(4),
                    AotEnumLayout::Ptr | AotEnumLayout::Tagged => InstanceSize::ObjArray,
                }
            }

            BytecodeType::Bool
            | BytecodeType::UInt8
            | BytecodeType::Char
            | BytecodeType::Int32
            | BytecodeType::Int64
            | BytecodeType::Float32
            | BytecodeType::Float64
            | BytecodeType::Address => InstanceSize::PrimitiveArray(self.size(element_ty.clone())),

            BytecodeType::TypeAlias(..)
            | BytecodeType::Assoc { .. }
            | BytecodeType::TypeParam(_)
            | BytecodeType::This
            | BytecodeType::Ref(..) => {
                unreachable!()
            }
        }
    }

    fn tuple_layout(&self, subtypes: BytecodeTypeArray) -> AotRecordLayout {
        if let Some(layout) = self.tuples.borrow().get(&subtypes) {
            return layout.clone();
        }

        let layout = self.compute_tuple_layout(&subtypes);
        let mut tuples = self.tuples.borrow_mut();
        tuples
            .entry(subtypes)
            .or_insert_with(|| layout.clone())
            .clone()
    }

    fn compute_tuple_layout(&self, subtypes: &BytecodeTypeArray) -> AotRecordLayout {
        let mut total_size = 0;
        let mut references = Vec::new();
        let mut total_align = 0;

        for ty in subtypes.iter() {
            assert!(ty.is_concrete_type());

            let element_size = self.size(ty.clone());
            let element_align = self.align(ty.clone());
            let element_offset = mem::align_i32(total_size, element_align);

            self.add_ref_fields(&mut references, element_offset, ty.clone());

            total_size = element_offset + element_size;
            total_align = max(total_align, element_align);
        }

        AotRecordLayout {
            size: mem::align_i32(total_size, total_align),
            align: total_align,
            refs: references,
        }
    }

    fn struct_layout(
        &self,
        struct_id: StructId,
        type_params: &BytecodeTypeArray,
    ) -> AotRecordLayout {
        let key = (struct_id, type_params.clone());

        if let Some(layout) = self.structs.borrow().get(&key) {
            return layout.clone();
        }

        let layout = self.compute_struct_layout(struct_id, type_params);
        let mut structs = self.structs.borrow_mut();
        structs.entry(key).or_insert_with(|| layout.clone()).clone()
    }

    fn compute_struct_layout(
        &self,
        struct_id: StructId,
        type_params: &BytecodeTypeArray,
    ) -> AotRecordLayout {
        let struct_ = self.program.struct_(struct_id);
        let mut struct_size = 0;
        let mut struct_align = 0;
        let mut ref_fields = Vec::new();

        for field in &struct_.fields {
            let ty = specialize_ty_in_program(self.program, None, field.ty.clone(), type_params);
            debug_assert!(ty.is_concrete_type());

            let field_size = self.size(ty.clone());
            let field_align = self.align(ty.clone());
            let offset = mem::align_i32(struct_size, field_align);

            struct_size = offset + field_size;
            struct_align = max(struct_align, field_align);

            self.add_ref_fields(&mut ref_fields, offset, ty);
        }

        AotRecordLayout {
            size: mem::align_i32(struct_size, struct_align),
            align: struct_align,
            refs: ref_fields,
        }
    }

    fn enum_layout(&self, enum_id: EnumId, type_params: &BytecodeTypeArray) -> AotEnumLayout {
        let key = (enum_id, type_params.clone());

        if let Some(&layout) = self.enums.borrow().get(&key) {
            return layout;
        }

        let layout = self.compute_enum_layout(enum_id, type_params);
        let mut enums = self.enums.borrow_mut();
        *enums.entry(key).or_insert(layout)
    }

    fn compute_enum_layout(
        &self,
        enum_id: EnumId,
        type_params: &BytecodeTypeArray,
    ) -> AotEnumLayout {
        let enum_ = self.program.enum_(enum_id);

        if enum_is_simple_integer(enum_) {
            AotEnumLayout::Int
        } else if enum_is_ptr(enum_, type_params) {
            AotEnumLayout::Ptr
        } else {
            AotEnumLayout::Tagged
        }
    }
}

fn enum_is_simple_integer(enum_: &EnumData) -> bool {
    enum_
        .variants
        .iter()
        .all(|variant| variant.arguments.is_empty())
}

fn enum_is_ptr(enum_: &EnumData, type_params: &BytecodeTypeArray) -> bool {
    if enum_.variants.len() != 2 {
        return false;
    }

    let variant1 = enum_.variants.first().unwrap();
    let variant2 = enum_.variants.last().unwrap();

    let (none_variant, some_variant) = if variant1.arguments.is_empty() {
        (variant1, variant2)
    } else {
        (variant2, variant1)
    };

    none_variant.arguments.is_empty()
        && some_variant.arguments.len() == 1
        && specialize_bty(some_variant.arguments.first().unwrap().clone(), type_params)
            .is_reference_type()
}
