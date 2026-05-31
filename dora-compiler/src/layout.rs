use std::cell::RefCell;
use std::cmp::max;
use std::collections::HashMap;

use dora_bytecode::{
    BytecodeType, BytecodeTypeArray, ClassId, EnumData, EnumId, FunctionId, Program, StructId,
    resolve_path,
};

use crate::{AotShapeKey, specialize_bty, specialize_ty_in_program};

#[inline(always)]
pub const fn ptr_width() -> i32 {
    std::mem::size_of::<*const u8>() as i32
}

#[inline(always)]
pub const fn align_i32(value: i32, align: i32) -> i32 {
    if align == 0 {
        return value;
    }

    ((value + align - 1) / align) * align
}

#[inline(always)]
pub const fn align_usize_up(value: usize, align: usize) -> usize {
    if align == 0 {
        return value;
    }

    ((value + align - 1) / align) * align
}

#[inline(always)]
pub const fn object_header_size() -> i32 {
    std::mem::size_of::<usize>() as i32
}

#[inline(always)]
pub const fn array_header_size() -> i32 {
    object_header_size() + ptr_width()
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MachineMode {
    Int8,
    Int32,
    Int64,
    IntPtr,
    Float32,
    Float64,
    Ptr,
}

impl MachineMode {
    pub fn size(self) -> i32 {
        match self {
            MachineMode::Int8 => 1,
            MachineMode::Int32 => 4,
            MachineMode::Int64 => 8,
            MachineMode::IntPtr | MachineMode::Ptr => ptr_width(),
            MachineMode::Float32 => 4,
            MachineMode::Float64 => 8,
        }
    }

    pub fn is_int8(self) -> bool {
        match self {
            MachineMode::Int8 => true,
            _ => false,
        }
    }

    pub fn is_float(self) -> bool {
        match self {
            MachineMode::Float32 | MachineMode::Float64 => true,
            _ => false,
        }
    }

    pub fn is64(self) -> bool {
        match self {
            MachineMode::Int8 | MachineMode::Int32 => false,
            MachineMode::Int64 | MachineMode::Ptr => true,
            _ => unreachable!(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum InstanceSize {
    Fixed(i32),
    PrimitiveArray(i32),
    ObjArray,
    UnitArray,
    StructArray(i32),
    FillerWord,
    FillerArray,
    FreeSpace,
    CodeObject,
    Str,
}

impl InstanceSize {
    pub fn instance_size(&self) -> Option<i32> {
        match self {
            InstanceSize::PrimitiveArray(_) => None,
            InstanceSize::ObjArray => None,
            InstanceSize::Str => None,
            InstanceSize::Fixed(value) => Some(*value),
            InstanceSize::FillerWord => Some(ptr_width()),
            InstanceSize::FillerArray | InstanceSize::FreeSpace => None,
            InstanceSize::StructArray(_) => None,
            InstanceSize::UnitArray => Some(array_header_size()),
            InstanceSize::CodeObject => None,
        }
    }

    pub fn element_size(&self) -> Option<i32> {
        match self {
            InstanceSize::PrimitiveArray(esize) => Some(*esize),
            InstanceSize::ObjArray => Some(ptr_width()),
            InstanceSize::Str => Some(1),
            InstanceSize::Fixed(_) | InstanceSize::FillerWord => None,
            InstanceSize::FillerArray | InstanceSize::FreeSpace => Some(ptr_width()),
            InstanceSize::StructArray(esize) => Some(*esize),
            InstanceSize::UnitArray => Some(0),
            InstanceSize::CodeObject => Some(ptr_width()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FieldInstance {
    pub offset: i32,
    pub ty: BytecodeType,
}

pub struct AotLayout<'a> {
    program: &'a Program,
    array_class_id: ClassId,
    string_class_id: ClassId,
    classes: RefCell<HashMap<(ClassId, BytecodeTypeArray), AotRecordLayout>>,
    lambdas: RefCell<HashMap<(FunctionId, BytecodeTypeArray), AotRecordLayout>>,
    tuples: RefCell<HashMap<BytecodeTypeArray, AotRecordLayout>>,
    structs: RefCell<HashMap<(StructId, BytecodeTypeArray), AotRecordLayout>>,
    enums: RefCell<HashMap<(EnumId, BytecodeTypeArray), AotEnumLayout>>,
    enum_variants: RefCell<HashMap<(EnumId, BytecodeTypeArray, u32), AotRecordLayout>>,
}

#[derive(Clone)]
pub struct AotRecordLayout {
    pub size: i32,
    pub align: i32,
    pub refs: Vec<i32>,
    pub fields: Vec<FieldInstance>,
}

#[derive(Clone, Copy)]
pub enum AotEnumLayout {
    Int,
    Ptr,
    Tagged,
}

impl<'a> AotLayout<'a> {
    pub fn new(program: &'a Program) -> AotLayout<'a> {
        let array_class_id = resolve_path(program, "std::collections::Array")
            .expect("'std::collections::Array' not found")
            .class_id()
            .expect("class expected");
        let string_class_id = resolve_path(program, "std::string::String")
            .expect("'std::string::String' not found")
            .class_id()
            .expect("class expected");

        AotLayout {
            program,
            array_class_id,
            string_class_id,
            classes: RefCell::new(HashMap::new()),
            lambdas: RefCell::new(HashMap::new()),
            tuples: RefCell::new(HashMap::new()),
            structs: RefCell::new(HashMap::new()),
            enums: RefCell::new(HashMap::new()),
            enum_variants: RefCell::new(HashMap::new()),
        }
    }

    pub fn size(&self, ty: BytecodeType) -> i32 {
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
            | BytecodeType::Ref(..) => ptr_width(),
            BytecodeType::Tuple(subtypes) => self.tuple_layout(subtypes).size,
            BytecodeType::Enum(enum_id, type_params) => {
                match self.enum_layout(enum_id, &type_params) {
                    AotEnumLayout::Int => 4,
                    AotEnumLayout::Ptr | AotEnumLayout::Tagged => ptr_width(),
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

    pub fn mode(&self, ty: BytecodeType) -> MachineMode {
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

    pub fn align(&self, ty: BytecodeType) -> i32 {
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
            | BytecodeType::Ref(..) => ptr_width(),
            BytecodeType::Tuple(subtypes) => self.tuple_layout(subtypes).align,
            BytecodeType::Enum(enum_id, type_params) => {
                match self.enum_layout(enum_id, &type_params) {
                    AotEnumLayout::Int => 4,
                    AotEnumLayout::Ptr | AotEnumLayout::Tagged => ptr_width(),
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

    pub fn add_ref_fields(&self, refs: &mut Vec<i32>, offset: i32, ty: BytecodeType) {
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

    pub fn array_shape_size(&self, element_ty: &BytecodeType) -> InstanceSize {
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

    pub fn class_instance_size(&self, class_id: ClassId, type_params: &BytecodeTypeArray) -> u32 {
        self.class_instance_size_kind(class_id, type_params)
            .instance_size()
            .unwrap_or(0) as u32
    }

    pub fn class_element_size(&self, class_id: ClassId, type_params: &BytecodeTypeArray) -> u32 {
        self.class_instance_size_kind(class_id, type_params)
            .element_size()
            .unwrap_or(-1) as u32
    }

    fn class_instance_size_kind(
        &self,
        class_id: ClassId,
        type_params: &BytecodeTypeArray,
    ) -> InstanceSize {
        if class_id == self.array_class_id {
            assert_eq!(type_params.len(), 1);
            return self.array_shape_size(&type_params[0]);
        }

        if class_id == self.string_class_id {
            return InstanceSize::Str;
        }

        InstanceSize::Fixed(self.class_layout(class_id, type_params).size)
    }

    pub fn class_field_offset(
        &self,
        class_id: ClassId,
        type_params: &BytecodeTypeArray,
        field_id: u32,
    ) -> u32 {
        let class = self.class_layout(class_id, type_params);
        class.fields[field_id as usize]
            .offset
            .try_into()
            .expect("overflow")
    }

    pub fn trait_object_size(&self, actual_object_ty: BytecodeType) -> i32 {
        debug_assert!(actual_object_ty.is_concrete_type());

        let field_size = self.size(actual_object_ty.clone());
        let field_align = self.align(actual_object_ty);
        let offset = align_i32(object_header_size(), field_align);
        align_i32(offset + field_size, ptr_width())
    }

    pub fn enum_variant_size(
        &self,
        enum_id: EnumId,
        type_params: &BytecodeTypeArray,
        variant_id: u32,
    ) -> u32 {
        self.enum_variant_layout(enum_id, type_params, variant_id)
            .size as u32
    }

    pub fn enum_variant_field_offset(
        &self,
        enum_id: EnumId,
        type_params: &BytecodeTypeArray,
        variant_id: u32,
        field_id: u32,
    ) -> i32 {
        let enum_ = self.program.enum_(enum_id);
        let variant = &enum_.variants[variant_id as usize];
        let units = variant.arguments[0..field_id as usize]
            .iter()
            .filter(|ty| ty.is_unit())
            .count() as u32;
        let field_id = 1 + field_id - units;
        let layout = self.enum_variant_layout(enum_id, type_params, variant_id);

        layout.fields[field_id as usize].offset
    }

    pub fn class_layout(
        &self,
        class_id: ClassId,
        type_params: &BytecodeTypeArray,
    ) -> AotRecordLayout {
        let key = (class_id, type_params.clone());

        if let Some(layout) = self.classes.borrow().get(&key) {
            return layout.clone();
        }

        let layout = self.compute_class_layout(class_id, type_params);
        let mut classes = self.classes.borrow_mut();
        classes.entry(key).or_insert_with(|| layout.clone()).clone()
    }

    pub fn class_shape_key(
        &self,
        class_id: ClassId,
        type_params: BytecodeTypeArray,
    ) -> AotShapeKey {
        if class_id == self.array_class_id {
            return AotShapeKey::Array(class_id, type_params);
        }

        if class_id == self.string_class_id {
            return AotShapeKey::String;
        }

        AotShapeKey::Class(class_id, type_params)
    }

    pub fn lambda_layout(
        &self,
        fct_id: FunctionId,
        type_params: &BytecodeTypeArray,
    ) -> AotRecordLayout {
        let key = (fct_id, type_params.clone());

        if let Some(layout) = self.lambdas.borrow().get(&key) {
            return layout.clone();
        }

        let layout = self.compute_lambda_layout(type_params);
        let mut lambdas = self.lambdas.borrow_mut();
        lambdas.entry(key).or_insert_with(|| layout.clone()).clone()
    }

    fn compute_lambda_layout(&self, type_params: &BytecodeTypeArray) -> AotRecordLayout {
        debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

        let context_offset = object_header_size();
        let size = align_i32(context_offset + ptr_width(), ptr_width());
        let fields = vec![FieldInstance {
            offset: context_offset,
            ty: BytecodeType::Ptr,
        }];

        AotRecordLayout {
            size,
            align: ptr_width(),
            refs: vec![context_offset],
            fields,
        }
    }

    fn compute_class_layout(
        &self,
        class_id: ClassId,
        type_params: &BytecodeTypeArray,
    ) -> AotRecordLayout {
        let class = self.program.class(class_id);
        let mut class_size = object_header_size();
        let mut fields = Vec::new();
        let mut refs = Vec::new();

        debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

        for field in &class.fields {
            let ty = specialize_ty_in_program(self.program, None, field.ty.clone(), type_params);
            debug_assert!(ty.is_concrete_type());

            let field_size = self.size(ty.clone());
            let field_align = self.align(ty.clone());
            let offset = align_i32(class_size, field_align);

            self.add_ref_fields(&mut refs, offset, ty.clone());
            fields.push(FieldInstance { offset, ty });
            class_size = offset + field_size;
        }

        AotRecordLayout {
            size: align_i32(class_size, ptr_width()),
            align: ptr_width(),
            refs,
            fields,
        }
    }

    pub fn tuple_layout(&self, subtypes: BytecodeTypeArray) -> AotRecordLayout {
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
        let mut fields = Vec::new();

        for ty in subtypes.iter() {
            assert!(ty.is_concrete_type());

            let element_size = self.size(ty.clone());
            let element_align = self.align(ty.clone());
            let element_offset = align_i32(total_size, element_align);

            self.add_ref_fields(&mut references, element_offset, ty.clone());
            fields.push(FieldInstance {
                offset: element_offset,
                ty,
            });

            total_size = element_offset + element_size;
            total_align = max(total_align, element_align);
        }

        AotRecordLayout {
            size: align_i32(total_size, total_align),
            align: total_align,
            refs: references,
            fields,
        }
    }

    pub fn struct_layout(
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
        let mut fields = Vec::new();

        for field in &struct_.fields {
            let ty = specialize_ty_in_program(self.program, None, field.ty.clone(), type_params);
            debug_assert!(ty.is_concrete_type());

            let field_size = self.size(ty.clone());
            let field_align = self.align(ty.clone());
            let offset = align_i32(struct_size, field_align);

            struct_size = offset + field_size;
            struct_align = max(struct_align, field_align);

            self.add_ref_fields(&mut ref_fields, offset, ty.clone());
            fields.push(FieldInstance { offset, ty });
        }

        AotRecordLayout {
            size: align_i32(struct_size, struct_align),
            align: struct_align,
            refs: ref_fields,
            fields,
        }
    }

    pub fn enum_layout(&self, enum_id: EnumId, type_params: &BytecodeTypeArray) -> AotEnumLayout {
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

    pub fn enum_variant_layout(
        &self,
        enum_id: EnumId,
        type_params: &BytecodeTypeArray,
        variant_id: u32,
    ) -> AotRecordLayout {
        let key = (enum_id, type_params.clone(), variant_id);

        if let Some(layout) = self.enum_variants.borrow().get(&key) {
            return layout.clone();
        }

        let layout = self.compute_enum_variant_layout(enum_id, type_params, variant_id);
        let mut enum_variants = self.enum_variants.borrow_mut();
        enum_variants
            .entry(key)
            .or_insert_with(|| layout.clone())
            .clone()
    }

    fn compute_enum_variant_layout(
        &self,
        enum_id: EnumId,
        type_params: &BytecodeTypeArray,
        variant_id: u32,
    ) -> AotRecordLayout {
        let enum_ = self.program.enum_(enum_id);
        let enum_variant = &enum_.variants[variant_id as usize];
        let mut size = object_header_size() + 4;
        let mut refs = Vec::new();
        let mut fields = vec![FieldInstance {
            offset: object_header_size(),
            ty: BytecodeType::Int32,
        }];

        for ty in &enum_variant.arguments {
            let ty = specialize_bty(ty.clone(), type_params);
            assert!(ty.is_concrete_type());

            let field_size = self.size(ty.clone());
            let field_align = self.align(ty.clone());
            let offset = align_i32(size, field_align);

            self.add_ref_fields(&mut refs, offset, ty.clone());
            fields.push(FieldInstance { offset, ty });
            size = offset + field_size;
        }

        AotRecordLayout {
            size: align_i32(size, ptr_width()),
            align: ptr_width(),
            refs,
            fields,
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
