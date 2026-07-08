use std::mem::offset_of;
use std::slice;

use crate::gc::Address;
use crate::mem;
use crate::runtime::ShapeKind;
use dora_compiler::wire::{ByteReader, decode_bytecode_type, decode_bytecode_type_array};
use dora_compiler::{
    AOT_SHAPE_KIND_ARRAY, AOT_SHAPE_KIND_CLASS, AOT_SHAPE_KIND_CODE, AOT_SHAPE_KIND_ENUM_VARIANT,
    AOT_SHAPE_KIND_FILLER_ARRAY, AOT_SHAPE_KIND_FILLER_WORD, AOT_SHAPE_KIND_FREE_SPACE,
    AOT_SHAPE_KIND_LAMBDA, AOT_SHAPE_KIND_STRING, AOT_SHAPE_KIND_TRAIT_OBJECT,
    AOT_SHAPE_REFS_BITMAP_MAX_WORD, AOT_SHAPE_REFS_BITMAP_TAG, AOT_SHAPE_VISITOR_INVALID,
    AOT_SHAPE_VISITOR_NONE, AOT_SHAPE_VISITOR_POINTER_ARRAY, AOT_SHAPE_VISITOR_RECORD_ARRAY,
    AOT_SHAPE_VISITOR_REGULAR, ShapeVisitor,
};

#[derive(Debug)]
// Runtime view of descriptors emitted into .dora.shapes. Keep in sync with
// dora_compiler::abi::ShapeLayout and assembly.rs::write_shape_metadata.
#[repr(C)]
pub struct Shape {
    visitor: usize,
    refs_data: *const i32,
    refs_len: usize,
    instance_size: usize,
    element_size: usize,
    vtable_length: usize,
    kind_data: *const u8,
    kind_len: usize,
}

impl Shape {
    pub fn address(&self) -> Address {
        Address::from_ptr(self as *const _)
    }

    pub fn kind(&self) -> ShapeKind {
        decode_shape_kind(self.kind_data())
    }

    pub fn instance_size(&self) -> usize {
        self.instance_size
    }

    pub fn element_size(&self) -> usize {
        self.element_size
    }

    pub fn visitor(&self) -> ShapeVisitor {
        decode_shape_visitor(self.visitor)
    }

    pub fn refs(&self) -> &[i32] {
        debug_assert!(!self.has_refs_bitmap());

        if self.refs_len == 0 {
            &[]
        } else {
            unsafe { slice::from_raw_parts(self.refs_data, self.refs_len) }
        }
    }

    pub fn has_reference_offsets(&self) -> bool {
        match self.refs_bitmap() {
            Some(bitmap) => bitmap != 0,
            None => self.refs_len > 0,
        }
    }

    fn refs_bitmap(&self) -> Option<usize> {
        let refs_data = self.refs_data as usize;
        if refs_data & AOT_SHAPE_REFS_BITMAP_TAG != 0 {
            Some(refs_data >> 1)
        } else {
            None
        }
    }

    pub fn visit_reference_offsets<F>(&self, mut f: F)
    where
        F: FnMut(usize),
    {
        if let Some(bitmap) = self.refs_bitmap() {
            visit_bitmap_reference_offsets(bitmap, f);
        } else {
            for &offset in self.refs() {
                debug_assert!(offset >= 0, "shape reference offset must be non-negative");
                debug_assert_eq!(
                    offset as usize % mem::ptr_width_usize(),
                    0,
                    "shape reference offset must be pointer-aligned"
                );
                f(offset as usize);
            }
        }
    }

    fn has_refs_bitmap(&self) -> bool {
        self.refs_bitmap().is_some()
    }

    pub fn table(&self) -> &[usize] {
        unsafe { slice::from_raw_parts(self.table_ptr(), self.vtable_length) }
    }

    fn table_ptr(&self) -> *const usize {
        let address = Address::from_ptr(self as *const _);
        address.offset(Shape::offset_of_vtable() as usize).to_ptr()
    }

    pub const fn offset_of_vtable() -> i32 {
        std::mem::size_of::<Shape>() as i32
    }

    fn kind_data(&self) -> &[u8] {
        if self.kind_len == 0 {
            &[]
        } else {
            unsafe { slice::from_raw_parts(self.kind_data, self.kind_len) }
        }
    }
}

fn visit_bitmap_reference_offsets<F>(bitmap: usize, mut f: F)
where
    F: FnMut(usize),
{
    let mut bits = bitmap;
    while bits != 0 {
        let word = bits.trailing_zeros() as usize;
        debug_assert!(word <= AOT_SHAPE_REFS_BITMAP_MAX_WORD);
        f(word * mem::ptr_width_usize());
        bits &= bits - 1;
    }
}

const _: [(); Shape::offset_of_vtable() as usize] =
    [(); dora_compiler::Shape::offset_of_vtable() as usize];
const _: [(); offset_of!(Shape, instance_size)] =
    [(); dora_compiler::Shape::offset_of_instance_size()];
const _: [(); offset_of!(Shape, element_size)] =
    [(); dora_compiler::Shape::offset_of_element_size()];
const _: [(); offset_of!(Shape, refs_data)] = [(); dora_compiler::Shape::offset_of_refs_data()];
const _: [(); offset_of!(Shape, refs_len)] = [(); dora_compiler::Shape::offset_of_refs_len()];
const _: [(); offset_of!(Shape, visitor)] = [(); dora_compiler::Shape::offset_of_visitor()];
const _: [(); offset_of!(Shape, vtable_length)] =
    [(); dora_compiler::Shape::offset_of_vtable_length()];
const _: [(); offset_of!(Shape, kind_data)] = [(); dora_compiler::Shape::offset_of_kind_data()];
const _: [(); offset_of!(Shape, kind_len)] = [(); dora_compiler::Shape::offset_of_kind_len()];

fn decode_shape_kind(bytes: &[u8]) -> ShapeKind {
    let mut reader = ByteReader::new(bytes.to_vec());
    let kind = match reader.read_u8() {
        AOT_SHAPE_KIND_FILLER_WORD => ShapeKind::FillerWord,
        AOT_SHAPE_KIND_STRING => ShapeKind::String,
        AOT_SHAPE_KIND_CLASS => {
            let class_id = (reader.read_u32() as usize).into();
            let type_params = decode_bytecode_type_array(&mut reader);
            ShapeKind::Class(class_id, type_params)
        }
        AOT_SHAPE_KIND_ARRAY => {
            let class_id = (reader.read_u32() as usize).into();
            let type_params = decode_bytecode_type_array(&mut reader);
            ShapeKind::Array(class_id, type_params)
        }
        AOT_SHAPE_KIND_ENUM_VARIANT => {
            let enum_id = (reader.read_u32() as usize).into();
            let type_params = decode_bytecode_type_array(&mut reader);
            let variant_id = reader.read_u32();
            ShapeKind::EnumVariant(enum_id, type_params, variant_id)
        }
        AOT_SHAPE_KIND_LAMBDA => {
            let fct_id = (reader.read_u32() as usize).into();
            let type_params = decode_bytecode_type_array(&mut reader);
            ShapeKind::Lambda(fct_id, type_params)
        }
        AOT_SHAPE_KIND_TRAIT_OBJECT => {
            let trait_ty = decode_bytecode_type(&mut reader);
            let actual_object_ty = decode_bytecode_type(&mut reader);
            ShapeKind::TraitObject {
                trait_ty,
                actual_object_ty,
            }
        }
        AOT_SHAPE_KIND_FILLER_ARRAY => ShapeKind::FillerArray,
        AOT_SHAPE_KIND_FREE_SPACE => ShapeKind::FreeSpace,
        AOT_SHAPE_KIND_CODE => ShapeKind::Code,
        value => panic!("invalid AOT shape kind {}", value),
    };

    assert!(
        !reader.has_more(),
        "encoded AOT shape kind has trailing bytes"
    );
    kind
}

fn decode_shape_visitor(visitor: usize) -> ShapeVisitor {
    match visitor {
        AOT_SHAPE_VISITOR_REGULAR => ShapeVisitor::Regular,
        AOT_SHAPE_VISITOR_POINTER_ARRAY => ShapeVisitor::PointerArray,
        AOT_SHAPE_VISITOR_RECORD_ARRAY => ShapeVisitor::RecordArray,
        AOT_SHAPE_VISITOR_NONE => ShapeVisitor::None,
        AOT_SHAPE_VISITOR_INVALID => ShapeVisitor::Invalid,
        _ => panic!("invalid shape visitor {}", visitor),
    }
}
