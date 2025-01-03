use crate::sema::{
    ClassDefinitionId, EnumDefinitionId, FctDefinitionId, StructDefinitionId, TraitDefinitionId,
};
use crate::ty::{SourceType, SourceTypeArray};

#[derive(Debug)]
pub struct KnownElements {
    pub classes: KnownClasses,
    pub traits: KnownTraits,
    pub functions: KnownFunctions,
    pub enums: KnownEnums,
    pub structs: KnownStructs,
}

impl KnownElements {
    pub fn new() -> KnownElements {
        KnownElements {
            classes: KnownClasses::new(),
            functions: KnownFunctions::new(),
            traits: KnownTraits::new(),
            enums: KnownEnums::new(),
            structs: KnownStructs::new(),
        }
    }
}

#[derive(Debug)]
pub struct KnownEnums {
    pub option: Option<EnumDefinitionId>,
    pub ordering: Option<EnumDefinitionId>,
}

impl KnownEnums {
    pub fn new() -> KnownEnums {
        KnownEnums {
            option: None,
            ordering: None,
        }
    }

    pub fn option(&self) -> EnumDefinitionId {
        self.option.expect("uninitialized")
    }

    pub fn ordering(&self) -> EnumDefinitionId {
        self.ordering.expect("uninitialized")
    }
}

#[derive(Debug)]
pub struct KnownClasses {
    pub atomic_int32: Option<ClassDefinitionId>,
    pub atomic_int64: Option<ClassDefinitionId>,
    pub array: Option<ClassDefinitionId>,
    pub string: Option<ClassDefinitionId>,
    pub string_buffer: Option<ClassDefinitionId>,
    pub stacktrace: Option<ClassDefinitionId>,
    pub stacktrace_element: Option<ClassDefinitionId>,
    pub thread: Option<ClassDefinitionId>,
    pub lambda: Option<ClassDefinitionId>,
}

impl KnownClasses {
    pub fn new() -> KnownClasses {
        KnownClasses {
            atomic_int32: None,
            atomic_int64: None,
            array: None,
            string: None,
            string_buffer: None,
            stacktrace: None,
            stacktrace_element: None,
            thread: None,
            lambda: None,
        }
    }

    pub fn atomic_int32(&self) -> ClassDefinitionId {
        self.atomic_int32.expect("uninitialized")
    }

    pub fn atomic_int64(&self) -> ClassDefinitionId {
        self.atomic_int64.expect("uninitialized")
    }

    pub fn array(&self) -> ClassDefinitionId {
        self.array.expect("uninitialized")
    }

    pub fn string(&self) -> ClassDefinitionId {
        self.string.expect("uninitialized")
    }

    pub fn string_buffer(&self) -> ClassDefinitionId {
        self.string_buffer.expect("uninitialized")
    }

    pub fn stacktrace(&self) -> ClassDefinitionId {
        self.stacktrace.expect("uninitialized")
    }

    pub fn stacktrace_element(&self) -> ClassDefinitionId {
        self.stacktrace_element.expect("uninitialized")
    }

    pub fn thread(&self) -> ClassDefinitionId {
        self.thread.expect("uninitialized")
    }

    pub fn lambda(&self) -> ClassDefinitionId {
        self.lambda.expect("uninitialized")
    }
}

#[derive(Debug)]
pub struct KnownStructs {
    pub bool: Option<StructDefinitionId>,
    pub uint8: Option<StructDefinitionId>,
    pub char: Option<StructDefinitionId>,
    pub int32: Option<StructDefinitionId>,
    pub int64: Option<StructDefinitionId>,
    pub float32: Option<StructDefinitionId>,
    pub float64: Option<StructDefinitionId>,
}

impl KnownStructs {
    pub fn new() -> KnownStructs {
        KnownStructs {
            bool: None,
            uint8: None,
            char: None,
            int32: None,
            int64: None,
            float32: None,
            float64: None,
        }
    }

    pub fn bool(&self) -> StructDefinitionId {
        self.bool.expect("uninitialized")
    }

    pub fn uint8(&self) -> StructDefinitionId {
        self.uint8.expect("uninitialized")
    }

    pub fn char(&self) -> StructDefinitionId {
        self.char.expect("uninitialized")
    }

    pub fn int32(&self) -> StructDefinitionId {
        self.int32.expect("uninitialized")
    }

    pub fn int64(&self) -> StructDefinitionId {
        self.int64.expect("uninitialized")
    }

    pub fn float32(&self) -> StructDefinitionId {
        self.float32.expect("uninitialized")
    }

    pub fn float64(&self) -> StructDefinitionId {
        self.float64.expect("uninitialized")
    }
}

#[derive(Debug)]
pub struct KnownTraits {
    pub add: Option<TraitDefinitionId>,
    pub sar: Option<TraitDefinitionId>,
    pub bit_and: Option<TraitDefinitionId>,
    pub bit_or: Option<TraitDefinitionId>,
    pub bit_xor: Option<TraitDefinitionId>,
    pub comparable: Option<TraitDefinitionId>,
    pub div: Option<TraitDefinitionId>,
    pub equals: Option<TraitDefinitionId>,
    pub into_iterator: Option<TraitDefinitionId>,
    pub iterator: Option<TraitDefinitionId>,
    pub shr: Option<TraitDefinitionId>,
    pub mod_: Option<TraitDefinitionId>,
    pub mul: Option<TraitDefinitionId>,
    pub neg: Option<TraitDefinitionId>,
    pub not: Option<TraitDefinitionId>,
    pub shl: Option<TraitDefinitionId>,
    pub stringable: Option<TraitDefinitionId>,
    pub sub: Option<TraitDefinitionId>,
    pub zero: Option<TraitDefinitionId>,
    pub index_get: Option<TraitDefinitionId>,
    pub index_set: Option<TraitDefinitionId>,
}

impl KnownTraits {
    pub fn new() -> KnownTraits {
        KnownTraits {
            add: None,
            sar: None,
            bit_and: None,
            bit_or: None,
            bit_xor: None,
            comparable: None,
            div: None,
            equals: None,
            into_iterator: None,
            iterator: None,
            shr: None,
            mod_: None,
            mul: None,
            neg: None,
            not: None,
            shl: None,
            stringable: None,
            sub: None,
            zero: None,
            index_get: None,
            index_set: None,
        }
    }

    pub fn add(&self) -> TraitDefinitionId {
        self.add.expect("uninitialized")
    }

    pub fn sar(&self) -> TraitDefinitionId {
        self.sar.expect("uninitialized")
    }

    pub fn bit_and(&self) -> TraitDefinitionId {
        self.bit_and.expect("uninitialized")
    }

    pub fn bit_or(&self) -> TraitDefinitionId {
        self.bit_or.expect("uninitialized")
    }

    pub fn bit_xor(&self) -> TraitDefinitionId {
        self.bit_xor.expect("uninitialized")
    }

    pub fn comparable(&self) -> TraitDefinitionId {
        self.comparable.expect("uninitialized")
    }

    pub fn div(&self) -> TraitDefinitionId {
        self.div.expect("uninitialized")
    }

    pub fn equals(&self) -> TraitDefinitionId {
        self.equals.expect("uninitialized")
    }

    pub fn shr(&self) -> TraitDefinitionId {
        self.shr.expect("uninitialized")
    }

    pub fn mod_(&self) -> TraitDefinitionId {
        self.mod_.expect("uninitialized")
    }

    pub fn mul(&self) -> TraitDefinitionId {
        self.mul.expect("uninitialized")
    }

    pub fn neg(&self) -> TraitDefinitionId {
        self.neg.expect("uninitialized")
    }

    pub fn not(&self) -> TraitDefinitionId {
        self.not.expect("uninitialized")
    }

    pub fn shl(&self) -> TraitDefinitionId {
        self.shl.expect("uninitialized")
    }

    pub fn stringable(&self) -> TraitDefinitionId {
        self.stringable.expect("uninitialized")
    }

    pub fn sub(&self) -> TraitDefinitionId {
        self.sub.expect("uninitialized")
    }

    pub fn zero(&self) -> TraitDefinitionId {
        self.zero.expect("uninitialized")
    }

    pub fn index_get(&self) -> TraitDefinitionId {
        self.index_get.expect("uninitialized")
    }

    pub fn index_set(&self) -> TraitDefinitionId {
        self.index_set.expect("uninitialized")
    }

    pub fn iterator(&self) -> TraitDefinitionId {
        self.iterator.expect("uninitialized")
    }

    pub fn into_iterator(&self) -> TraitDefinitionId {
        self.into_iterator.expect("uninitialized")
    }
}

#[derive(Debug)]
pub struct KnownFunctions {
    pub string_equals: Option<FctDefinitionId>,
    pub string_buffer_empty: Option<FctDefinitionId>,
    pub string_buffer_append: Option<FctDefinitionId>,
    pub string_buffer_to_string: Option<FctDefinitionId>,
    pub assert: Option<FctDefinitionId>,
    pub unreachable: Option<FctDefinitionId>,
    pub fatal_error: Option<FctDefinitionId>,
    pub option_is_some: Option<FctDefinitionId>,
    pub option_is_none: Option<FctDefinitionId>,
    pub option_unwrap: Option<FctDefinitionId>,
    pub ordering_is_ge: Option<FctDefinitionId>,
    pub ordering_is_gt: Option<FctDefinitionId>,
    pub ordering_is_le: Option<FctDefinitionId>,
    pub ordering_is_lt: Option<FctDefinitionId>,
}

impl KnownFunctions {
    pub fn new() -> KnownFunctions {
        KnownFunctions {
            string_equals: None,
            string_buffer_empty: None,
            string_buffer_append: None,
            string_buffer_to_string: None,
            assert: None,
            unreachable: None,
            fatal_error: None,
            option_is_none: None,
            option_is_some: None,
            option_unwrap: None,
            ordering_is_ge: None,
            ordering_is_gt: None,
            ordering_is_le: None,
            ordering_is_lt: None,
        }
    }

    pub fn string_equals(&self) -> FctDefinitionId {
        self.string_equals.expect("uninitialized")
    }

    pub fn string_buffer_empty(&self) -> FctDefinitionId {
        self.string_buffer_empty.expect("uninitialized")
    }

    pub fn string_buffer_append(&self) -> FctDefinitionId {
        self.string_buffer_append.expect("uninitialized")
    }

    pub fn string_buffer_to_string(&self) -> FctDefinitionId {
        self.string_buffer_to_string.expect("uninitialized")
    }

    pub fn assert(&self) -> FctDefinitionId {
        self.assert.expect("uninitialized")
    }

    pub fn unreachable(&self) -> FctDefinitionId {
        self.unreachable.expect("uninitialized")
    }

    pub fn fatal_error(&self) -> FctDefinitionId {
        self.fatal_error.expect("uninitialized")
    }

    pub fn option_is_some(&self) -> FctDefinitionId {
        self.option_is_some.expect("uninitialized")
    }

    pub fn option_is_none(&self) -> FctDefinitionId {
        self.option_is_none.expect("uninitialized")
    }

    pub fn option_unwrap(&self) -> FctDefinitionId {
        self.option_unwrap.expect("uninitialized")
    }

    pub fn ordering_is_ge(&self) -> FctDefinitionId {
        self.ordering_is_ge.expect("uninitialized")
    }

    pub fn ordering_is_gt(&self) -> FctDefinitionId {
        self.ordering_is_gt.expect("uninitialized")
    }

    pub fn ordering_is_le(&self) -> FctDefinitionId {
        self.ordering_is_le.expect("uninitialized")
    }

    pub fn ordering_is_lt(&self) -> FctDefinitionId {
        self.ordering_is_lt.expect("uninitialized")
    }
}

impl KnownElements {
    pub fn array_ty(&self, element: SourceType) -> SourceType {
        SourceType::Class(self.classes.array(), SourceTypeArray::single(element))
    }
}
