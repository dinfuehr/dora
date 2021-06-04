use parking_lot::RwLock;
use std::sync::Arc;

use crate::gc::Address;
use crate::mem;
use crate::object::Header;
use crate::size::InstanceSize;
use crate::stack;
use crate::stdlib;
use crate::sym::{NestedSymTable, Sym};
use crate::ty::{SourceType, SourceTypeArray};
use crate::vm::{
    ClassDef, ClassDefId, ClassId, EnumId, ExtensionId, FctId, Intrinsic, ModuleId, NamespaceId,
    StructId, TraitId, VM,
};
use crate::vtable::VTableBox;

pub fn resolve_internal_classes(vm: &mut VM) {
    let stdlib = vm.stdlib_namespace_id;
    vm.known.structs.bool = internal_struct(vm, stdlib, "Bool", Some(SourceType::Bool));

    vm.known.structs.uint8 = internal_struct(vm, stdlib, "UInt8", Some(SourceType::UInt8));
    vm.known.structs.char = internal_struct(vm, stdlib, "Char", Some(SourceType::Char));
    vm.known.structs.int32 = internal_struct(vm, stdlib, "Int32", Some(SourceType::Int32));
    vm.known.structs.int64 = internal_struct(vm, stdlib, "Int64", Some(SourceType::Int64));

    vm.known.structs.float32 = internal_struct(vm, stdlib, "Float32", Some(SourceType::Float32));
    vm.known.structs.float64 = internal_struct(vm, stdlib, "Float64", Some(SourceType::Float64));

    vm.known.classes.object = Some(find_class(vm, stdlib, "Object"));
    vm.known.classes.string = Some(internal_class(vm, stdlib, "String"));

    vm.known.classes.string_buffer = Some(find_class(vm, stdlib, "StringBuffer"));

    vm.known.classes.atomic_int32 = Some(find_class(vm, stdlib, "AtomicInt32"));
    vm.known.classes.atomic_int64 = Some(find_class(vm, stdlib, "AtomicInt64"));

    let cls = vm.classes.idx(vm.known.classes.string());
    let mut cls = cls.write();
    cls.is_str = true;

    vm.known.classes.array = Some(internal_class(vm, stdlib, "Array"));

    let cls = vm.classes.idx(vm.known.classes.array());
    let mut cls = cls.write();
    cls.is_array = true;

    vm.known.classes.testing = Some(find_class(vm, stdlib, "Testing"));

    vm.known.classes.stacktrace = Some(find_class(vm, stdlib, "Stacktrace"));
    vm.known.classes.stacktrace_element = Some(find_class(vm, stdlib, "StacktraceElement"));

    vm.known.traits.stringable = find_trait(vm, stdlib, "Stringable");
    vm.known.traits.zero = find_trait(vm, stdlib, "Zero");
    vm.known.traits.iterator = find_trait(vm, stdlib, "Iterator");

    vm.known.enums.option = find_enum(vm, stdlib, "Option");

    internal_free_classes(vm);
}

pub fn fill_prelude(vm: &mut VM) {
    let symbols = [
        "Bool",
        "UInt8",
        "Char",
        "Int32",
        "Int64",
        "Float32",
        "Float64",
        "Object",
        "String",
        "Array",
        "Vec",
        "print",
        "println",
        "Option",
        "unimplemented",
        "unreachable",
        "assert",
        "Result",
    ];

    let stdlib = vm.stdlib_namespace();
    let stdlib = stdlib.read();

    let prelude = vm.prelude_namespace();
    let mut prelude = prelude.write();

    for sym in &symbols {
        let name = vm.interner.intern(sym);
        let sym = stdlib.get(name);

        let old_sym = prelude.insert(name, sym.expect("missing sym"));
        assert!(old_sym.is_none());
    }

    let stdlib_name = vm.interner.intern("std");
    prelude.insert(stdlib_name, Sym::Namespace(vm.stdlib_namespace_id));

    {
        // include None and Some from Option
        let option_name = vm.interner.intern("Option");
        let option_id = stdlib.get(option_name);

        match option_id {
            Some(Sym::Enum(enum_id)) => {
                let xenum = &vm.enums[enum_id];
                let xenum = xenum.read();

                for variant in &xenum.variants {
                    let old_sym = prelude.insert(variant.name, Sym::EnumValue(enum_id, variant.id));
                    assert!(old_sym.is_none());
                }
            }

            _ => unreachable!(),
        }
    }

    {
        // include Ok and Err from Result
        let option_name = vm.interner.intern("Result");
        let option_id = stdlib.get(option_name);

        match option_id {
            Some(Sym::Enum(enum_id)) => {
                let xenum = &vm.enums[enum_id];
                let xenum = xenum.read();

                for variant in &xenum.variants {
                    let old_sym = prelude.insert(variant.name, Sym::EnumValue(enum_id, variant.id));
                    assert!(old_sym.is_none());
                }
            }

            _ => unreachable!(),
        }
    }
}

pub fn discover_known_methods(vm: &mut VM) {
    let stdlib = vm.stdlib_namespace_id;
    vm.known.functions.string_buffer_empty = find_static(vm, stdlib, "StringBuffer", "empty");
    vm.known.functions.string_buffer_append = find_method(vm, stdlib, "StringBuffer", "append");
    vm.known.functions.string_buffer_to_string =
        find_method(vm, stdlib, "StringBuffer", "toString");
}

fn internal_free_classes(vm: &mut VM) {
    let free_object: ClassDefId;
    let free_array: ClassDefId;

    {
        let mut class_defs = vm.class_defs.lock();
        let next = class_defs.len();

        free_object = next.into();
        free_array = (next + 1).into();

        class_defs.push(Arc::new(ClassDef {
            id: free_object,
            cls_id: None,
            trait_object: None,
            type_params: SourceTypeArray::empty(),
            parent_id: None,
            size: InstanceSize::Fixed(Header::size()),
            fields: Vec::new(),
            ref_fields: Vec::new(),
            vtable: RwLock::new(None),
        }));

        class_defs.push(Arc::new(ClassDef {
            id: free_array,
            cls_id: None,
            trait_object: None,
            type_params: SourceTypeArray::empty(),
            parent_id: None,
            size: InstanceSize::FreeArray,
            fields: Vec::new(),
            ref_fields: Vec::new(),
            vtable: RwLock::new(None),
        }));

        {
            let free_object_class_def = &class_defs[free_object.to_usize()];
            let clsptr = Arc::as_ptr(free_object_class_def);
            let vtable = VTableBox::new(clsptr, Header::size() as usize, 0, &[]);
            *free_object_class_def.vtable.write() = Some(vtable);
        }

        {
            let free_array_class_def = &class_defs[free_array.to_usize()];
            let clsptr = Arc::as_ptr(free_array_class_def);
            let vtable = VTableBox::new(clsptr, 0, mem::ptr_width_usize(), &[]);
            *free_array_class_def.vtable.write() = Some(vtable);
        }
    }

    vm.known.free_object_class_def = free_object;
    vm.known.free_array_class_def = free_array;
}

fn find_class(vm: &VM, namespace_id: NamespaceId, name: &str) -> ClassId {
    let iname = vm.interner.intern(name);
    let symtable = NestedSymTable::new(vm, namespace_id);
    symtable.get_class(iname).expect("class not found")
}

fn internal_class(vm: &VM, namespace_id: NamespaceId, name: &str) -> ClassId {
    let iname = vm.interner.intern(name);
    let symtable = NestedSymTable::new(vm, namespace_id);
    let clsid = symtable.get_class(iname).expect("class not found");

    let cls = vm.classes.idx(clsid);
    let mut cls = cls.write();
    cls.internal_resolved = true;

    clsid
}

fn internal_struct(
    vm: &mut VM,
    namespace_id: NamespaceId,
    name: &str,
    ty: Option<SourceType>,
) -> StructId {
    let iname = vm.interner.intern(name);
    let symtable = NestedSymTable::new(vm, namespace_id);
    let struct_id = symtable.get_struct(iname).expect("struct not found");

    let xstruct = vm.structs.idx(struct_id);
    let mut xstruct = xstruct.write();

    assert!(xstruct.internal);
    xstruct.primitive_ty = ty;
    xstruct.internal_resolved = true;

    struct_id
}

fn find_trait(vm: &mut VM, namespace_id: NamespaceId, name: &str) -> TraitId {
    let iname = vm.interner.intern(name);
    let symtable = NestedSymTable::new(vm, namespace_id);
    symtable.get_trait(iname).expect("trait not found")
}

fn find_enum(vm: &mut VM, namespace_id: NamespaceId, name: &str) -> EnumId {
    let iname = vm.interner.intern(name);
    let symtable = NestedSymTable::new(vm, namespace_id);
    symtable.get_enum(iname).expect("enum not found")
}

pub fn resolve_internal_functions(vm: &mut VM) {
    let stdlib = vm.stdlib_namespace_id;
    native_fct(vm, stdlib, "fatalError", stdlib::fatal_error as *const u8);
    native_fct(vm, stdlib, "abort", stdlib::abort as *const u8);
    native_fct(vm, stdlib, "exit", stdlib::exit as *const u8);
    intrinsic_fct(vm, stdlib, "unreachable", Intrinsic::Unreachable);

    native_fct(vm, stdlib, "print", stdlib::print as *const u8);
    native_fct(vm, stdlib, "println", stdlib::println as *const u8);
    intrinsic_fct(vm, stdlib, "assert", Intrinsic::Assert);
    intrinsic_fct(vm, stdlib, "debug", Intrinsic::Debug);
    native_fct(vm, stdlib, "argc", stdlib::argc as *const u8);
    native_fct(vm, stdlib, "argv", stdlib::argv as *const u8);
    native_fct(vm, stdlib, "forceCollect", stdlib::gc_collect as *const u8);
    native_fct(vm, stdlib, "timestamp", stdlib::timestamp as *const u8);
    native_fct(
        vm,
        stdlib,
        "forceMinorCollect",
        stdlib::gc_minor_collect as *const u8,
    );
    native_fct(vm, stdlib, "sleep", stdlib::sleep as *const u8);
    native_fct(
        vm,
        stdlib,
        "spawn",
        stdlib::process::spawn_process as *const u8,
    );

    if vm.args.flag_boots.is_some() {
        native_fct(
            vm,
            vm.boots_namespace_id,
            "encodedBytecode",
            stdlib::bytecode as *const u8,
        );
    }

    native_fct(vm, stdlib, "call", stdlib::call as *const u8);

    intrinsic_fct(vm, stdlib, "unsafeKillRefs", Intrinsic::UnsafeKillRefs);

    native_method(
        vm,
        stdlib,
        "UInt8",
        "toString",
        stdlib::uint8_to_string as *const u8,
    );

    intrinsic_method(vm, stdlib, "UInt8", "toInt64", Intrinsic::ByteToInt64);
    intrinsic_method(vm, stdlib, "UInt8", "toInt32", Intrinsic::ByteToInt32);
    intrinsic_method(vm, stdlib, "UInt8", "toChar", Intrinsic::ByteToChar);
    intrinsic_method(vm, stdlib, "UInt8", "equals", Intrinsic::ByteEq);
    intrinsic_method(vm, stdlib, "UInt8", "compareTo", Intrinsic::ByteCmp);

    native_method(
        vm,
        stdlib,
        "Char",
        "toString",
        stdlib::char_to_string as *const u8,
    );
    intrinsic_method(vm, stdlib, "Char", "toInt64", Intrinsic::CharToInt64);
    intrinsic_method(vm, stdlib, "Char", "toInt32", Intrinsic::CharToInt32);
    intrinsic_method(vm, stdlib, "Char", "equals", Intrinsic::CharEq);
    intrinsic_method(vm, stdlib, "Char", "compareTo", Intrinsic::CharCmp);

    native_method(
        vm,
        stdlib,
        "Int32",
        "toString",
        stdlib::int32_to_string as *const u8,
    );

    intrinsic_method(vm, stdlib, "Int32", "toUInt8", Intrinsic::Int32ToByte);
    intrinsic_method(
        vm,
        stdlib,
        "Int32",
        "toCharUnchecked",
        Intrinsic::Int32ToChar,
    );
    intrinsic_method(vm, stdlib, "Int32", "toInt64", Intrinsic::Int32ToInt64);

    intrinsic_method(vm, stdlib, "Int32", "toFloat32", Intrinsic::Int32ToFloat32);
    intrinsic_method(vm, stdlib, "Int32", "toFloat64", Intrinsic::Int32ToFloat64);

    intrinsic_method(
        vm,
        stdlib,
        "Int32",
        "asFloat32",
        Intrinsic::ReinterpretInt32AsFloat32,
    );

    intrinsic_method(vm, stdlib, "Int32", "equals", Intrinsic::Int32Eq);
    intrinsic_method(vm, stdlib, "Int32", "compareTo", Intrinsic::Int32Cmp);

    intrinsic_method(vm, stdlib, "Int32", "plus", Intrinsic::Int32Add);
    intrinsic_method(vm, stdlib, "Int32", "minus", Intrinsic::Int32Sub);
    intrinsic_method(vm, stdlib, "Int32", "times", Intrinsic::Int32Mul);
    intrinsic_method(vm, stdlib, "Int32", "div", Intrinsic::Int32Div);
    intrinsic_method(vm, stdlib, "Int32", "mod", Intrinsic::Int32Mod);

    intrinsic_method(vm, stdlib, "Int32", "bitwiseOr", Intrinsic::Int32Or);
    intrinsic_method(vm, stdlib, "Int32", "bitwiseAnd", Intrinsic::Int32And);
    intrinsic_method(vm, stdlib, "Int32", "bitwiseXor", Intrinsic::Int32Xor);

    intrinsic_method(vm, stdlib, "Int32", "shiftLeft", Intrinsic::Int32Shl);
    intrinsic_method(vm, stdlib, "Int32", "shiftRight", Intrinsic::Int32Shr);
    intrinsic_method(vm, stdlib, "Int32", "shiftRightSigned", Intrinsic::Int32Sar);

    intrinsic_method(
        vm,
        stdlib,
        "Int32",
        "rotateLeft",
        Intrinsic::Int32RotateLeft,
    );
    intrinsic_method(
        vm,
        stdlib,
        "Int32",
        "rotateRight",
        Intrinsic::Int32RotateRight,
    );

    intrinsic_method(vm, stdlib, "Int32", "unaryPlus", Intrinsic::Int32Plus);
    intrinsic_method(vm, stdlib, "Int32", "unaryMinus", Intrinsic::Int32Neg);
    intrinsic_method(vm, stdlib, "Int32", "not", Intrinsic::Int32Not);

    intrinsic_method(
        vm,
        stdlib,
        "Int32",
        "countZeroBits",
        Intrinsic::Int32CountZeroBits,
    );
    intrinsic_method(
        vm,
        stdlib,
        "Int32",
        "countOneBits",
        Intrinsic::Int32CountOneBits,
    );
    intrinsic_method(
        vm,
        stdlib,
        "Int32",
        "countZeroBitsLeading",
        Intrinsic::Int32CountZeroBitsLeading,
    );
    intrinsic_method(
        vm,
        stdlib,
        "Int32",
        "countOneBitsLeading",
        Intrinsic::Int32CountOneBitsLeading,
    );
    intrinsic_method(
        vm,
        stdlib,
        "Int32",
        "countZeroBitsTrailing",
        Intrinsic::Int32CountZeroBitsTrailing,
    );
    intrinsic_method(
        vm,
        stdlib,
        "Int32",
        "countOneBitsTrailing",
        Intrinsic::Int32CountOneBitsTrailing,
    );

    native_method(
        vm,
        stdlib,
        "Int64",
        "toString",
        stdlib::int64_to_string as *const u8,
    );

    intrinsic_method(
        vm,
        stdlib,
        "Int64",
        "toCharUnchecked",
        Intrinsic::Int64ToChar,
    );
    intrinsic_method(vm, stdlib, "Int64", "toInt32", Intrinsic::Int64ToInt32);
    intrinsic_method(vm, stdlib, "Int64", "toUInt8", Intrinsic::Int64ToByte);

    intrinsic_method(vm, stdlib, "Int64", "toFloat32", Intrinsic::Int64ToFloat32);
    intrinsic_method(vm, stdlib, "Int64", "toFloat64", Intrinsic::Int64ToFloat64);

    intrinsic_method(
        vm,
        stdlib,
        "Int64",
        "asFloat64",
        Intrinsic::ReinterpretInt64AsFloat64,
    );

    intrinsic_method(vm, stdlib, "Int64", "equals", Intrinsic::Int64Eq);
    intrinsic_method(vm, stdlib, "Int64", "compareTo", Intrinsic::Int64Cmp);

    intrinsic_method(vm, stdlib, "Int64", "plus", Intrinsic::Int64Add);
    intrinsic_method(vm, stdlib, "Int64", "minus", Intrinsic::Int64Sub);
    intrinsic_method(vm, stdlib, "Int64", "times", Intrinsic::Int64Mul);
    intrinsic_method(vm, stdlib, "Int64", "div", Intrinsic::Int64Div);
    intrinsic_method(vm, stdlib, "Int64", "mod", Intrinsic::Int64Mod);

    intrinsic_method(vm, stdlib, "Int64", "bitwiseOr", Intrinsic::Int64Or);
    intrinsic_method(vm, stdlib, "Int64", "bitwiseAnd", Intrinsic::Int64And);
    intrinsic_method(vm, stdlib, "Int64", "bitwiseXor", Intrinsic::Int64Xor);

    intrinsic_method(vm, stdlib, "Int64", "shiftLeft", Intrinsic::Int64Shl);
    intrinsic_method(vm, stdlib, "Int64", "shiftRight", Intrinsic::Int64Shr);
    intrinsic_method(vm, stdlib, "Int64", "shiftRightSigned", Intrinsic::Int64Sar);

    intrinsic_method(
        vm,
        stdlib,
        "Int64",
        "rotateLeft",
        Intrinsic::Int64RotateLeft,
    );
    intrinsic_method(
        vm,
        stdlib,
        "Int64",
        "rotateRight",
        Intrinsic::Int64RotateRight,
    );

    intrinsic_method(vm, stdlib, "Int64", "unaryPlus", Intrinsic::Int64Plus);
    intrinsic_method(vm, stdlib, "Int64", "unaryMinus", Intrinsic::Int64Neg);
    intrinsic_method(vm, stdlib, "Int64", "not", Intrinsic::Int64Not);

    intrinsic_method(
        vm,
        stdlib,
        "Int64",
        "countZeroBits",
        Intrinsic::Int64CountZeroBits,
    );
    intrinsic_method(
        vm,
        stdlib,
        "Int64",
        "countOneBits",
        Intrinsic::Int64CountOneBits,
    );
    intrinsic_method(
        vm,
        stdlib,
        "Int64",
        "countZeroBitsLeading",
        Intrinsic::Int64CountZeroBitsLeading,
    );
    intrinsic_method(
        vm,
        stdlib,
        "Int64",
        "countOneBitsLeading",
        Intrinsic::Int64CountOneBitsLeading,
    );
    intrinsic_method(
        vm,
        stdlib,
        "Int64",
        "countZeroBitsTrailing",
        Intrinsic::Int64CountZeroBitsTrailing,
    );
    intrinsic_method(
        vm,
        stdlib,
        "Int64",
        "countOneBitsTrailing",
        Intrinsic::Int64CountOneBitsTrailing,
    );

    intrinsic_method(vm, stdlib, "Bool", "toInt32", Intrinsic::BoolToInt32);
    intrinsic_method(vm, stdlib, "Bool", "toInt64", Intrinsic::BoolToInt64);
    intrinsic_method(vm, stdlib, "Bool", "equals", Intrinsic::BoolEq);
    intrinsic_method(vm, stdlib, "Bool", "not", Intrinsic::BoolNot);

    native_method(
        vm,
        stdlib,
        "String",
        "compareTo",
        stdlib::strcmp as *const u8,
    );
    native_method(
        vm,
        stdlib,
        "String",
        "toInt32Success",
        stdlib::str_to_int32_success as *const u8,
    );
    native_method(
        vm,
        stdlib,
        "String",
        "toInt64Success",
        stdlib::str_to_int64_success as *const u8,
    );
    native_method(
        vm,
        stdlib,
        "String",
        "toFloat32Success",
        stdlib::str_to_float32_success as *const u8,
    );
    native_method(
        vm,
        stdlib,
        "String",
        "toFloat64Success",
        stdlib::str_to_float64_success as *const u8,
    );
    native_method(
        vm,
        stdlib,
        "String",
        "toInt32OrZero",
        stdlib::str_to_int32 as *const u8,
    );
    native_method(
        vm,
        stdlib,
        "String",
        "toInt64OrZero",
        stdlib::str_to_int64 as *const u8,
    );
    native_method(
        vm,
        stdlib,
        "String",
        "toFloat32OrZero",
        stdlib::str_to_float32 as *const u8,
    );
    native_method(
        vm,
        stdlib,
        "String",
        "toFloat64OrZero",
        stdlib::str_to_float64 as *const u8,
    );
    native_method(vm, stdlib, "String", "plus", stdlib::strcat as *const u8);

    intrinsic_method(vm, stdlib, "String", "size", Intrinsic::StrLen);
    intrinsic_method(vm, stdlib, "String", "getByte", Intrinsic::StrGet);
    native_method(
        vm,
        stdlib,
        "String",
        "clone",
        stdlib::str_clone as *const u8,
    );

    native_method(
        vm,
        stdlib,
        "Float32",
        "toString",
        stdlib::float32_to_string as *const u8,
    );
    intrinsic_method(vm, stdlib, "Float32", "toInt32", Intrinsic::Float32ToInt32);
    intrinsic_method(vm, stdlib, "Float32", "toInt64", Intrinsic::Float32ToInt64);
    intrinsic_method(
        vm,
        stdlib,
        "Float32",
        "toFloat64",
        Intrinsic::PromoteFloat32ToFloat64,
    );

    intrinsic_method(
        vm,
        stdlib,
        "Float32",
        "asInt32",
        Intrinsic::ReinterpretFloat32AsInt32,
    );

    intrinsic_method(vm, stdlib, "Float32", "equals", Intrinsic::Float32Eq);
    intrinsic_method(vm, stdlib, "Float32", "compareTo", Intrinsic::Float32Cmp);

    intrinsic_method(vm, stdlib, "Float32", "plus", Intrinsic::Float32Add);
    intrinsic_method(vm, stdlib, "Float32", "minus", Intrinsic::Float32Sub);
    intrinsic_method(vm, stdlib, "Float32", "times", Intrinsic::Float32Mul);
    intrinsic_method(vm, stdlib, "Float32", "div", Intrinsic::Float32Div);

    intrinsic_method(vm, stdlib, "Float32", "unaryPlus", Intrinsic::Float32Plus);
    intrinsic_method(vm, stdlib, "Float32", "unaryMinus", Intrinsic::Float32Neg);

    intrinsic_method(vm, stdlib, "Float32", "isNan", Intrinsic::Float32IsNan);
    intrinsic_method(vm, stdlib, "Float32", "sqrt", Intrinsic::Float32Sqrt);

    native_method(
        vm,
        stdlib,
        "Float64",
        "toString",
        stdlib::float64_to_string as *const u8,
    );
    intrinsic_method(vm, stdlib, "Float64", "toInt32", Intrinsic::Float64ToInt32);
    intrinsic_method(vm, stdlib, "Float64", "toInt64", Intrinsic::Float64ToInt64);
    intrinsic_method(
        vm,
        stdlib,
        "Float64",
        "toFloat32",
        Intrinsic::DemoteFloat64ToFloat32,
    );

    intrinsic_method(
        vm,
        stdlib,
        "Float64",
        "asInt64",
        Intrinsic::ReinterpretFloat64AsInt64,
    );

    intrinsic_method(vm, stdlib, "Float64", "equals", Intrinsic::Float64Eq);
    intrinsic_method(vm, stdlib, "Float64", "compareTo", Intrinsic::Float64Cmp);

    intrinsic_method(vm, stdlib, "Float64", "plus", Intrinsic::Float64Add);
    intrinsic_method(vm, stdlib, "Float64", "minus", Intrinsic::Float64Sub);
    intrinsic_method(vm, stdlib, "Float64", "times", Intrinsic::Float64Mul);
    intrinsic_method(vm, stdlib, "Float64", "div", Intrinsic::Float64Div);

    intrinsic_method(vm, stdlib, "Float64", "unaryPlus", Intrinsic::Float64Plus);
    intrinsic_method(vm, stdlib, "Float64", "unaryMinus", Intrinsic::Float64Neg);

    intrinsic_method(vm, stdlib, "Float64", "isNan", Intrinsic::Float64IsNan);
    intrinsic_method(vm, stdlib, "Float64", "sqrt", Intrinsic::Float64Sqrt);

    native_static(
        vm,
        stdlib,
        "String",
        "fromBytesPart",
        stdlib::str_from_bytes as *const u8,
    );
    native_static(
        vm,
        stdlib,
        "String",
        "fromStringPart",
        stdlib::str_from_bytes as *const u8,
    );

    intrinsic_ctor(vm, stdlib, "Array", Intrinsic::ArrayWithValues);
    intrinsic_method(vm, stdlib, "Array", "size", Intrinsic::ArrayLen);
    intrinsic_method(vm, stdlib, "Array", "get", Intrinsic::ArrayGet);
    intrinsic_method(vm, stdlib, "Array", "set", Intrinsic::ArraySet);

    intrinsic_static(vm, stdlib, "Array", "unsafeNew", Intrinsic::ArrayNewOfSize);

    native_method(
        vm,
        stdlib,
        "Stacktrace",
        "retrieveStacktrace",
        stack::retrieve_stack_trace as *const u8,
    );
    native_method(
        vm,
        stdlib,
        "Stacktrace",
        "getStacktraceElement",
        stack::stack_element as *const u8,
    );

    native_method(
        vm,
        stdlib,
        "Thread",
        "start",
        stdlib::spawn_thread as *const u8,
    );

    native_method(
        vm,
        stdlib,
        "Thread",
        "join",
        stdlib::join_thread as *const u8,
    );

    native_method(vm, stdlib, "Mutex", "wait", stdlib::mutex_wait as *const u8);

    native_method(
        vm,
        stdlib,
        "Mutex",
        "notify",
        stdlib::mutex_notify as *const u8,
    );

    native_method(
        vm,
        stdlib,
        "Condition",
        "enqueue",
        stdlib::condition_enqueue as *const u8,
    );

    native_method(
        vm,
        stdlib,
        "Condition",
        "block",
        stdlib::condition_block_after_enqueue as *const u8,
    );

    native_method(
        vm,
        stdlib,
        "Condition",
        "wakeupOne",
        stdlib::condition_wakeup_one as *const u8,
    );

    native_method(
        vm,
        stdlib,
        "Condition",
        "wakeupAll",
        stdlib::condition_wakeup_all as *const u8,
    );

    intrinsic_method(vm, stdlib, "Option", "isNone", Intrinsic::OptionIsNone);
    intrinsic_method(vm, stdlib, "Option", "isSome", Intrinsic::OptionIsSome);
    intrinsic_method(vm, stdlib, "Option", "unwrap", Intrinsic::OptionUnwrap);

    intrinsic_method(vm, stdlib, "AtomicInt32", "get", Intrinsic::AtomicInt32Get);
    intrinsic_method(vm, stdlib, "AtomicInt32", "set", Intrinsic::AtomicInt32Set);
    intrinsic_method(
        vm,
        stdlib,
        "AtomicInt32",
        "exchange",
        Intrinsic::AtomicInt32Exchange,
    );
    intrinsic_method(
        vm,
        stdlib,
        "AtomicInt32",
        "compareExchange",
        Intrinsic::AtomicInt32CompareExchange,
    );
    intrinsic_method(
        vm,
        stdlib,
        "AtomicInt32",
        "fetchAdd",
        Intrinsic::AtomicInt32FetchAdd,
    );

    intrinsic_method(vm, stdlib, "AtomicInt64", "get", Intrinsic::AtomicInt64Get);
    intrinsic_method(vm, stdlib, "AtomicInt64", "set", Intrinsic::AtomicInt64Set);
    intrinsic_method(
        vm,
        stdlib,
        "AtomicInt64",
        "exchange",
        Intrinsic::AtomicInt64Exchange,
    );
    intrinsic_method(
        vm,
        stdlib,
        "AtomicInt64",
        "compareExchange",
        Intrinsic::AtomicInt64CompareExchange,
    );
    intrinsic_method(
        vm,
        stdlib,
        "AtomicInt64",
        "fetchAdd",
        Intrinsic::AtomicInt64FetchAdd,
    );
}

fn intrinsic_ctor(vm: &VM, namespace_id: NamespaceId, class_name: &str, intrinsic: Intrinsic) {
    let symtable = NestedSymTable::new(vm, namespace_id);
    let class_name_interned = vm.interner.intern(class_name);
    let cls_id = symtable
        .get_class(class_name_interned)
        .expect("class not expected");

    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();

    let ctor_id = cls.constructor.expect("no constructor");
    let ctor = vm.fcts.idx(ctor_id);
    let mut ctor = ctor.write();
    ctor.intrinsic = Some(intrinsic);
}

fn find_method(vm: &VM, namespace_id: NamespaceId, container_name: &str, name: &str) -> FctId {
    let container_name = vm.interner.intern(container_name);

    let cls_id = NestedSymTable::new(vm, namespace_id)
        .get_class(container_name)
        .expect("class not found");

    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();
    let intern_name = vm.interner.intern(name);

    for &mid in &cls.methods {
        let mtd = vm.fcts.idx(mid);
        let mtd = mtd.read();

        if mtd.name == intern_name {
            return mid;
        }
    }

    panic!("cannot find class method `{}`", name)
}

fn find_static(vm: &VM, namespace_id: NamespaceId, container_name: &str, name: &str) -> FctId {
    let container_name = vm.interner.intern(container_name);

    let symtable = NestedSymTable::new(vm, namespace_id);
    let sym = symtable.get(container_name);
    let intern_name = vm.interner.intern(name);

    match sym {
        Some(Sym::Module(module_id)) => {
            let module = vm.modules.idx(module_id);
            let module = module.read();

            for &mid in &module.methods {
                let mtd = vm.fcts.idx(mid);
                let mtd = mtd.read();

                if mtd.name == intern_name {
                    return mid;
                }
            }
        }

        Some(Sym::Class(cls_id)) => {
            let cls = vm.classes.idx(cls_id);
            let cls = cls.read();

            for &mid in &cls.methods {
                let mtd = vm.fcts.idx(mid);
                let mtd = mtd.read();

                if mtd.name == intern_name && mtd.is_static {
                    return mid;
                }
            }
        }

        _ => unreachable!(),
    }

    panic!("cannot find static method `{}`", name)
}

fn native_fct(vm: &mut VM, namespace_id: NamespaceId, name: &str, fctptr: *const u8) {
    common_fct(
        vm,
        namespace_id,
        name,
        FctImplementation::Native(Address::from_ptr(fctptr)),
    );
}

fn intrinsic_fct(vm: &mut VM, namespace_id: NamespaceId, name: &str, intrinsic: Intrinsic) {
    common_fct(
        vm,
        namespace_id,
        name,
        FctImplementation::Intrinsic(intrinsic),
    );
}

fn common_fct(vm: &mut VM, namespace_id: NamespaceId, name: &str, kind: FctImplementation) {
    let name = vm.interner.intern(name);
    let fctid = NestedSymTable::new(vm, namespace_id)
        .get_fct(name)
        .expect("function not found");

    let fct = vm.fcts.idx(fctid);
    let mut fct = fct.write();

    match kind {
        FctImplementation::Intrinsic(intrinsic) => fct.intrinsic = Some(intrinsic),
        FctImplementation::Native(address) => {
            fct.native_pointer = Some(address);
        }
    }
    fct.internal_resolved = true;
}

enum FctImplementation {
    Intrinsic(Intrinsic),
    Native(Address),
}

fn native_method(
    vm: &VM,
    namespace_id: NamespaceId,
    container_name: &str,
    method_name: &str,
    fctptr: *const u8,
) {
    common_method(
        vm,
        namespace_id,
        container_name,
        method_name,
        false,
        FctImplementation::Native(Address::from_ptr(fctptr)),
    );
}

fn intrinsic_method(
    vm: &VM,
    namespace_id: NamespaceId,
    container_name: &str,
    method_name: &str,
    intrinsic: Intrinsic,
) {
    common_method(
        vm,
        namespace_id,
        container_name,
        method_name,
        false,
        FctImplementation::Intrinsic(intrinsic),
    );
}

fn native_static(
    vm: &VM,
    namespace_id: NamespaceId,
    container_name: &str,
    method_name: &str,
    fctptr: *const u8,
) {
    common_method(
        vm,
        namespace_id,
        container_name,
        method_name,
        true,
        FctImplementation::Native(Address::from_ptr(fctptr)),
    );
}

fn intrinsic_static(
    vm: &VM,
    namespace_id: NamespaceId,
    container_name: &str,
    method_name: &str,
    intrinsic: Intrinsic,
) {
    common_method(
        vm,
        namespace_id,
        container_name,
        method_name,
        true,
        FctImplementation::Intrinsic(intrinsic),
    );
}

fn common_method(
    vm: &VM,
    namespace_id: NamespaceId,
    container_name: &str,
    method_name: &str,
    is_static: bool,
    implementation: FctImplementation,
) {
    let container_name_interned = vm.interner.intern(container_name);

    let symtable = NestedSymTable::new(vm, namespace_id);
    let sym = symtable.get(container_name_interned);

    match sym {
        Some(Sym::Class(cls_id)) => {
            internal_class_method(vm, cls_id, method_name, is_static, implementation);
        }

        Some(Sym::Module(module_id)) => {
            internal_module_method(vm, module_id, method_name, is_static, implementation);
        }

        Some(Sym::Struct(struct_id)) => {
            let xstruct = vm.structs.idx(struct_id);
            let xstruct = xstruct.read();
            internal_extension_method(
                vm,
                &xstruct.extensions,
                method_name,
                is_static,
                implementation,
            );
        }
        Some(Sym::Enum(enum_id)) => {
            let xenum = &vm.enums[enum_id].read();
            internal_extension_method(
                vm,
                &xenum.extensions,
                method_name,
                is_static,
                implementation,
            );
        }

        _ => panic!("unexpected type"),
    }
}

fn internal_module_method(
    vm: &VM,
    module_id: ModuleId,
    name: &str,
    _is_static: bool,
    implementation: FctImplementation,
) {
    let module = vm.modules.idx(module_id);
    let module = module.read();
    let name_interned = vm.interner.intern(name);

    for &mid in &module.methods {
        let mtd = vm.fcts.idx(mid);
        let mut mtd = mtd.write();

        if mtd.name == name_interned {
            match implementation {
                FctImplementation::Intrinsic(intrinsic) => mtd.intrinsic = Some(intrinsic),
                FctImplementation::Native(address) => {
                    mtd.native_pointer = Some(address);
                }
            }
            mtd.internal_resolved = true;
            return;
        }
    }
}

fn internal_class_method(
    vm: &VM,
    cls_id: ClassId,
    name: &str,
    is_static: bool,
    kind: FctImplementation,
) {
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();
    let name_interned = vm.interner.intern(name);

    for &mid in &cls.methods {
        let mtd = vm.fcts.idx(mid);
        let mut mtd = mtd.write();

        if mtd.name == name_interned && mtd.is_static == is_static {
            match kind {
                FctImplementation::Intrinsic(intrinsic) => mtd.intrinsic = Some(intrinsic),
                FctImplementation::Native(address) => {
                    mtd.native_pointer = Some(address);
                }
            }
            mtd.internal_resolved = true;
            return;
        }
    }

    internal_extension_method(vm, &cls.extensions, name, is_static, kind);
}

fn internal_extension_method(
    vm: &VM,
    extensions: &[ExtensionId],
    name_as_string: &str,
    is_static: bool,
    kind: FctImplementation,
) {
    let name = vm.interner.intern(name_as_string);

    for &extension_id in extensions {
        let extension = vm.extensions[extension_id].read();

        let table = if is_static {
            &extension.static_names
        } else {
            &extension.instance_names
        };

        if let Some(&method_id) = table.get(&name) {
            let fct = vm.fcts.idx(method_id);
            let mut fct = fct.write();

            match kind {
                FctImplementation::Intrinsic(intrinsic) => fct.intrinsic = Some(intrinsic),
                FctImplementation::Native(address) => {
                    fct.native_pointer = Some(address);
                }
            }
            fct.internal_resolved = true;
            return;
        }
    }

    panic!("method {} not found!", name_as_string)
}

#[cfg(test)]
mod tests {
    use crate::semck::tests::*;

    #[test]
    fn builtin_functions() {
        ok("fun f() { assert(true); }");
        ok("fun f() { print(\"test\"); }");
        ok("fun f() { println(\"test\"); }");
    }
}
