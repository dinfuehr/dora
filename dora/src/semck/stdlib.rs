use parking_lot::RwLock;
use std::sync::Arc;

use crate::gc::Address;
use crate::mem;
use crate::object::Header;
use crate::size::InstanceSize;
use crate::stack;
use crate::stdlib;
use crate::sym::{NestedSymTable, TermSym, TypeSym};
use crate::ty::{SourceType, SourceTypeArray};
use crate::vm::{
    ClassDef, ClassDefId, ClassId, EnumId, ExtensionId, FctId, Intrinsic, ModuleId, NamespaceId,
    StructId, TraitId, VM,
};
use crate::vtable::VTableBox;

pub fn resolve_internal_classes(vm: &mut VM) {
    vm.known.structs.bool = internal_struct(vm, "Bool", Some(SourceType::Bool));

    vm.known.structs.uint8 = internal_struct(vm, "UInt8", Some(SourceType::UInt8));
    vm.known.structs.char = internal_struct(vm, "Char", Some(SourceType::Char));
    vm.known.structs.int32 = internal_struct(vm, "Int32", Some(SourceType::Int32));
    vm.known.structs.int64 = internal_struct(vm, "Int64", Some(SourceType::Int64));

    vm.known.structs.float32 = internal_struct(vm, "Float32", Some(SourceType::Float32));
    vm.known.structs.float64 = internal_struct(vm, "Float64", Some(SourceType::Float64));

    vm.known.classes.object = find_class(vm, "Object");
    vm.known.classes.string = internal_class(vm, "String", None);
    vm.known.modules.string = internal_module(vm, "String", None);

    vm.known.classes.string_buffer = find_class(vm, "StringBuffer");
    vm.known.modules.string_buffer = internal_module(vm, "StringBuffer", None);

    let cls = vm.classes.idx(vm.known.classes.string);
    let mut cls = cls.write();
    cls.is_str = true;

    vm.known.classes.array = internal_class(vm, "Array", None);
    vm.known.modules.array = internal_module(vm, "Array", None);

    let cls = vm.classes.idx(vm.known.classes.array);
    let mut cls = cls.write();
    cls.is_array = true;

    vm.known.classes.testing = find_class(vm, "Testing");

    vm.known.classes.stacktrace = find_class(vm, "Stacktrace");
    vm.known.classes.stacktrace_element = find_class(vm, "StacktraceElement");

    vm.known.traits.stringable = find_trait(vm, "Stringable");
    vm.known.traits.zero = find_trait(vm, "Zero");
    vm.known.traits.iterator = find_trait(vm, "Iterator");

    vm.known.enums.option = find_enum(vm, "Option");

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
    ];

    let stdlib = vm.stdlib_namespace();
    let stdlib = stdlib.read();

    let prelude = vm.prelude_namespace();
    let mut prelude = prelude.write();

    for sym in &symbols {
        let name = vm.interner.intern(sym);
        let sym_term = stdlib.get_term(name);
        let sym_type = stdlib.get_type(name);

        assert!(sym_term.is_some() || sym_type.is_some());

        if let Some(sym_term) = sym_term {
            let old_sym = prelude.insert_term(name, sym_term);
            assert!(old_sym.is_none());
        }

        if let Some(sym_type) = sym_type {
            let old_sym = prelude.insert_type(name, sym_type);
            assert!(old_sym.is_none());
        }
    }

    let stdlib_name = vm.interner.intern("std");
    prelude.insert_term(stdlib_name, TermSym::Namespace(vm.stdlib_namespace_id));

    {
        // include None and Some from Option
        let option_name = vm.interner.intern("Option");
        let option_id = stdlib.get_type(option_name);

        match option_id {
            Some(TypeSym::Enum(enum_id)) => {
                let xenum = &vm.enums[enum_id];
                let xenum = xenum.read();

                for variant in &xenum.variants {
                    let old_sym =
                        prelude.insert_term(variant.name, TermSym::EnumValue(enum_id, variant.id));
                    assert!(old_sym.is_none());
                }
            }

            _ => unreachable!(),
        }
    }
}

pub fn discover_known_methods(vm: &mut VM) {
    vm.known.functions.string_buffer_empty =
        find_module_method(vm, vm.known.modules.string_buffer, "empty");
    vm.known.functions.string_buffer_append =
        find_class_method(vm, vm.known.classes.string_buffer, "append");
    vm.known.functions.string_buffer_to_string =
        find_class_method(vm, vm.known.classes.string_buffer, "toString");
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

fn find_class(vm: &VM, name: &str) -> ClassId {
    let iname = vm.interner.intern(name);
    vm.stdlib_namespace()
        .read()
        .get_class(iname)
        .expect("class not found")
}

fn internal_class(vm: &mut VM, name: &str, ty: Option<SourceType>) -> ClassId {
    let iname = vm.interner.intern(name);
    let clsid = vm.stdlib_namespace().read().get_class(iname);

    if let Some(clsid) = clsid {
        let cls = vm.classes.idx(clsid);
        let mut cls = cls.write();

        if cls.internal {
            if let Some(ty) = ty {
                cls.primitive_type = Some(ty);
            }

            cls.internal_resolved = true;
        }

        clsid
    } else {
        panic!("class {} not found!", name);
    }
}

fn internal_struct(vm: &mut VM, name: &str, ty: Option<SourceType>) -> StructId {
    let iname = vm.interner.intern(name);
    let struct_id = vm.stdlib_namespace().read().get_struct(iname);

    if let Some(struct_id) = struct_id {
        let xstruct = vm.structs.idx(struct_id);
        let mut xstruct = xstruct.write();

        assert!(xstruct.internal);
        xstruct.primitive_ty = ty;
        xstruct.internal_resolved = true;

        struct_id
    } else {
        panic!("struct {} not found!", name);
    }
}

fn internal_module(vm: &mut VM, name: &str, ty: Option<SourceType>) -> ModuleId {
    let iname = vm.interner.intern(name);
    let module_id = vm.stdlib_namespace().read().get_module(iname);

    if let Some(module_id) = module_id {
        let module = vm.modules.idx(module_id);
        let mut module = module.write();

        if module.internal {
            if let Some(ty) = ty {
                module.ty = ty;
            }

            module.internal_resolved = true;
        }

        module_id
    } else {
        panic!("module {} not found!", name);
    }
}

fn find_trait(vm: &mut VM, name: &str) -> TraitId {
    let iname = vm.interner.intern(name);

    vm.stdlib_namespace()
        .read()
        .get_trait(iname)
        .expect("trait not found")
}

fn find_enum(vm: &mut VM, name: &str) -> EnumId {
    let iname = vm.interner.intern(name);

    vm.stdlib_namespace()
        .read()
        .get_enum(iname)
        .expect("enum not found")
}

pub fn resolve_internal_functions(vm: &mut VM) {
    let stdlib = vm.stdlib_namespace_id;
    native_fct(vm, "fatalError", stdlib::fatal_error as *const u8, stdlib);
    native_fct(vm, "abort", stdlib::abort as *const u8, stdlib);
    native_fct(vm, "exit", stdlib::exit as *const u8, stdlib);
    intrinsic_fct(vm, "unreachable", Intrinsic::Unreachable, stdlib);

    native_fct(vm, "print", stdlib::print as *const u8, stdlib);
    native_fct(vm, "println", stdlib::println as *const u8, stdlib);
    intrinsic_fct(vm, "assert", Intrinsic::Assert, stdlib);
    intrinsic_fct(vm, "debug", Intrinsic::Debug, stdlib);
    native_fct(vm, "argc", stdlib::argc as *const u8, stdlib);
    native_fct(vm, "argv", stdlib::argv as *const u8, stdlib);
    native_fct(vm, "forceCollect", stdlib::gc_collect as *const u8, stdlib);
    native_fct(vm, "timestamp", stdlib::timestamp as *const u8, stdlib);
    native_fct(
        vm,
        "forceMinorCollect",
        stdlib::gc_minor_collect as *const u8,
        stdlib,
    );
    native_fct(vm, "sleep", stdlib::sleep as *const u8, stdlib);

    if vm.args.flag_boots.is_some() {
        native_fct(
            vm,
            "encodedBytecode",
            stdlib::bytecode as *const u8,
            vm.boots_namespace_id,
        );
    }

    native_fct(vm, "call", stdlib::call as *const u8, stdlib);

    intrinsic_fct(vm, "unsafeKillRefs", Intrinsic::UnsafeKillRefs, stdlib);
    intrinsic_fct(vm, "unsafeIsNull", Intrinsic::UnsafeIsNull, stdlib);
    intrinsic_fct(
        vm,
        "unsafeLoadEnumVariant",
        Intrinsic::UnsafeLoadEnumVariant,
        stdlib,
    );
    intrinsic_fct(
        vm,
        "unsafeLoadEnumElement",
        Intrinsic::UnsafeLoadEnumElement,
        stdlib,
    );

    native_method(
        vm,
        "UInt8",
        "toString",
        stdlib::uint8_to_string as *const u8,
    );

    intrinsic_method(vm, "UInt8", "toInt64", Intrinsic::ByteToInt64);
    intrinsic_method(vm, "UInt8", "toInt32", Intrinsic::ByteToInt32);
    intrinsic_method(vm, "UInt8", "toChar", Intrinsic::ByteToChar);
    intrinsic_method(vm, "UInt8", "equals", Intrinsic::ByteEq);
    intrinsic_method(vm, "UInt8", "compareTo", Intrinsic::ByteCmp);

    native_method(vm, "Char", "toString", stdlib::char_to_string as *const u8);
    intrinsic_method(vm, "Char", "toInt64", Intrinsic::CharToInt64);
    intrinsic_method(vm, "Char", "toInt32", Intrinsic::CharToInt32);
    intrinsic_method(vm, "Char", "equals", Intrinsic::CharEq);
    intrinsic_method(vm, "Char", "compareTo", Intrinsic::CharCmp);

    native_method(
        vm,
        "Int32",
        "toString",
        stdlib::int32_to_string as *const u8,
    );

    intrinsic_method(vm, "Int32", "toUInt8", Intrinsic::Int32ToByte);
    intrinsic_method(vm, "Int32", "toCharUnchecked", Intrinsic::Int32ToChar);
    intrinsic_method(vm, "Int32", "toInt64", Intrinsic::Int32ToInt64);

    intrinsic_method(vm, "Int32", "toFloat32", Intrinsic::Int32ToFloat32);
    intrinsic_method(vm, "Int32", "toFloat64", Intrinsic::Int32ToFloat64);

    intrinsic_method(
        vm,
        "Int32",
        "asFloat32",
        Intrinsic::ReinterpretInt32AsFloat32,
    );

    intrinsic_method(vm, "Int32", "equals", Intrinsic::Int32Eq);
    intrinsic_method(vm, "Int32", "compareTo", Intrinsic::Int32Cmp);

    intrinsic_method(vm, "Int32", "plus", Intrinsic::Int32Add);
    intrinsic_method(vm, "Int32", "minus", Intrinsic::Int32Sub);
    intrinsic_method(vm, "Int32", "times", Intrinsic::Int32Mul);
    intrinsic_method(vm, "Int32", "div", Intrinsic::Int32Div);
    intrinsic_method(vm, "Int32", "mod", Intrinsic::Int32Mod);

    intrinsic_method(vm, "Int32", "bitwiseOr", Intrinsic::Int32Or);
    intrinsic_method(vm, "Int32", "bitwiseAnd", Intrinsic::Int32And);
    intrinsic_method(vm, "Int32", "bitwiseXor", Intrinsic::Int32Xor);

    intrinsic_method(vm, "Int32", "shiftLeft", Intrinsic::Int32Shl);
    intrinsic_method(vm, "Int32", "shiftRight", Intrinsic::Int32Shr);
    intrinsic_method(vm, "Int32", "shiftRightSigned", Intrinsic::Int32Sar);

    intrinsic_method(vm, "Int32", "rotateLeft", Intrinsic::Int32RotateLeft);
    intrinsic_method(vm, "Int32", "rotateRight", Intrinsic::Int32RotateRight);

    intrinsic_method(vm, "Int32", "unaryPlus", Intrinsic::Int32Plus);
    intrinsic_method(vm, "Int32", "unaryMinus", Intrinsic::Int32Neg);
    intrinsic_method(vm, "Int32", "not", Intrinsic::Int32Not);

    intrinsic_method(vm, "Int32", "countZeroBits", Intrinsic::Int32CountZeroBits);
    intrinsic_method(vm, "Int32", "countOneBits", Intrinsic::Int32CountOneBits);
    intrinsic_method(
        vm,
        "Int32",
        "countZeroBitsLeading",
        Intrinsic::Int32CountZeroBitsLeading,
    );
    intrinsic_method(
        vm,
        "Int32",
        "countOneBitsLeading",
        Intrinsic::Int32CountOneBitsLeading,
    );
    intrinsic_method(
        vm,
        "Int32",
        "countZeroBitsTrailing",
        Intrinsic::Int32CountZeroBitsTrailing,
    );
    intrinsic_method(
        vm,
        "Int32",
        "countOneBitsTrailing",
        Intrinsic::Int32CountOneBitsTrailing,
    );

    native_method(
        vm,
        "Int64",
        "toString",
        stdlib::int64_to_string as *const u8,
    );

    intrinsic_method(vm, "Int64", "toCharUnchecked", Intrinsic::Int64ToChar);
    intrinsic_method(vm, "Int64", "toInt32", Intrinsic::Int64ToInt32);
    intrinsic_method(vm, "Int64", "toUInt8", Intrinsic::Int64ToByte);

    intrinsic_method(vm, "Int64", "toFloat32", Intrinsic::Int64ToFloat32);
    intrinsic_method(vm, "Int64", "toFloat64", Intrinsic::Int64ToFloat64);

    intrinsic_method(
        vm,
        "Int64",
        "asFloat64",
        Intrinsic::ReinterpretInt64AsFloat64,
    );

    intrinsic_method(vm, "Int64", "equals", Intrinsic::Int64Eq);
    intrinsic_method(vm, "Int64", "compareTo", Intrinsic::Int64Cmp);

    intrinsic_method(vm, "Int64", "plus", Intrinsic::Int64Add);
    intrinsic_method(vm, "Int64", "minus", Intrinsic::Int64Sub);
    intrinsic_method(vm, "Int64", "times", Intrinsic::Int64Mul);
    intrinsic_method(vm, "Int64", "div", Intrinsic::Int64Div);
    intrinsic_method(vm, "Int64", "mod", Intrinsic::Int64Mod);

    intrinsic_method(vm, "Int64", "bitwiseOr", Intrinsic::Int64Or);
    intrinsic_method(vm, "Int64", "bitwiseAnd", Intrinsic::Int64And);
    intrinsic_method(vm, "Int64", "bitwiseXor", Intrinsic::Int64Xor);

    intrinsic_method(vm, "Int64", "shiftLeft", Intrinsic::Int64Shl);
    intrinsic_method(vm, "Int64", "shiftRight", Intrinsic::Int64Shr);
    intrinsic_method(vm, "Int64", "shiftRightSigned", Intrinsic::Int64Sar);

    intrinsic_method(vm, "Int64", "rotateLeft", Intrinsic::Int64RotateLeft);
    intrinsic_method(vm, "Int64", "rotateRight", Intrinsic::Int64RotateRight);

    intrinsic_method(vm, "Int64", "unaryPlus", Intrinsic::Int64Plus);
    intrinsic_method(vm, "Int64", "unaryMinus", Intrinsic::Int64Neg);
    intrinsic_method(vm, "Int64", "not", Intrinsic::Int64Not);

    intrinsic_method(vm, "Int64", "countZeroBits", Intrinsic::Int64CountZeroBits);
    intrinsic_method(vm, "Int64", "countOneBits", Intrinsic::Int64CountOneBits);
    intrinsic_method(
        vm,
        "Int64",
        "countZeroBitsLeading",
        Intrinsic::Int64CountZeroBitsLeading,
    );
    intrinsic_method(
        vm,
        "Int64",
        "countOneBitsLeading",
        Intrinsic::Int64CountOneBitsLeading,
    );
    intrinsic_method(
        vm,
        "Int64",
        "countZeroBitsTrailing",
        Intrinsic::Int64CountZeroBitsTrailing,
    );
    intrinsic_method(
        vm,
        "Int64",
        "countOneBitsTrailing",
        Intrinsic::Int64CountOneBitsTrailing,
    );

    intrinsic_method(vm, "Bool", "toInt32", Intrinsic::BoolToInt32);
    intrinsic_method(vm, "Bool", "toInt64", Intrinsic::BoolToInt64);
    intrinsic_method(vm, "Bool", "equals", Intrinsic::BoolEq);
    intrinsic_method(vm, "Bool", "not", Intrinsic::BoolNot);

    native_method(vm, "String", "compareTo", stdlib::strcmp as *const u8);
    native_method(
        vm,
        "String",
        "toInt32Success",
        stdlib::str_to_int32_success as *const u8,
    );
    native_method(
        vm,
        "String",
        "toInt64Success",
        stdlib::str_to_int64_success as *const u8,
    );
    native_method(
        vm,
        "String",
        "toInt32OrZero",
        stdlib::str_to_int32 as *const u8,
    );
    native_method(
        vm,
        "String",
        "toInt64OrZero",
        stdlib::str_to_int64 as *const u8,
    );
    native_method(vm, "String", "plus", stdlib::strcat as *const u8);

    intrinsic_method(vm, "String", "size", Intrinsic::StrLen);
    intrinsic_method(vm, "String", "getByte", Intrinsic::StrGet);
    native_method(vm, "String", "clone", stdlib::str_clone as *const u8);

    native_method(
        vm,
        "Float32",
        "toString",
        stdlib::float32_to_string as *const u8,
    );
    intrinsic_method(vm, "Float32", "toInt32", Intrinsic::Float32ToInt32);
    intrinsic_method(vm, "Float32", "toInt64", Intrinsic::Float32ToInt64);
    intrinsic_method(
        vm,
        "Float32",
        "toFloat64",
        Intrinsic::PromoteFloat32ToFloat64,
    );

    intrinsic_method(
        vm,
        "Float32",
        "asInt32",
        Intrinsic::ReinterpretFloat32AsInt32,
    );

    intrinsic_method(vm, "Float32", "equals", Intrinsic::Float32Eq);
    intrinsic_method(vm, "Float32", "compareTo", Intrinsic::Float32Cmp);

    intrinsic_method(vm, "Float32", "plus", Intrinsic::Float32Add);
    intrinsic_method(vm, "Float32", "minus", Intrinsic::Float32Sub);
    intrinsic_method(vm, "Float32", "times", Intrinsic::Float32Mul);
    intrinsic_method(vm, "Float32", "div", Intrinsic::Float32Div);

    intrinsic_method(vm, "Float32", "unaryPlus", Intrinsic::Float32Plus);
    intrinsic_method(vm, "Float32", "unaryMinus", Intrinsic::Float32Neg);

    intrinsic_method(vm, "Float32", "isNan", Intrinsic::Float32IsNan);
    intrinsic_method(vm, "Float32", "sqrt", Intrinsic::Float32Sqrt);

    native_method(
        vm,
        "Float64",
        "toString",
        stdlib::float64_to_string as *const u8,
    );
    intrinsic_method(vm, "Float64", "toInt32", Intrinsic::Float64ToInt32);
    intrinsic_method(vm, "Float64", "toInt64", Intrinsic::Float64ToInt64);
    intrinsic_method(
        vm,
        "Float64",
        "toFloat32",
        Intrinsic::DemoteFloat64ToFloat32,
    );

    intrinsic_method(
        vm,
        "Float64",
        "asInt64",
        Intrinsic::ReinterpretFloat64AsInt64,
    );

    intrinsic_method(vm, "Float64", "equals", Intrinsic::Float64Eq);
    intrinsic_method(vm, "Float64", "compareTo", Intrinsic::Float64Cmp);

    intrinsic_method(vm, "Float64", "plus", Intrinsic::Float64Add);
    intrinsic_method(vm, "Float64", "minus", Intrinsic::Float64Sub);
    intrinsic_method(vm, "Float64", "times", Intrinsic::Float64Mul);
    intrinsic_method(vm, "Float64", "div", Intrinsic::Float64Div);

    intrinsic_method(vm, "Float64", "unaryPlus", Intrinsic::Float64Plus);
    intrinsic_method(vm, "Float64", "unaryMinus", Intrinsic::Float64Neg);

    intrinsic_method(vm, "Float64", "isNan", Intrinsic::Float64IsNan);
    intrinsic_method(vm, "Float64", "sqrt", Intrinsic::Float64Sqrt);

    let module_id = vm.known.modules.string;
    native_module_method(
        vm,
        module_id,
        "fromBytesPart",
        stdlib::str_from_bytes as *const u8,
    );
    native_module_method(
        vm,
        module_id,
        "fromStringPart",
        stdlib::str_from_bytes as *const u8,
    );

    let cls_id = vm.known.classes.array;
    intrinsic_ctor(vm, cls_id, Intrinsic::ArrayWithValues);
    intrinsic_method(vm, "Array", "size", Intrinsic::ArrayLen);
    intrinsic_method(vm, "Array", "get", Intrinsic::ArrayGet);
    intrinsic_method(vm, "Array", "set", Intrinsic::ArraySet);

    let module_id = vm.known.modules.array;
    intrinsic_module_method(vm, module_id, "ofSizeUnsafe", Intrinsic::ArrayNewOfSize);

    native_method(
        vm,
        "Stacktrace",
        "retrieveStacktrace",
        stack::retrieve_stack_trace as *const u8,
    );
    native_method(
        vm,
        "Stacktrace",
        "getStacktraceElement",
        stack::stack_element as *const u8,
    );

    native_method(vm, "Thread", "start", stdlib::spawn_thread as *const u8);

    intrinsic_method(vm, "Option", "isNone", Intrinsic::OptionIsNone);
    intrinsic_method(vm, "Option", "isSome", Intrinsic::OptionIsSome);
    intrinsic_method(vm, "Option", "unwrap", Intrinsic::OptionUnwrap);
}

fn intrinsic_ctor(vm: &mut VM, cls_id: ClassId, intrinsic: Intrinsic) {
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();

    if let Some(ctor_id) = cls.constructor {
        let ctor = vm.fcts.idx(ctor_id);
        let mut ctor = ctor.write();

        ctor.intrinsic = Some(intrinsic);
    } else {
        panic!("no constructor");
    }
}

fn find_class_method(vm: &VM, cls_id: ClassId, name: &str) -> FctId {
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

fn native_module_method(vm: &mut VM, module_id: ModuleId, name: &str, fctptr: *const u8) {
    internal_module_method(
        vm,
        module_id,
        name,
        FctImplementation::Native(Address::from_ptr(fctptr)),
    );
}

fn intrinsic_module_method(vm: &mut VM, module_id: ModuleId, name: &str, intrinsic: Intrinsic) {
    internal_module_method(vm, module_id, name, FctImplementation::Intrinsic(intrinsic));
}

fn internal_module_method(vm: &mut VM, module_id: ModuleId, name: &str, kind: FctImplementation) {
    let module = vm.modules.idx(module_id);
    let module = module.read();
    let name = vm.interner.intern(name);

    for &mid in &module.methods {
        let mtd = vm.fcts.idx(mid);
        let mut mtd = mtd.write();

        if mtd.name == name {
            if mtd.internal {
                match kind {
                    FctImplementation::Intrinsic(intrinsic) => mtd.intrinsic = Some(intrinsic),
                    FctImplementation::Native(address) => {
                        mtd.native_pointer = Some(address);
                    }
                }
                mtd.internal_resolved = true;
                break;
            } else {
                panic!(
                    "method {} found, but was not @internal",
                    vm.interner.str(name)
                )
            }
        }
    }
}

fn find_module_method(vm: &VM, module_id: ModuleId, name: &str) -> FctId {
    let module = vm.modules.idx(module_id);
    let module = module.read();
    let intern_name = vm.interner.intern(name);

    for &mid in &module.methods {
        let mtd = vm.fcts.idx(mid);
        let mtd = mtd.read();

        if mtd.name == intern_name {
            return mid;
        }
    }

    panic!("cannot find module method `{}`", name)
}

fn native_fct(vm: &mut VM, name: &str, fctptr: *const u8, namespace_id: NamespaceId) {
    internal_fct(
        vm,
        name,
        FctImplementation::Native(Address::from_ptr(fctptr)),
        namespace_id,
    );
}

fn intrinsic_fct(vm: &mut VM, name: &str, intrinsic: Intrinsic, namespace_id: NamespaceId) {
    internal_fct(
        vm,
        name,
        FctImplementation::Intrinsic(intrinsic),
        namespace_id,
    );
}

fn internal_fct(vm: &mut VM, name: &str, kind: FctImplementation, namespace_id: NamespaceId) {
    let name = vm.interner.intern(name);
    let fctid = NestedSymTable::new(vm, namespace_id).get_fct(name);

    if let Some(fctid) = fctid {
        let fct = vm.fcts.idx(fctid);
        let mut fct = fct.write();

        if fct.internal {
            match kind {
                FctImplementation::Intrinsic(intrinsic) => fct.intrinsic = Some(intrinsic),
                FctImplementation::Native(address) => {
                    fct.native_pointer = Some(address);
                }
            }
            fct.internal_resolved = true;
        }
    } else {
        panic!("function not found");
    }
}

enum FctImplementation {
    Intrinsic(Intrinsic),
    Native(Address),
}

fn intrinsic_impl(
    vm: &mut VM,
    cls_id: ClassId,
    trait_id: TraitId,
    name: &str,
    intrinsic: Intrinsic,
) {
    internal_impl(
        vm,
        cls_id,
        trait_id,
        name,
        FctImplementation::Intrinsic(intrinsic),
    );
}

fn internal_impl(
    vm: &mut VM,
    cls_id: ClassId,
    trait_id: TraitId,
    name: &str,
    kind: FctImplementation,
) {
    let name = vm.interner.intern(name);
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();

    for &impl_id in &cls.impls {
        let ximpl = vm.impls[impl_id].read();

        if Some(trait_id) == ximpl.trait_id {
            if let Some(&method_id) = ximpl.instance_names.get(&name) {
                let fct = vm.fcts.idx(method_id);
                let mut fct = fct.write();

                if fct.internal && fct.name == name {
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
        }
    }

    panic!("method not found");
}

fn native_method(vm: &VM, container_name: &str, method_name: &str, fctptr: *const u8) {
    common_method(
        vm,
        container_name,
        method_name,
        FctImplementation::Native(Address::from_ptr(fctptr)),
        false,
    );
}

fn intrinsic_method(vm: &VM, container_name: &str, method_name: &str, intrinsic: Intrinsic) {
    common_method(
        vm,
        container_name,
        method_name,
        FctImplementation::Intrinsic(intrinsic),
        false,
    );
}

fn common_method(
    vm: &VM,
    container_name: &str,
    method_name: &str,
    implementation: FctImplementation,
    is_static: bool,
) {
    let container_name_interned = vm.interner.intern(container_name);

    let sym_term = vm
        .stdlib_namespace()
        .read()
        .get_type(container_name_interned)
        .expect("symbol not found");

    match sym_term {
        TypeSym::Class(cls_id) => {
            internal_class_method(vm, cls_id, method_name, is_static, implementation);
        }
        TypeSym::Struct(struct_id) => {
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
        TypeSym::Enum(enum_id) => {
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
    name: &str,
    is_static: bool,
    kind: FctImplementation,
) {
    assert!(!is_static);
    let name = vm.interner.intern(name);

    for &extension_id in extensions {
        let extension = vm.extensions[extension_id].read();

        if let Some(&method_id) = extension.instance_names.get(&name) {
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

    panic!("method not found!")
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
