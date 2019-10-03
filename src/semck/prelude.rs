use parking_lot::RwLock;
use std::sync::Arc;

use crate::class::{ClassDef, ClassDefId, ClassId};
use crate::exception;
use crate::gc::Address;
use crate::object::Header;
use crate::size::InstanceSize;
use crate::stdlib;
use crate::ty::BuiltinType;
use crate::typeparams::TypeParams;
use crate::vm::{FctId, FctKind, Intrinsic, TraitId, VM};
use crate::vtable::VTableBox;

pub fn internal_classes<'ast>(vm: &mut VM<'ast>) {
    vm.vips.bool_class = internal_class(vm, "Bool", Some(BuiltinType::Bool));

    vm.vips.byte_class = internal_class(vm, "Byte", Some(BuiltinType::Byte));
    vm.vips.char_class = internal_class(vm, "Char", Some(BuiltinType::Char));
    vm.vips.int_class = internal_class(vm, "Int", Some(BuiltinType::Int));
    vm.vips.long_class = internal_class(vm, "Long", Some(BuiltinType::Long));

    vm.vips.float_class = internal_class(vm, "Float", Some(BuiltinType::Float));
    vm.vips.double_class = internal_class(vm, "Double", Some(BuiltinType::Double));

    vm.vips.object_class = internal_class(vm, "Object", None);
    vm.vips.string_class = internal_class(vm, "String", None);
    vm.vips.cls.string_buffer = internal_class(vm, "StringBuffer", None);

    let cls = vm.classes.idx(vm.vips.string_class);
    let mut cls = cls.write();
    cls.is_str = true;

    vm.vips.array_class = internal_class(vm, "Array", None);

    let cls = vm.classes.idx(vm.vips.array_class);
    let mut cls = cls.write();
    cls.is_array = true;

    vm.vips.testing_class = internal_class(vm, "Testing", None);

    vm.vips.throwable_class = internal_class(vm, "Throwable", None);
    vm.vips.error_class = internal_class(vm, "Error", None);
    vm.vips.exception_class = internal_class(vm, "Exception", None);
    vm.vips.stack_trace_element_class = internal_class(vm, "StackTraceElement", None);
    vm.vips.stringable_trait = find_trait(vm, "Stringable");

    *vm.vips.iterator_trait.lock() = Some(find_trait(vm, "Iterator"));

    internal_free_classes(vm);
}

pub fn known_methods<'ast>(vm: &mut VM<'ast>) {
    vm.vips.fct.string_buffer_empty = find_static_method(vm, vm.vips.cls.string_buffer, "empty");
    vm.vips.fct.string_buffer_append = find_method(vm, vm.vips.cls.string_buffer, "append");
    vm.vips.fct.string_buffer_to_string = find_method(vm, vm.vips.cls.string_buffer, "toString");
}

fn internal_free_classes<'ast>(vm: &mut VM<'ast>) {
    let free_object: ClassDefId;
    let free_array: ClassDefId;

    {
        let mut class_defs = vm.class_defs.lock();
        let next = class_defs.len();

        free_object = next.into();
        free_array = (next + 1).into();

        class_defs.push(Arc::new(RwLock::new(ClassDef {
            id: free_object,
            cls_id: None,
            type_params: TypeParams::empty(),
            parent_id: None,
            size: InstanceSize::Fixed(Header::size()),
            fields: Vec::new(),
            ref_fields: Vec::new(),
            vtable: None,
        })));

        class_defs.push(Arc::new(RwLock::new(ClassDef {
            id: free_array,
            cls_id: None,
            type_params: TypeParams::empty(),
            parent_id: None,
            size: InstanceSize::FreeArray,
            fields: Vec::new(),
            ref_fields: Vec::new(),
            vtable: None,
        })));

        {
            let free_object_class_def = &class_defs[free_object.to_usize()];
            let mut free_object_class_def = free_object_class_def.write();
            let clsptr = (&*free_object_class_def) as *const ClassDef as *mut ClassDef;
            let vtable = VTableBox::new(clsptr, &[]);
            free_object_class_def.vtable = Some(vtable);
        }

        {
            let free_array_class_def = &class_defs[free_array.to_usize()];
            let mut free_array_class_def = free_array_class_def.write();
            let clsptr = (&*free_array_class_def) as *const ClassDef as *mut ClassDef;
            let vtable = VTableBox::new(clsptr, &[]);
            free_array_class_def.vtable = Some(vtable);
        }
    }

    vm.vips.free_object_class_def = free_object;
    vm.vips.free_array_class_def = free_array;
}

fn internal_class<'ast>(vm: &mut VM<'ast>, name: &str, ty: Option<BuiltinType>) -> ClassId {
    let iname = vm.interner.intern(name);
    let clsid = vm.sym.lock().get_class(iname);

    if let Some(clsid) = clsid {
        let cls = vm.classes.idx(clsid);
        let mut cls = cls.write();

        if cls.internal {
            if let Some(ty) = ty {
                cls.ty = ty;
            }

            cls.internal_resolved = true;
        }

        clsid
    } else {
        panic!("class {} not found!", name);
    }
}

fn find_trait<'ast>(vm: &mut VM<'ast>, name: &str) -> TraitId {
    let iname = vm.interner.intern(name);

    let tid = vm.sym.lock().get_trait(iname);

    if let Some(tid) = tid {
        tid
    } else {
        panic!("trait {} not found!", name);
    }
}

pub fn internal_functions<'ast>(vm: &mut VM<'ast>) {
    native_fct(vm, "fatalError", stdlib::fatal_error as *const u8);
    native_fct(vm, "abort", stdlib::abort as *const u8);
    native_fct(vm, "exit", stdlib::exit as *const u8);

    native_fct(vm, "print", stdlib::print as *const u8);
    native_fct(vm, "println", stdlib::println as *const u8);
    native_fct(vm, "addressOf", stdlib::addr as *const u8);
    intrinsic_fct(vm, "assert", Intrinsic::Assert);
    intrinsic_fct(vm, "debug", Intrinsic::Debug);
    native_fct(vm, "argc", stdlib::argc as *const u8);
    native_fct(vm, "argv", stdlib::argv as *const u8);
    native_fct(vm, "forceCollect", stdlib::gc_collect as *const u8);
    native_fct(vm, "timestamp", stdlib::timestamp as *const u8);
    native_fct(
        vm,
        "forceMinorCollect",
        stdlib::gc_minor_collect as *const u8,
    );
    native_fct(vm, "sleep", stdlib::sleep as *const u8);

    native_fct(vm, "call", stdlib::call as *const u8);
    native_fct(vm, "throwFromNative", stdlib::throw_native as *const u8);
    native_fct(
        vm,
        "throwFromNativeButNotThrows",
        stdlib::throw_native as *const u8,
    );

    native_fct(vm, "loadFunction", stdlib::load_function as *const u8);
    native_fct(vm, "call0", stdlib::call0 as *const u8);
    native_fct(vm, "call1", stdlib::call1 as *const u8);
    native_fct(vm, "call2", stdlib::call2 as *const u8);
    native_fct(vm, "call3", stdlib::call3 as *const u8);

    native_fct(vm, "native_malloc", stdlib::native_malloc as *const u8);
    native_fct(vm, "native_free", stdlib::native_free as *const u8);
    intrinsic_fct(vm, "set_uint8", Intrinsic::SetUint8);
    intrinsic_fct(vm, "defaultValue", Intrinsic::DefaultValue);

    let clsid = vm.vips.byte_class;
    native_method(vm, clsid, "toString", stdlib::byte_to_string as *const u8);
    intrinsic_method(vm, clsid, "toLong", Intrinsic::ByteToLong);
    intrinsic_method(vm, clsid, "toInt", Intrinsic::ByteToInt);

    intrinsic_method(vm, clsid, "equals", Intrinsic::ByteEq);
    intrinsic_method(vm, clsid, "compareTo", Intrinsic::ByteCmp);
    intrinsic_method(vm, clsid, "not", Intrinsic::ByteNot);

    let clsid = vm.vips.char_class;
    native_method(vm, clsid, "toString", stdlib::char_to_string as *const u8);
    intrinsic_method(vm, clsid, "toLong", Intrinsic::CharToLong);
    intrinsic_method(vm, clsid, "toInt", Intrinsic::CharToInt);

    intrinsic_method(vm, clsid, "equals", Intrinsic::CharEq);
    intrinsic_method(vm, clsid, "compareTo", Intrinsic::CharCmp);

    let clsid = vm.vips.int_class;
    intrinsic_method(vm, clsid, "toByte", Intrinsic::IntToByte);
    intrinsic_method(vm, clsid, "toCharUnchecked", Intrinsic::IntToChar);
    intrinsic_method(vm, clsid, "toLong", Intrinsic::IntToLong);
    native_method(vm, clsid, "toString", stdlib::int_to_string as *const u8);

    intrinsic_method(vm, clsid, "toFloat", Intrinsic::IntToFloat);
    intrinsic_method(vm, clsid, "toDouble", Intrinsic::IntToDouble);

    intrinsic_method(vm, clsid, "asFloat", Intrinsic::IntAsFloat);

    intrinsic_method(vm, clsid, "equals", Intrinsic::IntEq);
    intrinsic_method(vm, clsid, "compareTo", Intrinsic::IntCmp);

    intrinsic_method(vm, clsid, "plus", Intrinsic::IntAdd);
    intrinsic_method(vm, clsid, "minus", Intrinsic::IntSub);
    intrinsic_method(vm, clsid, "times", Intrinsic::IntMul);
    intrinsic_method(vm, clsid, "div", Intrinsic::IntDiv);
    intrinsic_method(vm, clsid, "mod", Intrinsic::IntMod);

    intrinsic_method(vm, clsid, "bitwiseOr", Intrinsic::IntOr);
    intrinsic_method(vm, clsid, "bitwiseAnd", Intrinsic::IntAnd);
    intrinsic_method(vm, clsid, "bitwiseXor", Intrinsic::IntXor);

    intrinsic_method(vm, clsid, "shiftLeft", Intrinsic::IntShl);
    intrinsic_method(vm, clsid, "shiftRight", Intrinsic::IntSar);
    intrinsic_method(vm, clsid, "unsignedShiftRight", Intrinsic::IntShr);

    intrinsic_method(vm, clsid, "unaryPlus", Intrinsic::IntPlus);
    intrinsic_method(vm, clsid, "unaryMinus", Intrinsic::IntNeg);
    intrinsic_method(vm, clsid, "not", Intrinsic::IntNot);

    let clsid = vm.vips.long_class;
    native_method(vm, clsid, "toString", stdlib::long_to_string as *const u8);
    intrinsic_method(vm, clsid, "toCharUnchecked", Intrinsic::LongToChar);
    intrinsic_method(vm, clsid, "toInt", Intrinsic::LongToInt);
    intrinsic_method(vm, clsid, "toByte", Intrinsic::LongToByte);

    intrinsic_method(vm, clsid, "toFloat", Intrinsic::LongToFloat);
    intrinsic_method(vm, clsid, "toDouble", Intrinsic::LongToDouble);

    intrinsic_method(vm, clsid, "asDouble", Intrinsic::LongAsDouble);

    intrinsic_method(vm, clsid, "equals", Intrinsic::LongEq);
    intrinsic_method(vm, clsid, "compareTo", Intrinsic::LongCmp);

    intrinsic_method(vm, clsid, "plus", Intrinsic::LongAdd);
    intrinsic_method(vm, clsid, "minus", Intrinsic::LongSub);
    intrinsic_method(vm, clsid, "times", Intrinsic::LongMul);
    intrinsic_method(vm, clsid, "div", Intrinsic::LongDiv);
    intrinsic_method(vm, clsid, "mod", Intrinsic::LongMod);

    intrinsic_method(vm, clsid, "bitwiseOr", Intrinsic::LongOr);
    intrinsic_method(vm, clsid, "bitwiseAnd", Intrinsic::LongAnd);
    intrinsic_method(vm, clsid, "bitwiseXor", Intrinsic::LongXor);

    intrinsic_method(vm, clsid, "shiftLeft", Intrinsic::LongShl);
    intrinsic_method(vm, clsid, "shiftRight", Intrinsic::LongSar);
    intrinsic_method(vm, clsid, "unsignedShiftRight", Intrinsic::LongShr);

    intrinsic_method(vm, clsid, "unaryPlus", Intrinsic::LongPlus);
    intrinsic_method(vm, clsid, "unaryMinus", Intrinsic::LongNeg);
    intrinsic_method(vm, clsid, "not", Intrinsic::LongNot);

    let clsid = vm.vips.bool_class;
    intrinsic_method(vm, clsid, "toInt", Intrinsic::BoolToInt);
    intrinsic_method(vm, clsid, "toLong", Intrinsic::BoolToLong);
    intrinsic_method(vm, clsid, "equals", Intrinsic::BoolEq);
    intrinsic_method(vm, clsid, "not", Intrinsic::BoolNot);

    let clsid = vm.vips.string_class;
    native_method(vm, clsid, "compareTo", stdlib::strcmp as *const u8);
    native_method(vm, clsid, "parseInt", stdlib::str_parse_int as *const u8);
    native_method(vm, clsid, "parseLong", stdlib::str_parse_long as *const u8);
    native_method(vm, clsid, "plus", stdlib::strcat as *const u8);

    intrinsic_method(vm, clsid, "length", Intrinsic::StrLen);
    intrinsic_method(vm, clsid, "getByte", Intrinsic::StrGet);
    native_method(vm, clsid, "clone", stdlib::str_clone as *const u8);
    native_method(
        vm,
        clsid,
        "fromBytesPartOrNull",
        stdlib::str_from_bytes as *const u8,
    );
    native_method(
        vm,
        clsid,
        "fromStringPartOrNull",
        stdlib::str_from_bytes as *const u8,
    );

    let clsid = vm.vips.float_class;
    native_method(vm, clsid, "toString", stdlib::float_to_string as *const u8);
    intrinsic_method(vm, clsid, "toInt", Intrinsic::FloatToInt);
    intrinsic_method(vm, clsid, "toLong", Intrinsic::FloatToLong);
    intrinsic_method(vm, clsid, "toDouble", Intrinsic::FloatToDouble);

    intrinsic_method(vm, clsid, "asInt", Intrinsic::FloatAsInt);

    intrinsic_method(vm, clsid, "equals", Intrinsic::FloatEq);
    intrinsic_method(vm, clsid, "compareTo", Intrinsic::FloatCmp);

    intrinsic_method(vm, clsid, "plus", Intrinsic::FloatAdd);
    intrinsic_method(vm, clsid, "minus", Intrinsic::FloatSub);
    intrinsic_method(vm, clsid, "times", Intrinsic::FloatMul);
    intrinsic_method(vm, clsid, "div", Intrinsic::FloatDiv);

    intrinsic_method(vm, clsid, "unaryPlus", Intrinsic::FloatPlus);
    intrinsic_method(vm, clsid, "unaryMinus", Intrinsic::FloatNeg);

    intrinsic_method(vm, clsid, "isNan", Intrinsic::FloatIsNan);
    intrinsic_method(vm, clsid, "sqrt", Intrinsic::FloatSqrt);

    let clsid = vm.vips.double_class;
    native_method(vm, clsid, "toString", stdlib::double_to_string as *const u8);
    intrinsic_method(vm, clsid, "toInt", Intrinsic::DoubleToInt);
    intrinsic_method(vm, clsid, "toLong", Intrinsic::DoubleToLong);
    intrinsic_method(vm, clsid, "toFloat", Intrinsic::DoubleToFloat);

    intrinsic_method(vm, clsid, "asLong", Intrinsic::DoubleAsLong);

    intrinsic_method(vm, clsid, "equals", Intrinsic::DoubleEq);
    intrinsic_method(vm, clsid, "compareTo", Intrinsic::DoubleCmp);

    intrinsic_method(vm, clsid, "plus", Intrinsic::DoubleAdd);
    intrinsic_method(vm, clsid, "minus", Intrinsic::DoubleSub);
    intrinsic_method(vm, clsid, "times", Intrinsic::DoubleMul);
    intrinsic_method(vm, clsid, "div", Intrinsic::DoubleDiv);

    intrinsic_method(vm, clsid, "unaryPlus", Intrinsic::DoublePlus);
    intrinsic_method(vm, clsid, "unaryMinus", Intrinsic::DoubleNeg);

    intrinsic_method(vm, clsid, "isNan", Intrinsic::DoubleIsNan);
    intrinsic_method(vm, clsid, "sqrt", Intrinsic::DoubleSqrt);

    let clsid = vm.vips.array_class;
    intrinsic_method(vm, clsid, "length", Intrinsic::GenericArrayLen);
    intrinsic_method(vm, clsid, "get", Intrinsic::GenericArrayGet);
    intrinsic_method(vm, clsid, "set", Intrinsic::GenericArraySet);

    let clsid = vm.vips.throwable_class;
    native_method(
        vm,
        clsid,
        "retrieveStackTrace",
        exception::retrieve_stack_trace as *const u8,
    );
    native_method(
        vm,
        clsid,
        "getStackTraceElement",
        exception::stack_element as *const u8,
    );

    let iname = vm.interner.intern("Thread");
    let clsid = vm.sym.lock().get_class(iname);

    if let Some(clsid) = clsid {
        native_method(vm, clsid, "start", stdlib::spawn_thread as *const u8);
    }
}

fn native_method<'ast>(vm: &mut VM<'ast>, clsid: ClassId, name: &str, fctptr: *const u8) {
    internal_method(vm, clsid, name, FctKind::Native(Address::from_ptr(fctptr)));
}

fn intrinsic_method<'ast>(vm: &mut VM<'ast>, clsid: ClassId, name: &str, intrinsic: Intrinsic) {
    internal_method(vm, clsid, name, FctKind::Builtin(intrinsic));
}

fn internal_method<'ast>(vm: &mut VM<'ast>, clsid: ClassId, name: &str, kind: FctKind) {
    let cls = vm.classes.idx(clsid);
    let cls = cls.read();
    let name = vm.interner.intern(name);

    for &mid in &cls.methods {
        let mtd = vm.fcts.idx(mid);
        let mut mtd = mtd.write();

        if mtd.name == name && mtd.internal {
            mtd.kind = kind;
            mtd.internal_resolved = true;
            break;
        }
    }
}

fn find_method<'ast>(vm: &VM<'ast>, clsid: ClassId, name: &str) -> FctId {
    find_method_internal(vm, clsid, name, false)
}

fn find_static_method<'ast>(vm: &VM<'ast>, clsid: ClassId, name: &str) -> FctId {
    find_method_internal(vm, clsid, name, true)
}

fn find_method_internal<'ast>(vm: &VM<'ast>, clsid: ClassId, name: &str, is_static: bool) -> FctId {
    let cls = vm.classes.idx(clsid);
    let cls = cls.read();
    let intern_name = vm.interner.intern(name);

    for &mid in &cls.methods {
        let mtd = vm.fcts.idx(mid);
        let mtd = mtd.read();

        if mtd.name == intern_name && mtd.is_static == is_static {
            return mid;
        }
    }

    panic!("cannot find method `{}`", name)
}

fn native_fct<'ast>(vm: &mut VM<'ast>, name: &str, fctptr: *const u8) {
    internal_fct(vm, name, FctKind::Native(Address::from_ptr(fctptr)));
}

fn intrinsic_fct<'ast>(vm: &mut VM<'ast>, name: &str, intrinsic: Intrinsic) {
    internal_fct(vm, name, FctKind::Builtin(intrinsic));
}

fn internal_fct<'ast>(vm: &mut VM<'ast>, name: &str, kind: FctKind) {
    let name = vm.interner.intern(name);
    let fctid = vm.sym.lock().get_fct(name);

    if let Some(fctid) = fctid {
        let fct = vm.fcts.idx(fctid);
        let mut fct = fct.write();

        if fct.internal {
            fct.kind = kind;
            fct.internal_resolved = true;
        }
    }
}

fn intrinsic_impl<'ast>(
    vm: &mut VM<'ast>,
    clsid: ClassId,
    tid: TraitId,
    name: &str,
    intrinsic: Intrinsic,
) {
    internal_impl(vm, clsid, tid, name, FctKind::Builtin(intrinsic));
}

fn internal_impl<'ast>(vm: &mut VM<'ast>, clsid: ClassId, tid: TraitId, name: &str, kind: FctKind) {
    let name = vm.interner.intern(name);
    let cls = vm.classes.idx(clsid);
    let cls = cls.read();

    for &iid in &cls.impls {
        let i = vm.impls[iid].read();

        if Some(tid) == i.trait_id {
            for &fid in &i.methods {
                let fct = vm.fcts.idx(fid);
                let mut fct = fct.write();

                if fct.internal && fct.name == name {
                    fct.kind = kind;
                    fct.internal_resolved = true;
                    return;
                }
            }
        }
    }
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
