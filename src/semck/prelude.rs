use class::ClassId;
use ctxt::{Context, FctKind, Intrinsic};
use stdlib;
use ty::BuiltinType;

pub fn internal_classes<'ast>(ctxt: &mut Context<'ast>) {
    let size = BuiltinType::Bool.size(ctxt);
    ctxt.primitive_classes.bool_class = internal_class(ctxt, "bool", BuiltinType::Bool, size);

    let size = BuiltinType::Byte.size(ctxt);
    ctxt.primitive_classes.byte_class = internal_class(ctxt, "byte", BuiltinType::Byte, size);

    let size = BuiltinType::Int.size(ctxt);
    ctxt.primitive_classes.int_class = internal_class(ctxt, "int", BuiltinType::Int, size);

    let size = BuiltinType::Long.size(ctxt);
    ctxt.primitive_classes.long_class = internal_class(ctxt, "long", BuiltinType::Long, size);

    let size = BuiltinType::Float.size(ctxt);
    ctxt.primitive_classes.float_class = internal_class(ctxt, "float", BuiltinType::Float, size);

    let size = BuiltinType::Double.size(ctxt);
    ctxt.primitive_classes.double_class = internal_class(ctxt, "double", BuiltinType::Double, size);

    ctxt.primitive_classes.str_class = internal_class(ctxt, "Str", BuiltinType::Str, 0);
    ctxt.primitive_classes.byte_array =
        internal_class(ctxt, "ByteArray", BuiltinType::ByteArray, 0);
    ctxt.primitive_classes.int_array = internal_class(ctxt, "IntArray", BuiltinType::IntArray, 0);
    ctxt.primitive_classes.long_array =
        internal_class(ctxt, "LongArray", BuiltinType::LongArray, 0);
}

fn internal_class<'ast>(ctxt: &mut Context<'ast>,
                        name: &str,
                        ty: BuiltinType,
                        size: i32)
                        -> ClassId {
    let iname = ctxt.interner.intern(name);
    let clsid = ctxt.sym.borrow().get_class(iname);

    if let Some(clsid) = clsid {
        let mut cls = ctxt.classes[clsid].borrow_mut();

        if cls.internal {
            cls.ty = ty;
            cls.size = size;
            cls.internal_resolved = true;
        }

        clsid
    } else {
        panic!("class {} not found!", name);
    }
}

pub fn internal_functions<'ast>(ctxt: &mut Context<'ast>) {
    native_fct(ctxt, "print", stdlib::print as *const u8);
    native_fct(ctxt, "println", stdlib::println as *const u8);
    intrinsic_fct(ctxt, "assert", Intrinsic::Assert);
    native_fct(ctxt, "argc", stdlib::argc as *const u8);
    native_fct(ctxt, "argv", stdlib::argv as *const u8);
    native_fct(ctxt, "forceCollect", stdlib::gc_collect as *const u8);
    native_fct(ctxt,
               "intArrayWith",
               stdlib::ctor_int_array_elem as *const u8);
    native_fct(ctxt,
               "emptyIntArray",
               stdlib::ctor_int_array_empty as *const u8);
    native_fct(ctxt,
               "byteArrayWith",
               stdlib::ctor_byte_array_elem as *const u8);
    native_fct(ctxt,
               "emptyByteArray",
               stdlib::ctor_byte_array_empty as *const u8);
    native_fct(ctxt,
               "longArrayWith",
               stdlib::ctor_long_array_elem as *const u8);
    native_fct(ctxt,
               "emptyLongArray",
               stdlib::ctor_long_array_empty as *const u8);

    native_fct(ctxt, "loadFunction", stdlib::load_function as *const u8);
    native_fct(ctxt, "call0", stdlib::call0 as *const u8);
    native_fct(ctxt, "call1", stdlib::call1 as *const u8);
    native_fct(ctxt, "call2", stdlib::call2 as *const u8);
    native_fct(ctxt, "call3", stdlib::call3 as *const u8);

    native_fct(ctxt, "native_malloc", stdlib::native_malloc as *const u8);
    native_fct(ctxt, "native_free", stdlib::native_free as *const u8);
    intrinsic_fct(ctxt, "set_uint8", Intrinsic::SetUint8);

    let clsid = ctxt.primitive_classes.byte_class;
    native_method(ctxt, clsid, "toString", stdlib::byte_to_string as *const u8);
    intrinsic_method(ctxt, clsid, "toLong", Intrinsic::ByteToLong);
    intrinsic_method(ctxt, clsid, "toInt", Intrinsic::ByteToInt);

    intrinsic_method(ctxt, clsid, "equals", Intrinsic::ByteEq);
    intrinsic_method(ctxt, clsid, "compareTo", Intrinsic::ByteCmp);
    intrinsic_method(ctxt, clsid, "not", Intrinsic::ByteNot);

    let clsid = ctxt.primitive_classes.int_class;
    intrinsic_method(ctxt, clsid, "toLong", Intrinsic::IntToLong);
    intrinsic_method(ctxt, clsid, "toByte", Intrinsic::IntToByte);
    native_method(ctxt, clsid, "toString", stdlib::int_to_string as *const u8);

    intrinsic_method(ctxt, clsid, "equals", Intrinsic::IntEq);
    intrinsic_method(ctxt, clsid, "compareTo", Intrinsic::IntCmp);

    intrinsic_method(ctxt, clsid, "plus", Intrinsic::IntAdd);
    intrinsic_method(ctxt, clsid, "minus", Intrinsic::IntSub);
    intrinsic_method(ctxt, clsid, "times", Intrinsic::IntMul);
    intrinsic_method(ctxt, clsid, "div", Intrinsic::IntDiv);
    intrinsic_method(ctxt, clsid, "mod", Intrinsic::IntMod);

    intrinsic_method(ctxt, clsid, "bitwiseOr", Intrinsic::IntOr);
    intrinsic_method(ctxt, clsid, "bitwiseAnd", Intrinsic::IntAnd);
    intrinsic_method(ctxt, clsid, "bitwiseXor", Intrinsic::IntXor);

    intrinsic_method(ctxt, clsid, "shiftLeft", Intrinsic::IntShl);
    intrinsic_method(ctxt, clsid, "shiftRight", Intrinsic::IntSar);
    intrinsic_method(ctxt, clsid, "unsignedShiftRight", Intrinsic::IntShr);

    intrinsic_method(ctxt, clsid, "unaryPlus", Intrinsic::IntPlus);
    intrinsic_method(ctxt, clsid, "unaryMinus", Intrinsic::IntNeg);
    intrinsic_method(ctxt, clsid, "not", Intrinsic::IntNot);

    let clsid = ctxt.primitive_classes.long_class;
    native_method(ctxt, clsid, "toString", stdlib::long_to_string as *const u8);
    intrinsic_method(ctxt, clsid, "toInt", Intrinsic::LongToInt);
    intrinsic_method(ctxt, clsid, "toByte", Intrinsic::LongToByte);

    intrinsic_method(ctxt, clsid, "equals", Intrinsic::LongEq);
    intrinsic_method(ctxt, clsid, "compareTo", Intrinsic::LongCmp);

    intrinsic_method(ctxt, clsid, "plus", Intrinsic::LongAdd);
    intrinsic_method(ctxt, clsid, "minus", Intrinsic::LongSub);
    intrinsic_method(ctxt, clsid, "times", Intrinsic::LongMul);
    intrinsic_method(ctxt, clsid, "div", Intrinsic::LongDiv);
    intrinsic_method(ctxt, clsid, "mod", Intrinsic::LongMod);

    intrinsic_method(ctxt, clsid, "bitwiseOr", Intrinsic::LongOr);
    intrinsic_method(ctxt, clsid, "bitwiseAnd", Intrinsic::LongAnd);
    intrinsic_method(ctxt, clsid, "bitwiseXor", Intrinsic::LongXor);

    intrinsic_method(ctxt, clsid, "shiftLeft", Intrinsic::LongShl);
    intrinsic_method(ctxt, clsid, "shiftRight", Intrinsic::LongSar);
    intrinsic_method(ctxt, clsid, "unsignedShiftRight", Intrinsic::LongShr);

    intrinsic_method(ctxt, clsid, "unaryPlus", Intrinsic::LongPlus);
    intrinsic_method(ctxt, clsid, "unaryMinus", Intrinsic::LongNeg);
    intrinsic_method(ctxt, clsid, "not", Intrinsic::LongNot);

    let clsid = ctxt.primitive_classes.bool_class;
    intrinsic_method(ctxt, clsid, "toInt", Intrinsic::BoolToInt);
    intrinsic_method(ctxt, clsid, "toLong", Intrinsic::BoolToLong);
    native_method(ctxt, clsid, "toString", stdlib::bool_to_string as *const u8);
    intrinsic_method(ctxt, clsid, "equals", Intrinsic::BoolEq);
    intrinsic_method(ctxt, clsid, "not", Intrinsic::BoolNot);

    let clsid = ctxt.primitive_classes.str_class;
    native_method(ctxt, clsid, "equals", stdlib::streq as *const u8);
    native_method(ctxt, clsid, "compareTo", stdlib::strcmp as *const u8);
    native_method(ctxt, clsid, "parseInt", stdlib::str_parse_int as *const u8);
    native_method(ctxt, clsid, "plus", stdlib::strcat as *const u8);

    intrinsic_method(ctxt, clsid, "len", Intrinsic::StrLen);
    intrinsic_method(ctxt, clsid, "get", Intrinsic::StrGet);
    intrinsic_method(ctxt, clsid, "set", Intrinsic::StrSet);

    let clsid = ctxt.primitive_classes.float_class;
    intrinsic_method(ctxt, clsid, "equals", Intrinsic::FloatEq);
    intrinsic_method(ctxt, clsid, "compareTo", Intrinsic::FloatCmp);

    intrinsic_method(ctxt, clsid, "plus", Intrinsic::FloatAdd);
    intrinsic_method(ctxt, clsid, "minus", Intrinsic::FloatSub);
    intrinsic_method(ctxt, clsid, "times", Intrinsic::FloatMul);
    intrinsic_method(ctxt, clsid, "div", Intrinsic::FloatDiv);

    let clsid = ctxt.primitive_classes.double_class;
    intrinsic_method(ctxt, clsid, "equals", Intrinsic::DoubleEq);
    intrinsic_method(ctxt, clsid, "compareTo", Intrinsic::DoubleCmp);

    intrinsic_method(ctxt, clsid, "plus", Intrinsic::DoubleAdd);
    intrinsic_method(ctxt, clsid, "minus", Intrinsic::DoubleSub);
    intrinsic_method(ctxt, clsid, "times", Intrinsic::DoubleMul);
    intrinsic_method(ctxt, clsid, "div", Intrinsic::DoubleDiv);

    let clsid = ctxt.primitive_classes.int_array;
    intrinsic_method(ctxt, clsid, "len", Intrinsic::IntArrayLen);
    intrinsic_method(ctxt, clsid, "get", Intrinsic::IntArrayGet);
    intrinsic_method(ctxt, clsid, "set", Intrinsic::IntArraySet);

    let clsid = ctxt.primitive_classes.byte_array;
    intrinsic_method(ctxt, clsid, "len", Intrinsic::ByteArrayLen);
    intrinsic_method(ctxt, clsid, "get", Intrinsic::ByteArrayGet);
    intrinsic_method(ctxt, clsid, "set", Intrinsic::ByteArraySet);

    let clsid = ctxt.primitive_classes.long_array;
    intrinsic_method(ctxt, clsid, "len", Intrinsic::LongArrayLen);
    intrinsic_method(ctxt, clsid, "get", Intrinsic::LongArrayGet);
    intrinsic_method(ctxt, clsid, "set", Intrinsic::LongArraySet);
}

fn native_method<'ast>(ctxt: &mut Context<'ast>, clsid: ClassId, name: &str, fctptr: *const u8) {
    internal_method(ctxt, clsid, name, FctKind::Native(fctptr));
}

fn intrinsic_method<'ast>(ctxt: &mut Context<'ast>,
                          clsid: ClassId,
                          name: &str,
                          intrinsic: Intrinsic) {
    internal_method(ctxt, clsid, name, FctKind::Builtin(intrinsic));
}

fn internal_method<'ast>(ctxt: &mut Context<'ast>,
                         clsid: ClassId,
                         name: &str,
                         kind: FctKind<'ast>) {
    let cls = ctxt.classes[clsid].borrow();
    let name = ctxt.interner.intern(name);

    for &mid in &cls.methods {
        let mut mtd = ctxt.fcts[mid].borrow_mut();

        if mtd.name == name && mtd.internal {
            mtd.kind = kind;
            mtd.internal_resolved = true;
            break;
        }
    }
}

fn native_fct<'ast>(ctxt: &mut Context<'ast>, name: &str, fctptr: *const u8) {
    internal_fct(ctxt, name, FctKind::Native(fctptr));
}

fn intrinsic_fct<'ast>(ctxt: &mut Context<'ast>, name: &str, intrinsic: Intrinsic) {
    internal_fct(ctxt, name, FctKind::Builtin(intrinsic));
}

fn internal_fct<'ast>(ctxt: &mut Context<'ast>, name: &str, kind: FctKind<'ast>) {
    let name = ctxt.interner.intern(name);
    let fctid = ctxt.sym.borrow().get_fct(name);

    if let Some(fctid) = fctid {
        let mut fct = ctxt.fcts[fctid].borrow_mut();

        if fct.internal {
            fct.kind = kind;
            fct.internal_resolved = true;
        }
    }
}

#[cfg(test)]
mod tests {
    use semck::tests::*;

    #[test]
    fn builtin_functions() {
        ok("fun f() { assert(true); }");
        ok("fun f() { print(\"test\"); }");
        ok("fun f() { println(\"test\"); }");
    }
}
