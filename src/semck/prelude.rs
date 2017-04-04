use class::ClassId;
use ctxt::{Context, FctKind, Intrinsic};
use stdlib;
use ty::BuiltinType;

pub fn internal_classes<'ast>(ctxt: &mut Context<'ast>) {
    let size = BuiltinType::Bool.size(ctxt);
    ctxt.primitive_classes.bool_class = internal_class(ctxt, "bool", Some(BuiltinType::Bool), size);

    let size = BuiltinType::Byte.size(ctxt);
    ctxt.primitive_classes.byte_class = internal_class(ctxt, "byte", Some(BuiltinType::Byte), size);

    let size = BuiltinType::Char.size(ctxt);
    ctxt.primitive_classes.char_class = internal_class(ctxt, "char", Some(BuiltinType::Char), size);

    let size = BuiltinType::Int.size(ctxt);
    ctxt.primitive_classes.int_class = internal_class(ctxt, "int", Some(BuiltinType::Int), size);

    let size = BuiltinType::Long.size(ctxt);
    ctxt.primitive_classes.long_class = internal_class(ctxt, "long", Some(BuiltinType::Long), size);

    let size = BuiltinType::Float.size(ctxt);
    ctxt.primitive_classes.float_class =
        internal_class(ctxt, "float", Some(BuiltinType::Float), size);

    let size = BuiltinType::Double.size(ctxt);
    ctxt.primitive_classes.double_class =
        internal_class(ctxt, "double", Some(BuiltinType::Double), size);

    ctxt.primitive_classes.str_class = internal_class(ctxt, "Str", None, 0);
    ctxt.primitive_classes.generic_array = internal_class(ctxt, "Array", None, 0);
}

fn internal_class<'ast>(ctxt: &mut Context<'ast>,
                        name: &str,
                        ty: Option<BuiltinType>,
                        size: i32)
                        -> ClassId {
    let iname = ctxt.interner.intern(name);
    let clsid = ctxt.sym.borrow().get_class(iname);

    if let Some(clsid) = clsid {
        let mut cls = ctxt.classes[clsid].borrow_mut();

        if cls.internal {
            if let Some(ty) = ty {
                cls.ty = ty;
            }

            cls.size = size;
            cls.internal_resolved = true;
        }

        clsid
    } else {
        panic!("class {} not found!", name);
    }
}

pub fn internal_functions<'ast>(ctxt: &mut Context<'ast>) {
    native_fct(ctxt, "fatalError", stdlib::fatal_error as *const u8);
    native_fct(ctxt, "abort", stdlib::abort as *const u8);
    native_fct(ctxt, "exit", stdlib::exit as *const u8);

    native_fct(ctxt, "print", stdlib::print as *const u8);
    native_fct(ctxt, "println", stdlib::println as *const u8);
    intrinsic_fct(ctxt, "assert", Intrinsic::Assert);
    native_fct(ctxt, "argc", stdlib::argc as *const u8);
    native_fct(ctxt, "argv", stdlib::argv as *const u8);
    native_fct(ctxt, "forceCollect", stdlib::gc_collect as *const u8);

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

    let clsid = ctxt.primitive_classes.char_class;
    native_method(ctxt, clsid, "toString", stdlib::char_to_string as *const u8);
    intrinsic_method(ctxt, clsid, "toLong", Intrinsic::CharToLong);
    intrinsic_method(ctxt, clsid, "toInt", Intrinsic::CharToInt);

    intrinsic_method(ctxt, clsid, "equals", Intrinsic::CharEq);
    intrinsic_method(ctxt, clsid, "compareTo", Intrinsic::CharCmp);

    let clsid = ctxt.primitive_classes.int_class;
    intrinsic_method(ctxt, clsid, "toByte", Intrinsic::IntToByte);
    intrinsic_method(ctxt, clsid, "toChar", Intrinsic::IntToChar);
    intrinsic_method(ctxt, clsid, "toLong", Intrinsic::IntToLong);
    native_method(ctxt, clsid, "toString", stdlib::int_to_string as *const u8);

    intrinsic_method(ctxt, clsid, "toFloat", Intrinsic::IntToFloat);
    intrinsic_method(ctxt, clsid, "toDouble", Intrinsic::IntToDouble);

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
    intrinsic_method(ctxt, clsid, "toChar", Intrinsic::LongToChar);
    intrinsic_method(ctxt, clsid, "toInt", Intrinsic::LongToInt);
    intrinsic_method(ctxt, clsid, "toByte", Intrinsic::LongToByte);

    intrinsic_method(ctxt, clsid, "toFloat", Intrinsic::LongToFloat);
    intrinsic_method(ctxt, clsid, "toDouble", Intrinsic::LongToDouble);

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
    native_method(ctxt,
                  clsid,
                  "toString",
                  stdlib::float_to_string as *const u8);
    intrinsic_method(ctxt, clsid, "toInt", Intrinsic::FloatToInt);
    intrinsic_method(ctxt, clsid, "toLong", Intrinsic::FloatToLong);
    intrinsic_method(ctxt, clsid, "toDouble", Intrinsic::FloatToDouble);

    intrinsic_method(ctxt, clsid, "equals", Intrinsic::FloatEq);
    intrinsic_method(ctxt, clsid, "compareTo", Intrinsic::FloatCmp);

    intrinsic_method(ctxt, clsid, "plus", Intrinsic::FloatAdd);
    intrinsic_method(ctxt, clsid, "minus", Intrinsic::FloatSub);
    intrinsic_method(ctxt, clsid, "times", Intrinsic::FloatMul);
    intrinsic_method(ctxt, clsid, "div", Intrinsic::FloatDiv);

    intrinsic_method(ctxt, clsid, "unaryPlus", Intrinsic::FloatPlus);
    intrinsic_method(ctxt, clsid, "unaryMinus", Intrinsic::FloatNeg);

    intrinsic_method(ctxt, clsid, "isNan", Intrinsic::FloatIsNan);
    intrinsic_method(ctxt, clsid, "sqrt", Intrinsic::FloatSqrt);

    let clsid = ctxt.primitive_classes.double_class;
    native_method(ctxt,
                  clsid,
                  "toString",
                  stdlib::double_to_string as *const u8);
    intrinsic_method(ctxt, clsid, "toInt", Intrinsic::DoubleToInt);
    intrinsic_method(ctxt, clsid, "toLong", Intrinsic::DoubleToLong);
    intrinsic_method(ctxt, clsid, "toFloat", Intrinsic::DoubleToFloat);

    intrinsic_method(ctxt, clsid, "equals", Intrinsic::DoubleEq);
    intrinsic_method(ctxt, clsid, "compareTo", Intrinsic::DoubleCmp);

    intrinsic_method(ctxt, clsid, "plus", Intrinsic::DoubleAdd);
    intrinsic_method(ctxt, clsid, "minus", Intrinsic::DoubleSub);
    intrinsic_method(ctxt, clsid, "times", Intrinsic::DoubleMul);
    intrinsic_method(ctxt, clsid, "div", Intrinsic::DoubleDiv);

    intrinsic_method(ctxt, clsid, "unaryPlus", Intrinsic::DoublePlus);
    intrinsic_method(ctxt, clsid, "unaryMinus", Intrinsic::DoubleNeg);

    intrinsic_method(ctxt, clsid, "isNan", Intrinsic::DoubleIsNan);
    intrinsic_method(ctxt, clsid, "sqrt", Intrinsic::DoubleSqrt);

    let clsid = ctxt.primitive_classes.generic_array;
    intrinsic_method(ctxt, clsid, "len", Intrinsic::GenericArrayLen);
    intrinsic_method(ctxt, clsid, "get", Intrinsic::GenericArrayGet);
    intrinsic_method(ctxt, clsid, "set", Intrinsic::GenericArraySet);

    let iname = ctxt.interner.intern("Thread");
    let clsid = ctxt.sym.borrow().get_class(iname);

    if let Some(clsid) = clsid {
        native_method(ctxt, clsid, "start", stdlib::spawn_thread as *const u8);
    }
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

fn internal_method<'ast>(ctxt: &mut Context<'ast>, clsid: ClassId, name: &str, kind: FctKind) {
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

fn internal_fct<'ast>(ctxt: &mut Context<'ast>, name: &str, kind: FctKind) {
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
