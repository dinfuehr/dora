use class::ClassId;
use ctxt::{FctKind, Builtin, SemContext, TraitId};
use exception;
use gc::Address;
use stdlib;
use ty::BuiltinType;

pub fn internal_types<'ast>(ctxt: &mut SemContext<'ast>) {
    ctxt.vips.bool_class = builtin_class(ctxt, "bool", Some(BuiltinType::Bool));
    ctxt.vips.byte_class = builtin_class(ctxt, "byte", Some(BuiltinType::Byte));
    ctxt.vips.char_class = builtin_class(ctxt, "char", Some(BuiltinType::Char));
    ctxt.vips.int_class = builtin_class(ctxt, "int", Some(BuiltinType::Int));
    ctxt.vips.long_class = builtin_class(ctxt, "long", Some(BuiltinType::Long));

    ctxt.vips.float_class = builtin_class(ctxt, "float", Some(BuiltinType::Float));
    ctxt.vips.double_class = builtin_class(ctxt, "double", Some(BuiltinType::Double));

    ctxt.vips.object_class = builtin_class(ctxt, "Object", None);
    ctxt.vips.string_class = builtin_class(ctxt, "String", None);

    let cls = ctxt.classes.idx(ctxt.vips.string_class);
    let mut cls = cls.write();
    cls.is_str = true;

    ctxt.vips.array_class = builtin_class(ctxt, "Array", None);

    let cls = ctxt.classes.idx(ctxt.vips.array_class);
    let mut cls = cls.write();
    cls.is_array = true;

    ctxt.vips.testing_class = builtin_class(ctxt, "Testing", None);

    ctxt.vips.exception_class = builtin_class(ctxt, "Exception", None);
    ctxt.vips.stack_trace_element_class = builtin_class(ctxt, "StackTraceElement", None);

    ctxt.vips.comparable_trait = find_trait(ctxt, "Comparable");
    ctxt.vips.equals_trait = find_trait(ctxt, "Equals");
    *ctxt.vips.iterator_trait.lock() = Some(find_trait(ctxt, "Iterator"));
}

fn builtin_class<'ast>(
    ctxt: &mut SemContext<'ast>,
    name: &str,
    ty: Option<BuiltinType>,
) -> ClassId {
    let iname = ctxt.interner.intern(name);
    let clsid = ctxt.sym.lock().get_class(iname);

    if let Some(clsid) = clsid {
        let cls = ctxt.classes.idx(clsid);
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

fn find_trait<'ast>(ctxt: &mut SemContext<'ast>, name: &str) -> TraitId {
    let iname = ctxt.interner.intern(name);
    let tid = ctxt.sym.lock().get_trait(iname);

    if let Some(tid) = tid {
        tid
    } else {
        panic!("trait {} not found!", name);
    }
}

pub fn internal_functions<'ast>(ctxt: &mut SemContext<'ast>) {
    native_fct(ctxt, "fatalError", stdlib::fatal_error as *const u8);
    native_fct(ctxt, "abort", stdlib::abort as *const u8);
    native_fct(ctxt, "exit", stdlib::exit as *const u8);

    native_fct(ctxt, "print", stdlib::print as *const u8);
    native_fct(ctxt, "println", stdlib::println as *const u8);
    native_fct(ctxt, "addressOf", stdlib::addr as *const u8);
    builtin_fct(ctxt, "assert", Builtin::Assert);
    builtin_fct(ctxt, "debug", Builtin::Debug);
    native_fct(ctxt, "argc", stdlib::argc as *const u8);
    native_fct(ctxt, "argv", stdlib::argv as *const u8);
    native_fct(ctxt, "forceCollect", stdlib::gc_collect as *const u8);
    native_fct(ctxt, "timestamp", stdlib::timestamp as *const u8);
    native_fct(
        ctxt,
        "forceMinorCollect",
        stdlib::gc_minor_collect as *const u8,
    );
    native_fct(ctxt, "sleep", stdlib::sleep as *const u8);

    native_fct(ctxt, "call", stdlib::call as *const u8);
    native_fct(ctxt, "throwFromNative", stdlib::throw_native as *const u8);
    native_fct(
        ctxt,
        "throwFromNativeButNotThrows",
        stdlib::throw_native as *const u8,
    );

    native_fct(ctxt, "loadFunction", stdlib::load_function as *const u8);
    native_fct(ctxt, "call0", stdlib::call0 as *const u8);
    native_fct(ctxt, "call1", stdlib::call1 as *const u8);
    native_fct(ctxt, "call2", stdlib::call2 as *const u8);
    native_fct(ctxt, "call3", stdlib::call3 as *const u8);

    native_fct(ctxt, "native_malloc", stdlib::native_malloc as *const u8);
    native_fct(ctxt, "native_free", stdlib::native_free as *const u8);
    builtin_fct(ctxt, "set_uint8", Builtin::SetUint8);
    builtin_fct(ctxt, "defaultValue", Builtin::DefaultValue);

    let clsid = ctxt.vips.byte_class;
    native_method(ctxt, clsid, "toString", stdlib::byte_to_string as *const u8);
    builtin_method(ctxt, clsid, "toLong", Builtin::ByteToLong);
    builtin_method(ctxt, clsid, "toInt", Builtin::ByteToInt);

    builtin_method(ctxt, clsid, "equals", Builtin::ByteEq);
    builtin_method(ctxt, clsid, "compareTo", Builtin::ByteCmp);
    builtin_method(ctxt, clsid, "not", Builtin::ByteNot);

    let clsid = ctxt.vips.char_class;
    native_method(ctxt, clsid, "toString", stdlib::char_to_string as *const u8);
    builtin_method(ctxt, clsid, "toLong", Builtin::CharToLong);
    builtin_method(ctxt, clsid, "toInt", Builtin::CharToInt);

    builtin_method(ctxt, clsid, "equals", Builtin::CharEq);
    builtin_method(ctxt, clsid, "compareTo", Builtin::CharCmp);

    let clsid = ctxt.vips.int_class;
    builtin_method(ctxt, clsid, "toByte", Builtin::IntToByte);
    builtin_method(ctxt, clsid, "toCharUnchecked", Builtin::IntToChar);
    builtin_method(ctxt, clsid, "toLong", Builtin::IntToLong);
    native_method(ctxt, clsid, "toString", stdlib::int_to_string as *const u8);

    builtin_method(ctxt, clsid, "toFloat", Builtin::IntToFloat);
    builtin_method(ctxt, clsid, "toDouble", Builtin::IntToDouble);

    builtin_method(ctxt, clsid, "asFloat", Builtin::IntAsFloat);

    builtin_method(ctxt, clsid, "equals", Builtin::IntEq);
    builtin_method(ctxt, clsid, "compareTo", Builtin::IntCmp);

    builtin_method(ctxt, clsid, "plus", Builtin::IntAdd);
    builtin_method(ctxt, clsid, "minus", Builtin::IntSub);
    builtin_method(ctxt, clsid, "times", Builtin::IntMul);
    builtin_method(ctxt, clsid, "div", Builtin::IntDiv);
    builtin_method(ctxt, clsid, "mod", Builtin::IntMod);

    builtin_method(ctxt, clsid, "bitwiseOr", Builtin::IntOr);
    builtin_method(ctxt, clsid, "bitwiseAnd", Builtin::IntAnd);
    builtin_method(ctxt, clsid, "bitwiseXor", Builtin::IntXor);

    builtin_method(ctxt, clsid, "shiftLeft", Builtin::IntShl);
    builtin_method(ctxt, clsid, "shiftRight", Builtin::IntSar);
    builtin_method(ctxt, clsid, "unsignedShiftRight", Builtin::IntShr);

    builtin_method(ctxt, clsid, "unaryPlus", Builtin::IntPlus);
    builtin_method(ctxt, clsid, "unaryMinus", Builtin::IntNeg);
    builtin_method(ctxt, clsid, "not", Builtin::IntNot);

    let trait_id = ctxt.vips.equals_trait;
    builtin_impl(ctxt, clsid, trait_id, "equals", Builtin::IntEq);

    let trait_id = ctxt.vips.comparable_trait;
    builtin_impl(ctxt, clsid, trait_id, "compareTo", Builtin::IntCmp);

    let clsid = ctxt.vips.long_class;
    native_method(ctxt, clsid, "toString", stdlib::long_to_string as *const u8);
    builtin_method(ctxt, clsid, "toCharUnchecked", Builtin::LongToChar);
    builtin_method(ctxt, clsid, "toInt", Builtin::LongToInt);
    builtin_method(ctxt, clsid, "toByte", Builtin::LongToByte);

    builtin_method(ctxt, clsid, "toFloat", Builtin::LongToFloat);
    builtin_method(ctxt, clsid, "toDouble", Builtin::LongToDouble);

    builtin_method(ctxt, clsid, "asDouble", Builtin::LongAsDouble);

    builtin_method(ctxt, clsid, "equals", Builtin::LongEq);
    builtin_method(ctxt, clsid, "compareTo", Builtin::LongCmp);

    builtin_method(ctxt, clsid, "plus", Builtin::LongAdd);
    builtin_method(ctxt, clsid, "minus", Builtin::LongSub);
    builtin_method(ctxt, clsid, "times", Builtin::LongMul);
    builtin_method(ctxt, clsid, "div", Builtin::LongDiv);
    builtin_method(ctxt, clsid, "mod", Builtin::LongMod);

    builtin_method(ctxt, clsid, "bitwiseOr", Builtin::LongOr);
    builtin_method(ctxt, clsid, "bitwiseAnd", Builtin::LongAnd);
    builtin_method(ctxt, clsid, "bitwiseXor", Builtin::LongXor);

    builtin_method(ctxt, clsid, "shiftLeft", Builtin::LongShl);
    builtin_method(ctxt, clsid, "shiftRight", Builtin::LongSar);
    builtin_method(ctxt, clsid, "unsignedShiftRight", Builtin::LongShr);

    builtin_method(ctxt, clsid, "unaryPlus", Builtin::LongPlus);
    builtin_method(ctxt, clsid, "unaryMinus", Builtin::LongNeg);
    builtin_method(ctxt, clsid, "not", Builtin::LongNot);

    let clsid = ctxt.vips.bool_class;
    builtin_method(ctxt, clsid, "toInt", Builtin::BoolToInt);
    builtin_method(ctxt, clsid, "toLong", Builtin::BoolToLong);
    builtin_method(ctxt, clsid, "equals", Builtin::BoolEq);
    builtin_method(ctxt, clsid, "not", Builtin::BoolNot);

    let clsid = ctxt.vips.string_class;
    native_method(ctxt, clsid, "compareTo", stdlib::strcmp as *const u8);
    native_method(ctxt, clsid, "parseInt", stdlib::str_parse_int as *const u8);
    native_method(
        ctxt,
        clsid,
        "parseLong",
        stdlib::str_parse_long as *const u8,
    );
    native_method(ctxt, clsid, "plus", stdlib::strcat as *const u8);

    builtin_method(ctxt, clsid, "len", Builtin::StrLen);
    builtin_method(ctxt, clsid, "getByte", Builtin::StrGet);
    native_method(ctxt, clsid, "clone", stdlib::str_clone as *const u8);
    native_method(
        ctxt,
        clsid,
        "fromBytesPartOrNull",
        stdlib::str_from_bytes as *const u8,
    );
    native_method(
        ctxt,
        clsid,
        "fromStringPartOrNull",
        stdlib::str_from_bytes as *const u8,
    );

    let clsid = ctxt.vips.float_class;
    native_method(
        ctxt,
        clsid,
        "toString",
        stdlib::float_to_string as *const u8,
    );
    builtin_method(ctxt, clsid, "toInt", Builtin::FloatToInt);
    builtin_method(ctxt, clsid, "toLong", Builtin::FloatToLong);
    builtin_method(ctxt, clsid, "toDouble", Builtin::FloatToDouble);

    builtin_method(ctxt, clsid, "asInt", Builtin::FloatAsInt);

    builtin_method(ctxt, clsid, "equals", Builtin::FloatEq);
    builtin_method(ctxt, clsid, "compareTo", Builtin::FloatCmp);

    builtin_method(ctxt, clsid, "plus", Builtin::FloatAdd);
    builtin_method(ctxt, clsid, "minus", Builtin::FloatSub);
    builtin_method(ctxt, clsid, "times", Builtin::FloatMul);
    builtin_method(ctxt, clsid, "div", Builtin::FloatDiv);

    builtin_method(ctxt, clsid, "unaryPlus", Builtin::FloatPlus);
    builtin_method(ctxt, clsid, "unaryMinus", Builtin::FloatNeg);

    builtin_method(ctxt, clsid, "isNan", Builtin::FloatIsNan);
    builtin_method(ctxt, clsid, "sqrt", Builtin::FloatSqrt);

    let clsid = ctxt.vips.double_class;
    native_method(
        ctxt,
        clsid,
        "toString",
        stdlib::double_to_string as *const u8,
    );
    builtin_method(ctxt, clsid, "toInt", Builtin::DoubleToInt);
    builtin_method(ctxt, clsid, "toLong", Builtin::DoubleToLong);
    builtin_method(ctxt, clsid, "toFloat", Builtin::DoubleToFloat);

    builtin_method(ctxt, clsid, "asLong", Builtin::DoubleAsLong);

    builtin_method(ctxt, clsid, "equals", Builtin::DoubleEq);
    builtin_method(ctxt, clsid, "compareTo", Builtin::DoubleCmp);

    builtin_method(ctxt, clsid, "plus", Builtin::DoubleAdd);
    builtin_method(ctxt, clsid, "minus", Builtin::DoubleSub);
    builtin_method(ctxt, clsid, "times", Builtin::DoubleMul);
    builtin_method(ctxt, clsid, "div", Builtin::DoubleDiv);

    builtin_method(ctxt, clsid, "unaryPlus", Builtin::DoublePlus);
    builtin_method(ctxt, clsid, "unaryMinus", Builtin::DoubleNeg);

    builtin_method(ctxt, clsid, "isNan", Builtin::DoubleIsNan);
    builtin_method(ctxt, clsid, "sqrt", Builtin::DoubleSqrt);

    let clsid = ctxt.vips.array_class;
    builtin_method(ctxt, clsid, "len", Builtin::GenericArrayLen);
    builtin_method(ctxt, clsid, "get", Builtin::GenericArrayGet);
    builtin_method(ctxt, clsid, "set", Builtin::GenericArraySet);

    let clsid = ctxt.vips.exception_class;
    native_method(
        ctxt,
        clsid,
        "retrieveStackTrace",
        exception::retrieve_stack_trace as *const u8,
    );
    native_method(
        ctxt,
        clsid,
        "getStackTraceElement",
        exception::stack_element as *const u8,
    );

    let iname = ctxt.interner.intern("Thread");
    let clsid = ctxt.sym.lock().get_class(iname);

    if let Some(clsid) = clsid {
        native_method(ctxt, clsid, "start", stdlib::spawn_thread as *const u8);
    }
}

fn native_method<'ast>(ctxt: &mut SemContext<'ast>, clsid: ClassId, name: &str, fctptr: *const u8) {
    internal_method(
        ctxt,
        clsid,
        name,
        FctKind::Native(Address::from_ptr(fctptr)),
    );
}

fn builtin_method<'ast>(
    ctxt: &mut SemContext<'ast>,
    clsid: ClassId,
    name: &str,
    builtin: Builtin,
) {
    internal_method(ctxt, clsid, name, FctKind::Builtin(builtin));
}

fn internal_method<'ast>(ctxt: &mut SemContext<'ast>, clsid: ClassId, name: &str, kind: FctKind) {
    let cls = ctxt.classes.idx(clsid);
    let cls = cls.read();
    let name = ctxt.interner.intern(name);

    for &mid in &cls.methods {
        let mtd = ctxt.fcts.idx(mid);
        let mut mtd = mtd.write();

        if mtd.name == name && mtd.internal {
            mtd.kind = kind;
            mtd.internal_resolved = true;
            break;
        }
    }
}

fn native_fct<'ast>(ctxt: &mut SemContext<'ast>, name: &str, fctptr: *const u8) {
    internal_fct(ctxt, name, FctKind::Native(Address::from_ptr(fctptr)));
}

fn builtin_fct<'ast>(ctxt: &mut SemContext<'ast>, name: &str, builtin: Builtin) {
    internal_fct(ctxt, name, FctKind::Builtin(builtin));
}

fn internal_fct<'ast>(ctxt: &mut SemContext<'ast>, name: &str, kind: FctKind) {
    let name = ctxt.interner.intern(name);
    let fctid = ctxt.sym.lock().get_fct(name);

    if let Some(fctid) = fctid {
        let fct = ctxt.fcts.idx(fctid);
        let mut fct = fct.write();

        if fct.internal {
            fct.kind = kind;
            fct.internal_resolved = true;
        }
    }
}

fn builtin_impl<'ast>(
    ctxt: &mut SemContext<'ast>,
    clsid: ClassId,
    tid: TraitId,
    name: &str,
    builtin: Builtin,
) {
    internal_impl(ctxt, clsid, tid, name, FctKind::Builtin(builtin));
}

fn internal_impl<'ast>(
    ctxt: &mut SemContext<'ast>,
    clsid: ClassId,
    tid: TraitId,
    name: &str,
    kind: FctKind,
) {
    let name = ctxt.interner.intern(name);
    let cls = ctxt.classes.idx(clsid);
    let cls = cls.read();

    for &iid in &cls.impls {
        let i = ctxt.impls[iid].read();

        if Some(tid) == i.trait_id {
            for &fid in &i.methods {
                let fct = ctxt.fcts.idx(fid);
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
    use semck::tests::*;

    #[test]
    fn builtin_functions() {
        ok("fun f() { assert(true); }");
        ok("fun f() { print(\"test\"); }");
        ok("fun f() { println(\"test\"); }");
    }
}
