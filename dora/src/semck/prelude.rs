use parking_lot::RwLock;
use std::sync::Arc;

use crate::cpu::{has_lzcnt, has_popcnt, has_tzcnt};
use crate::gc::Address;
use crate::mem;
use crate::object::Header;
use crate::size::InstanceSize;
use crate::stack;
use crate::stdlib;
use crate::ty::{BuiltinType, TypeList};
use crate::vm::module::ModuleId;
use crate::vm::{ClassDef, ClassDefId, ClassId, FctId, FctKind, Intrinsic, TraitId, VM};
use crate::vtable::VTableBox;

pub fn internal_classes<'ast>(vm: &mut VM<'ast>) {
    vm.vips.unit_class = internal_class(vm, "Unit", Some(BuiltinType::Unit));
    vm.vips.bool_class = internal_class(vm, "Bool", Some(BuiltinType::Bool));

    vm.vips.uint8_class = internal_class(vm, "UInt8", Some(BuiltinType::UInt8));
    vm.vips.char_class = internal_class(vm, "Char", Some(BuiltinType::Char));
    vm.vips.int32_class = internal_class(vm, "Int32", Some(BuiltinType::Int32));
    vm.vips.int64_class = internal_class(vm, "Int64", Some(BuiltinType::Int64));

    vm.vips.float32_class = internal_class(vm, "Float", Some(BuiltinType::Float32));
    vm.vips.float64_class = internal_class(vm, "Double", Some(BuiltinType::Float64));

    vm.vips.object_class = internal_class(vm, "Object", None);
    vm.vips.string_class = internal_class(vm, "String", None);
    vm.vips.string_module = internal_module(vm, "String", None);

    vm.vips.cls.string_buffer = internal_class(vm, "StringBuffer", None);
    vm.vips.mods.string_buffer = internal_module(vm, "StringBuffer", None);

    let cls = vm.classes.idx(vm.vips.string_class);
    let mut cls = cls.write();
    cls.is_str = true;

    vm.vips.array_class = internal_class(vm, "Array", None);

    let cls = vm.classes.idx(vm.vips.array_class);
    let mut cls = cls.write();
    cls.is_array = true;

    vm.vips.testing_class = internal_class(vm, "Testing", None);

    vm.vips.stacktrace_class = internal_class(vm, "Stacktrace", None);
    vm.vips.stacktrace_element_class = internal_class(vm, "StacktraceElement", None);

    vm.vips.stringable_trait = find_trait(vm, "Stringable");
    vm.vips.zero_trait = find_trait(vm, "Zero");
    *vm.vips.iterator_trait.lock() = Some(find_trait(vm, "Iterator"));

    internal_free_classes(vm);
}

pub fn known_methods<'ast>(vm: &mut VM<'ast>) {
    vm.vips.fct.string_buffer_empty = find_module_method(vm, vm.vips.mods.string_buffer, "empty");
    vm.vips.fct.string_buffer_append = find_class_method(vm, vm.vips.cls.string_buffer, "append");
    vm.vips.fct.string_buffer_to_string =
        find_class_method(vm, vm.vips.cls.string_buffer, "toString");
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
            type_params: TypeList::empty(),
            parent_id: None,
            size: InstanceSize::Fixed(Header::size()),
            fields: Vec::new(),
            ref_fields: Vec::new(),
            vtable: None,
        })));

        class_defs.push(Arc::new(RwLock::new(ClassDef {
            id: free_array,
            cls_id: None,
            type_params: TypeList::empty(),
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
            let vtable = VTableBox::new(clsptr, Header::size() as usize, 0, &[]);
            free_object_class_def.vtable = Some(vtable);
        }

        {
            let free_array_class_def = &class_defs[free_array.to_usize()];
            let mut free_array_class_def = free_array_class_def.write();
            let clsptr = (&*free_array_class_def) as *const ClassDef as *mut ClassDef;
            let vtable = VTableBox::new(clsptr, 0, mem::ptr_width_usize(), &[]);
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

fn internal_module<'ast>(vm: &mut VM<'ast>, name: &str, ty: Option<BuiltinType>) -> ModuleId {
    let iname = vm.interner.intern(name);
    let module_id = vm.sym.lock().get_module(iname);

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
    native_fct(vm, "encodedBytecode", stdlib::bytecode as *const u8);

    native_fct(vm, "call", stdlib::call as *const u8);

    intrinsic_fct(vm, "defaultValue", Intrinsic::DefaultValue);

    let clsid = vm.vips.uint8_class;
    native_class_method(vm, clsid, "toString", stdlib::byte_to_string as *const u8);
    intrinsic_class_method(vm, clsid, "toInt64", Intrinsic::ByteToInt64);
    intrinsic_class_method(vm, clsid, "toInt", Intrinsic::ByteToInt32);
    intrinsic_class_method(vm, clsid, "toInt32", Intrinsic::ByteToInt32);
    intrinsic_class_method(vm, clsid, "toChar", Intrinsic::ByteToChar);

    intrinsic_class_method(vm, clsid, "equals", Intrinsic::ByteEq);
    intrinsic_class_method(vm, clsid, "compareTo", Intrinsic::ByteCmp);
    intrinsic_class_method(vm, clsid, "not", Intrinsic::ByteNot);

    let clsid = vm.vips.char_class;
    native_class_method(vm, clsid, "toString", stdlib::char_to_string as *const u8);
    intrinsic_class_method(vm, clsid, "toInt64", Intrinsic::CharToInt64);
    intrinsic_class_method(vm, clsid, "toInt", Intrinsic::CharToInt32);
    intrinsic_class_method(vm, clsid, "toInt32", Intrinsic::CharToInt32);

    intrinsic_class_method(vm, clsid, "equals", Intrinsic::CharEq);
    intrinsic_class_method(vm, clsid, "compareTo", Intrinsic::CharCmp);

    let clsid = vm.vips.int32_class;
    intrinsic_class_method(vm, clsid, "toUInt8", Intrinsic::Int32ToByte);
    intrinsic_class_method(vm, clsid, "toCharUnchecked", Intrinsic::Int32ToChar);
    intrinsic_class_method(vm, clsid, "toInt64", Intrinsic::Int32ToInt64);
    intrinsic_class_method(vm, clsid, "toInt", Intrinsic::Int32ToInt32);
    native_class_method(vm, clsid, "toString", stdlib::int_to_string as *const u8);

    intrinsic_class_method(vm, clsid, "toFloat", Intrinsic::Int32ToFloat32);
    intrinsic_class_method(vm, clsid, "toDouble", Intrinsic::Int32ToFloat64);

    intrinsic_class_method(vm, clsid, "asFloat", Intrinsic::ReinterpretInt32AsFloat32);

    intrinsic_class_method(vm, clsid, "equals", Intrinsic::Int32Eq);
    intrinsic_class_method(vm, clsid, "compareTo", Intrinsic::Int32Cmp);

    intrinsic_class_method(vm, clsid, "plus", Intrinsic::Int32Add);
    intrinsic_class_method(vm, clsid, "minus", Intrinsic::Int32Sub);
    intrinsic_class_method(vm, clsid, "times", Intrinsic::Int32Mul);
    intrinsic_class_method(vm, clsid, "div", Intrinsic::Int32Div);
    intrinsic_class_method(vm, clsid, "mod", Intrinsic::Int32Mod);

    intrinsic_class_method(vm, clsid, "bitwiseOr", Intrinsic::Int32Or);
    intrinsic_class_method(vm, clsid, "bitwiseAnd", Intrinsic::Int32And);
    intrinsic_class_method(vm, clsid, "bitwiseXor", Intrinsic::Int32Xor);

    intrinsic_class_method(vm, clsid, "shiftLeft", Intrinsic::Int32Shl);
    intrinsic_class_method(vm, clsid, "shiftRight", Intrinsic::Int32Shr);
    intrinsic_class_method(vm, clsid, "shiftRightSigned", Intrinsic::Int32Sar);

    intrinsic_class_method(vm, clsid, "rotateLeft", Intrinsic::Int32RotateLeft);
    intrinsic_class_method(vm, clsid, "rotateRight", Intrinsic::Int32RotateRight);

    intrinsic_class_method(vm, clsid, "unaryPlus", Intrinsic::Int32Plus);
    intrinsic_class_method(vm, clsid, "unaryMinus", Intrinsic::Int32Neg);
    intrinsic_class_method(vm, clsid, "not", Intrinsic::Int32Not);

    let clsid = vm.vips.int64_class;
    native_class_method(vm, clsid, "toString", stdlib::long_to_string as *const u8);
    intrinsic_class_method(vm, clsid, "toCharUnchecked", Intrinsic::Int64ToChar);
    intrinsic_class_method(vm, clsid, "toInt", Intrinsic::Int64ToInt32);
    intrinsic_class_method(vm, clsid, "toInt32", Intrinsic::Int64ToInt32);
    intrinsic_class_method(vm, clsid, "toUInt8", Intrinsic::Int64ToByte);

    intrinsic_class_method(vm, clsid, "toFloat", Intrinsic::Int64ToFloat32);
    intrinsic_class_method(vm, clsid, "toDouble", Intrinsic::Int64ToFloat64);

    intrinsic_class_method(vm, clsid, "asDouble", Intrinsic::ReinterpretInt64AsFloat64);

    intrinsic_class_method(vm, clsid, "equals", Intrinsic::Int64Eq);
    intrinsic_class_method(vm, clsid, "compareTo", Intrinsic::Int64Cmp);

    intrinsic_class_method(vm, clsid, "plus", Intrinsic::Int64Add);
    intrinsic_class_method(vm, clsid, "minus", Intrinsic::Int64Sub);
    intrinsic_class_method(vm, clsid, "times", Intrinsic::Int64Mul);
    intrinsic_class_method(vm, clsid, "div", Intrinsic::Int64Div);
    intrinsic_class_method(vm, clsid, "mod", Intrinsic::Int64Mod);

    intrinsic_class_method(vm, clsid, "bitwiseOr", Intrinsic::Int64Or);
    intrinsic_class_method(vm, clsid, "bitwiseAnd", Intrinsic::Int64And);
    intrinsic_class_method(vm, clsid, "bitwiseXor", Intrinsic::Int64Xor);

    intrinsic_class_method(vm, clsid, "shiftLeft", Intrinsic::Int64Shl);
    intrinsic_class_method(vm, clsid, "shiftRight", Intrinsic::Int64Shr);
    intrinsic_class_method(vm, clsid, "shiftRightSigned", Intrinsic::Int64Sar);

    intrinsic_class_method(vm, clsid, "rotateLeft", Intrinsic::Int64RotateLeft);
    intrinsic_class_method(vm, clsid, "rotateRight", Intrinsic::Int64RotateRight);

    intrinsic_class_method(vm, clsid, "unaryPlus", Intrinsic::Int64Plus);
    intrinsic_class_method(vm, clsid, "unaryMinus", Intrinsic::Int64Neg);
    intrinsic_class_method(vm, clsid, "not", Intrinsic::Int64Not);

    let clsid = vm.vips.bool_class;
    intrinsic_class_method(vm, clsid, "toInt", Intrinsic::BoolToInt32);
    intrinsic_class_method(vm, clsid, "toInt32", Intrinsic::BoolToInt32);
    intrinsic_class_method(vm, clsid, "toInt64", Intrinsic::BoolToInt64);
    intrinsic_class_method(vm, clsid, "equals", Intrinsic::BoolEq);
    intrinsic_class_method(vm, clsid, "not", Intrinsic::BoolNot);

    let clsid = vm.vips.string_class;
    let module_id = vm.vips.string_module;
    native_class_method(vm, clsid, "compareTo", stdlib::strcmp as *const u8);
    native_class_method(
        vm,
        clsid,
        "toInt32Success",
        stdlib::str_to_int_success as *const u8,
    );
    native_class_method(
        vm,
        clsid,
        "toInt64Success",
        stdlib::str_to_long_success as *const u8,
    );
    native_class_method(vm, clsid, "toInt32OrZero", stdlib::str_to_int as *const u8);
    native_class_method(vm, clsid, "toInt64OrZero", stdlib::str_to_long as *const u8);
    native_class_method(vm, clsid, "plus", stdlib::strcat as *const u8);

    intrinsic_class_method(vm, clsid, "length", Intrinsic::StrLen);
    intrinsic_class_method(vm, clsid, "getByte", Intrinsic::StrGet);
    native_class_method(vm, clsid, "clone", stdlib::str_clone as *const u8);
    native_module_method(
        vm,
        module_id,
        "fromBytesPartOrNull",
        stdlib::str_from_bytes as *const u8,
    );
    native_module_method(
        vm,
        module_id,
        "fromStringPartOrNull",
        stdlib::str_from_bytes as *const u8,
    );

    let clsid = vm.vips.float32_class;
    native_class_method(vm, clsid, "toString", stdlib::float_to_string as *const u8);
    intrinsic_class_method(vm, clsid, "toInt", Intrinsic::Float32ToInt32);
    intrinsic_class_method(vm, clsid, "toInt32", Intrinsic::Float32ToInt32);
    intrinsic_class_method(vm, clsid, "toInt64", Intrinsic::Float32ToInt64);
    intrinsic_class_method(vm, clsid, "toDouble", Intrinsic::PromoteFloat32ToFloat64);

    intrinsic_class_method(vm, clsid, "asInt", Intrinsic::ReinterpretFloat32AsInt32);
    intrinsic_class_method(vm, clsid, "asInt32", Intrinsic::ReinterpretFloat32AsInt32);

    intrinsic_class_method(vm, clsid, "equals", Intrinsic::Float32Eq);
    intrinsic_class_method(vm, clsid, "compareTo", Intrinsic::Float32Cmp);

    intrinsic_class_method(vm, clsid, "plus", Intrinsic::Float32Add);
    intrinsic_class_method(vm, clsid, "minus", Intrinsic::Float32Sub);
    intrinsic_class_method(vm, clsid, "times", Intrinsic::Float32Mul);
    intrinsic_class_method(vm, clsid, "div", Intrinsic::Float32Div);

    intrinsic_class_method(vm, clsid, "unaryPlus", Intrinsic::Float32Plus);
    intrinsic_class_method(vm, clsid, "unaryMinus", Intrinsic::Float32Neg);

    intrinsic_class_method(vm, clsid, "isNan", Intrinsic::Float32IsNan);
    intrinsic_class_method(vm, clsid, "sqrt", Intrinsic::Float32Sqrt);

    let clsid = vm.vips.float64_class;
    native_class_method(vm, clsid, "toString", stdlib::double_to_string as *const u8);
    intrinsic_class_method(vm, clsid, "toInt", Intrinsic::Float64ToInt32);
    intrinsic_class_method(vm, clsid, "toInt32", Intrinsic::Float64ToInt32);
    intrinsic_class_method(vm, clsid, "toInt64", Intrinsic::Float64ToInt64);
    intrinsic_class_method(vm, clsid, "toFloat", Intrinsic::DemoteFloat64ToFloat32);

    intrinsic_class_method(vm, clsid, "asInt64", Intrinsic::ReinterpretFloat64AsInt64);

    intrinsic_class_method(vm, clsid, "equals", Intrinsic::Float64Eq);
    intrinsic_class_method(vm, clsid, "compareTo", Intrinsic::Float64Cmp);

    intrinsic_class_method(vm, clsid, "plus", Intrinsic::Float64Add);
    intrinsic_class_method(vm, clsid, "minus", Intrinsic::Float64Sub);
    intrinsic_class_method(vm, clsid, "times", Intrinsic::Float64Mul);
    intrinsic_class_method(vm, clsid, "div", Intrinsic::Float64Div);

    intrinsic_class_method(vm, clsid, "unaryPlus", Intrinsic::Float64Plus);
    intrinsic_class_method(vm, clsid, "unaryMinus", Intrinsic::Float64Neg);

    intrinsic_class_method(vm, clsid, "isNan", Intrinsic::Float64IsNan);
    intrinsic_class_method(vm, clsid, "sqrt", Intrinsic::Float64Sqrt);

    let clsid = vm.vips.array_class;
    intrinsic_class_method(vm, clsid, "length", Intrinsic::GenericArrayLen);
    intrinsic_class_method(vm, clsid, "get", Intrinsic::GenericArrayGet);
    intrinsic_class_method(vm, clsid, "set", Intrinsic::GenericArraySet);

    let clsid = vm.vips.stacktrace_class;
    native_class_method(
        vm,
        clsid,
        "retrieveStacktrace",
        stack::retrieve_stack_trace as *const u8,
    );
    native_class_method(
        vm,
        clsid,
        "getStacktraceElement",
        stack::stack_element as *const u8,
    );

    let iname = vm.interner.intern("Thread");
    let clsid = vm.sym.lock().get_class(iname);

    if let Some(clsid) = clsid {
        native_class_method(vm, clsid, "start", stdlib::spawn_thread as *const u8);
    }
}

fn native_class_method<'ast>(vm: &mut VM<'ast>, clsid: ClassId, name: &str, fctptr: *const u8) {
    internal_class_method(vm, clsid, name, FctKind::Native(Address::from_ptr(fctptr)));
}

fn intrinsic_class_method<'ast>(
    vm: &mut VM<'ast>,
    clsid: ClassId,
    name: &str,
    intrinsic: Intrinsic,
) {
    internal_class_method(vm, clsid, name, FctKind::Builtin(intrinsic));
}

fn internal_class_method<'ast>(vm: &mut VM<'ast>, clsid: ClassId, name: &str, kind: FctKind) {
    let cls = vm.classes.idx(clsid);
    let cls = cls.read();
    let name = vm.interner.intern(name);

    for &mid in &cls.methods {
        let mtd = vm.fcts.idx(mid);
        let mut mtd = mtd.write();

        if mtd.name == name {
            if mtd.internal {
                mtd.kind = kind;
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

fn find_class_method<'ast>(vm: &VM<'ast>, clsid: ClassId, name: &str) -> FctId {
    let cls = vm.classes.idx(clsid);
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

fn native_module_method<'ast>(
    vm: &mut VM<'ast>,
    module_id: ModuleId,
    name: &str,
    fctptr: *const u8,
) {
    internal_module_method(
        vm,
        module_id,
        name,
        FctKind::Native(Address::from_ptr(fctptr)),
    );
}

fn internal_module_method<'ast>(vm: &mut VM<'ast>, module_id: ModuleId, name: &str, kind: FctKind) {
    let module = vm.modules.idx(module_id);
    let module = module.read();
    let name = vm.interner.intern(name);

    for &mid in &module.methods {
        let mtd = vm.fcts.idx(mid);
        let mut mtd = mtd.write();

        if mtd.name == name {
            if mtd.internal {
                mtd.kind = kind;
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

fn find_module_method<'ast>(vm: &VM<'ast>, module_id: ModuleId, name: &str) -> FctId {
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

pub(crate) fn install_conditional_intrinsics(vm: &mut VM) {
    let clsid = vm.vips.int32_class;
    if has_popcnt() {
        intrinsic_class_method(vm, clsid, "countZeroBits", Intrinsic::Int32CountZeroBits);
        intrinsic_class_method(vm, clsid, "countOneBits", Intrinsic::Int32CountOneBits);
    }
    if has_lzcnt() {
        intrinsic_class_method(
            vm,
            clsid,
            "countZeroBitsLeading",
            Intrinsic::Int32CountZeroBitsLeading,
        );
        intrinsic_class_method(
            vm,
            clsid,
            "countOneBitsLeading",
            Intrinsic::Int32CountOneBitsLeading,
        );
    }
    if has_tzcnt() {
        intrinsic_class_method(
            vm,
            clsid,
            "countZeroBitsTrailing",
            Intrinsic::Int32CountZeroBitsTrailing,
        );
        intrinsic_class_method(
            vm,
            clsid,
            "countOneBitsTrailing",
            Intrinsic::Int32CountOneBitsTrailing,
        );
    }

    let clsid = vm.vips.int64_class;
    if has_popcnt() {
        intrinsic_class_method(vm, clsid, "countZeroBits", Intrinsic::Int64CountZeroBits);
        intrinsic_class_method(vm, clsid, "countOneBits", Intrinsic::Int64CountOneBits);
    }
    if has_lzcnt() {
        intrinsic_class_method(
            vm,
            clsid,
            "countZeroBitsLeading",
            Intrinsic::Int64CountZeroBitsLeading,
        );
        intrinsic_class_method(
            vm,
            clsid,
            "countOneBitsLeading",
            Intrinsic::Int64CountOneBitsLeading,
        );
    }
    if has_tzcnt() {
        intrinsic_class_method(
            vm,
            clsid,
            "countZeroBitsTrailing",
            Intrinsic::Int64CountZeroBitsTrailing,
        );
        intrinsic_class_method(
            vm,
            clsid,
            "countOneBitsTrailing",
            Intrinsic::Int64CountOneBitsTrailing,
        );
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
