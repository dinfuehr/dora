use std::rc::Rc;

use crate::sema::{
    ClassDefinition, ClassDefinitionId, EnumDefinitionId, ExtensionDefinitionId, FctDefinitionId,
    Field, FieldId, ImplDefinitionId, Intrinsic, ModuleDefinition, Sema, StructDefinitionId,
    TraitDefinitionId, TypeParamDefinition, Visibility,
};
use crate::sym::{SymTable, SymbolKind};
use crate::ty::SourceType;

use crate::interner::Name;
use crate::ParsedType;

pub fn lookup_known_fundamental_types(sa: &mut Sema) {
    sa.known.structs.bool = Some(internal_struct(
        sa,
        "std::primitives::Bool",
        Some(SourceType::Bool),
    ));

    sa.known.structs.uint8 = Some(internal_struct(
        sa,
        "std::primitives::UInt8",
        Some(SourceType::UInt8),
    ));
    sa.known.structs.char = Some(internal_struct(
        sa,
        "std::primitives::Char",
        Some(SourceType::Char),
    ));
    sa.known.structs.int32 = Some(internal_struct(
        sa,
        "std::primitives::Int32",
        Some(SourceType::Int32),
    ));
    sa.known.structs.int64 = Some(internal_struct(
        sa,
        "std::primitives::Int64",
        Some(SourceType::Int64),
    ));

    sa.known.structs.float32 = Some(internal_struct(
        sa,
        "std::primitives::Float32",
        Some(SourceType::Float32),
    ));
    sa.known.structs.float64 = Some(internal_struct(
        sa,
        "std::primitives::Float64",
        Some(SourceType::Float64),
    ));

    sa.known.classes.string = Some(internal_class(sa, "std::string::String"));

    sa.known.classes.string_buffer = Some(find_class(sa, "std::string::StringBuffer"));

    sa.known.classes.atomic_int32 = Some(find_class(sa, "std::thread::AtomicInt32"));
    sa.known.classes.atomic_int64 = Some(find_class(sa, "std::thread::AtomicInt64"));

    let cls = &mut sa.classes[sa.known.classes.string()];
    cls.is_str = true;

    sa.known.classes.array = Some(internal_class(sa, "std::collections::Array"));

    let cls = &mut sa.classes[sa.known.classes.array()];
    cls.is_array = true;

    sa.known.classes.stacktrace = Some(find_class(sa, "std::Stacktrace"));
    sa.known.classes.stacktrace_element = Some(find_class(sa, "std::StacktraceElement"));
    sa.known.classes.thread = Some(find_class(sa, "std::thread::Thread"));

    sa.known.traits.add = Some(find_trait(sa, "std::traits::Add"));
    sa.known.traits.bit_and = Some(find_trait(sa, "std::traits::BitAnd"));
    sa.known.traits.bit_or = Some(find_trait(sa, "std::traits::BitOr"));
    sa.known.traits.bit_xor = Some(find_trait(sa, "std::traits::BitXor"));
    sa.known.traits.comparable = Some(find_trait(sa, "std::traits::Comparable"));
    sa.known.traits.div = Some(find_trait(sa, "std::traits::Div"));
    sa.known.traits.equals = Some(find_trait(sa, "std::traits::Equals"));
    sa.known.traits.into_iterator = Some(find_trait(sa, "std::traits::IntoIterator"));
    sa.known.traits.iterator = Some(find_trait(sa, "std::traits::Iterator"));
    sa.known.traits.mul = Some(find_trait(sa, "std::traits::Mul"));
    sa.known.traits.mod_ = Some(find_trait(sa, "std::traits::Mod"));
    sa.known.traits.neg = Some(find_trait(sa, "std::traits::Neg"));
    sa.known.traits.not = Some(find_trait(sa, "std::traits::Not"));
    sa.known.traits.sar = Some(find_trait(sa, "std::traits::Sar"));
    sa.known.traits.shl = Some(find_trait(sa, "std::traits::Shl"));
    sa.known.traits.shr = Some(find_trait(sa, "std::traits::Shr"));
    sa.known.traits.stringable = Some(find_trait(sa, "std::string::Stringable"));
    sa.known.traits.sub = Some(find_trait(sa, "std::traits::Sub"));
    sa.known.traits.zero = Some(find_trait(sa, "std::traits::Zero"));
    sa.known.traits.index_get = Some(find_trait(sa, "std::traits::IndexGet"));
    sa.known.traits.index_set = Some(find_trait(sa, "std::traits::IndexSet"));

    sa.known.enums.option = Some(find_enum(sa, "std::primitives::Option"));
    sa.known.enums.ordering = Some(find_enum(sa, "std::traits::Ordering"));
}

pub fn setup_prelude(sa: &mut Sema) {
    let stdlib_id = sa.stdlib_module_id();

    let symbols = [
        "std::primitives::Bool",
        "std::primitives::UInt8",
        "std::primitives::Char",
        "std::primitives::Int32",
        "std::primitives::Int64",
        "std::primitives::Int",
        "std::primitives::Float32",
        "std::primitives::Float64",
        "std::string::String",
        "std::collections::Array",
        "std::collections::Vec",
        "std::print",
        "std::println",
        "std::primitives::Option",
        "std::unimplemented",
        "std::unreachable",
        "std::assert",
        "std::primitives::Result",
    ];

    let mut prelude_table = SymTable::new();

    for name in &symbols {
        let sym = resolve_path(sa, name);
        let name = final_path_name(sa, name);
        let old_sym = prelude_table.insert(name, sym);
        assert!(old_sym.is_none());
    }

    {
        // include None and Some from Option
        let enum_id = resolve_path(sa, "std::primitives::Option")
            .to_enum()
            .expect("enum expected");

        let enum_ = sa.enum_(enum_id);

        for variant in enum_.variants() {
            let old_sym =
                prelude_table.insert(variant.name, SymbolKind::EnumVariant(enum_id, variant.id));
            assert!(old_sym.is_none());
        }
    }

    {
        // include Ok and Err from Result
        let enum_id = resolve_path(sa, "std::primitives::Result")
            .to_enum()
            .expect("enum expected");

        let enum_ = sa.enum_(enum_id);

        for variant in enum_.variants() {
            let old_sym =
                prelude_table.insert(variant.name, SymbolKind::EnumVariant(enum_id, variant.id));
            assert!(old_sym.is_none());
        }
    }

    let stdlib_name = sa.interner.intern("std");
    prelude_table.insert(stdlib_name, SymbolKind::Module(stdlib_id));

    let module = ModuleDefinition::new_top_level(None);
    assert!(module.table.set(Rc::new(prelude_table)).is_ok());
    let module_id = sa.modules.alloc(module);
    sa.set_prelude_module_id(module_id);
}

fn final_path_name(sa: &mut Sema, path: &str) -> Name {
    let name = path.split("::").last().expect("name missing");
    sa.interner.intern(name)
}

pub fn lookup_known_methods(sa: &mut Sema) {
    sa.known.functions.string_equals = Some(lookup_fct(
        sa,
        "std::traits::Equals for std::string::String#equals",
    ));

    sa.known.functions.string_buffer_empty =
        Some(lookup_fct(sa, "std::string::StringBuffer#empty"));
    sa.known.functions.string_buffer_append =
        Some(lookup_fct(sa, "std::string::StringBuffer#append"));
    sa.known.functions.string_buffer_to_string =
        Some(lookup_fct(sa, "std::string::StringBuffer#toString"));
}

pub fn create_lambda_class(sa: &mut Sema) {
    let class_name = sa.interner.intern("$Lambda");
    let context_name = sa.interner.intern("context");

    let field = Field {
        id: FieldId(0),
        name: Some(context_name),
        parsed_ty: ParsedType::new_ty(SourceType::Ptr),
        mutable: false,
        visibility: Visibility::Public,
    };

    let fields = vec![field];

    let class = ClassDefinition::new_without_source(
        sa.stdlib_package_id(),
        sa.stdlib_module_id(),
        None,
        None,
        class_name,
        Visibility::Public,
        TypeParamDefinition::empty(),
        fields,
    );

    let class_id = sa.classes.alloc(class);
    sa.classes[class_id].id = Some(class_id);
    sa.known.classes.lambda = Some(class_id);
}

fn find_class(sa: &Sema, name: &str) -> ClassDefinitionId {
    resolve_path(sa, name).to_class().expect("class expected")
}

fn internal_class(sa: &mut Sema, name: &str) -> ClassDefinitionId {
    let cls_id = find_class(sa, name);

    let cls = &mut sa.classes[cls_id];
    assert!(cls.is_internal);
    cls.internal_resolved = true;

    cls_id
}

fn internal_struct(sa: &mut Sema, name: &str, ty: Option<SourceType>) -> StructDefinitionId {
    let struct_id = resolve_path(sa, name).to_struct().expect("struct expected");

    let struct_ = &mut sa.structs[struct_id];
    assert!(struct_.is_internal);
    struct_.primitive_ty = ty;
    struct_.internal_resolved = true;

    struct_id
}

pub fn resolve_path(sa: &Sema, name: &str) -> SymbolKind {
    let mut path = name.split("::");

    let module_lookup = |sym: SymbolKind, name| {
        let module_id = sym.to_module().expect("module expected");
        let table = sa.module(module_id).table();

        let interned_name = sa.interner.intern(name);

        if let Some(current_sym) = table.get(interned_name) {
            current_sym
        } else {
            let module = sa.module(module_id);
            panic!("{} not found in module {}.", name, module.name(sa));
        }
    };

    let package_name = path.next().expect("missing package");
    let mut sym = if package_name == "<prog>" {
        SymbolKind::Module(sa.program_module_id())
    } else if let Some(package_id) = sa.package_names.get(package_name) {
        let package = &sa.packages[*package_id];
        SymbolKind::Module(package.top_level_module_id())
    } else {
        panic!("unknown package {} in {}", package_name, name);
    };

    for name in path {
        sym = module_lookup(sym, name);
    }

    sym
}

fn find_trait(sa: &Sema, name: &str) -> TraitDefinitionId {
    resolve_path(sa, name).to_trait().expect("trait expected")
}

fn find_enum(sa: &Sema, name: &str) -> EnumDefinitionId {
    resolve_path(sa, name).to_enum().expect("enum not found")
}

pub fn resolve_internal_functions(sa: &mut Sema) {
    resolve_string(sa);
    resolve_uint8(sa);
    resolve_bool(sa);
    resolve_char(sa);
    resolve_int32(sa);
    resolve_int64(sa);
    resolve_float32(sa);
    resolve_float64(sa);

    resolve_array(sa);
    resolve_option(sa);

    resolve_thread(sa);

    resolve_atomic_int32(sa);
    resolve_atomic_int64(sa);

    lookup_ordering(sa);

    resolve_functions(sa);
}

fn resolve_functions(sa: &mut Sema) {
    sa.known.functions.unreachable = Some(lookup_fct(sa, "std::unreachable"));
    sa.known.functions.fatal_error = Some(lookup_fct(sa, "std::fatalError"));
}

fn lookup_ordering(sa: &mut Sema) {
    sa.known.functions.ordering_is_ge = Some(lookup_fct(sa, "std::Ordering#is_ge"));
    sa.known.functions.ordering_is_gt = Some(lookup_fct(sa, "std::Ordering#is_gt"));
    sa.known.functions.ordering_is_le = Some(lookup_fct(sa, "std::Ordering#is_le"));
    sa.known.functions.ordering_is_lt = Some(lookup_fct(sa, "std::Ordering#is_lt"));
}

fn resolve_atomic_int32(sa: &mut Sema) {
    intrinsic_method(
        sa,
        "std::thread::AtomicInt32#get",
        Intrinsic::AtomicInt32Get,
    );
    intrinsic_method(
        sa,
        "std::thread::AtomicInt32#set",
        Intrinsic::AtomicInt32Set,
    );
    intrinsic_method(
        sa,
        "std::thread::AtomicInt32#exchange",
        Intrinsic::AtomicInt32Exchange,
    );
    intrinsic_method(
        sa,
        "std::thread::AtomicInt32#compareExchange",
        Intrinsic::AtomicInt32CompareExchange,
    );
    intrinsic_method(
        sa,
        "std::thread::AtomicInt32#fetchAdd",
        Intrinsic::AtomicInt32FetchAdd,
    );
}

fn resolve_atomic_int64(sa: &mut Sema) {
    intrinsic_method(
        sa,
        "std::thread::AtomicInt64#get",
        Intrinsic::AtomicInt64Get,
    );
    intrinsic_method(
        sa,
        "std::thread::AtomicInt64#set",
        Intrinsic::AtomicInt64Set,
    );
    intrinsic_method(
        sa,
        "std::thread::AtomicInt64#exchange",
        Intrinsic::AtomicInt64Exchange,
    );
    intrinsic_method(
        sa,
        "std::thread::AtomicInt64#compareExchange",
        Intrinsic::AtomicInt64CompareExchange,
    );
    intrinsic_method(
        sa,
        "std::thread::AtomicInt64#fetchAdd",
        Intrinsic::AtomicInt64FetchAdd,
    );
}

fn resolve_thread(sa: &mut Sema) {
    intrinsic_method(sa, "std::thread::Thread#current", Intrinsic::ThreadCurrent);
}

fn resolve_string(sa: &Sema) {
    intrinsic_method(sa, "std::string::String#size", Intrinsic::StrLen);
    intrinsic_method(sa, "std::string::String#getByte", Intrinsic::StrGet);
}

fn resolve_uint8(sa: &mut Sema) {
    intrinsic_method(
        sa,
        "std::primitives::UInt8#toInt64",
        Intrinsic::UInt8ToInt64,
    );
    intrinsic_method(
        sa,
        "std::primitives::UInt8#toInt32",
        Intrinsic::UInt8ToInt32,
    );
    intrinsic_method(sa, "std::primitives::UInt8#toChar", Intrinsic::UInt8ToChar);
    intrinsic_method(
        sa,
        "std::traits::Equals for std::primitives::UInt8#equals",
        Intrinsic::UInt8Eq,
    );
    intrinsic_method(
        sa,
        "std::traits::Comparable for std::primitives::UInt8#cmp",
        Intrinsic::UInt8Cmp,
    );
}

fn resolve_bool(sa: &Sema) {
    intrinsic_method(sa, "std::primitives::Bool#toInt32", Intrinsic::BoolToInt32);
    intrinsic_method(sa, "std::primitives::Bool#toInt64", Intrinsic::BoolToInt64);
    intrinsic_method(
        sa,
        "std::traits::Equals for std::primitives::Bool#equals",
        Intrinsic::BoolEq,
    );
    intrinsic_method(
        sa,
        "std::traits::Not for std::primitives::Bool#not",
        Intrinsic::BoolNot,
    );
}

fn resolve_char(sa: &Sema) {
    intrinsic_method(sa, "std::primitives::Char#toInt64", Intrinsic::CharToInt64);
    intrinsic_method(sa, "std::primitives::Char#toInt32", Intrinsic::CharToInt32);
    intrinsic_method(
        sa,
        "std::traits::Equals for std::primitives::Char#equals",
        Intrinsic::CharEq,
    );
    intrinsic_method(
        sa,
        "std::traits::Comparable for std::primitives::Char#cmp",
        Intrinsic::CharCmp,
    );
}

fn resolve_int32(sa: &Sema) {
    intrinsic_method(
        sa,
        "std::primitives::Int32#toUInt8",
        Intrinsic::Int32ToUInt8,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int32#toCharUnchecked",
        Intrinsic::Int32ToChar,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int32#toInt64",
        Intrinsic::Int32ToInt64,
    );

    intrinsic_method(
        sa,
        "std::primitives::Int32#toFloat32",
        Intrinsic::Int32ToFloat32,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int32#toFloat64",
        Intrinsic::Int32ToFloat64,
    );

    intrinsic_method(
        sa,
        "std::primitives::Int32#asFloat32",
        Intrinsic::ReinterpretInt32AsFloat32,
    );

    intrinsic_method(
        sa,
        "std::traits::Equals for std::primitives::Int32#equals",
        Intrinsic::Int32Eq,
    );
    intrinsic_method(
        sa,
        "std::traits::Comparable for std::primitives::Int32#cmp",
        Intrinsic::Int32Cmp,
    );

    intrinsic_method(
        sa,
        "std::traits::Add for std::primitives::Int32#add",
        Intrinsic::Int32Add,
    );
    intrinsic_method(
        sa,
        "std::traits::Sub for std::primitives::Int32#sub",
        Intrinsic::Int32Sub,
    );
    intrinsic_method(
        sa,
        "std::traits::Mul for std::primitives::Int32#mul",
        Intrinsic::Int32Mul,
    );
    intrinsic_method(
        sa,
        "std::traits::Div for std::primitives::Int32#div",
        Intrinsic::Int32Div,
    );
    intrinsic_method(
        sa,
        "std::traits::Mod for std::primitives::Int32#modulo",
        Intrinsic::Int32Mod,
    );

    intrinsic_method(
        sa,
        "std::primitives::Int32#wrappingAdd",
        Intrinsic::Int32AddUnchecked,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int32#wrappingSub",
        Intrinsic::Int32SubUnchecked,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int32#wrappingMul",
        Intrinsic::Int32MulUnchecked,
    );

    intrinsic_method(
        sa,
        "std::traits::BitOr for std::primitives::Int32#bitor",
        Intrinsic::Int32Or,
    );
    intrinsic_method(
        sa,
        "std::traits::BitAnd for std::primitives::Int32#bitand",
        Intrinsic::Int32And,
    );
    intrinsic_method(
        sa,
        "std::traits::BitXor for std::primitives::Int32#bitxor",
        Intrinsic::Int32Xor,
    );

    intrinsic_method(
        sa,
        "std::traits::Shl for std::primitives::Int32#shl",
        Intrinsic::Int32Shl,
    );
    intrinsic_method(
        sa,
        "std::traits::Shr for std::primitives::Int32#shr",
        Intrinsic::Int32Shr,
    );
    intrinsic_method(
        sa,
        "std::traits::Sar for std::primitives::Int32#sar",
        Intrinsic::Int32Sar,
    );

    intrinsic_method(
        sa,
        "std::primitives::Int32#rotateLeft",
        Intrinsic::Int32RotateLeft,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int32#rotateRight",
        Intrinsic::Int32RotateRight,
    );

    intrinsic_method(
        sa,
        "std::traits::Neg for std::primitives::Int32#neg",
        Intrinsic::Int32Neg,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int32#wrappingNeg",
        Intrinsic::Int32NegUnchecked,
    );
    intrinsic_method(
        sa,
        "std::traits::Not for std::primitives::Int32#not",
        Intrinsic::Int32Not,
    );

    intrinsic_method(
        sa,
        "std::primitives::Int32#countZeroBits",
        Intrinsic::Int32CountZeroBits,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int32#countOneBits",
        Intrinsic::Int32CountOneBits,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int32#countZeroBitsLeading",
        Intrinsic::Int32CountZeroBitsLeading,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int32#countOneBitsLeading",
        Intrinsic::Int32CountOneBitsLeading,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int32#countZeroBitsTrailing",
        Intrinsic::Int32CountZeroBitsTrailing,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int32#countOneBitsTrailing",
        Intrinsic::Int32CountOneBitsTrailing,
    );
}

fn resolve_float32(sa: &Sema) {
    intrinsic_method(
        sa,
        "std::primitives::Float32#toInt32",
        Intrinsic::Float32ToInt32,
    );
    intrinsic_method(
        sa,
        "std::primitives::Float32#toInt64",
        Intrinsic::Float32ToInt64,
    );
    intrinsic_method(
        sa,
        "std::primitives::Float32#toFloat64",
        Intrinsic::PromoteFloat32ToFloat64,
    );

    intrinsic_method(
        sa,
        "std::primitives::Float32#asInt32",
        Intrinsic::ReinterpretFloat32AsInt32,
    );

    intrinsic_method(
        sa,
        "std::traits::Equals for std::primitives::Float32#equals",
        Intrinsic::Float32Eq,
    );
    intrinsic_method(
        sa,
        "std::traits::Comparable for std::primitives::Float32#cmp",
        Intrinsic::Float32Cmp,
    );

    intrinsic_method(
        sa,
        "std::traits::Add for std::primitives::Float32#add",
        Intrinsic::Float32Add,
    );
    intrinsic_method(
        sa,
        "std::traits::Sub for std::primitives::Float32#sub",
        Intrinsic::Float32Sub,
    );
    intrinsic_method(
        sa,
        "std::traits::Mul for std::primitives::Float32#mul",
        Intrinsic::Float32Mul,
    );
    intrinsic_method(
        sa,
        "std::traits::Div for std::primitives::Float32#div",
        Intrinsic::Float32Div,
    );

    intrinsic_method(
        sa,
        "std::traits::Neg for std::primitives::Float32#neg",
        Intrinsic::Float32Neg,
    );

    intrinsic_method(
        sa,
        "std::primitives::Float32#isNan",
        Intrinsic::Float32IsNan,
    );
    intrinsic_method(sa, "std::primitives::Float32#abs", Intrinsic::Float32Abs);

    intrinsic_method(
        sa,
        "std::primitives::Float32#roundToZero",
        Intrinsic::Float32RoundToZero,
    );
    intrinsic_method(
        sa,
        "std::primitives::Float32#roundUp",
        Intrinsic::Float32RoundUp,
    );
    intrinsic_method(
        sa,
        "std::primitives::Float32#roundDown",
        Intrinsic::Float32RoundDown,
    );
    intrinsic_method(
        sa,
        "std::primitives::Float32#roundHalfEven",
        Intrinsic::Float32RoundHalfEven,
    );

    intrinsic_method(sa, "std::primitives::Float32#sqrt", Intrinsic::Float32Sqrt);
}

fn resolve_float64(sa: &Sema) {
    intrinsic_method(
        sa,
        "std::primitives::Float64#toInt32",
        Intrinsic::Float64ToInt32,
    );
    intrinsic_method(
        sa,
        "std::primitives::Float64#toInt64",
        Intrinsic::Float64ToInt64,
    );
    intrinsic_method(
        sa,
        "std::primitives::Float64#toFloat32",
        Intrinsic::DemoteFloat64ToFloat32,
    );

    intrinsic_method(
        sa,
        "std::primitives::Float64#asInt64",
        Intrinsic::ReinterpretFloat64AsInt64,
    );

    intrinsic_method(
        sa,
        "std::traits::Equals for std::primitives::Float64#equals",
        Intrinsic::Float64Eq,
    );
    intrinsic_method(
        sa,
        "std::traits::Comparable for std::primitives::Float64#cmp",
        Intrinsic::Float64Cmp,
    );

    intrinsic_method(
        sa,
        "std::traits::Add for std::primitives::Float64#add",
        Intrinsic::Float64Add,
    );
    intrinsic_method(
        sa,
        "std::traits::Sub for std::primitives::Float64#sub",
        Intrinsic::Float64Sub,
    );
    intrinsic_method(
        sa,
        "std::traits::Mul for std::primitives::Float64#mul",
        Intrinsic::Float64Mul,
    );
    intrinsic_method(
        sa,
        "std::traits::Div for std::primitives::Float64#div",
        Intrinsic::Float64Div,
    );

    intrinsic_method(
        sa,
        "std::traits::Neg for std::primitives::Float64#neg",
        Intrinsic::Float64Neg,
    );

    intrinsic_method(
        sa,
        "std::primitives::Float64#isNan",
        Intrinsic::Float64IsNan,
    );

    intrinsic_method(sa, "std::primitives::Float64#abs", Intrinsic::Float64Abs);

    intrinsic_method(
        sa,
        "std::primitives::Float64#roundToZero",
        Intrinsic::Float64RoundToZero,
    );
    intrinsic_method(
        sa,
        "std::primitives::Float64#roundUp",
        Intrinsic::Float64RoundUp,
    );
    intrinsic_method(
        sa,
        "std::primitives::Float64#roundDown",
        Intrinsic::Float64RoundDown,
    );
    intrinsic_method(
        sa,
        "std::primitives::Float64#roundHalfEven",
        Intrinsic::Float64RoundHalfEven,
    );

    intrinsic_method(sa, "std::primitives::Float64#sqrt", Intrinsic::Float64Sqrt);
}

fn resolve_int64(sa: &Sema) {
    intrinsic_method(
        sa,
        "std::primitives::Int64#toCharUnchecked",
        Intrinsic::Int64ToChar,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int64#toInt32",
        Intrinsic::Int64ToInt32,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int64#toUInt8",
        Intrinsic::Int64ToUInt8,
    );

    intrinsic_method(
        sa,
        "std::primitives::Int64#toFloat32",
        Intrinsic::Int64ToFloat32,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int64#toFloat64",
        Intrinsic::Int64ToFloat64,
    );

    intrinsic_method(
        sa,
        "std::primitives::Int64#asFloat64",
        Intrinsic::ReinterpretInt64AsFloat64,
    );

    intrinsic_method(
        sa,
        "std::traits::Equals for std::primitives::Int64#equals",
        Intrinsic::Int64Eq,
    );
    intrinsic_method(
        sa,
        "std::traits::Comparable for std::primitives::Int64#cmp",
        Intrinsic::Int64Cmp,
    );

    intrinsic_method(
        sa,
        "std::traits::Add for std::primitives::Int64#add",
        Intrinsic::Int64Add,
    );
    intrinsic_method(
        sa,
        "std::traits::Sub for std::primitives::Int64#sub",
        Intrinsic::Int64Sub,
    );
    intrinsic_method(
        sa,
        "std::traits::Mul for std::primitives::Int64#mul",
        Intrinsic::Int64Mul,
    );
    intrinsic_method(
        sa,
        "std::traits::Div for std::primitives::Int64#div",
        Intrinsic::Int64Div,
    );
    intrinsic_method(
        sa,
        "std::traits::Mod for std::primitives::Int64#modulo",
        Intrinsic::Int64Mod,
    );

    intrinsic_method(
        sa,
        "std::primitives::Int64#wrappingAdd",
        Intrinsic::Int64AddUnchecked,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int64#wrappingSub",
        Intrinsic::Int64SubUnchecked,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int64#wrappingMul",
        Intrinsic::Int64MulUnchecked,
    );

    intrinsic_method(
        sa,
        "std::traits::BitOr for std::primitives::Int64#bitor",
        Intrinsic::Int64Or,
    );
    intrinsic_method(
        sa,
        "std::traits::BitAnd for std::primitives::Int64#bitand",
        Intrinsic::Int64And,
    );
    intrinsic_method(
        sa,
        "std::traits::BitXor for std::primitives::Int64#bitxor",
        Intrinsic::Int64Xor,
    );

    intrinsic_method(
        sa,
        "std::traits::Shl for std::primitives::Int64#shl",
        Intrinsic::Int64Shl,
    );
    intrinsic_method(
        sa,
        "std::traits::Shr for std::primitives::Int64#shr",
        Intrinsic::Int64Shr,
    );
    intrinsic_method(
        sa,
        "std::traits::Sar for std::primitives::Int64#sar",
        Intrinsic::Int64Sar,
    );

    intrinsic_method(
        sa,
        "std::primitives::Int64#rotateLeft",
        Intrinsic::Int64RotateLeft,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int64#rotateRight",
        Intrinsic::Int64RotateRight,
    );

    intrinsic_method(
        sa,
        "std::traits::Neg for std::primitives::Int64#neg",
        Intrinsic::Int64Neg,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int64#wrappingNeg",
        Intrinsic::Int64NegUnchecked,
    );
    intrinsic_method(
        sa,
        "std::traits::Not for std::primitives::Int64#not",
        Intrinsic::Int64Not,
    );

    intrinsic_method(
        sa,
        "std::primitives::Int64#countZeroBits",
        Intrinsic::Int64CountZeroBits,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int64#countOneBits",
        Intrinsic::Int64CountOneBits,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int64#countZeroBitsLeading",
        Intrinsic::Int64CountZeroBitsLeading,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int64#countOneBitsLeading",
        Intrinsic::Int64CountOneBitsLeading,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int64#countZeroBitsTrailing",
        Intrinsic::Int64CountZeroBitsTrailing,
    );
    intrinsic_method(
        sa,
        "std::primitives::Int64#countOneBitsTrailing",
        Intrinsic::Int64CountOneBitsTrailing,
    );
}

fn resolve_array(sa: &Sema) {
    intrinsic_method(sa, "std::collections::Array#size", Intrinsic::ArrayLen);
    intrinsic_method(
        sa,
        "std::traits::IndexGet for std::collections::Array#get",
        Intrinsic::ArrayGet,
    );
    intrinsic_method(
        sa,
        "std::traits::IndexSet for std::collections::Array#set",
        Intrinsic::ArraySet,
    );

    intrinsic_method(
        sa,
        "std::collections::Array#unsafeNew",
        Intrinsic::ArrayNewOfSize,
    );
    intrinsic_method(
        sa,
        "std::collections::Array#new",
        Intrinsic::ArrayWithValues,
    );
}

fn resolve_option(sa: &mut Sema) {
    let fct_id = intrinsic_method(sa, "std::Option#isNone", Intrinsic::OptionIsNone);
    sa.known.functions.option_is_none = Some(fct_id);
    let fct_id = intrinsic_method(sa, "std::Option#isSome", Intrinsic::OptionIsSome);
    sa.known.functions.option_is_some = Some(fct_id);
    let fct_id = intrinsic_method(sa, "std::Option#getOrPanic", Intrinsic::OptionGetOrPanic);
    sa.known.functions.option_unwrap = Some(fct_id);
}

fn intrinsic_method(sa: &Sema, path: &str, intrinsic: Intrinsic) -> FctDefinitionId {
    let id = lookup_fct(sa, path);
    let fct = sa.fct(id);
    assert!(fct.intrinsic.set(intrinsic).is_ok());
    id
}

pub fn lookup_fct(sa: &Sema, path: &str) -> FctDefinitionId {
    if path.contains("#") {
        let parts = path.split("#").collect::<Vec<_>>();
        assert_eq!(parts.len(), 2);
        let path = parts[0];
        let method_name = parts[1];

        if path.contains(" for ") {
            let parts = path.split(" for ").collect::<Vec<_>>();
            assert_eq!(parts.len(), 2);

            let trait_path = parts[0];
            let extended_ty_path = parts[1];

            let trait_id = resolve_path(sa, trait_path)
                .to_trait()
                .expect("trait expected");
            let extended_ty = resolve_path(sa, extended_ty_path);
            let impl_id = lookup_impl_for_item(sa, trait_id, extended_ty).expect("impl not found");

            lookup_fct_by_impl_id_and_name(sa, impl_id, method_name)
                .expect("method in impl not found")
        } else {
            let extended_ty = resolve_path(sa, path);

            if extended_ty.is_trait() {
                let trait_id = extended_ty.to_trait().expect("trait expected");
                let trait_ = sa.trait_(trait_id);
                let method_name = sa.interner.intern(method_name);

                trait_
                    .instance_names()
                    .get(&method_name)
                    .cloned()
                    .expect("missing fct")
            } else {
                let extension_id =
                    lookup_extension_for_item(sa, extended_ty).expect("extension not found");

                lookup_fct_by_extension_id_and_name(sa, extension_id, method_name)
                    .expect("method in impl not found")
            }
        }
    } else {
        resolve_path(sa, path).to_fct().expect("function expected")
    }
}

fn lookup_impl_for_item(
    sa: &Sema,
    trait_id: TraitDefinitionId,
    extended_ty: SymbolKind,
) -> Option<ImplDefinitionId> {
    for (id, impl_) in sa.impls.iter() {
        if let Some(trait_ty) = impl_.trait_ty() {
            if trait_ty.trait_id != trait_id {
                continue;
            }

            if ty_matches_symbol(sa, impl_.extended_ty(), extended_ty.clone()) {
                return Some(id);
            }
        }
    }

    None
}

fn ty_matches_symbol(sa: &Sema, ty: SourceType, extended_ty: SymbolKind) -> bool {
    if let SymbolKind::Struct(id) = extended_ty {
        let struct_ = sa.struct_(id);

        if let Some(ref primitive_ty) = struct_.primitive_ty {
            return ty == primitive_ty.clone();
        }
    }

    match ty {
        SourceType::Class(class_id, ..) if extended_ty.to_class() == Some(class_id) => true,
        SourceType::Enum(enum_id, ..) if extended_ty.to_enum() == Some(enum_id) => true,
        SourceType::Struct(struct_id, ..) if extended_ty.to_struct() == Some(struct_id) => true,
        _ => false,
    }
}

fn lookup_extension_for_item(sa: &Sema, extended_ty: SymbolKind) -> Option<ExtensionDefinitionId> {
    for (id, ext) in sa.extensions.iter() {
        if ty_matches_symbol(sa, ext.ty().clone(), extended_ty.clone()) {
            return Some(id);
        }
    }

    None
}

fn lookup_fct_by_impl_id_and_name(
    sa: &Sema,
    impl_id: ImplDefinitionId,
    name: &str,
) -> Option<FctDefinitionId> {
    let impl_ = sa.impl_(impl_id);
    let name = sa.interner.intern(name);

    for &method_id in impl_.methods() {
        let method = sa.fct(method_id);
        if method.name == name {
            return Some(method_id);
        }
    }

    None
}

fn lookup_fct_by_extension_id_and_name(
    sa: &Sema,
    extension_id: ExtensionDefinitionId,
    name: &str,
) -> Option<FctDefinitionId> {
    let extension = sa.extension(extension_id);
    let name = sa.interner.intern(name);

    for &method_id in extension.methods() {
        let method = sa.fct(method_id);
        if method.name == name {
            return Some(method_id);
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn builtin_functions() {
        ok("fn f() { assert(true); }");
        ok("fn f() { print(\"test\"); }");
        ok("fn f() { println(\"test\"); }");
    }
}
