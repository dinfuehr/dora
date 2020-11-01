use parking_lot::RwLock;

use crate::error::msg::SemError;
use crate::sym::{SymTable, TermSym, TypeSym};
use crate::typeck;
use crate::vm::{FileId, NodeMap, VM};
use dora_parser::ast::{Expr, ExprBlockType, Stmt};
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

pub use readty::{read_type_namespace, read_type_table};

mod abstractck;
mod clsdefck;
mod constdefck;
mod enumck;
mod extensiondefck;
mod fctdefck;
mod flowck;
mod globaldef;
mod globaldefck;
mod implck;
mod impldefck;
mod importck;
mod moduledefck;
pub(crate) mod prelude;
mod readty;
mod returnck;
pub mod specialize;
mod structdefck;
mod superck;
mod traitdefck;
pub mod typeparamck;

macro_rules! return_on_error {
    ($vm: ident) => {{
        if $vm.diag.lock().has_errors() {
            return;
        }
    }};
}

pub fn check(vm: &mut VM) {
    let mut map_cls_defs = NodeMap::new(); // get ClassId from ast node
    let mut map_struct_defs = NodeMap::new(); // get StructId from ast node
    let mut map_trait_defs = NodeMap::new(); // get TraitId from ast node
    let mut map_impl_defs = NodeMap::new(); // get ImplId from ast node
    let mut map_module_defs = NodeMap::new(); // get ModuleId from ast node
    let mut map_global_defs = NodeMap::new(); // get GlobalId from ast node
    let mut map_const_defs = NodeMap::new(); // get ConstId from ast node
    let mut map_enum_defs = NodeMap::new(); // get EnumId from ast node
    let mut map_extension_defs = NodeMap::new(); // get ExtensionId from ast node
    let mut map_namespaces = NodeMap::new(); // get NamespaceId from ast node

    // add user defined fcts and classes to vm
    // this check does not look into fct or class bodies
    globaldef::check(
        vm,
        &mut map_cls_defs,
        &mut map_struct_defs,
        &mut map_trait_defs,
        &mut map_impl_defs,
        &mut map_module_defs,
        &mut map_global_defs,
        &mut map_const_defs,
        &mut map_enum_defs,
        &mut map_extension_defs,
        &mut map_namespaces,
    );
    return_on_error!(vm);

    // define internal classes
    prelude::internal_classes(vm);

    // find all trait implementations for classes
    impldefck::check(vm, &map_impl_defs);

    // checks class/struct/trait definitions/bodies
    clsdefck::check(vm, &map_cls_defs);
    moduledefck::check(vm, &map_module_defs);
    structdefck::check(vm, &map_struct_defs);
    traitdefck::check(vm, &map_trait_defs);
    globaldefck::check(vm, &map_global_defs);
    constdefck::check(vm, &map_const_defs);
    enumck::check(vm);
    extensiondefck::check(vm, &map_extension_defs);
    importck::check(vm, &map_namespaces);
    return_on_error!(vm);

    // check super class definition of classes
    clsdefck::check_super_definition(vm, &map_cls_defs);
    return_on_error!(vm);

    // check type definitions of params and return types in functions
    fctdefck::check(vm);
    return_on_error!(vm);

    superck::check_override(vm);
    return_on_error!(vm);

    // check impl methods against trait definition
    implck::check(vm);
    return_on_error!(vm);

    // define internal functions & methods
    prelude::internal_functions(vm);
    prelude::known_methods(vm);

    // check types of expressions in functions
    typeck::check(vm);
    return_on_error!(vm);

    // are break and continue used in the right places?
    flowck::check(vm);

    // checks if function has a return value
    returnck::check(vm);

    // add size of super classes to field offsets
    superck::check(vm);
    return_on_error!(vm);

    abstractck::check(vm);

    // check for internal functions or classes
    internalck(vm);
    return_on_error!(vm);
}

pub fn bytecode(vm: &VM) {
    use crate::bytecode;

    for fct in vm.fcts.iter() {
        let bc = {
            let fct = fct.read();

            if !fct.is_src() {
                continue;
            }

            let src = fct.src();
            let src = src.read();

            bytecode::generate_generic(vm, &*fct, &*src)
        };

        let mut fct = fct.write();
        fct.bytecode = Some(bc);
    }
}

fn internalck(vm: &VM) {
    for fct in vm.fcts.iter() {
        let fct = fct.read();

        if fct.in_class() {
            continue;
        }

        if fct.internal && !fct.internal_resolved && fct.kind.is_definition() {
            vm.diag
                .lock()
                .report(fct.file, fct.pos, SemError::UnresolvedInternal);
        }

        if fct.kind.is_definition() && !fct.in_trait() && !fct.internal {
            vm.diag
                .lock()
                .report(fct.file, fct.pos, SemError::MissingFctBody);
        }
    }

    for cls in vm.classes.iter() {
        let cls = cls.read();

        if cls.internal && !cls.internal_resolved {
            vm.diag
                .lock()
                .report(cls.file, cls.pos, SemError::UnresolvedInternal);
        }

        for method in &cls.methods {
            let method = vm.fcts.idx(*method);
            let method = method.read();

            if method.internal && !method.internal_resolved && method.kind.is_definition() {
                vm.diag
                    .lock()
                    .report(method.file, method.pos, SemError::UnresolvedInternal);
            }

            if method.kind.is_definition() && !method.is_abstract && !method.internal {
                vm.diag
                    .lock()
                    .report(method.file, method.pos, SemError::MissingFctBody);
            }
        }
    }
}

pub fn always_returns(s: &Stmt) -> bool {
    returnck::returns_value(s).is_ok()
}

pub fn expr_always_returns(e: &Expr) -> bool {
    returnck::expr_returns_value(e).is_ok()
}

pub fn expr_block_always_returns(e: &ExprBlockType) -> bool {
    returnck::expr_block_returns_value(e).is_ok()
}

pub fn report_type_shadow(vm: &VM, name: Name, file: FileId, pos: Position, sym: TypeSym) {
    let name = vm.interner.str(name).to_string();

    let msg = match sym {
        TypeSym::Class(_) => SemError::ShadowClass(name),
        TypeSym::Struct(_) => SemError::ShadowStruct(name),
        TypeSym::Trait(_) => SemError::ShadowTrait(name),
        TypeSym::Enum(_) => SemError::ShadowEnum(name),
        _ => unimplemented!(),
    };

    vm.diag.lock().report(file, pos, msg);
}

pub fn report_term_shadow(vm: &VM, name: Name, file: FileId, pos: Position, sym: TermSym) {
    let name = vm.interner.str(name).to_string();

    let msg = match sym {
        TermSym::Fct(_) => SemError::ShadowFunction(name),
        TermSym::Global(_) => SemError::ShadowGlobal(name),
        TermSym::Const(_) => SemError::ShadowConst(name),
        TermSym::Module(_) => SemError::ShadowModule(name),
        TermSym::Var(_) => SemError::ShadowParam(name),
        TermSym::ClassConstructor(_) | TermSym::ClassConstructorAndModule(_, _) => {
            SemError::ShadowClassConstructor(name)
        }
        TermSym::StructConstructor(_) | TermSym::StructConstructorAndModule(_, _) => {
            SemError::ShadowStructConstructor(name)
        }
        TermSym::Namespace(_) => SemError::ShadowNamespace(name),
        x => unimplemented!("{:?}", x),
    };

    vm.diag.lock().report(file, pos, msg);
}

struct SemanticAnalysis {
    pub global_namespace: RwLock<SymTable>,
}

#[cfg(test)]
pub mod tests {
    use crate::error::msg::SemError;
    use crate::test;
    use crate::vm::VM;
    use dora_parser::lexer::position::Position;

    pub fn ok(code: &'static str) {
        test::parse_with_errors(code, |vm| {
            let diag = vm.diag.lock();
            let errors = diag.errors();

            println!("errors = {:?}", errors);
            println!();

            for e in errors {
                println!("{}", e.message(vm));
            }

            assert!(!diag.has_errors());
        });
    }

    pub fn ok_with_test<F, R>(code: &'static str, f: F) -> R
    where
        F: FnOnce(&VM) -> R,
    {
        test::parse_with_errors(code, |vm| {
            let diag = vm.diag.lock();
            let errors = diag.errors();

            println!("errors = {:?}", errors);

            for e in errors {
                println!("{}", e.message(vm));
            }

            assert!(!diag.has_errors());

            f(vm)
        })
    }

    pub fn err(code: &'static str, pos: Position, msg: SemError) {
        test::parse_with_errors(code, |vm| {
            let diag = vm.diag.lock();
            let errors = diag.errors();

            println!("errors = {:?}", errors);

            assert_eq!(1, errors.len(), "found {} errors instead", errors.len());
            assert_eq!(pos, errors[0].pos);
            assert_eq!(msg, errors[0].msg);
        });
    }

    pub fn errors(code: &'static str, vec: &[(Position, SemError)]) {
        test::parse_with_errors(code, |vm| {
            let diag = vm.diag.lock();
            let errors = diag.errors();

            println!("errors = {:?}", errors);
            assert_eq!(vec.len(), errors.len());

            for (ind, error) in errors.iter().enumerate() {
                assert_eq!(vec[ind].0, error.pos);
                assert_eq!(vec[ind].1, error.msg);
            }
        });
    }

    pub fn pos(line: u32, col: u32) -> Position {
        Position::new(line, col)
    }
}
