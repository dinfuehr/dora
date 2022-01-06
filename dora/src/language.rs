use std::collections::HashSet;

use crate::language::error::msg::SemError;
use crate::language::sym::{NestedSymTable, Sym};
use crate::ty::SourceType;
use crate::vm::{FileId, SemAnalysis, TypeParam, TypeParamId};
use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

pub use globaldef::should_file_be_parsed;
pub use readty::{read_type, AllowSelf, TypeParamContext};

mod abstractck;
mod clsdefck;
mod constdefck;
mod enumck;
pub mod error;
mod extensiondefck;
mod fctbodyck;
mod fctdefck;
pub mod generator;
#[cfg(test)]
mod generator_tests;
mod globaldef;
mod globaldefck;
mod implck;
mod impldefck;
mod importck;
mod moduledefck;
mod readty;
mod returnck;
pub mod specialize;
pub(crate) mod stdlib;
mod structdefck;
mod superck;
pub mod sym;
#[cfg(test)]
pub mod test;
mod traitdefck;
pub mod typeparamck;

macro_rules! return_on_error {
    ($vm: ident) => {{
        if $vm.diag.lock().has_errors() {
            return true;
        }
    }};
}

pub fn check(sa: &mut SemAnalysis) -> bool {
    // add user defined fcts and classes to vm
    // this check does not look into fct or class bodies
    if let Err(_) = globaldef::check(sa) {
        return false;
    }
    return_on_error!(sa);

    // add internal annotations early
    stdlib::resolve_internal_annotations(sa);

    // define internal classes
    stdlib::resolve_internal_classes(sa);

    // discover all enum variants
    enumck::check_variants(sa);

    // fill prelude with important types and functions
    stdlib::fill_prelude(sa);

    // discover all types
    importck::check(sa);
    return_on_error!(sa);

    // find all trait implementations for classes
    impldefck::check(sa);

    // checks class/struct/trait definitions/bodies
    clsdefck::check(sa);
    moduledefck::check(sa);
    structdefck::check(sa);
    traitdefck::check(sa);
    globaldefck::check(sa);
    constdefck::check(sa);
    enumck::check(sa);
    extensiondefck::check(sa);
    return_on_error!(sa);

    // check super class definition of classes
    clsdefck::check_super_definition(sa);
    return_on_error!(sa);

    // check type definitions of params and return types in functions
    fctdefck::check(sa);
    return_on_error!(sa);

    superck::check_override(sa);
    return_on_error!(sa);

    // check impl methods against trait definition
    implck::check(sa);
    return_on_error!(sa);

    // define internal functions & methods
    stdlib::resolve_internal_functions(sa);
    stdlib::discover_known_methods(sa);

    // check for internal functions or classes
    internalck(sa);
    return_on_error!(sa);

    // add size of super classes to field offsets
    superck::check(sa);
    return_on_error!(sa);

    abstractck::check(sa);
    return_on_error!(sa);

    // check function body
    fctbodyck::check(sa);
    return_on_error!(sa);
    true
}

pub fn generate_bytecode(sa: &SemAnalysis) {
    for fct in sa.fcts.iter() {
        let bc = {
            let fct = fct.read();

            if !fct.has_body() {
                continue;
            }

            let analysis = fct.analysis();
            generator::generate(sa, &*fct, analysis)
        };

        fct.write().bytecode = Some(bc);
    }
}

fn internalck(sa: &SemAnalysis) {
    for fct in sa.fcts.iter() {
        let fct = fct.read();

        if fct.in_class() {
            continue;
        }

        if fct.internal && !fct.internal_resolved && !fct.has_body() {
            sa.diag
                .lock()
                .report(fct.file_id, fct.pos, SemError::UnresolvedInternal);
        }

        if !fct.has_body() && !fct.in_trait() && !fct.internal {
            sa.diag
                .lock()
                .report(fct.file_id, fct.pos, SemError::MissingFctBody);
        }
    }

    for xstruct in sa.structs.iter() {
        let xstruct = xstruct.read();

        if xstruct.internal && !xstruct.internal_resolved {
            sa.diag
                .lock()
                .report(xstruct.file_id, xstruct.pos, SemError::UnresolvedInternal);
        }
    }

    for cls in sa.classes.iter() {
        let cls = cls.read();

        if cls.internal && !cls.internal_resolved {
            sa.diag
                .lock()
                .report(cls.file_id, cls.pos, SemError::UnresolvedInternal);
        }

        for method in &cls.methods {
            let method = sa.fcts.idx(*method);
            let method = method.read();

            if method.internal && !method.internal_resolved && !method.has_body() {
                sa.diag
                    .lock()
                    .report(method.file_id, method.pos, SemError::UnresolvedInternal);
            }

            if !method.has_body() && !method.is_abstract && !method.internal {
                sa.diag
                    .lock()
                    .report(method.file_id, method.pos, SemError::MissingFctBody);
            }
        }
    }
}

pub fn always_returns(s: &ast::Stmt) -> bool {
    returnck::returns_value(s).is_ok()
}

pub fn expr_always_returns(e: &ast::Expr) -> bool {
    returnck::expr_returns_value(e).is_ok()
}

pub fn expr_block_always_returns(e: &ast::ExprBlockType) -> bool {
    returnck::expr_block_returns_value(e).is_ok()
}

pub fn report_sym_shadow(sa: &SemAnalysis, name: Name, file: FileId, pos: Position, sym: Sym) {
    let name = sa.interner.str(name).to_string();

    let msg = match sym {
        Sym::Class(_) => SemError::ShadowClass(name),
        Sym::Struct(_) => SemError::ShadowStruct(name),
        Sym::Trait(_) => SemError::ShadowTrait(name),
        Sym::Enum(_) => SemError::ShadowEnum(name),
        Sym::Fct(_) => SemError::ShadowFunction(name),
        Sym::Global(_) => SemError::ShadowGlobal(name),
        Sym::Const(_) => SemError::ShadowConst(name),
        Sym::Module(_) => SemError::ShadowModule(name),
        Sym::Var(_) => SemError::ShadowParam(name),
        Sym::Namespace(_) => SemError::ShadowNamespace(name),
        _ => unreachable!(),
    };

    sa.diag.lock().report(file, pos, msg);
}

fn check_type_params(
    sa: &SemAnalysis,
    ast_type_params: &[ast::TypeParam],
    type_params: &mut Vec<TypeParam>,
    symtable: &mut NestedSymTable,
    file_id: FileId,
    pos: Position,
) -> Vec<SourceType> {
    if ast_type_params.len() > 0 {
        let mut names = HashSet::new();
        let mut params = Vec::new();

        for (type_param_id, type_param) in ast_type_params.iter().enumerate() {
            if !names.insert(type_param.name) {
                let name = sa.interner.str(type_param.name).to_string();
                let msg = SemError::TypeParamNameNotUnique(name);
                sa.diag.lock().report(file_id, type_param.pos, msg);
            }

            params.push(SourceType::TypeParam(TypeParamId(type_param_id)));

            for bound in &type_param.bounds {
                let ty = read_type(
                    sa,
                    symtable,
                    file_id,
                    bound,
                    TypeParamContext::None,
                    AllowSelf::No,
                );

                match ty {
                    Some(SourceType::Trait(trait_id, _)) => {
                        if !type_params[type_param_id].trait_bounds.insert(trait_id) {
                            let msg = SemError::DuplicateTraitBound;
                            sa.diag.lock().report(file_id, type_param.pos, msg);
                        }
                    }

                    None => {
                        // unknown type, error is already thrown
                    }

                    _ => {
                        let msg = SemError::BoundExpected;
                        sa.diag.lock().report(file_id, bound.pos(), msg);
                    }
                }
            }

            let sym = Sym::TypeParam(TypeParamId(type_param_id));
            symtable.insert(type_param.name, sym);
        }

        params
    } else {
        let msg = SemError::TypeParamsExpected;
        sa.diag.lock().report(file_id, pos, msg);

        Vec::new()
    }
}

#[cfg(test)]
pub mod tests {
    use crate::language::error::msg::SemError;
    use crate::language::test;
    use crate::vm::SemAnalysis;
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
        F: FnOnce(&SemAnalysis) -> R,
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
