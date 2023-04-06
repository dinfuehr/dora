use crate::language::error::msg::ErrorMessage;
use crate::language::sem_analysis::{FctDefinition, SemAnalysis, SourceFileId};
use crate::language::sym::Sym;
use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::{Position, Span};

pub use program::emit_program;
pub use readty::{read_type, read_type_unchecked, AllowSelf, TypeParamContext};

pub(crate) mod access;
mod clsdefck;
mod constdefck;
mod dumper;
mod enumck;
pub mod error;
mod extensiondefck;
mod fctbodyck;
mod fctdefck;
pub mod generator;
#[cfg(test)]
mod generator_tests;
mod globaldefck;
mod implck;
mod impldefck;
pub mod program;
mod program_parser;
mod readty;
mod returnck;
pub mod sem_analysis;
mod specialize;
mod stdlib;
mod structdefck;
pub mod sym;
#[cfg(test)]
mod test;
mod traitdefck;
pub mod ty;
mod type_params;
mod typeparamck;
mod useck;

macro_rules! return_on_error {
    ($vm: ident) => {{
        if $vm.diag.lock().has_errors() {
            return false;
        }
    }};
}

pub fn check(sa: &mut SemAnalysis) -> bool {
    // This phase loads and parses all files. Also creates top-level-elements.
    program_parser::parse(sa);
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
    useck::check(sa);
    return_on_error!(sa);

    type_params::check(sa);
    return_on_error!(sa);

    // find all trait implementations for classes
    impldefck::check(sa);

    // checks class/struct/trait/enum definitions
    clsdefck::check(sa);
    structdefck::check(sa);
    traitdefck::check(sa);
    enumck::check(sa);
    return_on_error!(sa);

    globaldefck::check(sa);
    constdefck::check(sa);
    extensiondefck::check(sa);
    return_on_error!(sa);

    // check type definitions of params and return types in functions
    fctdefck::check(sa);
    return_on_error!(sa);

    // check impl methods against trait definition
    implck::check(sa);
    return_on_error!(sa);

    // define internal functions & methods
    stdlib::resolve_internal_functions(sa);
    stdlib::discover_known_methods(sa);
    stdlib::create_lambda_class(sa);

    // check for internal functions or classes
    internalck(sa);
    return_on_error!(sa);

    // check function body
    fctbodyck::check(sa);
    return_on_error!(sa);

    true
}

pub fn emit_ast(sa: &SemAnalysis, filter: &str) {
    for fct in sa.fcts.iter() {
        let fct = fct.read();

        if fct_pattern_match(sa, &*fct, filter) {
            ast::dump::dump_fct(&fct.ast, &sa.interner);
        }
    }
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

pub fn emit_bytecode(sa: &SemAnalysis, filter: &str) {
    for fct in sa.fcts.iter() {
        let fct = fct.read();

        if fct_pattern_match(sa, &*fct, filter) {
            dumper::dump(sa, Some(&*fct), fct.bytecode.as_ref().unwrap());
        }
    }
}

fn fct_pattern_match(sa: &SemAnalysis, fct: &FctDefinition, pattern: &str) -> bool {
    if pattern == "all" {
        return true;
    }

    let fct_name = fct.display_name(sa);

    for part in pattern.split(',') {
        if fct_name.contains(part) {
            return true;
        }
    }

    false
}

fn internalck(sa: &SemAnalysis) {
    for fct in sa.fcts.iter() {
        let fct = fct.read();

        if !fct.has_body() && !fct.in_trait() && !fct.internal {
            sa.diag
                .lock()
                .report_span(fct.file_id, fct.span, ErrorMessage::MissingFctBody);
        }
    }

    for struct_ in sa.structs.iter() {
        let struct_ = struct_.read();

        if struct_.internal && !struct_.internal_resolved {
            sa.diag.lock().report_span(
                struct_.file_id,
                struct_.span,
                ErrorMessage::UnresolvedInternal,
            );
        }
    }

    for cls in sa.classes.iter() {
        let cls = cls.read();

        if cls.internal && !cls.internal_resolved {
            sa.diag
                .lock()
                .report_span(cls.file_id(), cls.span(), ErrorMessage::UnresolvedInternal);
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

pub fn report_sym_shadow(
    sa: &SemAnalysis,
    name: Name,
    file: SourceFileId,
    pos: Position,
    sym: Sym,
) {
    let name = sa.interner.str(name).to_string();

    let msg = match sym {
        Sym::Class(_) => ErrorMessage::ShadowClass(name),
        Sym::Struct(_) => ErrorMessage::ShadowStruct(name),
        Sym::Trait(_) => ErrorMessage::ShadowTrait(name),
        Sym::Enum(_) => ErrorMessage::ShadowEnum(name),
        Sym::Fct(_) => ErrorMessage::ShadowFunction(name),
        Sym::Global(_) => ErrorMessage::ShadowGlobal(name),
        Sym::Const(_) => ErrorMessage::ShadowConst(name),
        Sym::Var(_) => ErrorMessage::ShadowParam(name),
        Sym::Module(_) => ErrorMessage::ShadowModule(name),
        Sym::TypeParam(_) => ErrorMessage::ShadowTypeParam(name),
        _ => unreachable!(),
    };

    sa.diag.lock().report(file, pos, msg);
}

pub fn report_sym_shadow_span(
    sa: &SemAnalysis,
    name: Name,
    file: SourceFileId,
    span: Span,
    sym: Sym,
) {
    let name = sa.interner.str(name).to_string();

    let msg = match sym {
        Sym::Class(_) => ErrorMessage::ShadowClass(name),
        Sym::Struct(_) => ErrorMessage::ShadowStruct(name),
        Sym::Trait(_) => ErrorMessage::ShadowTrait(name),
        Sym::Enum(_) => ErrorMessage::ShadowEnum(name),
        Sym::Fct(_) => ErrorMessage::ShadowFunction(name),
        Sym::Global(_) => ErrorMessage::ShadowGlobal(name),
        Sym::Const(_) => ErrorMessage::ShadowConst(name),
        Sym::Var(_) => ErrorMessage::ShadowParam(name),
        Sym::Module(_) => ErrorMessage::ShadowModule(name),
        Sym::TypeParam(_) => ErrorMessage::ShadowTypeParam(name),
        _ => unreachable!(),
    };

    sa.diag.lock().report_span(file, span, msg);
}

#[cfg(test)]
pub mod tests {
    use crate::language::error::msg::{ErrorDescriptor, ErrorMessage};
    use crate::language::sem_analysis::SemAnalysis;
    use crate::language::test;
    use dora_parser::lexer::position::Position;
    use dora_parser::{compute_line_column, compute_line_starts};

    pub fn ok(code: &'static str) {
        test::check(code, |vm| {
            let diag = vm.diag.lock();
            let errors = diag.errors();

            for e in errors {
                println!("{}", e.message(vm));
                println!("{:?}", e);
                println!();
            }

            assert!(!diag.has_errors(), "program should not have errors.");
        });
    }

    pub fn ok_with_test<F, R>(code: &'static str, f: F) -> R
    where
        F: FnOnce(&SemAnalysis) -> R,
    {
        test::check(code, |vm| {
            let diag = vm.diag.lock();
            let errors = diag.errors();

            for e in errors {
                println!("{}", e.message(vm));
                println!("{:?}", e);
                println!();
            }

            assert!(!diag.has_errors(), "program should not have errors.");

            f(vm)
        })
    }

    pub fn err(code: &'static str, pos: Position, msg: ErrorMessage) {
        test::check(code, |vm| {
            let diag = vm.diag.lock();
            let errors = diag.errors();

            let error_pos = if errors.len() == 1 {
                compute_pos(code, &errors[0])
            } else {
                None
            };

            if errors.len() != 1 || error_pos != Some(pos) || errors[0].msg != msg {
                println!("expected:");
                println!("\t{:?} at {}", msg, pos);
                println!();
                if errors.is_empty() {
                    println!("but got no error.");
                    println!();
                } else {
                    println!("but got:");
                    for error in errors {
                        println!("\t{:?} at {:?}", error.msg, compute_pos(code, error));
                        println!();
                    }
                }
            }

            assert_eq!(1, errors.len(), "found {} errors instead", errors.len());
            assert_eq!(Some(pos), error_pos);
            assert_eq!(msg, errors[0].msg);
        });
    }

    pub fn errors(code: &'static str, vec: &[(Position, ErrorMessage)]) {
        test::check(code, |vm| {
            let diag = vm.diag.lock();
            let errors = diag.errors();

            println!("errors = {:?}", errors);
            assert_eq!(vec.len(), errors.len());

            for (ind, error) in errors.iter().enumerate() {
                assert_eq!(Some(vec[ind].0), compute_pos(code, error));
                assert_eq!(vec[ind].1, error.msg);
            }
        });
    }

    fn compute_pos(code: &str, error: &ErrorDescriptor) -> Option<Position> {
        if let Some(pos) = error.pos {
            Some(pos)
        } else if let Some(span) = error.span {
            let line_starts = compute_line_starts(code);
            let (line, column) = compute_line_column(&line_starts, span.start());
            Some(Position::new(line, column))
        } else {
            None
        }
    }

    pub fn pos(line: u32, col: u32) -> Position {
        Position::new(line, col)
    }
}
