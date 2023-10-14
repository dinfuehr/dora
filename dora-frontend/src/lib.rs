use crate::error::msg::ErrorMessage;
use crate::interner::Name;
use crate::sema::{Sema, SourceFileId};
use crate::sym::{Symbol, SymbolKind};
use dora_bytecode::{dump_stdout, Program};
use dora_parser::ast;
use dora_parser::Span;

pub use program_emitter::emit_program;
pub use readty::{read_type, read_type_context, read_type_unchecked, AllowSelf, TypeParamContext};

pub(crate) mod access;
mod clsdefck;
mod constdefck;
mod enumck;
pub mod error;
mod extensiondefck;
mod fctdefck;
pub mod generator;
mod globaldefck;
mod implck;
mod impldefck;
mod interner;
pub mod program_emitter;
mod program_parser;
mod readty;
mod returnck;
pub mod sema;
mod specialize;
mod stdlib_lookup;
mod structdefck;
pub mod sym;
#[cfg(test)]
mod test;
mod traitdefck;
pub mod ty;
mod type_params;
mod typeck;
mod typeparamck;
mod useck;
pub mod utils;

pub const STDLIB: &[(&str, &str)] = &include!(concat!(env!("OUT_DIR"), "/dora_stdlib_bundle.rs"));

pub use utils::{GrowableVec, Id, MutableVec};

macro_rules! return_on_error {
    ($vm: ident) => {{
        if $vm.diag.borrow().has_errors() {
            return false;
        }
    }};
}

pub fn check_program(sa: &mut Sema) -> bool {
    // This phase loads and parses all files. Also creates top-level-elements.
    program_parser::parse(sa);
    return_on_error!(sa);

    // Define internal types.
    stdlib_lookup::resolve_internal_types(sa);

    // Discover all enum variants.
    enumck::check_variants(sa);

    // Fill prelude with important types and functions.
    stdlib_lookup::setup_prelude(sa);

    // Discover all types.
    useck::check(sa);
    return_on_error!(sa);

    type_params::check(sa);
    return_on_error!(sa);

    // Find all trait implementations for types.
    impldefck::check(sa);
    return_on_error!(sa);

    // Checks class/struct/trait/enum definitions.
    clsdefck::check(sa);
    structdefck::check(sa);
    traitdefck::check(sa);
    enumck::check(sa);
    return_on_error!(sa);

    globaldefck::check(sa);
    constdefck::check(sa);
    extensiondefck::check(sa);
    return_on_error!(sa);

    // Check type definitions of params and return types in functions.
    fctdefck::check(sa);
    return_on_error!(sa);

    // Check impl methods against trait definition.
    implck::check(sa);
    return_on_error!(sa);

    // Define internal functions & methods.
    stdlib_lookup::resolve_internal_functions(sa);
    stdlib_lookup::discover_known_methods(sa);
    stdlib_lookup::create_lambda_class(sa);

    // Check for internal functions, methods or types.
    internalck(sa);
    return_on_error!(sa);

    // Check function body.
    typeck::check(sa);
    return_on_error!(sa);

    true
}

pub fn emit_ast(sa: &Sema, filter: &str) {
    for fct in sa.fcts.iter() {
        let fct = fct.read();
        let fct_name = fct.display_name(sa);

        if fct_pattern_match(&fct_name, filter) {
            ast::dump::dump_fct(&fct.ast);
        }
    }
}

pub fn generate_bytecode(sa: &Sema) {
    for fct in sa.fcts.iter() {
        let bc = {
            let fct = fct.read();

            if !fct.has_body() {
                continue;
            }

            let analysis = fct.analysis();
            generator::generate_fct(sa, &*fct, analysis)
        };

        fct.write().bytecode = Some(bc);
    }

    for global in sa.globals.iter() {
        let bc = {
            let global = global.read();

            if global.ast.initial_value.is_none() {
                continue;
            }

            let analysis = global.analysis.as_ref().expect("missing analysis");
            generator::generate_global_initializer(sa, &*global, analysis)
        };

        global.write().bytecode = Some(bc);
    }
}

pub fn emit_bytecode(prog: &Program, filter: &str) {
    for fct in prog.functions.iter() {
        if let Some(ref bc) = fct.bytecode {
            if fct_pattern_match(&fct.name, filter) {
                dump_stdout(prog, fct, bc);
            }
        }
    }
}

fn fct_pattern_match(name: &str, pattern: &str) -> bool {
    if pattern == "all" {
        return true;
    }

    for part in pattern.split(',') {
        if name.contains(part) {
            return true;
        }
    }

    false
}

fn internalck(sa: &Sema) {
    for fct in sa.fcts.iter() {
        let fct = fct.read();

        if !fct.has_body() && !fct.in_trait() && !fct.is_internal {
            sa.report(fct.file_id, fct.span, ErrorMessage::MissingFctBody);
        }
    }

    for (_struct_id, struct_) in sa.structs.iter() {
        if struct_.is_internal && !struct_.internal_resolved {
            sa.report(
                struct_.file_id,
                struct_.span,
                ErrorMessage::UnresolvedInternal,
            );
        }
    }

    for (_cls_id, cls) in sa.classes.iter() {
        if cls.is_internal && !cls.internal_resolved {
            sa.report(cls.file_id(), cls.span(), ErrorMessage::UnresolvedInternal);
        }
    }
}

pub fn always_returns(s: &ast::StmtData) -> bool {
    returnck::returns_value(s).is_ok()
}

pub fn expr_always_returns(e: &ast::ExprData) -> bool {
    returnck::expr_returns_value(e).is_ok()
}

pub fn expr_block_always_returns(e: &ast::ExprBlockType) -> bool {
    returnck::expr_block_returns_value(e).is_ok()
}

pub fn report_sym_shadow_span(sa: &Sema, name: Name, file: SourceFileId, span: Span, sym: Symbol) {
    let name = sa.interner.str(name).to_string();

    let msg = match sym.kind() {
        SymbolKind::Class(_) => ErrorMessage::ShadowClass(name),
        SymbolKind::Struct(_) => ErrorMessage::ShadowStruct(name),
        SymbolKind::Trait(_) => ErrorMessage::ShadowTrait(name),
        SymbolKind::Enum(_) => ErrorMessage::ShadowEnum(name),
        SymbolKind::Fct(_) => ErrorMessage::ShadowFunction(name),
        SymbolKind::Global(_) => ErrorMessage::ShadowGlobal(name),
        SymbolKind::Const(_) => ErrorMessage::ShadowConst(name),
        SymbolKind::Var(_) => ErrorMessage::ShadowParam(name),
        SymbolKind::Module(_) => ErrorMessage::ShadowModule(name),
        SymbolKind::TypeParam(_) => ErrorMessage::ShadowTypeParam(name),
        _ => unreachable!(),
    };

    sa.report(file, span, msg);
}

#[cfg(test)]
pub mod tests {
    use crate::error::msg::{ErrorDescriptor, ErrorMessage};
    use crate::sema::Sema;
    use crate::test;
    use dora_parser::{compute_line_column, compute_line_starts};

    pub fn ok(code: &'static str) {
        test::check(code, |vm| {
            let diag = vm.diag.borrow();
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
        F: FnOnce(&Sema) -> R,
    {
        test::check(code, |vm| {
            let diag = vm.diag.borrow();
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

    pub fn err(code: &'static str, loc: (u32, u32), msg: ErrorMessage) {
        test::check(code, |vm| {
            let diag = vm.diag.borrow();
            let errors = diag.errors();

            let error_loc = if errors.len() == 1 {
                compute_pos(code, &errors[0])
            } else {
                None
            };

            if errors.len() != 1 || error_loc != Some(loc) || errors[0].msg != msg {
                println!("expected:");
                println!("\t{:?} at {}:{}", msg, loc.0, loc.1);
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
            assert_eq!(Some(loc), error_loc);
            assert_eq!(msg, errors[0].msg);
        });
    }

    pub fn errors(code: &'static str, vec: &[((u32, u32), ErrorMessage)]) {
        test::check(code, |vm| {
            let diag = vm.diag.borrow();
            let errors = diag.errors();

            println!("errors = {:?}", errors);
            assert_eq!(vec.len(), errors.len());

            for (ind, error) in errors.iter().enumerate() {
                assert_eq!(Some(vec[ind].0), compute_pos(code, error));
                assert_eq!(vec[ind].1, error.msg);
            }
        });
    }

    fn compute_pos(code: &str, error: &ErrorDescriptor) -> Option<(u32, u32)> {
        if let Some(span) = error.span {
            let line_starts = compute_line_starts(code);
            Some(compute_line_column(&line_starts, span.start()))
        } else {
            None
        }
    }
}
