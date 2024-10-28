pub use crate::error::msg::ErrorMessage;
use crate::interner::Name;
use crate::sema::{Sema, SourceFileId};
use crate::sym::{ModuleSymTable, SymTable, Symbol, SymbolKind};
#[allow(unused)]
use crate::ty::{contains_self, empty_sta, SourceType, SourceTypeArray, TraitType, TyKind};
use dora_bytecode::{display_fct, dump_stdout, FunctionId, Program};
use dora_parser::ast;
use dora_parser::Span;

pub use crate::extensiondefck::package_for_type;
pub use parsety::{ParsedTraitType, ParsedType, ParsedTypeAst};
pub use path::{parse_path, PathKind};
pub use program_emitter::emit_program;
pub use specialize::{
    replace_type, specialize_for_element, specialize_trait_type, specialize_type,
    specialize_type_array,
};

pub(crate) mod access;
mod aliasck;
mod clsdefck;
mod constdefck;
mod enumck;
pub mod error;
mod extensiondefck;
mod fctdefck;
pub mod generator;
mod globaldefck;
mod impldefck;
mod interner;
mod parsety;
mod path;
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
mod typeck;
mod typedefck;
mod typeparamck;
mod useck;

pub fn check_program(sa: &mut Sema) -> bool {
    // This phase loads and parses all files. Also creates all elements.
    let module_symtables = program_parser::parse(sa);

    // Discover all imported elements.
    useck::check(sa, module_symtables);

    if sa.diag.borrow().has_errors() {
        return false;
    }

    // Fill prelude with important types and functions.
    stdlib_lookup::setup_prelude(sa);

    // Define internal types.
    stdlib_lookup::lookup_known_fundamental_types(sa);

    // Now all types are known and we can start parsing types/type bounds.
    typedefck::parse_types(sa);
    // Connect aliases in impl to trait.
    impldefck::connect_aliases_to_trait(sa);
    // Detect and clear alias cycles.
    aliasck::detect_cycles(sa);
    // Check types/type bounds for type params.
    typedefck::check_types(sa);
    // Expand all alias types.
    typedefck::expand_types(sa);

    // Checks class/struct/trait/enum/impl definitions.
    impldefck::check_definition(sa);
    impldefck::check_definition_against_trait(sa);
    impldefck::check_type_aliases_bounds(sa);
    impldefck::check_super_traits(sa);
    enumck::check(sa);
    globaldefck::check(sa);
    extensiondefck::check(sa);

    // Check type definitions of params and return types in functions.
    fctdefck::check(sa);

    // Define internal functions & methods.
    stdlib_lookup::resolve_internal_functions(sa);
    stdlib_lookup::lookup_known_methods(sa);
    stdlib_lookup::create_lambda_class(sa);

    // Check for internal functions, methods or types.
    internalck(sa);

    // Check function body.
    typeck::check(sa);

    !sa.diag.borrow().has_errors()
}

pub fn emit_ast(sa: &Sema, filter: &str) {
    for (_id, fct) in sa.fcts.iter() {
        let fct_name = fct.display_name(sa);

        if fct_pattern_match(&fct_name, filter) {
            ast::dump::dump_fct(&fct.ast);
        }
    }
}

pub fn generate_bytecode(sa: &Sema) {
    for (_id, fct) in sa.fcts.iter() {
        let bc = {
            if !fct.has_body() {
                continue;
            }

            let analysis = fct.analysis();
            generator::generate_fct(sa, &*fct, analysis)
        };

        assert!(fct.bytecode.set(bc).is_ok());
    }

    for (_id, global) in sa.globals.iter() {
        if !global.has_initial_value() {
            continue;
        }

        let analysis = global.analysis();
        let bc = generator::generate_global_initializer(sa, global, analysis);

        assert!(global.bytecode.set(bc).is_ok());
    }
}

pub fn emit_bytecode(prog: &Program, filter: &str) {
    for (id, fct) in prog.functions.iter().enumerate() {
        let id = FunctionId(id.try_into().expect("overflow"));
        if let Some(ref bc) = fct.bytecode {
            let name = display_fct(prog, id);
            if fct_pattern_match(&name, filter) {
                println!("Bytecode for {}:", name);
                dump_stdout(prog, bc);
            }
        }
    }
}

fn fct_pattern_match(name: &str, pattern: &str) -> bool {
    if pattern == "all" || pattern == "*" {
        return true;
    }

    for part in pattern.split(',') {
        if name.ends_with(part) {
            return true;
        }
    }

    false
}

fn internalck(sa: &Sema) {
    for (_id, fct) in sa.fcts.iter() {
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
        test::check(code, |sa| {
            let diag = sa.diag.borrow();
            let mut errors = diag.errors().to_vec();
            errors.sort_by_key(|e| e.span);

            println!("expected errors:");
            for ((line, col), err) in vec {
                println!("{}:{}: {:?} -> {}", line, col, err, err.message());
            }
            println!("");

            println!("actual errors:");
            for error in &errors {
                if let Some((line, col)) = error.line_column(sa) {
                    print!("{}:{}: ", line, col);
                }

                println!("{:?} -> {}", error.msg, error.message(sa));
            }
            println!("\n");

            assert_eq!(
                vec.len(),
                errors.len(),
                "test expects {} errors but actually got {} errors.",
                vec.len(),
                errors.len()
            );

            for (ind, error) in errors.iter().enumerate() {
                println!("compare error {}", ind);
                assert_eq!(Some(vec[ind].0), compute_pos(code, error));
                assert_eq!(
                    vec[ind].1, error.msg,
                    "\nexpected: {:?}\n but got: {:?}",
                    vec[ind].1, error.msg
                );
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
