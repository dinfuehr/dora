use crate::error::diagnostics::{
    MISSING_FCT_BODY, SHADOW_CLASS, SHADOW_CONST, SHADOW_ENUM, SHADOW_FUNCTION, SHADOW_GLOBAL,
    SHADOW_MODULE, SHADOW_PARAM, SHADOW_STRUCT, SHADOW_TRAIT, SHADOW_TYPE_PARAM,
    UNRESOLVED_INTERNAL,
};
pub use crate::error::msg::{ErrorDescriptor, ErrorLevel};
use crate::interner::Name;
use crate::sema::{Sema, SourceFileId};
use crate::sym::{ModuleSymTable, SymTable, Symbol, SymbolKind};
#[allow(unused)]
use crate::ty::{SourceType, SourceTypeArray, TraitType, TyKind, contains_self, empty_sta};
use dora_bytecode::{FunctionId, Program, TypeParamMode, display_fct, dump_stdout};
use dora_parser::Span;
use dora_parser::ast;

pub use crate::extensiondefck::package_for_type;
pub use parsety::{ParsedTraitType, ParsedType, ParsedTypeAst};
pub use path::{PathKind, parse_path};
pub use program_emitter::emit_program;
pub use specialize::{
    CallSpecializationData, replace_type, specialize_for_element, specialize_trait_type,
    specialize_trait_type_generic, specialize_ty_for_call, specialize_ty_for_default_trait_method,
    specialize_ty_for_generic, specialize_ty_for_trait_object, specialize_type,
    specialize_type_array,
};
pub use vfs::Vfs;

pub(crate) mod access;
mod aliasck;
pub mod ast_dump;
mod clsdefck;
mod constdefck;
mod element_collector;
mod enumck;
pub mod error;
mod exhaustiveness;
mod expr_lowering;
mod extensiondefck;
mod fctdefck;
pub mod generator;
mod globaldefck;
mod impldefck;
mod interner;
mod parsety;
mod path;
pub mod program_emitter;
mod readty;
mod returnck;
pub mod sema;
mod specialize;
mod stdlib_lookup;
mod structdefck;
pub mod sym;
mod traitdefck;
pub mod ty;
mod typeck;
mod typedefck;
mod useck;
pub mod vfs;

pub fn check_program(sa: &mut Sema) -> bool {
    // This phase loads and parses all files. Also creates all elements.
    let module_symtables = element_collector::collect_elements(sa);

    // Discover all imported elements.
    useck::check(sa, module_symtables);

    // Fill prelude with important types and functions.
    stdlib_lookup::setup_prelude(sa);

    // Define internal types.
    stdlib_lookup::lookup_known_fundamental_types(sa);

    expr_lowering::lower_bodies(sa);

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

    if !sa.diag.borrow().has_errors() {
        exhaustiveness::check(sa);
    }

    !sa.diag.borrow().has_errors()
}

pub fn emit_ast(sa: &Sema, filter: &str) {
    for (_id, fct) in sa.fcts.iter() {
        let fct_name = fct.display_name(sa);

        if fct_pattern_match(&fct_name, filter) && fct.has_body(sa) {
            let file = sa.file(fct.file_id);
            ast_dump::dump_function(sa, file, fct);
        }
    }
}

pub fn emit_bytecode(prog: &Program, filter: &str) {
    for (id, fct) in prog.functions.iter().enumerate() {
        let id = FunctionId(id.try_into().expect("overflow"));
        if let Some(ref bc) = fct.bytecode {
            let name = display_fct(prog, id);
            if fct_pattern_match(&name, filter) {
                println!("Bytecode for {}:", name);
                dump_stdout(prog, bc, TypeParamMode::Unknown);
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
        if !fct.has_body(sa)
            && !fct.in_trait()
            && !fct.is_internal
            && !fct.use_trait_default_impl(sa)
        {
            sa.report(fct.file_id, fct.span, &MISSING_FCT_BODY, args!());
        }
    }

    for (_struct_id, struct_) in sa.structs.iter() {
        if struct_.is_internal && !struct_.internal_resolved {
            sa.report(struct_.file_id, struct_.span, &UNRESOLVED_INTERNAL, args!());
        }
    }

    for (_cls_id, cls) in sa.classes.iter() {
        if cls.is_internal && !cls.internal_resolved {
            sa.report(cls.file_id(), cls.span(), &UNRESOLVED_INTERNAL, args!());
        }
    }
}

pub fn always_returns(f: &ast::File, s: ast::AstStmt) -> bool {
    returnck::returns_value(f, s).is_ok()
}

pub fn expr_always_returns(f: &ast::File, expr: ast::AstExpr) -> bool {
    returnck::expr_returns_value(f, expr).is_ok()
}

pub fn expr_block_always_returns(f: &ast::File, e: ast::AstBlockExpr) -> bool {
    returnck::expr_block_returns_value(f, e).is_ok()
}

pub fn report_sym_shadow_span(sa: &Sema, name: Name, file: SourceFileId, span: Span, sym: Symbol) {
    let name = sa.interner.str(name).to_string();

    let desc = match sym.kind() {
        SymbolKind::Class(_) => &SHADOW_CLASS,
        SymbolKind::Struct(_) => &SHADOW_STRUCT,
        SymbolKind::Trait(_) => &SHADOW_TRAIT,
        SymbolKind::Enum(_) => &SHADOW_ENUM,
        SymbolKind::Fct(_) => &SHADOW_FUNCTION,
        SymbolKind::Global(_) => &SHADOW_GLOBAL,
        SymbolKind::Const(_) => &SHADOW_CONST,
        SymbolKind::Var(_) => &SHADOW_PARAM,
        SymbolKind::Module(_) => &SHADOW_MODULE,
        SymbolKind::TypeParam(_) => &SHADOW_TYPE_PARAM,
        _ => unreachable!(),
    };

    sa.report(file, span, desc, args!(name));
}

pub(crate) fn flatten_and(node: ast::AstBinExpr) -> Vec<ast::AstExpr> {
    assert_eq!(node.op(), ast::BinOp::And);
    let mut sub_lhs = node.lhs();
    let mut conditions = vec![node.rhs()];
    while let Some(bin_node) = sub_lhs.clone().to_bin_expr()
        && bin_node.op() == ast::BinOp::And
    {
        conditions.push(bin_node.rhs());
        sub_lhs = bin_node.lhs();
    }

    conditions.push(sub_lhs);
    conditions.reverse();
    conditions
}

#[cfg(test)]
mod tests {
    use crate::check_program;
    use crate::error::DescriptorArgs;
    use crate::error::diagnostics::{DiagnosticDescriptor, format_message};
    use crate::error::msg::{ErrorDescriptor, ErrorLevel};
    use crate::sema::{Sema, SemaCreationParams};
    use dora_parser::{compute_line_column, compute_line_starts};

    pub(crate) fn ok(code: &'static str) -> Sema {
        pkg_test(code, &[], &[])
    }

    pub(crate) fn err(
        code: &'static str,
        loc: (u32, u32),
        len: u32,
        level: ErrorLevel,
        desc: &'static DiagnosticDescriptor,
        args: DescriptorArgs,
    ) -> Sema {
        pkg_test(code, &[], &[(loc, Some(len), level, desc, args)])
    }

    pub(crate) fn has_errors(code: &'static str) -> Sema {
        let args: SemaCreationParams = SemaCreationParams::new().set_program_content(code);
        let mut sa = Sema::new(args);

        check_program(&mut sa);
        assert!(sa.diag.borrow().errors().len() > 0);
        sa
    }

    pub(crate) fn errors(
        code: &'static str,
        vec: Vec<(
            (u32, u32),
            u32,
            ErrorLevel,
            &'static DiagnosticDescriptor,
            DescriptorArgs,
        )>,
    ) -> Sema {
        let errors = vec
            .into_iter()
            .map(|(pos, len, level, desc, args)| (pos, Some(len), level, desc, args))
            .collect::<Vec<_>>();
        pkg_test(code, &[], &errors)
    }

    pub(crate) fn pkg_test(
        code: &str,
        packages: &[(&str, &str)],
        vec: &[(
            (u32, u32),
            Option<u32>,
            ErrorLevel,
            &DiagnosticDescriptor,
            DescriptorArgs,
        )],
    ) -> Sema {
        let args: SemaCreationParams = SemaCreationParams::new()
            .set_program_content(code)
            .set_package_contents(packages);
        let mut sa = Sema::new(args);

        check_program(&mut sa);

        println!("expected errors:");
        for ((line, col), len, level, desc, desc_args) in vec {
            let name = match level {
                crate::ErrorLevel::Error => "Error",
                ErrorLevel::Warn => "Warning",
            };
            let formatted = format_message(desc.message, desc_args, &sa);

            println!(
                "{} at {}:{} (len={:?}): {} -> {}",
                name, line, col, len, desc.message, formatted
            );
        }
        println!("");

        // By default warnings are ignored. They are only included when warnings are expected by the test.
        let uses_warnings = vec.iter().find(|e| e.2 == ErrorLevel::Warn).is_some();

        let mut errors = sa.diag.borrow().errors().to_vec();

        if uses_warnings {
            let mut warnings = sa.diag.borrow().warnings().to_vec();
            errors.append(&mut warnings);
        }

        println!("actual errors:");
        report_errors(&sa, &mut errors);

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
            if let Some(len) = vec[ind].1 {
                let got = error.span.expect("missing span").len();
                assert_eq!(len, got, "\nexpected length {} but got {}", len, got);
            }
            assert_eq!(
                vec[ind].2, error.level,
                "\nexpected: {:?}\n but got: {:?}",
                vec[ind].2, error.level
            );
            let expected_msg = format_message(vec[ind].3.message, &vec[ind].4, &sa);
            assert_eq!(
                expected_msg,
                error.message(&sa),
                "\nexpected: {:?}\n but got: {:?}",
                expected_msg,
                error.message(&sa)
            );
        }

        println!("============\n\n");
        sa
    }

    fn report_errors(sa: &Sema, errors: &mut Vec<ErrorDescriptor>) {
        errors.sort_by(crate::error::diag::sort_by);

        for e in errors {
            if let Some(file_id) = e.file_id {
                println!("{}:", sa.file(file_id).path.display());
            }

            if let Some((line, col)) = e.line_column(sa) {
                print!("{}:{}: ", line, col);
            }

            println!("{:?} -> {}", e.desc.message, e.message(sa));
        }
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
