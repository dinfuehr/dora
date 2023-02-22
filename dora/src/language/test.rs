use crate::language;
use crate::language::sem_analysis::{SemAnalysis, SemAnalysisArgs};

pub fn check_valid<F, T>(code: &'static str, f: F) -> T
where
    F: FnOnce(&SemAnalysis) -> T,
{
    check(code, |sa| {
        if sa.diag.lock().has_errors() {
            sa.diag.lock().dump(sa);
            println!("{}", code);
            panic!("unexpected error in test::parse()");
        }

        f(sa)
    })
}

pub fn check<F, T>(code: &'static str, f: F) -> T
where
    F: FnOnce(&SemAnalysis) -> T,
{
    let args: SemAnalysisArgs = SemAnalysisArgs::for_test(code);
    let mut sa = SemAnalysis::new(args);

    let result = language::check(&mut sa);
    assert_eq!(result, !sa.diag.lock().has_errors());

    f(&sa)
}
