use crate::driver::cmd::Args;
use crate::language;
use crate::language::sem_analysis::SemAnalysis;

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
    let args: Args = Default::default();
    let mut sa = SemAnalysis::new(args);
    sa.test_file_as_string = Some(code);

    let result = language::check(&mut sa);
    assert_eq!(result, !sa.diag.lock().has_errors());

    f(&sa)
}
