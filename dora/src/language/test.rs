use crate::driver::cmd::Args;
use crate::language;
use crate::vm::SemAnalysis;

pub fn parse<F, T>(code: &'static str, f: F) -> T
where
    F: FnOnce(&SemAnalysis) -> T,
{
    parse_with_errors(code, |sa| {
        if sa.diag.lock().has_errors() {
            sa.diag.lock().dump(sa);
            println!("{}", code);
            panic!("unexpected error in test::parse()");
        }

        f(sa)
    })
}

pub fn parse_with_errors<F, T>(code: &'static str, f: F) -> T
where
    F: FnOnce(&SemAnalysis) -> T,
{
    let args: Args = Default::default();
    let mut sa = SemAnalysis::new(args);
    sa.parse_arg_file = false;
    sa.test_file_as_string = Some(code);

    assert!(language::check(&mut sa));

    f(&sa)
}
