use crate::check_program;
use crate::sema::{Sema, SemaArgs};

pub fn check_valid<F, T>(code: &'static str, f: F) -> T
where
    F: FnOnce(&Sema) -> T,
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
    F: FnOnce(&Sema) -> T,
{
    let args: SemaArgs = SemaArgs::for_test(code);
    let mut sa = Sema::new(args);

    let result = check_program(&mut sa);
    assert_eq!(result, !sa.diag.lock().has_errors());

    f(&sa)
}
