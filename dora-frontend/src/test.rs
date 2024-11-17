use crate::check_program;
use crate::sema::{Sema, SemaFlags};

pub fn check<F, T>(code: &'static str, f: F) -> T
where
    F: FnOnce(&Sema) -> T,
{
    let args: SemaFlags = SemaFlags::for_test(code, &[]);
    let mut sa = Sema::new(args);

    let result = check_program(&mut sa);
    assert_eq!(result, !sa.diag.borrow().has_errors());

    f(&sa)
}
