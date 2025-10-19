#[cfg(test)]
mod tests {
    use dora_frontend::{
        check_program,
        sema::{Sema, SemaCreationParams, ToArcString},
    };

    fn def(code: &'static str, _offset: u32) -> u32 {
        let sema_params = SemaCreationParams::new().set_program_content(ToArcString::into(code));
        let mut sa = Sema::new(sema_params);
        check_program(&mut sa);
        0
    }

    #[test]
    fn class_in_type() {
        let def_at = def(
            "
            class Foo
            fn f(f: Foo) {}
        ",
            0,
        );
        assert_eq!(def_at, 0);
    }
}
