use crate::args;
use crate::error::diagnostics::IMPL_TRAIT_FOREIGN_TYPE;
use crate::sema::ConstValue;
use crate::stdlib_lookup::resolve_path;
use crate::tests::*;

#[test]
fn test_const_values() {
    let sa = ok("  const yes: Bool = true;
                        const x: UInt8 = 255u8;
                        const a: Int32 = 100i32;
                        const b: Int64 = 200i64;
                        const c: Char = 'A';
                        const d: Float32 = 3.0f32;
                        const e: Float64 = 6.0;");
    {
        let id = resolve_path(&sa, "<prog>::yes")
            .to_const()
            .expect("const expected");
        assert_eq!(&ConstValue::Bool(true), sa.const_(id).value());
    }

    {
        let id = resolve_path(&sa, "<prog>::x")
            .to_const()
            .expect("const expected");
        assert_eq!(&ConstValue::Int(255), sa.const_(id).value());
    }

    {
        let id = resolve_path(&sa, "<prog>::a")
            .to_const()
            .expect("const expected");
        assert_eq!(&ConstValue::Int(100), sa.const_(id).value());
    }

    {
        let id = resolve_path(&sa, "<prog>::b")
            .to_const()
            .expect("const expected");
        assert_eq!(&ConstValue::Int(200), sa.const_(id).value());
    }

    {
        let id = resolve_path(&sa, "<prog>::c")
            .to_const()
            .expect("const expected");
        assert_eq!(&ConstValue::Char('A'), sa.const_(id).value());
    }

    {
        let id = resolve_path(&sa, "<prog>::d")
            .to_const()
            .expect("const expected");
        assert_eq!(&ConstValue::Float(3.0), sa.const_(id).value());
    }

    {
        let id = resolve_path(&sa, "<prog>::e")
            .to_const()
            .expect("const expected");
        assert_eq!(&ConstValue::Float(6.0), sa.const_(id).value());
    }
}

#[test]
fn impl_method_call_with_impl_in_another_package() {
    pkg_test(
        "
        extern package dep1;

        use dep1::MyClass;

        pub trait Bar {
            fn f();
        }

        impl Bar for MyClass {
            fn f() {}
        }
    ",
        &[(
            "dep1",
            "
        pub trait Foo {
            fn f();
        }

        pub class MyClass

        impl Foo for MyClass {
            fn f() {}
        }

        fn f(x: MyClass) {
            x.f();
        }
            ",
        )],
        &[],
    );
}

#[test]
fn impl_method_call_with_invalid_impl_in_another_package() {
    pkg_test(
        "
        extern package dep1;

        use dep1::{Bar, MyClass};

        impl Bar for MyClass {
            fn f() {}
        }
    ",
        &[(
            "dep1",
            "
        pub trait Foo {
            fn f();
        }

        pub trait Bar {
            fn f();
        }

        pub class MyClass

        impl Foo for MyClass {
            fn f() {}
        }

        fn f(x: MyClass) {
            x.f();
        }
            ",
        )],
        &[(
            (6, 9),
            None,
            crate::ErrorLevel::Error,
            &IMPL_TRAIT_FOREIGN_TYPE,
            args!(),
        )],
    );
}
