use ctxt::SemContext;
use semck::prelude::{native_fct,native_method,internal_class};





#[macro_export]
macro_rules! dora_ffi {
        ($ctxt:expr;$(fun $i:ident($($arg:ident: $argty:ty),*) -> $ret:ty $b:block)*) => ($(
                let name = stringify!($i);

                pub extern "C" fn $i($($arg: $argty),*) -> $ret {
                        $b
                }
                native_fct($ctxt, name, $i as *const u8);

        )*);
        ($ctxt:expr;$(fun $i:ident($($arg:ident: $argty:ty),*) $b:block)*) => ($(
                let name = stringify!($i);

                pub extern "C" fn $i($($arg: $argty),*) {
                        $b
                }
                native_fct($ctxt, name, $i as *const u8);

        )*)
}
