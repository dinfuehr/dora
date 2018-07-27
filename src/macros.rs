use ctxt::SemContext;
use semck::prelude::{native_fct,native_method,internal_class};


#[macro_export]
macro_rules! dora_macro_items { ($($x:item)+) => ($($x)+) }

#[macro_export]
macro_rules! dora_fn_pop {
    ($rt:ident $arg:ident : $t:ty) => {
        let $arg: $t = try!($rt.pop());
    };
    ($rt:ident $arg:ident : $t:ty, $($args:tt : $ts:ty),+) => {
        dyon_fn_pop!($rt $($args: $ts),+);
        let $arg: $t = try!($rt.pop());
    };
}

#[macro_export]
macro_rules! dora_fn {
    ($ctxt:expr;fn $name:ident () $b:block) => {
        let name = stringify!($name);
        pub extern "C" fn $name() {
            fn inner() {
                $b
            }
            inner();
        }
       
        native_fct($ctxt, name, $name as *const u8);
    };
    ($ctxt:expr; fn $name:ident () -> $rt:ty $b:block) => {
        let name = stringify!($name);
        pub extern "C" fn $name() -> $rt {
            fn inner() -> $rt {
                $b
            }
            inner()
        }
        native_fct($ctxt, name, $name as *const u8);
    };
    ($ctxt:expr; fn $name:ident ($($arg:tt : $t:ty),+) -> $rt:ty $b:block) => {
        dora_macro_items! {
            let name = stringify!($name);
            #[allow(non_snake_case)]
            pub fn $name() -> $rt {
                fn inner($($arg: $t),+) -> $rt {
                    $b
                }
                inner($($arg),+)
            }
            native_fct($ctxt, name, $name as *const u8);
        }
        
    };


    () => {
        
    };
}