use ctxt::SemContext;
use semck::prelude::{native_fct,native_method,internal_class};



#[macro_export]
macro_rules! dora_ffi {
	($ctxt:expr;$(fun $i:ident($($arg:ident: $argty:ty),*) -> $ret:ty $b:block)*) => ($(
		
        let name = stringify!($i);
        
        #[no_mangle]
		pub extern  "C" fn $i() -> $ret {
			pub fn inner($($arg: $argty),*) -> $ret {
                $b
            }
            inner($($arg: $argty),*)
		}
        native_fct($ctxt, name, $i as *const u8);
	)*);

    ($ctxt:expr;$(fun $i:ident() -> $ret:ty $b:block)*) => ($(
		
        let name = stringify!($i);
        
        #[no_mangle]
		pub extern  "C" fn $i() -> $ret {
			pub fn inner() -> $ret {
                $b
            }
            inner()
		}
        native_fct($ctxt, name, $i as *const u8);
	)*);

    ($ctxt:expr;$(fun $i:ident() $b:block)*) => ($(
		
        let name = stringify!($i);
        
        #[no_mangle]
		pub extern  "C" fn $i() {
			pub fn inner() {
                $b
            }
            inner()
		}
        native_fct($ctxt, name, $i as *const u8);
	)*);

    ($ctxt:expr;$(fun $i:ident($($arg:ident: $argty:ty),*) $b:block)*) => ($(
		
        let name = stringify!($i);
        
        #[no_mangle]
		pub extern  "C" fn $i() {
			pub fn inner($($arg: $argty),*) {
                $b
            }
            inner($($arg: $argty),*)
		}
        native_fct($ctxt, name, $i as *const u8);
	)*);
}

