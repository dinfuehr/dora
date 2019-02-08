#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate dora;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
            dora::run_content(s);
    }
});
