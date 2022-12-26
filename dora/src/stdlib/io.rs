use std::str::FromStr;
use std::{fs, path::PathBuf};

use crate::handle::{handle_scope, Handle};
use crate::object::{Ref, Str};
use crate::vm::get_vm;

pub extern "C" fn read_file_as_string(name: Handle<Str>) -> Ref<Str> {
    let path = PathBuf::from_str(name.content_utf8()).expect("should be a valid path");
    let content = fs::read_to_string(path).expect("can't read file");

    handle_scope(|| {
        let vm = get_vm();
        Str::from_buffer(vm, content.as_bytes())
    })
}
