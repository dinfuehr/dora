use std::fs::File;
use std::io::{Read, Write};
use std::str::FromStr;
use std::{fs, path::PathBuf};

use crate::handle::{handle_scope, Handle};
use crate::object::{byte_array_from_buffer, Ref, Str, UInt8Array};
use crate::threads::parked_scope;
use crate::vm::get_vm;

pub const IO_NATIVE_FUNCTIONS: &[(&'static str, *const u8)] = &[
    ("stdlib::io::socketConnect", socket_connect as *const u8),
    ("stdlib::io::socketClose", socket_close as *const u8),
    ("stdlib::io::socketWrite", socket_write as *const u8),
    ("stdlib::io::socketRead", socket_read as *const u8),
    ("stdlib::io::socketBind", socket_bind as *const u8),
    ("stdlib::io::socketAccept", socket_accept as *const u8),
    (
        "stdlib::io::readFileAsString",
        read_file_as_string as *const u8,
    ),
    (
        "stdlib::io::readFileAsBytes",
        read_file_as_bytes as *const u8,
    ),
    (
        "stdlib::io::writeFileAsString",
        write_file_as_string as *const u8,
    ),
    (
        "stdlib::io::writeFileAsBytes",
        write_file_as_bytes as *const u8,
    ),
];

extern "C" fn read_file_as_string(name: Handle<Str>) -> Ref<Str> {
    handle_scope(|| {
        let path = PathBuf::from_str(name.content_utf8());
        if path.is_err() {
            return Ref::null();
        }
        let path = path.unwrap();
        let content = parked_scope(|| fs::read_to_string(path));
        if content.is_err() {
            return Ref::null();
        }
        let content = content.unwrap();

        let vm = get_vm();
        Str::from_buffer(vm, content.as_bytes())
    })
}

extern "C" fn read_file_as_bytes(name: Handle<Str>) -> Ref<UInt8Array> {
    handle_scope(|| {
        let path = PathBuf::from_str(name.content_utf8());
        if path.is_err() {
            return Ref::null();
        }
        let path = path.unwrap();
        let content: Option<Vec<u8>> = parked_scope(|| {
            let f = File::open(&path);
            if f.is_err() {
                return None;
            }
            let mut f = f.unwrap();
            let mut buffer = Vec::new();
            match f.read_to_end(&mut buffer) {
                Ok(_) => Some(buffer),
                Err(_) => None,
            }
        });

        if content.is_none() {
            return Ref::null();
        }

        let content = content.unwrap();

        let vm = get_vm();
        byte_array_from_buffer(vm, &content)
    })
}

extern "C" fn write_file_as_string(name: Handle<Str>, content: Handle<Str>) -> bool {
    write_file_common(name, Vec::from(content.content_utf8()))
}

extern "C" fn write_file_as_bytes(name: Handle<Str>, content: Handle<UInt8Array>) -> bool {
    write_file_common(name, Vec::from(content.slice()))
}

fn write_file_common(name: Handle<Str>, content: Vec<u8>) -> bool {
    let path = PathBuf::from_str(name.content_utf8());

    if path.is_err() {
        return false;
    }

    let result: std::io::Result<()> = parked_scope(|| {
        let mut f = File::create(&path.unwrap())?;
        f.write_all(&content)?;
        Ok(())
    });

    if result.is_ok() {
        true
    } else {
        false
    }
}

#[cfg(unix)]
extern "C" fn socket_connect(addr: Handle<Str>) -> i32 {
    let addr = String::from(addr.content_utf8());
    parked_scope(|| {
        use std::net::TcpStream;
        use std::os::unix::prelude::IntoRawFd;

        if let Ok(stream) = TcpStream::connect(&addr) {
            stream.into_raw_fd() as i32
        } else {
            -1
        }
    })
}

#[cfg(windows)]
extern "C" fn socket_connect(_addr: Handle<Str>) -> i32 {
    unimplemented!()
}

#[cfg(unix)]
extern "C" fn socket_write(fd: i32, array: Handle<UInt8Array>, offset: i64, len: i64) -> i64 {
    let offset = offset as usize;
    let len = len as usize;

    if offset + len > array.slice().len() {
        return -1;
    }

    let buffer = Vec::from(&array.slice()[offset..offset + len]);
    parked_scope(|| {
        use std::net::TcpStream;
        use std::os::unix::prelude::FromRawFd;

        let mut stream = unsafe { TcpStream::from_raw_fd(fd) };
        let bytes = match stream.write(&buffer) {
            Ok(bytes) => bytes as i64,
            Err(_) => -1,
        };
        std::mem::forget(stream);
        bytes
    })
}

#[cfg(windows)]
extern "C" fn socket_write(_fd: i32, _array: Handle<UInt8Array>, _offset: i64, _len: i64) -> i64 {
    unimplemented!()
}

#[cfg(unix)]
extern "C" fn socket_read(fd: i32, mut array: Handle<UInt8Array>, offset: i64, len: i64) -> i64 {
    let offset = offset as usize;
    let len = len as usize;

    if offset + len > array.slice().len() {
        return -1;
    }

    let mut buffer = vec![0; len];

    let bytes = parked_scope(|| {
        use std::net::TcpStream;
        use std::os::unix::prelude::FromRawFd;

        let mut stream = unsafe { TcpStream::from_raw_fd(fd) };
        let bytes = match stream.read(&mut buffer) {
            Ok(bytes) => bytes as i64,
            Err(_) => -1,
        };
        std::mem::forget(stream);
        bytes
    });

    if bytes < 0 {
        return bytes;
    }

    for i in 0..bytes as usize {
        array.set_at(offset + i, buffer[i]);
    }

    bytes
}

#[cfg(windows)]
extern "C" fn socket_read(_fd: i32, _array: Handle<UInt8Array>, _offset: i64, _len: i64) -> i32 {
    unimplemented!()
}

#[cfg(unix)]
extern "C" fn socket_close(fd: i32) {
    parked_scope(|| {
        use std::net::TcpStream;
        use std::os::unix::prelude::FromRawFd;

        let stream = unsafe { TcpStream::from_raw_fd(fd) };
        std::mem::drop(stream)
    });
}

#[cfg(windows)]
extern "C" fn socket_close(_fd: i32) {
    unimplemented!()
}

#[cfg(unix)]
extern "C" fn socket_bind(addr: Handle<Str>) -> i32 {
    let addr = String::from(addr.content_utf8());
    parked_scope(|| {
        use std::net::TcpListener;
        use std::os::unix::prelude::IntoRawFd;

        if let Ok(stream) = TcpListener::bind(&addr) {
            stream.into_raw_fd() as i32
        } else {
            -1
        }
    })
}

#[cfg(windows)]
extern "C" fn socket_bind(_addr: Handle<Str>) -> i32 {
    unimplemented!()
}

#[cfg(unix)]
extern "C" fn socket_accept(fd: i32) -> i32 {
    parked_scope(|| {
        use std::net::TcpListener;
        use std::os::unix::prelude::{FromRawFd, IntoRawFd};

        let listener = unsafe { TcpListener::from_raw_fd(fd) };
        let result = if let Ok((stream, _)) = listener.accept() {
            stream.into_raw_fd() as i32
        } else {
            -1
        };
        std::mem::forget(listener);
        result
    })
}

#[cfg(windows)]
extern "C" fn socket_accept(_fd: i32) -> i32 {
    unimplemented!()
}
