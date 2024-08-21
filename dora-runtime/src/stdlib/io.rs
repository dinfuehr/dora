use std::fs::File;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::str::FromStr;
use std::u64;
use std::{fs, path::PathBuf};

use crate::handle::{handle_scope, Handle};
use crate::object::{byte_array_from_buffer, Ref, Str, UInt8Array};
use crate::threads::parked_scope;
use crate::vm::{get_vm, FctImplementation};

use FctImplementation::Native as N;

pub const IO_FUNCTIONS: &[(&'static str, FctImplementation)] = &[
    ("stdlib::io::socketConnect", N(socket_connect as *const u8)),
    ("stdlib::io::socketClose", N(socket_close as *const u8)),
    ("stdlib::io::socketWrite", N(socket_write as *const u8)),
    ("stdlib::io::socketRead", N(socket_read as *const u8)),
    ("stdlib::io::socketBind", N(socket_bind as *const u8)),
    ("stdlib::io::socketAccept", N(socket_accept as *const u8)),
    (
        "stdlib::io::readFileAsString",
        N(read_file_as_string as *const u8),
    ),
    (
        "stdlib::io::readFileAsBytes",
        N(read_file_as_bytes as *const u8),
    ),
    (
        "stdlib::io::writeFileAsString",
        N(write_file_as_string as *const u8),
    ),
    (
        "stdlib::io::writeFileAsBytes",
        N(write_file_as_bytes as *const u8),
    ),
];

#[repr(C)]
struct NativeFd(u64);

const INVALID_FD: NativeFd = NativeFd(u64::MAX);

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

extern "C" fn socket_connect(addr: Handle<Str>) -> NativeFd {
    let addr = String::from(addr.content_utf8());
    parked_scope(|| {
        use std::net::TcpStream;

        if let Ok(stream) = TcpStream::connect(&addr) {
            tcp_stream_into_native_fd(stream)
        } else {
            INVALID_FD
        }
    })
}

extern "C" fn socket_write(fd: NativeFd, array: Handle<UInt8Array>, offset: i64, len: i64) -> i64 {
    let offset = offset as usize;
    let len = len as usize;

    if offset + len > array.slice().len() {
        return -1;
    }

    let buffer = Vec::from(&array.slice()[offset..offset + len]);
    parked_scope(|| {
        let mut stream = tcp_stream_from_native_fd(fd);
        let bytes = match stream.write(&buffer) {
            Ok(bytes) => bytes as i64,
            Err(_) => -1,
        };
        std::mem::forget(stream);
        bytes
    })
}

extern "C" fn socket_read(
    fd: NativeFd,
    mut array: Handle<UInt8Array>,
    offset: i64,
    len: i64,
) -> i64 {
    let offset = offset as usize;
    let len = len as usize;

    if offset + len > array.slice().len() {
        return -1;
    }

    let mut buffer = vec![0; len];

    let bytes = parked_scope(|| {
        let mut stream = tcp_stream_from_native_fd(fd);
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

extern "C" fn socket_close(fd: NativeFd) {
    parked_scope(|| {
        let stream = tcp_stream_from_native_fd(fd);
        std::mem::drop(stream)
    });
}

extern "C" fn socket_bind(addr: Handle<Str>) -> NativeFd {
    let addr = String::from(addr.content_utf8());
    parked_scope(|| {
        if let Ok(stream) = TcpListener::bind(&addr) {
            tcp_listener_into_native_fd(stream)
        } else {
            INVALID_FD
        }
    })
}

extern "C" fn socket_accept(fd: NativeFd) -> NativeFd {
    parked_scope(|| {
        let listener = tcp_listener_from_native_fd(fd);
        let result = if let Ok((stream, _)) = listener.accept() {
            tcp_stream_into_native_fd(stream)
        } else {
            INVALID_FD
        };
        std::mem::forget(listener);
        result
    })
}

#[cfg(windows)]
fn tcp_stream_into_native_fd(stream: TcpStream) -> NativeFd {
    use std::os::windows::io::IntoRawSocket;
    let socket = TcpStream::into_raw_socket(stream);
    NativeFd(socket)
}

#[cfg(windows)]
fn tcp_stream_from_native_fd(fd: NativeFd) -> TcpStream {
    use std::os::windows::io::FromRawSocket;

    unsafe { TcpStream::from_raw_socket(fd.0) }
}

#[cfg(windows)]
fn tcp_listener_into_native_fd(listener: TcpListener) -> NativeFd {
    use std::os::windows::io::IntoRawSocket;
    let socket = TcpListener::into_raw_socket(listener);
    NativeFd(socket)
}

#[cfg(windows)]
fn tcp_listener_from_native_fd(fd: NativeFd) -> TcpListener {
    use std::os::windows::io::FromRawSocket;
    unsafe { TcpListener::from_raw_socket(fd.0) }
}

#[cfg(unix)]
fn tcp_stream_into_native_fd(stream: TcpStream) -> NativeFd {
    use std::os::unix::prelude::IntoRawFd;
    let fd = TcpStream::into_raw_fd(stream);
    NativeFd(fd as u32 as u64)
}

#[cfg(unix)]
fn tcp_stream_from_native_fd(fd: NativeFd) -> TcpStream {
    use std::os::unix::prelude::FromRawFd;
    unsafe { TcpStream::from_raw_fd(fd.0 as i32) }
}

#[cfg(unix)]
fn tcp_listener_into_native_fd(stream: TcpListener) -> NativeFd {
    use std::os::unix::prelude::IntoRawFd;
    let fd = TcpListener::into_raw_fd(stream);
    NativeFd(fd as u32 as u64)
}

#[cfg(unix)]
fn tcp_listener_from_native_fd(fd: NativeFd) -> TcpListener {
    use std::os::unix::prelude::FromRawFd;
    unsafe { TcpListener::from_raw_fd(fd.0 as i32) }
}
