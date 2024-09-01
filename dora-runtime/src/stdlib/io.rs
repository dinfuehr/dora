use std::fs::File;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::PathBuf;
use std::str::FromStr;
use std::u64;

use crate::handle::Handle;
use crate::object::{Str, UInt8Array};
use crate::threads::parked_scope;
use crate::vm::FctImplementation;

use FctImplementation::Native as N;

pub const IO_FUNCTIONS: &[(&'static str, FctImplementation)] = &[
    ("stdlib::io::socketConnect", N(socket_connect as *const u8)),
    ("stdlib::io::socketClose", N(socket_close as *const u8)),
    ("stdlib::io::socketWrite", N(socket_write as *const u8)),
    ("stdlib::io::socketRead", N(socket_read as *const u8)),
    ("stdlib::io::socketBind", N(socket_bind as *const u8)),
    ("stdlib::io::socketAccept", N(socket_accept as *const u8)),
    ("stdlib::io::fileCreate", N(file_create as *const u8)),
    ("stdlib::io::fileOpen", N(file_open as *const u8)),
    ("stdlib::io::fileWrite", N(file_write as *const u8)),
    ("stdlib::io::fileRead", N(file_read as *const u8)),
    ("stdlib::io::fileClose", N(file_close as *const u8)),
    ("stdlib::io::getStdHandle", N(get_std_handle as *const u8)),
];

#[repr(C)]
#[derive(Copy, Clone)]
struct NativeFd(u64);

impl std::fmt::Debug for NativeFd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

const INVALID_FD: NativeFd = NativeFd(u64::MAX);

extern "C" fn file_create(name: Handle<Str>) -> NativeFd {
    let path = PathBuf::from_str(name.content_utf8());

    if path.is_err() {
        return INVALID_FD;
    }

    let path = path.unwrap();

    parked_scope(|| {
        if let Ok(file) = File::create(path) {
            file_into_native_fd(file)
        } else {
            INVALID_FD
        }
    })
}

extern "C" fn file_open(name: Handle<Str>) -> NativeFd {
    let path = PathBuf::from_str(name.content_utf8());

    if path.is_err() {
        return INVALID_FD;
    }

    let path = path.unwrap();

    parked_scope(|| {
        if let Ok(file) = File::open(path) {
            file_into_native_fd(file)
        } else {
            INVALID_FD
        }
    })
}

extern "C" fn file_write(fd: NativeFd, array: Handle<UInt8Array>, offset: i64, len: i64) -> i64 {
    let offset = offset as usize;
    let len = len as usize;

    if offset + len > array.slice().len() {
        return -1;
    }

    let buffer = Vec::from(&array.slice()[offset..offset + len]);
    parked_scope(|| {
        let mut file = file_from_native_fd(fd);
        let bytes = match file.write(&buffer) {
            Ok(bytes) => bytes as i64,
            Err(_) => -1,
        };
        std::mem::forget(file);
        bytes
    })
}

extern "C" fn file_read(fd: NativeFd, array: Handle<UInt8Array>, offset: i64, len: i64) -> i64 {
    let offset = offset as usize;
    let len = len as usize;

    if offset + len > array.slice().len() {
        return -1;
    }

    let mut buffer = vec![0; len];
    let result = parked_scope(|| {
        let mut file = file_from_native_fd(fd);
        let result = file.read(&mut buffer[..]);
        std::mem::forget(file);
        result
    });

    match result {
        Ok(bytes) => {
            let start = array.data_address().offset(offset);

            unsafe {
                std::ptr::copy_nonoverlapping(buffer.as_ptr(), start.to_mut_ptr::<u8>(), bytes);
            }

            bytes as i64
        }

        Err(_) => -1,
    }
}

extern "C" fn file_close(fd: NativeFd) {
    parked_scope(|| {
        let file = file_from_native_fd(fd);
        std::mem::drop(file);
    })
}

#[cfg(windows)]
extern "C" fn get_std_handle(std_fd: i32) -> i64 {
    use windows_sys::Win32::System::Console::{
        GetStdHandle, STD_ERROR_HANDLE, STD_INPUT_HANDLE, STD_OUTPUT_HANDLE,
    };

    let value = match std_fd {
        0 => STD_INPUT_HANDLE,
        1 => STD_OUTPUT_HANDLE,
        2 => STD_ERROR_HANDLE,
        _ => unimplemented!(),
    };

    unsafe { GetStdHandle(value) as i64 }
}

#[cfg(unix)]
extern "C" fn get_std_handle(std_fd: i32) -> i64 {
    std_fd as i64
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

#[cfg(windows)]
fn file_into_native_fd(file: File) -> NativeFd {
    use std::os::windows::io::IntoRawHandle;
    let socket = File::into_raw_handle(file);
    NativeFd(socket as u64)
}

#[cfg(windows)]
fn file_from_native_fd(fd: NativeFd) -> File {
    use std::os::{raw::c_void, windows::io::FromRawHandle};
    unsafe { File::from_raw_handle(fd.0 as *mut c_void) }
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

#[cfg(unix)]
fn file_into_native_fd(file: File) -> NativeFd {
    use std::os::unix::prelude::IntoRawFd;
    let fd = File::into_raw_fd(file);
    NativeFd(fd as u32 as u64)
}

#[cfg(unix)]
fn file_from_native_fd(fd: NativeFd) -> File {
    use std::os::unix::prelude::FromRawFd;
    unsafe { File::from_raw_fd(fd.0 as i32) }
}
