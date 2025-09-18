use lsp_server::Connection;
use std::error::Error;
use std::net::SocketAddr;

use crossbeam::select;

use crate::server::run_server;

mod server;
mod symbols;

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    let args = std::env::args().collect::<Vec<String>>();
    let mode = args.get(1).cloned().unwrap_or(String::new());
    let port = 8123;

    match mode.as_str() {
        "server" => {
            eprintln!("Start server listening on port {port}.");
            let address = SocketAddr::new("127.0.0.1".parse().expect("invalid IP"), port);
            let (conn, io_threads) = Connection::listen(address).expect("starting server failed");
            eprintln!("Accepted connection.");

            run_server(conn, io_threads)
        }

        "shim" => {
            let address = SocketAddr::new("127.0.0.1".parse().expect("invalid IP"), port);
            start_shim(address)
        }

        _ => {
            eprintln!("start server listening on stdin.");
            let (conn, io_threads) = Connection::stdio();
            run_server(conn, io_threads)
        }
    }
}

fn start_shim(address: SocketAddr) -> Result<(), Box<dyn Error + Sync + Send>> {
    eprintln!("Start shim and connect to port {}.", address.port());

    let (server_conn, server_io_threads) =
        Connection::connect(address).expect("connecting to server failed");
    eprintln!("Connected to server.");

    let (editor_conn, editor_io_threads) = Connection::stdio();
    let is_exited = false;

    while !is_exited {
        select! {
            recv(server_conn.receiver) -> msg => {
                match msg {
                    Ok(msg) => editor_conn.sender.send(msg).expect("sending to editor failed."),
                    Err(error) => {
                        eprintln!("error when receiving from server: {:?}.", error);
                        break;
                    }
                }
            }

            recv(editor_conn.receiver) -> msg => {
                match msg {
                    Ok(msg) => server_conn.sender.send(msg).expect("sending to server failed."),
                    Err(error) => {
                        eprintln!("error when receiving from server: {:?}.", error);
                        break;
                    }
                }
            }
        }
    }

    server_io_threads.join()?;
    editor_io_threads.join()?;

    Ok(())
}
