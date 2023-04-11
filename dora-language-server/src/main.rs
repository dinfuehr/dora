use crossbeam::channel::{Receiver, Sender};
use crossbeam::select;
use std::path::PathBuf;
use std::sync::Arc;
use std::{collections::HashMap, error::Error};
use threadpool::ThreadPool;

use dora_parser::{compute_line_column, compute_line_starts};
use lsp_server::{Connection, Message, Notification};
use lsp_types::notification::Notification as _;
use lsp_types::{
    ClientCapabilities, Diagnostic, InitializeParams, Position, PublishDiagnosticsParams, Range,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    let (connection, io_threads) = Connection::stdio();
    eprintln!("start dora language server...");

    // Run the server
    let (id, params) = connection.initialize_start()?;

    let init_params: InitializeParams = serde_json::from_value(params).unwrap();
    let _client_capabilities: ClientCapabilities = init_params.capabilities;
    let server_capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        ..Default::default()
    };

    let initialize_data = serde_json::json!({
        "capabilities": server_capabilities,
        "serverInfo": {
            "name": "dora-language-server",
            "version": "0.0.2"
        }
    });

    connection.initialize_finish(id, initialize_data)?;
    let mut state = ServerState::new();
    event_loop(&mut state, &connection)?;
    io_threads.join()?;
    Ok(())
}

struct ServerState {
    opened_files: HashMap<PathBuf, String>,
    threadpool: ThreadPool,
    threadpool_sender: Sender<MainLoopTask>,
    threadpool_receiver: Receiver<MainLoopTask>,
}

impl ServerState {
    fn new() -> ServerState {
        let threadpool = ThreadPool::new(1);
        let (sender, receiver) = crossbeam::channel::unbounded();
        ServerState {
            opened_files: HashMap::new(),
            threadpool,
            threadpool_sender: sender,
            threadpool_receiver: receiver,
        }
    }
}

fn event_loop(
    server_state: &mut ServerState,
    connection: &Connection,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let mut event;

    loop {
        event = next_event(server_state, connection);
        if event.is_none() {
            return Ok(());
        }

        match event {
            Some(Event::LanguageServer(msg)) => handle_message(server_state, msg),
            Some(Event::MainLoopTask(task)) => {
                handle_main_loop_task(server_state, connection, task)
            }
            None => {
                return Ok(());
            }
        }
    }
}

fn next_event(server_state: &mut ServerState, connection: &Connection) -> Option<Event> {
    select! {
        recv(connection.receiver) -> msg => {
            match msg {
                Ok(msg) => {
                    if let Message::Notification(ref notification) = msg {
                        if notification.method == lsp_types::notification::Exit::METHOD {
                            return None;
                        }
                    }

                    return Some(Event::LanguageServer(msg));
                }

                Err(error) => {
                    eprintln!("error from lsp connection: {}.", error);
                    return None;
                }
            }
        }

        recv(server_state.threadpool_receiver) -> msg => {
            match msg {
                Ok(msg) => Some(Event::MainLoopTask(msg)),
                Err(error) => {
                    eprintln!("error from thread pool: {}.", error);
                    None
                }
            }
        }
    }
}

fn handle_main_loop_task(
    _server_state: &mut ServerState,
    connection: &Connection,
    task: MainLoopTask,
) {
    match task {
        MainLoopTask::SendResponse(msg) => connection.sender.send(msg).expect("send failed"),
    }
}

fn handle_message(server_state: &mut ServerState, msg: Message) {
    match msg {
        Message::Notification(notification) => {
            if notification.method == "textDocument/didChange" {
                did_change_notification(server_state, notification);
            } else if notification.method == "textDocument/didOpen" {
                did_open_notification(server_state, notification);
            } else if notification.method == "textDocument/didClose" {
                did_close_notification(server_state, notification);
            } else {
                eprintln!("unknown notification {}", notification.method);
                eprintln!("{}", notification.params);
            }
        }

        Message::Request(request) => eprintln!("unknown request {}", request.method),
        Message::Response(response) => eprintln!("unknown response {:?}", response.result),
    }
}

fn did_change_notification(server_state: &mut ServerState, notification: Notification) {
    let result =
        serde_json::from_value::<lsp_types::DidChangeTextDocumentParams>(notification.params);
    match result {
        Ok(result) => {
            let uri = result.text_document.uri;
            let content = result.content_changes[0].text.clone();
            let version = result.text_document.version;
            let sender = server_state.threadpool_sender.clone();

            server_state.threadpool.execute(move || {
                check_file(uri, content, version, sender);
            })
        }

        Err(_) => {
            eprintln!("broken json");
        }
    }
}

fn check_file(uri: Url, content: String, version: i32, sender: Sender<MainLoopTask>) {
    let mut interner = dora_parser::interner::Interner::new();
    let line_starts = compute_line_starts(&content);
    let parser = dora_parser::Parser::from_shared_string(Arc::new(content), &mut interner);
    let (_, _, errors) = parser.parse();
    let mut diagnostics = Vec::new();
    for error in errors {
        let (line, column) = compute_line_column(&line_starts, error.span.start());
        let start = Position::new(line - 1, column - 1);
        let (line, column) = compute_line_column(&line_starts, error.span.end());
        let end = Position::new(line - 1, column - 1);
        diagnostics.push(Diagnostic {
            range: Range::new(start, end),
            message: error.error.message(),
            ..Default::default()
        })
    }
    let params = PublishDiagnosticsParams {
        uri,
        version: Some(version),
        diagnostics,
    };
    let notification =
        lsp_server::Notification::new("textDocument/publishDiagnostics".into(), params);
    sender
        .send(MainLoopTask::SendResponse(Message::Notification(
            notification,
        )))
        .expect("failed send");
}

fn did_open_notification(_server_state: &mut ServerState, notification: Notification) {
    let result =
        serde_json::from_value::<lsp_types::DidOpenTextDocumentParams>(notification.params);
    match result {
        Ok(result) => {
            let path = result
                .text_document
                .uri
                .to_file_path()
                .expect("file path expected");
            let text = result.text_document.text;

            _server_state.opened_files.insert(path, text);
        }
        Err(_) => {}
    }
}

fn did_close_notification(_server_state: &mut ServerState, notification: Notification) {
    let result =
        serde_json::from_value::<lsp_types::DidCloseTextDocumentParams>(notification.params);
    match result {
        Ok(result) => {
            let path = result
                .text_document
                .uri
                .to_file_path()
                .expect("file path expected");
            _server_state.opened_files.remove(&path);
        }
        Err(_) => {}
    }
}

enum MainLoopTask {
    SendResponse(Message),
}

enum Event {
    LanguageServer(Message),
    MainLoopTask(MainLoopTask),
}
