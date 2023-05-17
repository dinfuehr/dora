use crossbeam::channel::{Receiver, Sender};
use crossbeam::select;
use dora_parser::{compute_line_column, compute_line_starts};
use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::notification::Notification as _;
use lsp_types::{
    ClientCapabilities, Diagnostic, DocumentSymbolResponse, InitializeParams, Location, OneOf,
    Position, PublishDiagnosticsParams, Range, ServerCapabilities, SymbolInformation, SymbolKind,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use threadpool::ThreadPool;
use walkdir::WalkDir;

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    let (connection, io_threads) = Connection::stdio();
    eprintln!("start dora language server...");

    // Run the server
    let (id, params) = connection.initialize_start()?;

    let init_params: InitializeParams = serde_json::from_value(params).unwrap();
    let client_capabilities: ClientCapabilities = init_params.capabilities;
    let client_workspace_folders = init_params.workspace_folders.unwrap_or_default();
    let mut workspace_folders = Vec::new();

    for workspace_folder in client_workspace_folders {
        let file_path = workspace_folder
            .uri
            .to_file_path()
            .expect("file path expected");
        assert!(file_path.is_absolute());
        eprintln!("workspace folder: {}", file_path.display());
        workspace_folders.push(file_path);
    }

    let server_capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        document_symbol_provider: Some(OneOf::Left(true)),
        ..Default::default()
    };

    let initialize_data = serde_json::json!({
        "capabilities": server_capabilities,
        "serverInfo": {
            "name": "dora-language-server",
            "version": "0.0.2"
        }
    });

    let projects = find_projects(&workspace_folders);
    for project in &projects {
        eprintln!("project {} -> {}", project.name, project.main.display());
    }

    connection.initialize_finish(id, initialize_data)?;
    let mut state = ServerState::new(client_capabilities, workspace_folders, projects);
    event_loop(&mut state, &connection)?;
    io_threads.join()?;
    Ok(())
}

struct ServerState {
    opened_files: HashMap<PathBuf, Arc<String>>,
    #[allow(dead_code)]
    client_capabilities: ClientCapabilities,
    #[allow(dead_code)]
    workspace_folders: Vec<PathBuf>,
    projects: Vec<ProjectConfig>,
    files_with_errors: HashSet<PathBuf>,
    threadpool: ThreadPool,
    threadpool_sender: Sender<MainLoopTask>,
    threadpool_receiver: Receiver<MainLoopTask>,
}

impl ServerState {
    fn new(
        client_capabilities: ClientCapabilities,
        workspace_folders: Vec<PathBuf>,
        projects: Vec<ProjectConfig>,
    ) -> ServerState {
        let threadpool = ThreadPool::new(1);
        let (sender, receiver) = crossbeam::channel::unbounded();
        ServerState {
            opened_files: HashMap::new(),
            client_capabilities,
            workspace_folders,
            projects,
            files_with_errors: HashSet::new(),
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
    server_state: &mut ServerState,
    connection: &Connection,
    task: MainLoopTask,
) {
    match task {
        MainLoopTask::SendResponse(msg) => connection.sender.send(msg).expect("send failed"),
        MainLoopTask::ReportError(errors_by_file) => {
            let mut last_files_with_errors =
                std::mem::replace(&mut server_state.files_with_errors, HashSet::new());

            for (file, errors) in errors_by_file {
                let params = PublishDiagnosticsParams {
                    uri: Url::from_file_path(&file).expect("broken file path"),
                    version: None,
                    diagnostics: errors,
                };
                let notification =
                    lsp_server::Notification::new("textDocument/publishDiagnostics".into(), params);
                let msg = Message::Notification(notification);
                connection.sender.send(msg).expect("send() failed");

                last_files_with_errors.remove(&file);
                server_state.files_with_errors.insert(file);
            }

            for file in last_files_with_errors {
                let params = PublishDiagnosticsParams {
                    uri: Url::from_file_path(&file).expect("broken file path"),
                    version: None,
                    diagnostics: Vec::new(),
                };
                let notification =
                    lsp_server::Notification::new("textDocument/publishDiagnostics".into(), params);
                let msg = Message::Notification(notification);
                connection.sender.send(msg).expect("send() failed");
            }
        }
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
            } else if notification.method == "textDocument/didSave" {
                did_save_notification(server_state, notification);
            } else {
                eprintln!("unknown notification {}", notification.method);
                eprintln!("{}", notification.params);
            }
        }

        Message::Request(request) => {
            if request.method == "textDocument/documentSymbol" {
                document_symbol_request(server_state, request);
            } else {
                eprintln!("unknown request {}", request.method);
                eprintln!("{}", request.params);
            }
        }

        Message::Response(response) => eprintln!("unknown response {:?}", response.result),
    }
}

fn did_change_notification(_server_state: &mut ServerState, notification: Notification) {
    let result =
        serde_json::from_value::<lsp_types::DidChangeTextDocumentParams>(notification.params);
    match result {
        Ok(_result) => {
            // let uri = result.text_document.uri;
            // let content = result.content_changes[0].text.clone();
            // let version = result.text_document.version;
            // let sender = server_state.threadpool_sender.clone();

            // server_state.threadpool.execute(move || {
            //     check_file(uri, content, version, sender);
            // })
        }

        Err(_) => {
            eprintln!("broken json");
        }
    }
}

#[allow(dead_code)]
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

            _server_state.opened_files.insert(path, Arc::new(text));
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

fn did_save_notification(server_state: &mut ServerState, notification: Notification) {
    let result =
        serde_json::from_value::<lsp_types::DidSaveTextDocumentParams>(notification.params);
    match result {
        Ok(_result) => {
            let sender = server_state.threadpool_sender.clone();
            let projects = server_state.projects.clone();

            server_state.threadpool.execute(move || {
                for project in projects {
                    compile_project(project, sender.clone());
                }
            })
        }
        Err(_) => {}
    }
}

fn document_symbol_request(server_state: &mut ServerState, request: Request) {
    let result = serde_json::from_value::<lsp_types::DocumentSymbolParams>(request.params);
    match result {
        Ok(result) => {
            let uri = result.text_document.uri.clone();
            let path = result
                .text_document
                .uri
                .to_file_path()
                .expect("file path expected");
            if let Some(_content) = server_state.opened_files.get(&path) {
                eprintln!("now parse file");
                #[allow(deprecated)]
                let symbols = vec![
                    SymbolInformation {
                        name: "foo".into(),
                        kind: SymbolKind::FUNCTION,
                        tags: None,
                        deprecated: None,
                        container_name: None,
                        location: Location {
                            uri: uri.clone(),
                            range: Range {
                                start: Position {
                                    line: 6,
                                    character: 7,
                                },
                                end: Position {
                                    line: 6,
                                    character: 21,
                                },
                            },
                        },
                    },
                    SymbolInformation {
                        name: "bar".into(),
                        kind: SymbolKind::FUNCTION,
                        tags: None,
                        deprecated: None,
                        container_name: None,
                        location: Location {
                            uri: uri.clone(),
                            range: Range {
                                start: Position {
                                    line: 23,
                                    character: 18,
                                },
                                end: Position {
                                    line: 23,
                                    character: 21,
                                },
                            },
                        },
                    },
                ];
                let response = DocumentSymbolResponse::Flat(symbols);
                let response = Response::new_ok(request.id, response);
                server_state
                    .threadpool_sender
                    .send(MainLoopTask::SendResponse(Message::Response(response)))
                    .expect("send failed");
            } else {
                eprintln!("unknown file {}", path.display());
            }
        }
        Err(_) => {
            eprintln!("broken params");
        }
    }
}

fn compile_project(project: ProjectConfig, sender: Sender<MainLoopTask>) {
    use dora_frontend::sema::{Sema, SemaArgs};
    let sem_args = SemaArgs {
        arg_file: Some(project.main.to_string_lossy().into_owned()),
        packages: Vec::new(),
        test_file_as_string: None,
        check_global_initializer: false,
    };

    let mut sa = Sema::new(sem_args);

    let success = dora_frontend::check_program(&mut sa);
    assert_eq!(success, !sa.diag.lock().has_errors());
    let mut errors_by_file: HashMap<PathBuf, Vec<Diagnostic>> = HashMap::new();

    for error in sa.diag.lock().errors() {
        if let Some(file_id) = error.file {
            let span = error.span.expect("missing location");
            let source_file = sa.source_file(file_id);
            let line_starts = &source_file.line_starts;

            let (line, column) = compute_line_column(&line_starts, span.start());
            let start = Position::new(line - 1, column - 1);
            let (line, column) = compute_line_column(&line_starts, span.end());
            let end = Position::new(line - 1, column - 1);

            errors_by_file
                .entry(source_file.path.clone())
                .or_default()
                .push(Diagnostic {
                    range: Range::new(start, end),
                    message: error.msg.message(),
                    ..Default::default()
                })
        } else {
            unimplemented!()
        }
    }

    sender
        .send(MainLoopTask::ReportError(errors_by_file))
        .expect("failed send");
}

fn find_projects(workspaces: &[PathBuf]) -> Vec<ProjectConfig> {
    let mut projects = Vec::new();

    for workspace in workspaces {
        for entry in WalkDir::new(workspace) {
            let entry = entry.unwrap();
            if entry.file_name() == "dora-project.json" {
                let config = read_project_json(entry.path());

                match config {
                    Ok(config) => {
                        let path = entry.path().parent().expect("no parents");
                        let name = config.name;
                        let main_file = path.join(&config.main);
                        projects.push(ProjectConfig {
                            name,
                            main: main_file,
                        });
                    }

                    Err(_) => {
                        eprintln!("invalid project config at {}", entry.path().display());
                    }
                }
            }
        }
    }

    projects
}

fn read_project_json(path: &Path) -> Result<ProjectJsonConfig, Box<dyn Error>> {
    let mut file = File::open(path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;

    let value: serde_json::Value = serde_json::from_str(&content)?;
    let parsed_value = serde_json::from_value::<ProjectJsonConfig>(value)?;

    Ok(parsed_value)
}

enum MainLoopTask {
    SendResponse(Message),
    ReportError(HashMap<PathBuf, Vec<Diagnostic>>),
}

enum Event {
    LanguageServer(Message),
    MainLoopTask(MainLoopTask),
}

#[derive(Clone)]
struct ProjectConfig {
    name: String,
    main: PathBuf,
}

#[derive(Serialize, Deserialize)]
struct ProjectJsonConfig {
    name: String,
    main: String,
    packages: Vec<String>,
}
