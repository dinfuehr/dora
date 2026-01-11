use std::str::FromStr;
use std::sync::Arc;
use std::time::{Duration, Instant};

use crossbeam::channel::{Receiver, Sender};
use crossbeam::select;
use dora_parser::compute_line_column;
use lsp_server::{Connection, IoThreads, Message, Notification};
use lsp_types::notification::Notification as _;
use lsp_types::{
    ClientCapabilities, Diagnostic, DiagnosticSeverity, InitializeParams, OneOf, Position,
    PublishDiagnosticsParams, Range, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, Uri,
};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use threadpool::ThreadPool;
use url::Url;
use walkdir::WalkDir;

use crate::document_symbols::document_symbol_request;
use crate::workspace_symbols::workspace_symbol_request;
use dora_frontend::Vfs;

pub(crate) fn run_server(
    conn: Connection,
    io_threads: IoThreads,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    eprintln!("Initiate language server.");

    // Run the server
    eprintln!("Wait for client initiating server.");
    let (id, params) = conn.initialize_start()?;

    let init_params: InitializeParams = serde_json::from_value(params).unwrap();
    let client_capabilities: ClientCapabilities = init_params.capabilities;
    let client_workspace_folders = init_params.workspace_folders.unwrap_or_default();
    let mut workspace_folders = Vec::new();

    for workspace_folder in client_workspace_folders {
        let file_path = uri_to_file_path(&workspace_folder.uri);
        assert!(file_path.is_absolute());
        eprintln!("workspace folder: {}", file_path.display());
        workspace_folders.push(file_path);
    }

    let server_capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        document_symbol_provider: Some(OneOf::Left(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
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

    if projects.is_empty() {
        eprintln!("no project found");
    } else {
        eprintln!("found {} projects.", projects.len());

        for project in &projects {
            eprintln!("project {} -> {}", project.name, project.main.display());
        }
    }

    eprintln!("Send initialization response.");
    conn.initialize_finish(id, initialize_data)?;
    let mut state = ServerState::new(client_capabilities, workspace_folders, projects);
    event_loop(&mut state, &conn)?;
    io_threads.join()?;
    Ok(())
}

const DEBOUNCE_DURATION_MS: u64 = 500;

pub struct ServerState {
    pub vfs: Vfs,
    #[allow(dead_code)]
    pub client_capabilities: ClientCapabilities,
    #[allow(dead_code)]
    pub workspace_folders: Vec<PathBuf>,
    pub projects: Arc<Vec<ProjectConfig>>,
    pub files_with_errors: Vec<HashSet<PathBuf>>,
    pub threadpool: ThreadPool,
    pub threadpool_sender: Sender<MainThreadTask>,
    pub threadpool_receiver: Receiver<MainThreadTask>,
    pub pending_compilation: Option<(Instant, usize)>,
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
            vfs: Vfs::new(),
            client_capabilities,
            workspace_folders,
            files_with_errors: (0..projects.len())
                .map(|_| HashSet::new())
                .collect::<Vec<_>>(),
            projects: Arc::new(projects),
            threadpool,
            threadpool_sender: sender,
            threadpool_receiver: receiver,
            pending_compilation: None,
        }
    }

    fn open_file(&mut self, path: PathBuf, content: String) {
        let vfs = std::mem::replace(&mut self.vfs, Vfs::new());
        self.vfs = vfs.open_file(path, content);
    }

    fn update_file(&mut self, path: PathBuf, content: String) {
        let vfs = std::mem::replace(&mut self.vfs, Vfs::new());
        self.vfs = vfs.update_file(path, content);
    }

    fn close_file(&mut self, path: PathBuf) {
        let vfs = std::mem::replace(&mut self.vfs, Vfs::new());
        self.vfs = vfs.close_file(path);
    }

    #[allow(unused)]
    pub fn find_project_for_file(&self, file_path: &Path) -> Option<(usize, PathBuf)> {
        assert!(file_path.is_file());
        let mut current_dir = file_path.parent()?;

        loop {
            let project_json_path = current_dir.join("dora-project.toml");

            if project_json_path.exists() {
                for (idx, project) in self.projects.iter().enumerate() {
                    if project.project_file == project_json_path {
                        let project_dir = project.project_file.parent()?;
                        let relative_path = file_path.strip_prefix(project_dir).ok()?.to_path_buf();
                        return Some((idx, relative_path));
                    }
                }

                return None;
            }

            current_dir = current_dir.parent()?;
        }
    }
}

fn event_loop(
    server_state: &mut ServerState,
    connection: &Connection,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    loop {
        match next_event(server_state, connection) {
            Some(Event::LanguageServer(msg)) => handle_message(server_state, msg),
            Some(Event::MainLoopTask(task)) => {
                handle_main_loop_task(server_state, connection, task)
            }
            Some(Event::DebounceExpired) => {
                handle_debounce_expired(server_state);
            }
            Some(Event::Heartbeat) => {}
            None => {
                return Ok(());
            }
        }
    }
}

fn next_event(server_state: &mut ServerState, connection: &Connection) -> Option<Event> {
    let mut is_timeout_active = false;
    let mut select_timeout = Duration::from_secs(3600);

    if let Some((last_change, _project_id)) = server_state.pending_compilation {
        let elapsed = last_change.elapsed();
        let debounce_duration = Duration::from_millis(DEBOUNCE_DURATION_MS);

        if elapsed >= debounce_duration {
            return Some(Event::DebounceExpired);
        } else {
            is_timeout_active = true;
            select_timeout = debounce_duration - elapsed;
        }
    }

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

        default(select_timeout) => {
            if is_timeout_active {
                Some(Event::DebounceExpired)
            } else {
                Some(Event::Heartbeat)
            }
        }
    }
}

fn handle_main_loop_task(
    server_state: &mut ServerState,
    connection: &Connection,
    task: MainThreadTask,
) {
    match task {
        MainThreadTask::SendResponse(msg) => connection.sender.send(msg).expect("send failed"),
        MainThreadTask::ReportError(project_id, errors_by_file) => {
            let mut last_files_with_errors = std::mem::replace(
                &mut server_state.files_with_errors[project_id],
                HashSet::new(),
            );

            for (file, errors) in errors_by_file {
                let params = PublishDiagnosticsParams {
                    uri: file_path_to_uri(&file),
                    version: None,
                    diagnostics: errors,
                };
                let notification =
                    lsp_server::Notification::new("textDocument/publishDiagnostics".into(), params);
                let msg = Message::Notification(notification);
                connection.sender.send(msg).expect("send() failed");

                last_files_with_errors.remove(&file);
                server_state.files_with_errors[project_id].insert(file);
            }

            for file in last_files_with_errors {
                let params = PublishDiagnosticsParams {
                    uri: file_path_to_uri(&file),
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
            eprintln!("received notification {}", notification.method);
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
            eprintln!("received request {}", request.method);
            if request.method == "textDocument/documentSymbol" {
                document_symbol_request(server_state, request);
            } else if request.method == "workspace/symbol" {
                workspace_symbol_request(server_state, request);
            } else {
                eprintln!("unknown request {}", request.method);
                eprintln!("{}", request.params);
            }
        }

        Message::Response(response) => eprintln!("unknown response {:?}", response.result),
    }
}

fn did_change_notification(server_state: &mut ServerState, notification: Notification) {
    let result =
        serde_json::from_value::<lsp_types::DidChangeTextDocumentParams>(notification.params);
    match result {
        Ok(result) => {
            let path = uri_to_file_path(&result.text_document.uri);
            let content = result.content_changes[0].text.clone();
            eprintln!(
                "CHANGE: {} --> {} lines",
                path.display(),
                content.lines().count(),
            );

            server_state.update_file(path.clone(), content);

            if let Some((project_id, _relative_path)) = server_state.find_project_for_file(&path) {
                server_state.pending_compilation = Some((Instant::now(), project_id));
            } else {
                eprintln!("Project not found for file {}", path.display());
            }
        }

        Err(_) => {
            eprintln!("broken json");
        }
    }
}

fn did_open_notification(server_state: &mut ServerState, notification: Notification) {
    let result =
        serde_json::from_value::<lsp_types::DidOpenTextDocumentParams>(notification.params);
    match result {
        Ok(result) => {
            let path = uri_to_file_path(&result.text_document.uri);
            let text = result.text_document.text;

            server_state.open_file(path.clone(), text);

            if let Some((project_id, _relative_path)) = server_state.find_project_for_file(&path) {
                let sender = server_state.threadpool_sender.clone();
                let projects = server_state.projects.clone();
                let vfs = server_state.vfs.clone();

                server_state.threadpool.execute(move || {
                    let project = &projects[project_id];
                    compile_project(project_id, &project, vfs, sender);
                });
            } else {
                eprintln!("Project not found for file {}", path.display());
            }
        }
        Err(_) => {}
    }
}

fn did_close_notification(server_state: &mut ServerState, notification: Notification) {
    let result =
        serde_json::from_value::<lsp_types::DidCloseTextDocumentParams>(notification.params);
    match result {
        Ok(result) => {
            let path = uri_to_file_path(&result.text_document.uri);
            server_state.close_file(path);
        }
        Err(_) => {}
    }
}

fn did_save_notification(server_state: &mut ServerState, notification: Notification) {
    let result =
        serde_json::from_value::<lsp_types::DidSaveTextDocumentParams>(notification.params);
    match result {
        Ok(result) => {
            let path = uri_to_file_path(&result.text_document.uri);

            if let Some((project_id, _relative_path)) = server_state.find_project_for_file(&path) {
                let sender = server_state.threadpool_sender.clone();
                let projects = server_state.projects.clone();
                let vfs = server_state.vfs.clone();

                server_state.threadpool.execute(move || {
                    let project = &projects[project_id];
                    compile_project(project_id, &project, vfs, sender);
                });
            } else {
                eprintln!("Project not found for file {}", path.display());
            }
        }
        Err(_) => {}
    }
}

fn handle_debounce_expired(server_state: &mut ServerState) {
    if let Some((_last_change, project_id)) = server_state.pending_compilation {
        eprintln!(
            "Debounce expired, triggering compilation for project {}",
            server_state.projects[project_id].name
        );
        server_state.pending_compilation = None;

        let sender = server_state.threadpool_sender.clone();
        let projects = server_state.projects.clone();
        let vfs = server_state.vfs.clone();

        server_state.threadpool.execute(move || {
            let project = &projects[project_id];
            compile_project(project_id, project, vfs.clone(), sender.clone());
        });
    }
}

#[allow(unused)]
fn compile_all_projects(server_state: &mut ServerState) {
    let sender = server_state.threadpool_sender.clone();
    let projects = server_state.projects.clone();
    let vfs = server_state.vfs.clone();

    server_state.threadpool.execute(move || {
        for (project_id, project) in projects.iter().enumerate() {
            compile_project(project_id, project, vfs.clone(), sender.clone());
        }
    });
}

fn compile_project(
    project_id: usize,
    project: &ProjectConfig,
    vfs: Vfs,
    sender: Sender<MainThreadTask>,
) {
    eprintln!("compile project {}", project.name);
    let errors_by_file = compile_project_main(project, vfs);

    sender
        .send(MainThreadTask::ReportError(project_id, errors_by_file))
        .expect("failed send");
}

fn compile_project_main(project: &ProjectConfig, vfs: Vfs) -> HashMap<PathBuf, Vec<Diagnostic>> {
    use dora_frontend::sema::{Sema, SemaCreationParams};

    let sema_params = SemaCreationParams::new()
        .set_program_path(project.main.clone())
        .set_vfs(vfs)
        .set_standard_library(project.is_standard_library);
    let mut sa = Sema::new(sema_params);

    let success = dora_frontend::check_program(&mut sa);
    assert_eq!(success, !sa.diag.borrow().has_errors());

    let mut errors_by_file: HashMap<PathBuf, Vec<Diagnostic>> = HashMap::new();

    for error in sa.diag.borrow().errors() {
        if let Some(file_id) = error.file_id {
            let span = error.span.expect("missing location");
            let source_file = sa.file(file_id);
            let line_starts = &source_file.line_starts;

            let (start_line, start_column) = compute_line_column(&line_starts, span.start());
            let start = Position::new(start_line - 1, start_column - 1);
            let (line, column) = compute_line_column(&line_starts, span.end());
            let end = Position::new(line - 1, column - 1);

            eprintln!(
                "error at {}:{}: {}",
                start_line,
                start_column,
                error.message(&sa)
            );

            errors_by_file
                .entry(source_file.path.clone())
                .or_default()
                .push(Diagnostic {
                    range: Range::new(start, end),
                    message: error.message(&sa),
                    ..Default::default()
                })
        } else {
            unimplemented!()
        }
    }

    for warning in sa.diag.borrow().warnings() {
        let file_id = warning.file_id.expect("missing file");
        let span = warning.span.expect("missing location");
        let source_file = sa.file(file_id);
        let line_starts = &source_file.line_starts;

        let (start_line, start_column) = compute_line_column(&line_starts, span.start());
        let start = Position::new(start_line - 1, start_column - 1);
        let (line, column) = compute_line_column(&line_starts, span.end());
        let end = Position::new(line - 1, column - 1);

        eprintln!(
            "warning at {}:{}: {}",
            start_line,
            start_column,
            warning.message(&sa)
        );

        errors_by_file
            .entry(source_file.path.clone())
            .or_default()
            .push(Diagnostic {
                range: Range::new(start, end),
                severity: Some(DiagnosticSeverity::WARNING),
                message: warning.message(&sa),
                ..Default::default()
            })
    }

    eprintln!(
        "compile project {}: done ({} errors, {} warnings)",
        project.name,
        sa.diag.borrow().errors().len(),
        sa.diag.borrow().warnings().len()
    );

    errors_by_file
}

fn find_projects(workspaces: &[PathBuf]) -> Vec<ProjectConfig> {
    let mut projects = Vec::new();

    for workspace in workspaces {
        for entry in WalkDir::new(workspace) {
            let entry = entry.unwrap();
            if entry.file_name() == "dora-project.toml" {
                let config = read_project_toml(entry.path());

                match config {
                    Ok(config) => {
                        let path = entry.path().parent().expect("no parents");
                        let name = config.project.name;
                        let main_file = path.join(&config.project.main);
                        projects.push(ProjectConfig {
                            name,
                            main: main_file,
                            project_file: entry.path().to_path_buf(),
                            is_standard_library: config
                                .project
                                ._is_standard_library
                                .unwrap_or(false),
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

fn read_project_toml(path: &Path) -> Result<ProjectTomlConfig, Box<dyn Error>> {
    let mut file = File::open(path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;

    let parsed_value = toml::from_str::<ProjectTomlConfig>(&content)?;

    Ok(parsed_value)
}

pub(crate) fn uri_to_file_path(uri: &Uri) -> PathBuf {
    let url = Url::parse(uri.as_str()).expect("uri expected");
    url.to_file_path().expect("file path expected")
}

pub(crate) fn file_path_to_uri(path: &Path) -> Uri {
    let url = Url::from_file_path(path).expect("uri expected");
    Uri::from_str(url.as_str()).expect("uri expected")
}

pub enum MainThreadTask {
    SendResponse(Message),
    ReportError(usize, HashMap<PathBuf, Vec<Diagnostic>>),
}

enum Event {
    LanguageServer(Message),
    MainLoopTask(MainThreadTask),
    DebounceExpired,
    Heartbeat,
}

#[derive(Clone)]
pub struct ProjectConfig {
    pub name: String,
    pub main: PathBuf,
    pub project_file: PathBuf,
    pub is_standard_library: bool,
}

#[derive(Serialize, Deserialize)]
struct ProjectTomlConfig {
    project: ProjectTomlProject,
}

#[derive(Serialize, Deserialize)]
struct ProjectTomlProject {
    name: String,
    main: String,
    _is_standard_library: Option<bool>,
    packages: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn create_test_project(dir: &Path, project_name: &str, main_file: &str) -> PathBuf {
        let project_toml = format!(
            r#"[project]
name = "{}"
main = "{}"
packages = []
"#,
            project_name, main_file
        );

        let project_toml_path = dir.join("dora-project.toml");
        fs::write(&project_toml_path, project_toml).unwrap();

        let main_path = dir.join(main_file);
        if let Some(parent) = main_path.parent() {
            fs::create_dir_all(parent).ok();
        }
        fs::write(&main_path, "fn main() {}").unwrap();

        main_path
    }

    fn create_test_state(projects: Vec<ProjectConfig>) -> ServerState {
        ServerState::new(ClientCapabilities::default(), Vec::new(), projects)
    }

    #[test]
    fn test_find_project_for_file_in_project_root() {
        let temp_dir = TempDir::new().unwrap();
        let project_dir = temp_dir.path().join("project1");
        fs::create_dir(&project_dir).unwrap();

        let main_path = create_test_project(&project_dir, "test-project", "main.dora");

        let projects = vec![ProjectConfig {
            name: "test-project".to_string(),
            main: main_path.clone(),
            project_file: project_dir.join("dora-project.toml"),
            is_standard_library: false,
        }];

        let state = create_test_state(projects);

        // Test with a file in the same directory as main.dora
        let test_file = project_dir.join("other.dora");
        fs::write(&test_file, "fn test() {}").unwrap();

        let result = state.find_project_for_file(&test_file);
        assert_eq!(result, Some((0, PathBuf::from("other.dora"))));
    }

    #[test]
    fn test_find_project_for_file_in_subdirectory() {
        let temp_dir = TempDir::new().unwrap();
        let project_dir = temp_dir.path().join("project1");
        fs::create_dir(&project_dir).unwrap();

        let main_path = create_test_project(&project_dir, "test-project", "main.dora");

        let projects = vec![ProjectConfig {
            name: "test-project".to_string(),
            main: main_path.clone(),
            project_file: project_dir.join("dora-project.toml"),
            is_standard_library: false,
        }];

        let state = create_test_state(projects);

        // Test with a file in a subdirectory
        let subdir = project_dir.join("src");
        fs::create_dir(&subdir).unwrap();
        let test_file = subdir.join("lib.dora");
        fs::write(&test_file, "fn lib() {}").unwrap();

        let result = state.find_project_for_file(&test_file);
        assert_eq!(result, Some((0, PathBuf::from("src/lib.dora"))));
    }

    #[test]
    fn test_find_project_for_file_multiple_projects() {
        let temp_dir = TempDir::new().unwrap();

        // Create first project
        let project1_dir = temp_dir.path().join("project1");
        fs::create_dir(&project1_dir).unwrap();
        let main1_path = create_test_project(&project1_dir, "project1", "main.dora");

        // Create second project
        let project2_dir = temp_dir.path().join("project2");
        fs::create_dir(&project2_dir).unwrap();
        let main2_path = create_test_project(&project2_dir, "project2", "main.dora");

        let projects = vec![
            ProjectConfig {
                name: "project1".to_string(),
                main: main1_path.clone(),
                project_file: project1_dir.join("dora-project.toml"),
                is_standard_library: false,
            },
            ProjectConfig {
                name: "project2".to_string(),
                main: main2_path.clone(),
                project_file: project2_dir.join("dora-project.toml"),
                is_standard_library: false,
            },
        ];

        let state = create_test_state(projects);

        // Test file in project1
        let file1 = project1_dir.join("test1.dora");
        fs::write(&file1, "fn test1() {}").unwrap();
        assert_eq!(
            state.find_project_for_file(&file1),
            Some((0, PathBuf::from("test1.dora")))
        );

        // Test file in project2
        let file2 = project2_dir.join("test2.dora");
        fs::write(&file2, "fn test2() {}").unwrap();
        assert_eq!(
            state.find_project_for_file(&file2),
            Some((1, PathBuf::from("test2.dora")))
        );
    }

    #[test]
    fn test_find_project_for_file_not_found() {
        let temp_dir = TempDir::new().unwrap();
        let project_dir = temp_dir.path().join("project1");
        fs::create_dir(&project_dir).unwrap();

        let main_path = create_test_project(&project_dir, "test-project", "main.dora");

        let projects = vec![ProjectConfig {
            name: "test-project".to_string(),
            main: main_path.clone(),
            project_file: project_dir.join("dora-project.toml"),
            is_standard_library: false,
        }];

        let state = create_test_state(projects);

        // Test with a file outside the project
        let outside_file = temp_dir.path().join("outside.dora");
        fs::write(&outside_file, "fn outside() {}").unwrap();

        let result = state.find_project_for_file(&outside_file);
        assert_eq!(result, None);
    }

    #[test]
    fn test_find_project_for_file_nested_subdirectory() {
        let temp_dir = TempDir::new().unwrap();
        let project_dir = temp_dir.path().join("project1");
        fs::create_dir(&project_dir).unwrap();

        let main_path = create_test_project(&project_dir, "test-project", "src/main.dora");

        let projects = vec![ProjectConfig {
            name: "test-project".to_string(),
            main: main_path.clone(),
            project_file: project_dir.join("dora-project.toml"),
            is_standard_library: false,
        }];

        let state = create_test_state(projects);

        // Test with a file in a deeply nested subdirectory
        let deep_dir = project_dir.join("src").join("lib").join("utils");
        fs::create_dir_all(&deep_dir).unwrap();
        let test_file = deep_dir.join("helper.dora");
        fs::write(&test_file, "fn helper() {}").unwrap();

        let result = state.find_project_for_file(&test_file);
        assert_eq!(
            result,
            Some((0, PathBuf::from("src/lib/utils/helper.dora")))
        );
    }
}
