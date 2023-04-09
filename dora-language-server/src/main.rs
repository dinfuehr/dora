use std::error::Error;
use std::sync::Arc;

use dora_parser::{compute_line_column, compute_line_starts};
use lsp_server::{Connection, Message};
use lsp_types::{
    ClientCapabilities, Diagnostic, InitializeParams, Position, PublishDiagnosticsParams, Range,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
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

    loop {
        let message = connection.receiver.recv();
        match message {
            Ok(msg) => handle_message(&connection, msg),
            Err(_) => {
                eprintln!("error occurred.");
                break;
            }
        }
    }

    io_threads.join()?;
    Ok(())
}

fn handle_message(connection: &Connection, msg: Message) {
    match msg {
        Message::Notification(notification) => {
            if notification.method == "textDocument/didChange" {
                let result = serde_json::from_value::<lsp_types::DidChangeTextDocumentParams>(
                    notification.params,
                );
                match result {
                    Ok(result) => {
                        let uri = result.text_document.uri;
                        let version = result.text_document.version;
                        let content = result.content_changes[0].text.clone();
                        let mut interner = dora_parser::interner::Interner::new();
                        let line_starts = compute_line_starts(&content);
                        let parser = dora_parser::Parser::from_shared_string(
                            Arc::new(content),
                            &mut interner,
                        );
                        let (_, _, errors) = parser.parse();
                        let mut diagnostics = Vec::new();
                        for error in errors {
                            let (line, column) =
                                compute_line_column(&line_starts, error.span.start());
                            let start = Position::new(line - 1, column - 1);
                            let (line, column) =
                                compute_line_column(&line_starts, error.span.end());
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
                        let notification = lsp_server::Notification::new(
                            "textDocument/publishDiagnostics".into(),
                            params,
                        );
                        connection
                            .sender
                            .send(lsp_server::Message::Notification(notification))
                            .expect("failed send");
                    }

                    Err(_) => {
                        eprintln!("broken json");
                    }
                }
            } else {
                eprintln!("unknown notification {}", notification.method);
                eprintln!("{}", notification.params);
            }
        }

        Message::Request(request) => eprintln!("unknown request {}", request.method),
        Message::Response(response) => eprintln!("unknown response {:?}", response.result),
    }
}
