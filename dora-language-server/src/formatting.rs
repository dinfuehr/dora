use lsp_server::{Message, Request, Response};
use lsp_types::{Position, Range, TextEdit};

use crate::server::{MainThreadTask, ServerState, uri_to_file_path};

pub(super) fn formatting_request(server_state: &mut ServerState, request: Request) {
    let result =
        serde_json::from_value::<lsp_types::DocumentFormattingParams>(request.params.clone());
    match result {
        Ok(result) => {
            let path = uri_to_file_path(&result.text_document.uri);
            if let Some(content) = server_state.vfs.get(&path) {
                let sender = server_state.threadpool_sender.clone();

                server_state.threadpool.execute(move || {
                    let edits = format_document(&content);
                    let response = Response::new_ok(request.id, edits);
                    sender
                        .send(MainThreadTask::SendResponse(Message::Response(response)))
                        .expect("send failed");
                });
            } else {
                eprintln!("unknown file {}", path.display());
            }
        }
        Err(_) => {
            eprintln!("broken params");
        }
    }
}

fn format_document(content: &str) -> Vec<TextEdit> {
    match dora_format::format_source(content) {
        Ok(formatted) => {
            vec![TextEdit {
                range: Range::new(Position::new(0, 0), Position::new(u32::MAX, u32::MAX)),
                new_text: formatted.as_ref().clone(),
            }]
        }
        Err(_) => Vec::new(),
    }
}
