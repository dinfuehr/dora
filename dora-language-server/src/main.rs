use std::error::Error;

use lsp_server::Connection;
use lsp_types::{ClientCapabilities, InitializeParams, ServerCapabilities};

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    let (connection, _io_threads) = Connection::stdio();

    // Run the server
    let (id, params) = connection.initialize_start()?;

    let init_params: InitializeParams = serde_json::from_value(params).unwrap();
    let _client_capabilities: ClientCapabilities = init_params.capabilities;
    let server_capabilities = ServerCapabilities::default();

    let initialize_data = serde_json::json!({
        "capabilities": server_capabilities,
        "serverInfo": {
            "name": "dora-language-server",
            "version": "0.0.2"
        }
    });

    connection.initialize_finish(id, initialize_data)?;

    // TODO

    Ok(())
}
