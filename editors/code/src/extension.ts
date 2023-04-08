import * as vscode from 'vscode';
import * as path from 'path';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
	const statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 0);
	statusBarItem.text = "dora-server";
	statusBarItem.show();

	let disposable = vscode.commands.registerCommand('dora-lang.ping', () => {
		vscode.window.showInformationMessage("Action from Dora extension.");
	});

	context.subscriptions.push(disposable);

	const workspacePath = vscode.workspace.workspaceFolders?.at(0)?.uri.fsPath;

	if (workspacePath) {
		const serverPath = path.join(workspacePath, 'target', 'debug', 'dora-language-server');
		const serverOptions: ServerOptions = { command: serverPath };

		const clientOptions: LanguageClientOptions = {
			documentSelector: [{ scheme: 'file', language: 'dora' }],
		};

		client = new LanguageClient('dora-language-server', serverOptions, clientOptions);
		client.start();
	} else {
		vscode.window.showInformationMessage("No path for dora-language-server");
	}
}

export function deactivate(): Thenable<void> | undefined {
	if (client) {
		return client.stop();
	} else {
		return undefined;
	}
}
