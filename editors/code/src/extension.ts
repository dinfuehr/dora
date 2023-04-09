import * as vscode from 'vscode';
import * as path from 'path';
import * as os from 'os';

import * as lc from 'vscode-languageclient/node';

let client: lc.LanguageClient;
let outputChannel: vscode.OutputChannel;

export function activate(context: vscode.ExtensionContext) {
	const statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 0);
	statusBarItem.text = "dora-server";
	statusBarItem.show();

	let disposable = vscode.commands.registerCommand('dora-lang.ping', () => {
		vscode.window.showInformationMessage("Action from Dora extension.");
	});

	context.subscriptions.push(disposable);

	const homedir = os.homedir();
	const serverPath = path.join(homedir, 'code', 'dora', 'target', 'debug', 'dora-language-server');
	const serverOptions: lc.ServerOptions = { command: serverPath };

	outputChannel = vscode.window.createOutputChannel("Dora Language Server");

	const clientOptions: lc.LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'dora' }],
		outputChannel: outputChannel,
		revealOutputChannelOn: lc.RevealOutputChannelOn.Info,
	};

	client = new lc.LanguageClient('Dora Language Server', serverOptions, clientOptions, true);
	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (client) {
		return client.stop();
	} else {
		return undefined;
	}
}
