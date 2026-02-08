import * as vscode from 'vscode';
import * as path from 'path';
import * as os from 'os';

import * as lc from 'vscode-languageclient/node';

let client: lc.LanguageClient;
let outputChannel: vscode.OutputChannel;

export function activate(context: vscode.ExtensionContext) {
	const statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 0);
	statusBarItem.text = "(dora-server)";
	statusBarItem.show();

	let disposable = vscode.commands.registerCommand('dora-lang.ping', () => {
		vscode.window.showInformationMessage("Action from Dora extension.");
	});

	context.subscriptions.push(disposable);

	const config = vscode.workspace.getConfiguration("dora.languageServer");
	const serverPath: string | undefined = config.get("path");
	const serverArgs: string = config.get("args", "");

	if (serverPath) {
		createClient(serverPath, serverArgs, statusBarItem);
	} else {
		vscode.window.showInformationMessage("Configure path to server in settings.");
	}
}

function createClient(serverPath: string, serverArgs: string, statusBarItem: vscode.StatusBarItem) {
	const args = serverArgs.trim().split(/\s+/).filter(arg => arg.length > 0);
	const serverOptions: lc.ServerOptions = {
		command: serverPath,
		args: args
	};

	outputChannel = vscode.window.createOutputChannel("Dora Language Server");

	const formattingEnabled: boolean = vscode.workspace.getConfiguration("dora.formatting").get("enabled", false);

	const clientOptions: lc.LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'dora' }],
		outputChannel: outputChannel,
		revealOutputChannelOn: lc.RevealOutputChannelOn.Info,
		initializationOptions: {
			formatting: formattingEnabled,
		},
	};

	client = new lc.LanguageClient('Dora Language Server', serverOptions, clientOptions, true);
	client.start();

	client.onDidChangeState((state) => {
		if (state.newState === lc.State.Running) {
			statusBarItem.text = "dora-server";
		} else if (state.newState === lc.State.Starting) {
			statusBarItem.text = "dora-server (starting)";
		} else if (state.newState === lc.State.Stopped) {
			statusBarItem.text = "dora-server (stopped)";
		}
	});
}

export function deactivate(): Thenable<void> | undefined {
	if (client) {
		return client.stop();
	} else {
		return undefined;
	}
}
