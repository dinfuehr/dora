import * as vscode from 'vscode';

export function activate(context: vscode.ExtensionContext) {
	console.log('activate extension');

	let disposable = vscode.commands.registerCommand('dora-lang.ping', () => {
		vscode.window.showInformationMessage("Action from Dora extension.");
	});

	context.subscriptions.push(disposable);
}

export function deactivate() {
    console.log("deactivate extension");
}
