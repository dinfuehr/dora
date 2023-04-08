import * as vscode from 'vscode';

export function activate(context: vscode.ExtensionContext) {
	console.log('activate extension');

	const statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 0);
	statusBarItem.text = "dora-server";
	statusBarItem.show();

	let disposable = vscode.commands.registerCommand('dora-lang.ping', () => {
		vscode.window.showInformationMessage("Action from Dora extension.");
	});

	context.subscriptions.push(disposable);
}

export function deactivate() {
    console.log("deactivate extension");
}
