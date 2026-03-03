import * as path from "path";
import * as cp from "child_process";
import * as fs from "fs";
import { ExtensionContext, window, OutputChannel } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient;
let outputChannel: OutputChannel;

/** Resolve the Python executable to use: bundled first, system fallback. */
function resolvePython(context: ExtensionContext): string {
  const candidates = [
    context.asAbsolutePath(path.join("python", "python", "python.exe")),
    context.asAbsolutePath(path.join("python", "python", "bin", "python3")),
    "python",
  ];
  for (const c of candidates) {
    if (c === "python" || fs.existsSync(c)) {
      return c;
    }
  }
  return "python";
}

export function activate(context: ExtensionContext): void {
  outputChannel = window.createOutputChannel("PyDyna LSP");

  const serverScript = context.asAbsolutePath(
    path.join("server", "server.py")
  );
  const pythonExe = resolvePython(context);

  outputChannel.appendLine(`[PyDyna] Python: ${pythonExe}`);
  outputChannel.appendLine(`[PyDyna] Server: ${serverScript}`);
  outputChannel.appendLine(`[PyDyna] Server exists: ${fs.existsSync(serverScript)}`);

  // Probe: run python -c "import pygls" and capture output before starting LSP
  const probe = cp.spawnSync(pythonExe, ["-c", "import pygls; print('pygls ok')"], { encoding: "utf8" });
  outputChannel.appendLine(`[PyDyna] pygls probe stdout: ${probe.stdout?.trim()}`);
  outputChannel.appendLine(`[PyDyna] pygls probe stderr: ${probe.stderr?.trim()}`);
  outputChannel.appendLine(`[PyDyna] pygls probe status: ${probe.status}`);

  const serverOptions: ServerOptions = {
    command: pythonExe,
    args: [serverScript],
    transport: TransportKind.stdio,
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ language: "lsdyna" }],
    synchronize: {},
  };

  client = new LanguageClient(
    "pydyna-lsp",
    "PyDyna Language Server",
    serverOptions,
    clientOptions
  );

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
