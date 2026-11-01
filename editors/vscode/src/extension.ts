import { execFile, spawn } from "node:child_process";
import * as vscode from "vscode";
import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  State,
} from "vscode-languageclient/node";
import { resolveCliPath, resolveRuntimeConfig } from "./runtime.js";
import {
  ExtensionSettings,
  INSTALL_SCRIPT_COMMAND,
  NIX_INSTALL_COMMAND,
  buildInitializationOptions,
  findExecutable,
  normalizeSeverityOverrides,
  overrideSeverity,
  parseVersion,
} from "./settings.js";

const COMMANDS = {
  restart: "tynix.restartServer",
  showOutput: "tynix.showOutput",
  showVersion: "tynix.showVersion",
  runDoctor: "tynix.runDoctor",
  installServer: "tynix.installServer",
  showMenu: "tynix.showMenu",
} as const;

const SUPPRESS_INSTALL_PROMPT_KEY = "tynix.suppressInstallPrompt";

type Status = "starting" | "running" | "error" | "stopped" | "missing";

let client: LanguageClient | undefined;
let statusBarItem: vscode.StatusBarItem | undefined;
let outputChannel: vscode.LogOutputChannel | undefined;
let doctorChannel: vscode.OutputChannel | undefined;
let currentCommand = "tynix-lsp";
let currentArgs: string[] = [];
let serverVersion: string | undefined;
let settings: ExtensionSettings = {
  inlayHints: { enabled: true, typeHints: true, parameterHints: true },
  diagnostics: { enabled: true, severityOverrides: {} },
};

/**
 * Boot the tynix language client inside VS Code.
 *
 * The extension resolves `tynix-lsp` (explicit setting, Nix profiles, PATH),
 * offers to install it when it is missing, and layers a few client-side
 * conveniences on top of the server: a status bar menu, version/doctor
 * commands, inlay-hint toggles, and diagnostic severity overrides.
 */
export async function activate(
  context: vscode.ExtensionContext,
): Promise<void> {
  settings = readSettings();
  outputChannel = vscode.window.createOutputChannel("tynix", { log: true });
  doctorChannel = vscode.window.createOutputChannel("tynix doctor");
  statusBarItem = vscode.window.createStatusBarItem(
    "tynix.status",
    vscode.StatusBarAlignment.Right,
    100,
  );
  statusBarItem.name = "tynix";
  statusBarItem.command = COMMANDS.showMenu;
  context.subscriptions.push(outputChannel, doctorChannel, statusBarItem);

  context.subscriptions.push(
    vscode.commands.registerCommand(COMMANDS.restart, async () => {
      await stopClient();
      await startClient(context, { interactive: true });
    }),
    vscode.commands.registerCommand(COMMANDS.showOutput, () =>
      outputChannel?.show(true),
    ),
    vscode.commands.registerCommand(COMMANDS.showVersion, showVersion),
    vscode.commands.registerCommand(COMMANDS.runDoctor, runDoctor),
    vscode.commands.registerCommand(COMMANDS.installServer, () =>
      offerInstall(context, "install"),
    ),
    vscode.commands.registerCommand(COMMANDS.showMenu, showMenu),
    vscode.workspace.onDidChangeConfiguration(async (event) => {
      if (!event.affectsConfiguration("tynix")) return;
      settings = readSettings();
      if (
        event.affectsConfiguration("tynix.server") ||
        event.affectsConfiguration("tynix.cli")
      ) {
        await vscode.commands.executeCommand(COMMANDS.restart);
      }
    }),
  );

  await startClient(context, { interactive: false });
}

/**
 * Stop the language client when the extension is deactivated.
 *
 * Keeping shutdown explicit avoids orphaned child processes when VS Code unloads
 * the extension during reloads or window closure.
 */
export async function deactivate(): Promise<void> {
  await stopClient();
}

function readSettings(): ExtensionSettings {
  const config = vscode.workspace.getConfiguration("tynix");
  return {
    inlayHints: {
      enabled: config.get<boolean>("inlayHints.enabled", true),
      typeHints: config.get<boolean>("inlayHints.typeHints", true),
      parameterHints: config.get<boolean>("inlayHints.parameterHints", true),
    },
    diagnostics: {
      enabled: config.get<boolean>("diagnostics.enabled", true),
      severityOverrides: normalizeSeverityOverrides(
        config.get("diagnostics.severityOverrides"),
      ),
    },
  };
}

function createMiddleware(): NonNullable<LanguageClientOptions["middleware"]> {
  return {
    handleDiagnostics: (uri, diagnostics, next) => {
      if (!settings.diagnostics.enabled) return next(uri, []);
      const overrides = settings.diagnostics.severityOverrides;
      const adjusted: vscode.Diagnostic[] = [];
      for (const diagnostic of diagnostics) {
        const severity = overrideSeverity(
          diagnostic.code,
          diagnostic.message,
          diagnostic.severity,
          overrides,
        );
        if (severity === null) continue;
        diagnostic.severity = severity as vscode.DiagnosticSeverity;
        adjusted.push(diagnostic);
      }
      next(uri, adjusted);
    },
    provideInlayHints: async (document, range, token, next) => {
      const hints = settings.inlayHints;
      if (!hints.enabled) return [];
      const result = await next(document, range, token);
      if (!result) return result;
      return result.filter((hint) =>
        hint.kind === vscode.InlayHintKind.Parameter
          ? hints.parameterHints
          : hints.typeHints,
      );
    },
  };
}

async function startClient(
  context: vscode.ExtensionContext,
  options: { interactive: boolean },
): Promise<void> {
  const config = vscode.workspace.getConfiguration("tynix");
  const explicitPath = config.get<string>("server.path")?.trim() ?? "";
  const runtime = resolveRuntimeConfig(
    explicitPath,
    config.get<string[]>("server.args"),
    config.get<string>("server.cwd"),
    vscode.workspace.workspaceFolders?.map((folder) => folder.uri.fsPath),
  );
  currentCommand = runtime.command;
  currentArgs = runtime.args;
  serverVersion = undefined;

  if (explicitPath.length === 0 && !findExecutable(runtime.command)) {
    setStatus("missing");
    outputChannel?.warn(
      "tynix-lsp was not found on PATH or in the usual Nix profiles.",
    );
    const suppressed = context.globalState.get<boolean>(
      SUPPRESS_INSTALL_PROMPT_KEY,
      false,
    );
    if (
      options.interactive ||
      (!suppressed && config.get<boolean>("server.promptInstall", true))
    ) {
      void offerInstall(context, "missing");
    }
    return;
  }

  const executable: Executable = {
    command: runtime.command,
    args: runtime.args,
    options: { cwd: runtime.cwd, env: process.env },
  };
  const serverOptions: ServerOptions = {
    run: executable,
    debug: executable,
  };
  const clientOptions: LanguageClientOptions = {
    documentSelector: runtime.documentSelector,
    outputChannel,
    initializationOptions: buildInitializationOptions(settings),
    synchronize: {
      configurationSection: "tynix",
      fileEvents: vscode.workspace.createFileSystemWatcher(
        runtime.watchPattern,
      ),
    },
    middleware: createMiddleware(),
  };

  const created = new LanguageClient(
    "tynix",
    "tynix",
    serverOptions,
    clientOptions,
  );
  client = created;
  context.subscriptions.push(created);
  context.subscriptions.push(
    created.onDidChangeState((event) => updateStatus(event.newState)),
  );

  setStatus("starting");

  try {
    await created.start();
    serverVersion =
      created.initializeResult?.serverInfo?.version ??
      (await probeVersion(runtime.command));
    setStatus("running");
  } catch (error) {
    const detail = formatError(error);
    setStatus("error");
    const action = await vscode.window.showErrorMessage(
      `tynix-lsp failed to start using \`${describeCommand(runtime.command, runtime.args)}\`: ${detail}`,
      "Show Output",
      "Open Settings",
      "Restart Server",
    );
    if (action === "Show Output") {
      outputChannel?.show(true);
    } else if (action === "Open Settings") {
      await vscode.commands.executeCommand(
        "workbench.action.openSettings",
        "tynix.server",
      );
    } else if (action === "Restart Server") {
      await vscode.commands.executeCommand(COMMANDS.restart);
    }
  }
}

async function stopClient(): Promise<void> {
  if (client) {
    const stopping = client;
    client = undefined;
    try {
      await stopping.stop();
    } catch {
      // Stopping a client that already died is fine; suppress so deactivate stays clean.
    }
  }
}

async function offerInstall(
  context: vscode.ExtensionContext,
  reason: "missing" | "install",
): Promise<void> {
  const installScript = "Install (script)";
  const installNix = "Install with Nix";
  const setPath = "Set Path…";
  const never = "Don't Show Again";
  const message =
    reason === "missing"
      ? "tynix-lsp was not found. Install the tynix toolchain to enable type checking, hover and completion."
      : "Install the tynix toolchain (tynix and tynix-lsp).";
  const choices =
    reason === "missing"
      ? [installScript, installNix, setPath, never]
      : [installScript, installNix, setPath];
  const choice = await vscode.window.showInformationMessage(
    message,
    ...choices,
  );
  if (choice === installScript || choice === installNix) {
    const terminal = vscode.window.createTerminal({ name: "tynix install" });
    terminal.show();
    terminal.sendText(
      choice === installScript ? INSTALL_SCRIPT_COMMAND : NIX_INSTALL_COMMAND,
    );
    const restart = await vscode.window.showInformationMessage(
      "Restart the tynix language server once the installation finishes.",
      "Restart Server",
    );
    if (restart) await vscode.commands.executeCommand(COMMANDS.restart);
  } else if (choice === setPath) {
    await vscode.commands.executeCommand(
      "workbench.action.openSettings",
      "tynix.server.path",
    );
  } else if (choice === never) {
    await context.globalState.update(SUPPRESS_INSTALL_PROMPT_KEY, true);
  }
}

function workspaceCwd(): string | undefined {
  return vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
}

function cliCommand(): string {
  return resolveCliPath(
    vscode.workspace.getConfiguration("tynix").get<string>("cli.path"),
  );
}

function probeVersion(command: string): Promise<string | undefined> {
  return new Promise((resolve) => {
    execFile(
      command,
      ["--version"],
      { timeout: 5000, cwd: workspaceCwd() },
      (error, stdout, stderr) => {
        if (error) return resolve(undefined);
        resolve(parseVersion(`${stdout}\n${stderr}`));
      },
    );
  });
}

async function showVersion(): Promise<void> {
  const [lsp, cli] = await Promise.all([
    serverVersion
      ? Promise.resolve(serverVersion)
      : probeVersion(currentCommand),
    probeVersion(cliCommand()),
  ]);
  const extensionVersion =
    vscode.extensions.getExtension("ubugeeei.tynix")?.packageJSON.version ??
    "unknown";
  const parts = [
    `tynix-lsp ${lsp ?? "(not found)"}`,
    `tynix ${cli ?? "(not found)"}`,
    `extension ${extensionVersion}`,
  ];
  const message = parts.join(" · ");
  outputChannel?.info(message);
  if (!lsp || !cli) {
    const action = await vscode.window.showWarningMessage(
      message,
      "Install tynix",
    );
    if (action) await vscode.commands.executeCommand(COMMANDS.installServer);
  } else {
    void vscode.window.showInformationMessage(message);
  }
}

async function runDoctor(): Promise<void> {
  const channel = doctorChannel;
  if (!channel) return;
  const command = cliCommand();
  channel.clear();
  channel.show(true);
  channel.appendLine(`$ ${command} doctor`);
  await new Promise<void>((resolve) => {
    const child = spawn(command, ["doctor"], {
      cwd: workspaceCwd(),
      env: process.env,
    });
    child.stdout.on("data", (chunk: Buffer) =>
      channel.append(chunk.toString()),
    );
    child.stderr.on("data", (chunk: Buffer) =>
      channel.append(chunk.toString()),
    );
    child.on("error", async (error: NodeJS.ErrnoException) => {
      channel.appendLine(
        `\nfailed to run \`${command} doctor\`: ${error.message}`,
      );
      resolve();
      if (error.code === "ENOENT") {
        const action = await vscode.window.showErrorMessage(
          `The tynix CLI (\`${command}\`) was not found.`,
          "Install tynix",
          "Set Path…",
        );
        if (action === "Install tynix") {
          await vscode.commands.executeCommand(COMMANDS.installServer);
        } else if (action === "Set Path…") {
          await vscode.commands.executeCommand(
            "workbench.action.openSettings",
            "tynix.cli.path",
          );
        }
      }
    });
    child.on("close", (code) => {
      channel.appendLine(`\n[exit ${code ?? "?"}]`);
      if (code === 0) {
        void vscode.window.showInformationMessage(
          "tynix doctor: all checks passed.",
        );
      } else if (code !== null) {
        void vscode.window.showWarningMessage(
          "tynix doctor reported problems. See the “tynix doctor” output.",
        );
      }
      resolve();
    });
  });
}

async function showMenu(): Promise<void> {
  const items: (vscode.QuickPickItem & { command: string })[] = [
    {
      label: "$(debug-restart) Restart Language Server",
      command: COMMANDS.restart,
    },
    { label: "$(output) Show Output", command: COMMANDS.showOutput },
    { label: "$(info) Show Version", command: COMMANDS.showVersion },
    { label: "$(checklist) Run Doctor", command: COMMANDS.runDoctor },
    {
      label: "$(cloud-download) Install tynix…",
      command: COMMANDS.installServer,
    },
    {
      label: "$(gear) Open Settings",
      command: "workbench.action.openSettings",
    },
  ];
  const picked = await vscode.window.showQuickPick(items, {
    title: statusBarItem?.tooltip?.toString() ?? "tynix",
    placeHolder: "tynix",
  });
  if (!picked) return;
  if (picked.command === "workbench.action.openSettings") {
    await vscode.commands.executeCommand(picked.command, "@ext:ubugeeei.tynix");
  } else {
    await vscode.commands.executeCommand(picked.command);
  }
}

function setStatus(status: Status): void {
  if (!statusBarItem) return;
  const command = describeCommand(currentCommand, currentArgs);
  const version = serverVersion ? ` ${serverVersion}` : "";
  statusBarItem.backgroundColor = undefined;
  switch (status) {
    case "starting":
      statusBarItem.text = "$(sync~spin) tynix";
      statusBarItem.tooltip = `Starting tynix-lsp (${command})`;
      break;
    case "running":
      statusBarItem.text = `$(check) tynix${version}`;
      statusBarItem.tooltip = `tynix-lsp${version} running (${command}). Click for actions.`;
      break;
    case "error":
      statusBarItem.text = "$(error) tynix";
      statusBarItem.tooltip = `tynix-lsp failed (${command}). Click for actions.`;
      statusBarItem.backgroundColor = new vscode.ThemeColor(
        "statusBarItem.errorBackground",
      );
      break;
    case "stopped":
      statusBarItem.text = "$(circle-slash) tynix";
      statusBarItem.tooltip = "tynix-lsp stopped. Click for actions.";
      break;
    case "missing":
      statusBarItem.text = "$(warning) tynix";
      statusBarItem.tooltip =
        "tynix-lsp not found. Click to install or configure tynix.server.path.";
      statusBarItem.backgroundColor = new vscode.ThemeColor(
        "statusBarItem.warningBackground",
      );
      break;
  }
  statusBarItem.show();
}

function updateStatus(state: State): void {
  if (!client || !statusBarItem) return;
  switch (state) {
    case State.Starting:
      setStatus("starting");
      break;
    case State.Running:
      setStatus("running");
      break;
    case State.Stopped:
      setStatus("stopped");
      break;
  }
}

function describeCommand(command: string, args: readonly string[]): string {
  return [command, ...args].join(" ");
}

function formatError(error: unknown): string {
  if (error instanceof Error) return error.message;
  if (typeof error === "string") return error;
  return JSON.stringify(error);
}
