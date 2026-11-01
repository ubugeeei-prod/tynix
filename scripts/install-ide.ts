import { cpSync, existsSync, mkdirSync, mkdtempSync, rmSync } from "node:fs";
import { join } from "node:path";
import { tmpdir } from "node:os";
import { findExecutable, printUsage, run, rootDir } from "./utils.ts";

if (process.argv.includes("--help") || process.argv.includes("-h")) {
  printUsage([
    "Usage: vp ide",
    "",
    "Builds and installs the local editor integrations:",
    "- installs the local `tynix` / `tynix-lsp` toolchain into the active Nix profile",
    "- packages and installs the VS Code extension when `code` is available",
    "- builds and installs the Zed extension when Zed support files are available",
  ]);
  process.exit(0);
}

process.chdir(rootDir);

function installToolchain(): void {
  run("node", ["--experimental-strip-types", "./scripts/install-cli.ts"]);
}

function installVscode(): void {
  if (!findExecutable("code")) {
    console.log("Skipping VS Code extension install: `code` was not found.");
    return;
  }

  const stageDir = mkdtempSync(join(tmpdir(), "tynix-vscode-"));
  const vsixPath = join(stageDir, "tynix-vscode-local.vsix");

  try {
    console.log("Building VS Code extension...");
    run("pnpm", ["--filter", "tynix", "build"]);
    run("pnpm", [
      "--filter",
      "tynix",
      "exec",
      "vsce",
      "package",
      "--no-dependencies",
      "--out",
      vsixPath,
    ]);

    console.log("Installing VS Code extension...");
    run("code", ["--install-extension", vsixPath, "--force"]);
  } finally {
    rmSync(stageDir, { recursive: true, force: true });
  }
}

function buildZedWasm(): void {
  const script = [
    "set -euo pipefail",
    'tmpdir="$(mktemp -d)"',
    "trap 'rm -rf \"$tmpdir\"' EXIT",
    'export RUSTUP_HOME="$tmpdir/rustup"',
    'export CARGO_HOME="$tmpdir/cargo"',
    'export PATH="$CARGO_HOME/bin:$PATH"',
    "rustup toolchain install stable --profile minimal --target wasm32-wasip1",
    "cargo +stable build --manifest-path editors/zed/Cargo.toml --target wasm32-wasip1 --release",
  ].join("\n");

  run("nix", [
    "shell",
    "--accept-flake-config",
    "nixpkgs#rustup",
    "-c",
    "bash",
    "-lc",
    script,
  ]);
}

// Resolve Zed's per-OS support directory (where installed extensions live).
function resolveZedSupportDir(): string {
  const home = process.env.HOME ?? process.env.USERPROFILE ?? "";
  switch (process.platform) {
    case "darwin":
      return join(home, "Library", "Application Support", "Zed");
    case "win32":
      return join(
        process.env.LOCALAPPDATA ?? join(home, "AppData", "Local"),
        "Zed",
      );
    default: {
      // Linux / other XDG platforms.
      const dataHome =
        process.env.XDG_DATA_HOME ?? join(home, ".local", "share");
      return join(dataHome, "zed");
    }
  }
}

function installZed(): void {
  const zedSupportDir = resolveZedSupportDir();
  const installedDir = join(zedSupportDir, "extensions", "installed");
  const nixGrammarSource = join(installedDir, "nix", "grammars", "nix.wasm");

  if (!existsSync(zedSupportDir)) {
    console.log(
      "Skipping Zed extension install: Zed support directory was not found.",
    );
    return;
  }

  if (!existsSync(nixGrammarSource)) {
    console.log(
      "Skipping Zed extension install: install the built-in/published Nix Zed extension first so tynix can reuse its grammar.",
    );
    return;
  }

  console.log("Building Zed extension...");
  buildZedWasm();

  const destination = join(installedDir, "tynix");
  console.log(`Installing Zed extension into ${destination}...`);
  rmSync(destination, { recursive: true, force: true });
  mkdirSync(join(destination, "grammars"), { recursive: true });
  cpSync(
    join(rootDir, "editors/zed/extension.toml"),
    join(destination, "extension.toml"),
  );
  cpSync(
    join(rootDir, "editors/zed/target/wasm32-wasip1/release/tynix_zed.wasm"),
    join(destination, "extension.wasm"),
  );
  cpSync(
    join(rootDir, "editors/zed/languages"),
    join(destination, "languages"),
    { recursive: true },
  );
  cpSync(nixGrammarSource, join(destination, "grammars/nix.wasm"));
}

installToolchain();
console.log();
installVscode();
console.log();
installZed();
console.log();
console.log("IDE install complete.");
console.log(
  "Restart VS Code / Zed if they are already open so they pick up the updated extension.",
);
