import { findExecutable, printUsage, run, rootDir, tryRun } from "./utils.ts";

if (process.argv.includes("--help") || process.argv.includes("-h")) {
  printUsage([
    "Usage: vp cli",
    "",
    "Builds and installs the local tynix CLI toolchain into the active Nix profile",
    "so `tynix` and `tynix-lsp` are available on PATH.",
  ]);
  process.exit(0);
}

process.chdir(rootDir);

console.log("Installing tynix CLI binaries into the active Nix profile...");

tryRun("nix", ["profile", "remove", "tynix"]);
tryRun("nix", ["profile", "remove", "tynix-lsp"]);
tryRun("nix", ["profile", "remove", "tynix-toolchain"]);
run("nix", ["profile", "add", "--accept-flake-config", ".#tynix-toolchain"]);

const tynixPath = findExecutable("tynix");
const tynixLspPath = findExecutable("tynix-lsp");

console.log();
console.log("Installed binaries:");

if (!tynixPath || !tynixLspPath) {
  console.error(
    "Expected `tynix` and `tynix-lsp` to be available on PATH after installation.",
  );
  process.exit(1);
}

console.log(tynixPath);
run("tynix", ["--version"]);
console.log(tynixLspPath);
run("tynix-lsp", ["--version"]);
