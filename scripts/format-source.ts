import { run } from "./utils.ts";

const mode = process.argv[2] ?? "--check";
if (mode !== "--check" && mode !== "--write") {
  throw new Error("Usage: format-source.ts [--check | --write]");
}

const sources = [
  "scripts",
  "docs/.vite",
  "deploy/docs",
  "vite.config.ts",
  "vite.docs.config.ts",
  "editors/tree-sitter-tynix/grammar.js",
  "editors/tree-sitter-tynix/scripts",
];

run("pnpm", ["exec", "prettier", mode, ...sources]);
