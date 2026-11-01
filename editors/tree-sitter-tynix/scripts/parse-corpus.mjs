#!/usr/bin/env node
// Parse every tynix / Nix file shipped in the repository with the generated
// grammar and fail if any of them produces an ERROR or MISSING node.
//
// Usage: node scripts/parse-corpus.mjs [extra files...]
// Requires the `tree-sitter` CLI on PATH (or TREE_SITTER=/path/to/cli).

import { spawnSync } from "node:child_process";
import { readdirSync, statSync } from "node:fs";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const grammarDir = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const repoRoot = resolve(grammarDir, "..", "..");
const cli = process.env.TREE_SITTER ?? "tree-sitter";

const roots = [
  "examples",
  "dogfood",
  "registry",
  "packages/tynix-core/fixtures",
  "editors/tree-sitter-tynix/examples",
];
const skipDirs = new Set([
  "node_modules",
  ".git",
  "dist",
  "dist-newstyle",
  "target",
]);

function collect(dir, out) {
  let entries;
  try {
    entries = readdirSync(dir);
  } catch {
    return;
  }
  for (const name of entries) {
    if (skipDirs.has(name)) continue;
    const path = join(dir, name);
    const info = statSync(path);
    if (info.isDirectory()) collect(path, out);
    else if (name.endsWith(".tynix") || name.endsWith(".nix")) out.push(path);
  }
}

const files = [];
for (const root of roots) collect(join(repoRoot, root), files);
files.push(join(repoRoot, "flake.nix"));
files.push(...process.argv.slice(2).map((file) => resolve(file)));

let failures = 0;
for (const file of files) {
  const result = spawnSync(cli, ["parse", file], {
    cwd: grammarDir,
    encoding: "utf8",
  });
  const tree = result.stdout ?? "";
  const bad = /\((ERROR|MISSING)/.test(tree) || result.status !== 0;
  if (bad) {
    failures += 1;
    console.log(`FAIL ${relative(repoRoot, file)}`);
    const lines = tree.split("\n").filter((line) => /ERROR|MISSING/.test(line));
    for (const line of lines.slice(0, 5)) console.log(`    ${line.trim()}`);
    if (result.status !== 0 && lines.length === 0)
      console.log(result.stderr.trim().split("\n").slice(-3).join("\n"));
  }
}

console.log(
  `${files.length - failures}/${files.length} files parsed without errors`,
);
process.exit(failures === 0 ? 0 : 1);
