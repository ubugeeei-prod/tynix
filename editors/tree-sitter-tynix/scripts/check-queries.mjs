#!/usr/bin/env node
// Validate every query file shipped for tynix (grammar, Neovim, Zed) against
// the generated grammar by running `tree-sitter query` over a sample file.
// A query referencing an unknown node, field, or anonymous token fails here.
//
// Usage: node scripts/check-queries.mjs
// Requires the `tree-sitter` CLI on PATH (or TREE_SITTER=/path/to/cli).

import { spawnSync } from "node:child_process";
import { readdirSync } from "node:fs";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const grammarDir = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const repoRoot = resolve(grammarDir, "..", "..");
const cli = process.env.TREE_SITTER ?? "tree-sitter";
const sample = join(grammarDir, "examples", "kitchen-sink.tynix");

const queryDirs = [
  join(grammarDir, "queries"),
  join(repoRoot, "editors", "neovim", "queries", "tynix"),
  join(repoRoot, "editors", "zed", "languages", "tynix"),
];

let failures = 0;
let checked = 0;
for (const dir of queryDirs) {
  let entries = [];
  try {
    entries = readdirSync(dir).filter((name) => name.endsWith(".scm"));
  } catch {
    continue;
  }
  for (const name of entries) {
    const file = join(dir, name);
    const result = spawnSync(cli, ["query", file, sample], {
      cwd: grammarDir,
      encoding: "utf8",
    });
    checked += 1;
    const stderr = (result.stderr ?? "")
      .split("\n")
      .filter(
        (line) =>
          line.trim() &&
          !/parser directories|init-config|configuration file|language grammars/.test(
            line,
          ),
      )
      .join("\n");
    if (result.status !== 0 || /error/i.test(stderr)) {
      failures += 1;
      console.log(`FAIL ${relative(repoRoot, file)}\n${stderr}`);
    }
  }
}

console.log(`${checked - failures}/${checked} query files valid`);
process.exit(failures === 0 && checked > 0 ? 0 : 1);
