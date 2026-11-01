#!/usr/bin/env node
// Copy the canonical queries in editors/tree-sitter-tynix/queries into the
// Neovim runtime directory (editors/neovim/queries/tynix), which uses the same
// nvim-treesitter capture names.
//
// Usage:
//   node scripts/sync-queries.mjs          # write the copies
//   node scripts/sync-queries.mjs --check  # fail if the copies are stale
//
// Zed queries live in editors/zed/languages/tynix and use Zed's own capture
// names, so they are maintained by hand (validated by check-queries.mjs).

import { mkdirSync, readFileSync, readdirSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const grammarDir = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const source = join(grammarDir, "queries");
const target = resolve(grammarDir, "..", "neovim", "queries", "tynix");
const check = process.argv.includes("--check");

const header =
  "; Generated from editors/tree-sitter-tynix/queries by scripts/sync-queries.mjs. Do not edit.\n";

let stale = 0;
mkdirSync(target, { recursive: true });
for (const name of readdirSync(source).filter((file) =>
  file.endsWith(".scm"),
)) {
  const expected = header + readFileSync(join(source, name), "utf8");
  const destination = join(target, name);
  let current = null;
  try {
    current = readFileSync(destination, "utf8");
  } catch {
    current = null;
  }
  if (current === expected) continue;
  if (check) {
    stale += 1;
    console.log(`stale: editors/neovim/queries/tynix/${name}`);
  } else {
    writeFileSync(destination, expected);
    console.log(`wrote editors/neovim/queries/tynix/${name}`);
  }
}

if (check && stale > 0) {
  console.log(
    "run `node editors/tree-sitter-tynix/scripts/sync-queries.mjs` to refresh",
  );
  process.exit(1);
}
