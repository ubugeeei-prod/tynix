import assert from "node:assert/strict";
import {
  chmodSync,
  mkdirSync,
  mkdtempSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { delimiter, join } from "node:path";
import { test } from "node:test";
import { findExecutable } from "./utils.ts";

test("executable discovery respects PATH order and execute permission", () => {
  const directory = mkdtempSync(join(tmpdir(), "tynix-path-"));
  const previousPath = process.env.PATH;
  try {
    const first = join(directory, "first");
    const second = join(directory, "second");
    mkdirSync(first);
    mkdirSync(second);
    const name = process.platform === "win32" ? "tynix-test.cmd" : "tynix-test";
    const firstCandidate = join(first, name);
    const secondCandidate = join(second, name);
    writeFileSync(firstCandidate, "");
    writeFileSync(secondCandidate, "");
    chmodSync(firstCandidate, 0o755);
    chmodSync(secondCandidate, 0o755);
    process.env.PATH = ["", first, second].join(delimiter);

    assert.equal(findExecutable(name), firstCandidate);
    if (process.platform !== "win32") {
      chmodSync(firstCandidate, 0o644);
      assert.equal(findExecutable(name), secondCandidate);
    }
    assert.equal(findExecutable("tynix-does-not-exist"), undefined);
  } finally {
    if (previousPath === undefined) delete process.env.PATH;
    else process.env.PATH = previousPath;
    rmSync(directory, { recursive: true, force: true });
  }
});
