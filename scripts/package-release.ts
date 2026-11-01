import {
  chmodSync,
  copyFileSync,
  createReadStream,
  mkdirSync,
  rmSync,
} from "node:fs";
import { createHash } from "node:crypto";
import { readFile, writeFile } from "node:fs/promises";
import {
  basename,
  dirname,
  isAbsolute,
  join,
  relative,
  resolve,
  sep,
} from "node:path";
import { tmpdir } from "node:os";
import { capture, printUsage, run, rootDir } from "./utils.ts";

type ChecksumEntry = {
  expectedDigest: string;
  archiveName: string;
  archivePath: string;
  lineNumber: number;
};

async function sha256File(path: string): Promise<string> {
  const hash = createHash("sha256");

  await new Promise<void>((resolvePromise, reject) => {
    const stream = createReadStream(path);
    stream.on("data", (chunk) => hash.update(chunk));
    stream.on("end", () => resolvePromise());
    stream.on("error", reject);
  });

  return hash.digest("hex");
}

function printHelp(): void {
  printUsage([
    "Usage:",
    "  node --experimental-strip-types ./scripts/package-release.ts <version> <target> <archive> <sha> [--bundle <dir>]",
    "  node --experimental-strip-types ./scripts/package-release.ts verify-checksum <sha> [<sha> ...]",
    "  node --experimental-strip-types ./scripts/package-release.ts verify-portable <binary> [<binary> ...]",
    "",
    "Without --bundle, the binaries come from `nix build .#release-bundle` (static on",
    "Linux, /usr/lib-only on macOS). --bundle points at an already-built bundle dir.",
    "",
    "Examples:",
    "  node --experimental-strip-types ./scripts/package-release.ts v0.2.0 linux-x64 tynix-v0.2.0-linux-x64.tar.gz tynix-v0.2.0-linux-x64.sha256",
    "  node --experimental-strip-types ./scripts/package-release.ts verify-checksum tynix-v0.2.0-linux-x64.sha256",
  ]);
}

function resolveArchivePath(
  checksumPath: string,
  archiveName: string,
  lineNumber: number,
): string {
  if (isAbsolute(archiveName)) {
    throw new Error(
      `${checksumPath}:${lineNumber}: archive path must be relative`,
    );
  }

  const checksumDir = dirname(checksumPath);
  const archivePath = resolve(checksumDir, archiveName);
  const relativeArchivePath = relative(checksumDir, archivePath);

  if (
    relativeArchivePath === "" ||
    relativeArchivePath === ".." ||
    relativeArchivePath.startsWith(`..${sep}`) ||
    isAbsolute(relativeArchivePath)
  ) {
    throw new Error(
      `${checksumPath}:${lineNumber}: archive path must stay within the checksum file directory`,
    );
  }

  return archivePath;
}

function parseChecksumEntries(
  checksumPath: string,
  contents: string,
): ChecksumEntry[] {
  const entries: ChecksumEntry[] = [];
  const lines = contents.split("\n");

  for (const [index, rawLine] of lines.entries()) {
    const lineNumber = index + 1;
    const line = rawLine.endsWith("\r") ? rawLine.slice(0, -1) : rawLine;

    if (line === "" && index === lines.length - 1) {
      continue;
    }

    const match = /^([0-9a-fA-F]{64})  (.+)$/.exec(line);
    if (!match) {
      throw new Error(
        `${checksumPath}:${lineNumber}: expected "<sha256>  <archive>"`,
      );
    }

    const archiveName = match[2];
    if (archiveName.includes("\0")) {
      throw new Error(
        `${checksumPath}:${lineNumber}: archive path contains a NUL byte`,
      );
    }

    entries.push({
      expectedDigest: match[1].toLowerCase(),
      archiveName,
      archivePath: resolveArchivePath(checksumPath, archiveName, lineNumber),
      lineNumber,
    });
  }

  if (entries.length === 0) {
    throw new Error(`${checksumPath}: checksum file is empty`);
  }

  return entries;
}

async function verifyChecksumFile(checksumPath: string): Promise<void> {
  const contents = await readFile(checksumPath, "utf8");
  const entries = parseChecksumEntries(checksumPath, contents);

  for (const entry of entries) {
    const actualDigest = await sha256File(entry.archivePath);
    if (actualDigest !== entry.expectedDigest) {
      throw new Error(
        `${checksumPath}:${entry.lineNumber}: checksum mismatch for ${entry.archiveName}\n` +
          `  expected: ${entry.expectedDigest}\n` +
          `  actual:   ${actualDigest}`,
      );
    }

    console.log(`${entry.archiveName}: OK`);
  }
}

// Maps the supported release-target labels to the (platform, arch) of a host
// that can produce them. The release bundle is a native flake build
// (`.#release-bundle` for the current system), so the requested target must
// match the runner or the archive would be mislabeled.
const TARGET_HOSTS: Record<
  string,
  { platform: NodeJS.Platform; arch: string }
> = {
  "linux-x64": { platform: "linux", arch: "x64" },
  "linux-arm64": { platform: "linux", arch: "arm64" },
  "macos-arm64": { platform: "darwin", arch: "arm64" },
  "macos-x64": { platform: "darwin", arch: "x64" },
};

const RELEASE_BINARIES = ["tynix", "tynix-lsp"] as const;

function assertTargetMatchesHost(target: string): void {
  const expected = TARGET_HOSTS[target];
  if (!expected) {
    throw new Error(
      `Unknown release target "${target}". Expected one of: ${Object.keys(TARGET_HOSTS).join(", ")}.`,
    );
  }
  if (
    process.platform !== expected.platform ||
    process.arch !== expected.arch
  ) {
    throw new Error(
      `Refusing to build a "${target}" archive on ${process.platform}/${process.arch}: ` +
        `the release bundle is built natively for the host, so this would mislabel the archive. ` +
        `Run this target on a ${expected.platform}/${expected.arch} host.`,
    );
  }
}

// Fails unless `binary` can run on a machine without Nix: fully static on
// Linux, and only /usr/lib + /System dylibs on macOS.
function assertPortableBinary(binary: string): void {
  if (process.platform === "darwin") {
    const deps = capture("otool", ["-L", binary])
      .split("\n")
      .slice(1)
      .map((line) => line.trim().split(" ")[0])
      .filter((dep) => dep !== "");
    const foreign = deps.filter(
      (dep) =>
        !dep.startsWith("/usr/lib/") && !dep.startsWith("/System/Library/"),
    );
    if (foreign.length > 0) {
      throw new Error(
        `${binary} links libraries outside /usr/lib and /System:\n  ${foreign.join("\n  ")}`,
      );
    }
  } else if (process.platform === "linux") {
    const description = capture("file", ["-b", binary]);
    if (!/statically linked|static-pie linked/.test(description)) {
      throw new Error(`${binary} is not statically linked: ${description}`);
    }
  }

  console.log(`${binary}: portable`);
}

function buildReleaseBundle(): string {
  return capture("nix", [
    "build",
    "--accept-flake-config",
    "--no-link",
    "--print-out-paths",
    "-L",
    ".#release-bundle",
  ])
    .split("\n")
    .filter((line) => line.startsWith("/"))
    .at(-1)!;
}

async function packageRelease(
  version: string,
  target: string,
  archiveName: string,
  shaName: string,
  bundleDir: string | undefined,
): Promise<void> {
  assertTargetMatchesHost(target);

  const bundle = bundleDir ? resolve(bundleDir) : buildReleaseBundle();
  const stageDir = join(tmpdir(), `tynix-release-${process.pid}-${Date.now()}`);
  const releaseDir = join(
    stageDir,
    `tynix-${version.replace(/^v/, "")}-${target}`,
  );

  mkdirSync(join(releaseDir, "bin"), { recursive: true });

  try {
    for (const binary of RELEASE_BINARIES) {
      const destination = join(releaseDir, "bin", binary);
      copyFileSync(join(bundle, "bin", binary), destination);
      chmodSync(destination, 0o755);
      assertPortableBinary(destination);
    }
    copyFileSync(join(rootDir, "README.md"), join(releaseDir, "README.md"));
    copyFileSync(
      join(rootDir, "CHANGELOG.md"),
      join(releaseDir, "CHANGELOG.md"),
    );
    copyFileSync(join(rootDir, "LICENSE"), join(releaseDir, "LICENSE"));

    run("tar", ["-C", stageDir, "-czf", archiveName, basename(releaseDir)]);

    const digest = await sha256File(join(rootDir, archiveName));
    const checksumLine = `${digest}  ${archiveName}\n`;
    await writeFile(join(rootDir, shaName), checksumLine);
  } finally {
    rmSync(stageDir, { recursive: true, force: true });
  }
}

async function main(): Promise<void> {
  const args = process.argv.slice(2);
  const wantsHelp = args.includes("--help") || args.includes("-h");

  if (wantsHelp) {
    printHelp();
    return;
  }

  process.chdir(rootDir);

  if (args[0] === "verify-checksum") {
    const checksumNames = args.slice(1);
    if (checksumNames.length === 0) {
      printHelp();
      process.exit(1);
    }

    for (const checksumName of checksumNames) {
      await verifyChecksumFile(resolve(rootDir, checksumName));
    }
    return;
  }

  if (args[0] === "verify-portable") {
    const binaries = args.slice(1);
    if (binaries.length === 0) {
      printHelp();
      process.exit(1);
    }
    for (const binary of binaries) {
      assertPortableBinary(resolve(binary));
    }
    return;
  }

  let bundleDir: string | undefined;
  const positional: string[] = [];
  for (let index = 0; index < args.length; index += 1) {
    if (args[index] === "--bundle") {
      bundleDir = args[index + 1];
      index += 1;
    } else {
      positional.push(args[index]);
    }
  }

  const [version, target, archiveName, shaName] = positional;
  if (
    !version ||
    !target ||
    !archiveName ||
    !shaName ||
    positional.length !== 4
  ) {
    printHelp();
    process.exit(1);
  }

  await packageRelease(version, target, archiveName, shaName, bundleDir);
}

await main().catch((error: unknown) => {
  console.error(error instanceof Error ? error.message : String(error));
  process.exit(1);
});
