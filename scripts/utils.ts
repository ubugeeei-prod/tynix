import { accessSync, constants, existsSync } from "node:fs";
import { delimiter, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync, type SpawnSyncOptions } from "node:child_process";

export const rootDir = resolve(dirname(fileURLToPath(import.meta.url)), "..");

export function run(
  command: string,
  args: string[],
  options: SpawnSyncOptions = {},
): void {
  const result = spawnSync(command, args, {
    stdio: "inherit",
    cwd: rootDir,
    ...options,
  });

  if (result.error) {
    throw result.error;
  }

  if (result.status !== 0) {
    process.exit(result.status ?? 1);
  }
}

export function capture(
  command: string,
  args: string[],
  options: SpawnSyncOptions = {},
): string {
  const result = spawnSync(command, args, {
    stdio: ["inherit", "pipe", "inherit"],
    encoding: "utf8",
    cwd: rootDir,
    ...options,
  });

  if (result.error) {
    throw result.error;
  }

  if (result.status !== 0) {
    process.exit(result.status ?? 1);
  }

  return result.stdout.trim();
}

export function tryRun(
  command: string,
  args: string[],
  options: SpawnSyncOptions = {},
): boolean {
  const result = spawnSync(command, args, {
    stdio: "ignore",
    cwd: rootDir,
    ...options,
  });

  return result.status === 0;
}

function executableNames(name: string, isWindows: boolean): string[] {
  if (!isWindows || /\.[^./\\]+$/.test(name)) {
    return [name];
  }
  const extensions = (process.env.PATHEXT ?? ".COM;.EXE;.BAT;.CMD")
    .split(";")
    .map((extension) => extension.trim())
    .filter((extension) => extension.length > 0);
  return [name, ...extensions.map((extension) => name + extension)];
}

function isExecutable(path: string, isWindows: boolean): boolean {
  if (!existsSync(path)) return false;
  // Windows resolves executables by PATHEXT; its execute bit is not meaningful.
  if (isWindows) return true;
  try {
    accessSync(path, constants.X_OK);
    return true;
  } catch {
    return false;
  }
}

export function findExecutable(name: string): string | undefined {
  const isWindows = process.platform === "win32";
  const names = executableNames(name, isWindows);
  const directories = (process.env.PATH ?? "").split(delimiter).filter(Boolean);
  for (const directory of directories) {
    for (const candidateName of names) {
      const candidate = join(directory, candidateName);
      if (isExecutable(candidate, isWindows)) return candidate;
    }
  }
  return undefined;
}

export function printUsage(lines: string[]): void {
  console.log(lines.join("\n"));
}
