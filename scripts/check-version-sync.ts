import { readFile } from "node:fs/promises";

const rootPackagePath = new URL("../package.json", import.meta.url);
const vscodePackagePath = new URL(
  "../editors/vscode/package.json",
  import.meta.url,
);
const changelogPath = new URL("../CHANGELOG.md", import.meta.url);
const cabalPaths = [
  new URL("../packages/tynix-core/tynix-core.cabal", import.meta.url),
  new URL("../packages/tynix-cli/tynix-cli.cabal", import.meta.url),
  new URL("../packages/tynix-lsp/tynix-lsp.cabal", import.meta.url),
];

const errors: string[] = [];

// Parse a dotted version into its numeric components, ignoring any
// pre-release/build suffix. Returns null when no leading numeric version is
// present.
function parseVersionComponents(raw: string): number[] | null {
  const match = raw.trim().match(/^([0-9]+(?:\.[0-9]+)*)/);
  if (!match) {
    return null;
  }
  return match[1].split(".").map((part) => Number.parseInt(part, 10));
}

// Compare the first `count` components of two versions (default 3 = the semver
// major.minor.patch triple), tolerating extra trailing components such as the
// cabal four-segment form.
function sameVersionPrefix(
  left: number[],
  right: number[],
  count = 3,
): boolean {
  for (let index = 0; index < count; index += 1) {
    if ((left[index] ?? 0) !== (right[index] ?? 0)) {
      return false;
    }
  }
  return true;
}

const rootPackage = JSON.parse(await readFile(rootPackagePath, "utf8")) as {
  version: string;
};
const vscodePackage = JSON.parse(await readFile(vscodePackagePath, "utf8")) as {
  version: string;
};
const changelog = await readFile(changelogPath, "utf8");
const cabalFiles = await Promise.all(
  cabalPaths.map((path) => readFile(path, "utf8")),
);

const workspaceVersion = rootPackage.version;
const workspaceComponents = parseVersionComponents(workspaceVersion);
const changelogMatch = changelog.match(/^## v([0-9]+\.[0-9]+\.[0-9]+) - /m);

if (!workspaceComponents) {
  errors.push(
    `root package.json version "${workspaceVersion}" is not a parseable version.`,
  );
}

if (vscodePackage.version !== workspaceVersion) {
  errors.push(
    `editors/vscode/package.json version ${vscodePackage.version} does not match root package.json version ${workspaceVersion}.`,
  );
}

for (const [index, content] of cabalFiles.entries()) {
  const match = content.match(/^version:\s*(\S+)\s*$/m);
  const path = cabalPaths[index].pathname;

  if (!match) {
    errors.push(`${path} is missing a parseable version field.`);
    continue;
  }

  const cabalComponents = parseVersionComponents(match[1]);
  if (!cabalComponents) {
    errors.push(`${path} version "${match[1]}" is not a parseable version.`);
    continue;
  }

  if (
    workspaceComponents &&
    !sameVersionPrefix(cabalComponents, workspaceComponents)
  ) {
    errors.push(
      `${path} version ${match[1]} does not match the workspace version ${workspaceVersion} (first three components must agree).`,
    );
  }
}

if (!changelogMatch) {
  errors.push(
    "CHANGELOG.md is missing a top-level release heading like `## v0.2.0 - 2026-04-01`.",
  );
} else if (changelogMatch[1] !== workspaceVersion) {
  errors.push(
    `CHANGELOG.md top release ${changelogMatch[1]} does not match package.json version ${workspaceVersion}.`,
  );
}

if (errors.length > 0) {
  console.error("Version metadata is out of sync:");
  for (const error of errors) {
    console.error(`- ${error}`);
  }
  process.exit(1);
}

console.log(
  `Version metadata OK: workspace=${workspaceVersion}, vscode=${vscodePackage.version}, cabal matches major.minor.patch, changelog=${changelogMatch[1]}`,
);
