---
title: "1. Install"
description: Install the tynix CLI and the tynix-lsp language server with the installer script or Nix.
---

# 1. Install

tynix ships as two executables:

- `tynix`, the CLI that checks, compiles and emits declarations, and
- `tynix-lsp`, the language server your editor talks to.

Pick one of the installation methods below.

## Option A: the installer script

On Linux (x64, arm64) and macOS (arm64, x64), the quickest route is the
installer script. It downloads the prebuilt release archive for your platform,
verifies its SHA-256 checksum, and installs `tynix` and `tynix-lsp` into
`~/.tynix/bin`:

```bash
curl -fsSL https://tynix.dev/install.sh | sh
```

> [!NOTE]
> Prefer to read a script before piping it to a shell? Download it first with
> `curl -fsSL https://tynix.dev/install.sh -o install.sh`, inspect it, then run
> `sh install.sh`. The [installation section of Getting Started](../getting-started.md#installation)
> documents the options the script accepts.

## Option B: Nix profile

On any host with flakes enabled, install straight from the flake:

```bash
nix profile install github:ubugeeei-prod/tynix
```

The default package is `tynix-toolchain`, which contains both the `tynix` CLI
and the `tynix-lsp` language server (you need the server in step 10). The
binaries are also available on their own:

```bash
nix profile install github:ubugeeei-prod/tynix#tynix
nix profile install github:ubugeeei-prod/tynix#tynix-lsp
```

To pin a release, add a tag to the flake reference, for example
`github:ubugeeei-prod/tynix/v0.5.0`.

## Option C: run without installing

`nix run` is handy for a one-off check or for CI:

```bash
nix run github:ubugeeei-prod/tynix -- --version
```

## Check the installation

```bash
tynix --version
```

```text
tynix 0.5.0.0
```

`tynix --help` lists every command. You will meet most of them in this tutorial;
the [CLI reference](../reference/cli.md) documents all of them.

> [!TIP]
> Platform support differs per tier. Prebuilt archives exist for Linux x64 and
> arm64 and for macOS arm64 and x64; other platforms build through the flake.
> See the [support matrix](../support-matrix.md) for details.

## Set up a playground

Create a directory for the tutorial and make it a Git repository:

```bash
mkdir tynix-tour && cd tynix-tour
git init
```

The `git init` matters. tynix treats the nearest directory that contains `.git`,
`flake.nix`, `tynix.config.tynix`, `cabal.project` or `pnpm-workspace.yaml` as the
*workspace root*, and it discovers `.d.tynix` declaration files anywhere under
that root. You will rely on that in step 6.

<div class="tx-pager">

[← Tutorial overview](./index.md) [2. Your first file →](./first-file.md)

</div>
