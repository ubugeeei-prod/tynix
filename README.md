<p align="center">
  <a href="https://tynix.dev">
    <picture>
      <source media="(prefers-color-scheme: dark)" srcset="docs/public/brand/tynix-logo-dark.svg">
      <img src="docs/public/brand/tynix-logo.svg" alt="tynix" width="388" height="128">
    </picture>
  </a>
</p>

<p align="center"><strong>Gradual types for Nix.</strong><br>
Add types where they help, keep the rest dynamic, and ship plain <code>.nix</code> with zero runtime.</p>

<p align="center">
  <a href="https://tynix.dev">Docs</a> ·
  <a href="https://tynix.dev/tutorial">Tutorial</a> ·
  <a href="https://github.com/ubugeeei-prod/tynix/releases">Releases</a>
</p>

# tynix

`tynix` is a gradual type system and tooling stack for Nix. It compiles `.tynix` to `.nix`, provides static checking, and emits `.d.tynix` declaration files. It has no runtime and is intentionally limited to complementing existing Nix semantics rather than replacing them.

## Goals

- Add types to `.nix` without breaking its syntax or culture
- Allow ambient declarations for existing `.nix` files
- Adopt the parts of the TypeScript strategy that worked
  - incremental adoption
  - semantic preservation
  - strong editor tooling
- Be expressive enough for type-level programming
  - Hindley-Milner inference with let- and `rec`-polymorphism and higher-rank types
  - higher-kinded types with kind annotations
  - conditional types and `infer`-style type decomposition
  - effects (`! { Trace, Throw }`), effect polymorphism, and pure flakes
  - linear arrows (`%1 ->`) and capture sets (`->{fetch}`)
  - dependent arrows (`(n :: Nat) -> Vec n a`) and opaque / phantom types
  - hygienic, declarative, typed macros
- Use structural subtyping and `dynamic`, `unknown`, and `any` as the core gradual typing tools

## File Kinds

- `.tynix`
  - source files with type annotations
  - compiled to `.nix`
- `.d.tynix`
  - declaration-only ambient files
  - used to type existing `.nix` modules and external code
- `.nix`
  - runtime artifact
  - `tynix` does not change its behavior

## Example

```tynix
type Option a = { _tag :: "some"; value :: a; } | { _tag :: "none"; };

declare "./legacy/default.nix" {
  mkPkg :: { name :: String; version :: String; } -> Derivation;
};

let
  some :: forall a. a -> Option a;
  some = value: { _tag = "some"; inherit value; };

  labelOf = pkg: { label = pkg.name; };

  legacy = import ./legacy/default.nix;
in
{ name ? "hello", version, ... }@args:
{
  package = legacy.mkPkg { inherit name version; };
  tag = some (labelOf args).label;
}
```

`tynix check` infers `labelOf :: forall t0. { name :: t0; ... } %1 -> { label :: t0; }`
(a row-polymorphic record argument), types the file's argument as
`{ name? :: String; version :: String; ... }`, and checks the call to the
untyped `legacy/default.nix` against its declaration. `Derivation` comes from
the built-in prelude that types every Nix builtin. After compilation, type
information is erased and only ordinary Nix code remains.

## Design Docs

- [Getting Started](./docs/getting-started.md)
- [Tutorial](./docs/tutorial/index.md)
- [Editor Setup](./docs/editors.md)
- [Adopting tynix (Migration)](./docs/migration.md)
- [Troubleshooting](./docs/troubleshooting.md)
- [Language Reference](./docs/language-reference.md)
- [Grammar](./docs/grammar.md)
- [Language Design](./docs/language-design.md)
- [Type System](./docs/type-system.md)
- [Architecture](./docs/architecture.md)
- [Roadmap](./docs/roadmap.md)
- [Contributing](./CONTRIBUTING.md)
- [Governance](./GOVERNANCE.md)
- [Security Policy](./SECURITY.md)
- [Support Matrix](./docs/support-matrix.md)
- [CI/CD Hardening](./docs/ci-cd.md)
- [Diagnostic Codes](./docs/diagnostics.md)

## Current Status

`tynix` is now in its first integrated toolchain release.

- Haskell monorepo for parser, checker, compiler, emitter, CLI, and LSP
- Nix-based development environment
- `pnpm`-managed editor tooling
- VS Code, Cursor, VSCodium, Zed, Neovim, and Helix integrations, installed with
  `tynix ide install <editor>` and diagnosed with `tynix doctor`
- full Nix expression syntax: attrset patterns with defaults and `@` binders,
  `inherit (src)`, nested and dynamic attribute paths, `or` defaults, every
  operator including `/`, `->`, `|>` and `<|`, `<nixpkgs>` and interpolated
  paths; compiled output parses to the same AST as the source under
  `nix-instantiate --parse`
- Hindley-Milner let-polymorphism, row-polymorphic open records
  (`{ a :: T; ... }`), optional fields (`name? :: T`), `AttrsOf` dictionaries,
  and rigid `forall` signatures
- gradual typing with ambient declarations, HKT support, indexed `Vec` / `Matrix` / `Tensor`, and heterogeneous `Tuple`
- numeric singleton/primitive support via `Float`, `Number`, `Nat`, `Range`, and `Unit`
- explicit `expr as Type` casts for widening, narrowing, and gradual-boundary assertions
- TypeScript-style checker directives via `# @tynix-ignore` and `# @tynix-expected`
- diagnostics with exact source spans (`line:col: [CODE] message`), naming the
  missing or ill-typed field of a record
- a typed prelude for every Nix builtin, embedded in the binary, so `builtins.*`
  and globals such as `toString` and `map` are checked with no setup
- project bootstrapping via `tynix init`, `tynix scaffold`, and `tynix.config.tynix`
- shipped declaration files for `builtins`, `flake.nix`, and `tynix.config.tynix`
- bundled declaration packs under `registry/` for workspace files and popular Nix ecosystem surfaces

Unannotated Nix code is accepted gradually: dependencies injected through an
attrset pattern (`{ lib, fetchFromGitHub, ... }:`) stay `dynamic` instead of
being pinned to their first use, and about 98% of a sample of unannotated
nixpkgs files type-check as they are.

## Installation

On Linux and macOS, install the prebuilt CLI and language server with:

```bash
curl -fsSL https://tynix.dev/install.sh | sh
```

The installer picks the archive for your platform (Linux x64/arm64, macOS
arm64/x64), verifies its SHA-256 checksum, and installs `tynix` and `tynix-lsp`
into `~/.tynix/bin`. The binaries do not depend on Nix. Pin a release with
`curl -fsSL https://tynix.dev/install.sh | TYNIX_VERSION=0.5.0 sh`, choose
another directory with `TYNIX_INSTALL_DIR`, and remove everything with
`curl -fsSL https://tynix.dev/install.sh | sh -s -- --uninstall`.

With Nix, use the flake instead:

```bash
nix profile install github:ubugeeei-prod/tynix        # tynix + tynix-lsp
nix run github:ubugeeei-prod/tynix -- check ./main.tynix
```

The flake also provides `overlays.default` and NixOS / nix-darwin / Home Manager
modules (`programs.tynix.enable = true;`). See
[docs/getting-started.md](./docs/getting-started.md#installation) for details.

You can also download the archives (`tynix-<version>-<target>.tar.gz`, with
`.sha256` checksums and build provenance attestations) from GitHub Releases.
See [docs/support-matrix.md](./docs/support-matrix.md) for the per-platform
tier table. **Windows users:** the CLI is not tested on Windows. Use WSL2 and
run the install script there.

Quick verification:

```bash
tynix --version
tynix-lsp --version
tynix check ./examples/main.tynix
tynix check-project ./examples
```

### Editor setup

One command installs the editor extension and writes the editor config:

```bash
tynix ide install vscode     # or: cursor, vscodium, zed, neovim, helix
tynix ide install zed --global --dry-run   # preview user-level changes
tynix ide list               # supported editors and what is detected
tynix doctor                 # check tynix / tynix-lsp / project / editor wiring
```

Settings are merged, never clobbered, and re-running is a no-op. See
[docs/editors.md](./docs/editors.md) for what each editor gets and for every
flag.

For local development, enter the reproducible shell first:

```bash
nix develop
nix flake check --accept-flake-config
pnpm run check
vp run workspace:check
vp cli
vp ide
```

`nix flake check` now exercises the published flake outputs, version metadata,
smoke-tests the built `tynix` / `tynix-lsp` binaries, runs the Haskell package
test suites, and validates the dogfood/example corpus with the packaged CLI.
`pnpm run check` is the conventional npm-compatible entrypoint and delegates to
the full workspace verification suite, including editor integrations.
`vp run workspace:check` is the direct task-runner entrypoint.
`vp cli` installs the local `tynix` / `tynix-lsp` toolchain into your active Nix
profile. `vp ide` reuses that toolchain install, packages the VS Code
extension, and installs the local Zed extension when its support directory is
available.

## Bundled Registry Packs

The repository includes reusable `.d.tynix` packs under `registry/` so projects
can vendor common declarations instead of rewriting ambient files from scratch.

Workspace-oriented packs live under `registry/workspace/`:

- `registry/workspace/builtins.d.tynix`
- `registry/workspace/flake.d.tynix`
- `registry/workspace/tynix.config.d.tynix`

Ecosystem alias packs live under `registry/ecosystem/`:

- `registry/ecosystem/nixpkgs-lib.d.tynix`
- `registry/ecosystem/nixpkgs-pkgs.d.tynix`
- `registry/ecosystem/flake-ecosystem.d.tynix`
- `registry/ecosystem/community-flakes.d.tynix`

Typical usage is to copy the pack you want into your declaration directory and
reuse its aliases from local `declare` blocks:

```tynix
declare "./flake-utils.nix" { default :: NixFlakeUtilsFlake; };
declare "./devenv.nix" { default :: DevenvFlake; };
declare "./treefmt-nix.nix" { default :: TreefmtNixFlake; };
```

Projects can also point `tynix.config.tynix` at external pack files or
directories directly:

```tynix
{
  declarationPacks = [
    ../vendor/tynix/registry/ecosystem
    ../vendor/tynix/registry/workspace
  ];
}
```

When a configured pack comes from `registry/workspace/`, tynix rebases its
ambient `declare` targets to your project root so upstream workspace packs can
be used without copying them into the repo first.

The workspace packs assume they live under `registry/workspace/` so their
relative `declare` targets resolve back to the project root.

## Example Catalog

The repository ships a larger sample set under [`examples/`](./examples/README.md).
It includes basic language features, gradual typing examples, indexed container
samples, and legacy interop fixtures that can be checked with one command:

```bash
tynix check-project ./examples
```

See [CHANGELOG.md](./CHANGELOG.md) for the release history.

## Distribution

The primary distribution channel is GitHub Releases. Tagged releases publish
prebuilt `tynix` and `tynix-lsp` archives for supported platforms together with
checksums, plus a packaged VS Code `.vsix` extension. When marketplace tokens
are configured, the same tag also publishes the extension to VS Code
Marketplace and Open VSX.

The flake also exports installable packages and runnable apps:

```bash
nix build github:ubugeeei-prod/tynix#tynix
nix run github:ubugeeei-prod/tynix#tynix -- check ./main.tynix
nix run github:ubugeeei-prod/tynix#tynix-lsp
nix flake check github:ubugeeei-prod/tynix --accept-flake-config
```

See [RELEASING.md](./RELEASING.md) for the release flow.
