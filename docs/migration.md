# Adopting tynix in an Existing Project

`tynix` is built for incremental adoption, the same way TypeScript was added to
existing JavaScript codebases. You do **not** rewrite your `.nix` files. You add
types alongside them, type-check the surfaces you care about, and grow coverage
over time. Erased output is ordinary Nix, so adoption never changes runtime
behavior.

This guide walks an existing flake-based repository from zero to a checked
surface.

## 1. Add tynix to the toolchain

Install the CLI (and language server) from the flake, or vendor the repository
and run through the dev shell:

```bash
nix profile install github:ubugeeei-prod/tynix#tynix
nix profile install github:ubugeeei-prod/tynix#tynix-lsp
```

Verify it runs:

```bash
tynix --version
```

## 2. Create a project config

From the repository root:

```bash
tynix init
```

This writes a `tynix.config.tynix` plus starter files. Point `sourceDir`/`entries`
at the directories where your typed `.tynix` files will live, and set
`declarationDir` to where you'll keep ambient `.d.tynix` declarations.

## 3. Type your first existing module — don't rewrite it

Keep the runtime implementation in `.nix`. Describe its public API in a
`.d.tynix` (or an inline `declare`) and consume it from typed `.tynix`:

```tynix
declare "./legacy/default.nix" {
  default :: { name :: String; version :: String; };
};

import ./legacy/default.nix
```

This is the core bridge:

- the implementation stays in `.nix`,
- its surface is described once in a declaration,
- typed code consumes that surface and is checked against it.

When you do own the implementation, write it as `.tynix` and import the `.nix`
path it compiles to. A sibling `lib.tynix` types `import ./lib.nix`; you do not
restate that type in a `declare`. See
[declarations](./tutorial/declarations.md#when-the-implementation-is-tynix).

## 4. Type the flake surface

A flake can be converted to `flake.tynix` directly; see the
[flake tutorial](./tutorial/flakes-and-packages.md#a-flake). If you would
rather keep the full implementation in `flake.nix`, declare its surface and
write a typed *projection* over the parts you want checked:

```tynix
declare "./flake.nix" {
  description :: String;
  outputs :: dynamic -> {
    devShells :: { aarch64-darwin :: { default :: Derivation; }; };
  };
};

let
  flake = import ./flake.nix;
  outputs = flake.outputs { };
in {
  description = flake.description;
  devShell = outputs.devShells.aarch64-darwin.default;
}
```

You stay on a stable, checked surface while the real flake logic remains
untouched.

## 5. Reuse bundled ecosystem declarations

Instead of hand-writing ambient types for common dependencies, reuse the packs
under `registry/`. Either copy them in, or reference them without vendoring by
listing them in `declarationPacks` in `tynix.config.tynix`:

```tynix
{
  declarationPacks = [
    ../vendor/tynix/registry/ecosystem
    ../vendor/tynix/registry/workspace
  ];
}
```

Packs cover `nixpkgs.lib`, `pkgs` / `import nixpkgs`, `flake-utils`,
`home-manager`, `nix-darwin`, `flake-parts`, `devenv`, and more — see
[getting-started.md](./getting-started.md#bundled-ecosystem-declarations).

## 6. Check the project and wire it into CI

```bash
tynix check-project ./. --format json
```

This exits non-zero on type errors, so it gates a build directly. A typical CI
step runs the same command; see [ci-cd.md](./ci-cd.md) for hardening notes and
the JSON output contract.

## 7. Grow coverage incrementally

- Start with the highest-value surfaces (your flake outputs, shared libraries).
- Use `dynamic` / `unknown` / `any` at gradual boundaries and tighten them later
  (see [type-system.md](./type-system.md)).
- Use `expr as Type` casts at boundaries you can't yet prove, and
  `# @tynix-ignore` to defer individual diagnostics without blocking the rest.
- Generate declaration files for typed modules with `tynix emit` /
  `tynix emit-project` so downstream consumers get a checked surface.

## What not to migrate (yet)

Any Nix file can be renamed to `.tynix`: the parser accepts the whole Nix
expression language, and unannotated code is checked gradually (injected
dependencies stay `dynamic`). Whether converting is worth it is a separate
question. Modules whose value comes from the `config` fixpoint, such as NixOS
modules, are typed as ordinary functions today, so ambient typing (steps 3–4)
is often the better trade for them until `config`-aware typing lands (see the
[roadmap](./roadmap.md)).
