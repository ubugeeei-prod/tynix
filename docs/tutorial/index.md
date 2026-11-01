---
title: Tutorial
description: A hands-on, step-by-step tour of tynix, from installing the CLI to type-checking a flake in CI.
---

# Tutorial

This tutorial takes you from an empty directory to a type-checked flake that
runs in CI. Each step builds on the previous one and every example is a real
file that you can check with `tynix check`.

You should be comfortable reading Nix. No TypeScript or Haskell knowledge is
required, though readers who know either will recognize the ideas.

## What you will build

You start with a single `hello.tynix` file. Along the way you will type a small
package description, describe an untyped `.nix` module with a declaration file,
write generic helpers, author a `flake.tynix`, and finally turn the directory into
a tynix project that `tynix check-project` verifies on every pull request.

## Steps

<div class="tx-steps">

1. [Install](./install.md): get the `tynix` CLI and language server.
2. [Your first .tynix file](./first-file.md): check it, compile it, read an error.
3. [Annotations and inference](./annotations.md): signatures, literal types, functions and polymorphism.
4. [Attribute sets and structural typing](./attrsets.md): records, width subtyping, unions and dynamic keys.
5. [Gradual typing](./gradual.md): `dynamic`, `unknown`, `any`, `as` casts and directives.
6. [Typing existing .nix](./declarations.md): `.d.tynix` files, `declare` blocks and `tynix emit`.
7. [Generics and higher-kinded types](./generics.md): `forall`, generic aliases and kinds.
8. [Conditional types and infer](./conditional-types.md): type-level pattern matching.
9. [Typing a flake and a package.nix](./flakes-and-packages.md): real-world Nix shapes.
10. [Editor setup](./editor.md): `tynix ide install`, `tynix doctor` and the language server.
11. [Projects](./projects.md): `tynix init`, `tynix.config.tynix`, `check-project` and `build`.
12. [CI integration](./ci.md): gate pull requests on the checker.

Going further, each of these stands on its own:

- [13. Effects](./effects.md): tracing, throwing, reading, fetching, purity and pure flakes.
- [14. Linearity and captures](./linear-types.md): consume-once arguments and what closures may hold.
- [15. Dependent and opaque types](./dependent-types.md): value-indexed results, phantom types and kinds.
- [16. Macros](./macros.md): hygienic, declarative, typed code generation.

</div>

## Conventions

- Code blocks titled with a file name, such as `hello.tynix`, are files you
  create. Blocks without a title are terminal sessions or fragments.
- The output shown under `tynix check` is copied from the real CLI. Bindings are
  listed alphabetically after the `root:` line, and diagnostics start with the
  `line:column` of the offending expression.
- Everything is ordinary Nix syntax plus type annotations. Default arguments,
  `@` binders, `inherit (x)`, nested attribute paths, `<nixpkgs>`, every
  operator and the global builtins such as `toString` all work, so examples are
  written the way you would write Nix.

> [!TIP]
> Already know the basics and want the rules instead of a walkthrough? Jump to
> the [language reference](../language-reference.md) or to
> [how checking works](../reference/type-system-internals.md).

<div class="tx-pager">

[Start: 1. Install →](./install.md)

</div>
