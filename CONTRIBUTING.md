# Contributing

Thanks for helping make `tynix` sturdier. The project is still small, so the best
contributions are focused, reproducible, and easy to review.

## Development Environment

Use the Nix development shell before running workspace commands:

```bash
nix develop
pnpm install --frozen-lockfile
```

The shell provides GHC/Cabal, Node.js, pnpm, Rust/Cargo, Neovim, and the `vp`
task runner used by CI.

## Common Commands

```bash
vp run workspace:check
nix flake check --accept-flake-config
```

For narrower loops:

```bash
vp run test:haskell
pnpm --filter tynix test
vp run test:zed
vp run check:neovim
```

## Code Style

Use `pnpm fmt` for Haskell, shared JavaScript/TypeScript sources (including the
documentation CSS), and the VS Code extension. Haskell, JavaScript and
TypeScript use an 80-column formatting measure. CI checks both configurations.

Keep exceptional and fallback paths visible with guards or early returns.
Name intermediate results and separate parsing, validation, and rendering
when nesting obscures their order. Generated parsers and embedded fixtures
follow their generators' output rather than manual formatting.

## Pull Requests

- Use conventional commit-style titles when practical, such as `fix(cli): ...`
  or `docs(release): ...`.
- Keep PRs small enough that each one can be reviewed and reverted on its own.
- Link the issue being addressed with `Closes #123` when the PR fully resolves
  it.
- Include the most relevant local verification command in the PR body.

## Regression Tests

Bug fixes should include a regression test near the affected surface:

- Haskell parser/checker/compiler behavior lives under `packages/tynix-core/src`.
- CLI project behavior lives under `packages/tynix-cli/src/Cli.spec.hs`.
- LSP behavior lives under `packages/tynix-lsp/src`.
- VS Code runtime behavior lives under `editors/vscode/src`.
- Zed and Neovim integration checks live under `editors/zed` and
  `editors/neovim`.

When a test is not practical, explain the residual risk in the PR body.

## Adding a Diagnostic Code

Every user-visible error has a stable code (e.g. `[TC0013]`) catalogued in
[`docs/diagnostics.md`](docs/diagnostics.md). When you add a new diagnostic:

1. Pick the next free number for its phase prefix — `TP` (parser), `TK` (kind
   checker), `TC` (type checker / semantic analysis), or `TD` (driver / project
   / IO).
2. Emit it through the shared `withCode` helper so the code prefixes the message.
3. Add an entry to `docs/diagnostics.md` describing the cause and a fix.

Codes are stable once assigned: to retire one, leave its entry and stop emitting
it — never reuse a number.

## Project Governance

How decisions get made, how releases get cut, and how to escalate is documented
in [`GOVERNANCE.md`](GOVERNANCE.md). Read it once before opening a larger
change — for small fixes, the rest of this document is sufficient.
