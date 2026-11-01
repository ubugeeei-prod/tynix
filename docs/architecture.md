# Architecture

## Monorepo Strategy

Haskell is the core implementation language. Editor integrations stay thin and are written only in the languages required by each host editor.

### Haskell packages

- `tynix-core`
  - AST
  - parser
  - type checker
  - declaration emitter
  - `.tynix -> .nix` compiler

  `Check` retains the public checker API and syntax-directed expression
  inference. Its private modules separate the underlying responsibilities:

  - `Check.Inference` — inference state, transactional attempts, scheme generalization
  - `Check.Constraints` — directional constraints, structural unification, casts
  - `Check.Effects` — effect-row constraints and handler scopes
  - `Check.Expression` — dependency discovery and binder-usage analysis
  - `Check.Diagnostics` — shared rendering of diagnostic names and types

  Only `Check.Inference` reads or writes inference state. Failed speculative
  attempts restore substitutions, the fresh-variable supply, soft metas, effect scopes, and usage tracking.

- `tynix-cli`
  - `compile`
  - `check`
  - `emit`
- `tynix-lsp`
  - diagnostics
  - hover
  - signature help
  - document formatting (re-render; no-op when comments are present)
  - go to definition
  - declaration lookup

  `Session` composes request handlers and their text-based fallbacks.
  Shared request plumbing and feature algorithms live in focused modules:

  - `AnalysisCache` — content-keyed memoization of driver runs
  - `SessionTypes` — shared records (Documents, IndexedSymbol, etc.)
  - `SessionRequest` — private request decoding, analysis loading, UTF-16 position encoding
  - `SessionDocuments` — didOpen/didChange/didClose + workspace loader
  - `SessionWorkspace` — workspace traversal and marker discovery
  - `SessionSymbols` — document / workspace symbol index
  - `SessionReferences` — definition / references / rename resolution
  - `SessionText` — pure span / boundary helpers
  - `SessionDiagnostics` — diagnostic inspection and quick-fix builders
  - `SessionSemanticTokens` — semantic-tokens provider
  - `SessionFolding` — folding-range provider
  - `SessionLinks` — document-link provider
  - `SessionInlayHints` — inlay-hint provider for inferred `let` types

### Editor packages

- `editors/vscode`
  - TypeScript
  - managed by `pnpm`
  - built and checked with `vp`
- `editors/zed`
  - a thin Rust launcher
- `editors/neovim`
  - a thin Lua setup helper

## Development Environment

- environment provisioning with `nix`
- TypeScript package management with `pnpm`
- task runner with Vite+ via `vp`

`nix develop` is expected to provide:

- `ghc`
- `cabal`
- `haskell-language-server`
- `node`
- `pnpm`
- `vp`
- `rustc`
- `cargo`

## Compiler Pipeline

1. parse
   - parse `.tynix` and `.d.tynix` into ASTs
2. collect declarations
   - type aliases
   - ambient declarations
   - workspace declaration sidecars
3. type check
   - inference
   - subtype and consistency checks
   - diagnostics
4. emit
   - generate `.nix`
   - generate `.d.tynix`

## LSP Responsibilities

- parse and type-check the current `.tynix` buffer
- index workspace `.d.tynix` files
- resolve declarations for `import` targets
- publish diagnostics
- return inferred types and declaration origins in hover

## Editor Integration Strategy

### VS Code

- TextDocumentSync
- diagnostics push
- hover
- signature help
- document formatting
- semantic tokens in a later phase

### Zed

- a minimal extension whose main job is starting `tynix-lsp`
- treat `.tynix` as a Nix-like language

### neovim

- a thin helper around `vim.lsp.start`
- no custom UI layer

## Test Strategy

The most important tests live in `tynix-core`.

- parser golden tests
- subtype and consistency tests
- inference tests
- conditional and `infer` tests
- declaration emitter tests
- compile erasure tests
- regression fixtures

The LSP and editor integrations stay thin and mainly verify startup, protocol wiring, and small regression cases.

## Quality Strategy

- prefer explainable errors
- keep generated `.nix` readable
- stabilize `.d.tynix` as an API contract format
- keep implementation files below 250 lines with clearly separated responsibilities
