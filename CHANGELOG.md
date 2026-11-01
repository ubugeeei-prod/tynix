# Changelog

## Unreleased

### Added: a much richer type system

- **Effects.** Function types carry the effects a call may perform
  (`String -> String ! { Trace }`): `Trace`, `Throw`, `Abort`, `Read`, `Fetch`,
  `Store`, `Impure`. Effects are inferred, generalized as effect variables
  (`twice :: forall t0 e0. (t0 -> t0 ! e0) -> t0 -> t0 ! e0`), bounded by
  signatures (`! {}` demands purity, [`TC0023`]), and discharged by
  `builtins.tryEval`. `flake.tynix` files may not perform `Impure`
  ([`TC0024`]). The builtins prelude annotates every effectful builtin; arrows
  written without `!` stay untracked, so existing code is unaffected.
- **Linear types.** `A %1 -> B` is now enforced precisely: every path must
  consume the argument exactly once, and closures and unrestricted calls count
  as many uses ([`TC0026`]). Inferred multiplicities are accurate too, so a
  curried function's outer arrows are no longer reported as linear.
- **Capture tracking.** `A ->{fetch} B` bounds the effectful capabilities a
  closure captures; `->{}` allows none ([`TC0025`]).
- **Dependent arrows.** `(n :: Nat) -> Vec n a` lets a result depend on an
  argument's value, with the type-level operators `Get`, `KeyOf`, `Length`,
  `Add`, `Sub`, and `Mul`. `builtins.getAttr`, `builtins.genList`, and
  `builtins.length` are now dependent: `builtins.getAttr "port" { port = 80; }`
  is `80`.
- **Opaque and phantom types.** `opaque type Id t = String;` declares a nominal,
  invariant type; `as` converts to and from the representation, and selecting a
  field of an opaque type is [`TC0027`].
- **Higher-rank polymorphism.** `forall` may appear in argument positions;
  such parameters are instantiated at each use and arguments are checked
  against fresh skolems.
- **`rec` generalization.** `rec { ... }` fields generalize like `let`
  bindings.
- **Kind annotations** on alias parameters: `type Fix (f :: Type -> Type) = ...;`.
- **Type ascriptions** `(e :: T)`.
- **Hygienic, declarative, typed macros.** `macro name { (pattern) => (template); };`
  with `expr`, `ident`, `type`, `string`, and typed (`$x :: T`) fragments,
  repetitions, and `stringify!`. Template binders are renamed per expansion,
  free names are pinned to `builtins`, templates are type-checked at their
  definition, and typed arguments at the call. New diagnostics `TX0001` to
  `TX0006`.

New tutorial chapters (13 to 16) and a reference page,
[Effects, Linearity & Macros](https://tynix.dev/reference/advanced-types),
cover all of it.

[`TC0023`]: https://tynix.dev/diagnostics#tc0023--effect-not-allowed
[`TC0024`]: https://tynix.dev/diagnostics#tc0024--impure-operation-in-pure-evaluation
[`TC0025`]: https://tynix.dev/diagnostics#tc0025--closure-captures-a-capability-its-type-does-not-allow
[`TC0026`]: https://tynix.dev/diagnostics#tc0026--linearity-violation
[`TC0027`]: https://tynix.dev/diagnostics#tc0027--field-selected-from-an-opaque-type

## v2.0.0 - 2026-10-05

### Changed (breaking): tnix is now tynix

The project is renamed to **tynix** ("tie-nix", typed Nix) and moves to
[tynix.dev](https://tynix.dev). There is no compatibility layer; update
projects and editors as follows:

| Before | After |
| --- | --- |
| `.tnix`, `.d.tnix` | `.tynix`, `.d.tynix` |
| `tnix.config.tnix` | `tynix.config.tynix` |
| `tnix`, `tnix-lsp` | `tynix`, `tynix-lsp` |
| `# @tnix-ignore`, `# @tnix-expected` | `# @tynix-ignore`, `# @tynix-expected` |
| `TNIX_*` environment variables | `TYNIX_*` |
| VS Code extension `ubugeeei.tnix` | `ubugeeei.tynix` |
| `github:ubugeeei-prod/tnix` | `github:ubugeeei-prod/tynix` |
| `https://tnix.dev/install.sh` | `https://tynix.dev/install.sh` |

A project can be migrated with:

```bash
find . -name '*.tnix' -exec sh -c 'mv "$1" "${1%.tnix}.tynix"' _ {} \;
perl -pi -e 's/\@tnix-/\@tynix-/g' $(git ls-files '*.tynix')
```

The language, the type checker, and the declaration packs are unchanged from
v1.0.0. The wordmark is redrawn for the new name.

## v1.0.0 - 2026-10-05

### Added

#### Language: full Nix syntax

- `.tnix` accepts the whole Nix expression language: attrset lambda patterns with defaults and binders (`{ a, b ? 1, ... }@args:`, `args@{ ... }:`), optional erased field annotations (`{ name :: String, version ? "1" }:`), `inherit (src) a b;` in attribute sets and `let`, nested and dynamic attribute paths (`a.b.c = 1;`, `${k} = v;`), `x.a.b or default`, `x ? ${k}`, the operators `/`, `->` (implication), `|>` and `<|` (pipes) and prefix `-`, `<nixpkgs>` search paths, `~/` and interpolated paths (`./${name}.nix`), unquoted URIs, and `$$` string escapes.
- Only Nix keywords are reserved in expressions, so `type`, `any`, `import`, and `declare` are ordinary names. `as` is contextual: `as: as.x` works, and `e as T` is still a cast.
- Parity is verified against nixpkgs: 4000 sampled files parse, and their compiled output parses to the same AST as the source under `nix-instantiate --parse` (up to how equal strings are split). `scripts/check-nix-parity.sh` runs this check in CI.
- Haskell-style constraint contexts (`Functor f =>`, `(Eq a, Show a) =>`) parse. They are documentation only until tnix has type classes.

#### Type system

- Hindley-Milner let-polymorphism: unannotated `let` bindings are inferred per dependency group (strongly connected component) and generalized over the inference variables that are not free in the enclosing environment. `let id = x: x; in { a = id 1; b = id "s"; }` checks, and mutually recursive bindings such as `even` / `odd` solve without annotations.
- Row polymorphism: selecting from a value of unknown shape infers an open record, so `x: x.a + x.b` is `{ a :: Number; b :: Number; ... } -> Number`. Open records are written `{ a :: T; ... }` or with a named row `{ a :: T; ...r }`, and `//` is row-aware (`x: x // { y = 1; }` is `{ ...t0 } -> { y :: 1; ...t0 }`).
- Optional fields `name? :: T;`. Pattern fields with defaults produce them: `{ a, b ? 2, ... }: a + b` is `{ a :: Int; b? :: Int; ... } -> Int`, and omitting `a` at a call site is `TC0009`. `x ? null` defaults do not constrain the argument type.
- `AttrsOf a` dictionaries: `{ ${k} = 1; }` is `AttrsOf 1`, records are subtypes of `AttrsOf t` when every field fits `t`, and builtins such as `mapAttrs`, `listToAttrs`, and `readDir` use it. Mixing static and computed keys gives `{ a :: T; ... }`.
- `x.a or d` never makes `a` a required field.
- Gradual inference for unannotated Nix: arguments injected through attrset patterns (`{ lib, fetchFromGitHub, ... }:`) and fields selected from unknown records are "soft", so calling one is `dynamic -> dynamic` instead of pinning it to its first call site. Plain lambda binders keep full principal types (`compose = f: g: x: f (g x)` is `forall t0 t1 t2. (t1 -> t2) %1 -> (t0 -> t1) %1 -> t0 %1 -> t2`). About 98% of a sample of 800 unannotated nixpkgs files type-check.
- Literals are widened where precision would only cause false errors: pattern defaults, arguments to unknown callees, and `if` branches whose other side is unknown. `+` concatenates strings and paths (`path + string` is a `Path`).
- A richly typed `builtins` prelude (`registry/workspace/builtins.d.tnix`) is embedded into every binary, so `builtins.*` and the global builtins (`toString`, `map`, `throw`, `import`, `derivation`, `abort`, `baseNameOf`, `dirOf`, `fetchTarball`, `isNull`, `removeAttrs`, `placeholder`, ...) are typed with no project setup. It also provides aliases such as `Derivation`, `DerivationArgs`, `FetchedSource`, `PathLike`, `FileType`, `TypeName`, and `NameValuePair`. A workspace that declares `builtins` overrides it. `vp run generate:prelude` regenerates the embedded copy and `vp run check:prelude` checks it in CI.
- Diagnostics carry precise source spans. The CLI prints `line:col: [CODE] message`, call mismatches point at the argument, and record mismatches name the missing (`TC0009`) or ill-typed (`TC0013`) field. New code `TC0022`: dynamic attribute in `let`.

#### Language server

- Editors underline the exact span the checker reports; field and name errors narrow to the named token.
- Scope- and type-aware completion (member fields with types, names in scope, keywords, snippets, type positions, expected-record keys, path literals), hover cards with documentation, signature help with parameter names, scope-aware definition, references and rename, hierarchical symbols, selection ranges, and richer semantic tokens.
- Diagnostics with code links, related information, "did you mean" suggestions, debounced publishing, and pull diagnostics. New lints `TL0001` (unused binding, rendered as unnecessary) and `TL0002` (use of a declaration documented `@deprecated`).
- Code actions: did-you-mean, add a missing field, remove or `_`-prefix an unused binding, insert the inferred signature.
- Requests run on a worker queue with `$/cancelRequest` support.

#### Editors

- `tree-sitter-tnix` grammar (full Nix plus tnix) with highlight, injection, locals, indent, and fold queries, used by Zed and Neovim.
- VS Code: a generated TextMate grammar for full Nix plus tnix syntax (including optional fields and open rows) with a Markdown code-block injection, snippets, a walkthrough, file icons, doctor and toolchain commands, and `tnix-lsp` detection.
- Neovim: ftdetect/ftplugin, `vim.lsp.config` on 0.11+ with a fallback, nvim-treesitter registration, and `:checkhealth tnix`.

#### CLI

- `tnix ide install <vscode|cursor|vscodium|zed|neovim|helix>` installs the editor extension and merges editor configuration (`--project`/`--global`, `--dry-run`, `--no-extension`, `--force`, `--lsp-path`); `tnix ide list` shows supported editors and detection status.
- `tnix doctor [--format json]` checks `tnix` / `tnix-lsp` on `PATH` and their versions, the project config, each detected editor's integration, and `nix`.
- `tnix init --editor EDITOR` chains into `tnix ide install`.
- `tnix compile --no-check` erases types without type-checking.

#### Distribution

- `curl -fsSL https://tnix.dev/install.sh | sh` installs prebuilt, self-contained `tnix` and `tnix-lsp` binaries for Linux x64/arm64 and macOS arm64/x64 after verifying their SHA-256 checksums (`TNIX_VERSION`, `TNIX_INSTALL_DIR`, `--uninstall`).
- `nix profile install github:ubugeeei-prod/tnix` installs both binaries: the flake's default package is now `tnix-toolchain`. The flake also exports `overlays.default` and NixOS / nix-darwin / Home Manager modules (`programs.tnix.enable = true;`).

#### Documentation

- Documentation site with a brand system (logo, palette, typography, code theme), a landing page, a 12-step tutorial, and a reference section ("How Checking Works", CLI, configuration, builtins and registry), deployed to Cloudflare Pages.
- The tutorial, README, getting-started guide, and language reference are rewritten for the full Nix syntax, with every example checked against the CLI. The grammar, type-system, internals, CLI, roadmap, troubleshooting, and diagnostics pages describe HM inference, rows, optional fields, `AttrsOf`, soft dependencies, the builtins prelude, span diagnostics, and the `TLxxxx` lints.

### Fixed

- Compiled `.nix` output no longer breaks on strings that contain a quote, a backslash, or an antiquotation marker. `Pretty` renders string literals verbatim, so a source such as `message = "he said \"hi\""` compiled to `"he said "hi""`, which Nix rejects outright. Double-quoted text now escapes `"`, `\`, newline/CR/tab, and a `$` that would open an antiquotation; indented text escapes `${` and the single quotes that would pair with a delimiter. The same escaping applies inside interpolation segments, attribute names, type-level string literals, and `declare` paths.
- Indented strings no longer absorb the indentation of the block they sit in. Because the parser reads the body back verbatim, the extra whitespace compounded on every recompile and `tnix compile` was not idempotent for any file with a multi-line `''` string.
- `ParserExpr` accepts the two Nix escapes it was missing inside indented strings, `''$` and `''\<char>`. Both were previously unparseable, so this only widens what the frontend accepts.
- The parser and kind checker now emit the diagnostic codes `docs/diagnostics.md` already documented: `TP0001`-`TP0004` and `TK0004`. They were catalogued but never produced.

### Performance

- `tnix check-project` on a 60-source project with 25 declaration packs: **0.96 s -> 0.15 s**. `analyzeText` re-discovered and re-parsed every `.d.tnix` file in the workspace once per source file; `Driver` now exposes a `SupportCache` keyed by workspace root. `check-project`, `build`, and `emit-project` create one per command, and the language server creates one per request so it can never serve a declaration file the client has since edited.
- Type-layer hot paths, measured over the checker's own operations:
  - `joinTypes` on 200-member unions: **6.4 ms -> 0.69 ms**. `flattenUnion` deduplicated with a linear `elem` scan.
  - `isSubtype` on 200-member unions: **1.32 ms -> 0.19 ms**. Union-vs-union subtyping now consults a set of the right-hand members before falling back to the quadratic scan.
  - `resolveType` on a depth-14 record: **6.5 us -> 1.0 us**. Alias expansion and indexed normalization are whole-tree walks that were being re-run at every node.
  - `substituteMetas` with an empty substitution: **323 ns -> 18 ns**. Instantiating a monomorphic scheme and zonking before the first solved meta both rebuilt the whole type to produce an identical copy.
- `expandAliases` charges its recursion budget for expansions rather than for structural descent. Past 32 levels of nesting it silently stopped expanding aliases, which also made the result non-idempotent.
- `SessionText.wordSpans` no longer calls the O(n) `Text.index` once per match, and `collapseParentSegments` no longer rebuilds its accumulator per segment.
- The language server's analysis cache evicts a single least-recently-used entry instead of sorting and rebuilding the whole map. The keys hold full document text, so the rebuild re-compared hundreds of documents on every keystroke once the cache was full.

### Changed

- `forall` signatures are rigid: the body is checked with the quantified variables held abstract, so `id :: forall a. a -> a; id = x: 1;` is now rejected (`type mismatch: 1 vs a`). Code that relied on the old, unsound behavior needs a narrower signature.
- `builtins` is no longer `dynamic` without a declaration; it is typed by the embedded prelude. Code that used builtins in ways their real types do not allow may now report errors. A workspace `declare "builtins"` still replaces the prelude.
- Checker diagnostics are prefixed with `line:col:`, also in the `error` field of `--format json` reports.
- Declaration discovery only recurses inside real workspaces (a directory with `tnix.config.tnix`, `flake.nix`, `.git`, `cabal.project`, or `pnpm-workspace.yaml`) and skips hidden directories, `node_modules`, `dist-newstyle`, `result*` links, and symlinked directories. Outside a workspace, only declaration files next to the checked file are loaded.
- Indented-string escapes `''\n`, `''\r`, and `''\t` are kept verbatim in compiled output, preserving Nix's indentation stripping.
- `Driver` exports `SupportCache`, `newSupportCache`, and the `...With` analysis entry points. The existing functions are unchanged and create a private cache per call.
- `Check` exports `resolvePath` and `collapseParentSegments`; the driver kept a second copy that had to stay in sync with the checker's for ambient declarations to keep matching the imports they describe.
- `Diagnostics` exports `allDiagnosticCodes`, so tooling and tests enumerate the catalogue instead of restating it.

### Tests

- `cabal test all` runs 24 suites and 605 examples, up from 20 suites and 447.
- New suites: `Pretty.spec` (render/parse round-trip, including 300 generated cases over an alphabet of quotes, backslashes, and antiquotation markers), `Type.spec` (substitution and meta-closure laws), `Diagnostics.spec` (every code is uniquely identified, documented, and actually emitted by some non-test source), and `Check.spec` (import path resolution).
- Compiled output is now asserted to be a fixed point over a corpus covering the executable grammar: compiling twice must produce the same text, and the result must contain no type-only syntax.
- Property coverage for the algorithms replaced above: `flattenUnion` is idempotent and duplicate-free, `expandAliases` and `resolveType` reach a fixed point, `joinTypes` is an upper bound of both sides, union subtyping agrees with its member-by-member definition, and `wordSpans` agrees with a direct scan over every offset.
- `Kind.spec` grew from 5 examples to 19, `Indexed.spec` from 12 to 25, and `AnalysisCache.spec` from 6 to 11; `Cli.spec` covers project discovery filters, config decoding errors, and the JSON report contract.

### CI / Tooling

- The workspace builds warning-free under `-Wall`. Removed redundant imports, a missing top-level signature, a partial `head`, and the `other-modules` entries several LSP test suites were missing.

## v0.5.0 - 2026-05-19

### Added

- Stable diagnostic-code system: every parser, kind-checker, type-checker, and driver error is now tagged with a `[TPxxxx]` / `[TKxxxx]` / `[TCxxxx]` / `[TDxxxx]` prefix, catalogued in [`docs/diagnostics.md`](./docs/diagnostics.md).
- Full EBNF grammar reference in [`docs/grammar.md`](./docs/grammar.md), cross-checked against the parser sources.
- Project governance documentation in [`GOVERNANCE.md`](./GOVERNANCE.md) and platform-support tiers in [`docs/support-matrix.md`](./docs/support-matrix.md).
- Structured parser errors via `Parser.parseProgramDetailed` returning a `ParseError` with 1-based `(line, column, message)` so editors can attach diagnostics to the exact span.
- Workspace-wide analysis cache (`AnalysisCache`) that memoizes driver results by `(path, content)` with bounded FIFO eviction; cached entries are reused across `workspace/symbol`, `definition`, `references`, and `hover` requests.
- VS Code extension surfaces LSP startup failures and exposes a `tnix.restartServer` command plus a status bar indicator (running / starting / error / stopped).
- CycloneDX SBOMs published alongside every tagged release archive (`*-linux-x64.cdx.json`, `*-macos-arm64.cdx.json`, `tnix-vscode-*.cdx.json`) and attested with `actions/attest`.
- Release smoke step now exercises packaged `tnix` against the in-tree `examples/` and `dogfood/` corpora and drives a minimal JSON-RPC handshake against packaged `tnix-lsp`.
- LSP folding-range provider (`textDocument/foldingRange`) that folds `let-in` blocks, attribute sets, list and parenthesised literals, multi-line indented strings, and runs of consecutive `#` comment lines. The scan is text-driven, so it keeps producing useful folds even when the buffer has type errors. Covered by `tnix-lsp-session-folding-spec`.
- LSP document-highlight provider (`textDocument/documentHighlight`) that paints every occurrence of the identifier under the cursor inside the active buffer. Symbol resolution goes through the same machinery as `references` and `rename`, so dotted selections light up the same matches editors would jump or rebind to.
- LSP document-link provider (`textDocument/documentLink`) that turns every `import ./path` path literal and `declare "./path"` string literal into a clickable link. Relative paths are resolved against the source file's directory. The scan is text-driven so it keeps producing links even when the buffer fails to type-check. Covered by `tnix-lsp-session-links-spec`.
- LSP inlay-hint provider (`textDocument/inlayHint`) that surfaces inferred `:: Scheme` annotations next to top-level `let` bindings whose author has not written an explicit signature. Pretty-printed through the shared `renderScheme`, so the displayed text matches hover. Covered by `tnix-lsp-session-inlay-hints-spec`.

### Changed

- `Session.hs` (1191 LOC) split into eight focused modules (`SessionTypes`, `SessionWorkspace`, `SessionText`, `SessionSymbols`, `SessionReferences`, `SessionDocuments`, `SessionSemanticTokens`, `SessionDiagnostics`) — Session.hs is now ~397 LOC of handler dispatchers.
- Diagnostics are rendered through `Pretty` instead of `show`, so type / kind errors show surface syntax instead of internal AST constructors.
- Every previously invariant-guarded partial function in `tnix-core` (`Map.!`, `foldl1`, `foldr1`) is replaced with a total form.
- `tnix init` / `tnix scaffold` write scaffolded files via atomic tmp + rename so a crash mid-write cannot leave a half-populated `.tnix` / `.d.tnix` on disk.

### Fixed

- `tnix check-project` no longer hangs on directory symlink loops; the walker is cycle-safe and depth-bounded.
- LSP-side `didSave` notifications are now handled instead of dropped, and JSON-RPC framing errors are logged.
- `pnpm --filter tnix fmt` is no longer broken by including `.vscodeignore` (which has no prettier parser).

### CI / Tooling

- New `Lint (formatters)` CI job that runs prettier on the VS Code extension and `cargo fmt --check` on the Zed extension.
- New `Docs` CI workflow that runs `markdownlint-cli2` and `lycheeverse/lychee-action` (offline) on every `.md` change.
- Dependabot configuration covers GitHub Actions, the VS Code extension (npm), and the Zed extension (cargo).
- `.github/CODEOWNERS` routes review requests automatically.
- TypeScript bumped to 6.x; prettier to 3.8.3; `@vscode/vsce` to 3.9.1.

### Tests

- New unit suites for `Alias`, `Emit`, `AnalysisCache`, `SessionText`, `SessionWorkspace`, and `SessionDiagnostics` — `cabal test all` now runs 16 suites.

## v0.4.0 - 2026-05-18

- Harden GitHub Actions with pinned third-party actions, least-privilege
  permissions, checkout credential isolation, explicit timeouts, and release
  artifact attestations.
- Split CI into focused metadata, flake, Haskell, fixture, VS Code, Zed, and
  Neovim checks with a summary status for branch protection.
- Add release support documentation, CI/CD hardening notes, contribution and
  security policies, and conventional root `pnpm` scripts.
- Fix CLI JSON failure output and scaffolded string escaping, and make the
  Neovim integration detect `tnix.config.tnix` as a workspace root marker.

## v0.3.1 - 2026-04-06

- Support quoted attribute names, dynamic `${...}` selections, attrset lambda binders, and indented `'' ... ''` strings in executable `tnix`.
- Extend the checker, compiler, and pretty-printer so flake-oriented source shapes round-trip instead of falling out of the reliable subset.
- Add regression coverage for flake-style parsing, typing, and compile/emit behavior, including quoted selectors and dynamic package lookups.

## v0.3.0 - 2026-04-02

- Add project-aware CLI workflows for `check-project`, `build`, and `emit-project`, together with stronger config decoding and absolute root handling.
- Expand the LSP surface with richer editor features, cached analyzed documents, and better workspace-scale behavior.
- Ship a curated `examples/` showcase project and wire it into repeatable verification so sample code stays healthy in CI.
- Support `declarationPacks` in `tnix.config.tnix`, including direct consumption of upstream ecosystem packs and rebased `registry/workspace` packs without copying.
- Harden release and CI behavior with version sync validation, packaged binary smoke tests, and fixture lookup fixes for package builds.
- Extend core regression coverage with compile/emit golden tests and broader registry/config support tests.

## v0.2.0 - 2026-04-01

- Ship the first end-to-end `tnix` toolchain release with a Haskell core, CLI, LSP, and editor integrations for VS Code, Zed, and Neovim.
- Compile `.tnix` to `.nix`, type-check source files, and emit `.d.tnix` declaration files without adding any runtime layer.
- Add gradual typing primitives centered on `dynamic`, structural subtyping, ambient declarations for existing `.nix`, and declaration loading from workspace support files.
- Add type-level expressiveness including `forall`, conditional types, `infer`, higher-kinded aliases, and indexed container types for `Vec`, `Matrix`, `Tensor`, and `Tuple`.
- Infer exact container and sequence shapes from ordinary Nix list literals, including heterogeneous tuple inference and tensor widening back to structural list views when shapes diverge.
- Expand the regression suite across parser, checker, subtyping, compile/emit, CLI, LSP, VS Code, Zed, and Neovim validation.
