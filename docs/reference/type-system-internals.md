---
title: How Checking Works
description: A deep dive into the tynix type checker, from type representation and inference to the gradual lattice, kinds, conditional types, declarations, erasure and diagnostics.
---

# How Checking Works

This page explains what the tynix checker actually does, with references to the
modules in [`packages/tynix-core/src`](https://github.com/ubugeeei-prod/tynix/tree/main/packages/tynix-core/src)
that implement each part. It is written for people who want to predict the
checker's behavior, write declaration packs, or contribute to the core. For
the user-facing rules alone, see the [type system overview](../type-system.md)
and the [language reference](../language-reference.md).

The short version: tynix runs **Hindley-Milner inference** (let-polymorphism,
generalization per dependency group, rigid signatures) over a single structural
type tree, extended with **row-polymorphic records**, **subtyping** for records,
numbers and shapes, a **consistency** relation for the gradual `dynamic`
boundary, *soft* inference variables that keep injected dependencies gradual,
**kind inference** for higher-kinded aliases, and **structural reduction** for
aliases and conditional types. Types never reach runtime: compilation is pure
erasure. Effect rows, linearity, capture sets, dependent arrows, opaque types,
higher-rank polymorphism, and macros are specified separately in
[Effects, Linearity & Macros](./advanced-types.md).

## The pipeline

<figure class="tx-diagram">
<svg viewBox="0 0 960 250" xmlns="http://www.w3.org/2000/svg" role="img" aria-labelledby="pipe-title">
<title id="pipe-title">The tynix analysis pipeline, from source text to compiled Nix, declarations and diagnostics</title>
<defs><marker id="pipe-arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0 0 10 5 0 10z" class="d-arrowhead"/></marker></defs>
<rect x="10" y="20" width="120" height="64" rx="10" class="d-box"/>
<text x="70" y="47" text-anchor="middle" class="d-head">source</text>
<text x="70" y="67" text-anchor="middle" class="d-mono">.tynix</text>
<rect x="160" y="20" width="140" height="64" rx="10" class="d-accent"/>
<text x="230" y="47" text-anchor="middle" class="d-head">parse</text>
<text x="230" y="67" text-anchor="middle" class="d-muted">directives, Megaparsec</text>
<rect x="330" y="20" width="150" height="64" rx="10" class="d-accent"/>
<text x="405" y="47" text-anchor="middle" class="d-head">validate</text>
<text x="405" y="67" text-anchor="middle" class="d-muted">kinds, indexed shapes</text>
<rect x="510" y="20" width="150" height="64" rx="10" class="d-accent"/>
<text x="585" y="47" text-anchor="middle" class="d-head">check</text>
<text x="585" y="67" text-anchor="middle" class="d-muted">infer, constrain, unify</text>
<rect x="330" y="150" width="330" height="64" rx="10" class="d-box"/>
<text x="495" y="177" text-anchor="middle" class="d-head">declaration world</text>
<text x="495" y="197" text-anchor="middle" class="d-muted">workspace .d.tynix, declarationPacks, local declare blocks</text>
<rect x="700" y="8" width="250" height="44" rx="10" class="d-mint"/>
<text x="825" y="35" text-anchor="middle" class="d-text">compile: erase types → .nix</text>
<rect x="700" y="62" width="250" height="44" rx="10" class="d-mint"/>
<text x="825" y="89" text-anchor="middle" class="d-text">emit: public surface → .d.tynix</text>
<rect x="700" y="116" width="250" height="44" rx="10" class="d-amber"/>
<text x="825" y="143" text-anchor="middle" class="d-text">diagnostic: [Txxxxx] message</text>
<path d="M130 52h28" class="d-line" stroke-width="1.5" marker-end="url(#pipe-arrow)"/>
<path d="M300 52h28" class="d-line" stroke-width="1.5" marker-end="url(#pipe-arrow)"/>
<path d="M480 52h28" class="d-line" stroke-width="1.5" marker-end="url(#pipe-arrow)"/>
<path d="M495 150V86" class="d-line" stroke-width="1.5" marker-end="url(#pipe-arrow)"/>
<path d="M660 44 698 30" class="d-line" stroke-width="1.5" marker-end="url(#pipe-arrow)"/>
<path d="M660 52h38" class="d-line" stroke-width="1.5" marker-end="url(#pipe-arrow)"/>
<path d="M660 60 698 132" class="d-line" stroke-width="1.5" marker-end="url(#pipe-arrow)"/>
</svg>
<figcaption>Every command runs the same front half. <code>check</code> stops after checking, <code>compile</code> erases, <code>emit</code> renders declarations, and the language server keeps the analysis in memory.</figcaption>
</figure>

`Driver.analyzeText` is the single entry point used by the CLI, the language
server and the tests:

1. **Load the declaration world** for the file's workspace (cached per
   workspace root for the length of one CLI command or LSP request), on top
   of the built-in `builtins` prelude.
2. **Parse** the file (`Parser`, `ParserExpr`, `ParserType`, `ParserLexer`).
   Directive comments are scanned first, line by line, and attached to the next
   line of code.
3. **Validate kinds** of every alias and term-facing annotation (`Kind`).
4. **Validate indexed annotations** such as `Vec`, `Range` and `Unit`
   (`Indexed.validateProgramIndexedTypes`).
5. **Collect local `declare` blocks** and merge them with the world.
6. **Check** the root expression (`Check.checkProgram`).

The first error stops the pipeline. A file therefore reports **one diagnostic
at a time**; `check-project` reports one per file. Parser and checker errors
carry the source span they were raised at (see [diagnostics](#diagnostics)).

## Type representation

All phases share one data type, `Type` in
[`Type.hs`](https://github.com/ubugeeei-prod/tynix/blob/main/packages/tynix-core/src/Type.hs).
There is no separate elaborated IR: parsing, checking, hovering and emitting all
look at the same tree.

| Constructor | Surface syntax | Notes |
| --- | --- | --- |
| `TVar n` | `a`, `f` | lowercase identifiers in types |
| `TCon n` | `Int`, `List`, `Package` | uppercase identifiers: built-ins and aliases |
| `TLit l` | `"web"`, `8080`, `1.5`, `true` | singleton literal types |
| `TFun m a b` | `a -> b`, `a %1 -> b` | `m` is the multiplicity, `One` or `Many` |
| `TRecord fs` | `{ name :: String; }` | field map; closed for lookup, open for subtyping |
| `TOpenRecord fs r` | `{ name :: String; ... }`, `{ ...r }` | open record (row): known fields plus a tail. The tail is a meta during inference, a row variable after generalization, or `dynamic` for "unknown further fields" |
| `TOptional t` | `name? :: T` | marks an optional field; only meaningful as a field type |
| `TUnion ts` | `A \| B` | flattened and de-duplicated |
| `TApp f x` | `List Int`, `f a` | left-nested application, first-class for HKT |
| `TTypeList ts` | `[2 3 4]` | type-level list, used by `Tensor` shapes and `Tuple` |
| `TForall vs t` | `forall a b. t` | explicit quantification |
| `TConditional a b c d` | `a extends b ? c : d` | reduced structurally |
| `TInfer n` | `infer n` | only meaningful inside a conditional's pattern |
| `TDynamic`, `TUnknown`, `TAny` | `dynamic`, `unknown`, `any` | the gradual types |
| `TMeta i` | `?0` in messages | inference variable; never escapes a result |

Polymorphism outside annotations is stored as a `Scheme` (a list of quantified
variables plus a body). Results are made deterministic with `closeMetas`, which
renames leftover inference variables to `t0`, `t1`, ... in order of first
appearance.

Several "types" are encodings over `TCon` and `TApp` rather than constructors:

- `List a` is `TApp (TCon "List") a`, and the dictionary type `AttrsOf a` is
  `TApp (TCon "AttrsOf") a`.
- `Vec n a`, `Matrix r c a` and `Tensor [d1 d2 ...] a` are normalized by
  `Indexed.normalizeIndexedType` into one canonical tensor form, so all three
  spellings compare equal when they describe the same shape.
- `Tuple [a b c]` is a heterogeneous fixed-length list.
- `Range lo hi base` and `Unit "label" base` are numeric refinements and phantom
  units, interpreted by the subtyping relation.

## Inference

Inference lives in
[`Check.hs`](https://github.com/ubugeeei-prod/tynix/blob/main/packages/tynix-core/src/Check.hs).
It runs in a state monad holding a counter for fresh metas, a substitution
map from metas to types, and the set of **soft** metas (see below). `zonk`
applies the substitution, and `bindMeta` extends it after an **occurs check**
(failure is `TC0016`).

The algorithm is syntax-directed. The interesting rules:

| Expression | Rule |
| --- | --- |
| literal | its singleton type: `"web"`, `8080`, `true`; `null` is `Null`; a path, `<nixpkgs>` or interpolated path is `Path` |
| variable | instantiate its scheme with fresh metas; unbound is `TC0001` unless inside an open `with` scope. Global builtins share their type with the matching `builtins` member |
| `x: body` | fresh meta for `x` (or its annotation), infer `body`; multiplicity is `One` if `x` occurs exactly once syntactically, else `Many` |
| `{ a, b ? d, ... }@args: body` | a record with one **soft** meta per field (or its annotation); a field with a default is optional (`b? :: T`) and the widened type of `d` constrains it, except for `? null`; `...` makes the record open; `@args` binds the whole record |
| `f x` | infer `f`; `dynamic`/`any` callees give `dynamic`/`any`; a function constrains the argument against its domain; a soft meta becomes `dynamic -> dynamic` and the call is `dynamic`; another unknown callee is unified with `widen(arg) -> ?r`; a definitely non-callable type is `TC0018` |
| `import ./p` | the declared scheme for the resolved path, or `dynamic` |
| `e.a` | look the field up in the resolved type (see below) |
| `e.a.b or d` | the selection joined with `d`; if the path runs into a value of unknown shape, just `d` joined with a fresh meta, so no field becomes required |
| `e.${k}` | literal or literal-union keys select (and join) fields; `AttrsOf v` gives `v`; a `String` key gives `dynamic` |
| `e ? path` | `Bool`; dynamic keys must be strings |
| `{ ... }` | nested paths (`a.b = 1;`) are merged into records, `inherit (s) a` selects from `s`; only computed keys give `AttrsOf (join values)`, static plus computed keys give `{ fields...; ... }` |
| `if c then a else b` | `c` must be `Bool`; with unsolved metas the literal-widened branches are unified, otherwise the result is `join a b` |
| `[ ... ]` | shape inference, see [indexed types](#indexed-types) |
| `a // b` | record merge, right side wins; an unknown operand becomes an open row, so `x: x // { y = 1; }` is `{ ...t0 } -> { y :: 1; ...t0 }`; `TC0021` for non-records |
| `a ++ b` | `List (join elemA elemB)`; `TC0020` for non-lists |
| `+` | strings and paths concatenate (`Path + String` is a `Path`); otherwise numeric |
| `- * /`, prefix `-` | numeric family arithmetic (`Nat`, `Int`, `Float`, `Number`); `Nat - x` widens to `Int` |
| `< <= > >=` | both sides numeric or string; `TC0019` otherwise |
| `== !=` | always `Bool` |
| `&& \|\| -> !` | `Bool` operands, `Bool` result |
| `x \|> f`, `f <\| x` | application |
| `e as T` | `checkCast`, see [casts](#constrain-unify-and-cast) |
| `with s; body` | a known record scope adds its fields (lexical bindings win); any other scope makes unresolved names `dynamic` |

Field selection resolves the base type first. `any` and `dynamic` absorb the
selection; `unknown` refuses it (`TC0008`); records look the field up; an open
record with a `dynamic` tail gives `dynamic` for fields it does not list; a
union succeeds only if **every** member has the field, and joins the results.
Selecting from an **unsolved meta** binds it to an open record
`{ a :: ?f; ...?r }`, and selecting a new field from such a row extends the
tail. This is row polymorphism: `x: x.a + x.b` is
`{ a :: Number; b :: Number; ... } -> Number`. The field metas created this way
are soft.

### Soft metas

Some unknowns stand for values tynix cannot know but that are usually
polymorphic or overloaded in practice: arguments injected through an
attribute-set pattern (`callPackage`-style `{ lib, fetchFromGitHub, ... }:`)
and fields selected from values of unknown shape (`lib.mkOption`). Their metas
are marked **soft**. Calling a soft meta binds it to `dynamic -> dynamic`
instead of fixing it to the argument types of the first call site, so the next
call with different arguments does not fail. Plain lambda binders are not
soft and keep full principal types:
`compose = f: g: x: f (g x)` is
`forall t0 t1 t2. (t1 -> t2) %1 -> (t0 -> t1) %1 -> t0 %1 -> t2`.

### Literal widening

Singleton literal types are kept where they are useful and widened where they
would only cause spurious errors: the default of a pattern field (`b ? 2`
accepts any `Int`), the argument of a callee whose type is still unknown, and
`if` branches that are unified because the other side is unknown (so `true`
and `false` meet at `Bool`).

### `let` groups and generalization

A `let` block is checked in these phases:

1. Collect signatures. Duplicate signatures (`TC0003`), duplicate bindings
   (`TC0004`) and signatures without bindings (`TC0005`) are rejected. Nested
   paths are merged and `inherit (s) a` becomes a binding `a = s.a`. Dynamic
   names are rejected (`TC0022`).
2. Put every signed binding in scope with its signature's scheme. This is what
   allows polymorphic recursion through a signature.
3. Order the unsigned bindings by their free names and split them into
   **strongly connected components**. Each component is a group of mutually
   recursive bindings, processed in dependency order.
4. For each group, give its unsigned members fresh placeholder metas, infer
   each body, and `constrain` it against the placeholder.
5. **Generalize** each member over the metas that do not occur free in the
   environment *outside* the group. Metas shared with an enclosing lambda
   parameter stay monomorphic, as in Hindley-Milner.

Because every group is generalized before later groups see it, a helper is
polymorphic for the rest of the `let`:
`let id = x: x; in { a = id 1; b = id "s"; }` checks, and mutually recursive
`even` / `odd` solve without annotations.

**Signatures are rigid.** A signed binding's body is checked against the
signature with its quantified variables held abstract (skolemized): inside the
body, `a` is a type that only equals itself. `id :: forall a. a -> a;
id = x: 1;` is therefore rejected with `type mismatch: 1 vs a`. Other bindings
instantiate the signature freshly at every use.

### Directives

`# @tynix-ignore` and `# @tynix-expected` wrap the attempt to check one `let`
item or the root expression. On failure, the checker discards the attempt's
state and recovers by constraining `dynamic` against the expected type, so a
suppressed binding keeps its signature. `@tynix-expected` on code that checks
cleanly is itself an error (`TC0006`).

## Constrain, unify and cast

Three operations reconcile two types. They differ in direction and in how much
gradual slack they allow.

**`constrain actual expected`** is directional. It is used for signatures,
function arguments and recursive placeholders. In order:

1. A fixed-shape sequence (vector, tuple) meeting a plain `List` is compared
   through its list view.
2. Metas are bound. A meta passed where `unknown` or `AttrsOf unknown` is
   expected is not pinned to that top type (an unknown attribute set only
   becomes an open row).
3. Functions compare argument types contravariantly and results covariantly,
   and the actual multiplicity must be a sub-multiplicity of the expected one
   (`%1 ->` may stand in for `->`, not the reverse).
4. Records that still contain metas are compared field by field: every
   required expected field must be present, optional ones may be missing, and
   a missing field extends the actual record's row when its tail is still
   open. A record against `AttrsOf v` constrains the join of its field types
   against `v`.
5. Equal types, or `isSubtype actual expected`, succeed.
6. **Gradual consistency is used only when `dynamic` occurs somewhere in either
   type.** Two unrelated concrete types never pass by consistency.
7. If unsolved metas remain, fall back to `unify`.
8. Otherwise, for two records, the error names the first missing field
   (`TC0009`) or the first field whose type does not fit (`TC0013 type
   mismatch in field ...`); anything else is `TC0013 type mismatch`.

**`unify a b`** is symmetric and is used when both sides are partially unknown.
It binds metas, recurses through functions (same multiplicity), applications
and records (one record's fields must be a subset of the other's, else
`TC0014`), and accepts subtyping in either direction or consistency involving
`dynamic`, returning the join.

**`checkCast actual asserted`** is the most permissive. It succeeds if metas
can be unified, or if `actual <: asserted`, `asserted <: actual`, or the two are
consistent. So casts may widen, narrow, and cross any gradual boundary, but
`1 as String` is still `TC0015`.

## Subtyping and the gradual lattice

[`Subtyping.hs`](https://github.com/ubugeeei-prod/tynix/blob/main/packages/tynix-core/src/Subtyping.hs)
implements `isSubtype`, `isConsistent` and `joinTypes`. Both sides are fully
resolved first (aliases expanded, conditionals reduced, shapes normalized).

<figure class="tx-diagram">
<svg viewBox="0 0 880 330" xmlns="http://www.w3.org/2000/svg" role="img" aria-labelledby="lat-title">
<title id="lat-title">Where any, unknown and dynamic sit relative to ordinary types</title>
<defs><marker id="lat-arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0 0 10 5 0 10z" class="d-arrowhead"/></marker></defs>
<rect x="250" y="12" width="180" height="40" rx="10" class="d-amber"/>
<text x="340" y="37" text-anchor="middle" class="d-mono">any (top)</text>
<rect x="250" y="76" width="180" height="40" rx="10" class="d-accent"/>
<text x="340" y="101" text-anchor="middle" class="d-mono">unknown</text>
<rect x="60" y="150" width="130" height="40" rx="10" class="d-box"/>
<text x="125" y="175" text-anchor="middle" class="d-mono">Number</text>
<rect x="210" y="150" width="130" height="40" rx="10" class="d-box"/>
<text x="275" y="175" text-anchor="middle" class="d-mono">String</text>
<rect x="360" y="150" width="140" height="40" rx="10" class="d-box"/>
<text x="430" y="175" text-anchor="middle" class="d-mono">{ name :: String; }</text>
<rect x="60" y="214" width="130" height="40" rx="10" class="d-box"/>
<text x="125" y="239" text-anchor="middle" class="d-mono">Int / Nat / 8080</text>
<rect x="210" y="214" width="130" height="40" rx="10" class="d-box"/>
<text x="275" y="239" text-anchor="middle" class="d-mono">"web"</text>
<rect x="360" y="214" width="140" height="40" rx="10" class="d-box"/>
<text x="430" y="239" text-anchor="middle" class="d-mono">{ name; version; }</text>
<rect x="250" y="278" width="180" height="40" rx="10" class="d-amber"/>
<text x="340" y="303" text-anchor="middle" class="d-mono">any (bottom)</text>
<path d="M340 76V54" class="d-line" stroke-width="1.4" marker-end="url(#lat-arrow)"/>
<path d="M125 150 290 118" class="d-line" stroke-width="1.4" marker-end="url(#lat-arrow)"/>
<path d="M275 150 330 118" class="d-line" stroke-width="1.4" marker-end="url(#lat-arrow)"/>
<path d="M430 150 380 118" class="d-line" stroke-width="1.4" marker-end="url(#lat-arrow)"/>
<path d="M125 214v-22" class="d-line" stroke-width="1.4" marker-end="url(#lat-arrow)"/>
<path d="M275 214v-22" class="d-line" stroke-width="1.4" marker-end="url(#lat-arrow)"/>
<path d="M430 214v-22" class="d-line" stroke-width="1.4" marker-end="url(#lat-arrow)"/>
<path d="M300 278 140 256" class="d-line" stroke-width="1.4" marker-end="url(#lat-arrow)"/>
<path d="M330 278 285 256" class="d-line" stroke-width="1.4" marker-end="url(#lat-arrow)"/>
<path d="M380 278 420 256" class="d-line" stroke-width="1.4" marker-end="url(#lat-arrow)"/>
<rect x="640" y="130" width="200" height="56" rx="10" class="d-mint"/>
<text x="740" y="155" text-anchor="middle" class="d-mono">dynamic</text>
<text x="740" y="174" text-anchor="middle" class="d-muted">consistent with everything</text>
<path d="M640 158H520" class="d-line" stroke-width="1.4" stroke-dasharray="5 5"/>
<text x="580" y="148" text-anchor="middle" class="d-muted">~</text>
<text x="640" y="230" class="d-muted">every T &lt;: dynamic (sound)</text>
<text x="640" y="250" class="d-muted">dynamic &lt;: T only via consistency,</text>
<text x="640" y="268" class="d-muted">and only when dynamic is involved</text>
</svg>
<figcaption>Solid arrows point from subtype to supertype. <code>any</code> is both the top and the bottom of the subtype relation. <code>unknown</code> is the top of everything else. <code>dynamic</code> stands beside the lattice: everything is a subtype of it, and it flows back into precise types only through consistency.</figcaption>
</figure>

The rules, in the order the implementation tries them:

| Rule | Example |
| --- | --- |
| reflexivity | `T <: T` |
| `any` is top and bottom | `String <: any`, `any <: String` |
| everything is below `dynamic` | `Int <: dynamic` |
| `dynamic` is below nothing else | `dynamic <: String` is false (consistency handles it) |
| everything is below `unknown`; `unknown` is below nothing else | `"x" <: unknown` |
| literals sit below their constructor | `"web" <: String`, `8080 <: Int`, `true <: Bool` |
| non-negative integer literals are `Nat` | `3 <: Nat`, not `-1 <: Nat` |
| numeric tower | `Nat <: Int <: Number`, `Float <: Number` |
| unions | `A \| B <: C` iff both are; `A <: B \| C` iff either is |
| ranges | `Range 2 4 Nat <: Range 0 10 Nat`; a literal is in a range if within bounds |
| units | same label only: `Unit "ms" Nat` is not `<: Unit "s" Nat`; a bare literal may enter a unit |
| tuples and tensors | positional and axis-wise; any tensor is a subtype of its `List` view; an empty tensor accepts any element type |
| functions | contravariant argument, covariant result, `%1 ->` below `->` |
| records | **width subtyping**: every required expected field must exist and be a subtype; an expected optional field may be absent; a field that is optional in the actual type does not satisfy a required one |
| open records | an actual record with a `dynamic` tail may lack required fields (they are unknown, not absent) |
| dictionaries | a record is below `AttrsOf v` when every field is below `v` and it has no unknown further fields |
| applications | `F a <: G b` iff `F <: G` and `a <: b` (covariant) |

**Consistency** (`isConsistent`) is the gradual relation: two types are
consistent if either is `any` or `dynamic`, or if one is a subtype of the
other. The checker consults it only when `dynamic` actually appears in one of
the types, which keeps the escape hatch from blurring two concrete types.

**Joins** (`joinTypes`) compute the type of `if` branches, list elements and
union field lookups. `any` absorbs everything; same-label units join their
payloads; tuples join positionally; tensors of equal rank join axis by axis
(`Vec 2 Int` and `Vec 3 Int` join to `Vec (2 | 3) Int`); numeric families widen
(`Nat` and `Int` to `Int`, otherwise `Number`); a subtype joins to its supertype;
everything else becomes a flattened union.

## Kinds and higher-kinded types

[`Kind.hs`](https://github.com/ubugeeei-prod/tynix/blob/main/packages/tynix-core/src/Kind.hs)
infers kinds; there is no kind syntax. The kind language is `Type`, `k1 -> k2`
and kind metas.

- Built-in constructors have fixed kinds: `Int`, `String`, `Bool`, `Float`,
  `Number`, `Nat`, `Null` and `Path` are `Type`; `List` and `Tuple` are
  `Type -> Type`; `Vec`, `Tensor` and `Unit` take two arguments; `Matrix` and
  `Range` take three.
- Every alias gets a placeholder kind `k1 -> ... -> kn -> r` with fresh metas.
  Its body is inferred with the parameters bound to `k1 ... kn`, and the body's
  kind is unified with `r`. Because all aliases get placeholders first, aliases
  may refer to each other in any order.
- Application `f x` unifies `kind(f)` with `kind(x) -> ?r`.
- Arguments of `->`, record fields, union members and type-list items must have
  kind `Type`.
- A constructor that is neither built-in nor an alias (for example one only
  mentioned in a declaration pack you have not loaded) gets a flexible kind, so
  partial declaration packs do not fail kind checking.
- Every term-facing annotation (signatures, lambda annotations, casts, ambient
  entries) must have kind `Type` in the end, else `TK0003`.

Kind errors are `TK0001` (mismatch) and `TK0002` (occurs check). This is what
rejects `Int String` and `Twice List` while accepting `Functor List`.

Aliases are expanded by `Alias.expandAliases`. An application is reduced when
the head is an alias and at least as many arguments as parameters are present;
extra arguments are re-applied to the expansion, which is what lets an alias
return a type constructor. Expansion is bounded by a budget of 32 chained
expansions per path to keep self-referential aliases from looping.

## Conditional types and `infer`

`Subtyping.resolveType` normalizes a type before comparison: it erases a
top-level `forall`, expands aliases, normalizes shapes, and reduces conditional
types.

<figure class="tx-diagram">
<svg viewBox="0 0 900 220" xmlns="http://www.w3.org/2000/svg" role="img" aria-labelledby="cond-title">
<title id="cond-title">How a conditional type is reduced</title>
<defs><marker id="cond-arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0 0 10 5 0 10z" class="d-arrowhead"/></marker></defs>
<rect x="10" y="80" width="190" height="56" rx="10" class="d-box"/>
<text x="105" y="105" text-anchor="middle" class="d-mono">A extends P ? C : D</text>
<text x="105" y="124" text-anchor="middle" class="d-muted">resolve A and P first</text>
<rect x="250" y="80" width="190" height="56" rx="10" class="d-accent"/>
<text x="345" y="105" text-anchor="middle" class="d-head">matchPattern A P</text>
<text x="345" y="124" text-anchor="middle" class="d-muted">binds each infer x</text>
<rect x="500" y="12" width="380" height="56" rx="10" class="d-mint"/>
<text x="690" y="37" text-anchor="middle" class="d-text">match: substitute bindings into C, resolve again</text>
<text x="690" y="56" text-anchor="middle" class="d-muted">(re-expands aliases introduced by C)</text>
<rect x="500" y="98" width="190" height="56" rx="10" class="d-accent"/>
<text x="595" y="123" text-anchor="middle" class="d-head">A &lt;: P ?</text>
<text x="595" y="142" text-anchor="middle" class="d-muted">ordinary subtyping</text>
<rect x="730" y="98" width="150" height="40" rx="10" class="d-mint"/>
<text x="805" y="123" text-anchor="middle" class="d-text">yes: C</text>
<rect x="730" y="164" width="150" height="40" rx="10" class="d-amber"/>
<text x="805" y="189" text-anchor="middle" class="d-text">no: D</text>
<path d="M200 108h48" class="d-line" stroke-width="1.4" marker-end="url(#cond-arrow)"/>
<path d="M440 96 498 46" class="d-line" stroke-width="1.4" marker-end="url(#cond-arrow)"/>
<path d="M440 120 498 124" class="d-line" stroke-width="1.4" marker-end="url(#cond-arrow)"/>
<path d="M690 120h38" class="d-line" stroke-width="1.4" marker-end="url(#cond-arrow)"/>
<path d="M690 134 728 180" class="d-line" stroke-width="1.4" marker-end="url(#cond-arrow)"/>
<text x="470" y="64" class="d-muted">ok</text>
<text x="452" y="138" class="d-muted">no match</text>
</svg>
<figcaption>Reduction is bounded by a budget of 32 chained conditional reductions.</figcaption>
</figure>

`Alias.matchPattern` walks the checked type and the pattern together:

- `infer x` binds `x` to whatever is at that position. A second `infer x` must
  see an identical type.
- Functions match if their multiplicities are **identical** and both sides
  match.
- Records match field by field over the **pattern's** fields; extra fields in
  the checked type are ignored.
- Applications and type lists match component-wise.
- Anything else matches only if it is syntactically equal.

If matching fails, the conditional falls back to `isSubtype A P`. Consequences
worth knowing:

- There is **no distribution over unions**: a union is matched as a whole.
- A pattern written with `->` does not match a `%1 ->` type.
- A conditional alias that recurses into itself is expanded eagerly and hits
  the budget; keep conditional aliases non-recursive.
- Output and hovers print the alias as written (`ElementOf (List String)`);
  comparisons use the reduced form.

## Indexed types

[`Indexed.hs`](https://github.com/ubugeeei-prod/tynix/blob/main/packages/tynix-core/src/Indexed.hs)
has two jobs: inferring precise shapes from list literals, and validating
shape annotations before the checker trusts them.

Shape inference for a list literal:

| Literal | Inferred type | Rule |
| --- | --- | --- |
| `[ ]` | `Vec 0 dynamic` | no evidence for an element type |
| `[ 1 2 ]` | `Vec 2 (1 \| 2)` | homogeneous scalars become a vector of the joined element |
| `[ 1 "x" ]` | `Tuple [ 1 "x" ]` | heterogeneous scalars become a tuple |
| `[ [ 1 2 ] [ 3 4 ] ]` | `Matrix 2 2 (1 \| 2 \| 3 \| 4)` | equal-shape tensors gain an outer axis |
| `[ [ 1 ] [ 2 3 ] ]` | `List (Vec (1 \| 2) (1 \| 2 \| 3))` | ragged nesting widens to `List` |

Every tensor also has a **list view** (`Vec 3 Int` views as `List Int`,
`Matrix 2 3 Int` as `List (Vec 3 Int)`), which is how fixed shapes flow into
code that only asks for `List`.

Validation rejects annotations that cannot mean anything: tensor dimensions
that are not natural-number-like, `Range` bounds that are not numeric or are
reversed (`Range 4 2 Nat`), fractional bounds on a `Nat` range, and `Unit`
labels that are not string literals.

## Declarations and the ambient world

`Driver` builds the set of types that `import` can see.

<figure class="tx-diagram">
<svg viewBox="0 0 900 200" xmlns="http://www.w3.org/2000/svg" role="img" aria-labelledby="decl-title">
<title id="decl-title">How tynix assembles declarations for a source file</title>
<defs><marker id="decl-arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0 0 10 5 0 10z" class="d-arrowhead"/></marker></defs>
<rect x="10" y="20" width="200" height="56" rx="10" class="d-box"/>
<text x="110" y="45" text-anchor="middle" class="d-head">find workspace root</text>
<text x="110" y="64" text-anchor="middle" class="d-muted">.git, flake.nix, tynix.config.tynix, ...</text>
<rect x="250" y="20" width="200" height="56" rx="10" class="d-box"/>
<text x="350" y="45" text-anchor="middle" class="d-head">walk for *.d.tynix</text>
<text x="350" y="64" text-anchor="middle" class="d-muted">skips nested workspaces</text>
<rect x="250" y="120" width="200" height="56" rx="10" class="d-box"/>
<text x="350" y="145" text-anchor="middle" class="d-head">declarationPacks</text>
<text x="350" y="164" text-anchor="middle" class="d-muted">from tynix.config.tynix</text>
<rect x="490" y="70" width="190" height="56" rx="10" class="d-accent"/>
<text x="585" y="95" text-anchor="middle" class="d-head">merge worlds</text>
<text x="585" y="114" text-anchor="middle" class="d-muted">aliases + path → scheme</text>
<rect x="720" y="20" width="170" height="56" rx="10" class="d-mint"/>
<text x="805" y="45" text-anchor="middle" class="d-mono">import ./x.nix</text>
<text x="805" y="64" text-anchor="middle" class="d-muted">declared scheme</text>
<rect x="720" y="120" width="170" height="56" rx="10" class="d-amber"/>
<text x="805" y="145" text-anchor="middle" class="d-mono">no declaration</text>
<text x="805" y="164" text-anchor="middle" class="d-muted">dynamic</text>
<path d="M210 48h38" class="d-line" stroke-width="1.4" marker-end="url(#decl-arrow)"/>
<path d="M450 48 488 86" class="d-line" stroke-width="1.4" marker-end="url(#decl-arrow)"/>
<path d="M450 148 488 112" class="d-line" stroke-width="1.4" marker-end="url(#decl-arrow)"/>
<path d="M680 90 718 54" class="d-line" stroke-width="1.4" marker-end="url(#decl-arrow)"/>
<path d="M680 106 718 142" class="d-line" stroke-width="1.4" marker-end="url(#decl-arrow)"/>
</svg>
<figcaption>Local <code>declare</code> blocks in the file being checked are added to the merged world.</figcaption>
</figure>

- The **built-in prelude** is the base of every world. It is
  [`registry/workspace/builtins.d.tynix`](https://github.com/ubugeeei-prod/tynix/blob/main/registry/workspace/builtins.d.tynix),
  embedded into every binary (as the generated `BuiltinPrelude.hs`), and it
  declares `builtins` plus aliases such as `Derivation`, `DerivationArgs`,
  `FetchedSource`, `PathLike`, `FileType`, `TypeName` and `NameValuePair`.
  Project aliases with the same name win, and a workspace `declare "builtins"`
  replaces the prelude's.
- The **workspace root** is the nearest ancestor of the source file that
  contains `flake.nix`, `cabal.project`, `pnpm-workspace.yaml`,
  `tynix.config.tynix` or a `.git` directory. Without one, the file's own
  directory is used, and only the `.d.tynix` files directly in it are loaded.
- In a real workspace, every `.d.tynix` under the root is loaded, except inside
  nested directories that are themselves workspaces. Hidden directories,
  `node_modules`, `dist-newstyle`, `dist`, `target`, `result` and `result-*`
  build links, and symlinked directories are skipped. A declaration file never
  contributes to its own analysis, and it must not contain an expression
  (`TD0007`).
- `declarationPacks` in `tynix.config.tynix` add files or directories. Packs that
  live under a `registry/workspace/` directory are **rebased** onto your project
  root, so their relative targets (`../../flake.nix`) point at your files.
- Each `declare` target is resolved relative to the file that contains it,
  normalizing `.` and `..`. The target `"builtins"` is special and types the
  `builtins` identifier.
- A block with exactly one entry named `default` declares the module's whole
  value; any other block declares an attribute set of its entries.
- Each target may be declared once per world, else `TD0002`; duplicate entry
  names are `TD0003`.
- Aliases from all loaded declaration files share one namespace with the
  file's own aliases. Avoid defining the same alias name twice.

`import` is typed only when its argument is a path literal or a string literal;
the resolved absolute path is looked up in the world. Any other import, and any
path without a declaration, is `dynamic`. `builtins` is the record declared by
the prelude (or by the workspace's own `declare "builtins"`), and the global
builtins such as `toString`, `map`, `throw`, `import`, `derivation`,
`baseNameOf`, `dirOf`, `fetchTarball`, `isNull`, `removeAttrs` and
`placeholder` take the type of the matching member.

## Erasure and compilation

[`Compile.hs`](https://github.com/ubugeeei-prod/tynix/blob/main/packages/tynix-core/src/Compile.hs)
does not translate anything; it deletes:

- `type` aliases and `declare` blocks,
- `let` signatures (`name :: Type;`),
- lambda annotations (`(x :: Int):` becomes `x:`) and pattern-field
  annotations (`{ name :: String }:` becomes `{ name }:`),
- casts (`e as T` becomes `e`).

The remaining tree is pretty-printed as Nix. Names, structure, string forms
(double-quoted or indented, including their escapes), nested attribute paths
and operators are preserved; whitespace is normalized and comments are not
carried over. Over a sample of 4000 nixpkgs files, the output parses to the
same AST as the input under `nix-instantiate --parse`. Because the compiler only
deletes, the generated `.nix` evaluates exactly like the `.tynix` would if Nix
ignored the type syntax. `tynix compile` runs the full analysis first and
refuses to emit output for a file that does not check.

**Declaration emit** (`Emit.hs`) renders the file's aliases verbatim and a
`declare` block for the compiled `.nix` path, relative to where the
declaration is written. If the root expression is an attribute set (or
`rec { }`, or such a set behind casts) and its type is monomorphic, each field
becomes an entry. Otherwise the whole root becomes `default`, quantified with
`forall` if it is polymorphic.

## Diagnostics

[`Diagnostics.hs`](https://github.com/ubugeeei-prod/tynix/blob/main/packages/tynix-core/src/Diagnostics.hs)
assigns every message a stable code, prefixed by phase: `TP` parser, `TK` kind
checker, `TC` type checker, `TD` driver. The message format is
`[CODE] text`, prefixed with `line:col:` (and a space) when the error has a source span, and
codes are never reused. The full catalogue with fixes is in
[diagnostics](../diagnostics.md).

- **Parse errors** carry a line and column (`3:12: [TP0004] ...`) and the
  Megaparsec excerpt.
- **Checker errors** carry the span of the innermost expression whose
  inference failed: every parsed expression is wrapped in a located node, and
  a failure is attributed to the nearest enclosing one. The CLI prints the
  start as `line:col: [CODE] message`; the language server underlines the
  whole span. A call whose argument does not fit points at the argument, and
  a signature mismatch at the binding's body. A few errors raised before any
  expression is inferred, such as `TC0022`, have no span.
- The CLI prints text diagnostics to standard error and exits `1`. With
  `--format json`, a structured report goes to standard output instead,
  versioned by `schemaVersion`; see the [CLI reference](./cli.md#json-output).

## Known limitations

These follow directly from the design above and are good to keep in mind:

- One diagnostic per file per run; fix and re-run to see the next.
- Implementing records whose fields have their own `forall` is not accepted
  yet; declare such values instead.
- Unions are not narrowed by `if` conditions: `isAttrs x`, `x ? a` and `_tag`
  checks do not refine `x` in the branches.
- Constraint contexts (`Functor f =>`) are parsed but not enforced; there are
  no type classes.
- Soft metas trade precision for adoption: calls through unannotated injected
  dependencies are not checked. Annotate the pattern field to check them.
- NixOS modules are typed as ordinary functions; the `config` / `options`
  fixpoint is not modelled.
