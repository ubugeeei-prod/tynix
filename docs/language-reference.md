# Language Reference

## Overview

`tynix` keeps Nix value syntax and adds type-only syntax for checking and
tooling.

The runtime model is simple:

- parse `.tynix`
- type-check it
- erase type syntax
- emit ordinary `.nix`

## Files

- `.tynix`: implementation files with value-level Nix syntax plus types
- `.d.tynix`: declaration-only files that describe existing `.nix` surfaces
- `.nix`: erased runtime output

## Nix Compatibility

The expression grammar covers the whole Nix language. Every construct the
reference implementation accepts also parses as `.tynix`, and erasing a file
without type syntax gives back the same program: over a sample of 4000 nixpkgs
files, the compiled output parses to an AST identical to the source's under
`nix-instantiate --parse` (up to how equal strings are split into segments).

Only Nix keywords are reserved in expressions: `if`, `then`, `else`, `let`,
`in`, `rec`, `with`, `assert`, `inherit`, `or`, plus `true`, `false` and `null`.
tynix's own keywords are reserved only inside types, so `type`, `any`,
`import`, `declare`, `forall` and friends are ordinary names in expressions.
`as` is contextual: it is an ordinary binder and variable (`as: as.x` works),
and `e as T` is a cast only where a type can follow.

Unquoted URIs (`https://example.org/x.tar.gz`), which Nix itself deprecates,
are accepted in argument position. Quote them anywhere else, since a bare
`name:` at the start of an expression is read as a lambda.

## Expressions

### Literals

```tynix
{
  string = "hello";
  indented = ''
    echo "$out"
  '';
  int = 1;
  float = 1.5;
  negative = -3;
  bool = true;
  nothing = null;
  relative = ../path.nix;
  home = ~/.config;
  searchPath = <nixpkgs>;
  interpolated = ./${"default"}.nix;
}
```

Strings support `${...}` interpolation and Nix's escapes, including `$$` and
`''$` for a literal dollar sign. Paths may be interpolated too.

### Variables and builtins

```tynix
let
  value = 1;
in {
  inherit value;
  text = toString value;
  count = builtins.length [ 1 2 ];
}
```

`builtins` and the globals Nix exposes without the prefix (`toString`, `map`,
`throw`, `abort`, `import`, `derivation`, `baseNameOf`, `dirOf`,
`fetchTarball`, `isNull`, `removeAttrs`, `placeholder`, ...) are in scope and
typed by the built-in prelude. See [builtins and the registry](./reference/builtins.md).

### Lambdas

```tynix
{
  plain = x: x;
  annotated = (x :: Int): x;
  pattern = { a, b ? 2, ... }: a + b;
  named = { a, ... }@args: args;
  namedFirst = args@{ a, ... }: a;
  typedFields = { name :: String, version ? "1" }: "${name}-${version}";
}
```

Attribute-set patterns accept defaults (`b ? 2`), `...`, and an `@` binder on
either side. A pattern field may carry an annotation (`name :: String`), which
is erased like every other type. A field with a default makes that argument
optional: the pattern above has type `{ a :: Int; b? :: Int; ... } -> Int`.

### Application

```tynix
let
  f = x: x + 1;
  xs = [ { name = "a"; } ];
in {
  one = f 1;
  names = map (x: x.name) xs;
}
```

### `let`

```tynix
let
  value :: Int;
  value = 1;

  meta.license = "MIT";
  meta.homepage = "https://example.org";

  source = { pname = "hello"; version = "1"; };
  inherit (source) pname version;
in "${pname}-${version}-${meta.license}"
```

A `let` item is a signature (`name :: Type;`), a binding, a nested binding
path (`meta.license = ...;`), or `inherit` / `inherit (source)`. Dynamic
attribute names are not allowed in `let`, as in Nix (`TC0022`).

`let` items are also where the diagnostic directives are most useful:

```tynix
let
  # @tynix-expected
  value :: Int;
  value = "oops";
in value
```

### Attribute Sets

```tynix
let
  name = "tynix";
  key = "computed";
  src = { url = "https://example.org"; rev = "abc"; };
in {
  inherit name;
  inherit (src) url rev;
  version = "0.1.0";
  nested.enabled = true;
  nested.level = 2;
  "quoted-name" = 1;
  ${key} = "value";
}
```

Nested paths are merged into one record, exactly as Nix does. A computed key
(`${key} = ...;`) makes the result an open record (`{ name :: ...; ... }`), or an
`AttrsOf T` dictionary when every key is computed. `rec { ... }` lets fields
refer to each other.

### Selection, defaults and presence tests

```tynix
let
  cfg = { server = { port = 80; }; };
  key = "server";
in {
  port = cfg.server.port;
  dynamic = cfg.${key}.port;
  host = cfg.server.host or "localhost";
  hasTls = cfg ? server.tls;
  hasKey = cfg ? ${key};
}
```

`x.a.b or default` never requires `a` or `b` to exist, and `?` always yields
`Bool`.

### Lists

```tynix
[ 1 2 3 ]
```

Nested lists infer shapes such as `Matrix 2 2 Int`; see
[list shape inference](#list-shape-inference).

### Operators

```tynix
let
  n = 7;
in {
  arithmetic = n + 1 - 2 * 3 / 4;
  negated = -n;
  text = "a" + "b";
  path = ./dir + "/file.nix";
  lists = [ 1 ] ++ [ 2 ];
  merged = { a = 1; } // { b = 2; };
  logic = !(n > 1) || (n <= 9 && n != 3);
  implication = true -> false;
  piped = [ 1 2 3 ] |> builtins.length;
  pipedLeft = builtins.length <| [ 1 2 ];
}
```

`+` adds numbers and concatenates strings and paths (a path plus a string is a
`Path`). Precedence and associativity follow Nix; see the
[grammar](./grammar.md#operator-precedence-and-associativity).

### Conditionals, `with` and `assert`

```tynix
let
  cond = true;
  lib = { version = "1"; };
in
assert cond;
with lib;
if cond then version else "none"
```

### Import

```tynix
declare "./lib.nix" { default :: { value :: Int; }; };

{
  lib = import ./lib.nix;
  value = (import ./lib.nix).value;
  untyped = import ./other.nix;
}
```

`import` of a path with a declaration has the declared type; anything else is
`dynamic`.

## Type Annotations

Signatures go next to `let` bindings, annotations on lambda binders and pattern
fields:

```tynix
let
  name :: String;
  name = "x";

  f :: Int -> Int;
  f = x: x + 1;

  g = (x :: String): x;
  h = { port :: Int, host ? "localhost" }: "${host}:${toString port}";
in { inherit name f g h; }
```

A `forall` signature is **rigid**: the body must work for every instantiation
of its type variables, so `id :: forall a. a -> a; id = x: 1;` is rejected
with `type mismatch: 1 vs a`.

A type may carry a Haskell-style constraint context such as `Functor f =>` or
`(Eq a, Show a) =>`, at the start of the type or right after its `forall`
(`forall f a b. Functor f => (a -> b) -> f a -> f b`). Contexts are parsed for
documentation but not enforced, because tynix has no type classes yet.

## Type Ascriptions

```tynix
(builtins.fromJSON text :: { port :: Int; })
```

`(e :: T)` checks that `e` has type `T`. Unlike a cast, it never narrows. It
is erased.

## Macros

```tynix
macro enum {
  ( $( $tag:ident ),* ) => ({ $( $tag = stringify!($tag); )* });
};

enum!(red, green)
```

A macro is a list of `(pattern) => (template);` rules, declared before the
root expression and invoked as `name!( ... )`. Expansion is hygienic and every
template is type-checked at its definition. See the
[macros tutorial](./tutorial/macros.md) and the
[reference](./reference/advanced-types.md#macros).

## Casts

```tynix
let
  value :: unknown;
  value = 1;
in value as Int
```

`as` is a value-level cast. It changes the static type of the expression when
the source and target overlap in one of the ways `tynix` allows:

- normal widening (`1 as Number`)
- structural narrowing (`value as { name :: String; }`)
- explicit gradual assertions across `any`, `unknown`, or `dynamic`

Concrete unrelated casts such as `1 as String` are rejected (`TC0015`).

## Diagnostic Directives

`tynix` recognizes two line-comment directives modeled after TypeScript.

### `# @tynix-ignore`

Suppress the next root-expression or `let`-item checker failure.

```tynix
let
  # @tynix-ignore
  value = missing;
in value
```

### `# @tynix-expected`

Suppress the next failure, but raise an error if that line does not fail.

```tynix
# @tynix-expected
missing
```

This is useful for regression tests and documentation examples where a failure
is the expected outcome.

## Type Forms

The snippets in this section are types; write them after `::`, in `type`
aliases, or in `declare` blocks.

### Primitive Constructors

```tynix
String
Int
Float
Number
Nat
Bool
Path
Null
any
dynamic
unknown
```

### Literal Singleton Types

```tynix
"tynix"
1
1.5
true
false
```

### Function Types

```tynix
Int -> Int
String -> { name :: String; }
Int %1 -> Int
String -> String ! { Trace }
forall a e. (a -> a ! e) -> a -> a ! e
(String -> String ! { Fetch }) -> String ->{fetch} String ! { Fetch }
forall a. (n :: Nat) -> a -> Vec n a
```

- `%1 ->` is the **linear** arrow: the function consumes its argument exactly
  once. It is inferred for lambdas that do, and may be used wherever `->` is
  expected.
- `! { ... }` after the result lists the **effects** a full application may
  perform (`Trace`, `Throw`, `Abort`, `Read`, `Fetch`, `Store`, `Impure`).
  `! {}` means pure, `! e` and `! { Trace | e }` are effect-polymorphic, and an
  arrow without `!` does not track effects.
- `->{a, b}` is a **capture set**: the effectful capabilities a closure may
  hold. `->{}` holds none.
- `(n :: Nat) -> B` is a **dependent** arrow: `B` may mention `n`, which stands
  for the argument's precise type at each call.

See [Effects, Linearity & Macros](./reference/advanced-types.md) for the rules.

### Record Types

```tynix
{ name :: String; version :: String; }
{ name :: String; version? :: String; }
{ name :: String; ... }
{ name :: String; ...rest }
```

- A closed record lists its fields; values may still have more fields (width
  subtyping).
- `name? :: T` is an **optional field**: it may be absent, and when present it
  has type `T`.
- `...` makes the record **open**: "these fields, plus possibly others". A named
  tail such as `...rest` is a row variable, so a signature can say that a
  function returns the same other fields it received:
  `forall r. { ...r } -> { tag :: String; ...r }`.

### Dictionaries

```tynix
AttrsOf Int
AttrsOf (List String)
```

`AttrsOf T` is an attribute set with arbitrary names whose values all have type
`T`. A record is a subtype of `AttrsOf T` when every field fits `T`, so
`{ a = 1; b = 2; }` can be passed to `builtins.mapAttrs` or
`builtins.attrValues`.

### Union Types

```tynix
String | Int
{ ok :: true; value :: a; } | { ok :: false; error :: e; }
```

### Parametric Polymorphism

```tynix
forall a. a -> a
forall f a. f a -> f a
forall f a b. Functor f => (a -> b) -> f a -> f b
```

### Type Aliases

```tynix
type Box a = { value :: a; };
type Pair = Tuple [Int String];
type Fix (f :: Type -> Type) = f (Fix f);
opaque type StorePath = String;
opaque type Id t = String;
```

A parameter may carry a kind annotation. An `opaque type` is **nominal**: it is
distinct from every other type, including its representation, and its
parameters are invariant, so a phantom parameter such as `t` in `Id t` keeps
`Id User` and `Id Package` apart. Convert with `as` in either direction.

### Type-Level Operators

```tynix
Get { port :: Int; } "port"   # Int
KeyOf { a :: Int; b :: Int; } # "a" | "b"
Length (Vec 3 Int)            # 3
Add 2 3                       # 5
```

These reduce once their arguments are known, which is what makes dependent
signatures such as `builtins.getAttr :: forall r. (k :: String) -> r -> Get r k`
precise.

### Higher-Kinded Application

```tynix
type Apply f a = f a;
type Id f = f;
type ListOfInt = Apply (Id List) Int;
```

### Conditional Types

```tynix
type Element t = t extends List (infer a) ? a : t;
type ReturnOf f = f extends (infer a -> infer r) ? r : dynamic;
```

### Tuple Types

`Tuple` is the heterogeneous fixed-length sequence form.

```tynix
Tuple [Int String]
Tuple [1 "x" true]
```

### Indexed Containers

```tynix
Vec 3 Int
Matrix 2 4 Float
Tensor [2 3 4] Number
Vec (2 | 3 | Range 4 8 Nat) Int
Tensor [2 (Range 1 2 Nat) 1] Int
```

### Numeric Validators

```tynix
Nat
Range 0 10 Int
Range 0 5000 Nat
Range 0.0 1.0 Float
```

A numeric literal is a subtype of a validator when it satisfies it: `3` fits
`Range 0 10 Nat`, `0.5` fits `Range 0.0 1.0 Float`, and `11` does not fit
`Range 0 10 Nat`.

### Units

```tynix
Unit "ms" Nat
Unit "ms" (Range 0 5000 Nat)
Unit "MiB" Int
```

A bare literal may enter a unit (`1` fits `Unit "ms" Nat`), but different
labels never mix: `Unit "ms" Nat` is not a subtype of `Unit "s" Nat`.

## Declarations

Ambient declarations attach types to existing runtime files.

```tynix
declare "./legacy/default.nix" {
  value :: Int;
  mkPkg :: { name :: String; } -> Derivation;
};
```

A block whose only member is `default` declares the module's whole value; any
other block declares an attribute set of its members. The target `"builtins"`
replaces the built-in prelude for the whole workspace.

### Bundled Registry Packs

The repository also ships curated `.d.tynix` packs under `registry/`. These
packs are split into two groups:

- `registry/workspace/` for root-adjacent files such as `builtins`,
  `flake.nix`, and `tynix.config.tynix`
- `registry/ecosystem/` for alias-oriented ecosystem packs reused from local
  `declare` blocks

Current packs include:

- `NixpkgsLib`, `NixpkgsPkgs`, and related aliases for `nixpkgs`
- `NixFlakeUtilsFlake`, `HomeManagerFlake`, `NixDarwinFlake`, `FlakePartsLib`
- `DevenvFlake`, `TreefmtNixFlake`, `PreCommitHooksFlake`, `CraneFlake`
- `DeployRsFlake`, `NixvimFlake`, `SopsNixFlake`, `AgenixFlake`, `DiskoFlake`, `ColmenaFlake`

Example:

```tynix
declare "./nixpkgs.nix" { default :: NixpkgsImport; };
declare "./home-manager.nix" { default :: HomeManagerFlake; };
declare "./treefmt-nix.nix" { default :: TreefmtNixFlake; };
```

You can also register external pack paths from `tynix.config.tynix`:

```tynix
{
  declarationPacks = [
    ../vendor/tynix/registry/ecosystem
    ../vendor/tynix/registry/workspace
  ];
}
```

Configured `registry/workspace/` packs are resolved against the current project
root, so upstream workspace declarations still describe your local
`tynix.config.tynix` and `flake.nix`.

The checker resolves those aliases exactly like aliases written in your own
`.d.tynix` files.

## Inference Notes

### Let-polymorphism

Unannotated `let` bindings get principal polymorphic types. Bindings are
grouped by their dependencies, each group of mutually recursive bindings is
inferred together, and the group is generalized before later groups use it:

```tynix
let
  id = x: x;
  compose = f: g: x: f (g x);
in {
  n = id 1;
  s = id "one";
  inc = compose (x: x + 1) (x: x * 2);
}
```

Here `compose` is inferred as
`forall t0 t1 t2. (t1 -> t2) %1 -> (t0 -> t1) %1 -> t0 %1 -> t2`.

### Records and rows

Selecting from a value whose type is not known yet infers an open record:
`x: x.a + x.b` has type `{ a :: Number; b :: Number; ... } -> Number`, and
`x: x // { y = 1; }` has type `forall t0. { ...t0 } %1 -> { y :: 1; ...t0 }`.
Pattern fields with defaults become optional fields. `x.a or d` never makes
`a` required.

Width subtyping still applies to closed records: a value with more fields fits
a record type with fewer.

```tynix
let
  pkg :: { name :: String; };
  pkg = { name = "a"; version = "1"; };
in pkg
```

### Gradual Compatibility

The three gradual escape hatches have different roles:

- `any` is assignable to and from every type.
- `unknown` is a top type. Every value can be viewed as `unknown`, but `unknown` does not flow back into concrete types without an annotation or narrowing.
- `dynamic` keeps the existing tynix gradual-consistency behavior. It is consistent with every type, but not a concrete subtype of every type.

Unannotated code is accepted gradually where strict inference would produce
false errors:

- Dependencies injected through an attribute-set pattern
  (`{ lib, fetchFromGitHub, ... }:`) and fields selected from values of unknown
  shape are *soft*: calling one gives `dynamic -> dynamic` instead of fixing
  its type from the first call site. Plain lambda binders keep full
  Hindley-Milner principal types.
- Literals are widened where precision would only cause errors: in pattern
  defaults (`b ? 2` accepts any `Int`), in arguments to callees whose type is
  unknown, and in `if` branches whose other side is still unknown.

### List Shape Inference

```tynix
{
  empty = [];
  pair = [1 2];
  mixed = [1 "x"];
  grid = [[1 2] [3 4]];
  ragged = [[1] [2 3]];
}
```

```text
root: {
  empty :: Vec 0 dynamic;
  grid :: Matrix 2 2 (1 | 2 | 3 | 4);
  mixed :: Tuple [ 1 "x" ];
  pair :: Vec 2 (1 | 2);
  ragged :: List (Vec (1 | 2) (1 | 2 | 3));
}
```

## Erasure

The following syntax is erased during `.tynix -> .nix` compilation:

- `::` annotations on `let` bindings, lambda binders and pattern fields
- `as` casts
- `type` aliases
- `declare` blocks

Everything else is value-level Nix and is kept, with the same structure and
names.

## Common Patterns

### Typing A Legacy Import

```tynix
declare "./lib.nix" {
  default :: { value :: Int; };
};

(import ./lib.nix).value
```

### Narrowing A Gradual Boundary

```tynix
let
  payload = import ./opaque.nix;
in payload as { value :: String; }
```

### A Typed `callPackage` Function

```tynix
{ name :: String, version ? "1.0", doCheck ? true }:
{
  pname = name;
  inherit version doCheck;
}
```

### Bounded Sequence Contracts

```tynix
let
  xs :: Vec (Range 2 4 Nat) Int;
  xs = [1 2 3];
in xs
```

### Unit-Safe Numeric Contracts

```tynix
let
  timeout :: Unit "ms" (Range 0 5000 Nat);
  timeout = 2500;
in timeout
```

## Related Docs

- [Getting Started](./getting-started.md)
- [Grammar](./grammar.md)
- [Type System](./type-system.md)
- [How Checking Works](./reference/type-system-internals.md)
- [Language Design](./language-design.md)
