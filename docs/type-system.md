# Type System

## Direction

The `tynix` type system is a blend of Haskell and TypeScript. Its inference style is primarily Haskell-like, while its adoption model and tooling philosophy are TypeScript-like. The top priority is not maximal strictness. It is incremental adoption in existing Nix codebases.

## Core Features

### 1. `dynamic`

Untyped code and external boundaries are represented as `dynamic`. It is close to `unknown`, but not a full top type. It receives special treatment in consistency checking rather than replacing the subtype lattice.

### 2. `any`

`any` is the fully unsound escape hatch. It is assignable to and from every
type and is meant for the places where users explicitly want TypeScript-like
"just let this through" behavior.

### 3. `unknown`

`unknown` is a top type. Any value can be treated as `unknown`, but `unknown`
does not subtype concrete types without an explicit annotation or narrowing.

### 4. Explicit casts with `as`

`tynix` supports TypeScript-style `expr as Type` assertions. Casts are still
checked: they succeed for ordinary widening, structural narrowing, and explicit
crossings of gradual boundaries such as `any`, `unknown`, or `dynamic`.
Concrete unrelated casts remain errors.

### 5. Structural subtyping and rows

Attribute sets are compared structurally rather than nominally.

```tynix
{ name :: String; version :: String; } <: { name :: String; }
```

Function types are contravariant in arguments and covariant in results.

Records can also be **open** and have **optional fields**:

```tynix
{ name :: String; ... }
{ name :: String; version? :: String; }
forall r. { ...r } -> { tag :: String; ...r }
```

`...` stands for further fields of unknown type, and a named tail such as `...r`
is a row variable that relates the fields of an argument to those of a result.
`name? :: T` may be absent. Attribute sets with arbitrary names and uniform
values are `AttrsOf T`; a record is a subtype of `AttrsOf T` when all of its
fields fit `T`.

### 6. Union

Union types are included to support partial adoption.

```tynix
String | Int
```

### 7. Parametric polymorphism

```tynix
id :: forall a. a -> a;
```

Signatures are rigid: the body must work for every instantiation of the
quantified variables. Unannotated `let` bindings get principal polymorphic
types through Hindley-Milner let-polymorphism, so a signature is optional
for generic helpers.

### 8. Higher-kinded types

Type constructor application is treated as a first-class operation in the type language.

```tynix
type Functor f = {
  map :: forall a b. (a -> b) -> f a -> f b;
};
```

### 9. Conditional types

The language includes a TypeScript-style `extends ? :` form.

```tynix
type Element t = t extends List (infer a) ? a : t;
```

### 10. `infer`

`infer` introduces pattern variables inside the right-hand side of conditional types.

```tynix
type ReturnOf f = f extends (infer a -> infer r) ? r : dynamic;
```

### 11. Indexed dependent-ish containers

Lists can be preserved more precisely as indexed containers:

```tynix
Vec 3 Int
Matrix 2 4 Float
Tensor [2 3 4] Number
Vec (2 | 3 | Range 4 8 Nat) Int
```

Shape indices are type-level values, so the checker can express exact lengths,
bounded lengths, and unions of admissible lengths without introducing runtime
evidence.

Examples:

```tynix
[1 2]
# => Vec 2 (1 | 2)

[[1 2] [3 4]]
# => Matrix 2 2 (1 | 2 | 3 | 4)

[[1] [2 3]]
# => List (Vec (1 | 2) (1 | 2 | 3))

let xs :: Vec (Range 2 4 Nat) Int;
    xs = [1 2 3];
in xs
# => accepted

let xs :: Vec (2 | Range 4 8 Nat) Int;
    xs = [1 2 3];
in xs
# => rejected
```

### 12. Numeric validation

`tynix` supports a small numeric refinement surface:

```tynix
Nat
Range 0 100 Int
Range 0.0 1.0 Float
```

Numeric literals subtype these validators when they satisfy the corresponding
constraint. This also feeds back into indexed containers, so `Vec (Range 2 4
Nat) Int` can be checked directly against list literals of matching length.

Examples:

```tynix
let ratio :: Range 0.0 1.0 Float;
    ratio = 0.5;
in ratio
# => accepted

let ratio :: Range 0.0 1.0 Float;
    ratio = 1.5;
in ratio
# => rejected

let xs :: Vec (Range 0 0 Nat) Int;
    xs = [];
in xs
# => accepted
```

### 13. Units

Units are modeled as lightweight phantom wrappers over validated values:

```tynix
Unit "ms" (Range 0 5000 Nat)
Unit "MiB" Int
```

They are erased before runtime, but the checker keeps them distinct so values
with different units do not subtype each other accidentally.

Examples:

```tynix
let timeout :: Unit "ms" (Range 0 5000 Nat);
    timeout = 2500;
in timeout
# => accepted

let timeoutMs :: Unit "ms" Nat;
    timeoutMs = 1;
    timeoutS :: Unit "s" Nat;
    timeoutS = timeoutMs;
in timeoutS
# => rejected
```

### 14. Effects

Function types record the effects a call may perform: `String -> String ! {
Trace }`. Effects are inferred from the builtins a body calls, generalized
like type variables (`twice :: forall t0 e0. (t0 -> t0 ! e0) -> t0 -> t0 !
e0`), bounded by signatures (`! {}` demands purity), and discharged by
handlers (`builtins.tryEval` catches `Throw`). `flake.tynix` files must not
perform `Impure`. Arrows written without `!` stay untracked.

### 15. Linear types

`A %1 -> B` consumes its argument exactly once on every path; closures and
unrestricted calls count as many uses.

### 16. Capture tracking

`A ->{c} B` bounds the effectful capabilities a closure captures.

### 17. Dependent arrows

`(n :: Nat) -> Vec n a` lets the result depend on the argument's value,
computed with the `Get`, `KeyOf`, `Length`, `Add`, `Sub`, and `Mul` operators.

### 18. Opaque and phantom types

`opaque type Id t = String;` is nominal and invariant, so phantom parameters
are meaningful.

### 19. Higher-rank polymorphism and kind annotations

`forall` may appear in argument positions, `rec` fields generalize like `let`
bindings, and alias parameters accept kinds such as `(f :: Type -> Type)`.

### 20. Macros

Hygienic, declarative, typed macros: `macro name { (pattern) => (template); };`.

See [Effects, Linearity & Macros](./reference/advanced-types.md) for the full
rules.

## Consistency and Partial Adoption

`tynix` checks both subtyping and consistency.

- subtype
  - the strict static relation
- consistent
  - a gradual compatibility relation that accounts for `dynamic`

Examples:

- `String` and `dynamic` are consistent
- `String` is a subtype of `unknown`
- `any` is both a subtype of and a supertype of `String`
- `String` and `Int` are not consistent
- `Vec 2 Int` and `List Int` can still interact structurally
- `Unit "ms" Nat` and `Unit "s" Nat` are neither subtypes nor consistent by label alone

## Inference Strategy

### Basic rules

- Inference is Hindley-Milner with row-polymorphic records and gradual
  boundaries.
- Lambda parameters, pattern fields, and `let` bindings use their annotations
  when present.
- `let` signatures are rigid checking boundaries: the body is checked with the
  quantified variables held abstract, and later uses see the signature.
- Unannotated `let` bindings are grouped by dependency, inferred one strongly
  connected group at a time, and generalized over the inference variables that
  do not occur in the enclosing environment. A helper can therefore be used at
  several types in the same `let`, and mutual recursion works without
  annotations.
- Selecting a field from a value of unknown shape records an open row
  (`{ a :: T; ... }`) instead of failing, so `x: x.a + x.b` is
  `{ a :: Number; b :: Number; ... } -> Number`.
- Dependencies injected through attrset patterns, and fields selected from
  values of unknown shape, are gradual: calling one yields `dynamic` rather than
  fixing its type from the first call site.

```tynix
let
  id = x: x;
  pair = { a = id 1; b = id "s"; };
  get = x: x.a + x.b;
in { inherit pair get; }
```

See [How Checking Works](./reference/type-system-internals.md) for the full
algorithm.

### `builtins`

Every Nix builtin is typed by a prelude embedded in the binary (the source is
[`registry/workspace/builtins.d.tynix`](https://github.com/ubugeeei-prod/tynix/blob/main/registry/workspace/builtins.d.tynix)).
Both `builtins.map` and the global `map` are checked with no project setup. A
workspace that declares `builtins` itself replaces the prelude.

### `import`

`import ./foo.nix` is typed from a `declare "./foo.nix" { ... }` block, either
inline in the importing file or in any `.d.tynix` file under the workspace root
(conventionally `./foo.d.tynix` next to the module). If no declaration exists,
the checker falls back to `dynamic`.

Examples:

```tynix
declare "./lib.nix" { default :: { value :: Int; }; };

{
  typed = import ./lib.nix;
  untyped = import ./missing.nix;
}
```

```text
root: {
  typed :: {
    value :: Int;
  };
  untyped :: dynamic;
}
```

## Attribute sets

Attribute sets are modeled as `Record` types. Users should be able to annotate only the public surface they care about while still permitting additional fields.

```tynix
let
  pkg :: { name :: String; };
  pkg = { name = "a"; version = "1"; };
in pkg
```

## Type-class-like behavior

tynix does not implement Haskell-style type class resolution yet. Capabilities
are modeled as dictionary records:

```tynix
type Eq a = { eq :: a -> a -> Bool; };
```

Signatures may already carry a constraint context (`Functor f =>`,
`(Eq a, Show a) =>`). It is parsed and kept as documentation, but not enforced.

## Error Strategy

- Prefer explainable errors over maximal solver cleverness.
- Return messages that help users make fixes instead of exposing only large type equations.
- Point at the exact source span: call mismatches point at the argument, and
  record mismatches name the missing or ill-typed field.
- Surface `dynamic` fallbacks explicitly in hover and diagnostics.

## `.d.tynix` emitter

The emitter extracts only the public type surface from `.tynix`.

- If the root expression is an attribute set, its fields become the exported API.
- Any other root value is emitted as a `default`-style export.
- Required type aliases are emitted alongside it.

- The `declare` target is the compiled `.nix` path, relative to the emitted
  declaration file.

Example, for `user.tynix`:

```tynix
type User = { name :: String; };

{
  make = (name :: String): { inherit name; } as User;
}
```

Generated declaration:

```tynix
type User  = {
  name :: String;
};
declare "./user.nix" {
  make :: String %1 -> User;
};
```

If the root is polymorphic (for example `{ make = name: { inherit name; }; }`,
whose type is `forall t0. { make :: t0 %1 -> { name :: t0; }; }`), the whole
root is emitted as a single `default` member with a `forall`.

## Future Work

- type classes, so constraint contexts are enforced
- flow-sensitive narrowing through `isAttrs`, `?` and `_tag` guards
- `config`-aware typing of NixOS modules
- arithmetic on dependent singletons (`Vec (Add n 1) a` vs `Vec n a`)
- exporting macros from declaration files
- stronger bidirectional checking
- exhaustiveness hints
- richer declaration merging
- faster and more incremental solving

See the [roadmap](./roadmap.md).
