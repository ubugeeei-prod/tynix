---
title: Effects, Linearity & Macros
description: How tynix checks effects, linearity, captures, dependent arrows, opaque types, higher-rank polymorphism and kinds, and how hygienic macros are expanded and typed.
---

# Effects, Linearity & Macros

This page specifies the parts of the type system that go beyond plain
Hindley-Milner with records: effect rows, linear arrows, capture sets,
dependent arrows, opaque types, higher-rank polymorphism, kind annotations,
and macros. It complements [How Checking Works](./type-system-internals.md),
which covers the core engine, and the tutorial chapters starting at
[13. Effects](../tutorial/effects.md), which introduce each feature by example.

Everything here is **erased**. None of these features changes the `.nix` that
`tynix compile` produces, apart from macros, which expand to ordinary Nix.

## The arrow

Every function type carries four facts besides its domain and codomain
(`Arrow` in [`Type.hs`](https://github.com/ubugeeei-prod/tynix/blob/main/packages/tynix-core/src/Type.hs)):

| Part | Syntax | Default when not written |
| --- | --- | --- |
| multiplicity | `A %1 -> B` (linear) or `A -> B` | unrestricted |
| latent effects | `A -> B ! { Trace, Throw }`, `! { Trace \| e }`, `! e`, `! {}` | untracked |
| capture set | `A ->{f, g} B`, `A ->{} B` | untracked |
| dependent binder | `(x :: A) -> B` | none |

The effect suffix belongs to the innermost arrow it follows: in
`a -> b -> c ! { E }` the full application performs `E`, the partial
application nothing. Parenthesize to put effects on an outer arrow:
`a -> (b -> c) ! { E }`.

**Untracked** is the gradual default. An arrow written without an effect row
or a capture set neither constrains the functions that flow into it nor
records anything when it is called, so existing signatures and declaration
packs keep their meaning. Inferred lambdas, by contrast, always carry the
effect row and capture set their bodies produce.

## Effects

### Representation

An effect row is a row of labels, represented exactly like a record row: a
closed row `{ Trace, Throw }`, an open row `{ Trace | e }` whose tail is an
effect variable, or a bare variable `e`. The labels are `Trace`, `Throw`,
`Abort`, `Read`, `Fetch`, `Store`, and `Impure`; the builtins prelude
(`registry/workspace/builtins.d.tynix`) annotates each builtin with its row.

### Inference

Inference keeps an **ambient row**: the effect row of the innermost lambda
body being inferred. The rules are Koka-style:

- Inferring a lambda starts its body with a fresh row variable `ρ` as the
  ambient row; the lambda's latent effect is `ρ`.
- Applying a function whose arrow carries row `R` *performs* `R`: every label
  of `R` is added to the ambient row (extending its open tail), and an
  unsolved tail of `R` is unified with the ambient row. That second half is
  how calling an unknown function `f` inside `x: f x` makes the lambda's
  effect *be* `f`'s effect.
- A failed `assert` performs `Throw`; reading `builtins.currentTime`,
  `currentSystem`, or `nixPath` performs `Impure`.
- `builtins.tryEval e` is a handler: `e` is inferred in its own ambient row,
  and everything but `Throw` is performed outward.
- Effects are approximated call-by-value: a `let`-bound thunk's effects are
  attributed to the enclosing computation even though Nix may never force it.

At generalization, a row variable that occurs exactly once in a type and
nowhere in the environment carries no information and is closed to the pure
row; the remaining effect variables are quantified like type variables and
named `e0`, `e1`, .... That is why `x: x + 1` is shown as `Int -> Int`, while

```tynix
twice = f: x: f (f x);
# twice :: forall t0 e0. (t0 -> t0 ! e0) -> t0 -> t0 ! e0
```

### Checking

A lambda checked against an arrow with a tracked row `R` must have its
inferred row contained in `R`: every label must appear in `R` (or be absorbed
by an unsolved tail), and an effect variable of the body must be `R`'s own
variable. Function subtyping is covariant in the effect row:
`A -> B ! { Trace } <: A -> B ! { Trace, Throw }`. A violation is
[`TC0023`](../diagnostics.md).

In a file named `flake.tynix`, performing `Impure` anywhere is
[`TC0024`](../diagnostics.md), because flakes are evaluated in pure mode.

## Linearity

`A %1 -> B` consumes its argument exactly once. The checker counts how each
lambda binder is consumed while it infers the body, in the lattice
`0 < 1 < ω`, plus "mixed" for disagreeing branches:

| Construct | Count |
| --- | --- |
| a variable occurrence | `1`, scaled by the context |
| the argument of an unrestricted function | scaled to `ω` |
| the argument of a linear function | unchanged |
| inside a nested lambda | `ω` (the closure may run any number of times) |
| `if c then a else b` | `c + join(a, b)`; `join` of different counts is "mixed" |
| the right side of a `let` binding read more than once | scaled to `ω` |

A lambda whose binder ends at exactly `1` is inferred linear. A lambda checked
against `%1 ->` must end at `1`, otherwise [`TC0026`](../diagnostics.md) names
the problem (never used, used more than once, or not once on every branch).
Multiplicity subtyping is `%1 -> <: ->`: a linear function may be used where an
unrestricted one is expected, not the reverse.

Linearity is tracked for variable binders. Data structures holding a linear
value are not themselves tracked, and attribute-set patterns are never
linear.

## Capture sets

The inferred capture set of a lambda is the set of its free variables that
are bound in an enclosing (non-global) scope and whose type is a function
with a known, non-empty effect row: the effectful **capabilities** the closure
holds. A lambda checked against `A ->{c1, c2} B` may capture only `c1` and
`c2` ([`TC0025`](../diagnostics.md)); capture sets are compared by inclusion
in subtyping, and an untracked set on either side always fits.

Capture sets are displayed only when non-empty, for example
`String ->{fetch} String ! { Fetch }`.

## Dependent arrows

`(x :: A) -> B` binds `x` in `B`. At a call `f e`:

- if `e` is a variable that is itself a dependent binder in scope, `x` is that
  binder's singleton;
- otherwise, if the type of `e` is fully known (no inference variables, no
  `dynamic`), `x` is that type, which for a literal is the literal itself;
- otherwise `x` is `A`.

The codomain is then reduced. The built-in type operators are:

| Operator | Reduces to |
| --- | --- |
| `Get r k` | the type of field `k` of `r` (a union of literal keys joins the fields); for a key that is only known to be a `String`, a dictionary's value type, the join of a closed record's fields, or `dynamic` |
| `KeyOf r` | the union of a closed record's field names; `String` for an open record or dictionary |
| `Length xs` | `n` for `Vec n a`, the item count of a tuple, `Nat` for a list |
| `Add a b`, `Sub a b`, `Mul a b` | the literal result for integer literals, otherwise `Nat` / `Int` / `Number` |

An operator whose arguments are not known yet stays unreduced and is reduced
when they are.

Checking a lambda `x: body` against `(n :: A) -> B` gives `x` the
**singleton type** `x` (whose base is `A`), and checks `body` against `B` with
`n` replaced by that singleton. As an ordinary value the singleton behaves as
`A`; passed to another dependent arrow it carries the identity of `x`, which is
how `replicate = n: x: builtins.genList (_: x) n` checks against
`(n :: Nat) -> a -> Vec n a`. The checker does not do arithmetic on
singletons: `Vec (Add n 1) a` and `Vec n a` are different.

## Opaque types

`opaque type Name params = Representation;` declares a nominal type. It is
never expanded by alias resolution, so it is equal only to itself, and its
parameters are **invariant**: `Id User` and `Id Package` are unrelated even when
`t` is a phantom parameter that the representation does not mention. An `as`
cast is the only way across: a cast is accepted when the representation (after
substituting the parameters) is related to the other side. Selecting a field
of an opaque type is [`TC0027`](../diagnostics.md).

## Higher-rank polymorphism

A `forall` may appear anywhere in a signature, for example as a parameter:
`apply :: (forall a. a -> a) -> { n :: Int; s :: String; }`.

- A variable whose type is a `forall` is instantiated afresh at every use, so
  `f 1` and `f "x"` can both appear in `apply`'s body.
- A lambda checked against a `forall` type is checked against fresh rigid
  variables (skolems): `apply (x: x)` is accepted, `apply (x: x + 1)` is not.
- A polymorphic value is instantiated when it meets a monomorphic expectation.

## `rec` generalization

The fields of a `rec { ... }` are inferred one dependency group at a time and
generalized like `let` bindings. A polymorphic field keeps its quantifier in
the record type (`{ id :: forall t0. t0 -> t0; }`) and is instantiated afresh
at each selection.

## Kind annotations

An alias parameter may state its kind: `type Fix (f :: Type -> Type) = ...;`.
Kinds are written `Type` (or `*`) and `k1 -> k2`. An annotation fixes the
parameter's kind before the body is inferred, so a mismatch is a kind error
([`TK0001`](../diagnostics.md)) at the alias rather than a surprise at a use.

## Type ascription

`(e :: T)` checks that `e` has type `T` and gives the expression type `T`.
Unlike `e as T`, it never narrows. It is erased.

## Macros

### Syntax

```text
macro name {
  (pattern) => (template);
  ...
};
```

A pattern is a sequence of literal tokens (words and punctuation),
metavariables `$x:expr`, `$x:ident`, `$x:type`, `$x:string`, typed expressions
`$x :: T`, bracketed groups, and repetitions `$( ... ) sep? *|+|?`. A template
is a parenthesized tynix expression in which metavariables may appear wherever
an identifier may (as expressions, binders, attribute names, selectors, and
type variables), `$( ... ) sep? *` repeats, and `stringify!(x)` produces a
string from an identifier.

### Expansion

Macros are expanded while the file is parsed
([`ParserExpr.hs`](https://github.com/ubugeeei-prod/tynix/blob/main/packages/tynix-core/src/ParserExpr.hs),
[`Macro.hs`](https://github.com/ubugeeei-prod/tynix/blob/main/packages/tynix-core/src/Macro.hs)):

1. **Match.** An invocation `name!(` (no space before `!`) of a macro in scope
   tries each rule in order. A pattern is run as a parser over the invocation's
   input: literal tokens must appear verbatim and each metavariable parses its
   fragment with the ordinary grammar. The first rule that consumes the whole
   argument list wins ([`TX0001`](../diagnostics.md) when none does).
2. **Instantiate.** Repetitions in the template are expanded textually, once
   per match, and every metavariable occurrence is renamed after its
   repetition path (`$x`, `$x'0`, `$x'1'0`).
3. **Parse.** The instantiated template is parsed with the expression grammar.
   Nested invocations expand recursively, up to 64 levels
   ([`TX0005`](../diagnostics.md)).
4. **Hygiene.** Every binder the template itself introduces (lambda
   parameters, `let` names, `@` binders) is renamed with a suffix unique to the
   invocation (`tmp` becomes `tmp'1_364`). Free names must be Nix globals and
   are rewritten to `builtins.name`, so the call site cannot shadow them; any
   other free name, `with`, and `rec` are [`TX0003`](../diagnostics.md).
   Attribute-pattern fields keep their names, since they are matched by name.
5. **Substitute.** Matched fragments replace the metavariables. A typed
   argument `$x :: T` is spliced as the ascription `(arg :: T)`, located at the
   argument, so a wrong argument is reported where it was written.
6. **Locate.** The expansion's own nodes are located at the invocation.

### Typing

Each rule is checked once, when the macro is defined: its template is
instantiated with symbolic arguments (each repetition taken once), closed over
its metavariables as a lambda whose typed parameters carry their declared
types, and type-checked in the file's context. Invocations of other macros
inside the template are left as unknown values for this check, which keeps
recursive macros finite. A failure is [`TX0004`](../diagnostics.md).

After expansion the program is checked as usual, so a macro cannot produce an
ill-typed program even when a rule's definition-time check is approximate.

### Limitations

- Macros are file-local; declaration files cannot export them.
- An `expr` fragment extends as far as the expression grammar allows; separate
  fragments with punctuation or Nix keywords.
- Arguments are spliced, not shared: a template that uses `$x` twice
  duplicates the argument.
- The LSP formatter does not reformat files that declare macros.
