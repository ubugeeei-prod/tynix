---
title: "13. Effects"
description: See which functions trace, throw, read files, fetch, write to the store, or read the environment, require purity where it matters, and keep flakes pure.
---

# 13. Effects

Nix is pure, but evaluation can still *do* things: print a trace, throw,
read a file, fetch a tarball, write a derivation to the store, or read the
clock. tynix tracks these as **effects**. Every function type records the
effects a call may perform, inferred from the builtins its body uses, and a
signature can demand that a function performs none.

This step and the next three go beyond the everyday workflow. They are
independent of each other, so read the ones you need.

## Inferred effects

```tynix [effects.tynix]
let
  greet = name: builtins.trace "greeting ${name}" "Hello, ${name}!";

  double :: Int -> Int ! {};
  double = x: x * 2;

  safeDiv = a: b: if b == 0 then throw "division by zero" else a / b;
  attempt = builtins.tryEval (safeDiv 1 0);

  twice = f: x: f (f x);
in { inherit greet double safeDiv attempt twice; }
```

```bash
tynix check effects.tynix
```

```text
root: forall t0 t1 e0. {
  attempt :: TryEvalResult Number;
  double :: Int -> Int;
  greet :: t0 -> String ! {Trace};
  safeDiv :: Number -> Number -> Number ! {Throw};
  twice :: (t1 -> t1 ! e0) -> t1 -> t1 ! e0;
}
attempt :: TryEvalResult Number
double :: Int -> Int
greet :: forall t0. t0 -> String ! {Trace}
safeDiv :: Number -> Number -> Number ! {Throw}
twice :: forall t0 e0. (t0 -> t0 ! e0) -> t0 -> t0 ! e0
```

Read `A -> B ! { Trace }` as "a function from `A` to `B` that may trace". A few
things to notice:

- `greet` calls `builtins.trace`, so it performs `Trace`.
- `safeDiv` may `throw`, so it performs `Throw`. The effect sits on the last
  arrow: `safeDiv 1` alone does nothing; `safeDiv 1 0` throws.
- `attempt` performs nothing. `builtins.tryEval` is a **handler**: it catches
  `Throw`, so the effect does not escape.
- `twice` is **effect-polymorphic**. It performs whatever `f` performs, written
  as the effect variable `e0`. `twice (x: x + 1)` is pure;
  `twice (builtins.trace "hi")` traces.
- `double` and other pure functions print without an effect row.

The effects tynix knows about:

| Effect | Performed by |
| --- | --- |
| `Trace` | `trace`, `traceVerbose`, `warn`, `break` |
| `Throw` | `throw`, a failed `assert` (caught by `tryEval`) |
| `Abort` | `abort` (not catchable) |
| `Read` | `readFile`, `readDir`, `readFileType`, `pathExists`, `hashFile`, `findFile`, `path`, `filterSource` |
| `Fetch` | `fetchurl`, `fetchTarball`, `fetchGit`, `fetchTree`, `fetchMercurial` |
| `Store` | `derivation`, `toFile`, `storePath`, `path`, `filterSource` |
| `Impure` | `getEnv`, `currentTime`, `currentSystem`, `nixPath` |

## Demanding purity

An effect row in a signature is a promise. `! {}` promises no effects at all:

```tynix [pure.tynix]
let
  double :: Int -> Int ! {};
  double = x: builtins.trace "doubling" (x * 2);
in double
```

```text
3:12: [TC0023] effect `Trace` is not allowed here: the expected effects are {} (pure)
```

List what you allow, `! { Trace, Throw }`, or stay polymorphic with an effect
variable. Here `withDefault` performs exactly what its loader performs:

```tynix [poly.tynix]
let
  withDefault :: forall a e. (String -> a ! e) -> String -> a ! e;
  withDefault = load: name: load name;

  config = withDefault builtins.readFile "./config.json";
in config
```

`withDefault builtins.readFile` reads a file; `withDefault (n: n)` is pure.

> [!NOTE]
> An arrow written **without** `! { ... }` does not track effects. That keeps
> existing signatures and declaration packs working unchanged: `String ->
> String` accepts a tracing function, and calling one declared that way records
> nothing. Effects become a contract only where you write them down.

## Pure flakes

Flakes are evaluated in pure mode, where `builtins.getEnv` returns nothing and
`currentSystem` is unavailable. In a file named `flake.tynix`, tynix reports
the `Impure` effect wherever it happens:

```tynix [flake.tynix]
{
  outputs = { self, ... }: {
    packages.default = builtins.currentSystem;
  };
}
```

```text
3:24: [TC0024] impure operation in pure evaluation: flakes cannot read the environment, the clock, or the host platform
```

Take `system` as an input instead, the way `flake-utils` does.

## Recap

- Function types carry the effects a call may perform, inferred from the body.
- `! {}` demands purity, `! { Trace }` bounds the effects, `! e` is polymorphic.
- `builtins.tryEval` discharges `Throw`.
- Arrows without an effect row are untracked, so adoption stays gradual.
- `flake.tynix` files must not perform `Impure`.

<div class="tx-pager">

[← 12. CI integration](./ci.md) [14. Linearity and captures →](./linear-types.md)

</div>
