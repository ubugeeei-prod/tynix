---
title: "14. Linearity and captures"
description: Require a value to be consumed exactly once with linear arrows, and control which capabilities a closure may capture.
---

# 14. Linearity and captures

Some values should be used exactly once: a one-shot token, a builder state
that must be passed on, a handle that must not be duplicated. tynix checks this
with **linear arrows**. A related question is which effectful functions a
closure holds on to; **capture sets** answer it.

## Linear arrows

`A %1 -> B` is a function that consumes its argument exactly once:

```tynix [linear.tynix]
let
  consume :: String %1 -> String;
  consume = token: "used ${token}";

  forward :: String %1 -> String;
  forward = token: consume token;

  choose :: Bool -> String %1 -> String;
  choose = fresh: token: if fresh then consume token else token;
in { inherit forward choose; }
```

```text
root: {
  choose :: Bool -> String %1 -> String;
  forward :: String %1 -> String;
}
choose :: Bool -> String %1 -> String
consume :: String %1 -> String
forward :: String %1 -> String
```

`forward` passes its token to another linear function, which counts as one
use. `choose` uses the token once on *each* branch, which is also fine: only
one branch runs.

These are rejected:

```tynix [twice.tynix]
let
  consume :: String %1 -> String;
  consume = token: "${token}${token}";
in consume
```

```text
3:13: [TC0026] linear binder `token` is used more than once (directly, inside a closure, or as the argument of an unrestricted function)
```

```tynix [branch.tynix]
let
  keep :: String %1 -> String;
  keep = token: if token == "" then "empty" else token;
in keep
```

```text
3:10: [TC0026] linear binder `token` is not used exactly once on every branch
```

The rules, in short:

- Each direct use counts once; uses on the two branches of an `if` must agree.
- A use inside a nested lambda counts as *many*: the closure may run any
  number of times.
- Passing the binder to an unrestricted function (`->`) counts as many; to a
  linear one (`%1 ->`) counts as one.
- A `let` binding that is read more than once counts as many uses of what it
  consumed.

tynix also infers linearity: a lambda that happens to use its binder exactly
once is shown as `%1 ->`. A linear function can always be used where an
unrestricted one is expected, never the other way round.

## Capture sets

A closure that captures an effectful function can perform its effects later,
long after it was built. An arrow can state which **capabilities** a closure
may capture, written right after the arrow: `A ->{fetch} B` may capture
`fetch`, and `A ->{} B` captures nothing.

```tynix [captures.tynix]
let
  mkFetcher :: (String -> String ! { Fetch }) -> String ->{fetch} String ! { Fetch };
  mkFetcher = fetch: url: fetch url;

  offline :: (String -> String ! { Fetch }) -> String ->{} String;
  offline = fetch: url: "cached:${url}";
in { inherit mkFetcher offline; }
```

`offline` promises that the function it returns holds no fetcher, so it is safe
to use where the network is unavailable. Capturing it anyway is an error:

```tynix [leak.tynix]
let
  offline :: (String -> String ! { Fetch }) -> String ->{} String ! { Fetch };
  offline = fetch: url: fetch url;
in offline
```

```text
3:20: [TC0025] closure captures `fetch`, but its type only allows {}
```

A capability is any variable in scope whose type is a function with a known,
non-empty effect row. Globals such as `builtins.fetchurl` are always available
and are not tracked as captures. An arrow without a capture set does not
restrict captures.

## Recap

- `A %1 -> B` must consume its argument exactly once on every path.
- Closures and unrestricted calls count as many uses.
- `A ->{c} B` bounds the capabilities a closure captures; `->{}` allows none.

<div class="tx-pager">

[← 13. Effects](./effects.md) [15. Dependent and opaque types →](./dependent-types.md)

</div>
