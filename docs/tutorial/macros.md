---
title: "16. Macros"
description: Write hygienic, declarative macros whose templates are type-checked once and whose expansions are plain Nix.
---

# 16. Macros

Some repetition is not about values but about **syntax**: an attribute set
whose names mirror its values, a family of options that differ by one word.
Functions cannot generate names, so tynix has **macros**: pattern-based
rewrites that run at compile time and leave nothing but ordinary Nix behind.

tynix macros are:

- **declarative**: a macro is a list of `(pattern) => (template)` rules;
- **hygienic**: names a template introduces cannot clash with names at the
  call site, in either direction;
- **typed**: parameters may carry types, each template is type-checked once
  at its definition, and arguments are checked at the call.

## Declaring and invoking

```tynix [macros.tynix]
macro enum {
  ( $( $tag:ident ),* ) => ({ $( $tag = stringify!($tag); )* });
};

macro unless {
  ($cond :: Bool, $body:expr) => (if $cond then null else $body);
};

macro withTmp {
  ($value:expr, $use:expr) => (let tmp = $value; in $use tmp);
};

let tmp = "outer"; in {
  colors = enum!(red, green, blue);
  warning = unless!(true, "unreachable");
  wrapped = withTmp!(1, (x: [ x tmp ]));
}
```

```text
root: {
  colors :: {
    blue :: "blue";
    green :: "green";
    red :: "red";
  };
  warning :: Null | "unreachable";
  wrapped :: Tuple [ 1 "outer" ];
}
tmp :: "outer"
```

A macro is declared at the top of a file, next to type aliases, and is in
scope for the declarations after it and for the root expression. Invoke it as
`name!( ... )`, with no space before `!`.

A pattern mixes literal tokens with **metavariables**:

| Pattern | Matches |
| --- | --- |
| `$x:expr` | any expression |
| `$x :: T` | an expression of type `T` |
| `$x:ident` | an identifier (usable as a binder or attribute name) |
| `$x:type` | a type |
| `$x:string` | a string literal |
| `$( ... ),*` | zero or more repetitions, separated by `,` (`;` also works, `+` means one or more, `?` at most one) |

A template is a parenthesized expression in which metavariables are replaced
by what they matched. `$( ... )*` in a template repeats once per match, and
`stringify!($x)` turns an identifier into a string. Rules are tried in order;
the first whose pattern matches the whole invocation wins.

## Hygiene

`withTmp` binds `tmp`, and the call site has its own `tmp`. The two do not
mix: the argument `(x: [ x tmp ])` still sees the outer `"outer"`. Compiling
shows how:

```nix [macros.nix]
let
  tmp = "outer";
in {
  colors = {
    red = "red";
    green = "green";
    blue = "blue";
  };
  warning = if true
  then null
  else "unreachable";
  wrapped = let
    tmp'1_364 = 1;
  in (x: [ x tmp ]) tmp'1_364;
}
```

Every binder a template introduces is renamed for that one expansion. The
other direction is protected too: a template may only refer to its own
parameters and to Nix globals, and globals are routed through `builtins`, so a
local `map` at the call site cannot change what a template's `map` means. A
template that refers to anything else is rejected where it is defined:

```tynix [option.tynix]
macro option {
  ($ty:expr) => (lib.mkOption { type = $ty; });
};

option!(1)
```

```text
2:18: [TX0003] macro template refers to `lib`, which is not in scope where the macro is defined; take it as a parameter instead
```

Write `($lib:expr, $ty:expr) => ($lib.mkOption { type = $ty; })` instead.
For the same reason templates cannot use `with` or `rec`.

## Types

Each rule is type-checked once, at the definition, with typed parameters at
their declared types and the others inferred:

```tynix [greet.tynix]
macro greet {
  ($name :: String) => ("Hello, " + $name + 1);
};

greet!("Ada")
```

```text
1:1: [TX0004] rule 1 of macro `greet` does not type-check: [TC0013] type mismatch: String vs Number
```

At a call, a typed argument is checked against its type before it is spliced
in, so the error points at the argument:

```tynix [unless.tynix]
macro unless {
  ($cond :: Bool, $body:expr) => (if $cond then null else $body);
};

unless!("yes", 1)
```

```text
5:9: [TC0013] type mismatch: "yes" vs Bool
```

The expansion is then checked like any other code, so a macro can never
produce an ill-typed program.

> [!TIP]
> Arguments are spliced, not evaluated once: a template that uses `$x` twice
> duplicates the argument expression. Bind it with `let` in the template when
> it is expensive.

## Recap

- `macro name { (pattern) => (template); };` declares, `name!( ... )` expands.
- Templates are hygienic: their binders are renamed and their free names must
  be globals.
- Typed parameters and definition-time checking catch mistakes at the macro,
  not in its expansions.
- Expansions are plain Nix with no runtime cost.

<div class="tx-pager">

[← 15. Dependent and opaque types](./dependent-types.md) [Tutorial overview](./index.md)

</div>
