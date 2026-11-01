---
title: "15. Dependent and opaque types"
description: Let a result type depend on an argument's value, keep look-alike strings apart with opaque and phantom types, and annotate kinds.
---

# 15. Dependent and opaque types

Two more tools for making types say more: **dependent arrows**, whose result
type mentions the argument's value, and **opaque types**, which make two
structurally identical types distinct.

## Dependent arrows

Name the parameter, `(n :: Nat) -> ...`, and the result type can refer to it.
At each call, `n` stands for the argument's precise type, which for a literal
is the literal itself:

```tynix [dependent.tynix]
let
  replicate :: forall a. (n :: Nat) -> a -> Vec n a;
  replicate = n: x: builtins.genList (_: x) n;

  config = { port = 8080; host = "localhost"; };
in {
  three = replicate 3 "x";
  port = builtins.getAttr "port" config;
  size = builtins.length [ "a" "b" ];
}
```

```text
root: {
  port :: 8080;
  size :: 2;
  three :: Vec 3 "x";
}
config :: {
  host :: "localhost";
  port :: 8080;
}
replicate :: forall a. (n :: Nat) -> a -> Vec n a
```

The builtins prelude uses dependent arrows where Nix's behavior depends on a
value:

- `builtins.genList :: forall a. (Int -> a) -> (n :: Int) -> Vec n a`
- `builtins.getAttr :: forall r. (k :: String) -> r -> Get r k`
- `builtins.length :: forall a. (xs :: List a) -> Length xs`

`Get r k` is indexed access (the type of field `k` of `r`), and `Length xs` is
the length of an exact sequence. Together with `KeyOf r` (the union of a
record's field names) and `Add`, `Sub`, `Mul` on integer literals, these
operators reduce as soon as their arguments are known. When the argument is
not a known literal, the binder stands for the parameter type instead, so
`builtins.length xs` on a plain `List a` is a `Nat`.

Inside the body of `replicate`, `n` is checked as the *value* `n`: passing it
on to `genList` produces `Vec n a`, which is exactly the declared result.

## Opaque types

Many Nix values are strings that mean different things: store paths, user
names, package attribute names. `opaque type` declares a **nominal** type,
distinct from every other type including its own representation:

```tynix [opaque.tynix]
opaque type Id t = String;
type User = { name :: String; };
type Package = { pname :: String; };

let
  userId :: String -> Id User;
  userId = raw: raw as Id User;

  packageName :: Id Package -> String;
  packageName = id: id as String;
in packageName (userId "ada")
```

```text
11:17: [TC0013] type mismatch: Id User vs Id Package
```

`t` is a **phantom** parameter: it never appears in the representation, but
because the type is opaque, `Id User` and `Id Package` are different types.
Moving between an opaque type and its representation takes an explicit `as`
cast, in either direction, and selecting a field from an opaque record is an
error ([`TC0027`](../diagnostics.md)). Like every type, opaque types are erased:
`userId` compiles to `raw: raw`.

## Kind annotations

Higher-kinded parameters are inferred, but you can state them, which also
documents the alias:

```tynix [hkt.tynix]
type Compose (f :: Type -> Type) (g :: Type -> Type) a = f (g a);

let
  grid :: Compose List List Int;
  grid = [ [ 1 2 ] [ 3 ] ];
in grid
```

`Compose Int List Bool` is rejected with a kind error, because `Int` has kind
`Type`, not `Type -> Type`.

## Recap

- `(x :: A) -> B` names the argument; `B` may use `x` as its precise type.
- `Get`, `KeyOf`, `Length`, `Add`, `Sub`, and `Mul` compute types from
  known arguments.
- `opaque type` is nominal; phantom parameters keep instances apart; `as`
  crosses the boundary.
- Alias parameters accept kind annotations such as `(f :: Type -> Type)`.

<div class="tx-pager">

[← 14. Linearity and captures](./linear-types.md) [16. Macros →](./macros.md)

</div>
