---
title: "6. Typing existing .nix"
description: Describe untyped .nix modules with .d.tynix declaration files and declare blocks, type builtins, and generate declarations with tynix emit.
---

# 6. Typing existing .nix

You will not rewrite a whole Nix codebase in `.tynix`, and you do not need to.
**Declarations** describe the type of a `.nix` file without touching it, the way
`.d.ts` files describe JavaScript libraries. Typed code that imports the file
then sees real types instead of `dynamic`.

## A module to describe

Create an ordinary Nix file. tynix will never compile or modify it:

```nix [lib/strings.nix]
{
  shout = s: "${s}!";
  join = sep: xs: builtins.concatStringsSep sep xs;
  version = "1.4.0";
}
```

## Write a `.d.tynix` file

Next to it, create a declaration file:

```tynix [lib/strings.d.tynix]
declare "./strings.nix" {
  shout :: String -> String;
  join :: String -> List String -> String;
  version :: String;
};
```

A `declare` block names a target path, resolved relative to the file that
contains the block, and lists the module's members. A declaration file may
contain `type` aliases and `declare` blocks but no expression.

Now import the module from typed code:

```tynix [main.tynix]
let
  strings = import ./lib/strings.nix;
in {
  loud = strings.shout "hello";
  csv = strings.join "," [ "a" "b" ];
  inherit (strings) version;
}
```

```bash
tynix check main.tynix
```

```text
root: {
  csv :: String;
  loud :: String;
  version :: String;
}
strings :: {
  join :: String -> List String -> String;
  shout :: String -> String;
  version :: String;
}
```

`strings` is no longer `dynamic`, so mistakes surface: `strings.shout 42`
reports `[TC0013] type mismatch: 42 vs String`, pointing at the `42`.

### How declaration files are found

You did not tell `main.tynix` where `strings.d.tynix` lives. tynix finds the
**workspace root** (the nearest ancestor directory with `.git`, `flake.nix`,
`tynix.config.tynix`, `cabal.project` or `pnpm-workspace.yaml`; this is why you ran
`git init` in step 1) and loads **every** `.d.tynix` file under it. Each
`declare` block is keyed by the absolute path of its target, and `import` looks
that path up. Where the `.d.tynix` file lives is therefore up to you: next to the
module, or in a `types/` directory.

Two consequences:

- Each target may be declared only once in a workspace. A second `declare` for
  the same file is `[TD0002] duplicate ambient declarations`, and it fails
  every check in the workspace until you remove one.
- A nested directory that has its own workspace marker (say, a vendored repo
  with its own `.git`) is skipped, and so are hidden directories,
  `node_modules`, `dist-newstyle`, `result*` build links and symlinked
  directories.
- Without any workspace marker, only the `.d.tynix` files directly next to the
  checked file are loaded. Discovery never walks an arbitrary directory tree.

## When the implementation is `.tynix`

A declaration describes a `.nix` file you do not compile. When the
implementation itself is a `.tynix` file, do not restate it. Name the compiled
path in the import, and leave the source beside it:

```tynix [lib.tynix]
let
  shout :: String -> String;
  shout = s: s + "!";
in { inherit shout; }
```

```tynix [main.tynix]
(import ./lib.nix).shout "hi"
```

`tynix check main.tynix` reads `lib.tynix`, uses its checked type for
`import ./lib.nix`, and rejects `shout 1`. `tynix compile lib.tynix -o lib.nix`
writes the file Nix evaluates. A `declare "./lib.nix"` in the same workspace
does not override that source. If the two files import each other, the check
fails with `TD0009` instead of looping.

Generated `.d.tynix` files remain the surface you publish for consumers who
do not have the `.tynix` source. Inside the project, the source wins.

## Default exports

If a module evaluates to something other than an attribute set, such as a
function or a string, declare a single member called `default`:

```tynix [greeting.tynix]
declare "./mk-greeting.nix" {
  default :: String -> String;
};

(import ./mk-greeting.nix) "tynix"
```

```text
root: String
```

This example also shows that a `declare` block can sit at the top of a regular
`.tynix` file, before its expression. Inline declarations are handy for one-off
imports; shared ones belong in a `.d.tynix` file.

## Typed `builtins`

You never have to declare `builtins` yourself. Every tynix binary embeds a
**built-in prelude** that types every Nix builtin, so `builtins.*` members and
the globals Nix exposes without the prefix (`toString`, `map`, `throw`,
`import`, `derivation`, `baseNameOf`, `dirOf`, `fetchTarball`, `isNull`,
`removeAttrs`, `placeholder`, ...) are checked with no project setup:

```tynix [lists.tynix]
let
  n = builtins.length [ 1 2 3 ];
  inc = (x :: Int): x + 1;
  xs = map inc [ 1 2 ];
  names = builtins.attrNames { b = 1; a = 2; };
  label = "${toString n} items";
in { inherit n xs names label; }
```

```text
root: {
  label :: String;
  n :: Int;
  names :: List String;
  xs :: List Int;
}
inc :: Int %1 -> Int
label :: String
n :: Int
names :: List String
xs :: List Int
```

The prelude also defines aliases you can use in your own annotations, such as
`Derivation`, `DerivationArgs`, `FetchedSource`, `PathLike`, `FileType`,
`TypeName` and `NameValuePair a`. Its source is
[`registry/workspace/builtins.d.tynix`](https://github.com/ubugeeei-prod/tynix/blob/main/registry/workspace/builtins.d.tynix);
see [builtins and the registry](../reference/builtins.md).

`builtins` is a closed record: a misspelled member such as
`builtins.readFlie` is `` [TC0009] missing field `readFlie` ``.

### Overriding the prelude

A declaration target of `"builtins"` (a string, not a path) replaces the
prelude for the whole workspace:

```tynix [types/builtins.d.tynix]
declare "builtins" {
  head :: forall a. List a -> a;
  length :: forall a. List a -> Int;
  map :: forall a b. (a -> b) -> List a -> List b;
  toString :: unknown -> String;
};
```

The replacement is complete, not a merge: with this file in place,
`builtins.readFile` is a missing field. That is useful to pin the builtins to
an older Nix version or to forbid some of them. Otherwise, do not declare
`builtins` at all. Delete this file again before you continue the tutorial.

## Generate declarations with `tynix emit`

For files you *do* write in tynix, there is no need to hand-write declarations.
`tynix emit` derives one from the checked source:

```tynix [greetings.tynix]
type Greeting = { text :: String; loud :: Bool; };

{
  mk = (text :: String): { inherit text; loud = false; };
  default = { text = "hi"; loud = true; };
}
```

```bash
tynix emit greetings.tynix -o greetings.d.tynix
```

```tynix [greetings.d.tynix]
type Greeting  = {
  loud :: Bool;
  text :: String;
};
declare "./greetings.nix" {
  default :: {
    loud :: true;
    text :: "hi";
  };
  mk :: String %1 -> {
    loud :: false;
    text :: String;
  };
};
```

When the root expression is an attribute set, each field becomes a member;
otherwise the whole value becomes `default`. The file's type aliases are copied
along so the declaration stays readable. The target is the compiled `.nix`
path, which is what other modules will import at runtime.

> [!TIP]
> Emitted declarations record what was *inferred*, literal types included. If
> you want a stable, wider public API, add signatures (or `as` casts) to the
> exported fields before emitting.

## Recap

- `declare "./file.nix" { member :: Type; };` describes an existing module.
- All `.d.tynix` files under the workspace root are loaded automatically.
- Use `default` for modules that are not attribute sets.
- `builtins` and global builtins such as `toString` are typed out of the box;
  a workspace `declare "builtins" { ... }` replaces that prelude.
- `tynix emit` writes declarations for your own `.tynix` files.

<div class="tx-pager">

[← 5. Gradual typing](./gradual.md) [7. Generics and HKT →](./generics.md)

</div>
