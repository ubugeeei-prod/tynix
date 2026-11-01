# Examples

`./examples` is a small tynix showcase project with runnable, checkable samples.

Quick commands:

```bash
tynix check ./examples/main.tynix
tynix check-project ./examples
tynix check ./examples/polymorphism/hkt-map.tynix
tynix check ./examples/interop/inline-declare.tynix
```

Catalog:

- `main.tynix`: a compact "hello tynix" entry point using `builtins.map`
- `basics/`: literals, lets, lambdas, records, lists, and conditionals
- `gradual/`: `any`, `unknown`, `dynamic`, casts, and diagnostic directives
- `polymorphism/`: `forall`, unions, higher-kinded aliases, tuples, and linear arrows
- `indexed/`: `Vec`, `Matrix`, `Tensor`, `Range`, and `Unit`
- `interop/`: inline declarations, ambient `.d.tynix` files, and untyped imports
- `advanced/`: effects, linear arrows and capture sets, dependent and opaque types, kind annotations, and macros
- `legacy/`: plain `.nix` files used by the interop examples
- `support/`: workspace declarations for `builtins`, legacy modules, and `tynix.config.tynix`

The top-level `tynix.config.tynix` is wired so `tynix check-project ./examples`
walks the sample set as one project.
