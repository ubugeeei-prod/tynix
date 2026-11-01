---
title: "12. CI integration"
description: Run tynix check-project on every pull request with GitHub Actions or any CI that has Nix, and keep generated .nix files in sync.
---

# 12. CI integration

The last step makes the checker a gate: no pull request merges with a type
error, and the committed `.nix` output never drifts from its `.tynix` source.

## Exit codes

Every tynix command exits with `0` on success and `1` on any diagnostic, so any
CI system can use it directly. With `--format json`, the report is printed to
standard output in both cases, which makes it easy to archive or post-process.

## GitHub Actions

```yaml [.github/workflows/tynix.yml]
name: tynix

on:
  pull_request:
  push:
    branches: [main]

permissions:
  contents: read

jobs:
  check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          persist-credentials: false

      - uses: cachix/install-nix-action@v31

      - name: Type-check the project
        run: nix run github:ubugeeei-prod/tynix/v0.5.0 -- check-project

      - name: Build and verify generated files are committed
        run: |
          nix run github:ubugeeei-prod/tynix/v0.5.0 -- build
          nix run github:ubugeeei-prod/tynix/v0.5.0 -- compile src/flake.tynix -o flake.nix
          git diff --exit-code -- dist flake.nix
```

- Pin the tynix version (`/v0.5.0`) so a new release cannot change your build
  without a pull request. Bump it deliberately, like any other dependency.
- The second step fails when someone edits a `.tynix` file without committing
  the regenerated output, or edits generated `.nix` by hand.
- If you do not commit generated files, drop the second step and run
  `tynix build` wherever the `.nix` is consumed instead.

> [!TIP]
> Pin third-party actions by commit SHA rather than by tag, as the tynix
> repository itself does, and let Dependabot or Renovate keep them current.

### Without Nix

On Linux x64 runners you can use the installer script instead of Nix:

```yaml
      - name: Install tynix
        run: curl -fsSL https://tynix.dev/install.sh | sh
      - run: tynix check-project
```

Check the [installation notes](../getting-started.md#installation) for the
directory the script installs into, and add it to `PATH` if needed.

## Annotating pull requests

The JSON report carries the file, success flag and error message for every
source, which is all you need to produce annotations. For example, with `jq`:

```bash
tynix check-project --format json \
  | jq -r '.files[] | select(.success | not) | "::error file=\(.source)::\(.error)"'
```

GitHub turns each `::error file=...::message` line into an annotation on the
pull request. The full schema is in the
[CLI reference](../reference/cli.md#json-output).

## Any other CI

The pattern is the same everywhere: install Nix (or run the installer), then
run `tynix check-project` from the project root. For example, in GitLab CI:

```yaml [.gitlab-ci.yml]
tynix:
  image: nixos/nix:latest
  script:
    - nix --extra-experimental-features "nix-command flakes" run github:ubugeeei-prod/tynix/v0.5.0 -- check-project
```

## Where to go from here

You have installed tynix, typed files from simple bindings up to flakes, and
wired the checker into CI. Some good next reads:

- [Adopting tynix](../migration.md): a playbook for introducing tynix into an
  existing repository.
- [Language reference](../language-reference.md): every syntax form and type
  form in one place.
- [How checking works](../reference/type-system-internals.md): the inference
  algorithm, the gradual lattice and the reduction rules behind what you saw in
  this tutorial.
- [Diagnostics](../diagnostics.md): every code with an explanation.

<div class="tx-pager">

[← 11. Projects](./projects.md) [13. Effects →](./effects.md)

</div>
