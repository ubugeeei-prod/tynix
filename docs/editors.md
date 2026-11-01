# Editor Setup

`tynix ide install <editor>` installs the tynix editor integration and writes the
editor configuration in one step. `tynix doctor` then checks that the toolchain
and the editors are wired up.

Install `tynix` and `tynix-lsp` first, with the installer script or the flake
(its default package contains both binaries):

```bash
curl -fsSL https://tynix.dev/install.sh | sh
# or
nix profile install github:ubugeeei-prod/tynix
```

## Quick start

```bash
cd my-project
tynix ide install vscode     # or: cursor, vscodium, zed, neovim, helix
tynix doctor
```

New projects can do both at once:

```bash
tynix init my-project --editor vscode
```

## `tynix ide install`

```text
tynix ide install <vscode|cursor|vscodium|zed|neovim|helix>
                 [--project DIR | --global] [--dry-run] [--no-extension]
                 [--force] [--lsp-path PATH]
```

| Flag              | Effect                                                                                                            |
| ----------------- | ----------------------------------------------------------------------------------------------------------------- |
| `--project DIR`   | Write project-level settings under `DIR` (default: the current directory).                                        |
| `--global`, `-g`  | Write user-level settings instead.                                                                                |
| `--dry-run`, `-n` | Print the planned commands and a line diff of every file, without changing anything.                              |
| `--no-extension`  | Only write configuration; do not run the editor CLI.                                                              |
| `--force`         | Rewrite files that cannot be merged safely (see below), keeping a `.bak` copy of the original first.              |
| `--lsp-path`      | The `tynix-lsp` path to pin in settings. By default it is found on `PATH` or in a Nix profile. `""` pins nothing. |

Without `--project` or `--global`, every editor uses project settings except
Neovim, which uses your user config.

The command is idempotent: running it again reports `already up to date`
for files that already contain the settings, and skips the extension install
when the extension is already present. It exits non-zero only if a step fails,
for example when the editor CLI cannot install the extension or a file cannot
be written.

### What each editor gets

| Editor   | Extension                                                       | Project scope                                      | `--global`                                        |
| -------- | --------------------------------------------------------------- | -------------------------------------------------- | ------------------------------------------------- |
| vscode   | `code --install-extension ubugeeei.tynix` (VS Code Marketplace) | `.vscode/settings.json`, `.vscode/extensions.json` | VS Code user `settings.json`                      |
| cursor   | `cursor --install-extension ubugeeei.tynix` (Open VSX)          | same as vscode                                     | Cursor user `settings.json`                       |
| vscodium | `codium --install-extension ubugeeei.tynix` (Open VSX)          | same as vscode                                     | VSCodium user `settings.json`                     |
| zed      | `auto_install_extensions` in the settings file                  | `.zed/settings.json`                               | `~/.config/zed/settings.json`                     |
| neovim   | none, a generated Lua file                                      | `.nvim.lua` (needs `:set exrc`)                    | `~/.config/nvim/after/plugin/tynix.lua` (default) |
| helix    | none, `languages.toml` entries                                  | `.helix/languages.toml`                            | `~/.config/helix/languages.toml`                  |

User settings follow each editor's platform convention: `~/Library/Application
Support/<App>/User` on macOS, `$XDG_CONFIG_HOME/<App>/User` on Linux.
`$XDG_CONFIG_HOME` (default `~/.config`) is respected for Zed, Neovim, and
Helix.

**VS Code / Cursor / VSCodium** settings:

```json
{
  "tynix.server.path": "/home/me/.nix-profile/bin/tynix-lsp",
  "files.associations": {
    "*.tynix": "tynix",
    "*.d.tynix": "tynix"
  }
}
```

`tynix.server.path` is only written when `tynix-lsp` was found. It is a
machine-specific path. If you commit `.vscode/settings.json`, consider
`--lsp-path ""`: the extension then finds `tynix-lsp` itself, looking in the
common Nix profile locations and then on `PATH`. `.vscode/extensions.json` gets
`ubugeeei.tynix` added to `recommendations`, so collaborators are prompted to
install it.

If the editor CLI is not on `PATH`, the command prints where to install the
extension from by hand and still writes the settings. In VS Code you can add
the CLI with **Shell Command: Install 'code' command in PATH**.

**Zed** settings use the `tynix-lsp` language-server id from
`editors/zed/extension.toml`:

```json
{
  "auto_install_extensions": { "tynix": true },
  "lsp": { "tynix-lsp": { "binary": { "path": "/home/me/.nix-profile/bin/tynix-lsp" } } }
}
```

Until tynix is listed in the Zed extension registry, install the extension as a
dev extension: run **zed: install dev extension** and pick `editors/zed`, or
run `vp ide` from a tynix checkout.

**Neovim** gets a self-contained Lua file. It registers the `tynix` filetype for
`*.tynix` and `*.d.tynix`, then starts `tynix-lsp` in one of three ways, whichever
is available first:

1. `require("tynix").setup(...)` when the [editors/neovim](https://github.com/ubugeeei-prod/tynix/blob/main/editors/neovim/README.md)
   plugin is on the runtimepath;
2. `vim.lsp.config` and `vim.lsp.enable` (Neovim 0.11+);
3. a `FileType` autocmd that calls `vim.lsp.start` (Neovim 0.10).

The file starts with a `Managed by tynix ide install` header. Re-running the
command updates that file. If a file at that path does not have the header, it
is left alone unless you pass `--force`. Delete the header to take ownership of
the file.

**Helix** gets a `[[language]] name = "tynix"` entry, which reuses Helix's
built-in Nix grammar, and a `[language-server.tynix-lsp]` table. Either one is
only added if it is missing, so an existing definition is never edited. Check
the result with `hx --health tynix`.

### Comments and hand-written settings

VS Code and Zed settings files are JSONC, so they may contain comments and
trailing commas. `tynix ide install` reads them and merges keys while keeping
the existing order and indentation. Comments, however, cannot be preserved
through a rewrite. When a file has comments and needs changes, the command
leaves the file alone and prints the snippet to add by hand. `--force`
rewrites the file anyway, after saving the original as `settings.json.bak`.
A file that already contains the settings is never rewritten.

A file that cannot be parsed, or a value with an unexpected shape (for
example `"files.associations"` that is not an object), is also never
overwritten. The command prints the snippet to add by hand instead.

## `tynix ide list`

Lists the supported editors and whether each one is detected. An editor counts
as detected when its CLI (`code`, `cursor`, `codium`, `zed`, `nvim`, `hx`) is
on `PATH` or its configuration directory exists.

## `tynix doctor`

```bash
tynix doctor
tynix doctor --format json
```

| Check                | Fails when                                                     | Warns when                                            |
| -------------------- | -------------------------------------------------------------- | ----------------------------------------------------- |
| `tynix`              | `tynix --version` fails                                        | `tynix` is not on `PATH`, or its version differs      |
| `tynix-lsp`          | not on `PATH`, `tynix-lsp --version` fails, or version differs | the version cannot be read                            |
| project              | `tynix.config.tynix` exists but does not load                  | no `tynix.config.tynix` in this or a parent directory |
| each detected editor | —                                                              | the extension or config is missing                    |
| `nix`                | —                                                              | `nix` is not on `PATH`                                |

What "set up" means for each editor: VS Code, Cursor, and VSCodium have
`ubugeeei.tynix` in `--list-extensions`. Zed has the extension installed. Neovim
has the generated `after/plugin/tynix.lua`. Helix's user `languages.toml`
defines the tynix language and server.

The command exits with status 1 if any check fails. Warnings do not change the
exit status. Text output is colored on a terminal; set `NO_COLOR` to turn
color off. The JSON report follows the other `--format json` payloads:

```json
{
  "schemaVersion": 1,
  "action": "doctor",
  "success": true,
  "checks": [
    { "id": "tynix-lsp.version", "status": "ok", "message": "…", "hint": null }
  ]
}
```

Check `status` is one of `ok`, `warn`, `fail`, or `skip`.

See [troubleshooting.md](./troubleshooting.md) for fixes to common problems.
