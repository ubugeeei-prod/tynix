import { defineConfig } from "vite-plus";

const sequence = (...commands: string[]) => commands.join(" && ");

export default defineConfig({
  run: {
    tasks: {
      "workspace:build": {
        command: sequence(
          "vp run build:haskell",
          "pnpm --filter tynix build",
          "vp run build:zed",
          "vp run docs:build",
        ),
      },
      "workspace:check": {
        command: sequence(
          "vp run check:source-format",
          "vp run check:versions",
          "vp run check:prelude",
          "vp run check:haskell",
          "vp run test:haskell",
          "vp run check:dogfood",
          "vp run check:examples",
          "pnpm --filter tynix check",
          "pnpm --filter tynix test",
          "vp run check:zed",
          "vp run test:zed",
          "vp run check:neovim",
        ),
      },
      "workspace:fmt": {
        command: sequence(
          "vp run fmt:haskell",
          "vp run fmt:source",
          "pnpm --filter tynix fmt",
        ),
      },
      "check:source-format": {
        command:
          "node --experimental-strip-types scripts/format-source.ts --check",
        cache: false,
      },
      "fmt:source": {
        command:
          "node --experimental-strip-types scripts/format-source.ts --write",
        cache: false,
      },
      // Publishes dist/docs to the `tynix` Cloudflare Pages project (tynix.dev).
      // Requires `wrangler login` or CLOUDFLARE_API_TOKEN + CLOUDFLARE_ACCOUNT_ID.
      ide: {
        command: "node --experimental-strip-types ./scripts/install-ide.ts",
        cache: false,
      },
      cli: {
        command: "node --experimental-strip-types ./scripts/install-cli.ts",
        cache: false,
      },
      "build:haskell": {
        command: "cabal build all",
      },
      "check:haskell": {
        command: "cabal build all",
      },
      "test:haskell": {
        command: "cabal test all",
      },
      "fmt:haskell": {
        command:
          "if rg --files -g '*.hs' >/dev/null 2>&1; then fourmolu -m inplace $(rg --files -g '*.hs'); else echo 'no haskell sources'; fi",
        cache: false,
      },
      "check:prelude": {
        command:
          "node --experimental-strip-types ./scripts/generate-prelude.ts --check",
        cache: false,
      },
      "generate:brand": {
        command: "node --experimental-strip-types ./scripts/generate-brand.ts",
        cache: false,
      },
      "generate:prelude": {
        command:
          "node --experimental-strip-types ./scripts/generate-prelude.ts",
        cache: false,
      },
      "check:versions": {
        command:
          "node --experimental-strip-types ./scripts/check-version-sync.ts",
        cache: false,
      },
      "check:dogfood": {
        command: "cabal run tynix -- check ./dogfood/flake-surface.tynix",
        cache: false,
      },
      "check:examples": {
        command: "cabal run tynix -- check-project ./examples --format json",
        cache: false,
      },
      "build:zed": {
        command: "cargo build --manifest-path editors/zed/Cargo.toml",
        cache: false,
      },
      "check:zed": {
        command: "cargo check --manifest-path editors/zed/Cargo.toml",
        cache: false,
      },
      "test:zed": {
        command: "cargo test --manifest-path editors/zed/Cargo.toml",
        cache: false,
      },
      "check:neovim": {
        command:
          "nvim --headless -u NONE -c \"lua vim.opt.runtimepath:append(vim.fn.getcwd() .. '/editors/neovim')\" -l editors/neovim/test/config_spec.lua",
        cache: false,
      },
    },
  },
});
