import { rm } from "node:fs/promises";
import { resolve } from "node:path";
import { defineConfig, type Plugin } from "vite";
import { defineTheme, oxContent } from "@ox-content/vite-plugin";
import { codeTokens, fonts, headHtml, themeColors, themeCss, themeJs } from "./docs/.vite/brand.ts";
import { processSite } from "./docs/.vite/highlight.ts";

// Canonical public origin. Override with TYNIX_DOCS_SITE_URL for preview
// deployments that should advertise their own URL in OG tags.
const siteUrl = process.env.TYNIX_DOCS_SITE_URL ?? "https://tynix.dev";
const outDir = "dist/docs";

const navigation = [
  {
    title: "Start",
    items: [
      { title: "Overview", path: "/" },
      { title: "Getting Started", path: "/getting-started" },
      { title: "Support Matrix", path: "/support-matrix" },
    ],
  },
  {
    title: "Tutorial",
    items: [
      { title: "Tutorial Overview", path: "/tutorial" },
      { title: "1. Install", path: "/tutorial/install" },
      { title: "2. Your First File", path: "/tutorial/first-file" },
      { title: "3. Annotations & Inference", path: "/tutorial/annotations" },
      { title: "4. Attrsets & Structure", path: "/tutorial/attrsets" },
      { title: "5. Gradual Typing", path: "/tutorial/gradual" },
      { title: "6. Typing Existing .nix", path: "/tutorial/declarations" },
      { title: "7. Generics & HKT", path: "/tutorial/generics" },
      { title: "8. Conditional Types", path: "/tutorial/conditional-types" },
      { title: "9. Flakes & Packages", path: "/tutorial/flakes-and-packages" },
      { title: "10. Editor Setup", path: "/tutorial/editor" },
      { title: "11. Projects", path: "/tutorial/projects" },
      { title: "12. CI", path: "/tutorial/ci" },
      { title: "13. Effects", path: "/tutorial/effects" },
      { title: "14. Linearity & Captures", path: "/tutorial/linear-types" },
      { title: "15. Dependent & Opaque Types", path: "/tutorial/dependent-types" },
      { title: "16. Macros", path: "/tutorial/macros" },
    ],
  },
  {
    title: "Guides",
    items: [
      { title: "Adopting tynix", path: "/migration" },
      { title: "Editors", path: "/editors" },
      { title: "Troubleshooting", path: "/troubleshooting" },
    ],
  },
  {
    title: "Reference",
    items: [
      { title: "Language Reference", path: "/language-reference" },
      { title: "Grammar", path: "/grammar" },
      { title: "Type System", path: "/type-system" },
      { title: "How Checking Works", path: "/reference/type-system-internals" },
      { title: "Effects, Linearity & Macros", path: "/reference/advanced-types" },
      { title: "CLI", path: "/reference/cli" },
      { title: "Configuration", path: "/reference/config" },
      { title: "Builtins & Registry", path: "/reference/builtins" },
      { title: "Diagnostics", path: "/diagnostics" },
    ],
  },
  {
    title: "Project",
    items: [
      { title: "Language Design", path: "/language-design" },
      { title: "Architecture", path: "/architecture" },
      { title: "Roadmap", path: "/roadmap" },
      { title: "CI/CD", path: "/ci-cd" },
      { title: "Docs Site", path: "/docs-site" },
      { title: "Brand", path: "/brand" },
    ],
  },
];

const theme = defineTheme({
  colors: themeColors.light,
  darkColors: themeColors.dark,
  fonts,
  // Code colours are theme tokens: the native highlighter and the tynix pass
  // in docs/.vite/highlight.ts both read `--octc-syntax-*`.
  tokens: codeTokens,
  darkTokens: codeTokens,
  layout: {
    sidebarWidth: "264px",
    maxContentWidth: "736px",
  },
  header: {
    logo: "/tynix-logo.svg",
    logoWidth: 30,
    logoHeight: 30,
    showSiteNameText: true,
  },
  footer: {
    message: "Gradual types for Nix.",
    copyright: 'Released under the MIT license · <a href="https://github.com/ubugeeei-prod/tynix">GitHub</a>',
  },
  socialLinks: {
    github: "https://github.com/ubugeeei-prod/tynix",
  },
  embed: {
    head: headHtml,
  },
  css: themeCss,
  js: themeJs,
});

// Ox Content 3 highlights with a native tree-sitter engine that has no tynix
// grammar and no hook to register one. Highlight ```tynix fences (and frame
// every code block with a title bar and copy button) once the SSG step has
// written the pages.
function highlightCodeBlocks(): Plugin {
  return {
    name: "tynix-docs:highlight-code-blocks",
    apply: "build",
    closeBundle: {
      order: "post",
      sequential: true,
      async handler() {
        const pages = await processSite(resolve(outDir));
        this.info(`highlighted code blocks in ${pages} pages`);
      },
    },
  };
}

// Vite copies `docs/public` into the output directory while bundling the
// placeholder entry; the SSG step then writes pages next to those files.
// The placeholder entry itself is not part of the site, so drop it.
function dropBuildEntry(): Plugin {
  return {
    name: "tynix-docs:drop-build-entry",
    apply: "build",
    closeBundle: {
      order: "post",
      sequential: true,
      async handler() {
        await rm(resolve(outDir, "docs"), { recursive: true, force: true });
      },
    },
  };
}

export default defineConfig({
  publicDir: "docs/public",
  build: {
    outDir,
    // Start from an empty directory so stale pages never ship, then let the
    // public assets (_headers, _redirects, install.sh, brand files) and the
    // SSG pages land side by side. Do not enable `ssg.clean`: it runs after
    // Vite has copied the public directory and would delete it again.
    emptyOutDir: true,
    rollupOptions: {
      input: "docs/.vite/entry.html",
    },
  },
  plugins: [
    oxContent({
      srcDir: "docs",
      outDir,
      base: "/",
      docs: false,
      embeds: false,
      highlight: true,
      codeAnnotations: { notation: "both" },
      search: true,
      ssg: {
        clean: false,
        siteName: "tynix",
        siteUrl,
        ogImage: `${siteUrl}/og-image.png`,
        theme,
        navigation,
      },
    }),
    dropBuildEntry(),
    highlightCodeBlocks(),
  ],
});
