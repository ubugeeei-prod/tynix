---
layout: entry
title: Gradual types for Nix
description: Add types where they help, keep the rest dynamic, and ship plain .nix with zero runtime. tynix is a gradual, structural type system for Nix.
---

<section class="tx-hero">
<div class="tx-wrap">
<div class="tx-hero__grid">
<div>
<h1 class="tx-hero__title">Gradual types for Nix.</h1>
<p class="tx-hero__lede">Add types where they help, keep the rest dynamic, and ship plain <code>.nix</code> with zero runtime.</p>
<div class="tx-install">
<div class="tx-install__row"><span class="tx-install__label">Installer</span><code>curl -fsSL https://tynix.dev/install.sh | sh</code><button type="button" class="tx-copy" data-copy="curl -fsSL https://tynix.dev/install.sh | sh" aria-label="Copy the installer command"><span>Copy</span></button></div>
<div class="tx-install__row"><span class="tx-install__label">Nix flake</span><code>nix profile install github:ubugeeei-prod/tynix</code><button type="button" class="tx-copy" data-copy="nix profile install github:ubugeeei-prod/tynix" aria-label="Copy the nix profile command"><span>Copy</span></button></div>
</div>
<div class="tx-actions">
<a class="tx-btn tx-btn--primary" href="./tutorial/index.md">Start the tutorial</a>
<a class="tx-btn" href="./language-reference.md">Read the reference</a>
</div>
</div>
<div class="tx-hero__mark"><img src="/brand/tynix-mark-dark.svg" alt="The tynix mark: six interlocking lambdas around an amber double colon" width="360" height="360"></div>
</div>
<div class="tx-demo">
<div class="tx-demo__panes">
<div>

```tynix [greet.tynix]
type User = { name :: String; admin :: Bool; };

let
  greet :: User -> String;
  greet = (user :: User):
    if user.admin
    then "Welcome back, ${user.name}."
    else "Hello, ${user.name}!";
in greet { name = "Ada"; admin = true; }
```

</div>
<div class="tx-demo__arrow" aria-label="tynix compile"><svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.75" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M4 12h15M13 6l6 6-6 6"/></svg></div>
<div>

```nix [greet.nix]
let
  greet = user: if user.admin
  then "Welcome back, ${user.name}."
  else "Hello, ${user.name}!";
in greet {
  name = "Ada";
  admin = true;
}
```

</div>
</div>
<p class="tx-demo__caption"><b>Amber</b> is type-only syntax: tynix checks it, then erases it. Misspell <code>user.nmae</code> and you get <code>[TC0009] missing field `nmae`</code> before Nix evaluates anything.</p>
</div>
</div>
</section>

<section class="tx-section">
<div class="tx-wrap">
<h2 class="tx-section__title">Types that fit the Nix you already write</h2>
<p class="tx-section__lede"><code>.tynix</code> is Nix plus annotations. Attribute sets, lambdas, <code>with</code>, <code>rec</code>, <code>inherit</code> and imports work exactly as they do in Nix, and the type layer never changes what runs.</p>
<div class="tx-features">
<a class="tx-feature" href="./reference/type-system-internals.md">
<svg viewBox="0 0 32 32" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M10 6c-3 0-4 1.5-4 4v3c0 2-1 3-3 3 2 0 3 1 3 3v3c0 2.5 1 4 4 4M22 6c3 0 4 1.5 4 4v3c0 2 1 3 3 3-2 0-3 1-3 3v3c0 2.5-1 4-4 4"/><path class="a" d="M14.5 12.5l-1.5 3M14.5 18.5l-1.5 3M19.5 12.5l-1.5 3M19.5 18.5l-1.5 3"/></svg>
<h3>Zero runtime</h3>
<p>Annotations, aliases, declarations and casts are erased at build time. The output is ordinary Nix with the same layout and semantics.</p>
</a>
<a class="tx-feature" href="./tutorial/attrsets.md">
<svg viewBox="0 0 32 32" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><rect x="5" y="5" width="22" height="22" rx="4"/><path d="M10 11h5M10 16h5M10 21h5"/><path class="a" d="M18 11h4M18 16h4M18 21h2"/></svg>
<h3>Structural records</h3>
<p>Attribute sets are compared by shape, with width subtyping, literal types, unions and errors that name the field.</p>
</a>
<a class="tx-feature" href="./tutorial/gradual.md">
<svg viewBox="0 0 32 32" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M5 26h22"/><path d="M8 22v-4M14 22v-8"/><path class="a" d="M20 22V10M26 22V6"/></svg>
<h3>Gradual by design</h3>
<p><code>dynamic</code>, <code>unknown</code> and <code>any</code> are three distinct escape hatches. Adopt one file at a time and tighten as you go.</p>
</a>
<a class="tx-feature" href="./tutorial/declarations.md">
<svg viewBox="0 0 32 32" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M8 4h11l6 6v18H8z"/><path d="M19 4v6h6"/><path class="a" d="M12 17h2M12 22h2M17 17h4M17 22h4"/></svg>
<h3>Type existing .nix</h3>
<p>Describe modules you will not rewrite with <code>.d.tynix</code> declarations, the way DefinitelyTyped describes JavaScript.</p>
</a>
<a class="tx-feature" href="./reference/advanced-types.md">
<svg viewBox="0 0 32 32" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M11 7l-7 9 7 9M21 7l7 9-7 9"/><path class="a" d="M14 21l4-10M13 14h6"/></svg>
<h3>Types that say more</h3>
<p>Hindley-Milner with higher-rank and higher-kinded types, effects such as <code>! { Trace }</code>, linear arrows, dependent and opaque types, and hygienic, typed macros.</p>
</a>
<a class="tx-feature" href="./tutorial/editor.md">
<svg viewBox="0 0 32 32" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><rect x="4" y="6" width="24" height="20" rx="3"/><path d="M4 11h24"/><path class="a" d="M10 18.5l3 3 6-6"/></svg>
<h3>Editor and CI ready</h3>
<p>A language server with hover, inlay hints and diagnostics, plus stable diagnostic codes and JSON reports for CI.</p>
</a>
</div>
</div>
</section>

<section class="tx-section">
<div class="tx-wrap">
<h2 class="tx-section__title">How it works</h2>
<p class="tx-section__lede">One binary checks your files and compiles them back to Nix. Your flake, your builds and <code>nix eval</code> only ever see <code>.nix</code>.</p>
<div class="tx-flow">
<div class="tx-flow__step">
<h3>Write</h3>
<p>Annotate a <code>.tynix</code> file, or describe a <code>.nix</code> file you keep with a <code>.d.tynix</code> declaration.</p>

```bash
tynix init
```

</div>
<div class="tx-flow__step">
<h3>Check</h3>
<p>Type-check one file or the whole project. Every diagnostic has a stable code and a precise location.</p>

```bash
tynix check-project
```

</div>
<div class="tx-flow__step">
<h3>Ship</h3>
<p>Erase every type and emit plain <code>.nix</code> next to the declarations other projects can import.</p>

```bash
tynix build
```

</div>
</div>
<ul class="tx-editors" aria-label="Supported editors">
<li>VS Code</li>
<li>Cursor</li>
<li>VSCodium</li>
<li>Zed</li>
<li>Neovim</li>
<li>Helix</li>
</ul>
<p class="tx-section__lede" style="margin: 1rem 0 0 !important">Run <code>tynix ide install</code> to set up the language server in any of them. See <a href="./editors.md">Editors</a> for what each one gets.</p>
</div>
</section>

<section class="tx-section">
<div class="tx-wrap">
<div class="tx-cta">
<div>
<h2>Ready in fifteen minutes</h2>
<p>Install tynix, type your first file, and finish with a checked flake.</p>
</div>
<div><a class="tx-btn tx-btn--primary" href="./tutorial/index.md">Start the tutorial</a></div>
</div>
</div>
</section>
