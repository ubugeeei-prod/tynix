---
layout: entry
title: Gradual types for Nix
description: Add types where they help, keep the rest dynamic, and ship plain .nix with zero runtime. tynix is a gradual, structural type system for Nix.
---

<section class="tx-intro">
<div class="tx-wrap">
<div class="tx-intro__grid">
<div>
<h1>Gradual types for Nix.</h1>
<p class="tx-intro__lede">Add types where they help, keep the rest dynamic, and ship plain <code>.nix</code> with zero runtime.</p>
<p class="tx-intro__links"><a href="./tutorial/index.md">Start the tutorial →</a><a href="./language-reference.md">Language reference</a></p>
</div>
<div class="tx-install">
<h2>Install tynix</h2>
<div class="tx-install__row">
<div class="tx-install__heading"><span class="tx-install__label">Shell installer</span><button type="button" class="tx-copy" data-copy="curl -fsSL https://tynix.dev/install.sh | sh" aria-label="Copy the installer command"><span>Copy</span></button></div>
<code>curl -fsSL https://tynix.dev/install.sh | sh</code>
</div>
<div class="tx-install__row">
<div class="tx-install__heading"><span class="tx-install__label">Nix flake</span><button type="button" class="tx-copy" data-copy="nix profile install github:ubugeeei-prod/tynix" aria-label="Copy the nix profile command"><span>Copy</span></button></div>
<code>nix profile install github:ubugeeei-prod/tynix</code>
</div>
<p class="tx-install__help">See <a href="./tutorial/install.md">installation</a> for supported platforms and verification.</p>
</div>
</div>
<div class="tx-example">
<div class="tx-example__panes">
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
<p class="tx-example__caption">Type annotations appear in amber. <code>tynix compile</code> checks and erases them, leaving ordinary Nix. A misspelling such as <code>user.nmae</code> produces <code>[TC0009] missing field `nmae`</code>.</p>
</div>
</div>
</section>

<section class="tx-section">
<div class="tx-wrap tx-section__grid">
<div>
<h2>The type system</h2>
<p class="tx-section__intro">Nix syntax and evaluation, with a type layer you can adopt one file at a time.</p>
</div>
<dl class="tx-topics">
<div><dt><a href="./tutorial/attrsets.md">Structural records</a></dt><dd>Compare attribute sets by shape. Use width subtyping, literal types and unions, with diagnostics that name the field.</dd></div>
<div><dt><a href="./tutorial/gradual.md">Gradual typing</a></dt><dd>Choose <code>dynamic</code>, <code>unknown</code> or <code>any</code> at an untyped boundary. Add annotations as the surrounding code becomes known.</dd></div>
<div><dt><a href="./tutorial/declarations.md">Existing .nix files</a></dt><dd>Describe modules with <code>.d.tynix</code> declarations without rewriting their implementation.</dd></div>
<div><dt><a href="./reference/advanced-types.md">Advanced types</a></dt><dd>Higher-rank and higher-kinded types, effect rows, linear arrows, dependent and opaque types, and hygienic typed macros.</dd></div>
</dl>
</div>
</section>

<section class="tx-section">
<div class="tx-wrap tx-section__grid">
<div>
<h2>Working with a project</h2>
<p class="tx-section__intro">One toolchain for the command line, your editor and CI.</p>
</div>
<div class="tx-section__body">
<ul class="tx-command-list">
<li><code>tynix init</code><span>Create the project configuration.</span></li>
<li><code>tynix check-project</code><span>Check sources and report diagnostics.</span></li>
<li><code>tynix build</code><span>Emit plain <code>.nix</code> and type declarations.</span></li>
<li><code>tynix ide install</code><span>Set up the language server.</span></li>
</ul>
<p>The language server provides hover, inlay hints and diagnostics. See <a href="./editors.md">editor support</a> for VS Code, Cursor, VSCodium, Zed, Neovim and Helix, or <a href="./tutorial/ci.md">run the checker in CI</a>.</p>
</div>
</div>
</section>

<section class="tx-section">
<div class="tx-wrap tx-section__grid">
<div><h2>Documentation</h2></div>
<dl class="tx-topics">
<div><dt><a href="./tutorial/index.md">Tutorial</a></dt><dd>Install the tools, annotate a first file, and work through a checked flake.</dd></div>
<div><dt><a href="./language-reference.md">Language reference</a></dt><dd>Look up syntax, annotations, declarations and type-system behavior.</dd></div>
<div><dt><a href="./support-matrix.md">Support matrix</a></dt><dd>Check supported features and current limitations before adopting tynix.</dd></div>
</dl>
</div>
</section>
