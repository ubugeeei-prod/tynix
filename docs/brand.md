---
title: Brand
description: The tynix brand system, covering positioning, voice, name usage, logo, color, typography and code theme.
---

# Brand

This page is the source of truth for how tynix looks and sounds. The tokens on
this page are implemented in
[`docs/.vite/brand.ts`](https://github.com/ubugeeei-prod/tynix/blob/main/docs/.vite/brand.ts),
and the asset files live in
[`docs/public/brand/`](https://github.com/ubugeeei-prod/tynix/tree/main/docs/public/brand).
Change both together.

## Positioning

**Gradual types for Nix.** Add types where they help, keep the rest dynamic,
and ship plain `.nix` with zero runtime.

tynix is for people who already write Nix and are tired of finding attribute
typos at evaluation time. It takes the approach TypeScript took with
JavaScript: an optional, gradual, structural type layer with first-class editor
support, which compiles away completely.

| Aspect | Description |
| --- | --- |
| **Category** | gradual type system and toolchain for Nix |
| **Audience** | Nix users maintaining flakes, packages, modules and shared libraries |
| **Promise** | catch shape errors before evaluation, without changing what Nix runs |
| **Proof points** | erasure-only compiler, structural records, `.d.tynix` declarations, LSP, stable diagnostic codes |
| **Not** | a new Nix, a runtime contract system, or a replacement for the module system's option types |

### Taglines

- Headline: **Gradual types for Nix.**
- Supporting line: *Add types where they help, keep the rest dynamic, and ship
  plain `.nix` with zero runtime.*
- Short: **Type your Nix. Ship plain Nix.**

Use the headline with the supporting line on the home page, the README and the
OG image, and the headline alone in package descriptions and page titles. The
short one fits badges, social bios and slide footers. "TypeScript-like" is fine
as a comparison in body copy that explains the approach; it is not a tagline.

## Voice and tone

tynix speaks like a careful senior engineer reviewing your pull request:
precise, calm, and on your side.

- **Concrete over clever.** Show the code and the exact diagnostic. "Selecting
  `pkg.pname` from an unannotated parameter fails with `TC0009`" beats "tynix
  catches tricky bugs".
- **Honest about limits.** Say what is not supported yet and what to do
  instead. Mark upcoming features as upcoming.
- **Respect Nix.** tynix adds to Nix; it does not fix it. Never frame Nix as
  broken or tynix as a replacement.
- **Short sentences, active voice, second person.** "Annotate the parameter",
  not "the parameter should be annotated by the user".
- **No hype words.** Avoid "blazing", "magical", "revolutionary",
  "seamless". Measurable claims only.
- **Diagnostics are part of the voice.** Error messages state what was found and
  what was expected, in that order, with the code first:
  `[TC0013] type mismatch: 42 vs String`.

## Name usage

- The name is always lowercase **`tynix`**, including at the start of a
  sentence and in titles: "tynix checks records structurally."
- Never `TyNix`, `TYNIX`, `Ty-Nix` or `Tynix`. Not abbreviated, not pluralized.
- Pronounce it "tie-nix": **ty**ped **Nix**.
- In running text, use plain `tynix`. Use code formatting (`` `tynix` ``) only
  when you mean the command or the binary.
- File types are written with their dot: `.tynix`, `.d.tynix`, `.nix`.
- Nix is capitalized when you mean the language or package manager, as the Nix
  project does.

## Logo

The mark brings together the two languages tynix sits between:

- **Nix**: six lambdas interlock into a hexagonal ring, after the Nix lambda
  snowflake. They are woven, each lying over the next, so the ring has no
  start or end.
- **Haskell**: every stroke is cut flat, like the slanted bars of the Haskell
  logo, and the arms alternate Nix blues with Haskell purples.
- **tynix**: the ring holds `::`, the type annotation tynix shares with Haskell,
  drawn as two slanted bars in Annotation Amber, the color the docs use for
  erased type syntax.

The geometry lives in `scripts/generate-brand.ts`; edit it there and run
`vp run generate:brand` to regenerate every SVG and PNG below.

<div class="tx-logos">
<figure class="on-light"><img src="/brand/tynix-logo.svg" alt="tynix logo, color, for light backgrounds"><figcaption>tynix-logo.svg</figcaption></figure>
<figure class="on-dark"><img src="/brand/tynix-logo-dark.svg" alt="tynix logo, color, for dark backgrounds"><figcaption>tynix-logo-dark.svg</figcaption></figure>
<figure class="on-light"><img src="/brand/tynix-logo-mono.svg" alt="tynix logo, single color ink"><figcaption>tynix-logo-mono.svg</figcaption></figure>
<figure class="on-dark"><img src="/brand/tynix-logo-white.svg" alt="tynix logo, single color white"><figcaption>tynix-logo-white.svg</figcaption></figure>
<figure class="on-light"><img src="/brand/tynix-mark.svg" alt="tynix mark, color"><figcaption>tynix-mark.svg</figcaption></figure>
<figure class="on-dark"><img src="/brand/tynix-mark-dark.svg" alt="tynix mark, color, for dark backgrounds"><figcaption>tynix-mark-dark.svg</figcaption></figure>
<figure class="on-dark"><img src="/brand/tynix-mark-white.svg" alt="tynix mark, white"><figcaption>tynix-mark-white.svg</figcaption></figure>
<figure class="on-light"><img src="/brand/tynix-app-icon.svg" alt="tynix app icon"><figcaption>tynix-app-icon.svg</figcaption></figure>
</div>

| File | Use |
| --- | --- |
| `brand/tynix-logo.svg` | default lockup on light backgrounds |
| `brand/tynix-logo-dark.svg` | lockup on dark backgrounds |
| `brand/tynix-logo-mono.svg`, `brand/tynix-logo-white.svg` | one-color print, embossing, single-color UIs |
| `brand/tynix-mark.svg`, `brand/tynix-mark-dark.svg` | the mark alone on light / dark backgrounds |
| `brand/tynix-mark-mono.svg`, `brand/tynix-mark-white.svg` | one-color mark |
| `brand/tynix-app-icon.svg` | the mark on its ink tile: app icons, the docs header, the favicon |
| `brand/tynix-wordmark.svg` | the wordmark alone, where the mark already appears nearby |
| `brand/tynix-mark-512.png` | raster app icon for places that do not accept SVG |
| `favicon.svg`, `apple-touch-icon.png` | browser and home-screen icons |
| `og-image.png` (source `brand/og-image.svg`) | 1200 × 630 social preview |

Rules:

- Keep clear space around the logo of at least one stroke width on all
  sides. Do not place it on busy imagery.
- Minimum size: 16 px for the app icon, 24 px for the bare mark, 20 px tall
  for the lockup.
- Do not recolor, rotate, outline, add shadows to, or re-typeset the wordmark.
  The wordmark is drawn, not set in a font; use the files.
- On photos or saturated backgrounds, use the white one-color version.

## Color

The palette comes straight from the mark. **Nix blues** carry links, buttons
and focus; **Haskell purples** appear in the mark and
"important" callouts. **Annotation Amber** has one job: it marks type-only
syntax, everything the compiler erases. Surfaces are cool neutrals around the
mark's ink tile, never cream or pure black.

### Core

<div class="tx-swatches">
<div class="tx-swatch"><i style="background:#5277C3"></i><b>Nix Blue</b><code>#5277C3</code></div>
<div class="tx-swatch"><i style="background:#3F65B5"></i><b>Nix Deep</b><code>#3F65B5</code></div>
<div class="tx-swatch"><i style="background:#2F4F96"></i><b>Nix Deeper</b><code>#2F4F96</code></div>
<div class="tx-swatch"><i style="background:#7EBAE4"></i><b>Snow Blue</b><code>#7EBAE4</code></div>
<div class="tx-swatch"><i style="background:#5E5086"></i><b>Haskell Purple</b><code>#5E5086</code></div>
<div class="tx-swatch"><i style="background:#8F4E8B"></i><b>Haskell Magenta</b><code>#8F4E8B</code></div>
<div class="tx-swatch"><i style="background:#453A62"></i><b>Haskell Ink</b><code>#453A62</code></div>
<div class="tx-swatch"><i style="background:#F2B441"></i><b>Annotation Amber</b><code>#F2B441</code></div>
<div class="tx-swatch"><i style="background:#8A5A00"></i><b>Amber 700</b><code>#8A5A00</code></div>
</div>

### Neutrals

<div class="tx-swatches">
<div class="tx-swatch"><i style="background:#0A0E1A"></i><b>Ink 950</b><code>#0A0E1A</code></div>
<div class="tx-swatch"><i style="background:#0E1424"></i><b>Ink 900 (tile)</b><code>#0E1424</code></div>
<div class="tx-swatch"><i style="background:#131B2E"></i><b>Ink 850</b><code>#131B2E</code></div>
<div class="tx-swatch"><i style="background:#27324B"></i><b>Ink 700</b><code>#27324B</code></div>
<div class="tx-swatch"><i style="background:#505B74"></i><b>Slate 600</b><code>#505B74</code></div>
<div class="tx-swatch"><i style="background:#9AA6BF"></i><b>Slate 300</b><code>#9AA6BF</code></div>
<div class="tx-swatch"><i style="background:#DCE1EB"></i><b>Line</b><code>#DCE1EB</code></div>
<div class="tx-swatch"><i style="background:#EDF0F6"></i><b>Mist</b><code>#EDF0F6</code></div>
<div class="tx-swatch"><i style="background:#F7F8FB"></i><b>Paper</b><code>#F7F8FB</code></div>
</div>

### Theme tokens

| Token | Light | Dark |
| --- | --- | --- |
| `primary` | Nix Deep `#3F65B5` | `#93B4EE` |
| `primaryHover` | Nix Deeper `#2F4F96` | `#B9CEF5` |
| `background` | Paper `#F7F8FB` | Ink 950 `#0A0E1A` |
| `backgroundAlt` | Mist `#EDF0F6` | Ink 850 `#131B2E` |
| `text` | `#141A2B` | `#E4E9F4` |
| `textMuted` | Slate 600 `#505B74` | Slate 300 `#9AA6BF` |
| `border` | Line `#DCE1EB` | Ink 700 `#27324B` |
| `codeBackground` | Ink 900 `#0E1424` | Ink 900 `#0E1424` |

The home page hero and the closing call to action sit on an Ink 900 band in
both schemes, the same tile the app icon uses. Its primary button is Annotation
Amber with Ink 950 text.

### Contrast

All text pairings meet WCAG 2.2 AA (4.5:1 for body text); most meet AAA.

| Pair | Ratio |
| --- | --- |
| text `#141A2B` on Paper | 16.31:1 |
| Slate 600 on Paper | 6.40:1 |
| Nix Deep on Paper (links) | 5.29:1 |
| white on Nix Deep (buttons) | 5.61:1 |
| Amber 700 on Paper | 5.58:1 |
| `#E4E9F4` on Ink 950 | 15.83:1 |
| Slate 300 on Ink 950 | 7.87:1 |
| `#93B4EE` on Ink 950 (dark-mode links) | 9.17:1 |
| Annotation Amber on Ink 900 (code) | 9.95:1 |
| comment `#7482A0` on Ink 900 (code) | 4.76:1 |

Annotation Amber is for dark surfaces. On light surfaces, use Amber 700
`#8A5A00` for text.

## Typography

| Role | Typeface | Fallback stack |
| --- | --- | --- |
| Headings | IBM Plex Sans 500 to 600 | `system-ui`, -apple-system, Segoe UI, sans-serif |
| UI and prose | [IBM Plex Sans](https://fonts.google.com/specimen/IBM+Plex+Sans) 400, 500, 600, italic 400 | `system-ui`, -apple-system, Segoe UI, sans-serif |
| Code | [JetBrains Mono](https://fonts.google.com/specimen/JetBrains+Mono) 400, 500, 600 | `ui-monospace`, SF Mono, Menlo, Consolas, monospace |

Both typefaces are open source (SIL OFL) and served from Google Fonts.
Headings share the prose typeface, using weight and spacing to establish a
hierarchy. Body text is IBM Plex Sans 400 at 16 px with a 1.7 line height and
a measure of about 46 rem. Code is JetBrains Mono at 13 px without ligatures,
so `->` and `::` read exactly as typed. The wordmark is custom drawn.

## Code theme: tynix ink

Code blocks are always Ink 900 so the semantic colors work the same in both
site themes. The theme, `tynix ink`, follows one idea: **amber is for anything
the compiler erases.** The other colors come from the mark: purples for
keywords, blues for names and values.

| Token | Color |
| --- | --- |
| type-only syntax: `::`, `type`, `declare`, `forall`, `extends`, `infer`, `as`, type names | Annotation Amber `#F2B441` |
| gradual types: `dynamic`, `unknown`, `any` | Annotation Amber, italic |
| keywords: `let`, `in`, `if`, `with`, `rec`, `inherit`, `import` | `#C4ABF0` |
| strings and paths | `#93D6B5` |
| attribute names, numbers, `true`, `false`, `null` | `#93B8F2` |
| lambda parameters | `#E6B3DD` |
| comments | `#7482A0`, italic |
| `# @tynix-ignore` / `# @tynix-expected` | Snow `#A9D3F0`, semibold |
| operators and punctuation | `#8C97B2` |
| plain text | `#D6DDEC` |

The colors are Ox Content theme tokens (`--octc-syntax-*`), so blocks the
built-in highlighter paints (nix, bash, json, yaml, ts) use the same palette.

```tynix
type Package = { pname :: String; version :: String; };

let
  # @tynix-ignore
  describe = (pkg :: Package): "${pkg.pname}-${pkg.version}";
  hello = { pname = "hello"; version = "2.12.1"; } as Package;
in describe hello
```

The TextMate grammar used for docs highlighting lives in
[`docs/.vite/tynix-grammar.ts`](https://github.com/ubugeeei-prod/tynix/blob/main/docs/.vite/tynix-grammar.ts),
and [`docs/.vite/highlight.ts`](https://github.com/ubugeeei-prod/tynix/blob/main/docs/.vite/highlight.ts)
applies it after the site is built. Editors get semantic highlighting from the
language server instead.

## Imagery and layout

- Use typography, spacing and thin rules to organize documentation. Keep
  backgrounds flat, with small corner radii for code and controls.
- The home page leads with installation and a working code example. Group
  reference links as lists, with clear labels and descriptions.
- Show the current page in the sidebar. Long articles provide section links
  in a side index on wide screens and a disclosure within the article on
  smaller screens.
- Reserve the mark for identity and the syntax palette for code. Avoid
  decorative gradients, oversized logos and repeated promotional panels.
- Diagrams use the theme's surface colors with Nix blue for "process",
  Amber for "type-level" and Haskell purple for "output".
- Prefer real code and real diagnostics over illustrations.
