// Generate the tynix mark and every asset derived from it.
//
// The mark is six interlocking lambdas woven into a hexagonal ring, after the
// Nix lambda snowflake. Its strokes are cut flat like the slanted bars of the
// Haskell logo, the arms alternate Nix blues and Haskell purples, and the
// centre holds `::`, the type annotation tynix and Haskell share.
//
//   node --experimental-strip-types ./scripts/generate-brand.ts
//
// SVG files are written directly. PNG renders (app icon, touch icon, OG image,
// VS Code icon) are made with `rsvg-convert` when it is on PATH.
import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync } from "node:fs";

const root = new URL("../", import.meta.url).pathname;
const C = 256;

// Lambda geometry (in a 512 x 512 design space).
const H = 98; // half height of a lambda
const W = 50; // stroke width
const OFFSET: [number, number] = [-50, -150]; // lambda position before rotation
const TURN = 90; // lambda rotation before it is placed
const WEAVE: [number, number] = [180, 240]; // wedge where the first lambda goes over the last

type Point = [number, number];

const palettes = {
  // Nix blue, Haskell purple, Nix snow blue, Haskell magenta, Nix deep blue, Haskell ink.
  light: ["#5277C3", "#5E5086", "#7EBAE4", "#8F4E8B", "#3F65B5", "#453A62"],
  dark: ["#7EA0E6", "#9B8BD0", "#A9D3F0", "#C58AC1", "#6E91DA", "#8577B8"],
};
const AMBER = "#F2B441";
const INK = "#0E1525";
const TILE = "#0E1424";

function lambda(): Point[][] {
  const s = H * Math.tan(Math.PI / 6);
  const longLeg: Point[] = [
    [-s - W / 2, -H],
    [-s + W / 2, -H],
    [s + W / 2, H],
    [s - W / 2, H],
  ];
  const shortLeg: Point[] = [
    [-W / 2, 0],
    [W / 2, 0],
    [-s + W / 2, H],
    [-s - W / 2, H],
  ];
  return [longLeg, shortLeg];
}

function rotate([x, y]: Point, degrees: number): Point {
  const a = (degrees * Math.PI) / 180;
  return [x * Math.cos(a) - y * Math.sin(a), x * Math.sin(a) + y * Math.cos(a)];
}

function place(p: Point, arm: number): Point {
  const [x, y] = rotate(p, TURN);
  const [rx, ry] = rotate([x + OFFSET[0], y + OFFSET[1]], 60 * arm);
  return [C + rx, C + ry];
}

const arms: Point[][][] = [0, 1, 2, 3, 4, 5].map((arm) =>
  lambda().map((leg) => leg.map((p) => place(p, arm))),
);

function d(points: Point[]): string {
  return (
    "M" +
    points.map(([x, y]) => `${x.toFixed(1)} ${y.toFixed(1)}`).join(" L") +
    " Z"
  );
}

function colon(cx: number, cy: number, fill: string): string {
  const size = 52;
  const gap = 50;
  const slant = 22;
  return [-gap, gap]
    .map((dy) => {
      const y0 = cy + dy - size / 2;
      const y1 = cy + dy + size / 2;
      return `<path d="${d([
        [cx - size / 2 + slant / 2, y0],
        [cx + size / 2 + slant / 2, y0],
        [cx + size / 2 - slant / 2, y1],
        [cx - size / 2 - slant / 2, y1],
      ])}" fill="${fill}"/>`;
    })
    .join("");
}

function wedge([a0, a1]: [number, number]): string {
  const points: Point[] = [[C, C]];
  for (let a = a0; a <= a1; a += 5) {
    const r = (a * Math.PI) / 180;
    points.push([C + 900 * Math.cos(r), C + 900 * Math.sin(r)]);
  }
  return d(points);
}

/** The mark as a group filling a 512 x 512 box with `margin` on each side. */
function markGroup(
  colors: string[],
  colonFill: string,
  margin: number,
  id: string,
): string {
  const all = arms.flat(2);
  const xs = all.map(([x]) => x);
  const ys = all.map(([, y]) => y);
  const span = Math.max(
    Math.max(...xs) - Math.min(...xs),
    Math.max(...ys) - Math.min(...ys),
  );
  const scale = (512 - 2 * margin) / span;
  const cx = (Math.max(...xs) + Math.min(...xs)) / 2;
  const cy = (Math.max(...ys) + Math.min(...ys)) / 2;
  const legs = arms
    .map((arm, k) =>
      arm.map((leg) => `<path d="${d(leg)}" fill="${colors[k]}"/>`).join(""),
    )
    .join("");
  // Every lambda lies on the previous one; painting in order gets that right
  // except where the first meets the last, so the first is repainted there.
  const first = arms[0]
    .map((leg) => `<path d="${d(leg)}" fill="${colors[0]}"/>`)
    .join("");
  const weave = `<clipPath id="${id}-weave"><path d="${wedge(WEAVE)}"/></clipPath><g clip-path="url(#${id}-weave)">${first}</g>`;
  return `<g transform="translate(256 256) scale(${scale.toFixed(4)}) translate(${(-cx).toFixed(2)} ${(-cy).toFixed(2)})">${legs}${weave}${colon(cx, cy, colonFill)}</g>`;
}

function svg(
  body: string,
  viewBox: string,
  width: number,
  height: number,
  title = "tynix",
): string {
  return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="${viewBox}" width="${width}" height="${height}" role="img" aria-label="${title}">\n  <title>${title}</title>\n  ${body}\n</svg>\n`;
}

const mark = (colors: string[], colonFill = AMBER, id = "tynix") =>
  markGroup(colors, colonFill, 16, id);
const tile = (id: string) =>
  `<rect width="512" height="512" rx="116" fill="${TILE}"/>${markGroup(palettes.dark, AMBER, 76, id)}`;

// The wordmark is drawn, not set: rounded strokes in the 194 x 64 logo space,
// to the right of the 64 x 64 mark. The `y` reuses the slant of the `x`; its
// tail drops below the baseline.
const LOGO_WIDTH = 194;
function wordmark(color: string): string {
  const t = "M84 15.5v23.5q0 7.5 7.5 7.5h2M77.5 26.5h15";
  const y = "M103 26.5l8.2 20M120 26.5l-13 29.5";
  const n = "M131 26.5v20M131 35q0-8.5 8.5-8.5t8.5 8.5v11.5";
  const i = "M159 28v18.5";
  const x = "M170.5 27l17 19.5M187.5 27l-17 19.5";
  const paths = [t, y, n, i, x].map((d) => `<path d="${d}"/>`).join("");
  return `<g fill="none" stroke="${color}" stroke-width="6.5" stroke-linecap="round" stroke-linejoin="round">${paths}</g><circle cx="159" cy="17" r="3.9" fill="${color}"/>`;
}

/** A mark scaled into the 64 x 64 square at the left of the logo. */
const logoMark = (inner: string) => `<g transform="scale(0.125)">${inner}</g>`;

const files: Record<string, string> = {
  "docs/public/brand/tynix-mark.svg": svg(
    mark(palettes.light, AMBER, "m"),
    "0 0 512 512",
    64,
    64,
  ),
  "docs/public/brand/tynix-mark-dark.svg": svg(
    mark(palettes.dark, AMBER, "md"),
    "0 0 512 512",
    64,
    64,
  ),
  "docs/public/brand/tynix-mark-mono.svg": svg(
    mark(Array(6).fill(INK), INK, "mm"),
    "0 0 512 512",
    64,
    64,
  ),
  "docs/public/brand/tynix-mark-white.svg": svg(
    mark(Array(6).fill("#FFFFFF"), "#FFFFFF", "mw"),
    "0 0 512 512",
    64,
    64,
  ),
  "docs/public/brand/tynix-app-icon.svg": svg(
    tile("ai"),
    "0 0 512 512",
    512,
    512,
  ),
  "docs/public/brand/tynix-wordmark.svg": svg(
    wordmark(INK),
    `74 0 ${LOGO_WIDTH - 74} 64`,
    (LOGO_WIDTH - 74) * 2,
    128,
  ),
  "docs/public/brand/tynix-logo.svg": svg(
    logoMark(mark(palettes.light, AMBER, "l")) + wordmark(INK),
    `0 0 ${LOGO_WIDTH} 64`,
    LOGO_WIDTH * 2,
    128,
  ),
  "docs/public/brand/tynix-logo-dark.svg": svg(
    logoMark(mark(palettes.dark, AMBER, "ld")) + wordmark("#F4F7FC"),
    `0 0 ${LOGO_WIDTH} 64`,
    LOGO_WIDTH * 2,
    128,
  ),
  "docs/public/brand/tynix-logo-mono.svg": svg(
    logoMark(mark(Array(6).fill(INK), INK, "lm")) + wordmark(INK),
    `0 0 ${LOGO_WIDTH} 64`,
    LOGO_WIDTH * 2,
    128,
  ),
  "docs/public/brand/tynix-logo-white.svg": svg(
    logoMark(mark(Array(6).fill("#FFFFFF"), "#FFFFFF", "lw")) +
      wordmark("#FFFFFF"),
    `0 0 ${LOGO_WIDTH} 64`,
    LOGO_WIDTH * 2,
    128,
  ),
  // The site header and favicon use the tile: it reads on light and dark
  // backgrounds alike and stays legible at 16 px.
  "docs/public/tynix-logo.svg": svg(tile("h"), "0 0 512 512", 64, 64),
  "docs/public/favicon.svg": svg(tile("f"), "0 0 512 512", 64, 64),
  "editors/vscode/icons/icon.svg": svg(tile("v"), "0 0 512 512", 256, 256),
  "editors/vscode/icons/tynix-file-light.svg": svg(
    mark(palettes.light, "#C98A12", "fl"),
    "0 0 512 512",
    16,
    16,
  ),
  "editors/vscode/icons/tynix-file-dark.svg": svg(
    mark(palettes.dark, AMBER, "fd"),
    "0 0 512 512",
    16,
    16,
  ),
};

for (const [path, content] of Object.entries(files)) {
  writeFileSync(root + path, content);
  console.log(`wrote ${path}`);
}

// The OG card keeps its layout; only the logo group is regenerated.
const ogPath = root + "docs/public/brand/og-image.svg";
const og = readFileSync(ogPath, "utf8").replace(
  /<g transform="translate\(80 72\) scale\(1\.25\)">[\s\S]*?\n {2}<\/g>/,
  `<g transform="translate(80 72) scale(1.25)">${logoMark(mark(palettes.dark, AMBER, "og"))}${wordmark("#F4F7FC")}\n  </g>`,
);
writeFileSync(ogPath, og);
console.log("wrote docs/public/brand/og-image.svg");

function render(source: string, target: string, width: number): void {
  execFileSync("rsvg-convert", [
    "-w",
    String(width),
    root + source,
    "-o",
    root + target,
  ]);
  console.log(`rendered ${target}`);
}

try {
  execFileSync("rsvg-convert", ["--version"], { stdio: "ignore" });
  render(
    "docs/public/brand/tynix-app-icon.svg",
    "docs/public/brand/tynix-mark-512.png",
    512,
  );
  render(
    "docs/public/brand/tynix-app-icon.svg",
    "docs/public/apple-touch-icon.png",
    180,
  );
  render(
    "docs/public/brand/tynix-app-icon.svg",
    "editors/vscode/images/icon.png",
    256,
  );
  render(
    "docs/public/brand/og-image.svg",
    "docs/public/brand/og-image.png",
    1200,
  );
  render("docs/public/brand/og-image.svg", "docs/public/og-image.png", 1200);
} catch {
  console.warn("rsvg-convert not found: PNG assets were not re-rendered");
}
