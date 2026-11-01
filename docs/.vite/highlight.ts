// Code block post-processing for the built docs site.
//
// Ox Content 3 highlights fenced code with its native tree-sitter engine. That
// engine covers nix, bash, json, ts, yaml and friends, but it has no tynix
// grammar and no way to register one, so ```tynix fences come out as plain
// `<pre><code>`. This pass runs over the generated HTML after the SSG step:
//
// 1. Highlights `tynix` / `d.tynix` blocks with the TextMate grammar in
//    `tynix-grammar.ts` (through Shiki's core + JavaScript regex engine) and
//    emits the same `ox-highlight css-variables` markup the native engine
//    does, so every block is painted by the same `--octc-syntax-*` tokens
//    (see `codeTokens` in brand.ts). Type-only syntax gets its own
//    `--octc-syntax-token-annotation` token: the amber that marks "erased".
// 2. Renders any other block the native engine declined (for example `ebnf`)
//    as plain foreground text so it still sits on the themed surface.
// 3. Wraps every block in a `.tx-code` frame with a filename/language bar and
//    a copy button (wired up by `themeJs` in brand.ts).
import { readdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { createHighlighterCore, type HighlighterCore } from "@shikijs/core";
import { createJavaScriptRegexEngine } from "@shikijs/engine-javascript";
import { processNavigation } from "./navigation.ts";
import { tynixGrammar } from "./tynix-grammar.ts";

// Shiki needs concrete colours, so the theme paints each token class with a
// sentinel colour that is mapped back to a CSS custom property below.
const tokenVars = [
  "foreground",
  "token-comment",
  "token-keyword",
  "token-string",
  "token-string-expression",
  "token-constant",
  "token-function",
  "token-parameter",
  "token-punctuation",
  "token-annotation",
] as const;
type TokenVar = (typeof tokenVars)[number];
const sentinel = (name: TokenVar) =>
  `#0000${tokenVars.indexOf(name).toString(16).padStart(2, "0")}`;
const varFor = new Map(tokenVars.map((name) => [sentinel(name), name]));

const annotationScopes = [
  "keyword.other.type.tynix",
  "keyword.operator.annotation.tynix",
  "keyword.operator.type.tynix",
  "entity.name.type",
  "entity.name.type.alias.tynix",
  "variable.parameter.type.tynix",
  "punctuation.definition.type.record.tynix",
  "punctuation.terminator.type.tynix",
];

const sentinelTheme = {
  name: "tynix-tokens",
  type: "dark" as const,
  colors: {
    "editor.background": "#000000",
    "editor.foreground": sentinel("foreground"),
  },
  settings: [
    { settings: { foreground: sentinel("foreground") } },
    {
      scope: ["comment", "punctuation.definition.comment"],
      settings: { foreground: sentinel("token-comment"), fontStyle: "italic" },
    },
    {
      scope: ["keyword.control.directive.tynix"],
      settings: { foreground: sentinel("token-function"), fontStyle: "bold" },
    },
    {
      scope: ["keyword", "support.function.import"],
      settings: { foreground: sentinel("token-keyword") },
    },
    {
      scope: ["string", "string.unquoted.path"],
      settings: { foreground: sentinel("token-string") },
    },
    {
      scope: ["constant.character.escape"],
      settings: { foreground: sentinel("token-string-expression") },
    },
    {
      scope: ["punctuation.section.embedded"],
      settings: { foreground: sentinel("token-punctuation") },
    },
    {
      scope: [
        "constant.numeric",
        "constant.language",
        "variable.other.property",
        "support.variable",
      ],
      settings: { foreground: sentinel("token-constant") },
    },
    {
      scope: ["variable.parameter"],
      settings: { foreground: sentinel("token-parameter") },
    },
    {
      scope: ["keyword.operator", "punctuation"],
      settings: { foreground: sentinel("token-punctuation") },
    },
    {
      scope: annotationScopes,
      settings: { foreground: sentinel("token-annotation") },
    },
    {
      scope: ["support.type.gradual.tynix"],
      settings: {
        foreground: sentinel("token-annotation"),
        fontStyle: "italic",
      },
    },
  ],
};

let highlighter: Promise<HighlighterCore> | undefined;
function getHighlighter(): Promise<HighlighterCore> {
  highlighter ??= createHighlighterCore({
    themes: [sentinelTheme],
    langs: [tynixGrammar as never],
    engine: createJavaScriptRegexEngine({ forgiving: true }),
  });
  return highlighter;
}

const escapeHtml = (text: string) =>
  text
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
const decodeHtml = (html: string) =>
  html
    .replace(/<[^>]+>/g, "")
    .replace(/&lt;/g, "<")
    .replace(/&gt;/g, ">")
    .replace(/&quot;/g, '"')
    .replace(/&#39;|&#x27;/g, "'")
    .replace(/&amp;/g, "&");

const preStyle =
  "background-color:var(--octc-syntax-background);color:var(--octc-syntax-foreground)";

function span(text: string, color: TokenVar, fontStyle = 0): string {
  const styles = [`color:var(--octc-syntax-${color})`];
  if (fontStyle & 1) styles.push("font-style:italic");
  if (fontStyle & 2) styles.push("font-weight:600");
  return `<span style="${styles.join(";")}">${escapeHtml(text)}</span>`;
}

// Line annotations (`{2,4-5}`, `// [!code ...]`) arrive as classes on
// per-line spans; keep them when a block is re-highlighted.
const lineClasses = (inner: string) =>
  inner
    .split("\n")
    .map(
      (line) => line.match(/^<span class="(line(?:\s[^"]*)?)"/)?.[1] ?? "line",
    );

async function highlightTynix(
  code: string,
  classesByLine: string[],
): Promise<string> {
  const hl = await getHighlighter();
  const { tokens } = hl.codeToTokens(code.replace(/\n$/, ""), {
    lang: "tynix",
    theme: "tynix-tokens",
  });
  return tokens
    .map((line, i) => {
      const body = line
        .map((token) =>
          span(
            token.content,
            varFor.get((token.color ?? "").toLowerCase()) ?? "foreground",
            token.fontStyle ?? 0,
          ),
        )
        .join("");
      return `<span class="${classesByLine[i] ?? "line"}">${body}</span>`;
    })
    .join("\n");
}

const plainLines = (code: string, classesByLine: string[]) =>
  code
    .replace(/\n$/, "")
    .split("\n")
    .map(
      (line, i) =>
        `<span class="${classesByLine[i] ?? "line"}">${line ? span(line, "foreground") : ""}</span>`,
    )
    .join("\n");

const attr = (attrs: string, name: string) =>
  attrs.match(new RegExp(`\\s${name}="([^"]*)"`))?.[1];
const classes = (attrs: string) =>
  (attr(attrs, "class") ?? "").split(/\s+/).filter(Boolean);
const langLabels: Record<string, string> = {
  "d.tynix": "tynix",
  text: "",
  plaintext: "",
  sh: "shell",
  bash: "shell",
};

function frame(pre: string, preAttrs: string, lang: string): string {
  const title = attr(preAttrs, "data-code-title");
  const label = langLabels[lang] ?? lang;
  const copy =
    '<button type="button" class="tx-copy" aria-label="Copy code"><span>Copy</span></button>';
  const bar = title
    ? `<div class="tx-code__bar"><span class="tx-code__title">${title}</span>${label ? `<span class="tx-code__lang">${label}</span>` : ""}${copy}</div>`
    : `<div class="tx-code__float">${label ? `<span class="tx-code__lang">${label}</span>` : ""}${copy}</div>`;
  return `<div class="tx-code${title ? " tx-code--titled" : ""}" data-lang="${lang}">${bar}${pre}</div>`;
}

const PRE = /<pre(\s[^>]*)?>\s*<code(\s[^>]*)?>([\s\S]*?)<\/code>\s*<\/pre>/g;

/** Rewrite every code block in one HTML page. */
export async function processCodeBlocks(html: string): Promise<string> {
  const jobs: Promise<string>[] = [];
  const matches = [...html.matchAll(PRE)];
  for (const match of matches) {
    jobs.push(
      (async () => {
        const [whole, preAttrs = "", codeAttrs = "", inner] = match;
        const codeClasses = classes(codeAttrs);
        const lang =
          attr(preAttrs, "data-language") ??
          codeClasses.find((c) => c.startsWith("language-"))?.slice(9) ??
          "text";
        // The frame renders the title, so drop Ox Content's own title strip.
        const preClasses = classes(preAttrs).filter(
          (c) => c !== "ox-code-block--with-title",
        );
        let body = inner;
        if (!preClasses.includes("ox-highlight")) {
          const code = decodeHtml(inner);
          const perLine = lineClasses(inner);
          body =
            lang === "tynix" || lang === "d.tynix"
              ? await highlightTynix(code, perLine)
              : plainLines(code, perLine);
          preClasses.unshift("ox-highlight", "css-variables");
        }
        // The native engine leaves an empty line for the fence's final newline.
        body = body.replace(/\n?<span class="line"><\/span>\s*$/, "");
        const keptAttrs = preAttrs
          .replace(
            /\s(class|style|tabindex|data-language|data-code-title)="[^"]*"/g,
            "",
          )
          .trim();
        const pre =
          `<pre class="${preClasses.join(" ")}" style="${preStyle}" tabindex="0" data-language="${lang}"${keptAttrs ? ` ${keptAttrs}` : ""}>` +
          `<code class="language-${lang}" data-language="${lang}">${body}</code></pre>`;
        return frame(pre, preAttrs, lang);
      })(),
    );
  }
  const replaced = await Promise.all(jobs);
  let index = 0;
  return html.replace(PRE, () => replaced[index++]);
}

async function* htmlFiles(dir: string): AsyncGenerator<string> {
  for (const entry of await readdir(dir, { withFileTypes: true })) {
    const path = join(dir, entry.name);
    if (entry.isDirectory()) yield* htmlFiles(path);
    else if (entry.name.endsWith(".html")) yield path;
  }
}

/** Post-process every generated page under `outDir`. */
export async function processSite(outDir: string): Promise<number> {
  let count = 0;
  for await (const file of htmlFiles(outDir)) {
    const html = await readFile(file, "utf8");
    const next = processNavigation(await processCodeBlocks(html));
    if (next !== html) {
      await writeFile(file, next);
      count++;
    }
  }
  return count;
}
