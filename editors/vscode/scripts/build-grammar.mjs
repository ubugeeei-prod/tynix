#!/usr/bin/env node
// Generates the TextMate grammars shipped with the tynix VS Code extension.
//
// The grammar is authored here as plain JavaScript so the shared regex
// fragments (identifiers, attribute paths, keyword boundaries, ...) are written
// once and composed, instead of being copy-pasted across a large JSON file.
//
//   node scripts/build-grammar.mjs          # write syntaxes/*.json
//   node scripts/build-grammar.mjs --check  # fail if the JSON is out of date
//
// Scope names follow the TextMate conventions VS Code themes already style
// (TypeScript / Haskell flavoured), so stock themes colour tynix sensibly.

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");

// ---------------------------------------------------------------------------
// Shared regex fragments
// ---------------------------------------------------------------------------

/** Nix identifier: letters, digits, `_`, `'`, and `-` after the first char. */
const IDENT = String.raw`[A-Za-z_][A-Za-z0-9_'\-]*`;
/** Characters that may continue an identifier (used for word boundaries). */
const ID_CHAR = String.raw`[A-Za-z0-9_'\-]`;
/** Left boundary: not preceded by an identifier char or a selector dot. */
const L = String.raw`(?<![A-Za-z0-9_'\-.])`;
/** Right boundary: not followed by an identifier char. */
const R = String.raw`(?!${ID_CHAR})`;
/** A double-quoted string on a single line (used inside lookaheads). */
const QSTR = String.raw`"(?:[^"\\]|\\.)*"`;
/** A single-level `${ ... }` (used inside lookaheads). */
const INTERP = String.raw`\$\{(?:[^{}]|\{[^{}]*\})*\}`;
const ATTR = `(?:${IDENT}|${QSTR}|${INTERP})`;
const ATTRPATH = String.raw`${ATTR}(?:\s*\.\s*${ATTR})*`;
const PATH_CHAR = String.raw`[A-Za-z0-9._+\-]`;
const URI_CHAR = String.raw`[A-Za-z0-9%/?:@&=+$,\-_.!~*']`;
/** A `:` that ends a lambda head (and is not `::` or the start of a URI). */
const LAMBDA_COLON = String.raw`:(?![:A-Za-z0-9%/?@&=+$,_.!~*'\-])`;
/** Lookahead fragment recognising the start of a lambda on the same line. */
const LAMBDA_START = String.raw`(?:${IDENT}\s*(?:@\s*\{|${LAMBDA_COLON})|\{[^{}]*\}\s*(?:@\s*${IDENT}\s*)?${LAMBDA_COLON}|\(\s*${IDENT}\s*::)`;

const kw = (words) => `${L}(${words.join("|")})${R}`;

const BUILTIN_TYPES = [
  "String",
  "Int",
  "Float",
  "Number",
  "Nat",
  "Bool",
  "Path",
  "Null",
];
const BUILTIN_TYPE_CONSTRUCTORS = [
  "List",
  "Vec",
  "Matrix",
  "Tensor",
  "Range",
  "Unit",
  "Tuple",
];
const GLOBAL_BUILTINS = [
  "abort",
  "baseNameOf",
  "break",
  "derivation",
  "derivationStrict",
  "dirOf",
  "fetchGit",
  "fetchMercurial",
  "fetchTarball",
  "fetchTree",
  "fromTOML",
  "isNull",
  "map",
  "placeholder",
  "removeAttrs",
  "scopedImport",
  "throw",
  "toString",
];

// ---------------------------------------------------------------------------
// Grammar
// ---------------------------------------------------------------------------

const scope = (name) => `${name}.tynix`;

const grammar = {
  $schema:
    "https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json",
  name: "tynix",
  scopeName: "source.tynix",
  fileTypes: ["tynix", "d.tynix", "nix"],
  patterns: [{ include: "#declaration" }, { include: "#expression" }],
  repository: {
    // ----------------------------------------------------------- comments
    comments: {
      patterns: [
        { include: "#directive" },
        {
          name: scope("comment.block.documentation"),
          begin: String.raw`/\*\*(?!/)`,
          end: String.raw`\*/`,
          captures: { 0: { name: scope("punctuation.definition.comment") } },
          patterns: [{ include: "#comment-annotations" }],
        },
        {
          name: scope("comment.block"),
          begin: String.raw`/\*`,
          end: String.raw`\*/`,
          captures: { 0: { name: scope("punctuation.definition.comment") } },
          patterns: [{ include: "#comment-annotations" }],
        },
        {
          name: scope("comment.line.number-sign"),
          begin: "#",
          beginCaptures: {
            0: { name: scope("punctuation.definition.comment") },
          },
          end: "$",
          patterns: [{ include: "#comment-annotations" }],
        },
      ],
    },
    directive: {
      comment:
        "`# @tynix-ignore` / `# @tynix-expected` suppress the next checker failure.",
      match: String.raw`^\s*(#)\s*(@tynix-(?:ignore|expected))${R}(.*)$`,
      name: scope("comment.line.number-sign.directive"),
      captures: {
        1: { name: scope("punctuation.definition.comment") },
        2: { name: scope("keyword.control.directive") },
      },
    },
    "comment-annotations": {
      patterns: [
        {
          match: String.raw`\b(TODO|FIXME|XXX|HACK|NOTE|BUG)\b`,
          name: scope("keyword.other.todo"),
        },
      ],
    },

    // ------------------------------------------------------ declarations
    declaration: {
      patterns: [
        { include: "#comments" },
        { include: "#type-alias" },
        { include: "#declare" },
        { include: "#macro" },
      ],
    },
    "type-alias": {
      comment:
        "type Name params = Type;  opaque type Id t = String;  type F (f :: Type -> Type) = ...;",
      name: scope("meta.type-alias"),
      begin: String.raw`${L}(?:(opaque)\s+)?(type)\s+(${IDENT})((?:\s+(?:${IDENT}|\([^)]*\)))*)\s*(=)(?![=>])`,
      beginCaptures: {
        1: { name: scope("storage.modifier.opaque") },
        2: { name: scope("storage.type.type") },
        3: { name: scope("entity.name.type.alias") },
        4: {
          patterns: [
            {
              match: String.raw`::`,
              name: scope("keyword.operator.type.annotation"),
            },
            {
              match: String.raw`->`,
              name: scope("storage.type.function.arrow"),
            },
            { match: kw(["Type"]), name: scope("support.type.kind") },
            { match: IDENT, name: scope("entity.name.type.parameter") },
          ],
        },
        5: { name: scope("keyword.operator.assignment") },
      },
      end: ";",
      endCaptures: { 0: { name: scope("punctuation.terminator.statement") } },
      patterns: [{ include: "#type" }],
    },
    declare: {
      comment: 'declare "./file.nix" { name :: Type; };',
      name: scope("meta.declare"),
      begin: String.raw`${L}(declare)${R}(?=\s*(?:"|~?\.{0,2}/|<|$))`,
      beginCaptures: { 1: { name: scope("storage.modifier.declare") } },
      end: ";",
      endCaptures: { 0: { name: scope("punctuation.terminator.statement") } },
      patterns: [
        { include: "#comments" },
        { include: "#string-double" },
        { include: "#path" },
        {
          name: scope("meta.declare.body"),
          begin: String.raw`\{`,
          beginCaptures: {
            0: { name: scope("punctuation.section.braces.begin") },
          },
          end: String.raw`\}`,
          endCaptures: {
            0: { name: scope("punctuation.section.braces.end") },
          },
          patterns: [{ include: "#comments" }, { include: "#signature" }],
        },
      ],
    },
    macro: {
      comment: "macro name { ($x:expr) => (template); };",
      name: scope("meta.macro"),
      begin: String.raw`${L}(macro)\s+(${IDENT})\s*(\{)`,
      beginCaptures: {
        1: { name: scope("storage.type.macro") },
        2: { name: scope("entity.name.function.macro") },
        3: { name: scope("punctuation.section.braces.begin") },
      },
      end: String.raw`\}`,
      endCaptures: { 0: { name: scope("punctuation.section.braces.end") } },
      patterns: [
        { include: "#comments" },
        { match: "=>", name: scope("keyword.operator.macro.rule") },
        { include: "#metavariable" },
        { include: "#expression" },
      ],
    },
    "macro-call": {
      comment: "name!( ... )",
      match: String.raw`${L}(${IDENT})(!)(?=\()`,
      captures: {
        1: { name: scope("entity.name.function.macro") },
        2: { name: scope("punctuation.definition.macro") },
      },
    },
    metavariable: {
      patterns: [
        {
          comment: "$( ... ),*",
          begin: String.raw`\$\(`,
          beginCaptures: {
            0: { name: scope("keyword.operator.macro.repetition") },
          },
          end: String.raw`\)[,;]?[*+?]`,
          endCaptures: {
            0: { name: scope("keyword.operator.macro.repetition") },
          },
          patterns: [{ include: "#metavariable" }, { include: "#expression" }],
        },
        {
          match: String.raw`(\$${IDENT})(?:(:)(expr|ident|type|string)${R})?`,
          captures: {
            1: { name: scope("variable.other.metavariable") },
            2: { name: scope("punctuation.separator.fragment") },
            3: { name: scope("support.type.fragment") },
          },
        },
      ],
    },
    signature: {
      comment: "name :: Type;  (let items, attribute sets, declare bodies)",
      name: scope("meta.signature"),
      begin: String.raw`${L}(?:(${IDENT})|(${QSTR}))\s*(::)(?!:)`,
      beginCaptures: {
        1: { name: scope("entity.name.function") },
        2: { name: scope("string.quoted.double") },
        3: { name: scope("keyword.operator.type.annotation") },
      },
      // An unmatched `)` ends it too: `(expr :: Type)` is an ascription.
      end: String.raw`;|(?=\))`,
      endCaptures: { 0: { name: scope("punctuation.terminator.signature") } },
      patterns: [{ include: "#type" }],
    },

    // -------------------------------------------------------- expressions
    expression: {
      patterns: [
        { include: "#comments" },
        { include: "#signature" },
        { include: "#binding-function" },
        { include: "#binding" },
        { include: "#string-indented" },
        { include: "#string-double" },
        { include: "#declaration" },
        { include: "#typed-binder" },
        { include: "#inherit" },
        { include: "#with-assert" },
        { include: "#cast" },
        { include: "#type-annotation" },
        { include: "#keywords" },
        { include: "#uri" },
        { include: "#path" },
        { include: "#number" },
        { include: "#constants" },
        { include: "#builtins" },
        { include: "#macro-call" },
        { include: "#metavariable" },
        { include: "#lambda-head" },
        { include: "#braces" },
        { include: "#brackets" },
        { include: "#parens" },
        { include: "#select" },
        { include: "#operators" },
        { include: "#identifier" },
      ],
    },
    keywords: {
      patterns: [
        {
          match: kw(["if", "then", "else"]),
          name: scope("keyword.control.conditional"),
        },
        { match: kw(["assert"]), name: scope("keyword.control.assert") },
        { match: kw(["import"]), name: scope("keyword.control.import") },
        { match: kw(["let"]), name: scope("keyword.other.let") },
        { match: kw(["in"]), name: scope("keyword.other.in") },
        { match: kw(["with"]), name: scope("keyword.other.with") },
        { match: kw(["rec"]), name: scope("storage.modifier.rec") },
        { match: kw(["or"]), name: scope("keyword.operator.or") },
      ],
    },
    constants: {
      patterns: [
        { match: kw(["true"]), name: scope("constant.language.boolean.true") },
        {
          match: kw(["false"]),
          name: scope("constant.language.boolean.false"),
        },
        { match: kw(["null"]), name: scope("constant.language.null") },
      ],
    },
    number: {
      patterns: [
        {
          match: String.raw`${L}(?:\d+\.\d*|\.\d+)(?:[eE][+-]?\d+)?(?![A-Za-z0-9_'.])`,
          name: scope("constant.numeric.float"),
        },
        {
          match: String.raw`${L}\d+(?![A-Za-z0-9_'.])`,
          name: scope("constant.numeric.integer"),
        },
      ],
    },
    builtins: {
      patterns: [
        {
          match: String.raw`${L}(builtins)(?:\s*(\.)\s*(${IDENT}))?${R}`,
          captures: {
            1: { name: scope("variable.language.builtins") },
            2: { name: scope("punctuation.accessor") },
            3: { name: scope("support.function.builtin") },
          },
        },
        {
          match: kw(GLOBAL_BUILTINS),
          name: scope("support.function.builtin"),
        },
      ],
    },
    identifier: {
      match: `${L}${IDENT}`,
      name: scope("variable.other.readwrite"),
    },
    select: {
      patterns: [
        {
          match: String.raw`(\.)\s*(${IDENT})`,
          captures: {
            1: { name: scope("punctuation.accessor") },
            2: { name: scope("variable.other.property") },
          },
        },
        {
          match: String.raw`\.(?=\s*(?:"|\$\{))`,
          name: scope("punctuation.accessor"),
        },
      ],
    },
    operators: {
      patterns: [
        { match: "->", name: scope("keyword.operator.logical.implication") },
        { match: String.raw`\|>|<\|`, name: scope("keyword.operator.pipe") },
        { match: "//", name: scope("keyword.operator.update") },
        { match: String.raw`\+\+`, name: scope("keyword.operator.concat") },
        { match: "==|!=", name: scope("keyword.operator.comparison") },
        { match: "<=|>=|<|>", name: scope("keyword.operator.relational") },
        { match: String.raw`&&|\|\|`, name: scope("keyword.operator.logical") },
        { match: "!", name: scope("keyword.operator.logical.not") },
        {
          match: String.raw`\+|-|\*|/`,
          name: scope("keyword.operator.arithmetic"),
        },
        { match: String.raw`\?`, name: scope("keyword.operator.has-attr") },
        { match: "=", name: scope("keyword.operator.assignment") },
        { match: "@", name: scope("keyword.operator.at") },
        { match: ":", name: scope("storage.type.function.arrow") },
        { match: ";", name: scope("punctuation.terminator") },
        { match: ",", name: scope("punctuation.separator.comma") },
        { match: String.raw`\.`, name: scope("punctuation.accessor") },
      ],
    },

    // ------------------------------------------------------------ strings
    "string-double": {
      name: scope("string.quoted.double"),
      begin: '"',
      beginCaptures: {
        0: { name: scope("punctuation.definition.string.begin") },
      },
      end: '"',
      endCaptures: { 0: { name: scope("punctuation.definition.string.end") } },
      patterns: [
        { match: String.raw`\\.`, name: scope("constant.character.escape") },
        { include: "#interpolation" },
      ],
    },
    "string-indented": {
      name: scope("string.quoted.other.indented"),
      begin: "''",
      beginCaptures: {
        0: { name: scope("punctuation.definition.string.begin") },
      },
      end: String.raw`''(?!['$\\])`,
      endCaptures: { 0: { name: scope("punctuation.definition.string.end") } },
      patterns: [
        {
          match: String.raw`''(?:\$|'|\\.)`,
          name: scope("constant.character.escape"),
        },
        { include: "#interpolation" },
      ],
    },
    interpolation: {
      name: scope("meta.embedded.interpolation"),
      begin: String.raw`\$\{`,
      beginCaptures: {
        0: { name: scope("punctuation.section.embedded.begin") },
      },
      end: String.raw`\}`,
      endCaptures: {
        0: { name: scope("punctuation.section.embedded.end") },
      },
      contentName: "source.tynix",
      patterns: [{ include: "#expression" }],
    },

    // ---------------------------------------------------- paths and URIs
    uri: {
      match: String.raw`${L}[A-Za-z][A-Za-z0-9+\-.]*:(?!:)${URI_CHAR}+`,
      name: scope("string.unquoted.uri"),
    },
    path: {
      patterns: [
        {
          comment: "<nixpkgs> search path",
          match: String.raw`<${PATH_CHAR}+(?:/${PATH_CHAR}+)*>`,
          name: scope("string.unquoted.spath"),
        },
        {
          comment: "./foo/${bar}/baz — a path with interpolation",
          name: scope("string.unquoted.path"),
          begin: String.raw`(?<![A-Za-z0-9._+\-/'])(?:~|${PATH_CHAR}*)(?:/${PATH_CHAR}+)*/${PATH_CHAR}*(?=\$\{)`,
          end: String.raw`(?!${PATH_CHAR}|/|\$\{)`,
          patterns: [
            { include: "#interpolation" },
            { match: String.raw`(?:${PATH_CHAR}|/)+` },
          ],
        },
        {
          match: String.raw`(?<![A-Za-z0-9._+\-/'])(?:~|${PATH_CHAR}*)(?:/${PATH_CHAR}+)+/?`,
          name: scope("string.unquoted.path"),
        },
      ],
    },

    // ------------------------------------------------ bindings and sets
    "binding-function": {
      comment: "name = <lambda>; the key is highlighted as a function name.",
      name: scope("meta.binding"),
      begin: String.raw`(?=${L}${ATTRPATH}\s*=(?![=>])\s*${LAMBDA_START})`,
      end: ";",
      endCaptures: { 0: { name: scope("punctuation.terminator.binding") } },
      patterns: [
        {
          begin: String.raw`\G`,
          end: "=",
          endCaptures: { 0: { name: scope("keyword.operator.assignment") } },
          patterns: [
            { include: "#comments" },
            { include: "#string-double" },
            { include: "#interpolation" },
            { match: IDENT, name: scope("entity.name.function") },
            { match: String.raw`\.`, name: scope("punctuation.accessor") },
          ],
        },
        { include: "#expression" },
      ],
    },
    binding: {
      comment: 'a.b."c".${d} = value;',
      name: scope("meta.binding"),
      begin: String.raw`(?=${L}${ATTRPATH}\s*=(?![=>]))`,
      end: ";",
      endCaptures: { 0: { name: scope("punctuation.terminator.binding") } },
      patterns: [
        {
          begin: String.raw`\G`,
          end: "=",
          endCaptures: { 0: { name: scope("keyword.operator.assignment") } },
          patterns: [
            { include: "#comments" },
            { include: "#string-double" },
            { include: "#interpolation" },
            { match: IDENT, name: scope("entity.other.attribute-name") },
            { match: String.raw`\.`, name: scope("punctuation.accessor") },
          ],
        },
        { include: "#expression" },
      ],
    },
    "with-assert": {
      comment:
        "`with e;` / `assert e;` own their `;` so it does not end an enclosing binding.",
      patterns: [
        {
          name: scope("meta.with"),
          begin: kw(["with"]),
          beginCaptures: { 1: { name: scope("keyword.other.with") } },
          end: ";",
          endCaptures: { 0: { name: scope("punctuation.terminator.with") } },
          patterns: [{ include: "#expression" }],
        },
        {
          name: scope("meta.assert"),
          begin: kw(["assert"]),
          beginCaptures: { 1: { name: scope("keyword.control.assert") } },
          end: ";",
          endCaptures: { 0: { name: scope("punctuation.terminator.assert") } },
          patterns: [{ include: "#expression" }],
        },
      ],
    },
    inherit: {
      name: scope("meta.inherit"),
      begin: kw(["inherit"]),
      beginCaptures: { 1: { name: scope("keyword.other.inherit") } },
      end: ";",
      endCaptures: { 0: { name: scope("punctuation.terminator.inherit") } },
      patterns: [
        { include: "#comments" },
        { include: "#parens" },
        { include: "#string-double" },
        { match: IDENT, name: scope("entity.other.attribute-name") },
      ],
    },
    braces: {
      comment:
        "Attribute sets and lambda formals `{ a, b ? 1, ... }@args` share one rule.",
      name: scope("meta.braces"),
      begin: String.raw`\{`,
      beginCaptures: { 0: { name: scope("punctuation.section.braces.begin") } },
      end: String.raw`(\})(?:\s*(@)\s*(${IDENT}))?`,
      endCaptures: {
        1: { name: scope("punctuation.section.braces.end") },
        2: { name: scope("keyword.operator.at") },
        3: { name: scope("variable.parameter") },
      },
      patterns: [
        { include: "#comments" },
        { include: "#formal" },
        { include: "#expression" },
      ],
    },
    formal: {
      patterns: [
        {
          comment: "an item of a lambda formals list",
          match: String.raw`(?:(?<=[{,])|^)\s*(?!(?:inherit|let|in|if|then|else|with|assert|rec|or|import|true|false|null)${R})(${IDENT})(?:\s*(\?)|(?=\s*(?:,|\}|$)))`,
          captures: {
            1: { name: scope("variable.parameter") },
            2: { name: scope("keyword.operator.default") },
          },
        },
        { match: String.raw`\.\.\.`, name: scope("keyword.operator.ellipsis") },
      ],
    },
    "lambda-head": {
      patterns: [
        {
          comment: "args@{ ... }",
          match: String.raw`${L}(${IDENT})\s*(@)(?=\s*\{)`,
          captures: {
            1: { name: scope("variable.parameter") },
            2: { name: scope("keyword.operator.at") },
          },
        },
        {
          comment: "x: body",
          match: String.raw`${L}(${IDENT})\s*(${LAMBDA_COLON})`,
          captures: {
            1: { name: scope("variable.parameter") },
            2: { name: scope("storage.type.function.arrow") },
          },
        },
      ],
    },
    "typed-binder": {
      comment: "(x :: Type): body",
      name: scope("meta.parameter.typed"),
      begin: String.raw`(\()\s*(${IDENT})\s*(::)(?!:)`,
      beginCaptures: {
        1: { name: scope("punctuation.section.parens.begin") },
        2: { name: scope("variable.parameter") },
        3: { name: scope("keyword.operator.type.annotation") },
      },
      end: String.raw`\)`,
      endCaptures: { 0: { name: scope("punctuation.section.parens.end") } },
      patterns: [{ include: "#type" }],
    },
    brackets: {
      name: scope("meta.list"),
      begin: String.raw`\[`,
      beginCaptures: {
        0: { name: scope("punctuation.section.brackets.begin") },
      },
      end: String.raw`\]`,
      endCaptures: { 0: { name: scope("punctuation.section.brackets.end") } },
      patterns: [{ include: "#expression" }],
    },
    parens: {
      begin: String.raw`\(`,
      beginCaptures: { 0: { name: scope("punctuation.section.parens.begin") } },
      end: String.raw`\)`,
      endCaptures: { 0: { name: scope("punctuation.section.parens.end") } },
      patterns: [{ include: "#expression" }],
    },
    cast: {
      comment: "expr as Type",
      name: scope("meta.cast"),
      begin: kw(["as"]),
      beginCaptures: { 1: { name: scope("keyword.control.as") } },
      end: String.raw`(?=[;,)\]}<>+*]|-(?!>)|${L}(?:then|else|in|as|or)${R}|==|!=|&&|\|\||//|\|>|$)`,
      patterns: [{ include: "#type" }],
    },
    "type-annotation": {
      comment: "expr :: Type (documentation style annotation)",
      name: scope("meta.type.annotation"),
      begin: String.raw`::(?!:)`,
      beginCaptures: {
        0: { name: scope("keyword.operator.type.annotation") },
      },
      end: String.raw`(?=[;,)\]}])|$`,
      patterns: [{ include: "#type" }],
    },

    // -------------------------------------------------------------- types
    type: {
      patterns: [
        { include: "#comments" },
        {
          comment: "forall a b. T",
          match: String.raw`${L}(forall)${R}((?:\s*${IDENT})*)\s*(\.)`,
          captures: {
            1: { name: scope("keyword.other.forall") },
            2: {
              patterns: [
                { match: IDENT, name: scope("entity.name.type.parameter") },
              ],
            },
            3: { name: scope("punctuation.separator.forall") },
          },
        },
        {
          match: String.raw`${L}(infer)${R}\s*(${IDENT})?`,
          captures: {
            1: { name: scope("keyword.operator.expression.infer") },
            2: { name: scope("entity.name.type.parameter") },
          },
        },
        { match: kw(["extends"]), name: scope("storage.modifier.extends") },
        {
          match: kw(["any", "dynamic", "unknown"]),
          name: scope("support.type.primitive.gradual"),
        },
        { match: kw(BUILTIN_TYPES), name: scope("support.type.primitive") },
        {
          match: kw(BUILTIN_TYPE_CONSTRUCTORS),
          name: scope("support.type.builtin"),
        },
        { include: "#constants" },
        { include: "#number" },
        { include: "#string-double" },
        { match: "%1", name: scope("storage.modifier.linear") },
        {
          comment: "capture set glued to an arrow: `->{fetch, log}`",
          match: String.raw`(->)(\{)([^}]*)(\})`,
          captures: {
            1: { name: scope("storage.type.function.arrow") },
            2: { name: scope("punctuation.section.braces.begin") },
            3: {
              patterns: [
                { match: IDENT, name: scope("variable.other.capture") },
                { match: ",", name: scope("punctuation.separator.comma") },
              ],
            },
            4: { name: scope("punctuation.section.braces.end") },
          },
        },
        { match: "->", name: scope("storage.type.function.arrow") },
        {
          comment: "effect row: `! { Trace, Throw | e }` or `! e`",
          match: String.raw`(!)\s*(?:(\{)([^}]*)(\})|(${IDENT}))`,
          captures: {
            1: { name: scope("keyword.operator.effect") },
            2: { name: scope("punctuation.section.braces.begin") },
            3: {
              patterns: [
                {
                  match: String.raw`[A-Z][A-Za-z0-9_'\-]*`,
                  name: scope("support.type.effect"),
                },
                { match: IDENT, name: scope("entity.name.type.parameter") },
                {
                  match: String.raw`[,|]`,
                  name: scope("punctuation.separator.effect"),
                },
              ],
            },
            4: { name: scope("punctuation.section.braces.end") },
            5: { name: scope("entity.name.type.parameter") },
          },
        },
        { match: "::", name: scope("keyword.operator.type.annotation") },
        { match: "=>", name: scope("keyword.operator.type.constraint") },
        { match: String.raw`\|`, name: scope("keyword.operator.type.union") },
        {
          match: String.raw`\?|:(?!:)`,
          name: scope("keyword.operator.ternary"),
        },
        {
          name: scope("meta.type.record"),
          begin: String.raw`\{`,
          beginCaptures: {
            0: { name: scope("punctuation.section.braces.begin") },
          },
          end: String.raw`\}`,
          endCaptures: {
            0: { name: scope("punctuation.section.braces.end") },
          },
          patterns: [
            { include: "#comments" },
            { include: "#type-field" },
            {
              comment: "open row: `...` or `...r`",
              match: String.raw`(\.\.\.)(${IDENT})?`,
              captures: {
                1: { name: scope("punctuation.separator.rest") },
                2: { name: scope("entity.name.type.parameter") },
              },
            },
          ],
        },
        {
          name: scope("meta.type.list"),
          begin: String.raw`\[`,
          beginCaptures: {
            0: { name: scope("punctuation.section.brackets.begin") },
          },
          end: String.raw`\]`,
          endCaptures: {
            0: { name: scope("punctuation.section.brackets.end") },
          },
          patterns: [{ include: "#type" }],
        },
        {
          name: scope("meta.type.paren"),
          begin: String.raw`\(`,
          beginCaptures: {
            0: { name: scope("punctuation.section.parens.begin") },
          },
          end: String.raw`\)`,
          endCaptures: {
            0: { name: scope("punctuation.section.parens.end") },
          },
          patterns: [{ include: "#type" }],
        },
        {
          match: String.raw`${L}[A-Z][A-Za-z0-9_'\-]*`,
          name: scope("entity.name.type"),
        },
        { match: `${L}${IDENT}`, name: scope("entity.name.type.parameter") },
        { match: String.raw`\.`, name: scope("punctuation.accessor") },
      ],
    },
    "type-field": {
      name: scope("meta.type.field"),
      begin: String.raw`(?:(${IDENT})|(${QSTR}))\s*(\?)?\s*(::)(?!:)`,
      beginCaptures: {
        1: { name: scope("entity.other.attribute-name") },
        2: { name: scope("string.quoted.double") },
        3: { name: scope("keyword.operator.optional") },
        4: { name: scope("keyword.operator.type.annotation") },
      },
      end: ";",
      endCaptures: { 0: { name: scope("punctuation.terminator.field") } },
      patterns: [{ include: "#type" }],
    },
  },
};

const markdownInjection = {
  $schema:
    "https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json",
  scopeName: "markdown.tynix.codeblock",
  fileTypes: [],
  injectionSelector: "L:text.html.markdown",
  patterns: [{ include: "#tynix-code-block" }],
  repository: {
    "tynix-code-block": {
      name: "markup.fenced_code.block.markdown",
      begin: String.raw`(^|\G)(\s*)(\`{3,}|~{3,})\s*(?i:(tynix)((\s+|:|,|\{|\?)[^\`~]*)?$)`,
      beginCaptures: {
        3: { name: "punctuation.definition.markdown" },
        4: { name: "fenced_code.block.language.markdown" },
        5: { name: "fenced_code.block.language.attributes.markdown" },
      },
      end: String.raw`(^|\G)(\2|\s{0,3})(\3)\s*$`,
      endCaptures: { 3: { name: "punctuation.definition.markdown" } },
      patterns: [
        {
          begin: String.raw`(^|\G)(\s*)(.*)`,
          while: String.raw`(^|\G)(?!\s*([\`~]{3,})\s*$)`,
          contentName: "meta.embedded.block.tynix",
          patterns: [{ include: "source.tynix" }],
        },
      ],
    },
  },
};

const outputs = [
  ["syntaxes/tynix.tmLanguage.json", grammar],
  ["syntaxes/tynix-markdown-injection.tmLanguage.json", markdownInjection],
];

const check = process.argv.includes("--check");
let stale = false;
for (const [relative, value] of outputs) {
  const file = join(root, relative);
  const rendered = `${JSON.stringify(value, null, 2)}\n`;
  if (check) {
    let current = "";
    try {
      current = readFileSync(file, "utf8");
    } catch {
      // treated as stale below
    }
    if (current !== rendered) {
      stale = true;
      console.error(
        `${relative} is out of date; run: node scripts/build-grammar.mjs`,
      );
    }
  } else {
    writeFileSync(file, rendered);
    console.log(`wrote ${relative}`);
  }
}
if (stale) process.exit(1);
