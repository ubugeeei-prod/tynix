// TextMate grammar for `.tynix` / `.d.tynix` code fences on the docs site.
//
// Editors get precise highlighting from the language server's semantic
// tokens; this grammar only needs to be good enough for static docs. It knows
// the Nix expression surface plus the type-only layer tynix adds (`::`
// annotations, `type`, `opaque type`, `declare`, `forall`, `extends`/`infer`,
// effect rows, capture sets, `as`, macros, and the `# @tynix-*` directives) so
// that the brand code theme can paint every type-only construct in the
// "annotation" colour. Those are exactly the tokens that disappear when tynix
// compiles to `.nix`.

const typeExpression = {
  patterns: [
    { include: "#comment" },
    { include: "#metavariable" },
    { include: "#typeRecord" },
    { begin: "\\(", end: "\\)", patterns: [{ include: "#typeExpr" }] },
    { include: "#string" },
    { match: "\\b(forall|extends|infer)\\b", name: "keyword.other.type.tynix" },
    { match: "\\b(Type)\\b(?=\\s*(?:->|\\)|$))", name: "keyword.other.type.tynix" },
    { match: "::", name: "keyword.operator.annotation.tynix" },
    { match: "!(?!=)", name: "keyword.operator.type.tynix" },
    { match: "\\b(dynamic|unknown|any)\\b", name: "support.type.gradual.tynix" },
    { match: "\\b(true|false)\\b", name: "constant.language.boolean.tynix" },
    { match: "-?\\b\\d+(\\.\\d+)?\\b", name: "constant.numeric.tynix" },
    { match: "\\b[A-Z][A-Za-z0-9_'-]*", name: "entity.name.type.tynix" },
    { match: "%1\\s*->|->|\\||\\?|:(?!:)", name: "keyword.operator.type.tynix" },
    { match: "\\b[a-z_][A-Za-z0-9_'-]*", name: "variable.parameter.type.tynix" },
  ],
};

export const tynixGrammar = {
  name: "tynix",
  scopeName: "source.tynix",
  aliases: ["d.tynix"],
  patterns: [
    { include: "#directive" },
    { include: "#comment" },
    { include: "#typeAlias" },
    { include: "#declare" },
    { include: "#macro" },
    { include: "#macroCall" },
    { include: "#metavariable" },
    { include: "#signature" },
    { include: "#cast" },
    { include: "#typedBinder" },
    { include: "#string" },
    { include: "#indentedString" },
    { include: "#keyword" },
    { include: "#constant" },
    { include: "#path" },
    { include: "#attribute" },
    { include: "#operator" },
    { include: "#identifier" },
  ],
  repository: {
    typeExpr: typeExpression,
    directive: {
      match: "^\\s*(#)\\s*(@tynix-(?:ignore|expected))\\b.*$",
      captures: {
        1: { name: "comment.line.number-sign.tynix" },
        2: { name: "keyword.control.directive.tynix" },
      },
    },
    comment: {
      patterns: [
        { match: "#.*$", name: "comment.line.number-sign.tynix" },
        { begin: "/\\*", end: "\\*/", name: "comment.block.tynix" },
      ],
    },
    typeAlias: {
      begin: "\\b(?:(opaque)\\s+)?(type)\\s+([A-Za-z_][A-Za-z0-9_'-]*)((?:\\s+(?:[a-z_][A-Za-z0-9_'-]*|\\([^)]*\\)))*)\\s*(=)",
      beginCaptures: {
        1: { name: "keyword.other.type.tynix" },
        2: { name: "keyword.other.type.tynix" },
        3: { name: "entity.name.type.alias.tynix" },
        4: { name: "variable.parameter.type.tynix" },
        5: { name: "keyword.operator.type.tynix" },
      },
      end: ";",
      endCaptures: { 0: { name: "punctuation.terminator.type.tynix" } },
      patterns: [{ include: "#typeExpr" }],
    },
    declare: {
      match: "\\b(declare)\\b",
      name: "keyword.other.type.tynix",
    },
    // `macro name { ($x:expr) => (template); };` is compile-time only, so it is
    // painted like the rest of the type layer.
    macro: {
      match: "\\b(macro)\\s+([A-Za-z_][A-Za-z0-9_'-]*)",
      captures: {
        1: { name: "keyword.other.type.tynix" },
        2: { name: "entity.name.type.alias.tynix" },
      },
    },
    macroCall: {
      match: "\\b([A-Za-z_][A-Za-z0-9_'-]*)(!)(?=\\()",
      captures: {
        1: { name: "entity.name.type.alias.tynix" },
        2: { name: "keyword.other.type.tynix" },
      },
    },
    metavariable: {
      patterns: [
        { match: "\\$\\(|\\)[,;]?[*+?](?=\\s)|=>", name: "keyword.other.type.tynix" },
        {
          match: "(\\$[A-Za-z_][A-Za-z0-9_'-]*)(?:(:)(expr|ident|type|string)\\b)?",
          captures: {
            1: { name: "variable.parameter.type.tynix" },
            2: { name: "keyword.operator.annotation.tynix" },
            3: { name: "keyword.other.type.tynix" },
          },
        },
      ],
    },
    signature: {
      begin: "(::)",
      beginCaptures: { 1: { name: "keyword.operator.annotation.tynix" } },
      end: "(?=;)|(?=\\))",
      patterns: [{ include: "#typeExpr" }],
    },
    typeRecord: {
      begin: "\\{",
      end: "\\}",
      beginCaptures: { 0: { name: "punctuation.definition.type.record.tynix" } },
      endCaptures: { 0: { name: "punctuation.definition.type.record.tynix" } },
      patterns: [
        { include: "#comment" },
        // Effect rows (`! { Trace | e }`) and capture sets (`->{fetch}`).
        { match: "\\b[A-Z][A-Za-z0-9_'-]*\\b(?=\\s*[,|}])", name: "entity.name.type.tynix" },
        { match: "\\b[a-z_][A-Za-z0-9_'-]*\\b(?=\\s*[,}])", name: "variable.parameter.type.tynix" },
        { match: "[,|]", name: "keyword.operator.type.tynix" },
        {
          begin: "(\"[^\"]*\"|[A-Za-z_][A-Za-z0-9_'-]*)\\s*(\\?)?\\s*(::)",
          beginCaptures: {
            1: { name: "variable.other.property.tynix" },
            2: { name: "keyword.operator.type.tynix" },
            3: { name: "keyword.operator.annotation.tynix" },
          },
          end: ";",
          endCaptures: { 0: { name: "punctuation.terminator.type.tynix" } },
          patterns: [{ include: "#typeExpr" }],
        },
      ],
    },
    cast: {
      begin: "\\b(as)\\b",
      beginCaptures: { 1: { name: "keyword.other.type.tynix" } },
      end: "(?=[;)\\]}]|\\bin\\b|\\bthen\\b|\\belse\\b|$)",
      patterns: [{ include: "#typeExpr" }],
    },
    typedBinder: {
      begin: "\\(([a-z_][A-Za-z0-9_'-]*)\\s*(::)",
      beginCaptures: {
        1: { name: "variable.parameter.tynix" },
        2: { name: "keyword.operator.annotation.tynix" },
      },
      end: "(?=\\))",
      patterns: [{ include: "#typeExpr" }],
    },
    string: {
      begin: "\"",
      end: "\"",
      name: "string.quoted.double.tynix",
      patterns: [
        { match: "\\\\.", name: "constant.character.escape.tynix" },
        { include: "#interpolation" },
      ],
    },
    indentedString: {
      begin: "''",
      end: "''(?!['$\\\\])",
      name: "string.quoted.other.indented.tynix",
      patterns: [
        { match: "''(\\$|'|\\\\.)", name: "constant.character.escape.tynix" },
        { include: "#interpolation" },
      ],
    },
    interpolation: {
      begin: "\\$\\{",
      end: "\\}",
      beginCaptures: { 0: { name: "punctuation.section.embedded.begin.tynix" } },
      endCaptures: { 0: { name: "punctuation.section.embedded.end.tynix" } },
      name: "meta.embedded.tynix",
      patterns: [{ include: "$self" }],
    },
    keyword: {
      patterns: [
        { match: "\\b(let|in|if|then|else|assert|with|rec|inherit|or)\\b", name: "keyword.control.tynix" },
        { match: "\\b(import)\\b", name: "support.function.import.tynix" },
        { match: "\\b(builtins)\\b", name: "support.variable.builtins.tynix" },
      ],
    },
    constant: {
      patterns: [
        { match: "\\b(true|false|null)\\b", name: "constant.language.tynix" },
        { match: "-?\\b\\d+(\\.\\d+)?([eE][+-]?\\d+)?\\b", name: "constant.numeric.tynix" },
      ],
    },
    path: {
      match: "(?:\\.{1,2}|~)?/[A-Za-z0-9._+-]+(?:/[A-Za-z0-9._+-]+)*|<[A-Za-z0-9._+/-]+>",
      name: "string.unquoted.path.tynix",
    },
    attribute: {
      match: "([A-Za-z_][A-Za-z0-9_'-]*|\"[^\"]*\")(?=\\s*=(?!=))",
      name: "variable.other.property.tynix",
    },
    operator: {
      match: "\\+\\+|//|==|!=|<=|>=|&&|\\|\\||->|\\|>|[-+*/<>!?@:=]",
      name: "keyword.operator.tynix",
    },
    identifier: {
      match: "\\b[A-Za-z_][A-Za-z0-9_'-]*",
      name: "variable.other.tynix",
    },
  },
};
