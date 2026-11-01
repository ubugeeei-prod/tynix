/**
 * @file tree-sitter grammar for tynix: Nix plus a gradual type layer.
 *
 * The expression half follows nix-community/tree-sitter-nix closely (same node
 * names wherever possible) so Nix-oriented queries keep working. The tynix half
 * adds top-level `type` / `opaque type` aliases (with optional kind
 * annotations), `declare` blocks, `macro` declarations and `name!( ... )`
 * invocations, `name :: Type;` signatures, typed lambda binders
 * `(x :: T): body`, `expr as Type` casts, `(expr :: Type)` ascriptions, and a
 * full type sub-language (forall, constraints, arrows with linearity, capture
 * sets, dependent binders and effect rows, unions, records, conditional types
 * with `infer`, literal types, type lists).
 *
 * Macro patterns and templates are parsed as token trees: they are only
 * meaningful after expansion, so the grammar just delimits and highlights
 * them.
 */

/* eslint-disable no-undef */

const PREC = {
  pipe_left: 1,
  pipe_right: 1,
  impl: 2,
  or: 3,
  and: 4,
  eq: 5,
  neq: 5,
  lt: 6,
  gt: 6,
  leq: 6,
  geq: 6,
  update: 7,
  not: 8,
  plus: 9,
  minus: 9,
  mul: 10,
  div: 10,
  concat: 11,
  has_attr: 12,
  cast: 13,
  negate: 14,
  // Type-level productions always extend greedily (`x as A -> B` casts to a
  // function type rather than forming a Nix implication).
  type: 20,
};

const commaSep1 = (rule) => seq(rule, repeat(seq(",", rule)));

module.exports = grammar({
  name: "tynix",

  extras: ($) => [/\s/, $.directive, $.comment],

  externals: ($) => [
    $.string_fragment,
    $._indented_string_fragment,
    $._path_start,
    $.path_fragment,
  ],

  word: ($) => $.identifier,

  supertypes: ($) => [$._expression, $._type],

  inline: ($) => [$._type_union, $._type_app],

  conflicts: ($) => [[$.typed_parameter, $.variable_expression]],

  rules: {
    source_code: ($) =>
      seq(
        repeat(field("declaration", choice($.type_alias, $.ambient_declaration, $.macro_declaration))),
        optional(field("expression", $._expression)),
      ),

    // ---------------------------------------------------------------------
    // tynix declarations

    type_alias: ($) =>
      seq(
        optional(field("modifier", alias("opaque", $.opaque))),
        "type",
        field("name", alias($.identifier, $.type_identifier)),
        repeat(field("parameter", choice(alias($.identifier, $.type_variable), $.kinded_parameter))),
        "=",
        field("type", $._type),
        ";",
      ),

    // `(f :: Type -> Type)`
    kinded_parameter: ($) =>
      seq("(", field("name", alias($.identifier, $.type_variable)), "::", field("kind", $._kind), ")"),

    _kind: ($) => choice($.kind_arrow, $._kind_atom),

    kind_arrow: ($) => prec.right(seq(field("parameter", $._kind_atom), "->", field("result", $._kind))),

    _kind_atom: ($) => choice(alias($.identifier, $.kind), alias("*", $.kind), seq("(", $._kind, ")")),

    // ---------------------------------------------------------------------
    // Macros

    macro_declaration: ($) =>
      seq("macro", field("name", $.identifier), "{", repeat(field("rule", $.macro_rule)), "}", ";"),

    macro_rule: ($) =>
      seq(
        field("pattern", $.macro_token_tree),
        "=>",
        field("template", $.macro_token_tree),
        ";",
      ),

    macro_token_tree: ($) => seq("(", repeat($._macro_token), ")"),

    _macro_token: ($) =>
      choice(
        $.macro_token_tree,
        $.macro_repetition,
        $.metavariable,
        $.macro_invocation,
        seq("[", repeat($._macro_token), "]"),
        seq("{", repeat($._macro_token), "}"),
        $.string_expression,
        $.indented_string_expression,
        $.integer_expression,
        $.float_expression,
        $.path_expression,
        $.identifier,
        $.macro_punctuation,
      ),

    // `$( ... ),*`
    macro_repetition: ($) =>
      seq("$(", repeat($._macro_token), ")", optional(choice(",", ";")), choice("*", "+", "?")),

    // `$name`, `$name:fragment`
    metavariable: ($) =>
      seq(
        field("name", alias(token(seq("$", /[a-zA-Z_][a-zA-Z0-9_'\-]*/)), $.metavariable_name)),
        optional(seq(token.immediate(":"), field("fragment", alias(token.immediate(/expr|ident|type|string/), $.fragment)))),
      ),

    macro_punctuation: (_) => token(prec(-1, /[=<>\-+*\/!&|:.?@%^~,;]+/)),

    // `name!( ... )`
    macro_invocation: ($) =>
      prec(
        1,
        seq(
          field("name", $.identifier),
          token.immediate("!"),
          token.immediate("("),
          repeat(field("argument", $._macro_token)),
          ")",
        ),
      ),

    ambient_declaration: ($) =>
      seq(
        "declare",
        field(
          "path",
          choice($.string_expression, $.path_expression, $.hpath_expression, $.spath_expression),
        ),
        field("body", $.declaration_block),
        ";",
      ),

    declaration_block: ($) => seq("{", repeat(field("entry", $.type_signature)), "}"),

    type_signature: ($) =>
      seq(field("name", $._signature_name), "::", field("type", $._type), ";"),

    _signature_name: ($) => choice($.identifier, $.string_expression),

    // ---------------------------------------------------------------------
    // Expressions

    _expression: ($) => $._expr_function_expression,

    _expr_function_expression: ($) =>
      choice(
        $.function_expression,
        $.assert_expression,
        $.with_expression,
        $.let_expression,
        $.if_expression,
        $._expr_op,
      ),

    function_expression: ($) =>
      choice(
        seq(field("universal", $.identifier), ":", field("body", $._expr_function_expression)),
        seq(field("formals", $.formals), ":", field("body", $._expr_function_expression)),
        seq(
          field("formals", $.formals),
          "@",
          field("universal", $.identifier),
          ":",
          field("body", $._expr_function_expression),
        ),
        seq(
          field("universal", $.identifier),
          "@",
          field("formals", $.formals),
          ":",
          field("body", $._expr_function_expression),
        ),
        seq(field("typed", $.typed_parameter), ":", field("body", $._expr_function_expression)),
      ),

    typed_parameter: ($) =>
      seq("(", field("name", $.identifier), "::", field("type", $._type), ")"),

    formals: ($) =>
      choice(
        seq("{", "}"),
        seq("{", commaSep1(field("formal", $.formal)), optional(","), "}"),
        seq("{", commaSep1(field("formal", $.formal)), ",", field("ellipses", $.ellipses), optional(","), "}"),
        seq("{", field("ellipses", $.ellipses), optional(","), "}"),
      ),

    formal: ($) =>
      seq(field("name", $.identifier), optional(seq("?", field("default", $._expression)))),

    ellipses: (_) => "...",

    assert_expression: ($) =>
      seq("assert", field("condition", $._expression), ";", field("body", $._expr_function_expression)),

    with_expression: ($) =>
      seq("with", field("environment", $._expression), ";", field("body", $._expr_function_expression)),

    let_expression: ($) =>
      seq("let", optional($.binding_set), "in", field("body", $._expr_function_expression)),

    if_expression: ($) =>
      seq(
        "if",
        field("condition", $._expression),
        "then",
        field("consequence", $._expression),
        "else",
        field("alternative", $._expression),
      ),

    _expr_op: ($) =>
      choice(
        $.has_attr_expression,
        $.cast_expression,
        $.unary_expression,
        $.binary_expression,
        $._expr_apply_expression,
      ),

    has_attr_expression: ($) =>
      prec(
        PREC.has_attr,
        seq(field("expression", $._expr_op), field("operator", "?"), field("attrpath", $.attrpath)),
      ),

    cast_expression: ($) =>
      prec.left(
        PREC.cast,
        seq(field("expression", $._expr_op), field("operator", "as"), field("type", $._type)),
      ),

    unary_expression: ($) =>
      choice(
        ...[
          ["!", PREC.not],
          ["-", PREC.negate],
        ].map(([operator, precedence]) =>
          prec(precedence, seq(field("operator", operator), field("argument", $._expr_op))),
        ),
      ),

    binary_expression: ($) =>
      choice(
        // left assoc.
        ...[
          ["==", PREC.eq],
          ["!=", PREC.neq],
          ["<", PREC.lt],
          ["<=", PREC.leq],
          [">", PREC.gt],
          [">=", PREC.geq],
          ["&&", PREC.and],
          ["||", PREC.or],
          ["+", PREC.plus],
          ["-", PREC.minus],
          ["*", PREC.mul],
          ["/", PREC.div],
          ["|>", PREC.pipe_left],
        ].map(([operator, precedence]) =>
          prec.left(
            precedence,
            seq(field("left", $._expr_op), field("operator", operator), field("right", $._expr_op)),
          ),
        ),
        // right assoc.
        ...[
          ["->", PREC.impl],
          ["//", PREC.update],
          ["++", PREC.concat],
          ["<|", PREC.pipe_right],
        ].map(([operator, precedence]) =>
          prec.right(
            precedence,
            seq(field("left", $._expr_op), field("operator", operator), field("right", $._expr_op)),
          ),
        ),
      ),

    _expr_apply_expression: ($) => choice($.apply_expression, $._expr_select_expression),

    apply_expression: ($) =>
      seq(field("function", $._expr_apply_expression), field("argument", $._expr_select_expression)),

    _expr_select_expression: ($) => choice($.select_expression, $._expr_simple),

    select_expression: ($) =>
      choice(
        seq(field("expression", $._expr_simple), ".", field("attrpath", $.attrpath)),
        seq(
          field("expression", $._expr_simple),
          ".",
          field("attrpath", $.attrpath),
          "or",
          field("default", $._expr_select_expression),
        ),
      ),

    _expr_simple: ($) =>
      choice(
        $.variable_expression,
        $.integer_expression,
        $.float_expression,
        $.string_expression,
        $.indented_string_expression,
        $.path_expression,
        $.hpath_expression,
        $.spath_expression,
        $.uri_expression,
        $.macro_invocation,
        $.parenthesized_expression,
        $.attrset_expression,
        $.let_attrset_expression,
        $.rec_attrset_expression,
        $.list_expression,
      ),

    identifier: (_) => /[a-zA-Z_][a-zA-Z0-9_'\-]*/,

    variable_expression: ($) => field("name", $.identifier),

    integer_expression: (_) => /[0-9]+/,

    float_expression: (_) => /(([1-9][0-9]*\.[0-9]*)|(0?\.[0-9]+)|(0\.[0-9]*))([Ee][+-]?[0-9]+)?/,

    path_expression: ($) =>
      seq(
        alias($._path_start, $.path_fragment),
        repeat(choice($.path_fragment, alias($._immediate_interpolation, $.interpolation))),
      ),

    _hpath_start: (_) => /\~\/[a-zA-Z0-9\._\-\+\/]*/,

    hpath_expression: ($) =>
      seq(
        alias($._hpath_start, $.path_fragment),
        repeat(choice($.path_fragment, alias($._immediate_interpolation, $.interpolation))),
      ),

    spath_expression: (_) => /<[a-zA-Z0-9\._\-\+]+(\/[a-zA-Z0-9\._\-\+]+)*>/,

    uri_expression: (_) => /[a-zA-Z][a-zA-Z0-9\+\-\.]*:[a-zA-Z0-9%\/\?:@\&=\+\$,\-_\.\!\~\*\']+/,

    _immediate_interpolation: ($) =>
      seq(alias(token.immediate("${"), "${"), field("expression", $._expression), "}"),

    interpolation: ($) => seq("${", field("expression", $._expression), "}"),

    string_expression: ($) =>
      seq(
        '"',
        repeat(choice($.string_fragment, $.interpolation, $.escape_sequence, $.dollar_escape)),
        '"',
      ),

    escape_sequence: (_) => token.immediate(/\\([^$]|\s)/),

    dollar_escape: (_) => token.immediate(/\\\$/),

    indented_string_expression: ($) =>
      seq(
        "''",
        repeat(
          choice(
            alias($._indented_string_fragment, $.string_fragment),
            $.interpolation,
            alias($._indented_escape_sequence, $.escape_sequence),
            alias($._indented_dollar_escape, $.dollar_escape),
          ),
        ),
        "''",
      ),

    _indented_escape_sequence: (_) => token.immediate(/'''|''\\([^$]|\s)/),

    _indented_dollar_escape: (_) => token.immediate(/''\$|''\\\$/),

    parenthesized_expression: ($) =>
      seq("(", field("expression", $._expression), optional(seq("::", field("type", $._type))), ")"),

    attrset_expression: ($) => seq("{", optional($.binding_set), "}"),

    let_attrset_expression: ($) => seq("let", "{", optional($.binding_set), "}"),

    rec_attrset_expression: ($) => seq("rec", "{", optional($.binding_set), "}"),

    list_expression: ($) => seq("[", repeat(field("element", $._expr_select_expression)), "]"),

    binding_set: ($) =>
      repeat1(field("binding", choice($.binding, $.inherit, $.inherit_from, $.type_signature))),

    binding: ($) =>
      seq(field("attrpath", $.attrpath), "=", field("expression", $._expression), ";"),

    attrpath: ($) => seq(field("attr", $._attr), repeat(seq(".", field("attr", $._attr)))),

    _attr: ($) => choice($.identifier, $.string_expression, $.interpolation),

    inherit: ($) => seq("inherit", field("attrs", $.inherited_attrs), ";"),

    inherit_from: ($) =>
      seq("inherit", "(", field("expression", $._expression), ")", field("attrs", $.inherited_attrs), ";"),

    inherited_attrs: ($) => repeat1(field("attr", $._attr)),

    // ---------------------------------------------------------------------
    // Types

    _type: ($) =>
      choice(
        $.forall_type,
        $.constrained_type,
        $.conditional_type,
        $.function_type,
        $.union_type,
        $.type_application,
        $._type_atom,
      ),

    forall_type: ($) =>
      seq(
        "forall",
        repeat1(field("variable", alias($.identifier, $.type_variable))),
        ".",
        field("body", $._type),
      ),

    constrained_type: ($) =>
      prec.right(PREC.type, seq(field("context", $._type_union), "=>", field("body", $._type))),

    conditional_type: ($) =>
      prec.right(
        PREC.type,
        seq(
          field("check", $._type_union),
          "extends",
          field("pattern", $._type_union),
          "?",
          field("consequence", $._type),
          ":",
          field("alternative", $._type),
        ),
      ),

    function_type: ($) =>
      prec.right(
        PREC.type,
        seq(
          field("parameter", choice($._type_union, $.dependent_parameter)),
          optional(alias("%1", $.multiplicity)),
          "->",
          optional(field("captures", $.capture_set)),
          field("result", $._type),
          optional(field("effects", $.effect_row)),
        ),
      ),

    // `(n :: Nat) -> Vec n a`
    dependent_parameter: ($) =>
      seq("(", field("name", alias($.identifier, $.type_variable)), "::", field("type", $._type), ")"),

    // `->{fetch, log}`, glued to the arrow.
    capture_set: ($) =>
      seq(token.immediate("{"), optional(commaSep1(field("capability", $.identifier))), "}"),

    // `! { Trace, Throw | e }`, `! e`, `! {}`
    effect_row: ($) =>
      seq(
        "!",
        choice(
          seq(
            "{",
            optional(commaSep1(field("effect", alias($.identifier, $.effect_label)))),
            optional(seq("|", field("tail", alias($.identifier, $.type_variable)))),
            "}",
          ),
          field("tail", alias($.identifier, $.type_variable)),
        ),
      ),

    _type_union: ($) => choice($.union_type, $._type_app),

    union_type: ($) =>
      prec.left(
        PREC.type,
        seq(field("member", $._type_app), repeat1(seq("|", field("member", $._type_app)))),
      ),

    _type_app: ($) => choice($.type_application, $._type_atom),

    type_application: ($) =>
      prec.left(
        PREC.type,
        seq(field("constructor", $._type_atom), repeat1(field("argument", $._type_atom))),
      ),

    _type_atom: ($) =>
      choice(
        $.parenthesized_type,
        $.record_type,
        $.type_list,
        $.builtin_type,
        $.infer_type,
        $.literal_type,
        alias($.identifier, $.type_identifier),
      ),

    parenthesized_type: ($) => seq("(", field("type", $._type), ")"),

    // Record types: `name :: T;`, optional fields `name? :: T;`, and an
    // optional trailing open row `...` / `...r` (with or without `;`).
    record_type: ($) =>
      seq(
        "{",
        repeat(field("field", choice($.type_signature, $.optional_type_signature))),
        optional(field("row", $.row_tail)),
        "}",
      ),

    optional_type_signature: ($) =>
      seq(field("name", $._signature_name), "?", "::", field("type", $._type), ";"),

    row_tail: ($) => seq($.ellipses, optional(field("name", $.identifier)), optional(";")),

    type_list: ($) => seq("[", repeat(field("element", $._type_atom)), "]"),

    builtin_type: (_) => choice("any", "dynamic", "unknown"),

    infer_type: ($) => seq("infer", field("name", alias($.identifier, $.type_variable))),

    literal_type: ($) =>
      choice(
        $.string_expression,
        $.integer_expression,
        $.float_expression,
        seq("-", choice($.integer_expression, $.float_expression)),
        alias("true", $.boolean),
        alias("false", $.boolean),
      ),

    // ---------------------------------------------------------------------
    // Trivia

    directive: (_) => token(prec(2, /#[ \t]*@tynix-(ignore|expected)[^\n]*/)),

    comment: (_) =>
      token(choice(seq("#", /.*/), seq("/*", /[^*]*\*+([^/*][^*]*\*+)*/, "/"))),
  },
});
