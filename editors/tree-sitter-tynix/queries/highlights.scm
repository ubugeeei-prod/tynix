; tynix highlights (nvim-treesitter capture names).
;
; Later patterns win in Neovim, so generic captures come first and the more
; specific ones (functions, builtins, type constructors) follow.

; ---------------------------------------------------------------------------
; Trivia

(comment) @comment @spell

(directive) @keyword.directive

; ---------------------------------------------------------------------------
; Identifiers

(variable_expression
  name: (identifier) @variable)

(function_expression
  universal: (identifier) @variable.parameter)

(formal
  name: (identifier) @variable.parameter)

(typed_parameter
  name: (identifier) @variable.parameter)

(ellipses) @punctuation.special

; Attribute names
(attrpath
  attr: (identifier) @variable.member)

(inherited_attrs
  attr: (identifier) @variable.member)

(type_signature
  name: (identifier) @variable.member)

(binding_set
  binding: (type_signature
    name: (identifier) @variable))

; Functions
(binding
  attrpath: (attrpath
    attr: (identifier) @function .)
  expression: (function_expression))

(binding_set
  binding: (type_signature
    name: (identifier) @function
    type: [
      (function_type)
      (forall_type
        body: (function_type))
    ]))

(declaration_block
  entry: (type_signature
    name: (identifier) @function
    type: [
      (function_type)
      (forall_type
        body: (function_type))
    ]))

(apply_expression
  function: (variable_expression
    name: (identifier) @function.call))

(apply_expression
  function: (select_expression
    attrpath: (attrpath
      attr: (identifier) @function.call .)))

; Builtins
((variable_expression
  name: (identifier) @variable.builtin)
  (#eq? @variable.builtin "builtins"))

((variable_expression
  name: (identifier) @function.builtin)
  (#any-of? @function.builtin
    "abort" "baseNameOf" "break" "derivation" "derivationStrict" "dirOf" "fetchGit"
    "fetchMercurial" "fetchTarball" "fetchTree" "fromTOML" "import" "isNull" "map"
    "placeholder" "removeAttrs" "scopedImport" "throw" "toString"))

((select_expression
  expression: (variable_expression
    name: (identifier) @_builtins)
  attrpath: (attrpath
    attr: (identifier) @function.builtin))
  (#eq? @_builtins "builtins"))

((variable_expression
  name: (identifier) @boolean)
  (#any-of? @boolean "true" "false"))

((variable_expression
  name: (identifier) @constant.builtin)
  (#any-of? @constant.builtin "null" "__curPos" "__currentSystem" "__nixPath" "__storeDir"))

; ---------------------------------------------------------------------------
; Literals

(integer_expression) @number

(float_expression) @number.float

[
  (string_expression)
  (indented_string_expression)
] @string

[
  (escape_sequence)
  (dollar_escape)
] @string.escape

[
  (path_expression)
  (hpath_expression)
  (spath_expression)
] @string.special.path

(uri_expression) @string.special.url

(interpolation
  [
    "${"
    "}"
  ] @punctuation.special)

; ---------------------------------------------------------------------------
; Keywords

[
  "if"
  "then"
  "else"
] @keyword.conditional

[
  "let"
  "in"
  "with"
  "rec"
  "inherit"
  "declare"
] @keyword

"assert" @keyword.debug

"type" @keyword.type

[
  "forall"
  "extends"
  "infer"
] @keyword

[
  "as"
  "or"
] @keyword.operator

(multiplicity) @keyword.modifier

(opaque) @keyword.modifier

"macro" @keyword

; Effect rows (`! { Trace | e }`), capture sets (`->{fetch}`), and kinds.
(effect_row
  "!" @operator)

(effect_label) @type.builtin

(capture_set
  capability: (identifier) @variable.parameter)

(kind) @type.builtin

(kind_arrow
  "->" @operator)

; Macros: invocations, metavariables, and repetitions.
(macro_declaration
  name: (identifier) @function.macro)

(macro_invocation
  name: (identifier) @function.macro)

(metavariable
  name: (metavariable_name) @variable.parameter)

(metavariable
  fragment: (fragment) @type.builtin)

(macro_rule
  "=>" @operator)

(macro_repetition
  "$(" @punctuation.special)

; ---------------------------------------------------------------------------
; Types

(type_identifier) @type

((type_identifier) @variable.parameter
  (#lua-match? @variable.parameter "^[a-z_]"))

(type_variable) @variable.parameter

(type_alias
  name: (type_identifier) @type.definition)

((type_identifier) @type.builtin
  (#any-of? @type.builtin
    "AttrSet" "Attrs" "Bool" "Derivation" "Float" "Int" "List" "Matrix" "Nat" "Null" "Number"
    "Path" "Range" "String" "Tensor" "Tuple" "Unit" "Vec"))

(builtin_type) @type.builtin

(literal_type
  (boolean) @boolean)

; ---------------------------------------------------------------------------
; Operators and punctuation

(binary_expression
  operator: _ @operator)

(unary_expression
  operator: _ @operator)

(has_attr_expression
  operator: _ @operator)

[
  "="
  "@"
  "::"
  "->"
  "=>"
  "|"
] @operator

(literal_type
  "-" @operator)

[
  ";"
  "."
  ","
  ":"
] @punctuation.delimiter

(conditional_type
  [
    "?"
    ":"
  ] @keyword.conditional.ternary)

(formal
  "?" @operator)

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

; Optional record fields (`name? :: T;`) and open rows (`...`, `...r`)
(optional_type_signature
  name: (identifier) @variable.member)

(optional_type_signature
  "?" @punctuation.special)

(row_tail
  (ellipses) @punctuation.special)

(row_tail
  name: (identifier) @type.parameter)
