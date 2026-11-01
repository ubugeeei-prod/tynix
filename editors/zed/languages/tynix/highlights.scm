; tynix highlights for Zed.
;
; Zed resolves overlapping captures in favour of the LAST matching pattern,
; so generic captures come first and specific ones (functions, builtins,
; definitions) follow. Capture names fall back by prefix (`type.builtin` ->
; `type`) when a theme does not define the more specific key.

(comment) @comment

(directive) @preproc

; Identifiers ----------------------------------------------------------------

(variable_expression
  name: (identifier) @variable)

(function_expression
  universal: (identifier) @variable.parameter)

(formal
  name: (identifier) @variable.parameter)

(typed_parameter
  name: (identifier) @variable.parameter)

(ellipses) @punctuation.special

(attrpath
  attr: (identifier) @property)

(inherited_attrs
  attr: (identifier) @property)

(type_signature
  name: (identifier) @property)

(binding_set
  binding: (type_signature
    name: (identifier) @variable))

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
    name: (identifier) @function))

(apply_expression
  function: (select_expression
    attrpath: (attrpath
      attr: (identifier) @function .)))

((variable_expression
  name: (identifier) @variable.special)
  (#eq? @variable.special "builtins"))

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

; Literals -------------------------------------------------------------------

[
  (integer_expression)
  (float_expression)
] @number

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
] @string.special

(uri_expression) @link_uri

(interpolation
  [
    "${"
    "}"
  ] @punctuation.special)

(interpolation
  expression: (_) @embedded)

; Keywords -------------------------------------------------------------------

[
  "if"
  "then"
  "else"
  "let"
  "in"
  "with"
  "rec"
  "inherit"
  "assert"
  "type"
  "declare"
  "forall"
  "extends"
  "infer"
] @keyword

[
  "as"
  "or"
] @keyword.operator

(multiplicity) @attribute

(opaque) @keyword

"macro" @keyword

(effect_row
  "!" @operator)

(effect_label) @type

(capture_set
  capability: (identifier) @variable.parameter)

(kind) @type

(macro_declaration
  name: (identifier) @function)

(macro_invocation
  name: (identifier) @function)

(metavariable
  name: (metavariable_name) @variable.special)

(metavariable
  fragment: (fragment) @type)

(macro_rule
  "=>" @operator)

; Types ----------------------------------------------------------------------

(type_identifier) @type

((type_identifier) @type.parameter
  (#match? @type.parameter "^[a-z_]"))

(type_variable) @type.parameter

(type_alias
  name: (type_identifier) @type.definition)

((type_identifier) @type.builtin
  (#any-of? @type.builtin
    "AttrSet" "Attrs" "Bool" "Derivation" "Float" "Int" "List" "Matrix" "Nat" "Null" "Number"
    "Path" "Range" "String" "Tensor" "Tuple" "Unit" "Vec"))

(builtin_type) @type.builtin

(literal_type
  (boolean) @boolean)

; Operators and punctuation --------------------------------------------------

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

(conditional_type
  [
    "?"
    ":"
  ] @operator)

(formal
  "?" @operator)

[
  ";"
  "."
  ","
  ":"
] @punctuation.delimiter

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket
