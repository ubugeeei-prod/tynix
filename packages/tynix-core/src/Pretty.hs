{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pretty-printers for emitted `.nix`, `.d.tynix`, and human-facing type text.
--
-- One module owns all rendering so that CLI output, declaration files, and
-- debug/test expectations share the same surface representation.
module Pretty
  ( renderDeclarationFile,
    renderExpr,
    renderKind,
    renderProgram,
    renderProgramAsNix,
    renderScheme,
    renderType,
  )
where

import Data.Char (isAlphaNum, isLetter)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Indexed (tensorView, tupleView)
import Numeric (showGFloat)
import Prettyprinter
import Prettyprinter.Render.Text qualified as Render
import Syntax
import Type

-- | Render a full program back to tynix surface syntax, preserving type aliases,
-- ambient declarations, and embedded type annotations (unlike
-- 'renderProgramAsNix', which erases type-only syntax). Used by the LSP
-- formatter. Declarations are emitted before the root expression; comments are
-- not represented in the AST, so callers must guard against destroying them.
renderProgram :: Program -> Text
renderProgram program =
  render $
    vsep $
      map prettyAlias (programAliases program)
        <> map prettyAmbient (programAmbient program)
        <> maybe [] (\marked -> [prettyExpr 0 (markedValue marked)]) (programExpr program)

prettyAmbient :: AmbientDecl -> Doc ann
prettyAmbient decl =
  prettyDecl
    (ambientPath decl)
    [(ambientEntryName entry, ambientEntryType entry) | entry <- ambientEntries decl]

-- | Render an executable program back to plain Nix code.
renderProgramAsNix :: Program -> Either Text Text
renderProgramAsNix program =
  maybe (Left "declaration-only files cannot be compiled to .nix") (Right . render . prettyExpr 0 . markedValue) (programExpr program)

-- | Render a declaration file for a target path and exported entries.
renderDeclarationFile :: FilePath -> [TypeAlias] -> [(Name, Type)] -> Text
renderDeclarationFile path aliases entries = render $ vsep (map prettyAlias aliases <> [prettyDecl path entries])

-- | Render an expression using tynix/Nix surface syntax.
renderExpr :: Expr -> Text
renderExpr = render . prettyExpr 0

-- | Render a type without scheme quantifiers.
renderType :: Type -> Text
renderType = render . prettyType 0

-- | Render a kind using arrow syntax for higher-kinded constructors.
renderKind :: Kind -> Text
renderKind = render . prettyKind 0

-- | Render a polymorphic scheme for CLI and LSP display.
renderScheme :: Scheme -> Text
renderScheme (Scheme vars ty) =
  render $
    if null vars
      then prettyType 0 ty
      else "forall" <+> hsep (pretty <$> vars) <> "." <+> prettyType 0 ty

render :: Doc ann -> Text
render = Render.renderStrict . layoutPretty defaultLayoutOptions

prettyAlias :: TypeAlias -> Doc ann
prettyAlias alias =
  (if typeAliasOpaque alias then "opaque type" else "type")
    <+> pretty (typeAliasName alias)
    <+> hsep (zipWith prettyParam (typeAliasParams alias) (typeAliasParamKinds alias <> repeat Nothing))
    <+> "="
    <+> prettyType 0 (typeAliasBody alias)
    <> ";"

prettyParam :: Name -> Maybe Kind -> Doc ann
prettyParam name = \case
  Nothing -> pretty name
  Just kind -> parens (pretty name <+> "::" <+> prettyKind 0 kind)

prettyDecl :: FilePath -> [(Name, Type)] -> Doc ann
prettyDecl path entries =
  vsep
    [ "declare" <+> prettyQuoted (Text.pack path) <+> "{",
      indent 2 (vsep [prettyAttrName name <+> "::" <+> prettyType 0 ty <> ";" | (name, ty) <- entries]),
      "};"
    ]

-- | Render an expression at a given precedence context.
--
-- Precedence levels (higher binds tighter) follow Nix: 1 pipes, 2 `->`,
-- 3 `||`, 4 `&&`, 5 equality, 6 comparisons, 7 `//`, 8 `!`, 9 `+`/`-`,
-- 10 `*`/`/`, 11 `++`, 12 `?`, 13 prefix `-`, 14 `as`, 15 application,
-- 16 selection. Level 0 is an unrestricted expression position.
prettyExpr :: Int -> Expr -> Doc ann
prettyExpr p = \case
  ELoc _ inner -> prettyExpr p inner
  EVar name -> pretty name
  EString value -> prettyStringLiteral value
  EFloat value
    | value < 0 -> parenIf (p > 13) (pretty (prettyFloat value))
    | otherwise -> pretty (prettyFloat value)
  EInt value
    | value < 0 -> parenIf (p > 13) (pretty value)
    | otherwise -> pretty value
  EBool True -> "true"
  EBool False -> "false"
  ENull -> "null"
  EPath path -> pretty path
  ESearchPath path -> "<" <> pretty path <> ">"
  EPathInterp parts -> hcat (map prettyPathPart parts)
  -- Control-flow forms extend to the right, so they must be parenthesized
  -- whenever they appear in any tighter position (p > 0).
  ELambda pattern' body -> parenIf (p > 0) (prettyPattern pattern' <> ":" <+> prettyExpr 0 body)
  EIf a b c -> parenIf (p > 0) (vsep ["if" <+> prettyExpr 0 a, "then" <+> prettyExpr 0 b, "else" <+> prettyExpr 0 c])
  ELet items body -> parenIf (p > 0) (vsep ["let", indent 2 (vsep (map (prettyLet . markedValue) items)), "in" <+> prettyExpr 0 body])
  EAssert cond body -> parenIf (p > 0) ("assert" <+> prettyExpr 0 cond <> ";" <+> prettyExpr 0 body)
  EWith scope body -> parenIf (p > 0) ("with" <+> prettyExpr 0 scope <> ";" <+> prettyExpr 0 body)
  -- Operators: each level parenthesizes only when the surrounding context binds
  -- tighter than the operator, and the non-associative operand side is bumped by
  -- one so equal-precedence nesting parenthesizes correctly.
  EBinaryOp op left right ->
    let q = binOpPrec op
        (leftP, rightP)
          | binOpNonAssoc op = (q + 1, q + 1)
          | binOpRightAssoc op = (q + 1, q)
          | otherwise = (q, q + 1)
     in parenIf (p > q) (prettyExpr leftP left <+> pretty (binOpSymbol op) <+> prettyExpr rightP right)
  EUnaryOp OpNot operand -> parenIf (p > 8) ("!" <> prettyExpr 8 operand)
  EUnaryOp OpNeg operand -> parenIf (p > 13) ("-" <> prettyExpr 14 operand)
  EHasAttr base path -> parenIf (p > 12) (prettyExpr 13 base <+> "?" <+> prettyAttrPath path)
  ECast expr ty -> parenIf (p > 14) (prettyExpr 14 expr <+> "as" <+> prettyType 0 ty)
  EAscribe expr ty -> parens (prettyExpr 0 expr <+> "::" <+> prettyType 0 ty)
  EApp f x -> parenIf (p > 15) (prettyExpr 15 f <+> prettyExpr 16 x)
  ESelect base steps -> parenIf (p > 16) (prettyExpr 16 base <> foldMap prettySelectStep steps)
  ESelectOr base steps fallback -> parenIf (p > 15) (prettyExpr 16 base <> foldMap prettySelectStep steps <+> "or" <+> prettyExpr 16 fallback)
  -- Self-delimiting atoms never need outer parentheses; list/application
  -- operands are rendered tightly so nested calls and operators stay grouped.
  EAttrSet items -> vsep ["{", indent 2 (vsep (map prettyAttr items)), "}"]
  ERec items -> vsep ["rec {", indent 2 (vsep (map prettyAttr items)), "}"]
  EList items -> "[" <+> hsep (map (prettyExpr 16) items) <+> "]"
  EInterp form parts -> prettyInterp form parts

prettyPathPart :: StringPart -> Doc ann
prettyPathPart = \case
  StrText text -> pretty text
  StrEscape c -> pretty (Text.pack ['\\', c])
  StrExpr expr -> "${" <> prettyExpr 0 expr <> "}"

prettyLet :: LetItem -> Doc ann
prettyLet = \case
  LetSignature name ty -> pretty name <+> "::" <+> prettyType 0 ty <> ";"
  LetBinding name expr -> pretty name <+> "=" <+> prettyExpr 0 expr <> ";"
  LetInherit source names -> prettyInherit source names
  LetPath steps expr -> prettyAttrPath steps <+> "=" <+> prettyExpr 0 expr <> ";"

prettyInherit :: Maybe Expr -> [Name] -> Doc ann
prettyInherit source names =
  hsep (["inherit"] <> maybe [] (\expr -> [parens (prettyExpr 0 expr)]) source <> map prettyAttrName names) <> ";"

prettyAttrPath :: [SelectStep] -> Doc ann
prettyAttrPath steps = hcat (punctuate "." (map prettyKey steps))
  where
    prettyKey = \case
      SelectName name -> prettyAttrName name
      SelectDynamic expr -> prettyDynamicKey expr

prettyDynamicKey :: Expr -> Doc ann
prettyDynamicKey expr =
  case stripLocations expr of
    interp@EInterp{} -> prettyExpr 0 interp
    other -> "${" <> prettyExpr 0 other <> "}"

prettyPattern :: Pattern -> Doc ann
prettyPattern = \case
  PVar name _ -> pretty name
  PAttrSet fields open binder ->
    let body =
          case map prettyField fields <> ["..." | open] of
            [] -> "{ }"
            items -> "{ " <> hsep (punctuate "," items) <> " }"
     in case binder of
          Nothing -> body
          Just (BinderBefore name) -> pretty name <> "@" <> body
          Just (BinderAfter name) -> body <> "@" <> pretty name
    where
      prettyField field =
        pretty (patternFieldName field)
          <> maybe mempty (\expr -> " ?" <+> prettyExpr 0 expr) (patternFieldDefault field)

prettySelectStep :: SelectStep -> Doc ann
prettySelectStep = \case
  SelectName name -> "." <> prettyAttrName name
  SelectDynamic expr -> "." <> prettyDynamicKey expr

prettyAttr :: AttrItem -> Doc ann
prettyAttr = \case
  AttrField name expr -> prettyAttrName name <+> "=" <+> prettyExpr 0 expr <> ";"
  AttrInherit names -> prettyInherit Nothing names
  AttrInheritFrom source names -> prettyInherit (Just source) names
  AttrPath steps expr -> prettyAttrPath steps <+> "=" <+> prettyExpr 0 expr <> ";"

prettyAttrName :: Name -> Doc ann
prettyAttrName name
  | isBareAttrName name && name `notElem` ["inherit", "rec", "let", "in", "if", "then", "else", "assert", "with", "or"] = pretty name
  | otherwise = prettyQuoted name

prettyStringLiteral :: StringLiteral -> Doc ann
prettyStringLiteral = \case
  DoubleQuoted value -> prettyQuoted value
  Indented value -> "''" <> verbatim (escapeIndented True True value) <> "''"

-- | Render an interpolated string back to its surface form, restoring `${...}`
-- antiquotations around the embedded expressions.
prettyInterp :: InterpForm -> [StringPart] -> Doc ann
prettyInterp form parts =
  let lastIndex = length parts - 1
      -- A segment followed by an escape (`''\n`) borders a `''` just like
      -- the closing delimiter, so it gets the same quote escaping.
      bordersQuotes index = index == lastIndex || isEscape (drop (index + 1) parts)
      isEscape (StrEscape _ : _) = True
      isEscape _ = False
      body = hcat [prettyStringPart form (index == 0) (bordersQuotes index) part | (index, part) <- zip [0 ..] parts]
   in case form of
        InterpDouble -> dquotes body
        InterpIndented -> "''" <> body <> "''"

-- | Render one segment of an interpolated string.
--
-- The two flags say whether the segment touches the opening or the closing
-- delimiter, which is what decides whether a boundary @\'@ inside an indented
-- run has to be escaped.
prettyStringPart :: InterpForm -> Bool -> Bool -> StringPart -> Doc ann
prettyStringPart form atStart atEnd = \case
  StrText text ->
    -- A `$` right before an antiquotation would read back as the `$${`
    -- literal escape, so it is escaped explicitly.
    let (body, trailingDollar) =
          if not atEnd && "$" `Text.isSuffixOf` text
            then (Text.dropEnd 1 text, True)
            else (text, False)
     in case form of
          InterpDouble -> pretty (escapeDoubleQuoted body) <> (if trailingDollar then "\\$" else mempty)
          InterpIndented -> verbatim (escapeIndented atStart (atEnd || trailingDollar) body) <> (if trailingDollar then "''$" else mempty)
  StrExpr expr -> "${" <> prettyExpr 0 expr <> "}"
  StrEscape c ->
    case form of
      InterpIndented -> "''\\" <> pretty (Text.singleton c)
      InterpDouble -> pretty (escapeDoubleQuoted (Text.singleton (decodeEscape c)))
  where
    decodeEscape = \case
      'n' -> '\n'
      'r' -> '\r'
      't' -> '\t'
      other -> other

prettyType :: Int -> Type -> Doc ann
prettyType p ty =
  case tupleView ty of
    Just items -> parenIf (p > 2) ("Tuple" <+> prettyType 3 (TTypeList items))
    Nothing ->
      case tensorView ty of
        Just (dims, elemTy) ->
          case dims of
            [lenTy] -> parenIf (p > 2) ("Vec" <+> prettyType 3 lenTy <+> prettyType 3 elemTy)
            [rowsTy, colsTy] -> parenIf (p > 2) ("Matrix" <+> prettyType 3 rowsTy <+> prettyType 3 colsTy <+> prettyType 3 elemTy)
            _ -> parenIf (p > 2) ("Tensor" <+> prettyType 3 (TTypeList dims) <+> prettyType 3 elemTy)
        Nothing ->
          case ty of
            TVar name -> pretty name
            TCon name -> pretty name
            TMeta n -> pretty ("?" <> show n)
            TLit (LString text) -> prettyQuoted text
            TLit (LFloat n) -> pretty (prettyFloat n)
            TLit (LInt n) -> pretty n
            TLit (LBool True) -> "true"
            TLit (LBool False) -> "false"
            TAny -> "any"
            TTypeList items -> "[" <+> hsep (prettyType 3 <$> items) <+> "]"
            TDynamic -> "dynamic"
            TUnknown -> "unknown"
            TArrow arrow a b ->
              let mult =
                    case arrowMult arrow of
                      One -> "%1 "
                      Many -> mempty
                  captures = maybe mempty (\names -> if null names then mempty else braces (hsep (punctuate "," (pretty <$> names)))) (arrowCaptures arrow)
                  domain =
                    case arrowBinder arrow of
                      Just name -> parens (pretty name <+> "::" <+> prettyType 0 a)
                      Nothing -> prettyType 1 a
                  effects = prettyEffects (arrowEffects arrow)
                  codomain =
                    case (effects, b) of
                      (Just _, TArrow{}) -> parens (prettyType 0 b)
                      _ -> prettyType 0 b
               in parenIf (p > 0) (domain <+> mult <> "->" <> captures <+> codomain <> maybe mempty (" !" <+>) effects)
            TRecord fields -> prettyRecordType fields Nothing
            TOpenRecord fields tail' -> prettyRecordType fields (Just tail')
            TOptional inner -> prettyType p inner
            TUnion members -> parenIf (p > 1) (hsep (punctuate " |" (map (prettyType 2) members)))
            TApp f x -> parenIf (p > 2) (prettyType 2 f <+> prettyType 3 x)
            TForall vars body -> parenIf (p > 0) ("forall" <+> hsep (pretty <$> vars) <> "." <+> prettyType 0 body)
            TConditional a b c d -> parenIf (p > 0) (prettyType 2 a <+> "extends" <+> prettyType 2 b <+> "?" <+> prettyType 0 c <+> ":" <+> prettyType 0 d)
            TInfer name -> "infer" <+> pretty name
            TSingleton name _ -> pretty name

-- | Render an arrow's latent effect row, or nothing for a pure or untracked
-- arrow (the two read the same in hovers; only real effects are worth
-- showing).
prettyEffects :: Type -> Maybe (Doc ann)
prettyEffects = \case
  TDynamic -> Nothing
  TMeta _ -> Nothing
  TVar name -> Just (pretty name)
  TRecord fields
    | Map.null fields -> Nothing
    | otherwise -> Just (braces (hsep (punctuate "," (pretty <$> Map.keys fields))))
  TOpenRecord fields tail'
    | Map.null fields -> prettyEffects tail'
    | otherwise ->
        Just
          ( braces
              ( hsep (punctuate "," (pretty <$> Map.keys fields))
                  <> case tail' of
                    TVar name -> " |" <+> pretty name
                    _ -> mempty
              )
          )
  _ -> Nothing

-- | Render a record type. Optional fields print as `name? :: T;`, and an open
-- row ends in `...` (or `...r` when the row is a named variable).
prettyRecordType :: Map.Map Name Type -> Maybe Type -> Doc ann
prettyRecordType fields rowTail =
  vsep ["{", indent 2 (vsep (map prettyField (Map.toList fields) <> rowLine)), "}"]
  where
    prettyField (k, TOptional v) = prettyAttrName k <> "?" <+> "::" <+> prettyType 0 v <> ";"
    prettyField (k, v) = prettyAttrName k <+> "::" <+> prettyType 0 v <> ";"
    rowLine =
      case rowTail of
        Nothing -> []
        Just (TVar name) -> ["..." <> pretty name]
        Just _ -> ["..."]

-- | Emit text with its own line breaks, immune to the surrounding layout.
--
-- An indented string keeps its body verbatim, so the renderer must not add the
-- enclosing block's indentation after each newline: doing so changes the string
-- and compounds every time the file is compiled again. Resetting the nesting
-- level to zero for the body keeps rendering idempotent.
verbatim :: Text -> Doc ann
verbatim text = nesting (\level -> nest (negate level) (pretty text))

-- | Render text as a Nix double-quoted string literal, escaping every
-- character that would otherwise change how the result re-parses.
prettyQuoted :: Text -> Doc ann
prettyQuoted = dquotes . pretty . escapeDoubleQuoted

-- | Escape text for a Nix double-quoted string literal.
--
-- Quotes, backslashes, and the control characters Nix spells with an escape
-- are rewritten to their escaped forms. A @$@ is escaped only when it would
-- otherwise open an antiquotation, so ordinary shell-ish text stays readable.
escapeDoubleQuoted :: Text -> Text
escapeDoubleQuoted = Text.pack . go . Text.unpack
  where
    go [] = []
    go ('"' : rest) = '\\' : '"' : go rest
    go ('\\' : rest) = '\\' : '\\' : go rest
    go ('\n' : rest) = '\\' : 'n' : go rest
    go ('\r' : rest) = '\\' : 'r' : go rest
    go ('\t' : rest) = '\\' : 't' : go rest
    go ('$' : '{' : rest) = '\\' : '$' : '{' : go rest
    go (char : rest) = char : go rest

-- | Escape text for a Nix indented (@\'\'@) string literal.
--
-- Antiquotation openers become @\'\'${@. A single quote only needs escaping
-- when it would pair up with a neighbouring quote and be read back as
-- something else: at the opening delimiter, at the closing delimiter, before
-- another quote, or immediately before an escaped @${@. Everywhere else a
-- bare quote round-trips as itself, which keeps embedded shell snippets
-- legible.
--
-- @atStart@ and @atEnd@ say whether this run of text touches the opening or
-- the closing delimiter; interpolated strings pass 'False' for the segments
-- that sit next to an antiquotation instead.
escapeIndented :: Bool -> Bool -> Text -> Text
escapeIndented atStart atEnd = Text.pack . go atStart . Text.unpack
  where
    go _ [] = []
    go _ ('$' : '{' : rest) = '\'' : '\'' : '$' : '{' : go False rest
    go atBoundary ('\'' : rest)
      | needsEscape atBoundary rest = '\'' : '\'' : '\\' : '\'' : go False rest
      | otherwise = '\'' : go False rest
    go _ (char : rest) = char : go False rest

    needsEscape atBoundary rest =
      atBoundary
        || (atEnd && null rest)
        || take 1 rest == "'"
        || take 2 rest == "${"

isBareAttrName :: Text -> Bool
isBareAttrName name =
  case Text.uncons name of
    Just (first, rest) -> attrNameStart first && Text.all attrNameCont rest
    Nothing -> False
  where
    attrNameStart c = isLetter c || c == '_'
    attrNameCont c = isAlphaNum c || c `elem` ("_'-" :: String)

prettyKind :: Int -> Kind -> Doc ann
prettyKind p = \case
  KType -> "Type"
  KMeta n -> pretty ("?" <> show n)
  KFun a b -> parenIf (p > 0) (prettyKind 1 a <+> "->" <+> prettyKind 0 b)

parenIf :: Bool -> Doc ann -> Doc ann
parenIf True = parens
parenIf False = id

-- | Binding tightness of each binary operator (higher binds tighter), matching
-- the parser's precedence ladder so re-parsing reproduces the same tree.
binOpPrec :: BinOp -> Int
binOpPrec = \case
  OpPipeRight -> 1
  OpPipeLeft -> 1
  OpImpl -> 2
  OpOr -> 3
  OpAnd -> 4
  OpEq -> 5
  OpNeq -> 5
  OpLt -> 6
  OpGt -> 6
  OpLe -> 6
  OpGe -> 6
  OpUpdate -> 7
  OpAdd -> 9
  OpSub -> 9
  OpMul -> 10
  OpDiv -> 10
  OpConcat -> 11

-- | Equality and ordered comparisons do not chain in Nix (`a == b == c` is a
-- syntax error), so nested comparisons are always parenthesized.
binOpNonAssoc :: BinOp -> Bool
binOpNonAssoc = \case
  OpEq -> True
  OpNeq -> True
  OpLt -> True
  OpGt -> True
  OpLe -> True
  OpGe -> True
  _ -> False

-- | List concatenation and attribute-set update are right-associative.
binOpRightAssoc :: BinOp -> Bool
binOpRightAssoc = \case
  OpUpdate -> True
  OpConcat -> True
  OpImpl -> True
  OpPipeLeft -> True
  _ -> False

binOpSymbol :: BinOp -> Text
binOpSymbol = \case
  OpAdd -> "+"
  OpSub -> "-"
  OpMul -> "*"
  OpConcat -> "++"
  OpUpdate -> "//"
  OpEq -> "=="
  OpNeq -> "!="
  OpLt -> "<"
  OpGt -> ">"
  OpLe -> "<="
  OpGe -> ">="
  OpAnd -> "&&"
  OpOr -> "||"
  OpDiv -> "/"
  OpImpl -> "->"
  OpPipeRight -> "|>"
  OpPipeLeft -> "<|"

prettyFloat :: Double -> String
prettyFloat n =
  let rendered = showGFloat Nothing n ""
   in if any (`elem` (".eE" :: String)) rendered
        then rendered
        else rendered <> ".0"
