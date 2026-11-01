{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parser for top-level declarations and executable expressions.
--
-- The parser preserves Nix-like surface structure as much as possible so that
-- compilation can be implemented as a mostly mechanical erasure pass. The
-- executable grammar aims at parity with the Nix language: every construct
-- the reference implementation accepts should parse here too, with tynix's
-- type-only syntax layered on top.
--
-- Expressions are wrapped in 'ELoc' nodes carrying source offsets so that the
-- checker can report span-accurate diagnostics.
module ParserExpr (expressionParser, programParser) where

import Control.Monad (void, when)
import Control.Monad.Reader (ask, asks, local, runReaderT)
import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper, isDigit, isLetter)
import Data.Either (lefts)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Text qualified as Text
import Diagnostics (DiagnosticCode (..), withCode)
import Macro
import ParserLexer
import ParserType
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)
import Type

-- | Parse a full tynix source file.
--
-- Declarations come first. A macro is in scope from its declaration on, for
-- later declarations and the root expression alike.
programParser :: Parser Program
programParser = go [] [] []
  where
    go aliases ambient macros = do
      decl <- optional declarationParser
      case decl of
        Just (DeclAlias alias) -> go (alias : aliases) ambient macros
        Just (DeclAmbient amb) -> go aliases (amb : ambient) macros
        Just (DeclMacro macro) ->
          local
            (\env -> env{envMacros = Map.insert (macroName macro) macro (envMacros env)})
            (go aliases ambient (macro : macros))
        Nothing -> do
          expr <- optional (markCurrent expressionParser)
          pure
            Program
              { programAliases = reverse aliases,
                programAmbient = reverse ambient,
                programExpr = expr,
                programMacros = reverse macros
              }

data Declaration
  = DeclAlias TypeAlias
  | DeclAmbient AmbientDecl
  | DeclMacro MacroDef

-- | Parse a type alias, an ambient declaration, or a macro.
declarationParser :: Parser Declaration
declarationParser =
  try (DeclAlias <$> aliasParser)
    <|> (DeclAmbient <$> try ambientParser)
    <|> (DeclMacro <$> macroParser)

-- | Parse a top-level `type` alias declaration.
--
-- `opaque type Id t = String;` declares a nominal type, and a parameter may
-- carry a kind annotation: `type Fix (f :: Type -> Type) = ...;`.
aliasParser :: Parser TypeAlias
aliasParser = do
  opaque <- option False (True <$ reserved "opaque")
  reserved "type"
  name <- typeIdentifier
  params <- many param
  _ <- symbol "="
  body <- typeParser
  _ <- symbol ";"
  pure
    TypeAlias
      { typeAliasName = name,
        typeAliasParams = map fst params,
        typeAliasBody = body,
        typeAliasOpaque = opaque,
        typeAliasParamKinds = map snd params
      }
  where
    param =
      ((,Nothing) <$> typeIdentifier)
        <|> parens ((\p k -> (p, Just k)) <$> typeIdentifier <* symbol "::" <*> kindParser)

-- | Parse a `declare` block that describes an existing `.nix` module.
ambientParser :: Parser AmbientDecl
ambientParser = do
  reserved "declare"
  path <- pathLiteral <|> (Text.unpack <$> stringLiteral)
  entries <- braces (many ambientEntry)
  _ <- symbol ";"
  pure AmbientDecl{ambientPath = path, ambientEntries = entries}

-- | Parse a single ambiently-exported member.
ambientEntry :: Parser AmbientEntry
ambientEntry = do
  name <- attrName
  _ <- symbol "::"
  ty <- typeParser
  _ <- symbol ";"
  pure AmbientEntry{ambientEntryName = name, ambientEntryType = ty}

-- | Record the source offsets around a parser's result.
located :: Parser Expr -> Parser Expr
located parser = do
  start <- getOffset
  expr <- parser
  end <- getOffset
  pure $ case expr of
    ELoc{} -> expr
    _ -> ELoc (SrcSpan start end) expr

-- | Parse any expression form.
expressionParser :: Parser Expr
expressionParser = located (choice [ifParser, letParser, assertParser, withParser, lambdaParser, pipeParser])

-- | Parse a Nix-style conditional expression.
ifParser :: Parser Expr
ifParser = do
  reserved "if"
  cond <- expressionParser
  reserved "then"
  yesExpr <- expressionParser
  reserved "else"
  EIf cond yesExpr <$> expressionParser

-- | Parse a Nix-style `assert condition; body` expression.
assertParser :: Parser Expr
assertParser = do
  reserved "assert"
  cond <- expressionParser
  _ <- symbol ";"
  EAssert cond <$> expressionParser

-- | Parse a Nix-style `with scope; body` expression.
withParser :: Parser Expr
withParser = do
  reserved "with"
  scope <- expressionParser
  _ <- symbol ";"
  EWith scope <$> expressionParser

-- | Parse a `let ... in ...` block with optional type signatures.
letParser :: Parser Expr
letParser = do
  reserved "let"
  items <- many (markCurrent letItemParser)
  reserved "in"
  ELet items <$> expressionParser

-- | Parse one `let` item: an `inherit` clause, a type signature, or a value
-- binding whose left-hand side may be a nested attribute path.
--
-- Signatures and bindings both open with an attribute key, so the first key is
-- parsed once and the two tails are tried after it. The first key of a `let`
-- binding uses the strict identifier parser so that `in` terminates the block.
letItemParser :: Parser LetItem
letItemParser = inheritItem <|> keyed
  where
    inheritItem = do
      (source, names) <- inheritClause
      pure (LetInherit source names)
    keyed = do
      firstKey <- (SelectName <$> bindingIdentifier) <|> dynamicKey <|> stringKey
      case firstKey of
        SelectName name -> signatureFor name <|> bindingFor [firstKey]
        _ -> bindingFor [firstKey]
    signatureFor name = do
      _ <- symbol "::"
      ty <- typeParser
      _ <- symbol ";"
      pure (LetSignature name ty)
    bindingFor prefix = do
      rest <- many (symbol "." *> attrKey)
      _ <- symbol "="
      expr <- expressionParser
      _ <- symbol ";"
      pure $ case prefix <> rest of
        [SelectName name] -> LetBinding name expr
        steps -> LetPath steps expr

-- | Parse `inherit a b;` or `inherit (source) a b;`.
inheritClause :: Parser (Maybe Expr, [Name])
inheritClause = do
  reserved "inherit"
  source <- optional (parens expressionParser)
  names <- many attrName
  _ <- symbol ";"
  pure (source, names)

-- | Parse a lambda: a binder pattern followed by `:`.
--
-- Only the `pattern :` prefix is speculative. Once it is recognised the parser
-- commits to the lambda, so an error inside the body is reported where it
-- occurs instead of re-parsing the input as something else (which would also
-- make failures exponential in the nesting depth of lambdas).
lambdaParser :: Parser Expr
lambdaParser = do
  pattern' <- try (patternParser >>= \p -> p <$ lambdaColon p)
  ELambda pattern' <$> expressionParser
  where
    -- As in Nix, `name:rest` with no space is a URI (`x:x`,
    -- `github:NixOS/nixpkgs`), so a bare-name binder needs its colon to be
    -- followed by something that cannot continue a URI.
    lambdaColon = \case
      PVar _ Nothing -> lexeme (char ':' <* notFollowedBy (satisfy uriChar))
      _ -> lexeme (char ':' <* notFollowedBy (char ':'))
    uriChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("%/?:@&=+$,-_.!~*'" :: String)

-- | Parse the pipe operators (`|>` left-associative, `<|` right-associative),
-- which bind loosest of all operators. Mixing the two without parentheses is
-- an error in Nix, so each chain only accepts one direction.
pipeParser :: Parser Expr
pipeParser = do
  first <- implParser
  pipeRight first <|> pipeLeft first <|> pure first
  where
    pipeRight acc = do
      _ <- try (symbol "|>")
      next <- implParser
      let combined = binary OpPipeRight acc next
      pipeRight combined <|> pure combined
    pipeLeft acc = do
      _ <- try (symbol "<|")
      rest <- implParser
      more <- optional (pipeLeft rest)
      pure (binary OpPipeLeft acc (fromMaybe rest more))

-- | Logical implication (`->`), right-associative and looser than `||`.
implParser :: Parser Expr
implParser = chainRight1 orParser (binary OpImpl <$ try (symbol "->"))

-- | Operator precedence ladder, from loosest to tightest binding, mirroring
-- Nix: `||`, `&&`, equality, ordered comparisons, `//`, prefix `!`, `+`/`-`,
-- `*`/`/`, `++`, `?`, prefix `-`, application, selection.
orParser :: Parser Expr
orParser = chainLeft1 andParser (binary OpOr <$ symbol "||")

andParser :: Parser Expr
andParser = chainLeft1 equalityParser (binary OpAnd <$ symbol "&&")

equalityParser :: Parser Expr
equalityParser =
  chainLeft1
    relationalParser
    ((binary OpEq <$ symbol "==") <|> (binary OpNeq <$ symbol "!="))

relationalParser :: Parser Expr
relationalParser =
  chainLeft1
    updateParser
    ( choice
        [ binary OpLe <$ symbol "<=",
          binary OpGe <$ symbol ">=",
          binary OpLt <$ operator "<" "|",
          binary OpGt <$ symbol ">"
        ]
    )

-- | Parse right-associated attribute-set update (`//`).
updateParser :: Parser Expr
updateParser = chainRight1 notParser (binary OpUpdate <$ symbol "//")

-- | Parse prefix boolean negation, falling through to numeric addition.
notParser :: Parser Expr
notParser = located ((EUnaryOp OpNot <$> (operator "!" "=" *> notParser)) <|> additionParser)

-- | Parse left-associated additive arithmetic (`+`, `-`).
additionParser :: Parser Expr
additionParser =
  chainLeft1
    multiplicationParser
    ((binary OpAdd <$ operator "+" "+") <|> (binary OpSub <$ operator "-" ">"))

-- | Parse left-associated multiplicative arithmetic (`*`, `/`).
multiplicationParser :: Parser Expr
multiplicationParser =
  chainLeft1
    concatParser
    ((binary OpMul <$ symbol "*") <|> (binary OpDiv <$ operator "/" "/"))

-- | Parse right-associated list concatenation (`++`).
concatParser :: Parser Expr
concatParser = chainRight1 hasAttrParser (binary OpConcat <$ symbol "++")

-- | Parse the attribute-presence test (`e ? attrpath`).
hasAttrParser :: Parser Expr
hasAttrParser = located $ do
  base <- negationParser
  option base (EHasAttr base <$> (symbol "?" *> sepBy1 attrKey (symbol ".")))

-- | Parse prefix arithmetic negation. Negated numeric literals fold into
-- negative literals so singleton types such as `-1` survive checking.
negationParser :: Parser Expr
negationParser =
  located $
    (negateExpr <$> (operator "-" ">" *> negationParser)) <|> castParser
  where
    negateExpr operand =
      case stripLoc operand of
        EInt n -> EInt (negate n)
        EFloat n -> EFloat (negate n)
        _ -> EUnaryOp OpNeg operand
    stripLoc (ELoc _ inner) = stripLoc inner
    stripLoc other = other

-- | Parse TypeScript-style `expr as Type` chains.
castParser :: Parser Expr
castParser = located $ do
  base <- applicationParser
  casts <- many (reserved "as" *> typeParser)
  pure (foldl ECast base casts)

-- | Parse left-associated application chains.
applicationParser :: Parser Expr
applicationParser = located $ do
  head' <- postfixParser
  rest <- many postfixParser
  pure (foldl EApp head' rest)

-- | Parse an atom followed by field selections and an optional `or` default.
postfixParser :: Parser Expr
postfixParser = located $ do
  base <- atomParser
  steps <- many (try selectStepParser)
  if null steps
    then pure base
    else do
      fallback <- optional (reserved "or" *> postfixParser)
      pure (maybe (ESelect base steps) (ESelectOr base steps) fallback)

selectStepParser :: Parser SelectStep
selectStepParser = symbol "." *> attrKey

-- | Parse one attribute key: a bare name, a quoted (possibly interpolated)
-- string, or a `${expr}` antiquotation.
attrKey :: Parser SelectStep
attrKey = (SelectName <$> fieldName) <|> dynamicKey <|> stringKey

dynamicKey :: Parser SelectStep
dynamicKey = do
  _ <- try (symbol "${")
  stepExpr <- expressionParser
  _ <- symbol "}"
  pure (SelectDynamic stepExpr)

stringKey :: Parser SelectStep
stringKey = do
  expr <- lexeme doubleQuotedExpr
  pure $ case expr of
    EString literal -> SelectName (stringLiteralText literal)
    other -> SelectDynamic other

-- | Parse atomic expression forms.
atomParser :: Parser Expr
atomParser =
  located $
    choice
      [ macroInvocation,
        parens ascribed,
        recAttrSetParser,
        attrSetParser,
        listParser,
        stringExpr,
        pathExpr,
        ESearchPath <$> searchPathLiteral,
        EFloat <$> unsignedFloat,
        EInt <$> naturalLiteral,
        EBool True <$ reserved "true",
        EBool False <$ reserved "false",
        ENull <$ reserved "null",
        EString . DoubleQuoted <$> uriLiteral,
        EVar <$> identifier,
        EVar <$> asVariable
      ]

-- | A parenthesized expression, optionally ascribed a type: `(e :: T)`.
ascribed :: Parser Expr
ascribed = do
  expr <- expressionParser
  maybe expr (EAscribe expr) <$> optional (symbol "::" *> typeParser)

-- * Macros

-- | Maximum nesting of macro expansions (a recursive macro that never stops).
expansionLimit :: Int
expansionLimit = 64

-- | Parse `macro name { (pattern) => (template); ... };`.
macroParser :: Parser MacroDef
macroParser = do
  start <- getOffset
  name <- try (reserved "macro" *> identifier <* lookAhead (symbol "{"))
  rawRules <- braces (many ruleParser)
  _ <- symbol ";"
  end <- getOffset
  when (null rawRules) $ failAt start (withCode TX0002InvalidMacroPattern ("macro `" <> Text.unpack name <> "` has no rules"))
  let defSpan = SrcSpan start end
      draft = MacroDef{macroName = name, macroRules = [], macroSpan = defSpan}
  -- The template is checked with the macro itself in scope, so a rule may
  -- invoke its own macro (nested invocations stay unexpanded while checking).
  rules <-
    local (\env -> env{envMacros = Map.insert name draft (envMacros env)}) $
      traverse (\(pattern', template, offset) -> MacroRule pattern' template offset <$> checkInstance defSpan pattern' template offset) rawRules
  pure draft{macroRules = rules}
  where
    ruleParser = do
      pattern' <- symbol "(" *> many patternToken <* symbol ")"
      _ <- symbol "=>"
      (template, offset) <- templateGroup
      _ <- symbol ";"
      pure (pattern', template, offset)

-- | The definition-time instance of a rule: its template with every
-- metavariable symbolic, closed into a lambda for the checker.
checkInstance :: SrcSpan -> [MacroToken] -> Text.Text -> Int -> Parser Expr
checkInstance defSpan pattern' template offset = do
  (text, leaves) <- either (failAt offset) pure (instantiateTemplate (symbolicBindings pattern') template)
  env <- ask
  instance' <- subParse env{envCheckingTemplate = True} text
  hygienic <- either (failAt offset) pure (hygienize "'def" (relocate defSpan instance'))
  closeTemplate leaves <$> either (failAt offset) pure (substitute leaves hygienic)

-- | One pattern element.
patternToken :: Parser MacroToken
patternToken =
  choice
    [ repetition,
      metavariable,
      group "(" ")",
      group "[" "]",
      group "{" "}",
      MLiteral <$> lexeme (Text.pack <$> some (satisfy wordChar)),
      MLiteral . Text.singleton <$> lexeme (oneOf (",;" :: String)),
      MLiteral . Text.pack <$> lexeme (some (oneOf ("=<>-+*/!&|:.?@%^~" :: String)))
    ]
  where
    wordChar c = isAlphaNum c || c `elem` ("_'" :: String)
    group open close = do
      _ <- symbol open
      items <- many patternToken
      _ <- symbol close
      pure (MGroup open items close)
    repetition = do
      _ <- try (string "$(") <* sc
      items <- many patternToken
      _ <- symbol ")"
      separator <- optional (try (lexeme (oneOf (",;" :: String)) <* lookAhead (oneOf ("*+?" :: String))))
      kind <- lexeme ((ZeroOrMore <$ char '*') <|> (OneOrMore <$ char '+') <|> (ZeroOrOne <$ char '?'))
      pure (MRepeat items (Text.singleton <$> separator) kind)
    metavariable = do
      start <- getOffset
      name <- try (lookAhead (char '$') *> identifier)
      fragment <-
        (FragExpr . Just <$> (symbol "::" *> typeParser))
          <|> (char ':' *> fragmentKind start)
      pure (MVar (Text.drop 1 name) fragment)
    fragmentKind start = do
      kind <- lexeme (some (satisfy isLetter))
      case kind of
        "expr" -> pure (FragExpr Nothing)
        "ident" -> pure FragIdent
        "type" -> pure FragType
        "string" -> pure FragString
        other -> failAt start (withCode TX0002InvalidMacroPattern ("unknown fragment `" <> other <> "`: use expr, ident, type, or string (or `$x :: Type` for a typed expression)"))

-- | A parenthesized template, captured as raw text (with its offset) so its
-- repetitions can be expanded before it is parsed.
templateGroup :: Parser (Text.Text, Int)
templateGroup = do
  _ <- char '('
  offset <- getOffset
  body <- balancedText
  _ <- char ')'
  sc
  pure (Text.pack body, offset)

-- | Text up to the next unmatched closing bracket, skipping over strings.
balancedText :: Parser String
balancedText = concat <$> many piece
  where
    piece =
      choice
        [ quoted,
          indented,
          nested '(' ')',
          nested '[' ']',
          nested '{' '}',
          pure <$> satisfy (`notElem` ("()[]{}\"" :: String))
        ]
    nested open close = do
      _ <- char open
      inner <- balancedText
      _ <- char close
      pure (open : inner <> [close])
    quoted = do
      _ <- char '"'
      body <- many ((\a b -> [a, b]) <$> char '\\' <*> anySingle <|> pure <$> satisfy (/= '"'))
      _ <- char '"'
      pure ('"' : concat body <> "\"")
    indented = do
      _ <- try (string "''")
      body <- manyTill anySingle (try (string "''" <* notFollowedBy (oneOf ("'$\\" :: String))))
      pure ("''" <> body <> "''")

-- | Parse a macro invocation `name!( ... )`, expanding it in place.
--
-- Only a declared macro (or the built-in `stringify!`) is an invocation, and
-- only when `!(` follows the name immediately; anything else keeps its Nix
-- meaning (`f !(x)` is an application of `f` to a negation).
macroInvocation :: Parser Expr
macroInvocation = do
  start <- getOffset
  macros <- asks envMacros
  name <-
    try $ do
      raw <- Text.pack <$> some (satisfy (\c -> isAlphaNum c || c `elem` ("_'-" :: String)))
      _ <- char '!'
      _ <- lookAhead (char '(')
      if raw == "stringify" || Map.member raw macros then pure raw else fail "not a macro"
  _ <- symbol "("
  if name == "stringify"
    then stringifyInvocation
    else do
      checking <- asks envCheckingTemplate
      if checking
        then do
          _ <- balancedText
          _ <- symbol ")"
          pure (EVar "$nested")
        else case Map.lookup name macros of
          Nothing -> fail (withCode TX0001NoMatchingRule ("unknown macro `" <> Text.unpack name <> "`"))
          Just macro -> expandInvocation start macro

stringifyInvocation :: Parser Expr
stringifyInvocation = do
  arg <- expressionParser
  _ <- symbol ")"
  pure $ case stripLocations arg of
    EVar name | not (isPlaceholder name) -> EString (DoubleQuoted name)
    _ -> EApp (EVar "$stringify") arg

-- | Match the invocation against each rule in order and expand the first
-- that matches the whole argument list.
expandInvocation :: Int -> MacroDef -> Parser Expr
expandInvocation start macro = do
  depth <- asks envExpansionDepth
  when (depth >= expansionLimit) $
    fail (withCode TX0005ExpansionLimit ("macro `" <> Text.unpack (macroName macro) <> "` expanded more than " <> show expansionLimit <> " levels deep; is it recursive without a base case?"))
  matched <- optional (choice [try ((,) rule <$> matchTokens (rulePattern rule) <* symbol ")") | rule <- macroRules macro])
  case matched of
    Nothing ->
      fail (withCode TX0001NoMatchingRule ("no rule of macro `" <> Text.unpack (macroName macro) <> "` matches this invocation"))
    Just (rule, bindings) -> do
      end <- getOffset
      let invocationSpan = SrcSpan start end
          suffix = "'" <> Text.pack (show (depth + 1)) <> "_" <> Text.pack (show start)
      (text, leaves) <- either fail pure (instantiateTemplate bindings (ruleTemplate rule))
      env <- ask
      instance' <- subParse env{envExpansionDepth = depth + 1} text
      hygienic <- either fail pure (hygienize suffix (relocate invocationSpan instance'))
      expanded <- either fail pure (substitute leaves hygienic)
      pure (ELoc invocationSpan expanded)

-- | Parse a template instance with the ordinary expression grammar.
subParse :: ParseEnv -> Text.Text -> Parser Expr
subParse env text =
  case runParser (runReaderT (sc *> expressionParser <* eof) env) "macro" text of
    Right expr -> pure expr
    Left bundle ->
      let first :| _ = bundleErrors bundle
          message = Text.unpack (Text.strip (Text.pack (parseErrorTextPretty first)))
       in fail $
            if "[TX" `Text.isPrefixOf` Text.pack message
              then message
              else withCode TX0004InvalidTemplate ("macro expansion does not parse: " <> message)

-- | Match pattern tokens against invocation input.
matchTokens :: [MacroToken] -> Parser (Map Name Binding)
matchTokens items = Map.unions <$> traverse matchToken items
  where
    matchToken = \case
      MLiteral literal
        | Text.all (\c -> isAlphaNum c || c `elem` ("_'" :: String)) literal ->
            Map.empty <$ lexeme (try (string literal <* notFollowedBy (satisfy (\c -> isAlphaNum c || c `elem` ("_'-" :: String)))))
        | otherwise -> Map.empty <$ try (symbol literal)
      MVar name fragment -> Map.singleton name . Single <$> matchFragment fragment
      MGroup open inner close -> symbol open *> matchTokens inner <* symbol close
      MRepeat inner separator kind -> do
        let element = try (matchTokens inner)
            next = maybe element (\sep -> try (symbol sep *> matchTokens inner)) separator
        results <- case kind of
          ZeroOrOne -> maybeToList <$> optional element
          ZeroOrMore -> optional element >>= maybe (pure []) (\first -> (first :) <$> many next)
          OneOrMore -> element >>= \first -> (first :) <$> many next
        pure (Map.fromList [(name, Repeated [binding | result <- results, Just binding <- [Map.lookup name result]]) | (name, _, _) <- tokenVars inner])
    matchFragment = \case
      FragExpr ty -> (`LExpr` ty) <$> expressionParser
      FragIdent -> LIdent <$> bindingIdentifier
      FragType -> LType <$> typeParser
      FragString -> LStr <$> stringLiteral

-- | Fail with a message reported at the given offset (used for errors in a
-- macro definition; invocation errors are reported where parsing stopped, so
-- they win over the errors of alternatives that backtracked earlier).
failAt :: Int -> String -> Parser a
failAt offset message = setOffset offset *> fail message

-- | Parse an attribute set.
attrSetParser :: Parser Expr
attrSetParser = EAttrSet <$> braces (many attrParser)

-- | Parse a recursive attribute set (`rec { ... }`).
recAttrSetParser :: Parser Expr
recAttrSetParser = reserved "rec" *> (ERec <$> braces (many attrParser))

-- | Parse a path literal, possibly containing `${...}` antiquotations.
--
-- Supported prefixes are `./`, `../`, `/`, and `~/`. The first character after
-- the prefix must be a non-slash segment character (or an antiquotation), so
-- `//` and a bare `/` stay operators.
pathExpr :: Parser Expr
pathExpr = lexeme $ try $ do
  prefix <- choice [string "../", string "./", string "~/", string "/"]
  first <- pathSegment True
  rest <- many (pathSegment False)
  let parts = mergeText (StrText prefix : first : rest)
  pure $ case parts of
    [StrText whole] -> EPath (Text.unpack whole)
    _ -> EPathInterp parts
  where
    pathSegment isFirst =
      interpPart
        <|> ( StrText . Text.pack
                <$> ( if isFirst
                        then (:) <$> satisfy segmentStart <*> many (satisfy pathChar)
                        else some (satisfy pathChar <|> try (char '/' <* lookAhead (satisfy segmentStart <|> char '$')))
                    )
            )
    pathChar c = c `elem` ("._-+" :: String) || isPathAlnum c
    segmentStart = pathChar
    isPathAlnum c = isAsciiLower c || isAsciiUpper c || isDigit c
    mergeText = foldr merge []
    merge (StrText a) (StrText b : rest) = StrText (a <> b) : rest
    merge part rest = part : rest

-- | Parse a string literal, producing an interpolated string when it contains
-- `${...}` antiquotations and a plain 'EString' otherwise. Both double-quoted
-- and indented `'' ... ''` forms are supported.
stringExpr :: Parser Expr
stringExpr = lexeme (doubleQuotedExpr <|> indentedExpr)

doubleQuotedExpr :: Parser Expr
doubleQuotedExpr =
  mkStringExpr InterpDouble
    <$> (char '"' *> many (interpPart <|> doubleTextPart) <* char '"')

indentedExpr :: Parser Expr
indentedExpr =
  mkStringExpr InterpIndented
    <$> (string "''" *> many (interpPart <|> indentedEscape <|> indentedTextPart) <* string "''")

-- | An `''\n`, `''\r`, or `''\t` escape, kept verbatim (see 'StrEscape').
-- Any other `''\c` stands for the character itself and is read as text.
indentedEscape :: Parser StringPart
indentedEscape = StrEscape <$> try (string "''\\" *> oneOf ("nrt" :: String))

-- | An antiquoted `${ expr }` segment shared by strings and paths.
interpPart :: Parser StringPart
interpPart = StrExpr <$> (try (string "${") *> sc *> expressionParser <* char '}')

doubleTextPart :: Parser StringPart
doubleTextPart = StrText . Text.concat <$> some ((try (string "$$") $> "$$") <|> (Text.singleton <$> doubleTextChar))

-- | A literal character inside a double-quoted string. Nix recognises `\n`,
-- `\r`, and `\t`; a backslash before any other character yields that
-- character literally. A `$` that does not open an antiquotation is literal.
doubleTextChar :: Parser Char
doubleTextChar =
  (char '\\' *> (decode <$> anySingle))
    <|> (notFollowedBy (string "${") *> satisfy (\c -> c /= '"' && c /= '\\'))
  where
    decode 'n' = '\n'
    decode 'r' = '\r'
    decode 't' = '\t'
    decode other = other

indentedTextPart :: Parser StringPart
indentedTextPart = StrText . Text.concat <$> some indentedChunk

-- | A literal chunk inside an indented string.
--
-- The escapes mirror Nix: `''${` and `''$` produce a literal dollar, `'''`
-- produces a literal `''`, and `''\\` takes the following character literally
-- (with the usual `n`/`r`/`t` spellings). Every other character is preserved
-- verbatim.
indentedChunk :: Parser Text.Text
indentedChunk =
  (try (string "$$") $> "$$")
    <|> (try (string "''${") $> "${")
    <|> (try (string "''$") $> "$")
    <|> (try (string "'''") $> "''")
    <|> (Text.singleton <$> (try (string "''\\" <* notFollowedBy (oneOf ("nrt" :: String))) *> anySingle))
    <|> (Text.singleton <$> (notFollowedBy (string "${") *> notFollowedBy (string "''") *> anySingle))

-- | Collapse parsed segments into a plain 'EString' when there is no
-- interpolation; otherwise keep the interpolated representation.
mkStringExpr :: InterpForm -> [StringPart] -> Expr
mkStringExpr form parts
  | all isText parts = EString (literalFor form (Text.concat [t | StrText t <- parts]))
  | otherwise = EInterp form parts
  where
    isText StrText{} = True
    isText _ = False
    literalFor InterpDouble = DoubleQuoted
    literalFor InterpIndented = Indented

-- | Parse either an attribute binding or an `inherit` clause.
attrParser :: Parser AttrItem
attrParser = inheritItem <|> fieldParser
  where
    inheritItem = do
      (source, names) <- inheritClause
      pure (maybe (AttrInherit names) (`AttrInheritFrom` names) source)
    fieldParser = do
      path <- sepBy1 attrKey (symbol ".")
      _ <- symbol "="
      expr <- expressionParser
      _ <- symbol ";"
      pure $ case path of
        [SelectName name] -> AttrField name expr
        steps -> AttrPath steps expr

-- | Parse a list literal. Elements are selection-level expressions, as in Nix;
-- tynix additionally accepts `as` casts and a few compound forms that would
-- otherwise need parentheses.
listParser :: Parser Expr
listParser = EList <$> brackets (many listItem)
  where
    listItem = located (choice [ifParser, letParser, lambdaParser, listCastParser])
    listCastParser = do
      base <- postfixParser
      casts <- many (reserved "as" *> typeParser)
      pure (foldl ECast base casts)

-- | Parse a lambda binder pattern.
--
-- Supported forms: `x`, `(x :: T)`, `{ a, b ? d, ... }`, `{ ... }@args`, and
-- `args@{ ... }`. Attribute-set fields may carry an erased annotation:
-- `{ name :: String, version ? "1" }`.
patternParser :: Parser Pattern
patternParser =
  choice
    [ try (parens typed),
      try binderFirst,
      attrSetPattern Nothing,
      PVar <$> bindingIdentifier <*> pure Nothing
    ]
  where
    typed = do
      name <- bindingIdentifier
      _ <- symbol "::"
      PVar name . Just <$> typeParser
    binderFirst = do
      name <- bindingIdentifier
      _ <- symbol "@"
      attrSetPattern (Just (BinderBefore name))
    attrSetPattern before = do
      (fields, open) <- braces patternBody
      after <-
        case before of
          Just _ -> pure Nothing
          Nothing -> optional (BinderAfter <$> (symbol "@" *> bindingIdentifier))
      pure (PAttrSet fields open (before <|> after))
    patternBody = do
      items <- sepEndBy patternItem (symbol ",")
      pure (lefts items, any isRightItem items)
    patternItem = (Right () <$ symbol "...") <|> (Left <$> patternField)
    patternField = do
      name <- bindingIdentifier
      annotation <- optional (symbol "::" *> typeParser)
      fallback <- optional (symbol "?" *> expressionParser)
      pure PatternField{patternFieldName = name, patternFieldType = annotation, patternFieldDefault = fallback}
    isRightItem = isJust . either (const Nothing) Just

markCurrent :: Parser a -> Parser (Marked a)
markCurrent parser = Marked <$> directiveForCurrentLine <*> parser

-- | A single-character operator that must not be the prefix of a longer one
-- (e.g. `-` vs `->`, `/` vs `//`, `<` vs `<|`).
operator :: Text.Text -> String -> Parser ()
operator op forbiddenNext =
  void (lexeme (try (string op <* notFollowedBy (satisfy (`elem` forbiddenNext)))))

binary :: BinOp -> Expr -> Expr -> Expr
binary op left right =
  case (left, right) of
    (ELoc (SrcSpan start _) _, ELoc (SrcSpan _ end) _) -> ELoc (SrcSpan start end) (EBinaryOp op left right)
    _ -> EBinaryOp op left right

chainLeft1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainLeft1 item op = do
  first <- item
  rest first
  where
    rest acc =
      ( do
          f <- op
          next <- item
          rest (f acc next)
      )
        <|> pure acc

chainRight1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainRight1 item op = do
  first <- item
  ( do
      f <- op
      rest <- chainRight1 item op
      pure (f first rest)
    )
    <|> pure first
