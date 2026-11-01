{-# LANGUAGE OverloadedStrings #-}

-- | Shared lexical layer for tynix parsers.
--
-- The grammar stays intentionally close to Nix, so the lexer mostly handles
-- whitespace, comments, simple identifiers, and path literals instead of
-- inventing a heavy token stream.
module ParserLexer
  ( DirectiveTargets,
    ParseEnv (..),
    Parser,
    isPlaceholder,
    attrName,
    brackets,
    directiveForCurrentLine,
    braces,
    float,
    fieldName,
    identifier,
    bindingIdentifier,
    asVariable,
    indentedStringLiteral,
    integer,
    naturalLiteral,
    searchPathLiteral,
    typeIdentifier,
    unsignedFloat,
    uriLiteral,
    lexeme,
    parens,
    pathLiteral,
    reserved,
    sc,
    stringLiteral,
    termStringLiteral,
    symbol,
  )
where

import Control.Monad (void, when)
import Control.Monad.Reader (ReaderT, asks)
import Data.Char (isAlphaNum, isLetter)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Syntax (DiagnosticDirective)
import Syntax qualified
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Read (readMaybe)
import Type (Name)

-- | Parser type used throughout the frontend.
type DirectiveTargets = Map Int DiagnosticDirective

-- | Read-only parsing context: directive comments, the macros in scope (so
-- invocations can be expanded as they are parsed), and how deeply macro
-- expansions are nested.
data ParseEnv = ParseEnv
  { envDirectives :: DirectiveTargets,
    envMacros :: Map Name Syntax.MacroDef,
    envExpansionDepth :: Int,
    -- | Parsing a template for its definition-time check: nested macro
    -- invocations are left unexpanded (they are checked at their own
    -- definitions), which also keeps recursive macros finite.
    envCheckingTemplate :: Bool
  }

instance Semigroup ParseEnv where
  ParseEnv a b c d <> ParseEnv a' b' c' d' = ParseEnv (a <> a') (b <> b') (max c c') (d || d')

instance Monoid ParseEnv where
  mempty = ParseEnv Map.empty Map.empty 0 False

type Parser = ReaderT ParseEnv (Parsec Void Text)

-- | Look up whether the current source line is targeted by a directive comment.
directiveForCurrentLine :: Parser (Maybe DiagnosticDirective)
directiveForCurrentLine = do
  lineNo <- unPos . sourceLine <$> getSourcePos
  asks (Map.lookup lineNo . envDirectives)

-- | Macro metavariables are spelled `$name`. They are accepted wherever an
-- identifier is, so templates parse with the ordinary grammar; outside a
-- template they are rejected after parsing.
isPlaceholder :: Name -> Bool
isPlaceholder name =
  case Text.uncons name of
    Just ('$', rest) -> maybe False (identStart . fst) (Text.uncons rest)
    _ -> False

-- | Space consumer matching Nix-style comments.
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

-- | Attach trailing space consumption to a parser.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a fixed symbol and consume following layout.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parse a reserved keyword while ensuring it is not a longer identifier
-- prefix.
reserved :: Text -> Parser ()
reserved word = lexeme $ try $ string word *> notFollowedBy (satisfy identCont)

-- | Parse a term-level identifier.
--
-- Only genuine Nix keywords (plus tynix's `as` cast) are reserved here, so
-- ordinary Nix code that binds names such as `type`, `any`, or `declare` keeps
-- parsing. Type-only keywords are reserved by 'typeIdentifier' instead.
identifier :: Parser Name
identifier = identifierExcluding termReservedWords

-- | Parse a name in a binding position (lambda binders, pattern fields, `let`
-- keys). Here `as` is an ordinary name, as in Nix: `as: as.x` is common.
bindingIdentifier :: Parser Name
bindingIdentifier = identifierExcluding (filter (/= "as") termReservedWords)

-- | Parse a reference to a variable literally named `as`.
--
-- In expression position `as` usually starts a cast (`e as T`), so it is only
-- read as a variable when what follows cannot begin a type: a selection, a
-- delimiter, or an operator.
asVariable :: Parser Name
asVariable = lexeme . try $ do
  _ <- string "as"
  notFollowedBy (satisfy identCont)
  sc
  _ <- lookAhead terminator
  pure "as"
  where
    terminator =
      choice
        [ void (oneOf (".;,)]}=+*/<>!&|?:" :: String)),
          void (string "++"),
          void (string "-"),
          eof,
          void (choice (map (\w -> string w <* notFollowedBy (satisfy identCont)) ["in", "then", "else", "or"]))
        ]

-- | Parse a type-level identifier, where tynix's type keywords are reserved.
typeIdentifier :: Parser Name
identifierExcluding :: [Text] -> Parser Name
typeIdentifier = identifierExcluding reservedWords

identifierExcluding excluded = lexeme $ try $ do
  sigil <- option "" ("$" <$ try (char '$' <* lookAhead (satisfy identStart)))
  first <- satisfy identStart
  rest <- many (satisfy identCont)
  let name = Text.pack (sigil <> (first : rest))
  when (name `elem` excluded) (fail ("reserved word " <> show name))
  pure name

-- | Parse an identifier-shaped field or selector name.
--
-- Attribute names in Nix frequently overlap with keywords (`any`, `or`,
-- `inherit`, ...). Binding positions stay strict through 'identifier', while
-- record fields and dotted selections use this parser so declaration packs can
-- mirror real upstream APIs.
fieldName :: Parser Name
fieldName = lexeme $ do
  sigil <- option "" ("$" <$ try (char '$' <* lookAhead (satisfy identStart)))
  first <- satisfy identStart
  rest <- many (satisfy identCont)
  pure (Text.pack (sigil <> (first : rest)))

-- | Parse a field, selector, or declaration entry name.
--
-- tynix keeps quoted attribute names as ordinary textual field keys, so callers
-- that do not care about the original quoting can consume both forms through
-- this helper.
attrName :: Parser Name
attrName = fieldName <|> stringLiteral

-- | Parse a double-quoted string literal.
stringLiteral :: Parser Text
stringLiteral = lexeme $ Text.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

-- | Parse a raw Nix indented string literal.
--
-- This intentionally preserves the inner text verbatim so later phases can
-- round-trip shell hooks and install phases without first understanding every
-- string escape.
indentedStringLiteral :: Parser Text
indentedStringLiteral = lexeme $ Text.pack <$> (try (string "''") *> manyTill anySingle (try (string "''")))

-- | Parse any executable string literal form supported by tynix.
termStringLiteral :: Parser Syntax.StringLiteral
termStringLiteral =
  (Syntax.DoubleQuoted <$> stringLiteral)
    <|> (Syntax.Indented <$> indentedStringLiteral)

-- | Parse a decimal integer literal.
integer :: Parser Integer
integer = lexeme . try $ do
  sign <- optional (char '-')
  value <- L.decimal
  pure $
    case sign of
      Just _ -> negate value
      Nothing -> value

-- | Parse an unsigned decimal integer. Expressions use this so that `n -1`
-- stays a subtraction; negation is a separate prefix operator.
naturalLiteral :: Parser Integer
naturalLiteral = lexeme . try $ L.decimal <* notFollowedBy (satisfy identCont)

-- | Parse an unsigned float literal (`1.5`, `1.5e3`, `.5`).
unsignedFloat :: Parser Double
unsignedFloat = lexeme . try $ do
  whole <- many digitChar
  _ <- char '.'
  frac <- some digitChar
  expo <- optional exponentPart
  let rendered = (if null whole then "0" else whole) <> "." <> frac <> fromMaybe "" expo
  maybe (fail ("invalid float literal: " <> rendered)) pure (readMaybe rendered)
  where
    exponentPart = do
      marker <- oneOf ("eE" :: String)
      sign <- optional (oneOf ("+-" :: String))
      digits <- some digitChar
      pure (marker : maybe digits (: digits) sign)

-- | Parse a `<nixpkgs>` / `<nixpkgs/lib>` lookup path, returning the inner text.
searchPathLiteral :: Parser FilePath
searchPathLiteral = lexeme $ try $ do
  _ <- char '<'
  first <- some (satisfy searchChar)
  rest <- many ((:) <$> char '/' <*> some (satisfy searchChar))
  _ <- char '>'
  pure (first <> concat rest)
  where
    searchChar c = isAlphaNum c || c `elem` ("._-+" :: String)

-- | Parse an unquoted URI literal such as `https://example.org/x.tar.gz`.
-- Nix treats these as strings; the scheme needs at least two characters so
-- that ordinary `x:x`-style lambdas are not swallowed.
uriLiteral :: Parser Text
uriLiteral = lexeme $ try $ do
  first <- satisfy isLetter
  schemeRest <- many (satisfy (\c -> isAlphaNum c || c `elem` ("+-." :: String)))
  _ <- char ':'
  body <- some (satisfy uriChar)
  pure (Text.pack (first : schemeRest <> ":" <> body))
  where
    uriChar c = isAlphaNum c || c `elem` ("%/?:@&=+$,-_.!~*'" :: String)

-- | Parse a decimal float literal with a required fractional part.
float :: Parser Double
float = lexeme . try $ do
  sign <- optional (char '-')
  whole <- some digitChar
  _ <- char '.'
  frac <- some digitChar
  expo <- optional exponentPart
  let rendered = maybe "" pure sign <> whole <> "." <> frac <> fromMaybe "" expo
  maybe (fail ("invalid float literal: " <> rendered)) pure (readMaybe rendered)
  where
    exponentPart = do
      marker <- oneOf ("eE" :: String)
      sign <- optional (oneOf ("+-" :: String))
      digits <- some digitChar
      pure (marker : maybe digits (: digits) sign)

-- | Parse a Nix path literal such as `./foo.nix` or `/etc/hosts`.
--
-- The first character after the prefix must be a non-slash segment character.
-- This keeps `//` (the attribute-set update operator) and a bare `/` from
-- being lexed as paths, matching Nix where a path always has a segment.
pathLiteral :: Parser FilePath
pathLiteral = lexeme $ try $ do
  prefix <- Text.unpack <$> choice [string "../", string "./", string "/"]
  firstChar <- satisfy pathSegmentStart
  rest <- many (satisfy pathChar)
  pure (prefix <> (firstChar : rest))

-- | Standard bracket helpers used by both term and type parsers.
parens, braces, brackets :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
brackets = between (symbol "[") (symbol "]")

termReservedWords :: [Text]
termReservedWords =
  ["as", "assert", "else", "false", "if", "in", "inherit", "let", "null", "or", "rec", "then", "true", "with"]

reservedWords :: [Text]
reservedWords =
  ["Tuple", "any", "as", "assert", "declare", "dynamic", "else", "extends", "false", "forall", "if", "import", "in", "infer", "inherit", "let", "null", "rec", "then", "true", "type", "unknown", "with"]

identStart, identCont, pathChar, pathSegmentStart :: Char -> Bool
identStart c = isLetter c || c == '_'
identCont c = isAlphaNum c || c `elem` ("_'-" :: String)
pathChar c = isAlphaNum c || c `elem` ("._/-+" :: String)
pathSegmentStart c = pathChar c && c /= '/'
