{-# LANGUAGE OverloadedStrings #-}

-- | Parser for tynix type syntax.
--
-- The syntax combines Haskell-like binders (`forall`) with TypeScript-inspired
-- features (`extends`, `infer`) while keeping the visual shape light enough to
-- sit next to ordinary Nix code.
module ParserType (kindParser, typeParser) where

import Control.Monad (void)
import Data.Char (isUpper)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import ParserLexer
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)
import Type

-- | Entry point for type parsing.
typeParser :: Parser Type
typeParser = forallParser <|> (optional constraintContext *> conditionalParser)

-- | Parse explicit universal quantification.
forallParser :: Parser Type
forallParser = try $ do
  reserved "forall"
  vars <- some typeIdentifier
  _ <- symbol "."
  TForall vars <$> typeParser

-- | Parse a Haskell-style constraint context such as `Functor f =>` or
-- `(Eq a, Show a) =>`.
--
-- Constraints are accepted so signatures can document the capabilities they
-- rely on, but they are not enforced yet: tynix has no type classes, so the
-- context is dropped after parsing.
constraintContext :: Parser [Type]
constraintContext = try $ do
  constraints <- try (parens (sepBy1 appParser (symbol ","))) <|> (pure <$> appParser)
  _ <- symbol "=>"
  pure constraints

-- | Parse conditional types of the form `A extends B ? C : D`.
conditionalParser :: Parser Type
conditionalParser = do
  lhs <- functionParser
  option lhs $ do
    reserved "extends"
    rhs <- functionParser
    _ <- symbol "?"
    yesTy <- typeParser
    _ <- symbol ":"
    TConditional lhs rhs yesTy <$> typeParser

-- | Parse right-associative function arrows.
--
-- An arrow may carry, besides `%1` linearity:
--
-- * a dependent binder, `(n :: Nat) -> Vec n a`, naming the argument in the
--   codomain;
-- * a capture set written against the arrow, `A ->{fetch} B`;
-- * a latent effect row after the codomain, `A -> B ! { Trace }`. The row
--   belongs to the innermost arrow it follows, so in `a -> b -> c ! { E }`
--   only the full application performs `E`.
functionParser :: Parser Type
functionParser = fst <$> functionChain

-- | Parse a function type, reporting whether the result is an unparenthesized
-- arrow (whose own effect suffix has then already been parsed).
functionChain :: Parser (Type, Bool)
functionChain = do
  binder <- optional (try dependentBinder)
  case binder of
    Just (name, domain) -> (,True) <$> arrowTail (Just name) domain
    Nothing -> do
      lhs <- unionParser
      option (lhs, False) ((,True) <$> arrowTail Nothing lhs)
  where
    dependentBinder = do
      (name, domain) <- parens ((,) <$> typeIdentifier <* symbol "::" <*> typeParser)
      _ <- lookAhead (void (try (symbol "%1")) <|> void (string "->"))
      pure (name, domain)
    arrowTail binder domain = do
      (mult, captures) <- arrowParser
      (codomain, nested) <- functionChain
      effects <- if nested then pure Nothing else optional (try effectSuffix)
      pure
        ( TArrow
            Arrow
              { arrowMult = mult,
                arrowEffects = fromMaybe TDynamic effects,
                arrowCaptures = captures,
                arrowBinder = binder
              }
            domain
            codomain
        )

-- | Parse `->`, `%1 ->`, and an optional capture set glued to the arrow
-- (`->{a, b}`; `->{}` is a closure that captures nothing tracked).
arrowParser :: Parser (Multiplicity, Maybe [Name])
arrowParser = do
  mult <- option Many (One <$ try (symbol "%1" <* lookAhead (string "->")))
  captures <- lexeme $ do
    _ <- string "->"
    optional (try captureSet)
  pure (mult, captures)
  where
    captureSet = char '{' *> sc *> sepBy identifier (symbol ",") <* char '}'

-- | Parse an effect suffix: `! { Trace, Throw }`, `! { Trace | e }`, `! e`,
-- or `! {}` for a pure function.
effectSuffix :: Parser Type
effectSuffix = do
  _ <- lexeme (char '!' <* notFollowedBy (char '='))
  braces row <|> (TVar <$> typeIdentifier)
  where
    row = do
      labels <- sepBy typeIdentifier (symbol ",")
      tail' <- optional (symbol "|" *> typeIdentifier)
      pure (effectRow labels (TVar <$> tail'))

-- | Parse a kind: `Type`, `*`, or arrows between kinds.
kindParser :: Parser Kind
kindParser = do
  lhs <- atom
  option lhs (KFun lhs <$> (symbol "->" *> kindParser))
  where
    atom = (KType <$ (reserved "Type" <|> void (symbol "*"))) <|> parens kindParser

-- | Parse normalized unions.
unionParser :: Parser Type
unionParser = mkUnion <$> sepBy1 appParser (symbol "|")
  where
    mkUnion [oneTy] = oneTy
    mkUnion manyTypes = TUnion manyTypes

-- | Parse left-associated type applications.
appParser :: Parser Type
appParser = do
  head' <- atomParser
  rest <- many atomParser
  pure (foldl TApp head' rest)

-- | Parse atomic type forms.
atomParser :: Parser Type
atomParser =
  choice
    [ typeListParser,
      recordParser,
      TLit . LString <$> stringLiteral,
      TLit . LFloat <$> float,
      TLit . LInt <$> integer,
      TLit (LBool True) <$ reserved "true",
      TLit (LBool False) <$ reserved "false",
      TAny <$ reserved "any",
      TDynamic <$ reserved "dynamic",
      TUnknown <$ reserved "unknown",
      inferParser,
      parens typeParser,
      TCon "Tuple" <$ reserved "Tuple",
      varOrConParser
    ]

-- | Parse structural record types.
--
-- Fields are `name :: T;`, optional fields `name? :: T;`. A trailing `...`
-- (optionally named, `...r`) makes the record open: it may hold further
-- fields of unknown type.
recordParser :: Parser Type
recordParser = braces $ do
  fields <- many fieldParser
  rowTail <- optional (symbol "..." *> optional typeIdentifier <* optional (symbol ";"))
  pure $ case rowTail of
    Nothing -> TRecord (Map.fromList fields)
    Just Nothing -> TOpenRecord (Map.fromList fields) TDynamic
    Just (Just name) -> TOpenRecord (Map.fromList fields) (TVar name)
  where
    fieldParser = do
      name <- attrName
      isOptional <- option False (True <$ symbol "?")
      _ <- symbol "::"
      ty <- typeParser
      _ <- symbol ";"
      pure (name, if isOptional then TOptional ty else ty)

-- | Parse type-level shape lists such as `[2 3 4]`.
typeListParser :: Parser Type
typeListParser = TTypeList <$> brackets (many shapeItemParser)

shapeItemParser :: Parser Type
shapeItemParser =
  choice
    [ TLit . LString <$> stringLiteral,
      TLit . LFloat <$> float,
      TLit . LInt <$> integer,
      TLit (LBool True) <$ reserved "true",
      TLit (LBool False) <$ reserved "false",
      TAny <$ reserved "any",
      TDynamic <$ reserved "dynamic",
      TUnknown <$ reserved "unknown",
      inferParser,
      parens typeParser,
      TCon "Tuple" <$ reserved "Tuple",
      varOrConParser
    ]

-- | Parse an `infer` binder used inside conditional-type patterns.
inferParser :: Parser Type
inferParser = reserved "infer" *> (TInfer <$> typeIdentifier)

-- | Parse either a constructor-like name or a type variable.
--
-- Uppercase-leading identifiers are treated as constructors so users can write
-- aliases that feel familiar to both Haskell and TypeScript audiences.
varOrConParser :: Parser Type
varOrConParser = do
  name <- typeIdentifier
  pure $
    case name of
      _
        | Just (c, _) <- Text.uncons name,
          isUpper c ->
            TCon name
      _ -> TVar name
