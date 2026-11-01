{-# LANGUAGE LambdaCase #-}

-- | Surface syntax for tynix.
--
-- The AST intentionally stays close to ordinary Nix source. Type-only syntax is
-- attached as annotations or top-level declarations so that erasure back to
-- `.nix` is straightforward and existing mental models still apply.
module Syntax
  ( AmbientDecl (..),
    AmbientEntry (..),
    AttrItem (..),
    BinOp (..),
    DiagnosticDirective (..),
    Expr (..),
    InterpForm (..),
    LetItem (..),
    MacroDef (..),
    MacroFragment (..),
    MacroRule (..),
    MacroToken (..),
    RepeatKind (..),
    Marked (..),
    Pattern (..),
    PatternBinder (..),
    PatternField (..),
    Program (..),
    SrcSpan (..),
    SelectStep (..),
    StringLiteral (..),
    StringPart (..),
    UnaryOp (..),
    exprAnnotations,
    patternFieldNames,
    stringLiteralText,
    stripLocations,
  )
where

import Data.Text (Text)
import Type (Name, Type, TypeAlias)

-- | Source comment directives that affect checker diagnostics.
--
-- These mirror the intent of TypeScript's line-level suppression comments but
-- are currently scoped to executable surfaces the checker understands well:
-- root expressions and `let` items. The parser attaches a directive when a
-- preceding `# @tynix-...` comment targets the next line of code.
data DiagnosticDirective
  = TynixIgnore
  | TynixExpected
  deriving (Eq, Show)

-- | Wrapper for syntax nodes that may carry one diagnostic directive.
data Marked a = Marked
  { markedDirective :: Maybe DiagnosticDirective,
    markedValue :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | A complete tynix file.
--
-- A file may contain type aliases, ambient declarations, and optionally a root
-- expression. Declaration-only files model `.d.tynix` surfaces, while ordinary
-- `.tynix` files typically contain all three sections in varying combinations.
data Program = Program
  { programAliases :: [TypeAlias],
    programAmbient :: [AmbientDecl],
    programExpr :: Maybe (Marked Expr),
    -- | Macros declared in the file. Invocations are expanded while parsing,
    -- so the expression above never contains one; the definitions are kept
    -- so their templates can be type-checked once, at the definition.
    programMacros :: [MacroDef]
  }
  deriving (Eq, Show)

-- | A declarative macro: `macro name { (pattern) => (template); ... };`.
data MacroDef = MacroDef
  { macroName :: Name,
    macroRules :: [MacroRule],
    macroSpan :: SrcSpan
  }
  deriving (Eq, Show)

-- | One rule. The template is kept as source text (it may contain `$(...)*`
-- repetitions, which are expanded textually per invocation) together with
-- its offset in the file; 'ruleCheck' is the template instantiated once and
-- closed over its metavariables as a lambda, ready to type-check.
data MacroRule = MacroRule
  { rulePattern :: [MacroToken],
    ruleTemplate :: Text,
    ruleTemplateOffset :: Int,
    ruleCheck :: Expr
  }
  deriving (Eq, Show)

-- | A pattern element.
data MacroToken
  = -- | A literal token that must appear verbatim: a word or punctuation.
    MLiteral Text
  | -- | A metavariable `$name:fragment` (or `$name :: Type`).
    MVar Name MacroFragment
  | -- | A delimited group, `( ... )`, `[ ... ]`, or `{ ... }`.
    MGroup Text [MacroToken] Text
  | -- | A repetition `$( ... ) sep? *|+|?`.
    MRepeat [MacroToken] (Maybe Text) RepeatKind
  deriving (Eq, Show)

-- | What a metavariable matches.
data MacroFragment
  = -- | An expression; with a type, the argument must have that type.
    FragExpr (Maybe Type)
  | FragIdent
  | FragType
  | FragString
  deriving (Eq, Show)

data RepeatKind = ZeroOrMore | OneOrMore | ZeroOrOne
  deriving (Eq, Show)

-- | Ambient declaration describing an existing `.nix` file.
--
-- This is the bridge that lets tynix add types to code it does not compile
-- itself. The declaration path is resolved relative to the file that declared
-- it, matching how Nix imports are usually written.
data AmbientDecl = AmbientDecl
  { ambientPath :: FilePath,
    ambientEntries :: [AmbientEntry]
  }
  deriving (Eq, Show)

-- | A single exported member inside an ambient declaration.
data AmbientEntry = AmbientEntry
  { ambientEntryName :: Name,
    ambientEntryType :: Type
  }
  deriving (Eq, Show)

-- | Term-level expressions preserved by the compiler.
--
-- The set is intentionally small and currently covers the subset needed to
-- prove the architecture: records, lambdas, applications, imports, selections,
-- lists, local bindings, infix numeric addition, and explicit type assertions.
--
-- 'ECast' models TypeScript-style `expr as Type` assertions. The checker treats
-- them as an explicit user-directed boundary: casts may widen, narrow, or cross
-- a gradual boundary, but they still remain type-only syntax and are erased
-- back to plain `.nix`.
data Expr
  = EVar Name
  | EString StringLiteral
  | EFloat Double
  | EInt Integer
  | EBool Bool
  | ENull
  | EPath FilePath
  | ELambda Pattern Expr
  | EApp Expr Expr
  | EBinaryOp BinOp Expr Expr
  | EUnaryOp UnaryOp Expr
  | ELet [Marked LetItem] Expr
  | EAttrSet [AttrItem]
  | ERec [AttrItem]
  | ESelect Expr [SelectStep]
  | EHasAttr Expr [SelectStep]
  | EAssert Expr Expr
  | EWith Expr Expr
  | EIf Expr Expr Expr
  | EList [Expr]
  | ECast Expr Type
  | -- | A type ascription `(expr :: Type)`: unlike a cast, the expression
    -- must already have the type. Erased on compilation.
    EAscribe Expr Type
  | EInterp InterpForm [StringPart]
  | -- | `base.path or fallback`: selection with a default when any step of the
    -- path is missing.
    ESelectOr Expr [SelectStep] Expr
  | -- | `<nixpkgs>`-style lookup path.
    ESearchPath FilePath
  | -- | Path literal containing antiquotations, such as `./${name}.nix`. Text
    -- segments keep the literal spelling, including the leading `./`, `/`,
    -- or `~/`.
    EPathInterp [StringPart]
  | -- | Source location wrapper. The parser attaches these so the checker can
    -- report span-accurate diagnostics; 'stripLocations' removes them for
    -- consumers that compare trees structurally.
    ELoc SrcSpan Expr
  deriving (Eq, Show)

-- | A half-open source region as 0-based character offsets into the file.
--
-- The end offset may include trailing layout consumed by the lexer; consumers
-- converting to line/column positions trim it against the source text.
data SrcSpan = SrcSpan
  { spanStart :: Int,
    spanEnd :: Int
  }
  deriving (Eq, Ord, Show)

-- | Which string syntax an interpolated string was written in, so the compiler
-- can round-trip the original spelling.
data InterpForm
  = InterpDouble
  | InterpIndented
  deriving (Eq, Show)

-- | One segment of an interpolated string: either literal text or an
-- antiquoted @${expr}@ expression.
data StringPart
  = StrText Text
  | -- | An indented-string escape `''\c`, kept verbatim. Escaped newlines and
    -- tabs must not become literal ones on output, because Nix computes the
    -- indentation to strip from the literal lines only.
    StrEscape Char
  | StrExpr Expr
  deriving (Eq, Show)

-- | Binary operators preserved by the compiler.
--
-- The set mirrors the executable Nix surface tynix understands: numeric
-- arithmetic, list concatenation, structural equality, ordered comparisons,
-- and short-circuiting boolean connectives. Each operator is erased back to
-- the identical Nix spelling, so this enum doubles as the round-trip
-- representation.
data BinOp
  = OpAdd
  | OpSub
  | OpMul
  | OpConcat
  | OpUpdate
  | OpEq
  | OpNeq
  | OpLt
  | OpGt
  | OpLe
  | OpGe
  | OpAnd
  | OpOr
  | OpDiv
  | OpImpl
  | OpPipeRight
  | OpPipeLeft
  deriving (Eq, Show)

-- | Prefix operators preserved by the compiler.
data UnaryOp
  = OpNot
  | OpNeg
  deriving (Eq, Show)

-- | String literals preserved in executable tynix.
--
-- Double-quoted strings and indented `'' ... ''` strings are both first-class
-- so the compiler can round-trip the original Nix string form instead of
-- normalizing everything into one surface spelling.
data StringLiteral
  = DoubleQuoted Text
  | Indented Text
  deriving (Eq, Show)

stringLiteralText :: StringLiteral -> Text
stringLiteralText = \case
  DoubleQuoted text -> text
  Indented text -> text

-- | Lambda binder pattern.
--
-- tynix currently supports variable binders with an optional annotation. The
-- syntax is deliberately Haskell-like while remaining valid-looking to Nix
-- users.
data Pattern
  = PVar Name (Maybe Type)
  | -- | `{ a, b ? 1, ... }@args`: the fields, whether `...` is present, and an
    -- optional whole-argument binder.
    PAttrSet [PatternField] Bool (Maybe PatternBinder)
  deriving (Eq, Show)

-- | One field of an attribute-set lambda pattern. tynix additionally allows an
-- inline annotation, `{ name :: String, version ? "1" }:`, which is erased.
data PatternField = PatternField
  { patternFieldName :: Name,
    patternFieldType :: Maybe Type,
    patternFieldDefault :: Maybe Expr
  }
  deriving (Eq, Show)

-- | The `@name` binder of an attribute-set pattern, remembering which side of
-- the braces it was written on so compilation round-trips the spelling.
data PatternBinder
  = BinderBefore Name
  | BinderAfter Name
  deriving (Eq, Show)

patternFieldNames :: [PatternField] -> [Name]
patternFieldNames = map patternFieldName

-- | One step in an attribute selection chain.
--
-- Static selections cover both bare and quoted attribute names, while dynamic
-- steps preserve Nix antiquotation such as `self.packages.${system}`.
data SelectStep
  = SelectName Name
  | SelectDynamic Expr
  deriving (Eq, Show)

-- | Items allowed in a `let` block.
--
-- Signatures are erased before compilation but kept during checking and
-- declaration emission.
data LetItem
  = LetSignature Name Type
  | LetBinding Name Expr
  | -- | `inherit a b;` or `inherit (source) a b;` inside `let`.
    LetInherit (Maybe Expr) [Name]
  | -- | A nested or dynamic binding path such as `a.b = 1;`.
    LetPath [SelectStep] Expr
  deriving (Eq, Show)

-- | Record attributes inside an attribute set.
--
-- `inherit` is modeled explicitly so the checker can resolve inherited names
-- instead of flattening them away during parsing.
data AttrItem
  = AttrField Name Expr
  | AttrInherit [Name]
  | -- | `inherit (source) a b;`
    AttrInheritFrom Expr [Name]
  | -- | A nested (`a.b.c = v;`) or dynamic (`${k} = v;`) attribute path. Plain
    -- single-name fields always use 'AttrField'.
    AttrPath [SelectStep] Expr
  deriving (Eq, Show)

-- | Remove every 'ELoc' wrapper, recursively.
stripLocations :: Expr -> Expr
stripLocations = go
  where
    go = \case
      ELoc _ inner -> go inner
      ELambda pat body -> ELambda (goPat pat) (go body)
      EApp f x -> EApp (go f) (go x)
      EBinaryOp op l r -> EBinaryOp op (go l) (go r)
      EUnaryOp op x -> EUnaryOp op (go x)
      ELet items body -> ELet (map (fmap goLet) items) (go body)
      EAttrSet items -> EAttrSet (map goAttr items)
      ERec items -> ERec (map goAttr items)
      ESelect base steps -> ESelect (go base) (map goStep steps)
      ESelectOr base steps def -> ESelectOr (go base) (map goStep steps) (go def)
      EHasAttr base path -> EHasAttr (go base) (map goStep path)
      EAssert c b -> EAssert (go c) (go b)
      EWith s b -> EWith (go s) (go b)
      EIf c a b -> EIf (go c) (go a) (go b)
      EList xs -> EList (map go xs)
      ECast e ty -> ECast (go e) ty
      EAscribe e ty -> EAscribe (go e) ty
      EInterp form parts -> EInterp form (map goPart parts)
      EPathInterp parts -> EPathInterp (map goPart parts)
      other -> other
    goPart = \case
      StrExpr e -> StrExpr (go e)
      other -> other
    goStep = \case
      SelectDynamic e -> SelectDynamic (go e)
      other -> other
    goPat = \case
      PAttrSet fields open binder -> PAttrSet [f{patternFieldDefault = go <$> patternFieldDefault f} | f <- fields] open binder
      other -> other
    goLet = \case
      LetBinding n e -> LetBinding n (go e)
      LetInherit src names -> LetInherit (go <$> src) names
      LetPath steps e -> LetPath (map goStep steps) (go e)
      other -> other
    goAttr = \case
      AttrField n e -> AttrField n (go e)
      AttrInheritFrom src names -> AttrInheritFrom (go src) names
      AttrPath steps e -> AttrPath (map goStep steps) (go e)
      other -> other

-- | Every type annotation embedded in an expression, in source order: lambda
-- binder annotations, `let` signatures, and `as` casts.
exprAnnotations :: Expr -> [Type]
exprAnnotations = go
  where
    go = \case
      ELoc _ inner -> go inner
      ELambda pat body -> goPat pat <> go body
      EApp f x -> go f <> go x
      EBinaryOp _ l r -> go l <> go r
      EUnaryOp _ x -> go x
      ELet items body -> foldMap (goLet . markedValue) items <> go body
      EAttrSet items -> foldMap goAttr items
      ERec items -> foldMap goAttr items
      ESelect base steps -> go base <> foldMap goStep steps
      ESelectOr base steps def -> go base <> foldMap goStep steps <> go def
      EHasAttr base path -> go base <> foldMap goStep path
      EAssert c b -> go c <> go b
      EWith sc b -> go sc <> go b
      EIf c a b -> go c <> go a <> go b
      EList xs -> foldMap go xs
      ECast e ty -> go e <> [ty]
      EAscribe e ty -> go e <> [ty]
      EInterp _ parts -> foldMap goPart parts
      EPathInterp parts -> foldMap goPart parts
      EVar _ -> []
      EString _ -> []
      EFloat _ -> []
      EInt _ -> []
      EBool _ -> []
      ENull -> []
      EPath _ -> []
      ESearchPath _ -> []
    goPart = \case
      StrExpr e -> go e
      StrText _ -> []
      StrEscape _ -> []
    goStep = \case
      SelectDynamic e -> go e
      SelectName _ -> []
    goPat = \case
      PVar _ ann -> maybe [] pure ann
      PAttrSet fields _ _ -> foldMap (\f -> maybe [] pure (patternFieldType f) <> foldMap go (patternFieldDefault f)) fields
    goLet = \case
      LetSignature _ ty -> [ty]
      LetBinding _ e -> go e
      LetInherit src _ -> foldMap go src
      LetPath steps e -> foldMap goStep steps <> go e
    goAttr = \case
      AttrField _ e -> go e
      AttrInherit _ -> []
      AttrInheritFrom src _ -> go src
      AttrPath steps e -> foldMap goStep steps <> go e
