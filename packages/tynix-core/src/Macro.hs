{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Hygienic, declarative macros.
--
-- A macro is declared next to type aliases:
--
-- @
-- macro enum {
--   ( $( $tag:ident ),* ) => ({ $( $tag = stringify!($tag); )* });
-- };
-- @
--
-- and invoked as @enum!(red, green)@. Invocations are expanded while the file
-- is parsed (see "ParserExpr"); this module holds the parts of expansion that
-- do not need the parser:
--
-- * 'instantiateTemplate' expands @$( ... )*@ repetitions textually, naming
--   each metavariable occurrence by its repetition path;
-- * 'hygienize' renames every binder the template itself introduces, so it
--   can neither capture a name from the call site nor be captured by one, and
--   pins the template's free names to @builtins@, so a local @map@ at the
--   call site cannot hijack them;
-- * 'substitute' splices the matched fragments in.
--
-- Typed metavariables (@$x :: Int@) are spliced as type ascriptions, so the
-- checker verifies each argument against the declared type, and every rule's
-- template is type-checked once at the definition ("Driver").
module Macro
  ( Binding (..),
    Leaf (..),
    builtinGlobalNames,
    closeTemplate,
    hygienize,
    instantiateTemplate,
    placeholderNames,
    relocate,
    strayPlaceholders,
    substitute,
    symbolicBindings,
    tokenVars,
  )
where

import Control.Monad (forM, when)
import Data.Char (isAlphaNum, isLetter)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Diagnostics (DiagnosticCode (..), withCode)
import Pretty (renderExpr)
import Syntax
import Type hiding (LiteralType (..))

-- | What a metavariable matched: one fragment, or one entry per repetition.
data Binding
  = Single Leaf
  | Repeated [Binding]
  deriving (Eq, Show)

-- | A matched fragment. 'LSymbolic' stands for "some argument" when a
-- template is instantiated for definition-time checking.
data Leaf
  = LExpr Expr (Maybe Type)
  | LIdent Name
  | LType Type
  | LStr Text
  | LSymbolic MacroFragment
  deriving (Eq, Show)

-- | Names Nix puts in scope everywhere. A template may refer to them freely;
-- 'hygienize' reroutes each through @builtins@.
builtinGlobalNames :: [Name]
builtinGlobalNames =
  [ "abort",
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
    "import",
    "isNull",
    "map",
    "placeholder",
    "removeAttrs",
    "scopedImport",
    "throw",
    "toString"
  ]

-- | The metavariables bound by a pattern, with their repetition depth.
tokenVars :: [MacroToken] -> [(Name, MacroFragment, Int)]
tokenVars = concatMap (go 0)
  where
    go depth = \case
      MLiteral _ -> []
      MVar name fragment -> [(name, fragment, depth)]
      MGroup _ inner _ -> concatMap (go depth) inner
      MRepeat inner _ _ -> concatMap (go (depth + 1)) inner

-- | Bindings that match every metavariable symbolically, each repetition
-- taken once: the instance a template is checked with at its definition.
symbolicBindings :: [MacroToken] -> Map Name Binding
symbolicBindings tokens =
  Map.fromList [(name, wrap depth (Single (LSymbolic fragment))) | (name, fragment, depth) <- tokenVars tokens]
  where
    wrap :: Int -> Binding -> Binding
    wrap 0 binding = binding
    wrap n binding = Repeated [wrap (n - 1) binding]

-- | Expand a template's repetitions for one set of bindings.
--
-- Every metavariable occurrence is renamed after its repetition path
-- (@$x@ at the top level, @$x'0@ in the first iteration, @$x'1'0@ nested),
-- and the result maps each such placeholder to the fragment it stands for.
instantiateTemplate :: Map Name Binding -> Text -> Either String (Text, Map Name Leaf)
instantiateTemplate bindings template = go bindings "" (Text.unpack template)
  where
    go :: Map Name Binding -> String -> String -> Either String (Text, Map Name Leaf)
    go env path input = do
      (out, leaves) <- walk env path input
      pure (Text.pack out, leaves)

    walk :: Map Name Binding -> String -> String -> Either String (String, Map Name Leaf)
    walk _ _ [] = Right ([], Map.empty)
    walk env path ('$' : '(' : rest) = do
      (body, afterBody) <- balanced rest
      let (separator, kindAndRest) = splitSeparator afterBody
      (_, remaining) <- case kindAndRest of
        c : more | c `elem` ("*+?" :: String) -> Right (c, more)
        _ -> Left (withCode TX0004InvalidTemplate "a template repetition `$( ... )` must end in `*`, `+`, or `?`")
      let used = nub [name | name <- referencedVars body, Just (Repeated _) <- [Map.lookup name env]]
      when (null used) $
        Left (withCode TX0004InvalidTemplate "a template repetition must mention a repeated metavariable")
      lengths <- forM used $ \name -> case Map.lookup name env of
        Just (Repeated items) -> Right (length items)
        _ -> Right 0
      count <- case lengths of
        n : _ | all (== n) lengths -> Right n
        _ -> Left (withCode TX0004InvalidTemplate ("metavariables repeat a different number of times: " <> unwords (map (("$" <>) . Text.unpack) used)))
      iterations <- forM [0 .. count - 1] $ \i -> do
        let env' = foldr (Map.adjust (\case Repeated items -> items !! i; other -> other)) env used
        walk env' (path <> "'" <> show i) body
      (tailOut, tailLeaves) <- walk env path remaining
      let joined = intercalateWith separator (map fst iterations)
      pure (joined <> tailOut, Map.unions (tailLeaves : map snd iterations))
    walk env path ('$' : c : rest)
      | isLetter c || c == '_' =
          let (nameRest, remaining) = span identChar rest
              name = Text.pack (c : nameRest)
           in case Map.lookup name env of
                Just (Single leaf) -> do
                  let placeholder = "$" <> Text.unpack name <> path
                  (tailOut, tailLeaves) <- walk env path remaining
                  pure (placeholder <> tailOut, Map.insert (Text.pack placeholder) leaf tailLeaves)
                Just (Repeated _) ->
                  Left (withCode TX0004InvalidTemplate ("metavariable `$" <> Text.unpack name <> "` repeats, so it must be used inside `$( ... )*`"))
                -- Not a pattern variable: `stringify!`, or literal text such
                -- as a shell `$out` inside a string. A stray use in code is
                -- still rejected after expansion (TX0006).
                Nothing -> passThrough ('$' : c : nameRest) remaining
      where
        passThrough consumed remaining = do
          (tailOut, tailLeaves) <- walk env path remaining
          pure (consumed <> tailOut, tailLeaves)
    walk env path (c : rest) = do
      (tailOut, tailLeaves) <- walk env path rest
      pure (c : tailOut, tailLeaves)

    intercalateWith separator = \case
      [] -> []
      first : more -> first <> concatMap (\item -> maybe " " (<> " ") separator <> item) more

    splitSeparator = \case
      c : more | c `elem` ("*+?" :: String) -> (Nothing, c : more)
      c : more | c `elem` (",;" :: String) -> (Just [c], more)
      other -> (Nothing, other)

    identChar ch = isAlphaNum ch || ch `elem` ("_-" :: String)

    referencedVars body =
      [ Text.pack (c : takeWhile identChar rest)
      | ('$' : c : rest) <- suffixes body,
        isLetter c || c == '_'
      ]
    suffixes xs = case xs of
      [] -> []
      _ : rest -> xs : suffixes rest

-- | Split off the body of a `$( ... )` group, respecting nested brackets and
-- string literals.
balanced :: String -> Either String (String, String)
balanced = go (0 :: Int) []
  where
    go _ _ [] = Left (withCode TX0004InvalidTemplate "unterminated `$(` repetition in template")
    go 0 acc (')' : rest) = Right (reverse acc, rest)
    go depth acc (c : rest)
      | c `elem` ("([{" :: String) = go (depth + 1) (c : acc) rest
      | c `elem` (")]}" :: String) = go (depth - 1) (c : acc) rest
      | c == '"' =
          let (str, more) = stringBody rest
           in go depth (reverse ('"' : str) <> acc) more
      | otherwise = go depth (c : acc) rest
    stringBody = \case
      [] -> ([], [])
      '\\' : x : more -> let (s, r) = stringBody more in ('\\' : x : s, r)
      '"' : more -> ("\"", more)
      x : more -> let (s, r) = stringBody more in (x : s, r)

-- | Point every location in a template instance at the invocation, so
-- diagnostics inside an expansion land on the macro call.
relocate :: SrcSpan -> Expr -> Expr
relocate region = mapExpr (\case ELoc _ inner -> Just (ELoc region (relocate region inner)); _ -> Nothing)

-- | Rename every binder a template introduces and pin its free names to
-- @builtins@.
--
-- Binders are renamed with @suffix@ (unique per invocation): @x@ becomes
-- @x'1_42@. Metavariables (@$x@) are left alone — an identifier passed in by
-- the caller binds intentionally. A free name must be a Nix global (it
-- becomes @builtins.name@); anything else is an error, since the template
-- would otherwise see whatever the call site happens to bind under that name.
-- `with` and `rec` are rejected: both bind names that cannot be renamed.
hygienize :: Text -> Expr -> Either String Expr
hygienize suffix = go Map.empty
  where
    fresh name = name <> suffix
    bindAll = foldr (\name -> if isPlaceholderName name then id else Map.insert name (fresh name))
    renameBinder scope name = Map.findWithDefault name name scope

    go :: Map Name Name -> Expr -> Either String Expr
    go scope = \case
      EVar name
        | isPlaceholderName name -> Right (EVar name)
        | Just renamed <- Map.lookup name scope -> Right (EVar renamed)
        | name == "builtins" -> Right (EVar name)
        | name `elem` builtinGlobalNames -> Right (ESelect (EVar "builtins") [SelectName name])
        | otherwise ->
            Left
              ( withCode
                  TX0003HygieneViolation
                  ("macro template refers to `" <> Text.unpack name <> "`, which is not in scope where the macro is defined; take it as a parameter instead")
              )
      ELoc region inner -> ELoc region <$> go scope inner
      ELambda pattern' body -> do
        -- Attribute-pattern fields are matched by name, so they keep their
        -- spelling (and bind it); every other binder is renamed.
        let fieldScope = foldr (\name -> Map.insert name name) scope (fieldNames pattern')
            scope' = bindAll fieldScope (patternNames pattern')
        ELambda <$> goPattern scope' pattern' <*> go scope' body
      EApp f x -> EApp <$> go scope f <*> go scope x
      EBinaryOp op l r -> EBinaryOp op <$> go scope l <*> go scope r
      EUnaryOp op x -> EUnaryOp op <$> go scope x
      ELet items body -> do
        let scope' = bindAll scope (concatMap (letNames . markedValue) items)
        ELet <$> traverse (traverse (goLet scope scope')) items <*> go scope' body
      EAttrSet items -> EAttrSet <$> traverse (goAttr scope) items
      ERec _ -> Left (withCode TX0003HygieneViolation "macro templates cannot use `rec`, whose field names would be visible to the caller's arguments")
      EWith _ _ -> Left (withCode TX0003HygieneViolation "macro templates cannot use `with`, which would capture names in the caller's arguments")
      ESelect base steps -> ESelect <$> go scope base <*> traverse (goStep scope) steps
      ESelectOr base steps def -> ESelectOr <$> go scope base <*> traverse (goStep scope) steps <*> go scope def
      EHasAttr base steps -> EHasAttr <$> go scope base <*> traverse (goStep scope) steps
      EAssert c b -> EAssert <$> go scope c <*> go scope b
      EIf c a b -> EIf <$> go scope c <*> go scope a <*> go scope b
      EList xs -> EList <$> traverse (go scope) xs
      ECast e ty -> (`ECast` ty) <$> go scope e
      EAscribe e ty -> (`EAscribe` ty) <$> go scope e
      EInterp form parts -> EInterp form <$> traverse (goPart scope) parts
      EPathInterp parts -> EPathInterp <$> traverse (goPart scope) parts
      other -> Right other

    goPart scope = \case
      StrExpr e -> StrExpr <$> go scope e
      other -> Right other
    goStep scope = \case
      SelectDynamic e -> SelectDynamic <$> go scope e
      other -> Right other
    goPattern scope = \case
      PVar name ann -> Right (PVar (renameBinder scope name) ann)
      PAttrSet fields open binder ->
        PAttrSet
          <$> traverse
            ( \field -> do
                fallback <- traverse (go scope) (patternFieldDefault field)
                -- A pattern field is matched by name, so it keeps its
                -- spelling; only its binding inside the body is renamed by
                -- re-binding below.
                pure field{patternFieldDefault = fallback}
            )
            fields
          <*> pure open
          <*> pure (fmap (renamePatternBinder scope) binder)
    renamePatternBinder scope = \case
      BinderBefore name -> BinderBefore (renameBinder scope name)
      BinderAfter name -> BinderAfter (renameBinder scope name)
    -- Field patterns bind under their attribute name, which cannot change,
    -- so they are kept out of the renaming scope.
    patternNames = \case
      PVar name _ -> [name]
      PAttrSet _ _ binder -> maybe [] (pure . binderName) binder
    fieldNames = \case
      PAttrSet fields _ _ -> filter (not . isPlaceholderName) (map patternFieldName fields)
      PVar _ _ -> []
    binderName = \case
      BinderBefore name -> name
      BinderAfter name -> name
    letNames = \case
      LetBinding name _ -> [name]
      LetPath (SelectName name : _) _ -> [name]
      LetInherit _ names -> names
      LetSignature _ _ -> []
      LetPath _ _ -> []
    goLet outer scope = \case
      LetBinding name e -> LetBinding (renameBinder scope name) <$> go scope e
      LetSignature name ty -> Right (LetSignature (renameBinder scope name) ty)
      LetPath (SelectName name : rest) e -> LetPath (SelectName (renameBinder scope name) : rest) <$> go scope e
      LetPath steps e -> LetPath steps <$> go scope e
      LetInherit source names
        | all isPlaceholderName names -> (`LetInherit` names) <$> traverse (go scope) source
      -- `inherit x;` is `x = x;` with the right side in the outer scope.
      LetInherit Nothing [single] ->
        LetBinding (renameBinder scope single) <$> go outer (EVar single)
      -- `inherit (src) x;` is `x = src.x;`; the attribute keeps its spelling.
      LetInherit (Just source) [single] ->
        LetBinding (renameBinder scope single) . (\src -> ESelect src [SelectName single]) <$> go scope source
      LetInherit _ _ -> Left (withCode TX0004InvalidTemplate "write one `inherit` per name in a macro template")
    goAttr scope = \case
      AttrField name e -> AttrField name <$> go scope e
      AttrPath steps e -> AttrPath <$> traverse (goStep scope) steps <*> go scope e
      AttrInheritFrom source names -> (`AttrInheritFrom` names) <$> go scope source
      -- `inherit x;` reads the variable `x`; spell it out so the reference
      -- can be renamed or pinned like any other.
      AttrInherit names
        | all isPlaceholderName names -> Right (AttrInherit names)
        | otherwise -> expandInherit scope names
    expandInherit scope names =
      case names of
        [single] -> AttrField single <$> go scope (EVar single)
        _ -> Left (withCode TX0004InvalidTemplate "write one `inherit` per name in a macro template")

-- | Splice matched fragments into a hygienized template instance.
substitute :: Map Name Leaf -> Expr -> Either String Expr
substitute leaves = go
  where
    leafFor name = Map.lookup name leaves
    identFor :: Name -> Either String Name
    identFor name
      | not (isPlaceholderName name) = Right name
      | otherwise = case leafFor name of
          Just (LIdent ident) -> Right ident
          Just (LSymbolic FragIdent) -> Right name
          Just _ -> Left (withCode TX0004InvalidTemplate ("metavariable `" <> Text.unpack name <> "` is used as a name, so it must be an `ident` fragment"))
          Nothing -> Right name

    go :: Expr -> Either String Expr
    go = \case
      EApp (EVar "$stringify") arg -> Right (EString (DoubleQuoted (stringified arg)))
      EVar name
        | isPlaceholderName name ->
            case leafFor name of
              Just (LExpr e Nothing) -> Right e
              -- The ascription keeps the argument's location, so a type
              -- error in the argument is reported at the argument.
              Just (LExpr e (Just ty)) -> keepLocation e . EAscribe e <$> goType ty
              Just (LIdent ident) -> Right (EVar ident)
              Just (LStr text) -> Right (EString (DoubleQuoted text))
              Just (LType _) -> Left (withCode TX0004InvalidTemplate ("metavariable `" <> Text.unpack name <> "` is a type, but it is used as an expression"))
              _ -> Right (EVar name)
      ELoc region inner -> ELoc region <$> go inner
      ELambda pattern' body -> ELambda <$> goPattern pattern' <*> go body
      EApp f x -> EApp <$> go f <*> go x
      EBinaryOp op l r -> EBinaryOp op <$> go l <*> go r
      EUnaryOp op x -> EUnaryOp op <$> go x
      ELet items body -> ELet <$> traverse (traverse goLet) items <*> go body
      EAttrSet items -> EAttrSet <$> traverse goAttr items
      ERec items -> ERec <$> traverse goAttr items
      ESelect base steps -> ESelect <$> go base <*> traverse goStep steps
      ESelectOr base steps def -> ESelectOr <$> go base <*> traverse goStep steps <*> go def
      EHasAttr base steps -> EHasAttr <$> go base <*> traverse goStep steps
      EAssert c b -> EAssert <$> go c <*> go b
      EWith s b -> EWith <$> go s <*> go b
      EIf c a b -> EIf <$> go c <*> go a <*> go b
      EList xs -> EList <$> traverse go xs
      ECast e ty -> ECast <$> go e <*> goType ty
      EAscribe e ty -> EAscribe <$> go e <*> goType ty
      EInterp form parts -> EInterp form <$> traverse goPart parts
      EPathInterp parts -> EPathInterp <$> traverse goPart parts
      other -> Right other

    keepLocation e wrapped = case e of
      ELoc region _ -> ELoc region wrapped
      _ -> wrapped

    stringified arg =
      case stripLocations arg of
        EVar name
          | isPlaceholderName name ->
              case leafFor name of
                Just (LIdent ident) -> ident
                Just (LStr text) -> text
                Just (LExpr e _) -> renderExpr e
                _ -> name
          | otherwise -> name
        other -> renderExpr other

    goType = Right . substituteTypePlaceholders
    substituteTypePlaceholders ty =
      let names = Set.toList (typePlaceholders ty)
          subst = Map.fromList [(name, replacement) | name <- names, Just replacement <- [typeLeaf name]]
       in substituteTypeVars subst ty
    typeLeaf name = case leafFor name of
      Just (LType ty) -> Just ty
      Just (LIdent ident) -> Just (TVar ident)
      _ -> Nothing

    goPart = \case
      StrExpr e -> StrExpr <$> go e
      other -> Right other
    goStep = \case
      SelectName name -> SelectName <$> identFor name
      SelectDynamic e -> SelectDynamic <$> go e
    goPattern = \case
      PVar name ann -> PVar <$> identFor name <*> traverse goType ann
      PAttrSet fields open binder ->
        PAttrSet
          <$> traverse
            ( \field ->
                (\name ty fallback -> field{patternFieldName = name, patternFieldType = ty, patternFieldDefault = fallback})
                  <$> identFor (patternFieldName field)
                  <*> traverse goType (patternFieldType field)
                  <*> traverse go (patternFieldDefault field)
            )
            fields
          <*> pure open
          <*> traverse goBinder binder
    goBinder = \case
      BinderBefore name -> BinderBefore <$> identFor name
      BinderAfter name -> BinderAfter <$> identFor name
    goLet = \case
      LetBinding name e -> LetBinding <$> identFor name <*> go e
      LetSignature name ty -> LetSignature <$> identFor name <*> goType ty
      LetInherit source names -> LetInherit <$> traverse go source <*> traverse identFor names
      LetPath steps e -> LetPath <$> traverse goStep steps <*> go e
    goAttr = \case
      AttrField name e -> AttrField <$> identFor name <*> go e
      AttrInherit names -> AttrInherit <$> traverse identFor names
      AttrInheritFrom source names -> AttrInheritFrom <$> go source <*> traverse identFor names
      AttrPath steps e -> AttrPath <$> traverse goStep steps <*> go e

-- | Close a symbolically-instantiated template over its metavariables, giving
-- a lambda the checker can verify once: a typed `$x :: T` becomes a binder
-- annotated with `T`, every other metavariable an unannotated binder.
closeTemplate :: Map Name Leaf -> Expr -> Expr
closeTemplate leaves body =
  foldr (\(name, ann) inner -> ELambda (PVar name ann) inner) body params
  where
    used = freePlaceholders body
    params =
      [ (name, annotation)
      | name <- Set.toList used,
        name /= "$stringify",
        let annotation = case Map.lookup name leaves of
              Just (LSymbolic (FragExpr ty)) -> ty
              _ -> Nothing
      ]

-- | Placeholders a template instance mentions anywhere.
placeholderNames :: Expr -> [Name]
placeholderNames expr = Set.toList (freePlaceholders expr)

freePlaceholders :: Expr -> Set.Set Name
freePlaceholders = foldExpr (\case EVar name | isPlaceholderName name -> Set.singleton name; _ -> Set.empty)

typePlaceholders :: Type -> Set.Set Name
typePlaceholders = Set.filter isPlaceholderName . freeTypeVars

-- | Metavariable names left in ordinary code (outside any template).
strayPlaceholders :: Expr -> [Name]
strayPlaceholders expr =
  Set.toList (freePlaceholders expr <> foldExpr binderPlaceholders expr)
  where
    -- Attribute names are not checked: a quoted `"$x" = 1;` is ordinary Nix.
    binderPlaceholders = \case
      ELambda (PVar name _) _ | isPlaceholderName name -> Set.singleton name
      ELet items _ -> Set.fromList [name | Marked _ (LetBinding name _) <- items, isPlaceholderName name]
      _ -> Set.empty

-- | A metavariable: `$` followed by an identifier.
isPlaceholderName :: Name -> Bool
isPlaceholderName name =
  case Text.uncons name of
    Just ('$', rest) -> maybe False (\(c, _) -> isLetter c || c == '_') (Text.uncons rest)
    _ -> False

-- | Rewrite expressions top-down: where @f@ answers 'Just', its answer
-- replaces the node; elsewhere the children are rewritten.
mapExpr :: (Expr -> Maybe Expr) -> Expr -> Expr
mapExpr f = go
  where
    go expr = fromMaybe (descend expr) (f expr)
    descend = \case
      ELoc region inner -> ELoc region (go inner)
      ELambda pattern' body -> ELambda (goPattern pattern') (go body)
      EApp a b -> EApp (go a) (go b)
      EBinaryOp op l r -> EBinaryOp op (go l) (go r)
      EUnaryOp op x -> EUnaryOp op (go x)
      ELet items body -> ELet (map (fmap goLet) items) (go body)
      EAttrSet items -> EAttrSet (map goAttr items)
      ERec items -> ERec (map goAttr items)
      ESelect base steps -> ESelect (go base) (map goStep steps)
      ESelectOr base steps def -> ESelectOr (go base) (map goStep steps) (go def)
      EHasAttr base steps -> EHasAttr (go base) (map goStep steps)
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
    goPattern = \case
      PAttrSet fields open binder -> PAttrSet [field{patternFieldDefault = go <$> patternFieldDefault field} | field <- fields] open binder
      other -> other
    goLet = \case
      LetBinding name e -> LetBinding name (go e)
      LetInherit source names -> LetInherit (go <$> source) names
      LetPath steps e -> LetPath (map goStep steps) (go e)
      other -> other
    goAttr = \case
      AttrField name e -> AttrField name (go e)
      AttrInheritFrom source names -> AttrInheritFrom (go source) names
      AttrPath steps e -> AttrPath (map goStep steps) (go e)
      other -> other

-- | Fold a monoid over every expression node.
foldExpr :: (Monoid m) => (Expr -> m) -> Expr -> m
foldExpr f expr = f expr <> foldMap (foldExpr f) (children expr)
  where
    children = \case
      ELoc _ inner -> [inner]
      ELambda pattern' body -> patternChildren pattern' <> [body]
      EApp a b -> [a, b]
      EBinaryOp _ l r -> [l, r]
      EUnaryOp _ x -> [x]
      ELet items body -> concatMap (letChildren . markedValue) items <> [body]
      EAttrSet items -> concatMap attrChildren items
      ERec items -> concatMap attrChildren items
      ESelect base steps -> base : concatMap stepChildren steps
      ESelectOr base steps def -> base : concatMap stepChildren steps <> [def]
      EHasAttr base steps -> base : concatMap stepChildren steps
      EAssert c b -> [c, b]
      EWith s b -> [s, b]
      EIf c a b -> [c, a, b]
      EList xs -> xs
      ECast e _ -> [e]
      EAscribe e _ -> [e]
      EInterp _ parts -> [e | StrExpr e <- parts]
      EPathInterp parts -> [e | StrExpr e <- parts]
      _ -> []
    patternChildren = \case
      PAttrSet fields _ _ -> [e | field <- fields, Just e <- [patternFieldDefault field]]
      PVar _ _ -> []
    letChildren = \case
      LetBinding _ e -> [e]
      LetInherit source _ -> maybe [] pure source
      LetPath steps e -> concatMap stepChildren steps <> [e]
      LetSignature _ _ -> []
    attrChildren = \case
      AttrField _ e -> [e]
      AttrInheritFrom source _ -> [source]
      AttrPath steps e -> concatMap stepChildren steps <> [e]
      AttrInherit _ -> []
    stepChildren = \case
      SelectDynamic e -> [e]
      SelectName _ -> []
