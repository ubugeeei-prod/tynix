{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Core type-language definitions used by every tynix phase.
--
-- The design deliberately models the \"type layer\" as data rather than as a
-- separate elaborated IR. That keeps the compiler simple: parsing, checking,
-- declaration emission, and LSP hover all inspect the same tree. It also fits
-- tynix's TypeScript-inspired strategy where types are erased before runtime and
-- may remain partially unresolved for a while.
module Type
  ( Arrow (..),
    Kind (..),
    LiteralType (..),
    Multiplicity (..),
    Name,
    Scheme (..),
    Type (.., TFun),
    TypeAlias (..),
    closeMetas,
    effectOnlyMetas,
    effectLabel,
    effectLabels,
    effectRow,
    eraseForall,
    freeMetas,
    freeMetasScheme,
    freeTypeMetas,
    freeTypeVars,
    isPureEffects,
    loneEffectMetas,
    plainAlias,
    plainArrow,
    pureEffects,
    schemeFromAnnotation,
    tAny,
    tAttrsOf,
    mkOpenRecord,
    substituteMetas,
    substituteTypeVars,
    tBool,
    tDynamic,
    tFloat,
    tInt,
    tList,
    tNat,
    tNull,
    tNumber,
    tPath,
    tString,
    tUnknown,
  )
where

import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

-- | Identifier type shared by terms and types.
--
-- tynix intentionally reuses plain textual names so that generated `.nix`
-- output stays close to source and ambient declarations can mirror existing
-- Nix code without a name-mangling phase.
type Name = Text

-- | Literal singleton types.
--
-- These provide the \"type puzzle\" building blocks that gradual systems often
-- rely on. Literal types participate in structural subtyping, joins, and
-- declaration emission, which lets the checker preserve precise information
-- until widening becomes necessary.
data LiteralType = LBool Bool | LFloat Double | LInt Integer | LString Text
  deriving (Eq, Ord, Show)

-- | The lightweight kind language used to validate higher-kinded types.
--
-- tynix does not surface kinds in source syntax yet, but it still infers them
-- so aliases such as `Compose f g a = f (g a)` are accepted while mistakes like
-- `Int String` are rejected early.
data Kind
  = KType
  | KFun Kind Kind
  | KMeta Int
  deriving (Eq, Ord, Show)

-- | Argument multiplicity for function arrows.
--
-- `One` models linear functions that must consume their argument exactly once,
-- while `Many` is the ordinary unrestricted arrow used by plain Nix code.
data Multiplicity
  = One
  | Many
  deriving (Eq, Ord, Show)

-- | Everything an arrow knows besides its domain and codomain.
--
-- * 'arrowMult' — `->` (unrestricted) or `%1 ->` (linear).
-- * 'arrowEffects' — the latent effect row performed when the function is
--   fully applied. It is a row in the same representation as records
--   ('TRecord' for a closed row, 'TOpenRecord' for an open one, a 'TVar' or
--   'TMeta' for an effect variable), whose labels map to 'effectLabel'.
--   'TDynamic' means \"not tracked\": the gradual default for arrows written
--   without `! { ... }`.
-- * 'arrowCaptures' — the capabilities a closure captures, `A ->{c} B`.
--   'Nothing' is the universal capture set (untracked).
-- * 'arrowBinder' — the name of a dependent parameter, `(n :: Nat) -> Vec n
--   a`. The name is bound in the codomain, where it stands for the precise
--   (singleton) type of the argument.
data Arrow = Arrow
  { arrowMult :: Multiplicity,
    arrowEffects :: Type,
    arrowCaptures :: Maybe [Name],
    arrowBinder :: Maybe Name
  }
  deriving (Eq, Ord, Show)

-- | The tynix type language.
--
-- A few design choices are worth calling out:
--
-- * 'TDynamic' is the gradual escape hatch and is never compiled to runtime
--   checks.
-- * 'TAny' models TypeScript-style unsound escape hatches where values may
--   flow to and from any expected shape.
-- * 'TUnknown' is the top type: every value may be viewed as `unknown`, but
--   callers must narrow or annotate before using it as something more precise.
-- * 'TApp' keeps higher-kinded and alias applications first-class.
-- * 'TConditional' and 'TInfer' provide TypeScript-style type-level pattern
--   matching.
-- * 'TMeta' exists only during inference and is closed away before public
--   results are reported.
data Type
  = TVar Name
  | TCon Name
  | TMeta Int
  | TLit LiteralType
  | TTypeList [Type]
  | TAny
  | TDynamic
  | TUnknown
  | -- | A function arrow. Most code matches the 'TFun' pattern, which ignores
    -- the effect, capture, and dependency information; code that cares uses
    -- this constructor directly.
    TArrow Arrow Type Type
  | TRecord (Map Name Type)
  | -- | An open record (row): at least these fields, plus whatever the tail
    -- describes. During inference the tail is a meta, so field requirements
    -- can accumulate; after generalization it becomes a row variable. A
    -- `dynamic` tail means "unknown further fields".
    TOpenRecord (Map Name Type) Type
  | -- | Marks a record field that may be absent, written `name? :: T`. Only
    -- meaningful as a field type of 'TRecord' or 'TOpenRecord'.
    TOptional Type
  | TUnion [Type]
  | TApp Type Type
  | TForall [Name] Type
  | TConditional Type Type Type Type
  | TInfer Name
  | -- | The singleton type of a term variable: \"the value bound to this
    -- name\", whose type is the second field. Introduced when a lambda is
    -- checked against a dependent arrow, so the codomain can mention the
    -- argument's value.
    TSingleton Name Type
  deriving (Eq, Ord, Show)

{-# COMPLETE TVar, TCon, TMeta, TLit, TTypeList, TAny, TDynamic, TUnknown, TFun, TRecord, TOpenRecord, TOptional, TUnion, TApp, TForall, TConditional, TInfer, TSingleton #-}

-- | A plain (non-dependent, capture-agnostic) function arrow. Matching ignores
-- the arrow's effects, captures, and binder; constructing one yields an arrow
-- whose effects are untracked ('TDynamic').
pattern TFun :: Multiplicity -> Type -> Type -> Type
pattern TFun mult a b <- TArrow (arrowMult -> mult) a b
  where
    TFun mult a b = TArrow (plainArrow mult) a b

-- | An arrow with untracked effects and captures and no dependent binder.
plainArrow :: Multiplicity -> Arrow
plainArrow mult = Arrow{arrowMult = mult, arrowEffects = TDynamic, arrowCaptures = Nothing, arrowBinder = Nothing}

-- | The payload every effect label maps to inside an effect row.
effectLabel :: Type
effectLabel = TCon "Effect"

-- | Build an effect row from labels and an optional tail.
effectRow :: [Name] -> Maybe Type -> Type
effectRow labels = \case
  Nothing -> TRecord fields
  Just tail' -> mkOpenRecord fields tail'
  where
    fields = Map.fromList [(label, effectLabel) | label <- labels]

-- | The empty, closed effect row: a pure function.
pureEffects :: Type
pureEffects = TRecord Map.empty

isPureEffects :: Type -> Bool
isPureEffects = \case
  TRecord fields -> Map.null fields
  _ -> False

-- | The labels of an effect row, ignoring its tail.
effectLabels :: Type -> [Name]
effectLabels = \case
  TRecord fields -> Map.keys fields
  TOpenRecord fields _ -> Map.keys fields
  _ -> []

-- | A user-facing polymorphic type scheme.
--
-- The checker closes remaining metas into synthetic type variables before it
-- returns a scheme. That makes CLI output, LSP hover, and declaration files
-- deterministic even when inference started from fresh unknowns.
data Scheme = Scheme {schemeVars :: [Name], schemeType :: Type}
  deriving (Eq, Ord, Show)

-- | A named type alias.
--
-- Aliases are intentionally expressive enough to encode generic helpers, HKT-
-- shaped encodings, and ambient library surfaces while still being easy to
-- expand structurally.
data TypeAlias = TypeAlias
  { typeAliasName :: Name,
    typeAliasParams :: [Name],
    typeAliasBody :: Type,
    -- | `opaque type`: a nominal type. Its body is the representation, which
    -- is only revealed by an explicit `as` cast, so `Id User` and
    -- `Id Package` stay distinct even when `t` is a phantom parameter.
    typeAliasOpaque :: Bool,
    -- | Explicit kind annotations on parameters, `(f :: Type -> Type)`.
    typeAliasParamKinds :: [Maybe Kind]
  }
  deriving (Eq, Ord, Show)

-- | A transparent alias with unannotated parameters.
plainAlias :: Name -> [Name] -> Type -> TypeAlias
plainAlias name params body =
  TypeAlias
    { typeAliasName = name,
      typeAliasParams = params,
      typeAliasBody = body,
      typeAliasOpaque = False,
      typeAliasParamKinds = map (const Nothing) params
    }

tString, tInt, tFloat, tNumber, tNat, tBool, tNull, tPath, tAny, tDynamic, tUnknown :: Type
tString = TCon "String"
tInt = TCon "Int"
tFloat = TCon "Float"
tNumber = TCon "Number"
tNat = TCon "Nat"
tBool = TCon "Bool"
tNull = TCon "Null"
tPath = TCon "Path"
tAny = TAny
tDynamic = TDynamic
tUnknown = TUnknown

-- | Smart constructor for the built-in attribute-set dictionary type.
tAttrsOf :: Type -> Type
tAttrsOf = TApp (TCon "AttrsOf")

-- | Build an open record, flattening nested rows: a tail that is itself a
-- record contributes its fields (earlier fields win), and a closed tail closes
-- the result.
mkOpenRecord :: Map Name Type -> Type -> Type
mkOpenRecord fields = \case
  TOpenRecord more tail' -> mkOpenRecord (Map.union fields more) tail'
  TRecord more -> TRecord (Map.union fields more)
  tail' -> TOpenRecord fields tail'

-- | Smart constructor for the built-in list type constructor.
tList :: Type -> Type
tList = TApp (TCon "List")

-- | Convert a parsed annotation into a scheme.
--
-- Source annotations may spell polymorphism directly via 'TForall'. Everywhere
-- else in the implementation we store polymorphism in 'Scheme', so this helper
-- is the boundary between those two representations.
schemeFromAnnotation :: Type -> Scheme
schemeFromAnnotation (TForall vars body) = Scheme vars body
schemeFromAnnotation ty = Scheme [] ty

-- | Remove explicit universal quantifiers from a type tree.
--
-- Structural operations such as alias expansion and subtyping compare the body
-- shape rather than a top-level syntactic binder wrapper, but nested
-- polymorphic fields must stay intact so ambient records can expose generic
-- members like `builtins.map`.
eraseForall :: Type -> Type
eraseForall = \case
  TForall _ body -> eraseForall body
  other -> other

freeTypeVars :: Type -> Set Name
freeTypeVars = \case
  TVar name -> Set.singleton name
  TTypeList items -> foldMap freeTypeVars items
  TAny -> Set.empty
  TArrow arrow a b ->
    freeTypeVars a
      <> freeTypeVars (arrowEffects arrow)
      <> maybe id Set.delete (arrowBinder arrow) (freeTypeVars b)
  TRecord fields -> foldMap freeTypeVars fields
  TOpenRecord fields tail' -> foldMap freeTypeVars fields <> freeTypeVars tail'
  TOptional inner -> freeTypeVars inner
  TUnion members -> foldMap freeTypeVars members
  TApp f x -> freeTypeVars f <> freeTypeVars x
  TForall vars body -> freeTypeVars body `Set.difference` Set.fromList vars
  TConditional a b c d -> foldMap freeTypeVars [a, b, c, d]
  TSingleton _ base -> freeTypeVars base
  _ -> Set.empty

-- | Collect unresolved inference metas.
--
-- This is used for occurs checks during unification and when closing a final
-- inferred type into a stable scheme.
freeMetas :: Type -> Set Int
freeMetas = metasWith True

-- | Like 'freeMetas', but ignoring arrow effect rows. Effect variables never
-- make a /value/ type less known, so checks such as \"does this type still
-- contain unknowns?\" use this view.
freeTypeMetas :: Type -> Set Int
freeTypeMetas = metasWith False

metasWith :: Bool -> Type -> Set Int
metasWith effects = go
  where
    go = \case
      TMeta n -> Set.singleton n
      TTypeList items -> foldMap go items
      TAny -> Set.empty
      TArrow arrow a b -> go a <> go b <> (if effects then go (arrowEffects arrow) else Set.empty)
      TRecord fields -> foldMap go fields
      TOpenRecord fields tail' -> foldMap go fields <> go tail'
      TOptional inner -> go inner
      TUnion members -> foldMap go members
      TApp f x -> go f <> go x
      TForall _ body -> go body
      TConditional a b c d -> foldMap go [a, b, c, d]
      TSingleton _ base -> go base
      _ -> Set.empty

-- | Convenience wrapper around 'freeMetas' for schemes.
freeMetasScheme :: Scheme -> Set Int
freeMetasScheme = freeMetas . schemeType

-- | Substitute universally-quantified variables inside a type.
--
-- Alias expansion, conditional-type pattern matching, and scheme
-- instantiation all route through this one operation so they share the same
-- binder-avoidance behavior.
substituteTypeVars :: Map Name Type -> Type -> Type
substituteTypeVars env
  -- Instantiating a monomorphic scheme substitutes nothing, and that is by far
  -- the most common call in the checker. Returning the argument untouched
  -- avoids rebuilding the whole type tree to produce an identical copy.
  | Map.null env = id
  | otherwise = go
  where
    go = \case
      TVar name -> Map.findWithDefault (TVar name) name env
      TTypeList items -> TTypeList (go <$> items)
      TAny -> TAny
      TArrow arrow a b ->
        let codomainEnv = maybe env (`Map.delete` env) (arrowBinder arrow)
         in TArrow arrow{arrowEffects = go (arrowEffects arrow)} (go a) (substituteTypeVars codomainEnv b)
      TRecord fields -> TRecord (fmap go fields)
      TOpenRecord fields tail' -> mkOpenRecord (fmap go fields) (go tail')
      TOptional inner -> TOptional (go inner)
      TUnion members -> TUnion (go <$> members)
      TApp f x -> TApp (go f) (go x)
      TForall vars body -> TForall vars (substituteTypeVars (foldr Map.delete env vars) body)
      TConditional a b c d -> TConditional (go a) (go b) (go c) (go d)
      TSingleton name base -> TSingleton name (go base)
      other -> other

-- | Substitute inference metas with their solved types.
--
-- Unlike 'substituteTypeVars', this walk recursively chases already-solved
-- metas so callers get a normalized view of the inference state.
substituteMetas :: Map Int Type -> Type -> Type
substituteMetas env
  -- `zonk` runs on every inference step, including before any meta has been
  -- solved. With no substitution to apply the walk can only ever rebuild an
  -- identical tree, so skip it.
  | Map.null env = id
  | otherwise = go
  where
    go = \case
      TMeta n -> maybe (TMeta n) go (Map.lookup n env)
      TTypeList items -> TTypeList (go <$> items)
      TAny -> TAny
      TArrow arrow a b -> TArrow arrow{arrowEffects = go (arrowEffects arrow)} (go a) (go b)
      TRecord fields -> TRecord (fmap go fields)
      TOpenRecord fields tail' -> mkOpenRecord (fmap go fields) (go tail')
      TOptional inner -> TOptional (go inner)
      TUnion members -> TUnion (go <$> members)
      TApp f x -> TApp (go f) (go x)
      TForall vars body -> TForall vars (go body)
      TConditional a b c d -> TConditional (go a) (go b) (go c) (go d)
      TSingleton name base -> TSingleton name (go base)
      other -> other

-- | Close remaining metas into a user-visible polymorphic scheme.
--
-- The resulting variable names are synthetic but deterministic (`t0`, `t1`,
-- ...). This mirrors how TypeScript surfaces fresh type variables in tooling
-- even when the source never wrote them explicitly.
--
-- Effect variables that occur only once carry no information (\"this
-- function performs nothing it is not told to\"), so they are closed to the
-- pure row first; the effect variables that remain are named `e0`, `e1`, ...
closeMetas :: Type -> Scheme
closeMetas ty0 =
  let ty = substituteMetas (Map.fromSet (const pureEffects) (loneEffectMetas ty0)) ty0
      metas = sort (Set.toList (freeMetas ty))
      effectsOnly = effectOnlyMetas ty
      (effectMetas, typeMetas) = partitionBy (`Set.member` effectsOnly) metas
      typeVars = [Text.pack ("t" <> show i) | i <- [0 .. length typeMetas - 1]]
      effectVars = [Text.pack ("e" <> show i) | i <- [0 .. length effectMetas - 1]]
      subst = Map.fromList (zip typeMetas (TVar <$> typeVars) <> zip effectMetas (TVar <$> effectVars))
   in Scheme (typeVars <> effectVars) (substituteMetas subst ty)
  where
    partitionBy p xs = (filter p xs, filter (not . p) xs)

-- | Count meta occurrences, split by whether they sit in an arrow's effect
-- row or in an ordinary type position.
metaOccurrences :: Type -> (Map Int Int, Map Int Int)
metaOccurrences = go False
  where
    go inEffect = \case
      TMeta n -> if inEffect then (Map.empty, Map.singleton n 1) else (Map.singleton n 1, Map.empty)
      TArrow arrow a b -> go inEffect a <+> go inEffect b <+> go True (arrowEffects arrow)
      TTypeList items -> foldr ((<+>) . go inEffect) mempty' items
      TRecord fields -> foldr ((<+>) . go inEffect) mempty' (Map.elems fields)
      TOpenRecord fields tail' -> foldr ((<+>) . go inEffect) (go inEffect tail') (Map.elems fields)
      TOptional inner -> go inEffect inner
      TUnion members -> foldr ((<+>) . go inEffect) mempty' members
      TApp f x -> go inEffect f <+> go inEffect x
      TForall _ body -> go inEffect body
      TConditional a b c d -> foldr ((<+>) . go inEffect) mempty' [a, b, c, d]
      TSingleton _ base -> go inEffect base
      _ -> mempty'
    mempty' = (Map.empty, Map.empty)
    (a, b) <+> (c, d) = (Map.unionWith (+) a c, Map.unionWith (+) b d)

-- | Metas that occur exactly once, and only inside an effect row.
loneEffectMetas :: Type -> Set Int
loneEffectMetas ty =
  let (typeCounts, effectCounts) = metaOccurrences ty
   in Map.keysSet (Map.filterWithKey (\n count -> count == 1 && not (Map.member n typeCounts)) effectCounts)

-- | Metas that occur only inside effect rows (effect variables).
effectOnlyMetas :: Type -> Set Int
effectOnlyMetas ty =
  let (typeCounts, effectCounts) = metaOccurrences ty
   in Map.keysSet effectCounts `Set.difference` Map.keysSet typeCounts
