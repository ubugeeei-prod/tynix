-- | Alias expansion and type-pattern utilities.
--
-- This module is the workhorse behind generic aliases, higher-kinded encodings,
-- and TypeScript-style `infer` patterns. The rest of the checker treats these
-- capabilities as structural rewrites over 'Type'.
module Alias
  ( AliasEnv,
    collectApps,
    expandAliases,
    expandAliasHead,
    flattenUnion,
    isOpaqueHead,
    matchPattern,
    mkAliasEnv,
    revealOpaque,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Type

-- | Environment keyed by alias name.
type AliasEnv = Map Name TypeAlias

-- | Build an alias environment from parsed declarations.
--
-- Later declarations with the same name win, which mirrors the simple \"last
-- definition in scope\" strategy used elsewhere in this prototype.
mkAliasEnv :: [TypeAlias] -> AliasEnv
mkAliasEnv = Map.fromList . map (\alias -> (typeAliasName alias, alias))

-- | Normalize nested unions and remove duplicates.
--
-- This keeps joins and alias expansion from producing deeply nested `a | (b |
-- c)` shapes that are annoying for both users and subsequent algorithms.
--
-- A repeated member keeps the position of its /last/ occurrence, which is what
-- callers already depend on for stable rendering. Deduplication runs through a
-- 'Set' rather than a linear membership scan, so a wide union costs
-- @O(n log n)@ instead of @O(n^2)@ — joins over unions of a few hundred
-- members were the dominant cost in the checker before that change.
flattenUnion :: Type -> Type
flattenUnion (TUnion members) =
  case reverse (dedupe Set.empty (reverse (concatMap expand members))) of
    [] -> TUnion []
    [single] -> single
    xs -> TUnion xs
  where
    expand (TUnion xs) = concatMap expand xs
    expand other = [other]
    dedupe _ [] = []
    dedupe seen (item : rest)
      | item `Set.member` seen = dedupe seen rest
      | otherwise = item : dedupe (Set.insert item seen) rest
flattenUnion other = other

-- | Split a left-associated type application into its head and arguments.
--
-- `F A B` is represented as `TApp (TApp F A) B`, while many alias algorithms
-- want the pair `(F, [A, B])`.
collectApps :: Type -> (Type, [Type])
collectApps = go []
  where
    go acc (TApp f x) = go (x : acc) f
    go acc headTy = (headTy, acc)

-- | Maximum number of chained alias expansions performed on any one path.
--
-- The budget exists to stop self-referential aliases from diverging. It counts
-- /expansions/, not structural depth, so an ordinary type stays fully expanded
-- no matter how deeply it nests — the previous depth-per-node accounting
-- silently stopped expanding aliases below 32 levels of nesting and therefore
-- made 'expandAliases' non-idempotent on large types.
aliasExpansionBudget :: Int
aliasExpansionBudget = 32

-- | Expand type aliases up to a fixed recursion budget.
--
-- The implementation is intentionally eager enough to make hovers and emitted
-- declarations useful, while still guarding against runaway self-recursive
-- aliases.
expandAliases :: AliasEnv -> Type -> Type
expandAliases env = goPath Set.empty 0
  where
    -- An alias is not re-expanded below itself: a recursive alias such as
    -- `type A = { next :: A; }` stays a named reference at the recursion
    -- point. Without this, branching recursive aliases expanded
    -- exponentially (to the budget's depth) and hung the checker.
    goPath :: Set.Set Name -> Int -> Type -> Type
    goPath seen depth ty
      | depth > aliasExpansionBudget = ty
      | otherwise =
          case ty of
            TCon name
              | Just alias <- Map.lookup name env,
                not (typeAliasOpaque alias),
                null (typeAliasParams alias),
                not (Set.member name seen) ->
                  goPath (Set.insert name seen) (depth + 1) (typeAliasBody alias)
            TApp f x
              | (TCon name, _) <- collectApps (TApp f x),
                Map.member name env,
                Set.member name seen ->
                  ty
            _ -> goWith seen depth ty

    goWith :: Set.Set Name -> Int -> Type -> Type
    goWith seen depth ty =
      let go = goPath seen
          reduce = reduceWith seen
       in case ty of
            TTypeList items -> TTypeList (map (go depth) items)
            TArrow arrow a b -> TArrow arrow{arrowEffects = go depth (arrowEffects arrow)} (go depth a) (go depth b)
            TRecord fields -> TRecord (fmap (go depth) fields)
            TOpenRecord fields tail' -> mkOpenRecord (fmap (go depth) fields) (go depth tail')
            TOptional inner -> TOptional (go depth inner)
            TUnion members -> flattenUnion (TUnion (map (go depth) members))
            TApp f x -> reduce depth (go depth f) (go depth x)
            TForall vars body -> TForall vars (go depth body)
            TConditional a b c d -> TConditional (go depth a) (go depth b) (go depth c) (go depth d)
            other -> other

    reduceWith :: Set.Set Name -> Int -> Type -> Type -> Type
    reduceWith seen depth f x =
      case collectApps (TApp f x) of
        (TCon name, args)
          | Just alias <- Map.lookup name env,
            not (typeAliasOpaque alias),
            length args >= length (typeAliasParams alias),
            not (Set.member name seen) ->
              let (used, rest) = splitAt (length (typeAliasParams alias)) args
                  subst = Map.fromList (zip (typeAliasParams alias) used)
                  body = substituteTypeVars subst (typeAliasBody alias)
               in foldl TApp (goPath (Set.insert name seen) (depth + 1) body) rest
        (headTy, args) -> foldl TApp headTy args

-- | Expand aliases at the head of a type only, leaving nested types (record
-- fields, function arguments, ...) as written. Consumers that walk a type
-- resolve each level as they reach it, so large or recursive aliases are
-- expanded lazily and display with their names.
expandAliasHead :: AliasEnv -> Type -> Type
expandAliasHead env = go 0
  where
    go :: Int -> Type -> Type
    go depth ty
      | depth > aliasExpansionBudget = ty
      | otherwise =
          case ty of
            TCon name
              | Just alias <- Map.lookup name env,
                not (typeAliasOpaque alias),
                null (typeAliasParams alias) ->
                  go (depth + 1) (typeAliasBody alias)
            TApp _ _
              | (TCon name, args) <- collectApps ty,
                Just alias <- Map.lookup name env,
                not (typeAliasOpaque alias),
                length args >= length (typeAliasParams alias) ->
                  let (used, rest) = splitAt (length (typeAliasParams alias)) args
                      subst = Map.fromList (zip (typeAliasParams alias) used)
                   in go (depth + 1) (foldl TApp (substituteTypeVars subst (typeAliasBody alias)) rest)
            _ -> ty

-- | Whether a type is headed by an `opaque type` (fully applied or not).
isOpaqueHead :: AliasEnv -> Type -> Bool
isOpaqueHead env ty =
  case collectApps ty of
    (TCon name, _) -> maybe False typeAliasOpaque (Map.lookup name env)
    _ -> False

-- | Unfold one opaque type at the head into its representation. This is the
-- only place an opaque type is looked through: explicit `as` casts use it to
-- move between a nominal type and the type it is built from.
revealOpaque :: AliasEnv -> Type -> Maybe Type
revealOpaque env ty =
  case collectApps ty of
    (TCon name, args)
      | Just alias <- Map.lookup name env,
        typeAliasOpaque alias,
        length args == length (typeAliasParams alias) ->
          Just (substituteTypeVars (Map.fromList (zip (typeAliasParams alias) args)) (typeAliasBody alias))
    _ -> Nothing

-- | Match a concrete type against a pattern containing 'TInfer' placeholders.
--
-- This is the structural heart of conditional types: when a pattern matches, we
-- recover the inferred bindings and substitute them into the `true` branch.
matchPattern :: Type -> Type -> Maybe (Map Name Type)
matchPattern = go Map.empty
  where
    go env value (TInfer name) =
      case Map.lookup name env of
        Nothing -> Just (Map.insert name value env)
        Just prev | prev == value -> Just env
        _ -> Nothing
    go env (TFun actualMult a b) (TFun patternMult c d)
      | actualMult == patternMult = go env a c >>= \env' -> go env' b d
      | otherwise = Nothing
    go env (TRecord a) (TRecord b) = foldl step (Just env) (Map.toList b)
      where
        step acc (name, ty) = do
          env' <- acc
          actualTy <- Map.lookup name a
          go env' actualTy ty
    go env (TTypeList actualItems) (TTypeList patternItems)
      | length actualItems == length patternItems =
          foldl
            (\acc (actualTy, nextPatternTy) -> acc >>= \env' -> go env' actualTy nextPatternTy)
            (Just env)
            (zip actualItems patternItems)
      | otherwise = Nothing
    go env (TApp a b) (TApp c d) = go env a c >>= \env' -> go env' b d
    go env value other | value == other = Just env
    go _ _ _ = Nothing
