{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Directional constraints, structural unification, and gradual compatibility.
module Check.Constraints
  ( constrain,
    unify,
    checkCast,
    widenLiterals,
    isMeta,
    sequenceListView,
    hasUnresolvedMetas,
    hasDynamic,
  )
where

import Alias
import Check.Diagnostics
import Check.Effects (constrainEffects, unifyEffects)
import Check.Inference
import Control.Monad (forM_, unless, void, zipWithM)
import Data.Functor (($>), (<&>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Diagnostics (DiagnosticCode (..), withCode)
import Indexed
import Subtyping
import Type

-- | Check that an inferred type satisfies an expected type.
--
-- Compared with `unify`, `constrain` is intentionally directional. It is used
-- for user annotations and function arguments where one side represents an
-- obligation rather than an unknown peer.
--
-- A few policy choices are important here:
--
-- * exact subtyping succeeds immediately,
-- * gradual consistency is only accepted when `dynamic` participates,
-- * plain concrete mismatches do /not/ fall through to permissive unification,
-- * sequence types may compare through their structural `List` view when one
--   side explicitly asks for `List`.
--
-- Representative examples:
--
-- @
-- constrain (Vec 2 Int) (List Int)
--   => succeeds through the structural list view
--
-- constrain (Vec 3 Int) (Vec (2 | Range 4 8 Nat) Int)
--   => fails
--
-- constrain dynamic String
--   => succeeds, because the mismatch is genuinely gradual
-- @
constrain :: CheckContext -> Type -> Type -> InferM Type
constrain ctx actualRaw expected = do
  -- A polymorphic value may be used at any instance (a `rec` field such as
  -- `id :: forall t0. t0 -> t0` meeting `Int -> Int`).
  actual <- zonkHead actualRaw >>= instantiateRank
  decomposed <- constrainStructurally ctx actual expected
  case decomposed of
    Just result -> pure result
    Nothing -> constrainResolved ctx actual expected

-- | Decompose a constraint along the *unzonked* structure of the expected
-- type, so a meta inside it (say the `a` of `List a`) is still identifiable
-- when it is reached:
--
-- * `List`/`AttrsOf` are covariant: `List x <: List y` is `x <: y`, and an
--   exact sequence (`Vec`/`Tuple`) meets `List y` through its element join;
-- * a union on the left must fit member-wise, one on the right is tried
--   member by member while metas remain;
-- * a meta already solved to a literal (from an earlier argument) widens to
--   the join when another literal of the same family arrives, so
--   `max 1 2` and `elem 1 [ 1 2 ]` check.
constrainStructurally :: CheckContext -> Type -> Type -> InferM (Maybe Type)
constrainStructurally ctx actual expected = do
  let aliases = checkAliases ctx
  actualHead <- normalizeIndexedType . resolveHead aliases <$> zonkHead actual
  expectedHead <- resolveHead aliases <$> zonkHead expected
  bound <- boundMeta expected
  actualFull <- zonk actual
  case (actualHead, expectedHead) of
    _
      | Just (n, solved) <- bound,
        literalish solved,
        literalish actualFull,
        sameLiteralFamily solved actualFull,
        not (isSubtype aliases actualFull solved) -> do
          let joined = joinTypes aliases solved actualFull
          replaceMetaSolution n joined
          pure (Just joined)
    (TApp (TCon c) x, TApp (TCon c') y)
      | c == c',
        c `elem` ["List", "AttrsOf"] ->
          Just . TApp (TCon c) <$> constrain ctx x y
    (_, TApp (TCon "List") y)
      | Just listTy <- sequenceListView actualHead,
        TApp (TCon "List") x <- listTy ->
          Just . tList <$> constrain ctx x y
    -- (An unsolved meta on the right simply takes the whole union.)
    (TUnion members, _)
      | not (isMeta expectedHead),
        hasUnresolvedMetas actualFull expectedHead || hasMetaInside expected -> do
          forM_ members (\member -> constrain ctx member expected)
          Just <$> zonk expected
    (_, TUnion members)
      | hasUnresolvedMetas actualFull actualFull -> firstSuccess members
    _ -> pure Nothing
  where
    firstSuccess [] = pure Nothing
    firstSuccess (member : rest) = do
      attempt <- catchInfer (constrain ctx actual member)
      case attempt of
        Right _ -> Just <$> zonk expected
        Left _ -> firstSuccess rest
    hasMetaInside ty = not (Set.null (freeTypeMetas ty))

-- | Literal singletons and unions of them.
literalish :: Type -> Bool
literalish = \case
  TLit _ -> True
  TUnion members -> all literalish members
  _ -> False

sameLiteralFamily :: Type -> Type -> Bool
sameLiteralFamily a b = widenLiterals a == widenLiterals b

constrainResolved :: CheckContext -> Type -> Type -> InferM Type
constrainResolved ctx actual expected = do
  -- Aliases are expanded lazily, so resolve each side's head before
  -- comparing structure (`Mapper a b` must meet `Int -> Int` as a function).
  actualZ <- zonk actual
  expectedZ <- zonk expected
  let actual' = normalizeIndexedType (resolveHead (checkAliases ctx) actualZ)
      expected' = normalizeIndexedType (resolveHead (checkAliases ctx) expectedZ)
  case (actual', expected') of
    _
      | Just actualList <- sequenceListView actual',
        isPlainListType expected' ->
          constrain ctx actualList expected'
      | isPlainListType actual',
        Just expectedList <- sequenceListView expected' ->
          constrain ctx actual' expectedList
    (TMeta n, TMeta m) | n == m -> pure actual'
    -- Passing a still-unknown value where *anything* (or any attribute set)
    -- is accepted must not pin it to that top type: `builtins.hasAttr k x`
    -- says nothing about the rest of `x`.
    (TMeta _, TUnknown) -> pure expected'
    (TMeta n, TApp (TCon "AttrsOf") TUnknown) -> do
      rowTail <- freshMeta
      _ <- bindMeta n (TOpenRecord Map.empty rowTail)
      pure expected'
    -- Metas are solved to the unresolved type so alias names survive.
    (TMeta n, _) -> bindMeta n expectedZ
    (_, TMeta n) -> bindMeta n actualZ
    (TTypeList xs, TTypeList ys)
      | length xs == length ys ->
          TTypeList <$> zipWithM (constrain ctx) xs ys
    (TArrow actualArrow actualArg actualResult, TArrow expectedArrow expectedArg expectedResult)
      | multiplicitySubtype (arrowMult actualArrow) (arrowMult expectedArrow) -> do
          constrainEffects (arrowEffects actualArrow) (arrowEffects expectedArrow)
          unless (capturesWithin (arrowCaptures actualArrow) (arrowCaptures expectedArrow)) $
            throwCheck (withCode TC0025CaptureNotAllowed ("closure captures more than its expected type allows: " <> showType actual' <> " vs " <> showType expected'))
          _ <- constrain ctx expectedArg actualArg
          -- Dependent binders on either side denote the same argument.
          let shared = TSingleton "it" expectedArg
          _ <- constrain ctx (eraseBinder actualArrow shared actualResult) (eraseBinder expectedArrow shared expectedResult)
          zonk expected'
      | otherwise ->
          throwCheck (withCode TC0026LinearityViolation ("expected a linear function (`%1 ->`), but got an unrestricted one: " <> showType actual'))
    (TOptional a, TOptional b) -> TOptional <$> constrain ctx a b
    _
      | hasUnresolvedMetas actual' expected',
        Just (actualFields, actualTail) <- recordView (resolveHead (checkAliases ctx) actual'),
        Just (expectedFields, expectedTail) <- recordView (resolveHead (checkAliases ctx) expected') -> do
          constrainRecord ctx actualFields actualTail expectedFields
          -- An open expected row (`{ ...r }` from a signature) captures the
          -- fields the expectation did not mention, so `r` is solved.
          -- When both sides already share that row (a record meeting
          -- itself through an instantiated signature), there is nothing to
          -- capture, and binding it would only build an infinite row.
          case expectedTail of
            Just (TMeta n)
              | actualTail /= Just (TMeta n) -> do
                  let extra = Map.difference actualFields expectedFields
                  _ <- bindMeta n (maybe (TRecord extra) (mkOpenRecord extra) actualTail)
                  pure ()
            _ -> pure ()
          zonk expected'
      | hasUnresolvedMetas actual' expected',
        Just (actualFields, _) <- recordView (resolveHead (checkAliases ctx) actual'),
        Just valueTy <- attrsOfView (resolveHead (checkAliases ctx) expected') ->
          -- A record used as a dictionary: its values must fit together, so
          -- their join (not the first field) determines the value type.
          case map unOptional (Map.elems actualFields) of
            [] -> pure expected'
            x : xs -> do
              first <- zonk x
              rest <- traverse zonk xs
              let joined = foldRight1 (joinTypes (checkAliases ctx)) first rest
              constrain ctx (widenLiterals joined) valueTy $> expected'
    _ | actual' == expected' -> pure expected'
    _ | isSubtype (checkAliases ctx) actual' expected' -> pure expected'
    _ | allowsGradualConsistency actual' expected' && isConsistent (checkAliases ctx) actual' expected' -> pure expected'
    _ | hasUnresolvedMetas actual' expected' -> unify ctx actual' expected'
    _
      | Just detail <- recordMismatchDetail (checkAliases ctx) actual' expected' -> throwCheck detail
    _ -> throwCheck (withCode TC0013TypeMismatch ("type mismatch: " <> showType actual' <> " vs " <> showType expected'))

-- | Explain why one record does not satisfy another: the first missing
-- required field, or the first field whose type does not fit.
recordMismatchDetail :: AliasEnv -> Type -> Type -> Maybe String
recordMismatchDetail aliases actual expected = do
  (actualFields, actualTail) <- recordView (resolveHead aliases actual)
  (expectedFields, _) <- recordView (resolveHead aliases expected)
  let open = actualTail == Just tDynamic || actualTail == Just tAny
      missing =
        [ name
        | (name, ty) <- Map.toList expectedFields,
          not (isOptionalField ty),
          not (Map.member name actualFields),
          not open
        ]
      wrong =
        [ (name, unOptional a, unOptional e)
        | (name, e) <- Map.toList expectedFields,
          Just a <- [Map.lookup name actualFields],
          not (isSubtype aliases (unOptional a) (unOptional e))
        ]
  case (missing, wrong) of
    (name : _, _) ->
      Just (withCode TC0009MissingField ("missing field " <> quoteName name <> ": expected " <> showType expected <> " but got " <> showType actual))
    ([], (name, a, e) : _) ->
      Just (withCode TC0013TypeMismatch ("type mismatch in field " <> quoteName name <> ": " <> showType a <> " vs " <> showType e))
    _ -> Nothing
  where
    isOptionalField = \case
      TOptional _ -> True
      _ -> False

-- | Width-subtyping obligation between record shapes that still contain
-- metas: every expected field must be provided (unless optional). A missing
-- field extends the actual row when its tail is still open.
constrainRecord :: CheckContext -> Map Name Type -> Maybe Type -> Map Name Type -> InferM ()
constrainRecord ctx actualFields actualTail expectedFields =
  forM_ (Map.toList expectedFields) $ \(name, expectedTy) ->
    case Map.lookup name actualFields of
      Just actualTy -> void (constrain ctx (unOptional actualTy) (unOptional expectedTy))
      Nothing ->
        case expectedTy of
          TOptional _ -> pure ()
          _ -> do
            tail' <- traverse zonk actualTail
            case tail' of
              Just (TMeta n) -> do
                rowTail <- freshMeta
                void (bindMeta n (TOpenRecord (Map.singleton name expectedTy) rowTail))
              Just ty | ty == tDynamic || ty == tAny -> pure ()
              _ -> throwCheck (withCode TC0009MissingField ("missing field " <> quoteName name <> " required by " <> showType (TRecord expectedFields)))

-- | Symmetric structural unification used while solving metas.
--
-- Unlike `constrain`, both sides are treated as peers here. When metas are
-- present the function may bind them, recursively unify structured types, or
-- join gradually consistent shapes when `dynamic` is involved.
--
-- Representative examples:
--
-- @
-- unify ?0 Int
--   => binds ?0 := Int
--
-- unify (List ?0) (List String)
--   => binds ?0 := String
--
-- unify Int String
--   => fails
-- @
unify :: CheckContext -> Type -> Type -> InferM Type
unify ctx left right = do
  left' <- normalizeIndexedType . resolveHead (checkAliases ctx) <$> zonk left
  right' <- normalizeIndexedType . resolveHead (checkAliases ctx) <$> zonk right
  case (left', right') of
    _
      | Just leftList <- sequenceListView left',
        isPlainListType right' ->
          unify ctx leftList right'
      | isPlainListType left',
        Just rightList <- sequenceListView right' ->
          unify ctx left' rightList
    (TMeta n, TMeta m) | n == m -> pure left'
    (TMeta n, ty) -> bindMeta n ty
    (ty, TMeta n) -> bindMeta n ty
    (TTypeList xs, TTypeList ys)
      | length xs == length ys ->
          TTypeList <$> zipWithM (unify ctx) xs ys
    -- Two exact sequence shapes that disagree (a 1-element and a 3-element
    -- list literal, say) meet at their common `List` view.
    _
      | Just leftList <- sequenceListView left',
        Just rightList <- sequenceListView right',
        hasUnresolvedMetas left' right' || not (isSubtype (checkAliases ctx) left' right' || isSubtype (checkAliases ctx) right' left') ->
          unify ctx leftList rightList
    (TArrow leftArrow a b, TArrow rightArrow c d)
      | arrowMult leftArrow == arrowMult rightArrow -> do
          effects <- unifyEffects (arrowEffects leftArrow) (arrowEffects rightArrow)
          dom <- unify ctx a c
          cod <- unify ctx (eraseBinder leftArrow dom b) (eraseBinder rightArrow dom d)
          pure (TArrow leftArrow{arrowEffects = effects, arrowBinder = Nothing} dom cod)
    (TRecord a, TRecord b) -> unifyRecord a b
    _
      | Just (a, ta) <- recordView left',
        Just (b, tb) <- recordView right',
        isJust ta || isJust tb ->
          unifyRows left' right' a ta b tb
    (TOptional a, TOptional b) -> TOptional <$> unify ctx a b
    (TApp f x, TApp g y) -> TApp <$> unify ctx f g <*> unify ctx x y
    _ | left' == right' -> pure left'
    _ | isSubtype (checkAliases ctx) left' right' -> pure right'
    _ | isSubtype (checkAliases ctx) right' left' -> pure left'
    _ | allowsGradualConsistency left' right' && isConsistent (checkAliases ctx) left' right' -> pure (joinTypes (checkAliases ctx) left' right')
    _ -> throwCheck (withCode TC0013TypeMismatch ("type mismatch: " <> showType left' <> " vs " <> showType right'))
  where
    -- Rows unify field-wise on their common labels; each side's tail absorbs
    -- the labels only the other side has, sharing one fresh rest-row.
    unifyRows leftTy rightTy a ta b tb = do
      sequence_ (Map.intersectionWith (unify ctx) a b)
      let onlyA = Map.difference a b
          onlyB = Map.difference b a
      case (ta, tb) of
        (Nothing, Just tailB)
          | Map.null onlyB -> unify ctx tailB (TRecord onlyA) *> zonk leftTy
        (Just tailA, Nothing)
          | Map.null onlyA -> unify ctx tailA (TRecord onlyB) *> zonk rightTy
        (Just tailA, Just tailB) -> do
          rest <- freshMeta
          _ <- unify ctx tailA (mkOpenRecord onlyB rest)
          _ <- unify ctx tailB (mkOpenRecord onlyA rest)
          zonk leftTy
        _ -> throwCheck (withCode TC0014RecordMismatch ("record mismatch: " <> showType leftTy <> " vs " <> showType rightTy))
    unifyRecord a b
      | Map.keysSet b `Set.isSubsetOf` Map.keysSet a =
          Map.traverseWithKey (\name bTy -> maybe (pure bTy) (\aTy -> unify ctx aTy bTy) (Map.lookup name a)) b
            <&> TRecord
      | Map.keysSet a `Set.isSubsetOf` Map.keysSet b =
          Map.traverseWithKey (\name aTy -> maybe (pure aTy) (unify ctx aTy) (Map.lookup name b)) a
            <&> TRecord
      | otherwise = throwCheck (withCode TC0014RecordMismatch ("record mismatch: " <> showRecord a <> " vs " <> showRecord b))

-- | Validate an explicit `expr as Type` assertion.
--
-- Casts deliberately live between plain assignment and fully-unsound escape
-- hatches. They are accepted when the two sides already overlap structurally,
-- when a gradual boundary such as `any`, `unknown`, or `dynamic` connects
-- them, or when the cast still contains unresolved inference metas that can be
-- solved by unification.
--
-- Representative examples:
--
-- @
-- { value = 1; } as { value :: Int; }
--   => accepted
--
-- import ./opaque.nix as { value :: String; }
--   => accepted when the import is `dynamic`
--
-- 1 as String
--   => rejected
-- @
checkCast :: CheckContext -> Type -> Type -> InferM Type
checkCast ctx actual expected = do
  -- Aliases are expanded lazily, so resolve each side's head before
  -- comparing structure (`Mapper a b` must meet `Int -> Int` as a function).
  actual' <- normalizeIndexedType . resolveHead (checkAliases ctx) <$> zonk actual
  expected' <- normalizeIndexedType . resolveHead (checkAliases ctx) <$> zonk expected
  let aliases = checkAliases ctx
  -- An opaque type is converted to and from its representation only here.
  let reveal ty = fromMaybe ty (revealOpaque aliases ty)
      related a b = isSubtype aliases a b || isSubtype aliases b a || isConsistent aliases a b
  if hasUnresolvedMetas actual' expected'
    then unify ctx (reveal actual') (reveal expected') $> expected
    else
      if related actual' expected'
        || related (reveal actual') expected'
        || related actual' (reveal expected')
        then pure expected
        else throwCheck (withCode TC0015InvalidCast ("invalid cast: " <> showType actual' <> " as " <> showType expected'))

-- | Replace singleton literal types by their primitive base.
widenLiterals :: Type -> Type
widenLiterals = \case
  TLit (LInt _) -> tInt
  TLit (LFloat _) -> tFloat
  TLit (LString _) -> tString
  TLit (LBool _) -> tBool
  TRecord fields -> TRecord (fmap widenLiterals fields)
  TOpenRecord fields tail' -> TOpenRecord (fmap widenLiterals fields) tail'
  TOptional inner -> TOptional (widenLiterals inner)
  TArrow arrow a b -> TArrow arrow (widenLiterals a) (widenLiterals b)
  TApp f x -> TApp f (widenLiterals x)
  TUnion members -> case nubOrdTypes (map widenLiterals members) of
    [single] -> single
    many' -> TUnion many'
  other -> other
  where
    nubOrdTypes = Set.toList . Set.fromList

isMeta :: Type -> Bool
isMeta = \case
  TMeta _ -> True
  _ -> False

multiplicitySubtype :: Multiplicity -> Multiplicity -> Bool
multiplicitySubtype actual expected =
  actual == expected
    || case (actual, expected) of
      (One, Many) -> True
      _ -> False

-- | Widen a precise tuple/tensor into its structural list view when possible.
--
-- This is the bridge that lets exact sequence types interact with explicit
-- `List` annotations and ambient declarations.
sequenceListView :: Type -> Maybe Type
sequenceListView ty =
  case tensorListView ty of
    Just listTy -> Just listTy
    Nothing -> tupleListView ty

-- | Recognize the plain built-in `List a` shape.
isPlainListType :: Type -> Bool
isPlainListType ty =
  case collectApps ty of
    (TCon "List", [_]) -> True
    _ -> False

-- | Decide whether a consistency-based escape hatch is allowed.
--
-- Gradual consistency exists to smooth interop with genuinely dynamic values,
-- not to silently blur two unrelated concrete types.
allowsGradualConsistency :: Type -> Type -> Bool
allowsGradualConsistency left right = hasDynamic left || hasDynamic right

-- | Check whether either side still contains unsolved metas.
hasUnresolvedMetas :: Type -> Type -> Bool
hasUnresolvedMetas left right = not (Set.null (freeTypeMetas left <> freeTypeMetas right))

-- | Detect whether a type tree mentions `dynamic` anywhere inside it.
hasDynamic :: Type -> Bool
hasDynamic = \case
  TDynamic -> True
  TTypeList items -> any hasDynamic items
  TFun _ left right -> hasDynamic left || hasDynamic right
  TRecord fields -> any hasDynamic fields
  TOpenRecord fields tail' -> any hasDynamic fields || hasDynamic tail'
  TOptional inner -> hasDynamic inner
  TUnion members -> any hasDynamic members
  TApp fun arg -> hasDynamic fun || hasDynamic arg
  TForall _ body -> hasDynamic body
  TConditional actual patternTy yesTy noTy ->
    any hasDynamic [actual, patternTy, yesTy, noTy]
  _ -> False
