{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Structural subtyping, consistency, and type reduction.
--
-- tynix prioritizes incremental adoption over maximum strictness. This module is
-- where that policy becomes concrete: `dynamic` participates through
-- consistency, records use width subtyping, and conditional types are reduced
-- structurally.
--
-- The key design tension in this module is "preserve as much information as we
-- can, but stay usable for ordinary Nix code". That leads to a few notable
-- choices:
--
-- * `dynamic` participates through consistency rather than ordinary subtyping.
-- * exact list shapes are preserved through `Vec`/`Matrix`/`Tensor` when the
--   source proves them,
-- * structural `List` views are still available when a consumer does not care
--   about exact shapes,
-- * numeric refinements such as `Range 0 10 Nat` and unit wrappers such as
--   `Unit "ms" (...)` are interpreted structurally instead of elaborated into
--   runtime contracts.
--
-- As a result, the relation implemented here is intentionally not a proof
-- system for dependent types. It is a pragmatic static approximation that keeps
-- useful facts alive for later phases.
module Subtyping
  ( attrsOfView,
    capturesWithin,
    effectsWithin,
    eraseBinder,
    foldRight1,
    isConsistent,
    isSubtype,
    joinTypes,
    lookupRecordField,
    recordView,
    reduceOperators,
    resolveHead,
    resolveType,
    unOptional,
  )
where

import Alias
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Set qualified as Set
import Indexed
import Type

-- | Reduce aliases, erase top-level `forall`, and evaluate conditional types.
--
-- Most higher-level algorithms call this before comparing types so they see a
-- normalized structural view instead of the user-written surface form.
--
-- The reduction performs a bounded fixed-point walk. That bound prevents alias
-- cycles or accidentally self-referential conditional types from diverging
-- forever while still letting ordinary nested aliases expand naturally.
--
-- Representative examples:
--
-- @
-- resolveType env (Box Int)
--   => { value :: Int; }
--
-- resolveType env (Matrix 2 3 Int)
--   => Tensor [2 3] Int
--
-- resolveType env (List Int extends List (infer a) ? a : dynamic)
--   => Int
-- @
resolveType :: AliasEnv -> Type -> Type
resolveType env = go 0 . prepare . eraseForall
  where
    -- Alias expansion and indexed normalization are both whole-tree walks, so
    -- running them once up front leaves nothing for the structural pass below
    -- to re-expand. Only a reduced conditional can introduce fresh aliases,
    -- and that branch re-prepares explicitly.
    prepare :: Type -> Type
    prepare = normalizeIndexedType . expandAliases env

    go :: Int -> Type -> Type
    go depth ty
      | depth > conditionalReductionBudget = ty
      | otherwise =
          case ty of
            TTypeList items -> TTypeList (map (go depth) items)
            TArrow arrow a b -> TArrow arrow (go depth a) (go depth b)
            TRecord fields -> TRecord (fmap (go depth) fields)
            TOpenRecord fields tail' -> mkOpenRecord (fmap (go depth) fields) (go depth tail')
            TOptional inner -> TOptional (go depth inner)
            TUnion members -> flattenUnion (TUnion (map (go depth) members))
            TApp f x ->
              let app = TApp (go depth f) (go depth x)
               in maybe app (go (depth + 1) . prepare) (reduceTypeOperator env app)
            TForall vars body -> TForall vars (go depth body)
            TConditional a b c d ->
              case matchPattern (go (depth + 1) a) (go (depth + 1) b) of
                Just subst -> go (depth + 1) (prepare (substituteTypeVars subst c))
                Nothing ->
                  if isSubtype env a b
                    then go (depth + 1) c
                    else go (depth + 1) d
            other -> other

-- | Resolve only the outermost structure of a type: expand head aliases,
-- erase a top-level `forall`, normalize indexed constructors, reduce a head
-- conditional, and resolve union members' heads. Nested types are left as
-- written, so callers that walk a type resolve each level lazily; field types
-- keep their alias names for display.
resolveHead :: AliasEnv -> Type -> Type
resolveHead env = go 0
  where
    go :: Int -> Type -> Type
    go depth ty
      | depth > conditionalReductionBudget = ty
      | otherwise =
          case expandAliasHead env (eraseForall ty) of
            TConditional a b c d ->
              let a' = resolveType env a
                  b' = resolveType env b
               in case matchPattern a' b' of
                    Just subst -> go (depth + 1) (substituteTypeVars subst c)
                    Nothing
                      | isSubtype env a b -> go (depth + 1) c
                      | otherwise -> go (depth + 1) d
            TUnion members -> flattenUnion (TUnion (map (go depth) members))
            app@(TApp _ _)
              | Just reduced <- reduceTypeOperator env app -> go (depth + 1) reduced
              | otherwise -> normalizeIndexedType app
            other -> other

-- | Reduce a built-in type-level operator applied to known arguments.
--
-- These are the computations dependent signatures rely on once a singleton
-- argument has been substituted into the codomain:
--
-- * @Get r k@ — the type of field @k@ (a string literal, or a union of them)
--   in record @r@, like TypeScript's indexed access @r[k]@;
-- * @KeyOf r@ — the union of a closed record's field names;
-- * @Add@ / @Sub@ / @Mul@ — arithmetic on integer literals;
-- * @Length xs@ — the length of an exact sequence (@Vec n a@, a tuple).
--
-- An operator whose arguments are not known enough stays unreduced; one whose
-- arguments are known but too wide degrades to the widest sensible answer
-- (@Add Int Int@ is @Int@).
reduceTypeOperator :: AliasEnv -> Type -> Maybe Type
reduceTypeOperator env ty =
  case collectApps ty of
    (TCon "Get", [recordTy, keyTy]) -> do
      let record' = resolveHead env recordTy
          key' = resolveHead env keyTy
      if record' == tDynamic || record' == tAny
        then Just record'
        else case stringKeys key' of
          Just keys -> do
            fieldTys <- traverse (lookupRecordField env record') keys
            case fieldTys of
              x : xs -> Just (foldRight1 (joinTypes env) x xs)
              [] -> Nothing
          Nothing
            | symbolic key' -> Nothing
            -- A known but wide key (`String`) may name any field: a
            -- dictionary's value type, the join of a closed record's
            -- fields, or `dynamic` when the fields are not all known.
            | Just valueTy <- attrsOfView record' -> Just valueTy
            | TRecord fields <- record',
              x : xs <- map unOptional (Map.elems fields) ->
                Just (foldRight1 (joinTypes env) x xs)
            | isSubtype env key' tString -> Just tDynamic
            | otherwise -> Nothing
    (TCon "KeyOf", [recordTy]) ->
      case resolveHead env recordTy of
        TRecord fields -> Just (unionOf [TLit (LString name) | name <- Map.keys fields])
        TOpenRecord _ _ -> Just tString
        resolved | isJust (attrsOfView resolved) -> Just tString
        _ -> Nothing
    (TCon op, [a, b])
      | Just apply <- lookup op arithmetic ->
          case (resolveHead env a, resolveHead env b) of
            (TLit (LInt x), TLit (LInt y)) -> Just (TLit (LInt (apply x y)))
            (a', b')
              | symbolic a' || symbolic b' -> Nothing
              | isSubtype env a' tInt && isSubtype env b' tInt -> Just (if op == "Sub" || not (isSubtype env a' tNat && isSubtype env b' tNat) then tInt else tNat)
              | otherwise -> Just tNumber
    (TCon "Length", [seqTy]) ->
      let seq' = normalizeIndexedType (resolveHead env seqTy)
       in case (tensorView seq', tupleView seq') of
            (Just (lenTy : _, _), _) -> Just lenTy
            (_, Just items) -> Just (TLit (LInt (fromIntegral (length items))))
            _ | isJust (listView seq') || isListApp seq' -> Just tNat
            _ -> Nothing
    _ -> Nothing
  where
    arithmetic = [("Add", (+)), ("Sub", (-)), ("Mul", (*))]
    stringKeys = \case
      TLit (LString name) -> Just [name]
      TUnion members -> concat <$> traverse stringKeys members
      _ -> Nothing
    unionOf = \case
      [single] -> single
      members -> TUnion members
    symbolic = \case
      TVar _ -> True
      TMeta _ -> True
      TSingleton _ _ -> True
      TApp _ _ -> True
      _ -> False
    isListApp = \case
      TApp (TCon "List") _ -> True
      _ -> False

-- | Reduce every built-in type-level operator in a type whose arguments are
-- known, bottom-up. Used on the codomain of a dependent arrow right after its
-- binder has been instantiated, so results display reduced (`Vec 3 String`,
-- not `Vec (Add 1 2) String`).
reduceOperators :: AliasEnv -> Type -> Type
reduceOperators env = go
  where
    go = \case
      TApp f x ->
        let app = TApp (go f) (go x)
         in maybe app go (reduceTypeOperator env app)
      TArrow arrow a b -> TArrow arrow (go a) (go b)
      TTypeList items -> TTypeList (map go items)
      TRecord fields -> TRecord (fmap go fields)
      TOpenRecord fields tail' -> mkOpenRecord (fmap go fields) (go tail')
      TOptional inner -> TOptional (go inner)
      TUnion members -> flattenUnion (TUnion (map go members))
      other -> other

-- | Erase an arrow's dependency: the binder, if any, is replaced in the
-- codomain by @replacement@ (usually the domain, or a shared singleton).
eraseBinder :: Arrow -> Type -> Type -> Type
eraseBinder arrow replacement cod =
  case arrowBinder arrow of
    Just name -> substituteTypeVars (Map.singleton name replacement) cod
    Nothing -> cod

-- | Whether an actual latent effect row fits an expected one.
--
-- Untracked rows ('TDynamic') fit anything in either direction, which is what
-- keeps unannotated code gradual. Otherwise every actual label must be listed
-- by the expectation (or absorbed by an unknown tail), and an actual effect
-- variable must be the expectation's own variable.
effectsWithin :: Type -> Type -> Bool
effectsWithin actual expected
  | actual == TDynamic || expected == TDynamic = True
  | otherwise =
      let (actualLabels, actualTail) = rowParts actual
          (expectedLabels, expectedTail) = rowParts expected
          absorbs = case expectedTail of
            Just (TMeta _) -> True
            Just TDynamic -> True
            _ -> False
          labelsOk = absorbs || all (`elem` expectedLabels) actualLabels
          tailOk = case actualTail of
            Nothing -> True
            Just (TMeta _) -> True
            Just t -> absorbs || expectedTail == Just t
       in labelsOk && tailOk
  where
    rowParts = \case
      TRecord fields -> (Map.keys fields, Nothing)
      TOpenRecord fields tail' -> (Map.keys fields, Just tail')
      other -> ([], Just other)

-- | Whether a closure's captured capabilities fit an expected capture set.
-- An untracked set on either side is the gradual case and always fits.
capturesWithin :: Maybe [Name] -> Maybe [Name] -> Bool
capturesWithin (Just actual) (Just expected) = all (`elem` expected) actual
capturesWithin _ _ = True

-- | Maximum number of chained conditional-type reductions.
--
-- Reducing a conditional can substitute into a branch that reduces again, so
-- the walk needs a budget to stay total on accidentally self-referential
-- conditional aliases. Ordinary structural descent is not counted: the type
-- tree is finite, so recursing into it always terminates on its own.
conditionalReductionBudget :: Int
conditionalReductionBudget = 32

-- | Resolve a field selection against a record-like type.
--
-- Union members are searched left-to-right and the first matching field is
-- returned. This is intentionally permissive to preserve gradual adoption.
--
-- A field lookup on a union only succeeds when every member contributes the
-- field. When that is true, the resulting field type is joined across members.
-- When one member omits the field, the entire lookup fails so callers do not
-- accidentally treat a partial record union as total.
--
-- Representative examples:
--
-- @
-- lookupRecordField ({ value :: 1; } | { value :: String; }) "value"
--   => Just (1 | String)
--
-- lookupRecordField ({ value :: Int; } | { other :: String; }) "value"
--   => Nothing
-- @
lookupRecordField :: AliasEnv -> Type -> Name -> Maybe Type
lookupRecordField env ty field =
  case resolveHead env ty of
    TRecord fields -> unOptional <$> Map.lookup field fields
    TOpenRecord fields tail' ->
      case Map.lookup field fields of
        Just fieldTy -> Just (unOptional fieldTy)
        Nothing
          | tail' == tDynamic || tail' == tAny -> Just tail'
          | otherwise -> Nothing
    TApp (TCon "AttrsOf") valueTy -> Just valueTy
    TUnion members ->
      let hits = mapMaybe (\member -> lookupRecordField env member field) members
       in case hits of
            x : xs
              | length hits == length members ->
                  Just (foldRight1 (joinTypes env) x xs)
            _ -> Nothing
    _ -> Nothing

-- | Compute the least-upper-bound style merge used by the checker.
--
-- Where possible this returns an existing supertype; otherwise it constructs a
-- normalized union.
--
-- The merge prefers preserving semantic structure over flattening everything
-- into `TUnion`. For example:
--
-- * matching `Unit` labels join through their payloads,
-- * tuples join positionally,
-- * tensors of the same rank join their shapes axis-by-axis,
-- * numeric families join through `Nat -> Int -> Number`.
--
-- Only when no more meaningful merge exists does the function fall back to a
-- normalized union.
--
-- Representative examples:
--
-- @
-- joinTypes Nat Int
--   => Int
--
-- joinTypes (Vec 2 Int) (Vec 3 Int)
--   => Vec (2 | 3) Int
--
-- joinTypes (Unit "ms" (Range 0 10 Nat)) (Unit "ms" Nat)
--   => Unit "ms" Nat
-- @
joinTypes :: AliasEnv -> Type -> Type -> Type
joinTypes env left right =
  case () of
    _
      | left' == tAny || right' == tAny -> tAny
      | otherwise ->
          case (unitView left', unitView right') of
            (Just (leftUnit, leftBase), Just (rightUnit, rightBase))
              | leftUnit == rightUnit ->
                  TApp (TApp (TCon "Unit") leftUnit) (joinTypes env leftBase rightBase)
            _ ->
              case (tupleView left', tupleView right') of
                (Just leftItems, Just rightItems)
                  | length leftItems == length rightItems ->
                      TApp (TCon "Tuple") (TTypeList (zipWith (joinTypes env) leftItems rightItems))
                _ ->
                  case (tensorView left', tensorView right') of
                    (Just (leftShape, leftElem), Just (rightShape, rightElem))
                      | length leftShape == length rightShape ->
                          surfaceTensor (zipWith (joinTypes env) leftShape rightShape) (joinTypes env leftElem rightElem)
                      | otherwise ->
                          case (listView left', listView right') of
                            (Just leftList, Just rightList) -> joinTypes env leftList rightList
                            _ -> fallback
                    _ ->
                      case (listView left', listView right') of
                        (Just leftList, Just rightList) -> joinTypes env leftList rightList
                        _ -> fallback
  where
    left' = resolveHead env left
    right' = resolveHead env right
    fallback
      | left' == right' = left'
      | isSubtype env left' right' = right'
      | isSubtype env right' left' = left'
      | not (bothNumericLiterals left' right'),
        Just joinedNumeric <- joinNumericTypes left' right' =
          joinedNumeric
      | otherwise = flattenUnion (TUnion [left', right'])

-- | Decide whether two types can coexist under gradual typing rules.
--
-- Consistency is weaker than subtyping: `dynamic` is consistent with anything
-- even when it is not a subtype of that thing.
--
-- The checker uses this relation when it wants to preserve the "gradual"
-- escape hatch without claiming a precise structural subtype relation exists.
--
-- Representative examples:
--
-- @
-- isConsistent dynamic String => True
-- isConsistent String Int     => False
-- @
isConsistent :: AliasEnv -> Type -> Type -> Bool
isConsistent env = go
  where
    go left right =
      let left' = resolveHead env left
          right' = resolveHead env right
       in left' == TAny
            || right' == TAny
            || left' == TDynamic
            || right' == TDynamic
            || left' == right'
            || isSubtype env left' right'
            || isSubtype env right' left'
            || structural left' right'
    -- Consistency is structural: `List dynamic` is consistent with
    -- `List String`, and `dynamic -> Bool` with `String -> Bool`.
    structural a b =
      case (a, b) of
        -- Exact sequences meet lists through their element view.
        _
          | Just listA <- sequenceView a,
            Just listB <- sequenceView b,
            (listA, listB) /= (a, b) ->
              go listA listB
        (TApp f x, TApp g y) -> go f g && go x y
        (TFun _ x y, TFun _ x' y') -> go x x' && go y y'
        (TOptional x, TOptional y) -> go x y
        (TUnion members, _) -> all (`go` b) members
        (_, TUnion members) -> any (go a) members
        _
          | Just (fields, tailA) <- recordView a,
            Just (fields', tailB) <- recordView b ->
              and (Map.intersectionWith (\x y -> go (unOptional x) (unOptional y)) fields fields')
                && coveredBy fields tailA fields'
                && coveredBy fields' tailB fields
        _ -> False
    sequenceView ty =
      case (tensorListView ty, tupleListView ty) of
        (Just listTy, _) -> Just listTy
        (_, Just listTy) -> Just listTy
        _ -> case ty of
          TApp (TCon "List") _ -> Just ty
          _ -> Nothing
    -- Every required field of @other@ must exist on a closed record.
    coveredBy fields tail' other =
      isJust tail'
        || all (\(name, ty) -> Map.member name fields || isOptional ty) (Map.toList other)
    isOptional = \case
      TOptional _ -> True
      _ -> False

-- | Structural subtyping relation used by the checker.
--
-- Functions are contravariant in their argument and covariant in their result;
-- records use width subtyping; literals subtype their primitive constructor.
--
-- Beyond those classic rules, tynix also treats a few richer forms
-- structurally:
--
-- * `Range` values subtype their numeric base and enclosed super-ranges,
-- * `Unit` values subtype same-label unit wrappers and may accept bare numeric
--   literals at the outer boundary,
-- * tensors subtype structural lists, and exact empty tensors are considered
--   compatible with any element type because they carry no contradicting
--   evidence.
--
-- Representative examples:
--
-- @
-- 3 <: Range 0 10 Nat                 => True
-- Range 2 4 Nat <: Range 0 10 Nat     => True
-- Unit "ms" Nat <: Unit "s" Nat       => False
-- Vec 2 Int <: List Int               => True
-- Vec 0 dynamic <: Vec (Range 0 2 Nat) Int => True
-- @
isSubtype :: AliasEnv -> Type -> Type -> Bool
isSubtype env = go
  where
    -- Each level is resolved as it is reached, so large and recursive aliases
    -- are only expanded as far as the comparison actually looks.
    go a b = step (resolveHead env a) (resolveHead env b)
    step a b | a == b = True
    step (TSingleton _ base) b = go base b
    step _ ty | ty == tAny = True
    step _ ty | ty == tDynamic = True
    step ty _ | ty == tAny = True
    step ty _ | ty == tDynamic = False
    step _ ty | ty == tUnknown = True
    step ty _ | ty == tUnknown = False
    step (TLit (LString _)) ty | ty == tString = True
    step (TLit (LFloat _)) ty | ty == tFloat = True
    step (TLit lit) ty | ty == tNumber = isNumericLiteral lit
    step (TLit (LInt _)) ty | ty == tInt = True
    step (TLit (LInt n)) ty | ty == tNat = n >= 0
    step (TLit (LBool _)) ty | ty == tBool = True
    step ty other | ty == tNat, other == tInt = True
    step ty other | ty == tNat, other == tNumber = True
    step ty other | ty == tInt, other == tNumber = True
    step ty other | ty == tFloat, other == tNumber = True
    step (TTypeList xs) (TTypeList ys) = length xs == length ys && and (zipWith go xs ys)
    step (TUnion leftMembers) (TUnion rightMembers) =
      -- Every left member must be covered by some right member. An exact match
      -- is by far the common case (identical or overlapping unions), and `go`
      -- already answers True for equal types, so consult a set first and only
      -- fall back to the quadratic scan for members with no exact counterpart.
      let rightSet = Set.fromList rightMembers
       in all
            (\member -> Set.member member rightSet || any (go member) rightMembers)
            leftMembers
    step a (TUnion members) = any (go a) members
    step (TUnion members) b = all (`go` b) members
    step a b
      | Just (leftLower, leftUpper, leftBase) <- rangeView a,
        Just (rightLower, rightUpper, rightBase) <- rangeView b =
          rangeBaseSubtype go leftLower leftUpper leftBase rightBase
            && rangeBoundsWithin leftLower leftUpper rightLower rightUpper
      | Just (leftLower, leftUpper, leftBase) <- rangeView a,
        b == tNat =
          (leftBase == tInt || leftBase == tNat) && nonNegativeIntegerBounds leftLower leftUpper
      | Just (_, _, leftBase) <- rangeView a =
          go leftBase b
    step a b
      | Just (rightLower, rightUpper, rightBase) <- rangeView b =
          go a rightBase && literalWithinRange a rightLower rightUpper
    step a b
      | Just (leftUnit, leftBase) <- unitView a,
        Just (rightUnit, rightBase) <- unitView b =
          leftUnit == rightUnit && go leftBase rightBase
      | Just (rightUnit, rightBase) <- unitView b =
          unitLabelLiteral rightUnit && numericLiteralType a && go a rightBase
    step a b
      | Just leftItems <- tupleView a,
        Just rightItems <- tupleView b =
          length leftItems == length rightItems && and (zipWith go leftItems rightItems)
      | Just leftList <- tupleListView a =
          go leftList b
    step a b
      | Just (leftShape, leftElem) <- tensorView a,
        Just (rightShape, rightElem) <- tensorView b =
          length leftShape == length rightShape
            && and (zipWith go leftShape rightShape)
            && (shapeDefinitelyEmpty leftShape || go leftElem rightElem)
      | Just leftList <- tensorListView a =
          go leftList b
    step (TArrow leftArrow a b) (TArrow rightArrow c d) =
      let shared = TSingleton "it" c
       in multiplicitySubtype (arrowMult leftArrow) (arrowMult rightArrow)
            && effectsWithin (arrowEffects leftArrow) (arrowEffects rightArrow)
            && capturesWithin (arrowCaptures leftArrow) (arrowCaptures rightArrow)
            && go c a
            && go (eraseBinder leftArrow shared b) (eraseBinder rightArrow shared d)
    -- An opaque type is nominal and invariant in its parameters: phantom
    -- parameters must agree exactly.
    step a b
      | isOpaqueHead env a || isOpaqueHead env b =
          case (collectApps a, collectApps b) of
            ((TCon left, leftArgs), (TCon right, rightArgs)) ->
              left == right
                && length leftArgs == length rightArgs
                && and (zipWith (\x y -> go x y && go y x) leftArgs rightArgs)
            _ -> False
    step a b
      | Just (fields, actualTail) <- recordView a,
        Just (expected, _) <- recordView b =
          -- Width subtyping; an optional expected field may be absent, and an
          -- actual row with a `dynamic` tail may hold any further field.
          all (fieldSatisfied fields actualTail) (Map.toList expected)
      | Just (fields, actualTail) <- recordView a,
        Just valueTy <- attrsOfView b =
          all (\ty -> go (unOptional ty) valueTy) (Map.elems fields)
            && maybe True (\tail' -> tail' == tDynamic || tail' == tAny) actualTail
    step a b
      | Just valueTy <- attrsOfView a,
        Just (expected, Just tail') <- recordView b,
        tail' == tDynamic || tail' == tAny =
          -- A dictionary may lack any key, so it only meets an open record
          -- whose listed fields are all optional.
          all (\case TOptional ty -> go valueTy ty; _ -> False) (Map.elems expected)
    step (TOptional a) (TOptional b) = go a b
    step (TApp f x) (TApp g y) = go f g && go x y
    step _ _ = False

    fieldSatisfied fields actualTail (name, expectedTy) =
      case (Map.lookup name fields, expectedTy) of
        (Just (TOptional actualTy), TOptional inner) -> go actualTy inner
        (Just (TOptional _), _) -> False
        (Just actualTy, TOptional inner) -> go actualTy inner
        (Just actualTy, _) -> go actualTy expectedTy
        (Nothing, TOptional _) -> True
        (Nothing, _) -> actualTail == Just tDynamic || actualTail == Just tAny

-- | View a closed or open record as its fields plus an optional row tail.
recordView :: Type -> Maybe (Map.Map Name Type, Maybe Type)
recordView = \case
  TRecord fields -> Just (fields, Nothing)
  TOpenRecord fields tail' -> Just (fields, Just tail')
  _ -> Nothing

-- | Recognize the built-in dictionary type `AttrsOf a`.
attrsOfView :: Type -> Maybe Type
attrsOfView = \case
  TApp (TCon "AttrsOf") valueTy -> Just valueTy
  _ -> Nothing

-- | Drop an optional-field marker.
unOptional :: Type -> Type
unOptional = \case
  TOptional inner -> inner
  other -> other

-- | Multiplicity subtyping for function arrows.
--
-- A linear function can be used where an unrestricted function is expected, but
-- not the other way around.
multiplicitySubtype :: Multiplicity -> Multiplicity -> Bool
multiplicitySubtype left right =
  left == right
    || case (left, right) of
      (One, Many) -> True
      _ -> False

-- | Recover the nicest surface tensor spelling for a joined shape.
--
-- This mirrors `surfaceTensorType` in `Indexed`, but lives locally so the
-- subtyping layer can rebuild user-facing shapes after canonical comparisons.
--
-- Representative examples:
--
-- @
-- surfaceTensor [2] Int       => Vec 2 Int
-- surfaceTensor [2, 4] Int    => Matrix 2 4 Int
-- surfaceTensor [2, 3, 4] Int => Tensor [2 3 4] Int
-- @
surfaceTensor :: [Type] -> Type -> Type
surfaceTensor dims elemTy =
  case dims of
    [lenTy] -> TApp (TApp (TCon "Vec") lenTy) elemTy
    [rowsTy, colsTy] -> TApp (TApp (TApp (TCon "Matrix") rowsTy) colsTy) elemTy
    _ -> TApp (TApp (TCon "Tensor") (TTypeList dims)) elemTy

-- | Structural list view used when exact tuples/tensors meet list consumers.
listView :: Type -> Maybe Type
listView ty =
  case tupleListView ty of
    Just listTy -> Just listTy
    Nothing -> tensorListView ty

-- | Pattern-match the encoded `Range` type application.
rangeView :: Type -> Maybe (Type, Type, Type)
rangeView ty =
  case collectApps ty of
    (TCon "Range", [lowerTy, upperTy, baseTy]) -> Just (lowerTy, upperTy, baseTy)
    _ -> Nothing

-- | Pattern-match the encoded `Unit` type application.
unitView :: Type -> Maybe (Type, Type)
unitView ty =
  case collectApps ty of
    (TCon "Unit", [unitTy, baseTy]) -> Just (unitTy, baseTy)
    _ -> Nothing

-- | Normalized numeric values used while comparing ranges and literals.
data NumericBound
  = NumericInt Integer
  | NumericFloat Double
  deriving (Eq, Ord, Show)

-- | Check whether a numeric singleton lies within an inclusive range.
literalWithinRange :: Type -> Type -> Type -> Bool
literalWithinRange actual lowerTy upperTy =
  case (numericLiteralTypeValue actual, numericBoundValue lowerTy, numericBoundValue upperTy) of
    (Just actualValue, Just lowerValue, Just upperValue) ->
      compareNumericBound actualValue lowerValue /= LT && compareNumericBound actualValue upperValue /= GT
    _ -> False

-- | Check whether one inclusive range is enclosed by another.
rangeBoundsWithin :: Type -> Type -> Type -> Type -> Bool
rangeBoundsWithin leftLower leftUpper rightLower rightUpper =
  case (numericBoundValue leftLower, numericBoundValue leftUpper, numericBoundValue rightLower, numericBoundValue rightUpper) of
    (Just leftLow, Just leftHigh, Just rightLow, Just rightHigh) ->
      compareNumericBound leftLow rightLow /= LT && compareNumericBound leftHigh rightHigh /= GT
    _ -> False

-- | Recognize integer ranges that stay non-negative and ordered.
--
-- This is the predicate that allows integer-based ranges to subtype `Nat`.
nonNegativeIntegerBounds :: Type -> Type -> Bool
nonNegativeIntegerBounds lowerTy upperTy =
  case (numericBoundValue lowerTy, numericBoundValue upperTy) of
    (Just (NumericInt low), Just (NumericInt high)) -> low >= 0 && high >= 0 && low <= high
    _ -> False

-- | Recognize numeric singleton types.
numericLiteralType :: Type -> Bool
numericLiteralType = isJust . numericLiteralTypeValue

-- | Decode a singleton numeric literal into the internal comparison domain.
numericLiteralTypeValue :: Type -> Maybe NumericBound
numericLiteralTypeValue = \case
  TLit (LInt n) -> Just (NumericInt n)
  TLit (LFloat n) -> Just (NumericFloat n)
  _ -> Nothing

-- | Decode a bound expression into the internal comparison domain.
numericBoundValue :: Type -> Maybe NumericBound
numericBoundValue = \case
  TLit (LInt n) -> Just (NumericInt n)
  TLit (LFloat n) -> Just (NumericFloat n)
  _ -> Nothing

-- | Compare numeric bounds after lifting them into a shared ordered domain.
compareNumericBound :: NumericBound -> NumericBound -> Ordering
compareNumericBound left right =
  compare (toDouble left) (toDouble right)
  where
    toDouble = \case
      NumericInt n -> fromInteger n
      NumericFloat n -> n

-- | Recognize numeric singleton literal constructors.
isNumericLiteral :: LiteralType -> Bool
isNumericLiteral = \case
  LInt _ -> True
  LFloat _ -> True
  _ -> False

-- | Collapse a type into its broad numeric family when possible.
--
-- This is used by numeric joins so that ranges and literals can still produce a
-- meaningful common supertype.
numericBaseType :: Type -> Maybe Type
numericBaseType ty
  | ty == tNat = Just tNat
  | ty == tInt = Just tInt
  | ty == tFloat = Just tFloat
  | ty == tNumber = Just tNumber
numericBaseType (TLit (LInt _)) = Just tInt
numericBaseType (TLit (LFloat _)) = Just tFloat
numericBaseType ty
  | Just (_, _, baseTy) <- rangeView ty = numericBaseType baseTy
numericBaseType _ = Nothing

-- | Check whether both inputs are numeric singleton types.
bothNumericLiterals :: Type -> Type -> Bool
bothNumericLiterals left right = numericLiteralType left && numericLiteralType right

-- | Join two numeric families when a richer structural join is unavailable.
joinNumericTypes :: Type -> Type -> Maybe Type
joinNumericTypes left right = do
  leftBase <- numericBaseType left
  rightBase <- numericBaseType right
  pure (joinNumericBases leftBase rightBase)

-- | Join numeric carrier families with the smallest useful widening.
--
-- The widening order is:
--
-- @
-- Nat < Int < Number
-- Float < Number
-- @
joinNumericBases :: Type -> Type -> Type
joinNumericBases left right
  | left == right = left
  | left == tNat, right == tInt = tInt
  | left == tInt, right == tNat = tInt
  | otherwise = tNumber

-- | Check whether a range's carrier family may subtype another numeric family.
--
-- This is separate from bound inclusion because `Range 0 10 Nat` can subtype
-- `Number` even before we compare its exact interval with anything.
rangeBaseSubtype :: (Type -> Type -> Bool) -> Type -> Type -> Type -> Type -> Bool
rangeBaseSubtype subtype lowerTy upperTy leftBase rightBase
  | leftBase == rightBase = True
  | rightBase == tNat = (leftBase == tInt || leftBase == tNat) && nonNegativeIntegerBounds lowerTy upperTy
  | otherwise = subtype leftBase rightBase

-- | Recognize string singleton labels suitable for `Unit`.
unitLabelLiteral :: Type -> Bool
unitLabelLiteral = \case
  TLit (LString _) -> True
  _ -> False

-- | Detect whether any tensor axis proves the overall tensor must be empty.
--
-- When this predicate holds, the element type cannot be observed at runtime, so
-- subtyping allows any payload type to flow into the empty tensor.
shapeDefinitelyEmpty :: [Type] -> Bool
shapeDefinitelyEmpty = any typeDefinitelyZero

-- | Recognize types that describe the exact length zero.
--
-- This includes the singleton `0` and exact zero ranges such as
-- `Range 0 0 Nat`. A union only counts as definitely zero when every member is
-- definitely zero.
--
-- Representative examples:
--
-- @
-- typeDefinitelyZero 0                => True
-- typeDefinitelyZero (Range 0 0 Nat)  => True
-- typeDefinitelyZero (0 | Range 0 0 Nat) => True
-- typeDefinitelyZero (0 | 1)          => False
-- @
typeDefinitelyZero :: Type -> Bool
typeDefinitelyZero = \case
  TLit (LInt 0) -> True
  TUnion members -> not (null members) && all typeDefinitelyZero members
  ty
    | Just (lowerTy, upperTy, _) <- rangeView ty ->
        case (numericBoundValue lowerTy, numericBoundValue upperTy) of
          (Just (NumericInt 0), Just (NumericInt 0)) -> True
          _ -> False
  _ -> False

-- | Right-associative fold over a non-empty list produced from a head + tail.
--
-- Equivalent to @foldr1 f (x : xs)@ but total: callers pass the head and tail
-- separately so the empty case is unrepresentable.
foldRight1 :: (a -> a -> a) -> a -> [a] -> a
foldRight1 _ x [] = x
foldRight1 f x (y : ys) = f x (foldRight1 f y ys)
