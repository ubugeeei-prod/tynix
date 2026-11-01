{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Indexed container helpers for value-shaped tynix types.
--
-- The language stays runtime-free, but the checker can still track exact list
-- shapes by reflecting them into `Vec`, `Matrix`, and `Tensor` types.
--
-- This module does two related jobs:
--
-- * It infers shape-rich sequence types from ordinary list syntax.
-- * It validates user-written indexed annotations before the checker relies on
--   them.
--
-- The implementation deliberately stays "runtime free". No evidence terms or
-- proof objects are introduced. Instead, the checker keeps enough static shape
-- information around to answer questions such as:
--
-- * "is this a rectangular matrix or a ragged list of vectors?"
-- * "does this axis stay nat-like?"
-- * "is a `Range`/`Unit` annotation structurally well-formed before type
--   inference touches it?"
--
-- A few examples capture the intended surface behavior:
--
-- @
-- [1 2]                 => Vec 2 (1 | 2)
-- [[1 2] [3 4]]         => Matrix 2 2 (1 | 2 | 3 | 4)
-- [[1] [2 3]]           => List (Vec (1 | 2) (1 | 2 | 3))
-- Vec (Range 2 4 Nat) a => accepted as a nat-like indexed container
-- Vec (Unit \"ms\" Nat) a => rejected before checking
-- @
module Indexed
  ( inferListType,
    normalizeIndexedType,
    tensorListView,
    tensorView,
    tupleListView,
    tupleView,
    validateProgramIndexedTypes,
  )
where

import Alias (collectApps)
import Control.Monad (unless, when)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Syntax (AmbientDecl (ambientEntries), AmbientEntry (ambientEntryType), Expr, Marked (markedValue), Program (..), exprAnnotations)
import Type (LiteralType (..), Name, Type (..), TypeAlias (typeAliasBody), mkOpenRecord, tDynamic, tFloat, tInt, tList, tNat, tNumber)

-- | Infer the most precise sequence type that can be justified from a list
-- literal's member types.
--
-- The caller supplies the element join operation so this function can stay
-- independent from the rest of the subtyping implementation. In practice the
-- checker passes `joinTypes`.
--
-- The inference strategy is intentionally shape-first:
--
-- * Empty lists become `Vec 0 dynamic` because there is no evidence for an
--   element type yet.
-- * Uniform nested tensors stay tensor-shaped and gain an outer dimension.
-- * Flat homogeneous lists become `Vec`.
-- * Flat heterogeneous lists become `Tuple`.
-- * Mixed or ragged nested sequences widen to structural `List`.
--
-- This means the function prefers preserving evidence when it exists, but it
-- does not pretend a rectangular shape exists when the source only proves a
-- ragged container.
--
-- Representative examples:
--
-- @
-- inferListType join [1, 2]
--   => Vec 2 (1 | 2)
--
-- inferListType join [[1, 2], [3, 4]]
--   => Matrix 2 2 (1 | 2 | 3 | 4)
--
-- inferListType join [[1], [2, 3]]
--   => List (Vec (1 | 2) (1 | 2 | 3))
--
-- inferListType join []
--   => Vec 0 dynamic
-- @
inferListType :: (Type -> Type -> Type) -> [Type] -> Type
inferListType joinElem members =
  case members of
    [] -> surfaceTensorType [TLit (LInt 0)] tDynamic
    firstMember : restMembers ->
      let normalized = normalizeIndexedType <$> members
          normalizedHead = normalizeIndexedType firstMember
          normalizedRest = normalizeIndexedType <$> restMembers
          views = tensorView <$> normalized
          elemTy = foldl joinElem normalizedHead normalizedRest
          lenTy = TLit (LInt (toInteger (length members)))
       in case sequence views >>= foldTensorMembers joinElem of
            Just (shape, baseTy) -> surfaceTensorType (lenTy : shape) baseTy
            Nothing
              | all isNothing views && hasUniformSequenceFamily normalized -> surfaceTensorType [lenTy] elemTy
              | all isNothing views -> tupleType normalized
              | otherwise -> tList elemTy

-- | Rewrite surface `Vec`/`Matrix`/`Tensor` spellings into one canonical
-- tensor representation.
--
-- Canonicalization makes later algorithms much simpler because they can reason
-- about one normalized shape form even if the user wrote a nicer surface alias.
-- The result still lives in the ordinary `Type` syntax tree; this is merely a
-- structural normalization pass, not a separate typed IR.
--
-- The normalization is recursive, so shapes embedded in unions, function
-- arrows, records, or conditional types are also normalized before later
-- passes inspect them.
--
-- Representative examples:
--
-- @
-- normalizeIndexedType (Vec 3 Int)
--   => Tensor [3] Int
--
-- normalizeIndexedType (Matrix 2 4 String)
--   => Tensor [2 4] String
--
-- normalizeIndexedType (List (Vec 2 Int | Vec 3 Int))
--   => List (Tensor [2] Int | Tensor [3] Int)
-- @
normalizeIndexedType :: Type -> Type
normalizeIndexedType ty
  -- Only `Vec`, `Matrix`, and `Tensor` applications are rewritten; every other
  -- node is rebuilt into an identical copy. Most types mention none of them,
  -- and the checker normalizes on every constrain/unify step, so a cheap
  -- allocation-free scan up front pays for itself many times over.
  | mentionsIndexedConstructor ty = normalizeIndexed ty
  | otherwise = ty

-- | Conservatively detect types that 'normalizeIndexedType' might rewrite.
--
-- Over-approximating is safe (it only costs a redundant rebuild); missing a
-- mention would not be, so this looks for the constructor names themselves
-- rather than trying to match full application shapes.
mentionsIndexedConstructor :: Type -> Bool
mentionsIndexedConstructor = \case
  TCon name -> name == "Vec" || name == "Matrix" || name == "Tensor"
  TTypeList items -> any mentionsIndexedConstructor items
  TFun _ left right -> mentionsIndexedConstructor left || mentionsIndexedConstructor right
  TRecord fields -> any mentionsIndexedConstructor fields
  TOpenRecord fields tail' -> any mentionsIndexedConstructor fields || mentionsIndexedConstructor tail'
  TOptional inner -> mentionsIndexedConstructor inner
  TUnion members -> any mentionsIndexedConstructor members
  TApp fun arg -> mentionsIndexedConstructor fun || mentionsIndexedConstructor arg
  TForall _ body -> mentionsIndexedConstructor body
  TConditional actual patternTy yesTy noTy ->
    any mentionsIndexedConstructor [actual, patternTy, yesTy, noTy]
  _ -> False

normalizeIndexed :: Type -> Type
normalizeIndexed = \case
  TTypeList items -> TTypeList (normalizeIndexedType <$> items)
  TArrow arrow left right -> TArrow arrow (normalizeIndexedType left) (normalizeIndexedType right)
  TRecord fields -> TRecord (fmap normalizeIndexedType fields)
  TOpenRecord fields tail' -> mkOpenRecord (fmap normalizeIndexedType fields) (normalizeIndexedType tail')
  TOptional inner -> TOptional (normalizeIndexedType inner)
  TUnion members -> TUnion (normalizeIndexedType <$> members)
  TApp fun arg ->
    case collectApps (TApp (normalizeIndexedType fun) (normalizeIndexedType arg)) of
      (TCon "Vec", [lenTy, elemTy]) -> canonicalTensorType [lenTy] elemTy
      (TCon "Matrix", [rowsTy, colsTy, elemTy]) -> canonicalTensorType [rowsTy, colsTy] elemTy
      (TCon "Tensor", [TTypeList dims, elemTy]) -> canonicalTensorType dims elemTy
      (headTy, args) -> foldl TApp headTy args
  TForall vars body -> TForall vars (normalizeIndexedType body)
  TConditional actual patternTy yesTy noTy ->
    TConditional
      (normalizeIndexedType actual)
      (normalizeIndexedType patternTy)
      (normalizeIndexedType yesTy)
      (normalizeIndexedType noTy)
  other -> other

-- | View a type as an indexed tensor when its head constructor is one of the
-- supported sequence families.
--
-- The returned pair is `(shape, elementType)`. `Vec` contributes one axis,
-- `Matrix` contributes two axes, and `Tensor` returns its shape list as-is.
-- Callers generally run `normalizeIndexedType` first so that all three surface
-- spellings behave uniformly.
--
-- Representative examples:
--
-- @
-- tensorView (Vec 3 Int)             => Just ([3], Int)
-- tensorView (Matrix 2 4 String)     => Just ([2, 4], String)
-- tensorView (Tensor [2 3 1] Number) => Just ([2, 3, 1], Number)
-- tensorView (Tuple [Int String])    => Nothing
-- @
tensorView :: Type -> Maybe ([Type], Type)
tensorView ty =
  case collectApps ty of
    (TCon "Vec", [lenTy, elemTy]) -> Just ([lenTy], elemTy)
    (TCon "Matrix", [rowsTy, colsTy, elemTy]) -> Just ([rowsTy, colsTy], elemTy)
    (TCon "Tensor", [TTypeList dims, elemTy]) -> Just (dims, elemTy)
    _ -> Nothing

-- | View a type as a fixed heterogeneous tuple.
--
-- Tuples are encoded as `Tuple [a b c]` rather than as a dedicated AST node.
-- This helper centralizes that convention for the rest of the checker.
--
-- Representative examples:
--
-- @
-- tupleView (Tuple [Int String]) => Just [Int, String]
-- tupleView (Vec 2 Int)          => Nothing
-- @
tupleView :: Type -> Maybe [Type]
tupleView ty =
  case collectApps ty of
    (TCon "Tuple", [TTypeList items]) -> Just items
    _ -> Nothing

-- | Widen a tensor-like type into the structural `List` view that ordinary Nix
-- code would observe.
--
-- For example:
--
-- @
-- Vec 3 Int              => List Int
-- Matrix 2 3 Int         => List (Vec 3 Int)
-- Tensor [2 3 4] Int     => List (Tensor [3 4] Int)
-- Tensor [2 0 4] Int     => List (Tensor [0 4] Int)
-- @
--
-- This helper is used by structural subtyping and consistency to explain how
-- fixed-shape tensors relate to ordinary list consumers.
tensorListView :: Type -> Maybe Type
tensorListView ty = do
  (dims, elemTy) <- tensorView (normalizeIndexedType ty)
  case dims of
    [] -> Nothing
    _ : rest ->
      Just
        ( tList
            ( normalizeIndexedType
                (if null rest then elemTy else surfaceTensorType rest elemTy)
            )
        )

-- | Widen a tuple into a structural homogeneous list when possible.
--
-- Because tuples are fixed and heterogeneous, this operation necessarily joins
-- all element types. The result is therefore a lossier structural view than the
-- original tuple, but it lets list-oriented relations compare tuples with
-- `List` consumers.
--
-- Representative examples:
--
-- @
-- tupleListView (Tuple [Int String])
--   => Just (List (Int | String))
--
-- tupleListView (Tuple [])
--   => Just (Vec 0 dynamic)
-- @
tupleListView :: Type -> Maybe Type
tupleListView ty = do
  items <- tupleView (normalizeIndexedType ty)
  pure $
    case items of
      [] -> surfaceTensorType [TLit (LInt 0)] tDynamic
      x : xs -> tList (foldl joinTupleItems x xs)

-- | Validate all indexed, numeric-refinement, and unit annotations inside a
-- parsed program before the type checker consumes them.
--
-- This is a pure structural validation pass. It does not ask whether a program
-- is well-typed overall; it only checks whether the annotation syntax expresses
-- something meaningful in tynix's indexed world.
--
-- In particular it rejects:
--
-- * non-nat-like tensor dimensions,
-- * malformed `Range` declarations,
-- * malformed `Unit` labels,
-- * impossible nat-like ranges such as `Range 4 2 Nat`.
--
-- Representative examples:
--
-- @
-- type Grid t = Matrix (Range 1 2 Nat) (2 | 3) t;
--   => accepted
--
-- let xs :: Vec (Range 0.0 2.0 Nat) Int;
--   => rejected
--
-- type Delay = Unit 1 Int;
--   => rejected
-- @
validateProgramIndexedTypes :: Program -> Either String ()
validateProgramIndexedTypes program =
  traverse_ (validateType "type alias" . typeAliasBody) (programAliases program)
    *> traverse_ validateAmbientDecl (programAmbient program)
    *> traverse_ (validateExpr . markedValue) (programExpr program)

-- | Build the internal canonical tensor spelling used by normalization.
canonicalTensorType :: [Type] -> Type -> Type
canonicalTensorType dims = TApp (TApp (TCon "Tensor") (TTypeList dims))

-- | Recover the nicest surface spelling for a tensor shape.
--
-- One axis becomes `Vec`, two axes become `Matrix`, and any higher-rank shape
-- stays as `Tensor`. The checker uses this when it wants to preserve user-facing
-- ergonomics after performing canonical internal work.
--
-- Representative examples:
--
-- @
-- surfaceTensorType [3] Int       => Vec 3 Int
-- surfaceTensorType [2, 4] Int    => Matrix 2 4 Int
-- surfaceTensorType [2, 3, 4] Int => Tensor [2 3 4] Int
-- @
surfaceTensorType :: [Type] -> Type -> Type
surfaceTensorType dims elemTy =
  case dims of
    [lenTy] -> TApp (TApp (TCon "Vec") lenTy) elemTy
    [rowsTy, colsTy] -> TApp (TApp (TApp (TCon "Matrix") rowsTy) colsTy) elemTy
    _ -> canonicalTensorType dims elemTy

-- | Build the encoded tuple form.
tupleType :: [Type] -> Type
tupleType = TApp (TCon "Tuple") . TTypeList

-- | Combine the shapes of already-recognized tensor members.
--
-- The fold only succeeds when every member proves the same inner shape. When
-- one member disagrees, the caller must widen to a structural list instead of
-- pretending a rectangular tensor exists.
--
-- Representative examples:
--
-- @
-- foldTensorMembers join [([2], Int), ([2], String)]
--   => Just ([2], Int | String)
--
-- foldTensorMembers join [([1], Int), ([2], Int)]
--   => Nothing
-- @
foldTensorMembers :: (Type -> Type -> Type) -> [([Type], Type)] -> Maybe ([Type], Type)
foldTensorMembers joinElem = \case
  [] -> Nothing
  (shape, elemTy) : rest -> go shape elemTy rest
  where
    go shape elemTy pending =
      case pending of
        [] -> Just (shape, elemTy)
        (nextShape, nextElemTy) : rest
          | nextShape == shape -> go shape (joinElem elemTy nextElemTy) rest
          | otherwise -> Nothing

-- | Validate types attached to ambient declaration entries.
validateAmbientDecl :: AmbientDecl -> Either String ()
validateAmbientDecl ambientDecl = traverse_ (validateType "ambient entry" . ambientEntryType) (ambientEntries ambientDecl)

-- | Walk an expression tree and validate every embedded annotation it exposes.
--
-- Executable expressions themselves are not shape-validated here; only the type
-- syntax attached to them is inspected.
validateExpr :: Expr -> Either String ()
validateExpr = traverse_ (validateType "term annotation") . exprAnnotations

-- | Validate one type for indexed, numeric, and unit structure.
--
-- The traversal is recursive and intentionally conservative. If a shape is
-- clearly impossible, later phases never get a chance to reinterpret it.
validateType :: Text -> Type -> Either String ()
validateType label ty =
  case ty of
    TTypeList dims -> traverse_ (validateNatType label "tensor shape") dims
    TFun _ left right -> validateType label left *> validateType label right
    TRecord fields -> traverse_ (validateType label) fields
    TOpenRecord fields tail' -> traverse_ (validateType label) fields *> validateType label tail'
    TOptional inner -> validateType label inner
    TUnion members -> traverse_ (validateType label) members
    TApp fun arg ->
      case collectApps (TApp fun arg) of
        (TCon "Tuple", [TTypeList items]) ->
          traverse_ (validateType label) items
        (TCon "Range", [lowerTy, upperTy, baseTy]) ->
          validateRangeType label lowerTy upperTy baseTy
        (TCon "Unit", [unitTy, baseTy]) ->
          validateUnitType label unitTy baseTy
        (TCon "Vec", [lenTy, elemTy]) ->
          validateNatType label "Vec length" lenTy *> validateType label elemTy
        (TCon "Matrix", [rowsTy, colsTy, elemTy]) ->
          traverse_ (validateNatType label "Matrix dimension") [rowsTy, colsTy] *> validateType label elemTy
        (TCon "Tensor", [shapeTy, elemTy]) ->
          validateShapeType label shapeTy *> validateType label elemTy
        _ -> validateType label fun *> validateType label arg
    TForall _ body -> validateType label body
    TConditional actual patternTy yesTy noTy ->
      traverse_ (validateType label) [actual, patternTy, yesTy, noTy]
    _ -> pure ()

-- | Validate the shape argument of a `Tensor`.
--
-- `Tensor` allows either an explicit type list of axes or a not-obviously-wrong
-- placeholder that later phases may refine. Obvious mistakes such as strings,
-- floats, functions, or `Unit` wrappers are rejected here.
validateShapeType :: Text -> Type -> Either String ()
validateShapeType label = \case
  TTypeList dims -> traverse_ (validateNatType label "tensor shape") dims
  ty | isObviouslyInvalidIndex ty -> Left (renderIndexError label "Tensor shape" ty)
  _ -> pure ()

-- | Validate that a type is nat-like enough to describe a container axis.
--
-- Accepted shapes include:
--
-- * non-negative integer singletons,
-- * `Nat`,
-- * unions of accepted nat-like members,
-- * nat-like `Range` declarations.
validateNatType :: Text -> Text -> Type -> Either String ()
validateNatType label site = \case
  TLit (LInt n)
    | n >= 0 -> pure ()
    | otherwise -> Left (renderIndexError label site (TLit (LInt n)))
  TCon "Nat" -> pure ()
  TUnion members -> traverse_ (validateNatType label site) members
  ty
    | Just (lowerTy, upperTy, baseTy) <- rangeView ty ->
        validateNatRangeType label site lowerTy upperTy baseTy
  ty
    | isObviouslyInvalidIndex ty -> Left (renderIndexError label site ty)
    | otherwise -> pure ()

-- | Validate a `Range lower upper base` declaration.
--
-- This checks three separate invariants:
--
-- * both bounds are numeric literals,
-- * the carrier/base type is one of tynix's numeric families,
-- * the bounds make sense for that base type and appear in increasing order.
validateRangeType :: Text -> Type -> Type -> Type -> Either String ()
validateRangeType label lowerTy upperTy baseTy = do
  lower <- validateNumericBoundType label "Range lower bound" lowerTy
  upper <- validateNumericBoundType label "Range upper bound" upperTy
  validateNumericCarrierType label "Range base" baseTy
  validateRangeBounds label lower upper baseTy

-- | Validate a `Unit "label" base` declaration.
--
-- Unit wrappers are intentionally lightweight. The label must be a string
-- singleton, and the wrapped payload must itself already be a valid tynix type.
validateUnitType :: Text -> Type -> Type -> Either String ()
validateUnitType label unitTy baseTy = do
  validateUnitLabelType label unitTy
  validateType label baseTy

-- | Validate that a `Range` can act as a nat-like shape.
--
-- Compared with general `Range` validation, this is stricter:
--
-- * the carrier must be `Int` or `Nat`,
-- * both bounds must be integers,
-- * both bounds must be non-negative,
-- * the interval must not be inverted.
validateNatRangeType :: Text -> Text -> Type -> Type -> Type -> Either String ()
validateNatRangeType label site lowerTy upperTy baseTy = do
  lower <- validateNumericBoundType label "nat-like lower bound" lowerTy
  upper <- validateNumericBoundType label "nat-like upper bound" upperTy
  case baseTy of
    ty | ty == tInt || ty == tNat -> pure ()
    _ -> Left (renderIndexError label site (TApp (TApp (TApp (TCon "Range") lowerTy) upperTy) baseTy))
  case (lower, upper) of
    (NumericInt low, NumericInt high)
      | low < 0 || high < 0 -> Left (renderIndexError label site (TApp (TApp (TApp (TCon "Range") lowerTy) upperTy) baseTy))
      | low <= high -> pure ()
      | otherwise -> Left (renderRangeError label "Range bounds are inverted" lowerTy upperTy)
    _ -> Left (renderIndexError label site (TApp (TApp (TApp (TCon "Range") lowerTy) upperTy) baseTy))

-- | Decode one numeric bound from singleton type syntax.
validateNumericBoundType :: Text -> Text -> Type -> Either String NumericBound
validateNumericBoundType label site = \case
  TLit (LInt n) -> pure (NumericInt n)
  TLit (LFloat n) -> pure (NumericFloat n)
  ty -> Left (renderRangeSiteError label site ty)

-- | Check that a `Range` carrier belongs to tynix's numeric families.
validateNumericCarrierType :: Text -> Text -> Type -> Either String ()
validateNumericCarrierType label site = \case
  ty
    | ty == tInt || ty == tFloat || ty == tNumber || ty == tNat -> pure ()
  other -> Left (renderRangeSiteError label site other)

-- | Check that numeric bounds agree with their base family and interval order.
validateRangeBounds :: Text -> NumericBound -> NumericBound -> Type -> Either String ()
validateRangeBounds label lower upper baseTy = do
  unless (boundsFitBase lower upper baseTy) $
    Left (renderRangeError label "Range bounds do not match base type" (boundType lower) (boundType upper))
  when (compareNumericBound lower upper == GT) $
    Left (renderRangeError label "Range bounds are inverted" (boundType lower) (boundType upper))

-- | Ensure a `Unit` label is spelled as a string singleton.
validateUnitLabelType :: Text -> Type -> Either String ()
validateUnitLabelType label = \case
  TLit (LString _) -> pure ()
  other -> Left (renderRangeSiteError label "Unit label" other)

-- | Render a shape-specific validation error.
renderIndexError :: Text -> Text -> Type -> String
renderIndexError label site ty =
  show label
    <> " uses "
    <> show site
    <> " that is not nat-like: "
    <> show ty

-- | Render a `Range`/`Unit` site error.
renderRangeSiteError :: Text -> Text -> Type -> String
renderRangeSiteError label site ty =
  show label
    <> " uses "
    <> show site
    <> " that is not numeric/unit-like enough: "
    <> show ty

-- | Render a detailed invalid-range error.
renderRangeError :: Text -> String -> Type -> Type -> String
renderRangeError label message leftTy rightTy =
  show label
    <> " has invalid numeric validation: "
    <> message
    <> " ("
    <> show leftTy
    <> ", "
    <> show rightTy
    <> ")"

-- | Fast syntactic rejection for axis types that are definitely nonsense.
--
-- The checker still permits unknown or user-defined constructors here because a
-- later phase may assign them meaning through aliases. This predicate only
-- rejects shapes that are immediately incompatible with nat-like reasoning.
isObviouslyInvalidIndex :: Type -> Bool
isObviouslyInvalidIndex = \case
  TLit (LString _) -> True
  TLit (LFloat _) -> True
  TLit (LBool _) -> True
  TTypeList _ -> True
  TFun{} -> True
  TRecord _ -> True
  TOpenRecord _ _ -> True
  TForall _ _ -> True
  TCon name -> isPrimitiveTypeName name
  ty
    | Just (TCon "Unit", _) <- pure (collectApps ty) -> True
  _ -> False

-- | Recognize primitive built-in type constructors that can never stand for a
-- user-defined nat-like shape.
isPrimitiveTypeName :: Name -> Bool
isPrimitiveTypeName name = name `elem` ["Bool", "Float", "Int", "List", "Null", "Number", "Path", "String"]

-- | Check whether every flat element belongs to the same coarse sequence
-- family.
--
-- This lets `[1 2]` become `Vec 2 (...)` while `[1 "x"]` becomes a tuple.
hasUniformSequenceFamily :: [Type] -> Bool
hasUniformSequenceFamily members =
  case traverse sequenceFamily members of
    Nothing -> False
    Just [] -> False
    Just (family : rest) -> all (== family) rest

-- | Collapse literal and primitive constructors into coarse sequence families.
--
-- Numeric families are intentionally grouped together so mixed integer/float
-- lists can still become `Vec`s instead of immediately degenerating into
-- tuples.
sequenceFamily :: Type -> Maybe Name
sequenceFamily = \case
  TLit (LFloat _) -> Just "Number"
  TLit (LInt _) -> Just "Number"
  TLit (LString _) -> Just "String"
  TLit (LBool _) -> Just "Bool"
  TCon "Float" -> Just "Number"
  TCon "Int" -> Just "Number"
  TCon "Nat" -> Just "Number"
  TCon "Number" -> Just "Number"
  TCon name -> Just name
  _ -> Nothing

-- | Pattern-match the encoded `Range` type application.
rangeView :: Type -> Maybe (Type, Type, Type)
rangeView ty =
  case collectApps ty of
    (TCon "Range", [lowerTy, upperTy, baseTy]) -> Just (lowerTy, upperTy, baseTy)
    _ -> Nothing

-- | Numeric bounds used while validating `Range` declarations.
data NumericBound
  = NumericInt Integer
  | NumericFloat Double
  deriving (Eq, Ord, Show)

-- | Re-embed a decoded bound back into singleton syntax for diagnostics.
boundType :: NumericBound -> Type
boundType = \case
  NumericInt n -> TLit (LInt n)
  NumericFloat n -> TLit (LFloat n)

-- | Check whether a pair of bounds is representable by a base family.
--
-- `Int`/`Nat` require integral bounds, whereas `Float` and `Number` accept both
-- integer and float literals.
boundsFitBase :: NumericBound -> NumericBound -> Type -> Bool
boundsFitBase lower upper baseTy
  | baseTy == tInt = all isIntegerBound [lower, upper]
  | baseTy == tNat = all isIntegerBound [lower, upper]
  | baseTy == tFloat = True
  | baseTy == tNumber = True
  | otherwise = False

-- | Recognize integral bounds.
isIntegerBound :: NumericBound -> Bool
isIntegerBound = \case
  NumericInt _ -> True
  NumericFloat _ -> False

-- | Compare numeric bounds after coercing them into a common ordered domain.
compareNumericBound :: NumericBound -> NumericBound -> Ordering
compareNumericBound left right =
  compare (toDouble left) (toDouble right)
  where
    toDouble = \case
      NumericInt n -> fromInteger n
      NumericFloat n -> n

-- | Join tuple elements conservatively when widening to a list.
joinTupleItems :: Type -> Type -> Type
joinTupleItems left right
  | left == right = left
  | otherwise = TUnion [left, right]

traverse_ :: (Foldable f) => (a -> Either String b) -> f a -> Either String ()
traverse_ step = foldr (\item acc -> step item *> acc) (Right ())
