{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Indexed
import Parser (parseProgram)
import Subtyping (joinTypes)
import Test.Hspec
import Test.QuickCheck
import TestSupport (expectRight)
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "indexed containers" $ do
  it "normalizes Vec and Matrix into canonical tensor shapes" $ do
    normalizeIndexedType (TApp (TApp (TCon "Vec") (TLit (LInt 3))) tInt)
      `shouldBe` TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 3)])) tInt
    normalizeIndexedType
      (TApp (TApp (TApp (TCon "Matrix") (TLit (LInt 2))) (TLit (LInt 4))) tString)
      `shouldBe` TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 2), TLit (LInt 4)])) tString

  it "normalizes Tensor spellings and leaves the canonical form alone" $ do
    normalizeIndexedType
      (TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 2), TLit (LInt 3)])) tInt)
      `shouldBe` TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 2), TLit (LInt 3)])) tInt
    normalizeIndexedType
      ( TApp
          (TApp (TCon "Vec") (TLit (LInt 1)))
          (TApp (TApp (TCon "Vec") (TLit (LInt 2))) tInt)
      )
      `shouldBe` TApp
        (TApp (TCon "Tensor") (TTypeList [TLit (LInt 1)]))
        (TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 2)])) tInt)

  it "normalizes shapes nested inside every structural position" $ do
    let vec = TApp (TApp (TCon "Vec") (TLit (LInt 3))) tInt
        tensor = TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 3)])) tInt
    normalizeIndexedType (TRecord (Map.fromList [("xs", vec)]))
      `shouldBe` TRecord (Map.fromList [("xs", tensor)])
    normalizeIndexedType (TUnion [vec, tInt]) `shouldBe` TUnion [tensor, tInt]
    normalizeIndexedType (TFun Many vec vec) `shouldBe` TFun Many tensor tensor
    normalizeIndexedType (TForall ["a"] vec) `shouldBe` TForall ["a"] tensor
    normalizeIndexedType (TTypeList [vec]) `shouldBe` TTypeList [tensor]
    normalizeIndexedType (TConditional vec vec vec vec)
      `shouldBe` TConditional tensor tensor tensor tensor

  it "leaves types that mention no indexed constructor untouched" $
    -- The normalizer short-circuits on these, so the identity has to hold
    -- exactly rather than up to reconstruction.
    property $ \(PlainType ty) ->
      normalizeIndexedType ty === ty

  it "reaches a fixed point" $
    property $ \(ShapedType ty) ->
      normalizeIndexedType (normalizeIndexedType ty) === normalizeIndexedType ty

  it "views each surface spelling as the same shape" $ do
    tensorView (TApp (TApp (TCon "Vec") (TLit (LInt 3))) tInt)
      `shouldBe` Just ([TLit (LInt 3)], tInt)
    tensorView
      (TApp (TApp (TApp (TCon "Matrix") (TLit (LInt 2))) (TLit (LInt 3))) tInt)
      `shouldBe` Just ([TLit (LInt 2), TLit (LInt 3)], tInt)
    tensorView (TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 2)])) tInt)
      `shouldBe` Just ([TLit (LInt 2)], tInt)
    tensorView (TApp (TCon "Tuple") (TTypeList [tInt])) `shouldBe` Nothing
    tensorView tInt `shouldBe` Nothing

  it "views tuples only when spelled as a Tuple application" $ do
    tupleView (TApp (TCon "Tuple") (TTypeList [tInt, tString]))
      `shouldBe` Just [tInt, tString]
    tupleView (TApp (TCon "Tuple") tInt) `shouldBe` Nothing
    tupleView (TApp (TApp (TCon "Vec") (TLit (LInt 1))) tInt) `shouldBe` Nothing

  it "peels one axis at a time when widening a tensor to a list" $ do
    tensorListView (TApp (TApp (TCon "Vec") (TLit (LInt 3))) tInt)
      `shouldBe` Just (tList tInt)
    tensorListView (TApp (TApp (TCon "Tensor") (TTypeList [])) tInt)
      `shouldBe` Nothing
    tensorListView tInt `shouldBe` Nothing

  it "widens an empty tuple to a zero-length vector" $
    tupleListView (TApp (TCon "Tuple") (TTypeList []))
      `shouldBe` Just (TApp (TApp (TCon "Vec") (TLit (LInt 0))) tDynamic)

  it "infers an empty list as a zero-length vector of dynamic" $
    inferListType (joinTypes mempty) []
      `shouldBe` TApp (TApp (TCon "Vec") (TLit (LInt 0))) tDynamic

  it "keeps mixed numeric literals in one vector rather than a tuple" $
    -- Int and Float share a sequence family, so the shape stays a vector even
    -- though the element join keeps both literals precisely.
    inferListType (joinTypes mempty) [TLit (LInt 1), TLit (LFloat 2.5)]
      `shouldBe` TApp
        (TApp (TCon "Vec") (TLit (LInt 2)))
        (TUnion [TLit (LInt 1), TLit (LFloat 2.5)])

  it "splits genuinely different families into a tuple" $
    inferListType (joinTypes mempty) [TLit (LInt 1), TLit (LBool True)]
      `shouldBe` TApp (TCon "Tuple") (TTypeList [TLit (LInt 1), TLit (LBool True)])

  it "spells rank-three shapes as Tensor rather than Vec or Matrix" $
    inferListType
      (joinTypes mempty)
      [ TApp (TApp (TApp (TCon "Matrix") (TLit (LInt 2))) (TLit (LInt 2))) tInt,
        TApp (TApp (TApp (TCon "Matrix") (TLit (LInt 2))) (TLit (LInt 2))) tInt
      ]
      `shouldBe` TApp
        (TApp (TCon "Tensor") (TTypeList [TLit (LInt 2), TLit (LInt 2), TLit (LInt 2)]))
        tInt

  it "widens to a structural list when only some members are shaped" $
    inferListType
      (joinTypes mempty)
      [TApp (TApp (TCon "Vec") (TLit (LInt 1))) tInt, tInt]
      `shouldSatisfy` \inferred ->
        case collectHead inferred of
          Just name -> name == "List"
          Nothing -> False

  it "infers exact vector, matrix, and tensor shapes from list members" $ do
    inferListType (joinTypes mempty) [TLit (LInt 1), TLit (LInt 2)]
      `shouldBe` TApp (TApp (TCon "Vec") (TLit (LInt 2))) (TUnion [TLit (LInt 1), TLit (LInt 2)])
    inferListType
      (joinTypes mempty)
      [ TApp (TApp (TCon "Vec") (TLit (LInt 2))) tInt,
        TApp (TApp (TCon "Vec") (TLit (LInt 2))) tInt
      ]
      `shouldBe` TApp (TApp (TApp (TCon "Matrix") (TLit (LInt 2))) (TLit (LInt 2))) tInt

  it "infers heterogeneous lists as tuples" $
    inferListType (joinTypes mempty) [TLit (LInt 1), TLit (LString "x")]
      `shouldBe` TApp (TCon "Tuple") (TTypeList [TLit (LInt 1), TLit (LString "x")])

  it "preserves ragged nested tensors as vectors with dependent length unions" $
    inferListType
      (joinTypes mempty)
      [ TApp (TApp (TCon "Vec") (TLit (LInt 1))) tInt,
        TApp (TApp (TCon "Vec") (TLit (LInt 2))) tInt
      ]
      `shouldBe` tList (TApp (TApp (TCon "Vec") (TUnion [TLit (LInt 1), TLit (LInt 2)])) tInt)

  it "infers zero-width matrices from empty rows" $
    inferListType
      (joinTypes mempty)
      [ TApp (TApp (TCon "Vec") (TLit (LInt 0))) tDynamic,
        TApp (TApp (TCon "Vec") (TLit (LInt 0))) tDynamic
      ]
      `shouldBe` TApp (TApp (TApp (TCon "Matrix") (TLit (LInt 2))) (TLit (LInt 0))) tDynamic

  it "treats tensors as nested lists when widened structurally" $
    tensorListView
      (TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 2), TLit (LInt 3)])) tInt)
      `shouldBe` Just
        (tList (normalizeIndexedType (TApp (TApp (TCon "Vec") (TLit (LInt 3))) tInt)))

  it "widens tuples to structural lists through joined element types" $
    tupleListView
      (TApp (TCon "Tuple") (TTypeList [TLit (LInt 1), TLit (LString "x")]))
      `shouldBe` Just (tList (TUnion [TLit (LInt 1), TLit (LString "x")]))

  it "rejects obviously invalid indices inside aliases and annotations" $ do
    program <-
      expectRight $
        parseProgram
          "main.tynix"
          "type Bad = Tensor [\"x\"] Int; let xs :: Vec String Int; xs = [1]; in xs"
    validateProgramIndexedTypes program `shouldSatisfy` isLeft

  it "accepts nat-like unions and bounded lengths in indexed containers" $ do
    program <-
      expectRight $
        parseProgram
          "main.tynix"
          "type Batch t = Vec (2 | 3 | Range 4 8 Nat) t; let xs :: Vec (Range 2 4 Nat) Int; xs = [1 2 3]; in xs"
    validateProgramIndexedTypes program `shouldBe` Right ()

  it "accepts bounded matrix and tensor dimensions when every axis stays nat-like" $ do
    program <-
      expectRight $
        parseProgram
          "main.tynix"
          "type Grid t = Matrix (Range 1 2 Nat) (2 | 3) t; type Cube t = Tensor [2 (Range 1 2 Nat) 1] t; let grid :: Grid Int; grid = [[1 2] [3 4]]; in grid"
    validateProgramIndexedTypes program `shouldBe` Right ()

  it "accepts exact-zero ranges and zero dimensions as valid nat-like shapes" $ do
    program <-
      expectRight $
        parseProgram
          "main.tynix"
          "type EmptyVec t = Vec (Range 0 0 Nat) t; type EmptyGrid t = Matrix 0 (Range 0 2 Nat) t;"
    validateProgramIndexedTypes program `shouldBe` Right ()

  it "rejects non-nat ranges and malformed unit validators" $ do
    natRangeProgram <-
      expectRight $
        parseProgram
          "main.tynix"
          "let xs :: Vec (Range 0.0 2.0 Nat) Int; xs = [1 2]; in xs"
    floatIntRangeProgram <-
      expectRight $
        parseProgram
          "main.tynix"
          "type Bad = Range 0.0 2.0 Int; 1"
    invertedRangeProgram <-
      expectRight $
        parseProgram
          "main.tynix"
          "let xs :: Vec (Range 4 2 Nat) Int; xs = [1 2]; in xs"
    numberNatRangeProgram <-
      expectRight $
        parseProgram
          "main.tynix"
          "let xs :: Vec (Range 0 2 Number) Int; xs = [1 2]; in xs"
    unitProgram <-
      expectRight $
        parseProgram
          "main.tynix"
          "type Bad = Unit 1 Int; let timeout :: Unit \"ms\" (Range 0 10 String); timeout = 1; in timeout"
    unitShapeProgram <-
      expectRight $
        parseProgram
          "main.tynix"
          "let xs :: Tensor [Unit \"ms\" Nat] Int; xs = [[1]]; in xs"
    validateProgramIndexedTypes natRangeProgram `shouldSatisfy` isLeft
    validateProgramIndexedTypes floatIntRangeProgram `shouldSatisfy` isLeft
    validateProgramIndexedTypes invertedRangeProgram `shouldSatisfy` isLeft
    validateProgramIndexedTypes numberNatRangeProgram `shouldSatisfy` isLeft
    validateProgramIndexedTypes unitProgram `shouldSatisfy` isLeft
    validateProgramIndexedTypes unitShapeProgram `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _ = False

-- | Head constructor name of a left-associated application, if there is one.
collectHead :: Type -> Maybe Name
collectHead = \case
  TApp fun _ -> collectHead fun
  TCon name -> Just name
  _ -> Nothing

-- | Types built without ever mentioning `Vec`, `Matrix`, or `Tensor`.
newtype PlainType = PlainType Type
  deriving (Show)

instance Arbitrary PlainType where
  arbitrary = PlainType <$> sized (genType False)
  shrink (PlainType ty) = PlainType <$> shrinkType ty

-- | Types that may mention the indexed constructors anywhere.
newtype ShapedType = ShapedType Type
  deriving (Show)

instance Arbitrary ShapedType where
  arbitrary = ShapedType <$> sized (genType True)
  shrink (ShapedType ty) = ShapedType <$> shrinkType ty

genType :: Bool -> Int -> Gen Type
genType shaped size
  | size <= 0 = genLeaf shaped
  | otherwise =
      oneof $
        [ genLeaf shaped,
          TTypeList <$> resize 3 (listOf child),
          TFun <$> elements [One, Many] <*> child <*> child,
          TRecord . Map.fromList <$> resize 3 (listOf ((,) <$> genName <*> child)),
          TUnion <$> resize 3 (listOf child),
          TApp <$> child <*> child,
          TForall ["a"] <$> child,
          TConditional <$> child <*> child <*> child <*> child
        ]
          <> [genShape child | shaped]
  where
    child = genType shaped (size `div` 2)

genShape :: Gen Type -> Gen Type
genShape child =
  oneof
    [ vecType <$> genDimension <*> child,
      matrixType <$> genDimension <*> genDimension <*> child,
      tensorType <$> resize 3 (listOf genDimension) <*> child
    ]

vecType :: Type -> Type -> Type
vecType len = TApp (TApp (TCon "Vec") len)

matrixType :: Type -> Type -> Type -> Type
matrixType rows cols = TApp (TApp (TApp (TCon "Matrix") rows) cols)

tensorType :: [Type] -> Type -> Type
tensorType dims = TApp (TApp (TCon "Tensor") (TTypeList dims))

genDimension :: Gen Type
genDimension = TLit . LInt <$> chooseInteger (0, 4)

genLeaf :: Bool -> Gen Type
genLeaf shaped =
  oneof
    [ elements
        [ tString,
          tInt,
          tFloat,
          tNumber,
          tNat,
          tBool,
          tNull,
          tPath,
          tAny,
          tDynamic,
          tUnknown
        ],
      TVar <$> genName,
      TCon
        <$> if shaped
          then elements ["List", "Vec", "Matrix", "Tensor", "Tuple"]
          else elements ["List", "Tuple", "Box"],
      TLit . LInt <$> chooseInteger (0, 8),
      TLit . LString <$> genName
    ]

genName :: Gen Name
genName =
  Text.pack <$> ((:) <$> elements ['a' .. 'd'] <*> listOf (elements ['0' .. '2']))

shrinkType :: Type -> [Type]
shrinkType = \case
  TTypeList items -> items
  TFun _ input output -> [input, output]
  TRecord fields -> Map.elems fields
  TUnion members -> members
  TApp fun arg -> [fun, arg]
  TForall _ body -> [body]
  TConditional a b c d -> [a, b, c, d]
  _ -> []
