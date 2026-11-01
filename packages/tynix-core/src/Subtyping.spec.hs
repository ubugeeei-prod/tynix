{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Alias (AliasEnv, mkAliasEnv)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Subtyping (isConsistent, isSubtype, joinTypes, lookupRecordField, resolveType)
import Test.Hspec
import Test.QuickCheck
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "subtyping and type reduction" $ do
  it "keeps generated concrete types reflexive under subtyping" $
    property $ \(ConcreteType ty) ->
      isSubtype mempty ty ty === True

  it "resolves types to a fixed point" $
    property $ \(ConcreteType ty) ->
      let env = sampleAliasEnv
       in resolveType env (resolveType env ty) === resolveType env ty

  it "treats consistency as symmetric" $
    property $ \(ConcreteType left) (ConcreteType right) ->
      isConsistent mempty left right === isConsistent mempty right left

  it "joins to a common supertype of both sides" $
    property $ \(ConcreteType left) (ConcreteType right) ->
      let joined = joinTypes mempty left right
       in counterexample ("joined as " <> show joined) $
            isSubtype mempty left joined .&&. isSubtype mempty right joined

  it "keeps a join with itself at the same type" $
    property $ \(ConcreteType ty) ->
      joinTypes mempty ty ty === resolveType mempty ty

  it "decides union subtyping member-by-member" $
    -- The relation is "every left member is covered by some right member".
    -- The implementation short-circuits through a set of the right-hand
    -- members, so state the definition separately and require them to agree.
    -- The pool is deliberately free of gradual types: `dynamic` answers
    -- through consistency instead of subtyping, which the case below records.
    property $ \(UnionPair leftMembers rightMembers) ->
      isSubtype mempty (TUnion leftMembers) (TUnion rightMembers)
        === all (\left -> any (isSubtype mempty left) rightMembers) leftMembers

  it "keeps dynamic out of subtyping even when a union offers it" $ do
    -- A one-member union collapses to that member, and `dynamic` on the left
    -- is never a subtype of anything it is not equal to. Gradual code reaches
    -- the same conclusion through consistency.
    isSubtype mempty tDynamic tDynamic `shouldBe` True
    isSubtype mempty tDynamic (TUnion [tDynamic, TLit (LString "a")]) `shouldBe` False
    isConsistent mempty tDynamic (TUnion [tDynamic, TLit (LString "a")]) `shouldBe` True

  it "treats literal values as subtypes of primitive constructors" $ do
    isSubtype mempty (TLit (LString "tynix")) tString `shouldBe` True
    isSubtype mempty (TLit (LInt 1)) tInt `shouldBe` True
    isSubtype mempty (TLit (LInt 1)) tNat `shouldBe` True
    isSubtype mempty (TLit (LInt (-1))) tNat `shouldBe` False
    isSubtype mempty (TLit (LFloat 1.5)) tFloat `shouldBe` True
    isSubtype mempty (TLit (LFloat 1.5)) tNat `shouldBe` False
    isSubtype mempty (TLit (LFloat 1.5)) tNumber `shouldBe` True
    isSubtype mempty (TLit (LBool False)) tBool `shouldBe` True

  it "supports structural width subtyping for records" $
    isSubtype mempty (TRecord (Map.fromList [("a", tInt), ("b", tString)])) (TRecord (Map.fromList [("a", tInt)]))
      `shouldBe` True

  it "uses contravariant inputs, covariant outputs, and multiplicity subtyping for functions" $ do
    let broad = TFun Many tString (TRecord (Map.fromList [("name", tString), ("value", tInt)]))
        narrow = TFun Many (TLit (LString "x")) (TRecord (Map.fromList [("name", tString)]))
        linear = TFun One tString tString
        unrestricted = TFun Many tString tString
    isSubtype mempty broad narrow `shouldBe` True
    isSubtype mempty linear unrestricted `shouldBe` True
    isSubtype mempty unrestricted linear `shouldBe` False

  it "reduces generic aliases including HKT-shaped applications" $ do
    let env =
          mkAliasEnv
            [ plainAlias "Box" ["T"] (TRecord (Map.fromList [("value", TVar "T")])),
              plainAlias "Apply" ["F", "A"] (TApp (TVar "F") (TVar "A"))
            ]
    resolveType env (TApp (TCon "Box") tInt)
      `shouldBe` TRecord (Map.fromList [("value", tInt)])
    resolveType env (TApp (TApp (TCon "Apply") (TCon "List")) tString)
      `shouldBe` tList tString

  it "reduces nullary aliases without requiring synthetic arguments" $ do
    let env = mkAliasEnv [plainAlias "Inputs" [] (TRecord (Map.fromList [("self", tDynamic)]))]
    resolveType env (TCon "Inputs")
      `shouldBe` TRecord (Map.fromList [("self", tDynamic)])

  it "normalizes indexed containers to canonical tensor shapes" $ do
    resolveType mempty (TApp (TApp (TCon "Vec") (TLit (LInt 3))) tInt)
      `shouldBe` TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 3)])) tInt
    resolveType mempty (TApp (TApp (TApp (TCon "Matrix") (TLit (LInt 2))) (TLit (LInt 4))) tString)
      `shouldBe` TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 2), TLit (LInt 4)])) tString

  it "evaluates conditional types by pattern matching with infer" $ do
    let elemOf = TConditional (tList tInt) (TApp (TCon "List") (TInfer "U")) (TVar "U") tDynamic
    resolveType mempty elemOf `shouldBe` tInt
    let rowsOfMatrix =
          TConditional
            (TApp (TApp (TApp (TCon "Matrix") (TLit (LInt 2))) (TLit (LInt 3))) tInt)
            (TApp (TApp (TCon "Tensor") (TTypeList [TInfer "Rows", TInfer "Cols"])) (TInfer "Elem"))
            (TVar "Rows")
            tDynamic
    resolveType mempty rowsOfMatrix `shouldBe` TLit (LInt 2)

  it "does not reduce conditional types through gradual consistency alone" $ do
    let dynamicCheck = TConditional tDynamic tString tInt tBool
    resolveType mempty dynamicCheck `shouldBe` tBool

  it "joins compatible types and falls back to unions" $ do
    joinTypes mempty (TLit (LString "x")) tString `shouldBe` tString
    joinTypes mempty tInt tFloat `shouldBe` tNumber
    joinTypes mempty tInt tString `shouldBe` TUnion [tInt, tString]

  it "treats tensors as nested lists for subtyping and joins" $ do
    let vec2 = TApp (TApp (TCon "Vec") (TLit (LInt 2))) tInt
        vec3 = TApp (TApp (TCon "Vec") (TLit (LInt 3))) tInt
        matrix22 = TApp (TApp (TApp (TCon "Matrix") (TLit (LInt 2))) (TLit (LInt 2))) tInt
    isSubtype mempty vec2 (tList tInt) `shouldBe` True
    isSubtype mempty matrix22 (tList vec2) `shouldBe` True
    joinTypes mempty vec2 vec3 `shouldBe` TApp (TApp (TCon "Vec") (TUnion [TLit (LInt 2), TLit (LInt 3)])) tInt

  it "joins matrix shapes dimension-wise when ranks agree" $ do
    let matrix22 = TApp (TApp (TApp (TCon "Matrix") (TLit (LInt 2))) (TLit (LInt 2))) tInt
        matrix32 = TApp (TApp (TApp (TCon "Matrix") (TLit (LInt 3))) (TLit (LInt 2))) tInt
    joinTypes mempty matrix22 matrix32
      `shouldBe` TApp (TApp (TApp (TCon "Matrix") (TUnion [TLit (LInt 2), TLit (LInt 3)])) (TLit (LInt 2))) tInt

  it "treats statically empty tensors as compatible with any element type" $ do
    let emptyVec = TApp (TApp (TCon "Vec") (TLit (LInt 0))) tDynamic
        bounded = TApp (TApp (TCon "Vec") (TApp (TApp (TApp (TCon "Range") (TLit (LInt 0))) (TLit (LInt 2))) tNat)) tInt
    isSubtype mempty emptyVec bounded `shouldBe` True

  it "rejects nat subtyping when integer ranges cross below zero" $ do
    let signed = TApp (TApp (TApp (TCon "Range") (TLit (LInt (-1)))) (TLit (LInt 2))) tInt
        positive = TApp (TApp (TApp (TCon "Range") (TLit (LInt 0))) (TLit (LInt 2))) tNat
    isSubtype mempty signed tNat `shouldBe` False
    isSubtype mempty signed positive `shouldBe` False

  it "checks numeric refinements and unit wrappers structurally" $ do
    let smallNat = TApp (TApp (TApp (TCon "Range") (TLit (LInt 0))) (TLit (LInt 10))) tNat
        widerNat = TApp (TApp (TApp (TCon "Range") (TLit (LInt 0))) (TLit (LInt 20))) tNat
        smallFloat = TApp (TApp (TApp (TCon "Range") (TLit (LFloat 0.0))) (TLit (LFloat 1.0))) tFloat
        widerNumber = TApp (TApp (TApp (TCon "Range") (TLit (LFloat 0.0))) (TLit (LFloat 10.0))) tNumber
        timeout = TApp (TApp (TCon "Unit") (TLit (LString "ms"))) smallNat
        timeoutSeconds = TApp (TApp (TCon "Unit") (TLit (LString "s"))) smallNat
        widerTimeout = TApp (TApp (TCon "Unit") (TLit (LString "ms"))) tNumber
    isSubtype mempty (TLit (LInt 0)) smallNat `shouldBe` True
    isSubtype mempty (TLit (LInt 3)) smallNat `shouldBe` True
    isSubtype mempty (TLit (LInt 10)) smallNat `shouldBe` True
    isSubtype mempty (TLit (LInt 11)) smallNat `shouldBe` False
    isSubtype mempty smallNat tNat `shouldBe` True
    isSubtype mempty smallNat widerNat `shouldBe` True
    isSubtype mempty (TLit (LFloat 0.5)) smallFloat `shouldBe` True
    isSubtype mempty (TLit (LFloat 1.0)) smallFloat `shouldBe` True
    isSubtype mempty (TLit (LFloat 1.5)) smallFloat `shouldBe` False
    isSubtype mempty smallFloat widerNumber `shouldBe` True
    isSubtype mempty (TLit (LInt 3)) timeout `shouldBe` True
    isSubtype mempty timeout widerTimeout `shouldBe` True
    isSubtype mempty timeout timeoutSeconds `shouldBe` False
    joinTypes mempty timeout (TApp (TApp (TCon "Unit") (TLit (LString "ms"))) widerNat)
      `shouldBe` TApp (TApp (TCon "Unit") (TLit (LString "ms"))) widerNat
    joinTypes mempty timeout timeoutSeconds
      `shouldBe` TUnion [timeout, timeoutSeconds]

  it "preserves enclosing refinements when joining enclosed literals" $ do
    let smallNat = TApp (TApp (TApp (TCon "Range") (TLit (LInt 0))) (TLit (LInt 10))) tNat
        timeout = TApp (TApp (TCon "Unit") (TLit (LString "ms"))) smallNat
    joinTypes mempty (TLit (LInt 3)) smallNat `shouldBe` smallNat
    joinTypes mempty (TLit (LInt 3)) timeout `shouldBe` timeout
    isSubtype mempty (TLit (LString "oops")) timeout `shouldBe` False

  it "treats tuples as fixed heterogeneous lists" $ do
    let pair = TApp (TCon "Tuple") (TTypeList [tInt, tString])
        pairLits = TApp (TCon "Tuple") (TTypeList [TLit (LInt 1), TLit (LString "x")])
    isSubtype mempty pairLits pair `shouldBe` True
    isSubtype mempty pairLits (tList (TUnion [tInt, tString])) `shouldBe` True
    joinTypes mempty pairLits pair
      `shouldBe` TApp (TCon "Tuple") (TTypeList [tInt, tString])

  it "joins shared fields across union members and rejects partial records" $ do
    lookupRecordField mempty (TUnion [TRecord (Map.fromList [("value", TLit (LInt 1))]), TRecord (Map.fromList [("value", tString)])]) "value"
      `shouldBe` Just (TUnion [TLit (LInt 1), tString])
    lookupRecordField mempty (TUnion [TRecord (Map.fromList [("value", tInt)]), TRecord (Map.fromList [("other", tString)])]) "value"
      `shouldBe` Nothing

  it "treats dynamic as consistent but not as a subtype of concrete types" $ do
    isConsistent mempty tDynamic tString `shouldBe` True
    isSubtype mempty tDynamic tString `shouldBe` False

  it "models any as unsound and unknown as a top type" $ do
    isConsistent mempty tAny tString `shouldBe` True
    isSubtype mempty tAny tString `shouldBe` True
    isSubtype mempty tString tAny `shouldBe` True
    isSubtype mempty tString tUnknown `shouldBe` True
    isSubtype mempty tUnknown tString `shouldBe` False
    isSubtype mempty tUnknown tAny `shouldBe` True
    joinTypes mempty tAny tString `shouldBe` tAny
    joinTypes mempty tUnknown tString `shouldBe` tUnknown

  it "joins exact indexed lengths into bounded or union-like shapes" $ do
    let exact = TApp (TApp (TCon "Vec") (TLit (LInt 2))) tInt
        bounded = TApp (TApp (TCon "Vec") (TApp (TApp (TApp (TCon "Range") (TLit (LInt 2))) (TLit (LInt 4))) tNat)) tInt
    joinTypes mempty exact bounded `shouldBe` bounded

  it "joins exact literals into exact unit-preserving vectors" $ do
    let timeout = TApp (TApp (TCon "Unit") (TLit (LString "ms"))) tNat
        leftVec = TApp (TApp (TCon "Vec") (TLit (LInt 1))) timeout
        rightVec = TApp (TApp (TCon "Vec") (TLit (LInt 2))) timeout
    joinTypes mempty leftVec rightVec
      `shouldBe` TApp (TApp (TCon "Vec") (TUnion [TLit (LInt 1), TLit (LInt 2)])) timeout

newtype ConcreteType = ConcreteType Type
  deriving (Show)

instance Arbitrary ConcreteType where
  arbitrary = sized (fmap ConcreteType . genType)
  shrink (ConcreteType ty) = ConcreteType <$> shrinkType ty

genType :: Int -> Gen Type
genType size
  | size <= 0 = genLeafType
  | otherwise =
      oneof
        [ genLeafType,
          tList <$> child,
          TFun <$> elements [One, Many] <*> child <*> child,
          TRecord . Map.fromList <$> resize 3 (listOf ((,) <$> genName <*> child))
        ]
  where
    child = genType (size `div` 2)

genLeafType :: Gen Type
genLeafType =
  oneof
    [ elements [tString, tInt, tFloat, tNumber, tNat, tBool, tNull, tPath, tAny, tDynamic, tUnknown],
      TLit . LInt <$> chooseInteger (0, 1000),
      TLit . LBool <$> arbitrary,
      TLit . LString <$> genName
    ]

genName :: Gen Name
genName = Text.pack <$> ((:) <$> elements ['a' .. 'z'] <*> listOf (elements (['a' .. 'z'] <> ['0' .. '9'])))

shrinkType :: Type -> [Type]
shrinkType = \case
  TApp _ item -> [item]
  TFun _ input output -> [input, output]
  TRecord fields -> Map.elems fields
  _ -> []

-- | Environment used by the fixed-point property: nullary and parameterized
-- aliases, a chain, and a self-referential alias that must not diverge.
sampleAliasEnv :: AliasEnv
sampleAliasEnv =
  mkAliasEnv
    [ plainAlias "Leaf" [] tInt,
      plainAlias "Box" ["a"] (TRecord (Map.fromList [("value", TVar "a")])),
      plainAlias "A" [] (TCon "B"),
      plainAlias "B" [] tString,
      plainAlias "Loop" [] (TCon "Loop")
    ]

-- | Two union member lists drawn from a shared pool, so overlap between them
-- is common enough to exercise both sides of the set short-circuit.
data UnionPair = UnionPair [Type] [Type]
  deriving (Show)

instance Arbitrary UnionPair where
  arbitrary =
    UnionPair
      <$> resize 5 (listOf (elements unionPool))
      <*> resize 5 (listOf (elements unionPool))
  shrink (UnionPair left right) =
    [UnionPair left' right | left' <- shrinkList (const []) left]
      <> [UnionPair left right' | right' <- shrinkList (const []) right]

unionPool :: [Type]
unionPool =
  [ tInt,
    tString,
    tBool,
    tNat,
    tNumber,
    TLit (LInt 1),
    TLit (LInt 2),
    TLit (LString "a"),
    TRecord (Map.fromList [("x", tInt)])
  ]
