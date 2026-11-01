{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Alias
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Test.Hspec
import Test.QuickCheck
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "flattenUnion" $ do
    it "leaves non-union types unchanged" $
      flattenUnion tInt `shouldBe` tInt

    it "collapses single-element unions to the underlying type" $
      flattenUnion (TUnion [tInt]) `shouldBe` tInt

    it "lifts nested unions into a flat list" $
      flattenUnion (TUnion [tInt, TUnion [tString, TUnion [tBool, tNat]]])
        `shouldBe` TUnion [tInt, tString, tBool, tNat]

    it "removes duplicates while preserving the first-seen order" $
      flattenUnion (TUnion [tInt, tString, tInt, TUnion [tString, tBool]])
        `shouldBe` TUnion [tInt, tString, tBool]

    it "preserves an empty union representation" $
      flattenUnion (TUnion []) `shouldBe` TUnion []

    it "keeps a repeated member at the position of its last occurrence" $
      flattenUnion (TUnion [tInt, tString, tInt])
        `shouldBe` TUnion [tString, tInt]

    it "is idempotent" $
      property $ \(UnionMembers members) ->
        flattenUnion (flattenUnion (TUnion members)) === flattenUnion (TUnion members)

    it "never leaves a duplicate member behind" $
      property $ \(UnionMembers members) ->
        case flattenUnion (TUnion members) of
          TUnion flattened -> flattened === nub flattened
          _ -> property True

    it "preserves the set of leaf members" $
      property $ \(UnionMembers members) ->
        memberSet (flattenUnion (TUnion members)) === memberSet (TUnion members)

    it "flattens arbitrarily nested unions in one pass" $
      flattenUnion (TUnion [TUnion [TUnion [tInt, tString], tBool], tNull])
        `shouldBe` TUnion [tInt, tString, tBool, tNull]

  describe "collectApps" $ do
    it "returns the type unchanged when there are no applications" $
      collectApps tInt `shouldBe` (tInt, [])

    it "splits a left-associated application chain head-first" $
      collectApps (TApp (TApp (TCon "Pair") tInt) tString)
        `shouldBe` (TCon "Pair", [tInt, tString])

    it "stops at the leftmost non-application head" $
      collectApps (TApp (TVar "f") tInt)
        `shouldBe` (TVar "f", [tInt])

  describe "expandAliases" $ do
    let env =
          mkAliasEnv
            [ plainAlias "Id" [] tInt,
              plainAlias "Pair" ["a", "b"] (TRecord (Map.fromList [("fst", TVar "a"), ("snd", TVar "b")])),
              plainAlias "Forever" [] (TCon "Forever")
            ]

    it "leaves unrelated types untouched" $
      expandAliases env tBool `shouldBe` tBool

    it "expands nullary aliases recursively" $
      expandAliases env (TCon "Id") `shouldBe` tInt

    it "substitutes parameters when reducing parameterized aliases" $
      expandAliases env (TApp (TApp (TCon "Pair") tInt) tString)
        `shouldBe` TRecord (Map.fromList [("fst", tInt), ("snd", tString)])

    it "preserves leftover arguments past the alias arity" $
      expandAliases env (TApp (TApp (TApp (TCon "Pair") tInt) tString) tBool)
        `shouldBe` TApp (TRecord (Map.fromList [("fst", tInt), ("snd", tString)])) tBool

    it "terminates on self-referential aliases instead of looping forever" $
      expandAliases env (TCon "Forever") `shouldBe` TCon "Forever"

    it "expands aliases nested far below the old structural budget" $ do
      -- The recursion budget counts expansions, not structural depth, so a
      -- deeply nested type still resolves its aliases all the way down.
      let env = mkAliasEnv [plainAlias "Leaf" [] tInt]
          nest 0 = TCon "Leaf"
          nest n = TRecord (Map.fromList [("next", nest (n - 1 :: Int))])
          expected 0 = tInt
          expected n = TRecord (Map.fromList [("next", expected (n - 1 :: Int))])
      expandAliases env (nest 64) `shouldBe` expected 64

    it "is idempotent for every generated environment" $
      property $ \(AliasCase env ty) ->
        expandAliases env (expandAliases env ty) === expandAliases env ty

    it "rewrites parameters inside nested records" $
      expandAliases env (TRecord (Map.fromList [("value", TApp (TApp (TCon "Pair") tInt) tBool)]))
        `shouldBe` TRecord
          ( Map.fromList
              [ ("value", TRecord (Map.fromList [("fst", tInt), ("snd", tBool)]))
              ]
          )

  describe "matchPattern" $ do
    it "binds a single infer variable to the matched type" $
      matchPattern tInt (TInfer "a")
        `shouldBe` Just (Map.fromList [("a", tInt)])

    it "matches concrete patterns without binding when the shape is equal" $
      matchPattern (TRecord (Map.fromList [("value", tInt)])) (TRecord (Map.fromList [("value", tInt)]))
        `shouldBe` Just Map.empty

    it "captures inner positions through structural patterns" $
      matchPattern
        (TFun Many tInt tString)
        (TFun Many (TInfer "argument") (TInfer "result"))
        `shouldBe` Just (Map.fromList [("argument", tInt), ("result", tString)])

    it "rejects record patterns whose required fields are missing in the actual" $
      matchPattern (TRecord (Map.fromList [("foo", tInt)])) (TRecord (Map.fromList [("bar", TInfer "x")]))
        `shouldBe` Nothing

    it "rejects type-list patterns with a different arity" $
      matchPattern (TTypeList [tInt, tString]) (TTypeList [TInfer "x"])
        `shouldBe` Nothing

    it "requires repeated infer variables to bind to equal types" $ do
      matchPattern (TTypeList [tInt, tInt]) (TTypeList [TInfer "x", TInfer "x"])
        `shouldBe` Just (Map.fromList [("x", tInt)])
      matchPattern (TTypeList [tInt, tString]) (TTypeList [TInfer "x", TInfer "x"])
        `shouldBe` Nothing

    it "matches type application patterns positionally" $
      matchPattern
        (TApp (TCon "List") tInt)
        (TApp (TCon "List") (TInfer "element"))
        `shouldBe` Just (Map.fromList [("element", tInt)])

-- | Collect the leaf members of a (possibly nested) union so flattening can be
-- compared by content rather than by shape or order.
memberSet :: Type -> Set Type
memberSet = Set.fromList . go
  where
    go (TUnion members) = concatMap go members
    go other = [other]

-- | Members for a union, drawn from a small pool so duplicates and nesting are
-- both common in generated cases.
newtype UnionMembers = UnionMembers [Type]
  deriving (Show)

instance Arbitrary UnionMembers where
  arbitrary = UnionMembers <$> resize 6 (listOf (sized genUnionMember))
  shrink (UnionMembers members) = UnionMembers <$> shrinkList (const []) members

genUnionMember :: Int -> Gen Type
genUnionMember size
  | size <= 0 = genPoolType
  | otherwise =
      frequency
        [ (3, genPoolType),
          (1, TUnion <$> resize 3 (listOf (genUnionMember (size `div` 2))))
        ]

genPoolType :: Gen Type
genPoolType = elements [tInt, tString, tBool, tNull, tNat, TLit (LInt 1), TLit (LString "x")]

-- | An alias environment paired with a type that mentions it.
data AliasCase = AliasCase AliasEnv Type

instance Show AliasCase where
  show (AliasCase env ty) = "AliasCase " <> show (Map.keys env) <> " " <> show ty

instance Arbitrary AliasCase where
  arbitrary = do
    env <- elements aliasEnvironments
    ty <- sized genAliasedType
    pure (AliasCase env ty)

-- | A few environments that between them cover nullary aliases, parameterized
-- aliases, chains, higher-kinded encodings, and self-reference.
aliasEnvironments :: [AliasEnv]
aliasEnvironments =
  [ mempty,
    mkAliasEnv [plainAlias "Leaf" [] tInt],
    mkAliasEnv
      [ plainAlias "Box" ["a"] (TRecord (Map.fromList [("value", TVar "a")])),
        plainAlias "Pair" ["a", "b"] (TRecord (Map.fromList [("fst", TVar "a"), ("snd", TVar "b")]))
      ],
    mkAliasEnv
      [ plainAlias "A" [] (TCon "B"),
        plainAlias "B" [] (TCon "C"),
        plainAlias "C" [] tString
      ],
    mkAliasEnv
      [ plainAlias "Apply" ["f", "a"] (TApp (TVar "f") (TVar "a")),
        plainAlias "Box" ["a"] (TRecord (Map.fromList [("value", TVar "a")]))
      ],
    mkAliasEnv [plainAlias "Loop" [] (TCon "Loop")],
    mkAliasEnv [plainAlias "Grow" ["a"] (TApp (TCon "Grow") (TVar "a"))]
  ]

genAliasedType :: Int -> Gen Type
genAliasedType size
  | size <= 0 = genAliasedLeaf
  | otherwise =
      oneof
        [ genAliasedLeaf,
          TApp <$> child <*> child,
          TFun Many <$> child <*> child,
          TRecord . Map.fromList <$> resize 2 (listOf ((,) <$> genAliasName <*> child)),
          TUnion <$> resize 3 (listOf child),
          TForall ["a"] <$> child
        ]
  where
    child = genAliasedType (size `div` 2)

genAliasedLeaf :: Gen Type
genAliasedLeaf =
  oneof
    [ elements [tInt, tString, tBool, tDynamic],
      TCon <$> elements ["Leaf", "Box", "Pair", "A", "B", "C", "Apply", "Loop", "Grow", "Unknown"],
      TVar <$> genAliasName
    ]

genAliasName :: Gen Name
genAliasName = Text.pack <$> elements ["a", "b", "value", "fst"]
