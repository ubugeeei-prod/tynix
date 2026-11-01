{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Laws for the shared type representation.
--
-- Substitution, meta closure, and free-variable collection sit underneath the
-- checker, the subtyping relation, and every emitted declaration, so their
-- invariants are worth stating directly rather than inferring from end-to-end
-- behaviour. Several of these also pin down the short-circuits the hot paths
-- rely on: skipping an empty substitution is only safe because applying one is
-- the identity.
module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Test.Hspec
import Test.QuickCheck
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "substituteTypeVars" $ do
    it "is the identity for an empty substitution" $
      property $ \(AnyType ty) ->
        substituteTypeVars Map.empty ty === ty

    it "replaces every free occurrence of a bound name" $
      substituteTypeVars
        (Map.fromList [("a", tInt)])
        (TFun Many (TVar "a") (TRecord (Map.fromList [("x", TVar "a"), ("y", TVar "b")])))
        `shouldBe` TFun Many tInt (TRecord (Map.fromList [("x", tInt), ("y", TVar "b")]))

    it "does not substitute under a shadowing forall" $
      substituteTypeVars
        (Map.fromList [("a", tInt)])
        (TForall ["a"] (TFun Many (TVar "a") (TVar "a")))
        `shouldBe` TForall ["a"] (TFun Many (TVar "a") (TVar "a"))

    it "still substitutes names a forall does not bind" $
      substituteTypeVars
        (Map.fromList [("b", tInt)])
        (TForall ["a"] (TFun Many (TVar "a") (TVar "b")))
        `shouldBe` TForall ["a"] (TFun Many (TVar "a") tInt)

    it "removes the substituted names from the free variables" $
      property $ \(AnyType ty) ->
        let substituted =
              substituteTypeVars
                (Map.fromList [(name, tInt) | name <- Set.toList (freeTypeVars ty)])
                ty
         in freeTypeVars substituted === Set.empty

  describe "substituteMetas" $ do
    it "is the identity for an empty substitution" $
      property $ \(AnyType ty) ->
        substituteMetas Map.empty ty === ty

    it "chases metas that resolve to other metas" $
      substituteMetas
        (Map.fromList [(0, TMeta 1), (1, TMeta 2), (2, tString)])
        (TFun Many (TMeta 0) (TMeta 2))
        `shouldBe` TFun Many tString tString

    it "leaves unsolved metas in place" $
      substituteMetas (Map.fromList [(0, tInt)]) (TFun Many (TMeta 0) (TMeta 1))
        `shouldBe` TFun Many tInt (TMeta 1)

    it "solves every meta it has an entry for" $
      property $ \(AnyType ty) ->
        let solution = Map.fromList [(meta, tInt) | meta <- Set.toList (freeMetas ty)]
         in freeMetas (substituteMetas solution ty) === Set.empty

  describe "freeTypeVars" $ do
    it "excludes forall-bound names" $
      freeTypeVars (TForall ["a"] (TFun Many (TVar "a") (TVar "b")))
        `shouldBe` Set.fromList ["b"]

    it "collects from every structural position" $
      freeTypeVars
        ( TRecord
            ( Map.fromList
                [ ("fun", TFun Many (TVar "a") (TVar "b")),
                  ("union", TUnion [TVar "c", tInt]),
                  ("app", TApp (TVar "d") (TVar "e")),
                  ("list", TTypeList [TVar "f"]),
                  ("cond", TConditional (TVar "g") (TVar "h") (TVar "i") (TVar "j"))
                ]
            )
        )
        `shouldBe` Set.fromList ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]

  describe "freeMetas" $ do
    it "collects metas from every structural position" $
      freeMetas
        ( TRecord
            ( Map.fromList
                [ ("fun", TFun One (TMeta 1) (TMeta 2)),
                  ("union", TUnion [TMeta 3]),
                  ("app", TApp (TMeta 4) (TMeta 5)),
                  ("list", TTypeList [TMeta 6]),
                  ("forall", TForall ["a"] (TMeta 7)),
                  ("cond", TConditional (TMeta 8) (TMeta 9) (TMeta 10) (TMeta 11))
                ]
            )
        )
        `shouldBe` Set.fromList [1 .. 11]

    it "agrees with freeMetasScheme" $
      property $ \(AnyType ty) ->
        freeMetasScheme (Scheme [] ty) === freeMetas ty

  describe "closeMetas" $ do
    it "leaves no unsolved metas behind" $
      property $ \(AnyType ty) ->
        freeMetas (schemeType (closeMetas ty)) === Set.empty

    it "quantifies exactly one variable per distinct meta" $
      property $ \(AnyType ty) ->
        length (schemeVars (closeMetas ty)) === Set.size (freeMetas ty)

    it "names variables deterministically from the lowest meta up" $
      closeMetas (TFun Many (TMeta 7) (TFun Many (TMeta 3) (TMeta 7)))
        `shouldBe` Scheme ["t0", "t1"] (TFun Many (TVar "t1") (TFun Many (TVar "t0") (TVar "t1")))

    it "is stable under renumbering of the metas" $
      closeMetas (TFun Many (TMeta 100) (TMeta 200))
        `shouldBe` closeMetas (TFun Many (TMeta 1) (TMeta 2))

    it "leaves meta-free types untouched" $
      property $ \(AnyType ty) ->
        Set.null (freeMetas ty) ==> closeMetas ty === Scheme [] ty

  describe "eraseForall" $ do
    it "strips stacked top-level quantifiers" $
      eraseForall (TForall ["a"] (TForall ["b"] (TFun Many (TVar "a") (TVar "b"))))
        `shouldBe` TFun Many (TVar "a") (TVar "b")

    it "leaves nested quantifiers alone" $
      eraseForall (TRecord (Map.fromList [("f", TForall ["a"] (TVar "a"))]))
        `shouldBe` TRecord (Map.fromList [("f", TForall ["a"] (TVar "a"))])

    it "is idempotent" $
      property $ \(AnyType ty) ->
        eraseForall (eraseForall ty) === eraseForall ty

  describe "schemeFromAnnotation" $ do
    it "lifts a written forall into the scheme binder" $
      schemeFromAnnotation (TForall ["a", "b"] (TFun Many (TVar "a") (TVar "b")))
        `shouldBe` Scheme ["a", "b"] (TFun Many (TVar "a") (TVar "b"))

    it "leaves a monomorphic annotation unquantified" $
      schemeFromAnnotation tInt `shouldBe` Scheme [] tInt

  describe "smart constructors" $
    it "spells List as an application of the built-in constructor" $
      tList tInt `shouldBe` TApp (TCon "List") tInt

-- | Unconstrained generator covering every constructor of 'Type', including
-- the ones the checker only produces internally (metas, infer binders, and
-- conditional types).
newtype AnyType = AnyType Type
  deriving (Show)

instance Arbitrary AnyType where
  arbitrary = sized (fmap AnyType . genType)
  shrink (AnyType ty) = AnyType <$> shrinkType ty

genType :: Int -> Gen Type
genType size
  | size <= 0 = genLeaf
  | otherwise =
      oneof
        [ genLeaf,
          TTypeList <$> resize 3 (listOf child),
          TFun <$> elements [One, Many] <*> child <*> child,
          TRecord . Map.fromList <$> resize 3 (listOf ((,) <$> genName <*> child)),
          TUnion <$> resize 3 (listOf child),
          TApp <$> child <*> child,
          TForall <$> resize 2 (listOf genName) <*> child,
          TConditional <$> child <*> child <*> child <*> child
        ]
  where
    child = genType (size `div` 2)

genLeaf :: Gen Type
genLeaf =
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
      TCon <$> genName,
      TMeta <$> chooseInt (0, 8),
      TInfer <$> genName,
      TLit . LInt <$> chooseInteger (-100, 100),
      TLit . LFloat . fromIntegral <$> chooseInt (-100, 100),
      TLit . LBool <$> arbitrary,
      TLit . LString <$> genName
    ]

genName :: Gen Name
genName =
  Text.pack <$> ((:) <$> elements ['a' .. 'f'] <*> listOf (elements ['0' .. '3']))

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
