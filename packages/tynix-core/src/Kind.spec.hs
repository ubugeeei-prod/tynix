{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Map.Strict qualified as Map
import Kind
import Parser (parseProgram)
import Syntax (Program (programAliases))
import Test.Hspec
import TestSupport (expectRight, source)
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "higher-kinded kind inference" $ do
  it "accepts aliases that abstract over unary type constructors" $ do
    kinds <-
      expectRight $
        inferAliasKinds
          [ plainAlias "Box" ["a"] (TRecord (Map.fromList [("value", TVar "a")])),
            plainAlias "Apply" ["f", "a"] (TApp (TVar "f") (TVar "a")),
            plainAlias "Twice" ["f", "a"] (TApp (TVar "f") (TApp (TVar "f") (TVar "a")))
          ]
    inferTypeKind kinds (TApp (TApp (TCon "Apply") (TCon "Box")) tInt)
      `shouldBe` Right KType
    inferTypeKind kinds (TApp (TApp (TCon "Twice") (TCon "List")) tInt)
      `shouldBe` Right KType

  it "accepts aliases that return higher-kinded constructors" $ do
    kinds <-
      expectRight $
        inferAliasKinds
          [ plainAlias "Id" ["f"] (TVar "f"),
            plainAlias "Apply" ["f", "a"] (TApp (TVar "f") (TVar "a"))
          ]
    inferTypeKind kinds (TApp (TApp (TCon "Apply") (TApp (TCon "Id") (TCon "List"))) tInt)
      `shouldBe` Right KType

  it "rejects applying concrete types as constructors" $
    inferAliasKinds [plainAlias "Bad" ["a"] (TApp tInt (TVar "a"))]
      `shouldSatisfy` isLeft

  it "rejects oversaturated constructors" $
    inferTypeKind mempty (TApp (tList tInt) tString)
      `shouldSatisfy` isLeft

  it "gives every builtin constructor the arity its surface syntax implies" $ do
    inferTypeKind mempty tInt `shouldBe` Right KType
    inferTypeKind mempty (TCon "List") `shouldBe` Right (KFun KType KType)
    inferTypeKind mempty (TCon "Vec") `shouldBe` Right (KFun KType (KFun KType KType))
    inferTypeKind mempty (TCon "Matrix")
      `shouldBe` Right (KFun KType (KFun KType (KFun KType KType)))
    inferTypeKind mempty (TCon "Tensor") `shouldBe` Right (KFun KType (KFun KType KType))
    inferTypeKind mempty (TCon "Tuple") `shouldBe` Right (KFun KType KType)
    inferTypeKind mempty (TCon "Range")
      `shouldBe` Right (KFun KType (KFun KType (KFun KType KType)))
    inferTypeKind mempty (TCon "Unit") `shouldBe` Right (KFun KType (KFun KType KType))

  it "gives every ground type form kind Type" $
    mapM_
      (\ty -> inferTypeKind mempty ty `shouldBe` Right KType)
      [ tAny,
        tDynamic,
        tUnknown,
        TLit (LInt 1),
        TLit (LString "x"),
        TMeta 0,
        TTypeList [tInt, tString],
        TFun Many tInt tString,
        TFun One tInt tString,
        TRecord (Map.fromList [("a", tInt)]),
        TUnion [tInt, tString],
        tList tInt,
        TForall ["a"] (TFun Many (TVar "a") (TVar "a"))
      ]

  it "reports an unconstrained type as kind-polymorphic rather than as Type" $ do
    -- A bare `forall a. a` or an unseen constructor constrains nothing, so
    -- inference leaves a kind metavariable standing for "any kind". Callers
    -- that need a ground answer (term annotations, ambient entries) default it
    -- through `validateProgramKinds`.
    inferTypeKind mempty (TForall ["a"] (TVar "a")) `shouldSatisfy` isMetaKind
    inferTypeKind mempty (TCon "Mystery") `shouldSatisfy` isMetaKind

  it "requires both branches of a conditional type to agree" $ do
    inferTypeKind mempty (TConditional tInt tInt tString tBool) `shouldBe` Right KType
    inferTypeKind mempty (TConditional tInt tInt (TCon "List") tBool)
      `shouldSatisfy` isLeft

  it "keeps unknown constructors flexible so ambient authoring stays incremental" $ do
    inferTypeKind mempty (TCon "Mystery") `shouldSatisfy` isRight
    inferTypeKind mempty (TApp (TCon "Mystery") tInt) `shouldSatisfy` isRight

  it "gives an unknown constructor one consistent kind across a type" $
    -- The same name cannot be both a constructor and a ground type.
    inferTypeKind mempty (TFun Many (TCon "Mystery") (TApp (TCon "Mystery") tInt))
      `shouldSatisfy` isLeft

  it "infers alias arity from the body" $ do
    kinds <-
      expectRight $
        inferAliasKinds
          [ plainAlias "Ground" [] tInt,
            plainAlias "Wrap" ["a"] (tList (TVar "a")),
            plainAlias "Const" ["a", "b"] (TVar "a")
          ]
    Map.lookup "Ground" kinds `shouldBe` Just KType
    Map.lookup "Wrap" kinds `shouldBe` Just (KFun KType KType)
    -- `Const` constrains neither parameter, so it stays kind-polymorphic: two
    -- arguments, and a result equal to the first of them.
    case Map.lookup "Const" kinds of
      Just (KFun first (KFun _ result)) -> result `shouldBe` first
      other -> expectationFailure ("expected a binary alias kind, got " <> show other)

  it "returns an empty environment for an empty alias list" $
    inferAliasKinds [] `shouldBe` Right Map.empty

  it "lets a later alias definition win over an earlier one of the same name" $ do
    kinds <-
      expectRight $
        inferAliasKinds
          [ plainAlias "Dup" ["a"] (TVar "a"),
            plainAlias "Dup" [] tInt
          ]
    Map.lookup "Dup" kinds `shouldBe` Just KType

  it "rejects a self-application that would need an infinite kind" $
    inferAliasKinds [plainAlias "Loop" ["f"] (TApp (TVar "f") (TVar "f"))]
      `shouldSatisfy` isLeft

  it "rejects undersaturated constructors in term positions" $ do
    program <-
      expectRight $
        parseProgram
          "main.tynix"
          (source ["let value :: List;", "    value = [];", "in value"])
    validateProgramKinds (programAliases program) program `shouldSatisfy` isLeft

  it "rejects higher-kinded ambient entries" $ do
    program <-
      expectRight $
        parseProgram
          "main.tynix"
          (source ["declare \"./lib.nix\" { default :: Vec Int; };"])
    validateProgramKinds (programAliases program) program `shouldSatisfy` isLeft

  it "still allows alias bodies to be higher-kinded" $ do
    program <-
      expectRight $
        parseProgram
          "main.tynix"
          (source ["type Partial = Vec 3;", "1"])
    validateProgramKinds (programAliases program) program `shouldSatisfy` isRight

  it "validates annotations buried inside expressions" $ do
    program <-
      expectRight $
        parseProgram
          "main.tynix"
          (source ["{ value = ([] as List); }"])
    validateProgramKinds (programAliases program) program `shouldSatisfy` isLeft

  it "validates higher-kinded annotations inside parsed programs" $ do
    program <-
      expectRight $
        parseProgram
          "main.tynix"
          ( source
              [ "type Id f = f;",
                "type Apply f a = f a;",
                "declare \"./lib.nix\" { default :: Apply (Id List) Int; };",
                "let value :: Apply (Id List) Int;",
                "    value = import ./lib.nix;",
                "in value"
              ]
          )
    validateProgramKinds (programAliases program) program
      `shouldSatisfy` isRight
  where
    isLeft (Left _) = True
    isLeft _ = False
    isRight (Right _) = True
    isRight _ = False
    isMetaKind (Right (KMeta _)) = True
    isMetaKind _ = False
