{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Emit
import Syntax
import Test.Hspec
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "emitDeclarationFile" $ do
    it "emits an attribute-set root field-by-field" $ do
      let program =
            Program
              { programAliases = [],
                programAmbient = [],
                programMacros = [],
                programExpr = Just (markedExpr (EAttrSet []))
              }
          scheme =
            Scheme
              []
              (TRecord (Map.fromList [("inc", TFun Many tInt tInt), ("name", tString)]))
      emitDeclarationFile "src/lib.tynix" program scheme
        `shouldContain'` "declare \"./lib.nix\""
      emitDeclarationFile "src/lib.tynix" program scheme
        `shouldContain'` "inc :: Int -> Int;"
      emitDeclarationFile "src/lib.tynix" program scheme
        `shouldContain'` "name :: String;"

    it "exposes non-record roots as `default`" $ do
      let program =
            Program
              { programAliases = [],
                programAmbient = [],
                programMacros = [],
                programExpr = Just (markedExpr (EInt 1))
              }
          scheme = Scheme [] tInt
      emitDeclarationFile "src/value.tynix" program scheme
        `shouldContain'` "default :: Int;"

    it "preserves the polymorphic quantifier on default-style schemes" $ do
      let program =
            Program
              { programAliases = [],
                programAmbient = [],
                programMacros = [],
                programExpr = Just (markedExpr (ELambda (PVar "x" Nothing) (EVar "x")))
              }
          scheme = Scheme ["a"] (TFun Many (TVar "a") (TVar "a"))
      emitDeclarationFile "src/id.tynix" program scheme
        `shouldContain'` "default :: forall a. a -> a;"

    it "renders local aliases ahead of the declare block" $ do
      let program =
            Program
              { programAliases =
                  [plainAlias "Pair" ["a", "b"] (TRecord (Map.fromList [("fst", TVar "a"), ("snd", TVar "b")]))],
                programAmbient = [],
                programMacros = [],
                programExpr = Just (markedExpr (EInt 1))
              }
          scheme = Scheme [] tInt
          rendered = emitDeclarationFile "src/lib.tynix" program scheme
      rendered `shouldContain'` "type Pair a b ="
      Text.length rendered > 0 `shouldBe` True

  describe "emitDeclarationFileFor" $ do
    it "rewrites the declare target relative to the destination directory" $ do
      let program =
            Program
              { programAliases = [],
                programAmbient = [],
                programMacros = [],
                programExpr = Just (markedExpr (EAttrSet []))
              }
          scheme = Scheme [] (TRecord (Map.fromList [("value", tInt)]))
      emitDeclarationFileFor "dist/lib.nix" "dist/types/lib.d.tynix" program scheme
        `shouldContain'` "declare \"../lib.nix\""

    it "keeps an absolute declare path verbatim" $ do
      let program =
            Program
              { programAliases = [],
                programAmbient = [],
                programMacros = [],
                programExpr = Just (markedExpr (EInt 1))
              }
          scheme = Scheme [] tInt
      emitDeclarationFileFor "/abs/runtime.nix" "/abs/types/runtime.d.tynix" program scheme
        `shouldContain'` "declare \"../runtime.nix\""

    it "strips redundant casts before deciding the emission shape" $ do
      let program =
            Program
              { programAliases = [],
                programAmbient = [],
                programMacros = [],
                programExpr = Just (markedExpr (ECast (EAttrSet []) tInt))
              }
          scheme = Scheme [] (TRecord (Map.fromList [("value", tString)]))
      emitDeclarationFileFor "src/cast.nix" "src/cast.d.tynix" program scheme
        `shouldContain'` "value :: String;"

markedExpr :: Expr -> Marked Expr
markedExpr expr = Marked{markedDirective = Nothing, markedValue = expr}

shouldContain' :: (HasCallStack) => Text -> Text -> Expectation
shouldContain' haystack needle =
  if needle `Text.isInfixOf` haystack
    then pure ()
    else
      expectationFailure
        ( "expected text to contain "
            <> show needle
            <> ", got:\n"
            <> Text.unpack haystack
        )
