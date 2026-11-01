{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Driver (Analysis (..))
import SessionInlayHints
import Syntax
import Test.Hspec
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "inlayHintsFor" $ do
    it "adds an inferred-type hint next to an unannotated let-binding" $ do
      let content = Text.unlines ["let", "  value = 1;", "in value"]
          program =
            Program
              { programAliases = [],
                programAmbient = [],
                programMacros = [],
                programExpr =
                  Just
                    ( Marked
                        Nothing
                        ( ELet
                            [ Marked Nothing (LetBinding "value" (EInt 1))
                            ]
                            (EVar "value")
                        )
                    )
              }
          analysis = stubAnalysis program (Map.fromList [("value", Scheme [] tInt)])
      inlayHintsFor content (Right analysis)
        `shouldBe` [InlayHint 1 7 ":: Int"]

    it "skips bindings that already carry an explicit signature" $ do
      let content = Text.unlines ["let", "  value :: Int;", "  value = 1;", "in value"]
          program =
            Program
              { programAliases = [],
                programAmbient = [],
                programMacros = [],
                programExpr =
                  Just
                    ( Marked
                        Nothing
                        ( ELet
                            [ Marked Nothing (LetSignature "value" tInt),
                              Marked Nothing (LetBinding "value" (EInt 1))
                            ]
                            (EVar "value")
                        )
                    )
              }
          analysis = stubAnalysis program (Map.fromList [("value", Scheme [] tInt)])
      inlayHintsFor content (Right analysis) `shouldBe` []

    it "renders polymorphic schemes with the forall prefix" $ do
      let content = Text.unlines ["let", "  identity = x: x;", "in identity"]
          scheme = Scheme ["a"] (TFun Many (TVar "a") (TVar "a"))
          program =
            Program
              { programAliases = [],
                programAmbient = [],
                programMacros = [],
                programExpr =
                  Just
                    ( Marked
                        Nothing
                        ( ELet
                            [ Marked Nothing (LetBinding "identity" (ELambda (PVar "x" Nothing) (EVar "x")))
                            ]
                            (EVar "identity")
                        )
                    )
              }
          analysis = stubAnalysis program (Map.fromList [("identity", scheme)])
          hints = inlayHintsFor content (Right analysis)
      map inlayHintLabel hints `shouldBe` [":: forall a. a -> a"]

    it "returns no hints when analysis failed" $
      inlayHintsFor "let value = 1; in value" (Left "boom") `shouldBe` []

    it "covers multiple unannotated bindings in one let block" $ do
      let content = Text.unlines ["let", "  a = 1;", "  b = 2;", "in a"]
          program =
            Program
              { programAliases = [],
                programAmbient = [],
                programMacros = [],
                programExpr =
                  Just
                    ( Marked
                        Nothing
                        ( ELet
                            [ Marked Nothing (LetBinding "a" (EInt 1)),
                              Marked Nothing (LetBinding "b" (EInt 2))
                            ]
                            (EVar "a")
                        )
                    )
              }
          analysis = stubAnalysis program (Map.fromList [("a", Scheme [] tInt), ("b", Scheme [] tInt)])
          hints = inlayHintsFor content (Right analysis)
      hints `shouldBe` [InlayHint 1 3 ":: Int", InlayHint 2 3 ":: Int"]

  describe "encodeInlayHints" $
    it "produces an LSP payload with position, label, kind, and paddingLeft" $
      LBS.unpack (encode (encodeInlayHints [InlayHint 1 7 ":: Int"]))
        `shouldContain` "\"paddingLeft\":true"

stubAnalysis :: Program -> Map.Map Name Scheme -> Analysis
stubAnalysis program bindings =
  Analysis
    { analysisProgram = program,
      analysisRoot = Nothing,
      analysisBindings = bindings,
      analysisAliases = mempty,
      analysisAmbient = mempty
    }
