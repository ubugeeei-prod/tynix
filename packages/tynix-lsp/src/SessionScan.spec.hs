{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import SessionScan
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "tokenize" $ do
    it "splits interpolated strings into pieces around the antiquotation" $
      map (\t -> (tokKind t, tokText t)) (tokenize "\"a${b}c\"")
        `shouldBe` [ (KString, "\"a"),
                     (KSymbol, "${"),
                     (KIdent, "b"),
                     (KSymbol, "}"),
                     (KString, "c\"")
                   ]

    it "recognises paths, search paths, comments, and keywords" $
      map
        (\t -> (tokKind t, tokText t))
        (tokenize "let p = ./a/b.nix; q = <nixpkgs>; in p # done")
        `shouldBe` [ (KKeyword, "let"),
                     (KIdent, "p"),
                     (KSymbol, "="),
                     (KPath, "./a/b.nix"),
                     (KSymbol, ";"),
                     (KIdent, "q"),
                     (KSymbol, "="),
                     (KPath, "<nixpkgs>"),
                     (KSymbol, ";"),
                     (KKeyword, "in"),
                     (KIdent, "p"),
                     (KComment, "# done")
                   ]

    it "keeps a bare ./ as a path so completion can react to it" $
      map tokText (tokenize "import ./") `shouldBe` ["import", "./"]

    it "never fails on unterminated input" $ do
      length (tokenize "\"abc ${ x") `shouldSatisfy` (> 0)
      length (tokenize "''\n  text ${") `shouldSatisfy` (> 0)

  describe "line index" $
    it "round-trips offsets through UTF-16 positions" $ do
      let content = "a\n\x1f600x\nlast"
          idx = mkLineIndex content
      offsetToPosition idx 3 `shouldBe` (1, 1 + 1)
      positionToOffset idx (1, 2) `shouldBe` 3
      positionToOffset idx (2, 4) `shouldBe` 9

  describe "scanDocument binders" $ do
    it "binds let names, lambda params, and pattern fields with scopes" $ do
      let scan = scanDocument "let f = { a, b ? 1, ... }@args: x: a; in f"
      sort [(binderName b, binderKind b) | b <- scanBinders scan]
        `shouldBe` sort
          [ ("f", BindLet),
            ("a", BindPatternField),
            ("b", BindPatternField),
            ("args", BindPatternAlias),
            ("x", BindParam)
          ]

    it "records owners so parameter types can come from the function signature" $ do
      let scan = scanDocument "let f :: Int -> Int -> Int; f = x: y: x; in f"
          owners =
            [ (binderName b, snd <$> binderOwner b)
            | b <- scanBinders scan,
              binderKind b == BindParam
            ]
      sort owners `shouldBe` [("x", Just 0), ("y", Just 1)]
      [binderAnnotation b | b <- scanBinders scan, binderName b == "f"]
        `shouldBe` [Just "Int -> Int -> Int"]

    it "resolves references to the innermost binder and ignores field selections" $ do
      let content = "let x = 1; y = x: x.x; in y x"
          scan = scanDocument content
          refs = refsTo scan
      lookup "x@4" refs `shouldBe` Just 1
      lookup "x@15" refs `shouldBe` Just 1

    it "reports unused let bindings, params, and pattern fields" $ do
      let scan = scanDocument "let used = 1; unused = 2; f = { a, b }: _: a; in f used"
      sort (map binderName (unusedBinders scan)) `shouldBe` ["b", "unused"]

    it "does not treat type annotations or attr keys as references" $ do
      let scan =
            scanDocument
              "let a = 1; t :: forall a. a -> a; t = v: v; r = { a = 2; }; in r.a + t 1"
      map binderName (unusedBinders scan) `shouldBe` ["a"]

    it "counts inherit inside an attrset as a use of the outer binding" $ do
      let scan = scanDocument "let name = \"x\"; in { inherit name; }"
      unusedBinders scan `shouldBe` []

    it "counts references inside string interpolation" $ do
      let scan = scanDocument "let v = 1; in \"${toString v}\""
      unusedBinders scan `shouldBe` []

    it "lists binders in scope innermost first" $ do
      let content = "let outer = 1; in (inner: inner)"
          scan = scanDocument content
          off = Text.length "let outer = 1; in (inner: inn"
      map binderName (bindersInScopeAt scan off) `shouldBe` ["inner", "outer"]

    it "keeps working on half-typed input" $ do
      let scan = scanDocument "let a = 1; b = a."
      map binderName (bindersInScopeAt scan 17) `shouldBe` ["a", "b"]

    it "collects documentation comments above a declaration" $ do
      let scan =
            scanDocument "let\n  # Adds one.\n  # @tynix-ignore\n  inc = x: x + 1;\nin inc"
      docCommentAtLine scan 3 `shouldBe` Just "Adds one."

refsTo :: Scan -> [(Text, Int)]
refsTo scan =
  mapMaybe
    ( \b ->
        Just
          ( binderName b <> "@" <> Text.pack (show (binderNameStart b)),
            length (binderReferences scan b)
          )
    )
    (scanBinders scan)
