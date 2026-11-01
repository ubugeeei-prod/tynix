{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Driver (Analysis (..), analyzeFile, analyzeFileWith, analyzeText, compileFile, emitFile, newSupportCache)
import Pretty (renderScheme)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec
import TestSupport (expectLeftContaining, expectRight, source, withTempTree)
import Type

main :: IO ()
main = hspec spec

-- | A root scheme with each arrow's effect and capture details dropped, for
-- tests that are about argument and result types only.
plainRoot :: Analysis -> Maybe Scheme
plainRoot = fmap (\(Scheme vars ty) -> Scheme vars (plain ty)) . analysisRoot
  where
    plain = \case
      TArrow arrow a b -> TFun (arrowMult arrow) (plain a) (plain b)
      TRecord fields -> TRecord (fmap plain fields)
      other -> other

spec :: Spec
spec = describe "analysis" $ do
  it "infers literal roots" $ do
    analysis <- analyzeText "main.tynix" "1" >>= expectRight
    analysisRoot analysis `shouldBe` Just (Scheme [] (TLit (LInt 1)))

  it "infers float literal roots" $ do
    analysis <- analyzeText "main.tynix" "1.5" >>= expectRight
    analysisRoot analysis `shouldBe` Just (Scheme [] (TLit (LFloat 1.5)))

  it "infers int addition inside legacy nix lambdas" $ do
    analysis <- analyzeText "math.nix" "{ inc = x: x + 1; }" >>= expectRight
    plainRoot analysis
      `shouldBe` Just
        ( Scheme
            []
            (TRecord (Map.fromList [("inc", TFun One tInt tInt)]))
        )

  it "preserves declared binding schemes while allowing gradual root inference" $ do
    analysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "let",
              "  id :: forall a. a -> a;",
              "  id = x: x;",
              "in id"
            ]
        )
        >>= expectRight
    Map.lookup "id" (analysisBindings analysis)
      `shouldBe` Just (Scheme ["a"] (TFun Many (TVar "a") (TVar "a")))
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "forall t0. t0 -> t0"

  it "follows structural field selections" $ do
    analysis <- analyzeText "main.tynix" "{ nested = { value = 1; }; }.nested.value" >>= expectRight
    analysisRoot analysis `shouldBe` Just (Scheme [] (TLit (LInt 1)))

  it "follows quoted field selections" $ do
    analysis <- analyzeText "main.tynix" "{ \"aarch64-darwin\" = 1; }.\"aarch64-darwin\"" >>= expectRight
    analysisRoot analysis `shouldBe` Just (Scheme [] (TLit (LInt 1)))

  it "joins dynamic field selections from string literal unions" $ do
    analysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "let",
              "  system :: \"aarch64-darwin\" | \"x86_64-linux\";",
              "  system = \"aarch64-darwin\";",
              "  packages = { \"aarch64-darwin\" = 1; x86_64-linux = 2; };",
              "in packages.${system}"
            ]
        )
        >>= expectRight
    analysisRoot analysis `shouldBe` Just (Scheme [] (TUnion [TLit (LInt 1), TLit (LInt 2)]))

  it "infers attrset lambda binders used in flake outputs" $ do
    analysis <- analyzeText "main.tynix" "{ self, nixpkgs, ... }: self" >>= expectRight
    fmap renderScheme (analysisRoot analysis)
      `shouldBe` Just "forall t0 t1. {\n  nixpkgs :: t1;\n  self :: t0;\n  ...\n} -> t0"

  it "counts mutually exclusive if branches once for lambda multiplicity" $ do
    analysis <- analyzeText "main.tynix" "x: if true then x else x" >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "forall t0. t0 %1 -> t0"

  it "infers numeric results for subtraction and multiplication" $ do
    mul <- analyzeText "math.nix" "{ area = w: h: w * h; }" >>= expectRight
    plainRoot mul
      `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("area", TFun Many tNumber (TFun One tNumber tNumber))])))
    sub <- analyzeText "math.nix" "{ diff = a: b: a - b; }" >>= expectRight
    plainRoot sub
      `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("diff", TFun Many tNumber (TFun One tNumber tNumber))])))

  it "widens Nat-only subtraction to Int" $ do
    analysis <- analyzeText "main.nix" "{ diff = (a :: Nat): (b :: Nat): a - b; }" >>= expectRight
    plainRoot analysis
      `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("diff", TFun Many tNat (TFun One tNat tInt))])))

  it "concatenates annotated lists into a joined list element type" $ do
    analysis <- analyzeText "main.nix" "{ cat = (xs :: List Int): (ys :: List Int): xs ++ ys; }" >>= expectRight
    plainRoot analysis
      `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("cat", TFun Many (tList tInt) (TFun One (tList tInt) (tList tInt)))])))

  it "concatenates list literals into a structural list" $ do
    analysis <- analyzeText "main.tynix" "[1 2] ++ [3 4]" >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldSatisfy` maybe False (Text.isInfixOf "List")

  it "rejects concatenating non-list operands" $
    analyzeText "main.tynix" "1 ++ 2" >>= (`expectLeftContaining` "cannot concatenate")

  it "infers recursive attribute sets where fields reference each other" $ do
    analysis <- analyzeText "main.tynix" "rec { a = 1; b = a; }" >>= expectRight
    analysisRoot analysis
      `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("a", TLit (LInt 1)), ("b", TLit (LInt 1))])))

  it "resolves forward references inside a recursive attribute set" $ do
    analysis <- analyzeText "main.tynix" "rec { b = a; a = 1; }" >>= expectRight
    analysisRoot analysis
      `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("a", TLit (LInt 1)), ("b", TLit (LInt 1))])))

  it "types attribute-presence tests as Bool regardless of field presence" $ do
    present <- analyzeText "main.tynix" "{ a = 1; } ? a" >>= expectRight
    analysisRoot present `shouldBe` Just (Scheme [] tBool)
    absent <- analyzeText "main.tynix" "{ a = 1; } ? b" >>= expectRight
    analysisRoot absent `shouldBe` Just (Scheme [] tBool)

  it "types an assert expression as the type of its body" $ do
    analysis <- analyzeText "main.tynix" "assert true; 1" >>= expectRight
    analysisRoot analysis `shouldBe` Just (Scheme [] (TLit (LInt 1)))

  it "rejects an assert whose condition is not Bool" $
    analyzeText "main.tynix" "assert 1; 2" >>= (`expectLeftContaining` "type mismatch")

  it "merges attribute sets with the update operator, right side overriding" $ do
    analysis <- analyzeText "main.tynix" "{ a = 1; } // { a = 2; b = 3; }" >>= expectRight
    analysisRoot analysis
      `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("a", TLit (LInt 2)), ("b", TLit (LInt 3))])))

  it "rejects updating non-record operands" $
    analyzeText "main.tynix" "1 // 2" >>= (`expectLeftContaining` "cannot update")

  it "infers Bool from relational, equality, and boolean operators" $ do
    lt <- analyzeText "main.tynix" "1 < 2" >>= expectRight
    analysisRoot lt `shouldBe` Just (Scheme [] tBool)
    eq <- analyzeText "main.tynix" "1 == 2" >>= expectRight
    analysisRoot eq `shouldBe` Just (Scheme [] tBool)
    conj <- analyzeText "main.tynix" "true && false" >>= expectRight
    analysisRoot conj `shouldBe` Just (Scheme [] tBool)
    neg <- analyzeText "main.tynix" "!true" >>= expectRight
    analysisRoot neg `shouldBe` Just (Scheme [] tBool)

  it "compares strings as ordered values" $ do
    cmp <- analyzeText "main.tynix" "\"a\" < \"b\"" >>= expectRight
    analysisRoot cmp `shouldBe` Just (Scheme [] tBool)

  it "rejects comparing non-comparable operands" $
    analyzeText "main.tynix" "true < false" >>= (`expectLeftContaining` "cannot compare")

  it "rejects boolean connectives applied to non-boolean operands" $
    analyzeText "main.tynix" "1 && true" >>= (`expectLeftContaining` "type mismatch")

  it "types interpolated strings as String and checks embedded expressions" $ do
    analysis <- analyzeText "main.tynix" "let name = \"x\"; in \"hi ${name}\"" >>= expectRight
    analysisRoot analysis `shouldBe` Just (Scheme [] tString)

  it "reports unbound names used inside interpolation" $
    analyzeText "main.tynix" "\"value is ${missing}\"" >>= (`expectLeftContaining` "unbound name")

  it "brings a record scope's fields into scope with `with`" $ do
    analysis <- analyzeText "main.tynix" "with { a = 1; }; a" >>= expectRight
    analysisRoot analysis `shouldBe` Just (Scheme [] (TLit (LInt 1)))

  it "still reports names absent from a known `with` record scope" $
    analyzeText "main.tynix" "with { a = 1; }; b" >>= (`expectLeftContaining` "unbound name")

  it "stays lenient for names resolved through a gradual `with` scope" $ do
    analysis <- analyzeText "main.tynix" "(pkgs :: dynamic): with pkgs; somePackage" >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldSatisfy` maybe False ("dynamic" `Text.isInfixOf`)

  it "reports missing fields" $
    analyzeText "main.tynix" "{ value = 1; }.missing" >>= (`expectLeftContaining` "missing field")

  it "rejects applying a concrete non-function value" $ do
    analyzeText "main.tynix" "1 2" >>= (`expectLeftContaining` "cannot call an integer as a function")
    analyzeText "main.tynix" "{ x = 1; } 2" >>= (`expectLeftContaining` "cannot call an attribute set as a function")

  it "renders diagnostics with surface syntax and no raw AST constructors" $ do
    let assertHumanReadable input needle =
          analyzeText "main.tynix" input >>= \case
            Left err -> do
              err `shouldContain` needle
              err `shouldNotContain` "TRecord"
              err `shouldNotContain` "TLit"
              err `shouldNotContain` "TFun"
              err `shouldNotContain` "TUnion"
            Right _ -> expectationFailure ("expected diagnostic containing " <> show needle)
    assertHumanReadable "{ value = 1; }.missing" "missing field `missing`"
    assertHumanReadable "missing" "unbound name: `missing`"
    assertHumanReadable
      ( source
          [ "let",
            "  pkg :: { name :: String; };",
            "  pkg = { name = 1; };",
            "in pkg"
          ]
      )
      "type mismatch"

  it "resolves field selections through previously inferred let bindings" $ do
    analysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "let",
              "  record = { value = 1; nested = { label = \"tynix\"; }; };",
              "  value = record.nested.label;",
              "in value"
            ]
        )
        >>= expectRight
    analysisRoot analysis `shouldBe` Just (Scheme [] (TLit (LString "tynix")))

  it "reports unbound names" $
    analyzeText "main.tynix" "missing" >>= (`expectLeftContaining` "unbound name")

  it "reports filesystem read failures as ordinary errors" $ do
    analyzeFile "/tmp/tynix-does-not-exist/main.tynix" >>= (`expectLeftContaining` "failed to read")
    compileFile "/tmp/tynix-does-not-exist/main.tynix" >>= (`expectLeftContaining` "failed to read")
    emitFile "/tmp/tynix-does-not-exist/main.tynix" >>= (`expectLeftContaining` "failed to read")

  it "infers exact vector roots while preserving precise element unions" $ do
    analysis <- analyzeText "main.tynix" "[1 2]" >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "Vec 2 (1 | 2)"

  it "infers heterogeneous list roots as tuples" $ do
    analysis <- analyzeText "main.tynix" "[1 \"x\"]" >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "Tuple [ 1 \"x\" ]"

  it "infers empty lists as zero-length vectors" $ do
    analysis <- analyzeText "main.tynix" "[]" >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "Vec 0 dynamic"

  it "infers matrix and tensor roots from nested list literals" $ do
    matrixAnalysis <- analyzeText "main.tynix" "[[1 2] [3 4]]" >>= expectRight
    tensorAnalysis <- analyzeText "main.tynix" "[[[1] [2]] [[3] [4]]]" >>= expectRight
    fmap renderScheme (analysisRoot matrixAnalysis) `shouldBe` Just "Matrix 2 2 (1 | 2 | 3 | 4)"
    fmap renderScheme (analysisRoot tensorAnalysis) `shouldBe` Just "Tensor [ 2 2 1 ] (1 | 2 | 3 | 4)"

  it "preserves ragged nested list roots as structural lists of vectors" $ do
    analysis <- analyzeText "main.tynix" "[[1] [2 3]]" >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "List (Vec (1 | 2) (1 | 2 | 3))"

  it "uses inline ambient declarations for imports" $ do
    analysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "declare \"./lib.nix\" { default :: { value :: Int; }; };",
              "import ./lib.nix"
            ]
        )
        >>= expectRight
    analysisRoot analysis
      `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("value", tInt)])))

  it "uses ambient declarations for string imports" $ do
    analysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "declare \"./lib.nix\" { default :: { value :: Int; }; };",
              "import \"./lib.nix\""
            ]
        )
        >>= expectRight
    analysisRoot analysis
      `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("value", tInt)])))

  it "builds record schemes from named ambient exports" $ do
    analysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "declare \"./lib.nix\" { value :: Int; label :: String; };",
              "import ./lib.nix"
            ]
        )
        >>= expectRight
    analysisRoot analysis
      `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("label", tString), ("value", tInt)])))

  it "loads builtins declarations from ambient support files" $
    withTempTree
      [ ("flake.nix", "{}"),
        ( "builtins.d.tynix",
          source
            [ "declare \"builtins\" {",
              "  add :: Int -> Int -> Int;",
              "  head :: forall a. List a -> a;",
              "};"
            ]
        ),
        ("app/main.tynix", "let sum = builtins.add 1 2; first = builtins.head [1 2]; in { inherit sum first; }")
      ]
      ( \root -> do
          analysis <- analyzeFile (root <> "/app/main.tynix") >>= expectRight
          analysisRoot analysis
            `shouldBe` Just
              ( Scheme
                  []
                  ( TRecord
                      ( Map.fromList
                          [ ("first", TUnion [TLit (LInt 1), TLit (LInt 2)]),
                            ("sum", tInt)
                          ]
                      )
                  )
              )
      )

  it "supports higher-kinded aliases in ambient declarations and signatures" $ do
    analysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "type Id f = f;",
              "type Apply f a = f a;",
              "declare \"./lib.nix\" { default :: Apply (Id List) Int; };",
              "let",
              "  value :: Apply (Id List) Int;",
              "  value = import ./lib.nix;",
              "in value"
            ]
        )
        >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "Apply (Id List) Int"

  it "checks Vec, Matrix, and Tensor annotations against list literals" $ do
    vectorAnalysis <-
      analyzeText "main.tynix" (source ["let xs :: Vec 3 Int;", "    xs = [1 2 3];", "in xs"])
        >>= expectRight
    matrixAnalysis <-
      analyzeText "main.tynix" (source ["let grid :: Matrix 2 2 Int;", "    grid = [[1 2] [3 4]];", "in grid"])
        >>= expectRight
    tensorAnalysis <-
      analyzeText
        "main.tynix"
        (source ["let cube :: Tensor [2 2 1] Int;", "    cube = [[[1] [2]] [[3] [4]]];", "in cube"])
        >>= expectRight
    fmap renderScheme (analysisRoot vectorAnalysis) `shouldBe` Just "Vec 3 Int"
    fmap renderScheme (analysisRoot matrixAnalysis) `shouldBe` Just "Matrix 2 2 Int"
    analysisRoot tensorAnalysis
      `shouldBe` Just
        ( Scheme
            []
            (TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 2), TLit (LInt 2), TLit (LInt 1)])) tInt)
        )

  it "accepts dependent-ish numeric length constraints for vectors" $ do
    analysis <-
      analyzeText "main.tynix" (source ["let xs :: Vec (Range 2 4 Nat) Int;", "    xs = [1 2 3];", "in xs"])
        >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "Vec (Range 2 4 Nat) Int"

  it "accepts bounded matrix and tensor annotations across multiple axes" $ do
    matrixAnalysis <-
      analyzeText
        "main.tynix"
        (source ["let grid :: Matrix (Range 1 2 Nat) 2 Int;", "    grid = [[1 2] [3 4]];", "in grid"])
        >>= expectRight
    tensorAnalysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "let cube :: Tensor [2 (Range 1 2 Nat) 1] Int;",
              "    cube = [[[1] [2]] [[3] [4]]];",
              "in cube"
            ]
        )
        >>= expectRight
    fmap renderScheme (analysisRoot matrixAnalysis) `shouldBe` Just "Matrix (Range 1 2 Nat) 2 Int"
    fmap renderScheme (analysisRoot tensorAnalysis) `shouldBe` Just "Tensor [ 2 (Range 1 2 Nat) 1 ] Int"

  it "checks numeric validation and units on annotated bindings" $ do
    analysis <-
      analyzeText
        "main.tynix"
        (source ["let timeout :: Unit \"ms\" (Range 0 5000 Nat);", "    timeout = 2500;", "in timeout"])
        >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "Unit \"ms\" (Range 0 5000 Nat)"
    analyzeText
      "main.tynix"
      (source ["let timeout :: Unit \"ms\" (Range 0 5000 Nat);", "    timeout = 9000;", "in timeout"])
      >>= (`expectLeftContaining` "type mismatch")

  it "accepts exact-zero bounded vectors" $ do
    analysis <-
      analyzeText "main.tynix" (source ["let xs :: Vec (Range 0 0 Nat) Int;", "    xs = [];", "in xs"])
        >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "Vec (Range 0 0 Nat) Int"

  it "accepts inclusive endpoints for int and float validators" $ do
    timeoutLow <-
      analyzeText "main.tynix" (source ["let timeout :: Unit \"ms\" (Range 0 5000 Nat);", "    timeout = 0;", "in timeout"])
        >>= expectRight
    timeoutHigh <-
      analyzeText "main.tynix" (source ["let timeout :: Unit \"ms\" (Range 0 5000 Nat);", "    timeout = 5000;", "in timeout"])
        >>= expectRight
    ratioLow <-
      analyzeText "main.tynix" (source ["let ratio :: Range 0.0 1.0 Float;", "    ratio = 0.0;", "in ratio"])
        >>= expectRight
    ratioHigh <-
      analyzeText "main.tynix" (source ["let ratio :: Range 0.0 1.0 Float;", "    ratio = 1.0;", "in ratio"])
        >>= expectRight
    fmap renderScheme (analysisRoot timeoutLow) `shouldBe` Just "Unit \"ms\" (Range 0 5000 Nat)"
    fmap renderScheme (analysisRoot timeoutHigh) `shouldBe` Just "Unit \"ms\" (Range 0 5000 Nat)"
    fmap renderScheme (analysisRoot ratioLow) `shouldBe` Just "Range 0.0 1.0 Float"
    fmap renderScheme (analysisRoot ratioHigh) `shouldBe` Just "Range 0.0 1.0 Float"

  it "checks float ranges and unit mismatches through let-bound names" $ do
    floatAnalysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "let",
              "  ratio :: Range 0.0 1.0 Float;",
              "  ratio = 0.5;",
              "in ratio"
            ]
        )
        >>= expectRight
    fmap renderScheme (analysisRoot floatAnalysis) `shouldBe` Just "Range 0.0 1.0 Float"
    analyzeText
      "main.tynix"
      ( source
          [ "let",
            "  ratio :: Range 0.0 1.0 Float;",
            "  ratio = 1.5;",
            "in ratio"
          ]
      )
      >>= (`expectLeftContaining` "type mismatch")
    analyzeText
      "main.tynix"
      ( source
          [ "let",
            "  timeoutMs :: Unit \"ms\" Nat;",
            "  timeoutMs = 1;",
            "  timeoutS :: Unit \"s\" Nat;",
            "  timeoutS = timeoutMs;",
            "in timeoutS"
          ]
      )
      >>= (`expectLeftContaining` "type mismatch")
    analyzeText
      "main.tynix"
      ( source
          [ "let",
            "  timeout :: Unit \"ms\" Nat;",
            "  timeout = 0.5;",
            "in timeout"
          ]
      )
      >>= (`expectLeftContaining` "type mismatch")

  it "lets any flow through field access and application" $ do
    fieldAnalysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "let",
              "  value :: any;",
              "  value = { nested = 1; };",
              "in value.missing"
            ]
        )
        >>= expectRight
    callAnalysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "let",
              "  fn :: any;",
              "  fn = x: x;",
              "in fn 1"
            ]
        )
        >>= expectRight
    fmap renderScheme (analysisRoot fieldAnalysis) `shouldBe` Just "any"
    fmap renderScheme (analysisRoot callAnalysis) `shouldBe` Just "any"

  it "accepts unknown as an annotation but rejects using it as a concrete type" $ do
    analysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "let",
              "  value :: unknown;",
              "  value = 1;",
              "in value"
            ]
        )
        >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "unknown"
    analyzeText
      "main.tynix"
      ( source
          [ "let",
            "  value :: unknown;",
            "  value = 1;",
            "  label :: String;",
            "  label = value;",
            "in label"
          ]
      )
      >>= (`expectLeftContaining` "type mismatch")

  it "rejects field selection on unknown instead of widening to dynamic" $ do
    analyzeText
      "main.tynix"
      ( source
          [ "let",
            "  value :: unknown;",
            "  value = { nested = 1; };",
            "in value.missing"
          ]
      )
      >>= (`expectLeftContaining` "cannot select field")
    analyzeText
      "main.tynix"
      ( source
          [ "let",
            "  value :: unknown;",
            "  value = { nested = 1; };",
            "  key :: \"nested\";",
            "  key = \"nested\";",
            "in value.${key}"
          ]
      )
      >>= (`expectLeftContaining` "cannot select dynamic field from unknown")
    analyzeText
      "main.tynix"
      ( source
          [ "let",
            "  value = { nested = 1; };",
            "  key :: unknown;",
            "  key = \"nested\";",
            "in value.${key}"
          ]
      )
      >>= (`expectLeftContaining` "string-like key")

  it "supports widening, narrowing, and gradual as-casts" $ do
    widenAnalysis <- analyzeText "main.tynix" "1 as Number" >>= expectRight
    narrowAnalysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "let",
              "  xs :: List Int;",
              "  xs = [1 2];",
              "in xs as Vec 2 Int"
            ]
        )
        >>= expectRight
    unknownAnalysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "let",
              "  value :: unknown;",
              "  value = 1;",
              "in value as Int"
            ]
        )
        >>= expectRight
    dynamicAnalysis <- analyzeText "main.tynix" "import ./opaque.nix as { value :: Int; }" >>= expectRight
    fmap renderScheme (analysisRoot widenAnalysis) `shouldBe` Just "Number"
    fmap renderScheme (analysisRoot narrowAnalysis) `shouldBe` Just "Vec 2 Int"
    fmap renderScheme (analysisRoot unknownAnalysis) `shouldBe` Just "Int"
    fmap renderScheme (analysisRoot dynamicAnalysis) `shouldBe` Just "{\n  value :: Int;\n}"

  it "rejects unrelated concrete as-casts" $ do
    analyzeText "main.tynix" "1 as String" >>= (`expectLeftContaining` "invalid cast")
    analyzeText
      "main.tynix"
      ( source
          [ "let",
            "  value = { label = \"x\"; };",
            "in value as { count :: Int; }"
          ]
      )
      >>= (`expectLeftContaining` "invalid cast")

  it "rejects invalid numeric validator declarations and out-of-range shape unions" $ do
    analyzeText
      "main.tynix"
      ( source
          [ "let",
            "  bad :: Range 2 1 Nat;",
            "  bad = 1;",
            "in bad"
          ]
      )
      >>= (`expectLeftContaining` "Range bounds are inverted")
    analyzeText
      "main.tynix"
      ( source
          [ "let",
            "  xs :: Vec (2 | Range 4 8 Nat) Int;",
            "  xs = [1 2 3];",
            "in xs"
          ]
      )
      >>= (`expectLeftContaining` "type mismatch")
    analyzeText
      "main.tynix"
      ( source
          [ "let",
            "  grid :: Matrix 2 2 Int;",
            "  grid = [[1 2] [3]];",
            "in grid"
          ]
      )
      >>= (`expectLeftContaining` "type mismatch")
    analyzeText
      "main.tynix"
      ( source
          [ "let",
            "  grid :: Matrix (Range 1 2 Nat) 2 Int;",
            "  grid = [[1 2] [3 4] [5 6]];",
            "in grid"
          ]
      )
      >>= (`expectLeftContaining` "type mismatch")
    analyzeText
      "main.tynix"
      ( source
          [ "let",
            "  cube :: Tensor [2 (Range 1 2 Nat) 1] Int;",
            "  cube = [[[1] [2] [3]] [[4] [5] [6]]];",
            "in cube"
          ]
      )
      >>= (`expectLeftContaining` "type mismatch")

  it "accepts boundary lengths for bounded vectors" $ do
    lowAnalysis <-
      analyzeText "main.tynix" (source ["let xs :: Vec (Range 0 2 Nat) Int;", "    xs = [];", "in xs"])
        >>= expectRight
    highAnalysis <-
      analyzeText "main.tynix" (source ["let xs :: Vec (Range 0 2 Nat) Int;", "    xs = [1 2];", "in xs"])
        >>= expectRight
    fmap renderScheme (analysisRoot lowAnalysis) `shouldBe` Just "Vec (Range 0 2 Nat) Int"
    fmap renderScheme (analysisRoot highAnalysis) `shouldBe` Just "Vec (Range 0 2 Nat) Int"

  it "checks tuple annotations against heterogeneous list literals" $ do
    tupleAnalysis <-
      analyzeText "main.tynix" (source ["let pair :: Tuple [Int String];", "    pair = [1 \"x\"];", "in pair"])
        >>= expectRight
    fmap renderScheme (analysisRoot tupleAnalysis) `shouldBe` Just "Tuple [ Int String ]"

  it "accepts explicitly linear functions whose binders are consumed once" $ do
    analysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "let",
              "  consume :: Int %1 -> Int;",
              "  consume = x: x;",
              "in consume"
            ]
        )
        >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "Int %1 -> Int"

  it "rejects explicitly linear functions that drop or duplicate their binders" $ do
    analyzeText
      "main.tynix"
      (source ["let", "  drop :: Int %1 -> Int;", "  drop = x: 1;", "in drop"])
      >>= (`expectLeftContaining` "[TC0026] linear binder `x` is never used")
    analyzeText
      "main.tynix"
      (source ["let", "  dup :: Int %1 -> Tuple [Int Int];", "  dup = x: [x x];", "in dup"])
      >>= (`expectLeftContaining` "[TC0026] linear binder `x` is used more than once")

  it "treats imports without declarations as dynamic for incremental adoption" $ do
    analysis <- analyzeText "main.tynix" "import ./unknown.nix" >>= expectRight
    analysisRoot analysis `shouldBe` Just (Scheme [] tDynamic)

  it "loads ambient declarations from sibling .d.tynix files" $
    withTempTree
      [ ("app/main.tynix", "import ./lib.nix"),
        ("app/flake.nix", "{}"),
        ("app/types.d.tynix", "declare \"./lib.nix\" { default :: { value :: Int; label :: String; }; };")
      ]
      ( \root -> do
          analysis <- analyzeFile (root <> "/app/main.tynix") >>= expectRight
          analysisRoot analysis
            `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("label", tString), ("value", tInt)])))
      )

  it "gives a shared support cache the same answers as an uncached analysis" $
    withTempTree
      [ ("flake.nix", "{}"),
        ("types.d.tynix", "declare \"./lib.nix\" { default :: { value :: Int; }; };"),
        ("other.d.tynix", "declare \"./other.nix\" { default :: String; };"),
        ("a.tynix", "import ./lib.nix"),
        ("b.tynix", "import ./other.nix"),
        ("c.tynix", "1 + 1")
      ]
      ( \root -> do
          let files = [root </> name | name <- ["a.tynix", "b.tynix", "c.tynix"]]
          uncached <- traverse analyzeFile files
          cache <- newSupportCache
          cached <- traverse (analyzeFileWith cache) files
          map (fmap analysisRoot) cached `shouldBe` map (fmap analysisRoot) uncached
      )

  it "keeps declaration support separate per workspace root when sharing a cache" $
    withTempTree
      [ ("one/flake.nix", "{}"),
        ("one/types.d.tynix", "declare \"./lib.nix\" { default :: { value :: Int; }; };"),
        ("one/main.tynix", "import ./lib.nix"),
        ("two/flake.nix", "{}"),
        ("two/types.d.tynix", "declare \"./lib.nix\" { default :: { value :: String; }; };"),
        ("two/main.tynix", "import ./lib.nix")
      ]
      ( \root -> do
          cache <- newSupportCache
          first <- analyzeFileWith cache (root </> "one/main.tynix") >>= expectRight
          second <- analyzeFileWith cache (root </> "two/main.tynix") >>= expectRight
          analysisRoot first
            `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("value", tInt)])))
          analysisRoot second
            `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("value", tString)])))
      )

  it "still excludes a declaration file from its own support when cached" $
    withTempTree
      [ ("flake.nix", "{}"),
        ("self.d.tynix", "declare \"./self.nix\" { default :: Int; };"),
        ("main.tynix", "import ./self.nix")
      ]
      ( \root -> do
          cache <- newSupportCache
          -- Analyzing the declaration file itself must not fail on a duplicate
          -- ambient target, and must not poison the entry a sibling source sees.
          _ <- analyzeFileWith cache (root </> "self.d.tynix")
          analysis <- analyzeFileWith cache (root </> "main.tynix") >>= expectRight
          analysisRoot analysis `shouldBe` Just (Scheme [] tInt)
      )

  it "surfaces a broken declaration file through the cache too" $
    withTempTree
      [ ("flake.nix", "{}"),
        ("broken.d.tynix", "declare \"./lib.nix\" { default :: ; };"),
        ("main.tynix", "1")
      ]
      ( \root -> do
          cache <- newSupportCache
          first <- analyzeFileWith cache (root </> "main.tynix")
          second <- analyzeFileWith cache (root </> "main.tynix")
          expectLeftContaining first "failed to load declaration file"
          expectLeftContaining second "failed to load declaration file"
      )

  it "loads ambient declarations from the workspace root for nested source files" $
    withTempTree
      [ ("flake.nix", "{}"),
        ("types.d.tynix", "declare \"./lib.nix\" { default :: { value :: Int; }; };"),
        ("app/nested/main.tynix", "import ../../lib.nix")
      ]
      ( \root -> do
          analysis <- analyzeFile (root <> "/app/nested/main.tynix") >>= expectRight
          analysisRoot analysis
            `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("value", tInt)])))
      )

  it "treats tynix.config.tynix as a workspace marker when loading support files" $
    withTempTree
      [ ( "tynix.config.tynix",
          source
            [ "{",
              "  name = \"demo\";",
              "  sourceDir = ./src;",
              "  entry = ./src/main.tynix;",
              "  declarationDir = ./types;",
              "  builtins = false;",
              "}"
            ]
        ),
        ("types.d.tynix", "declare \"./lib.nix\" { default :: { value :: Int; }; };"),
        ("src/nested/main.tynix", "import ../../lib.nix")
      ]
      ( \root -> do
          analysis <- analyzeFile (root <> "/src/nested/main.tynix") >>= expectRight
          analysisRoot analysis
            `shouldBe` Just (Scheme [] (TRecord (Map.fromList [("value", tInt)])))
      )

  it "does not load ambient declarations from nested workspaces into the parent workspace" $
    withTempTree
      [ ("flake.nix", "{}"),
        ( "builtins.d.tynix",
          source
            [ "declare \"builtins\" {",
              "  add :: Int -> Int -> Int;",
              "};"
            ]
        ),
        ("examples/tynix.config.tynix", "{ declarationPacks = []; }"),
        ( "examples/builtins.d.tynix",
          source
            [ "declare \"builtins\" {",
              "  add :: String -> String -> String;",
              "};"
            ]
        ),
        ("app/main.tynix", "builtins.add 1 2")
      ]
      ( \root -> do
          analysis <- analyzeFile (root <> "/app/main.tynix") >>= expectRight
          fmap renderScheme (analysisRoot analysis) `shouldBe` Just "Int"
      )

  it "loads a bundled tynix.config declaration for config imports" $
    withTempTree
      [ ( "tynix.config.tynix",
          source
            [ "{",
              "  name = \"demo\";",
              "  sourceDir = ./src;",
              "  entry = ./src/main.tynix;",
              "  declarationDir = ./types;",
              "  declarationPacks = [];",
              "  buildDir = ./dist;",
              "  generatedDeclarationDir = ./dist/types;",
              "  entries = [];",
              "  include = [];",
              "  exclude = [];",
              "  builtins = true;",
              "}"
            ]
        ),
        ( "tynix.config.d.tynix",
          source
            [ "type TynixProjectPath = Path | String;",
              "type TynixProjectConfig = {",
              "  name :: String;",
              "  sourceDir :: TynixProjectPath;",
              "  entry :: TynixProjectPath;",
              "  declarationDir :: TynixProjectPath;",
              "  declarationPacks :: List TynixProjectPath;",
              "  buildDir :: TynixProjectPath;",
              "  generatedDeclarationDir :: TynixProjectPath;",
              "  entries :: List TynixProjectPath;",
              "  include :: List TynixProjectPath;",
              "  exclude :: List TynixProjectPath;",
              "  builtins :: Bool;",
              "};",
              "declare \"./tynix.config.tynix\" {",
              "  default :: TynixProjectConfig;",
              "};"
            ]
        ),
        ("src/main.tynix", "(import ../tynix.config.tynix).declarationDir")
      ]
      ( \root -> do
          analysis <- analyzeFile (root <> "/src/main.tynix") >>= expectRight
          fmap renderScheme (analysisRoot analysis) `shouldBe` Just "TynixProjectPath"
      )

  it "loads external declaration packs listed in tynix.config.tynix" $
    withTempTree
      [ ("types.d.tynix", "declare \"./lib.nix\" { default :: ExternalSurface; };"),
        ("src/main.tynix", "(import ../lib.nix).value")
      ]
      ( \root -> do
          let packRoot = root <> "-packs"
              packFile = packRoot </> "external.d.tynix"
          createDirectoryIfMissing True packRoot
          TextIO.writeFile packFile "type ExternalSurface = { value :: Int; };"
          TextIO.writeFile
            (root </> "tynix.config.tynix")
            ( source
                [ "{",
                  "  name = \"demo\";",
                  "  declarationPacks = [ \"" <> Text.pack packRoot <> "\" ];",
                  "}"
                ]
            )
          analysis <- analyzeFile (root </> "src/main.tynix") >>= expectRight
          fmap renderScheme (analysisRoot analysis) `shouldBe` Just "Int"
      )

  it "joins field types when selecting from unions of compatible records" $ do
    analysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "let",
              "  candidate = if true then { value = 1; } else { value = \"x\"; };",
              "in candidate.value"
            ]
        )
        >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "1 | \"x\""

  it "rejects duplicate let bindings and signatures" $ do
    analyzeText "main.tynix" (source ["let", "  value = 1;", "  value = 2;", "in value"])
      >>= (`expectLeftContaining` "duplicate bindings")
    analyzeText "main.tynix" (source ["let", "  value :: Int;", "  value :: String;", "  value = 1;", "in value"])
      >>= (`expectLeftContaining` "duplicate signatures")

  it "rejects passing concrete types to higher-kinded parameters" $
    analyzeText
      "main.tynix"
      ( source
          [ "type Twice f a = f (f a);",
            "let",
            "  bad :: Twice Int String;",
            "  bad = 1;",
            "in bad"
          ]
      )
      >>= (`expectLeftContaining` "kind mismatch")

  it "rejects indexed annotations with invalid dimensions and mismatched shapes" $ do
    analyzeText "main.tynix" "let xs :: Vec \"wide\" Int; xs = [1]; in xs"
      >>= (`expectLeftContaining` "nat-like")
    analyzeText "main.tynix" "let xs :: Vec (Unit \"ms\" Nat) Int; xs = [1]; in xs"
      >>= (`expectLeftContaining` "nat-like")
    analyzeText "main.tynix" "let xs :: Vec (Range 0.0 2.0 Nat) Int; xs = [1]; in xs"
      >>= (`expectLeftContaining` "nat-like")
    analyzeText "main.tynix" "let xs :: Vec 2 Int; xs = [1 2 3]; in xs"
      >>= (`expectLeftContaining` "type mismatch")

  it "rejects missing bindings and explicit signature mismatches" $ do
    analyzeText "main.tynix" (source ["let", "  value :: Int;", "in 1"])
      >>= (`expectLeftContaining` "missing bindings for signatures")
    analyzeText "main.tynix" (source ["let", "  value :: String;", "  value = 1;", "in value"])
      >>= (`expectLeftContaining` "type mismatch")

  it "rejects duplicate attribute names from fields and inherit clauses" $ do
    analyzeText "main.tynix" (source ["let", "  value = 1;", "in { value = 2; inherit value; }"])
      >>= (`expectLeftContaining` "duplicate attribute")

  it "reports occurs checks for self-application" $
    analyzeText "main.tynix" "let omega = x: x x; in omega" >>= (`expectLeftContaining` "occurs check failed")

  it "reports parse failures from sibling declaration files" $
    withTempTree
      [ ("app/main.tynix", "import ./lib.nix"),
        ("app/types.d.tynix", "declare \"./lib.nix\" { default :: ; };")
      ]
      (\root -> analyzeFile (root <> "/app/main.tynix") >>= (`expectLeftContaining` "failed to load declaration file"))

  it "rejects executable declaration files while loading ambient support" $
    withTempTree
      [ ("app/main.tynix", "import ./lib.nix"),
        ("app/types.d.tynix", "declare \"./lib.nix\" { default :: Int; }; 1")
      ]
      (\root -> analyzeFile (root <> "/app/main.tynix") >>= (`expectLeftContaining` "must not contain executable expressions"))

  it "rejects duplicate ambient targets across declaration files" $
    withTempTree
      [ ("app/main.tynix", "import ./lib.nix"),
        ("app/flake.nix", "{}"),
        ("app/a.d.tynix", "declare \"./lib.nix\" { default :: Int; };"),
        ("app/b.d.tynix", "declare \"./lib.nix\" { default :: String; };")
      ]
      (\root -> analyzeFile (root <> "/app/main.tynix") >>= (`expectLeftContaining` "duplicate ambient declarations"))

  it "rejects duplicate entries inside one ambient declaration" $
    analyzeText
      "main.tynix"
      "declare \"./lib.nix\" { value :: Int; value :: String; }; import ./lib.nix"
      >>= (`expectLeftContaining` "duplicate ambient entry")

  it "prefers inline ambient declarations over workspace declarations" $
    withTempTree
      [ ("flake.nix", "{}"),
        ("types.d.tynix", "declare \"./lib.nix\" { default :: String; };"),
        ( "app/main.tynix",
          source
            [ "declare \"../lib.nix\" { default :: Int; };",
              "import ../lib.nix"
            ]
        )
      ]
      ( \root -> do
          analysis <- analyzeFile (root <> "/app/main.tynix") >>= expectRight
          analysisRoot analysis `shouldBe` Just (Scheme [] tInt)
      )

  it "suppresses binding errors with @tynix-ignore and falls back to dynamic" $ do
    analysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "let",
              "  # @tynix-ignore",
              "  value = missing;",
              "in value"
            ]
        )
        >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "dynamic"
    fmap renderScheme (Map.lookup "value" (analysisBindings analysis)) `shouldBe` Just "dynamic"

  it "lets a signature-line @tynix-expected suppress the matching binding mismatch" $ do
    analysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "let",
              "  # @tynix-expected",
              "  value :: Int;",
              "  value = \"oops\";",
              "in value"
            ]
        )
        >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "Int"
    fmap renderScheme (Map.lookup "value" (analysisBindings analysis)) `shouldBe` Just "Int"

  it "suppresses root-expression failures with @tynix-ignore" $ do
    analysis <-
      analyzeText
        "main.tynix"
        ( source
            [ "# @tynix-ignore",
              "missing"
            ]
        )
        >>= expectRight
    fmap renderScheme (analysisRoot analysis) `shouldBe` Just "dynamic"

  it "rejects unused @tynix-expected directives on bindings and roots" $ do
    analyzeText
      "main.tynix"
      ( source
          [ "let",
            "  # @tynix-expected",
            "  value = 1;",
            "in value"
          ]
      )
      >>= (`expectLeftContaining` "unused @tynix-expected directive on binding")
    analyzeText
      "main.tynix"
      ( source
          [ "# @tynix-expected",
            "1"
          ]
      )
      >>= (`expectLeftContaining` "unused @tynix-expected directive on root expression")
