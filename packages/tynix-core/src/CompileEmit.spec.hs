{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Compile (compileProgram)
import Control.Monad (when)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Driver (compileFile, compileText, emitFile, emitFileTo, emitText, parseText)
import Parser (parseProgram)
import Syntax
import System.FilePath ((</>))
import Test.Hspec
import TestSupport (expectLeftContaining, expectRight, fixturePathCandidates, source, withCopiedFixtureTree, withTempTree)
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "compile and emit" $ do
  it "erases type syntax when compiling to nix" $ do
    output <-
      compileText
        "main.tynix"
        ( source
            [ "let",
              "  id :: forall a. a -> a;",
              "  id = x: x;",
              "in id"
            ]
        )
        >>= expectRight
    "::" `Text.isInfixOf` output `shouldBe` False
    "id = x: x;" `Text.isInfixOf` output `shouldBe` True

  it "matches the exact pretty-printed nix output for nested control flow" $ do
    output <-
      compileText
        "main.tynix"
        ( source
            [ "let",
              "  value :: Int;",
              "  value = 1;",
              "in",
              "  if true then { inherit value; mapper = (x :: Int): x; } else { value = 2; }"
            ]
        )
        >>= expectRight
    output
      `shouldBe` Text.stripEnd
        ( source
            [ "let",
              "  value = 1;",
              "in if true",
              "then {",
              "  inherit value;",
              "  mapper = x: x;",
              "}",
              "else {",
              "  value = 2;",
              "}"
            ]
        )

  it "refuses to compile declaration-only sources" $
    compileText "types.d.tynix" "declare \"./lib.nix\" { default :: Int; };" >>= (`expectLeftContaining` "declaration-only")

  it "erases nested annotations while preserving nix control flow and inherit" $ do
    output <-
      compileText
        "main.tynix"
        ( source
            [ "let",
              "  value :: Int;",
              "  value = 1;",
              "in",
              "  if true then { inherit value; mapper = (x :: Int): x; } else { value = 2; }"
            ]
        )
        >>= expectRight
    "::" `Text.isInfixOf` output `shouldBe` False
    "if true" `Text.isInfixOf` output `shouldBe` True
    "then {" `Text.isInfixOf` output `shouldBe` True
    "inherit value;" `Text.isInfixOf` output `shouldBe` True
    "mapper =" `Text.isInfixOf` output `shouldBe` True
    "x:" `Text.isInfixOf` output `shouldBe` True

  it "erases as-casts when compiling executable output" $ do
    output <-
      compileText
        "main.tynix"
        ( source
            [ "let",
              "  value = { count = 1; }.count as Int;",
              "in value"
            ]
        )
        >>= expectRight
    " as " `Text.isInfixOf` output `shouldBe` False
    "value = {" `Text.isInfixOf` output `shouldBe` True
    ".count;" `Text.isInfixOf` output `shouldBe` True

  it "preserves relational and boolean operators when compiling to nix" $ do
    output <-
      compileText
        "main.tynix"
        ( source
            [ "let",
              "  a = 1;",
              "  b = 2;",
              "  c = 2;",
              "in if a < 10 && b == c then 1 else 2"
            ]
        )
        >>= expectRight
    "<" `Text.isInfixOf` output `shouldBe` True
    "&&" `Text.isInfixOf` output `shouldBe` True
    "==" `Text.isInfixOf` output `shouldBe` True

  it "preserves subtraction and multiplication when compiling to nix" $ do
    output <-
      compileText
        "main.tynix"
        ( source
            [ "let",
              "  a = 6;",
              "  b = 2;",
              "in a - b * a"
            ]
        )
        >>= expectRight
    " - " `Text.isInfixOf` output `shouldBe` True
    " * " `Text.isInfixOf` output `shouldBe` True

  it "renders same-precedence operator chains without redundant parentheses" $ do
    output <-
      compileText
        "main.tynix"
        ( source
            [ "let",
              "  a = 1;",
              "  b = 2;",
              "  c = 3;",
              "in a + b + c"
            ]
        )
        >>= expectRight
    "a + b + c" `Text.isInfixOf` output `shouldBe` True

  it "parenthesizes control flow used as an operator operand for valid nix" $ do
    output <-
      compileText
        "main.tynix"
        ( source
            [ "let",
              "  a = 1;",
              "in a + (if true then 1 else 2)"
            ]
        )
        >>= expectRight
    "+ (if true" `Text.isInfixOf` output `shouldBe` True

  it "preserves list concatenation when compiling to nix" $ do
    output <-
      compileText
        "main.tynix"
        "[1 2] ++ [3 4]"
        >>= expectRight
    "++" `Text.isInfixOf` output `shouldBe` True

  it "preserves attribute-set update when compiling to nix" $ do
    output <-
      compileText
        "main.tynix"
        "{ a = 1; } // { b = 2; }"
        >>= expectRight
    "//" `Text.isInfixOf` output `shouldBe` True

  it "preserves the attribute-presence test when compiling to nix" $ do
    output <-
      compileText
        "main.tynix"
        "{ a = 1; } ? a"
        >>= expectRight
    "?" `Text.isInfixOf` output `shouldBe` True

  it "preserves assert expressions when compiling to nix" $ do
    output <-
      compileText
        "main.tynix"
        "assert true; 1"
        >>= expectRight
    "assert true;" `Text.isInfixOf` output `shouldBe` True

  it "preserves recursive attribute sets when compiling to nix" $ do
    output <- compileText "main.tynix" "rec { a = 1; b = a; }" >>= expectRight
    "rec {" `Text.isInfixOf` output `shouldBe` True

  it "emits field-wise declarations for attrset roots" $ do
    output <- emitText "main.tynix" "{ name = \"tynix\"; count = 1; }" >>= expectRight
    program <- expectRight (parseDecl "main.d.tynix" output)
    programAmbient program
      `shouldBe` [ AmbientDecl
                     "./main.nix"
                     [ AmbientEntry "count" (TLit (LInt 1)),
                       AmbientEntry "name" (TLit (LString "tynix"))
                     ]
                 ]

  it "matches the exact emitted declaration text for record roots" $ do
    output <- emitText "main.tynix" "{ name = \"tynix\"; count = 1; }" >>= expectRight
    output
      `shouldBe` Text.stripEnd
        ( source
            [ "declare \"./main.nix\" {",
              "  count :: 1;",
              "  name :: \"tynix\";",
              "};"
            ]
        )

  it "emits cast-asserted root types while preserving record-shaped exports" $ do
    scalarOutput <- emitText "main.tynix" "1 as Number" >>= expectRight
    recordOutput <- emitText "main.tynix" "{ value = 1; } as { value :: Int; }" >>= expectRight
    Text.isInfixOf "default :: Number;" scalarOutput `shouldBe` True
    Text.isInfixOf "default ::" recordOutput `shouldBe` False
    Text.isInfixOf "value :: Int;" recordOutput `shouldBe` True

  it "compiles float literals without changing their surface form" $ do
    output <- compileText "main.tynix" "1.5" >>= expectRight
    output `shouldBe` "1.5"

  it "keeps integer-looking floats distinct from integers" $ do
    compiled <- compileText "main.tynix" "1.0" >>= expectRight
    emitted <- emitText "main.tynix" "1.0" >>= expectRight
    compiled `shouldBe` "1.0"
    Text.isInfixOf "default :: 1.0;" emitted `shouldBe` True

  it "compiles and emits negative numeric literals" $ do
    compiled <-
      compileText
        "main.tynix"
        ( source
            [ "let",
              "  x = -1;",
              "  y = -1.0;",
              "in y"
            ]
        )
        >>= expectRight
    emitted <- emitText "main.tynix" "-1.0" >>= expectRight
    Text.isInfixOf "x = -1;" compiled `shouldBe` True
    Text.isInfixOf "y = -1.0;" compiled `shouldBe` True
    Text.isInfixOf "default :: -1.0;" emitted `shouldBe` True

  it "compiles infix addition without rewriting the operator away" $ do
    output <- compileText "math.nix" "{ inc = x: x + 1; }" >>= expectRight
    "x + 1" `Text.isInfixOf` output `shouldBe` True

  it "preserves with expressions when compiling to nix" $ do
    output <-
      compileText
        "main.tynix"
        ( source
            [ "let",
              "  scope = { a = 1; };",
              "in with scope; a"
            ]
        )
        >>= expectRight
    "with scope;" `Text.isInfixOf` output `shouldBe` True

  it "preserves string interpolation when compiling to nix" $ do
    output <-
      compileText
        "main.tynix"
        ( source
            [ "let",
              "  x = \"hi\";",
              "in \"a${x}b\""
            ]
        )
        >>= expectRight
    "\"a${x}b\"" `Text.isInfixOf` output `shouldBe` True

  it "compiles quoted attr names and dynamic selections without erasing them" $ do
    output <-
      compileText
        "main.tynix"
        ( source
            [ "let",
              "  system = \"aarch64-darwin\";",
              "in { \"aarch64-darwin\" = 1; }.${system}"
            ]
        )
        >>= expectRight
    "aarch64-darwin = 1;" `Text.isInfixOf` output `shouldBe` True
    ".${system}" `Text.isInfixOf` output `shouldBe` True

  it "compiles attrset lambda binders and indented strings" $ do
    output <-
      compileText
        "main.tynix"
        ( source
            [ "{ self, nixpkgs, ... }:",
              "  {",
              "    shellHook = ''",
              "      export LANG=C.UTF-8",
              "      echo ready",
              "    '';",
              "    inherit self nixpkgs;",
              "  }"
            ]
        )
        >>= expectRight
    "{ self, nixpkgs, ... }:" `Text.isInfixOf` output `shouldBe` True
    "shellHook = ''" `Text.isInfixOf` output `shouldBe` True
    "echo ready" `Text.isInfixOf` output `shouldBe` True

  it "emits float literal roots precisely" $ do
    output <- emitText "main.tynix" "1.5" >>= expectRight
    Text.isInfixOf "default :: 1.5;" output `shouldBe` True

  it "emits default for non-record or quantified roots" $ do
    output <-
      emitText
        "main.tynix"
        ( source
            [ "let",
              "  id :: forall a. a -> a;",
              "  id = x: x;",
              "in id"
            ]
        )
        >>= expectRight
    program <- expectRight (parseDecl "main.d.tynix" output)
    programAmbient program
      `shouldBe` [ AmbientDecl
                     "./main.nix"
                     [AmbientEntry "default" (TForall ["t0"] (TFun Many (TVar "t0") (TVar "t0")))]
                 ]

  it "matches the exact emitted declaration text for polymorphic defaults" $ do
    output <-
      emitText
        "main.tynix"
        ( source
            [ "let",
              "  id :: forall a. a -> a;",
              "  id = x: x;",
              "in id"
            ]
        )
        >>= expectRight
    output
      `shouldBe` Text.stripEnd
        ( source
            [ "declare \"./main.nix\" {",
              "  default :: forall t0. t0 -> t0;",
              "};"
            ]
        )

  it "preserves aliases when emitting declaration files" $ do
    output <- emitText "main.tynix" "type Box t = { value :: t; }; { boxed = { value = 1; }; }" >>= expectRight
    program <- expectRight (parseDecl "main.d.tynix" output)
    programAliases program `shouldBe` [plainAlias "Box" ["t"] (TRecord (Map.fromList [("value", TVar "t")]))]

  it "emits indexed container roots with their inferred shapes" $ do
    output <- emitText "main.tynix" "[[1 2] [3 4]]" >>= expectRight
    program <- expectRight (parseDecl "main.d.tynix" output)
    programAmbient program
      `shouldBe` [ AmbientDecl
                     "./main.nix"
                     [ AmbientEntry
                         "default"
                         (TApp (TApp (TApp (TCon "Matrix") (TLit (LInt 2))) (TLit (LInt 2))) (TUnion [TLit (LInt 1), TLit (LInt 2), TLit (LInt 3), TLit (LInt 4)]))
                     ]
                 ]

  it "emits tuple roots for heterogeneous list literals" $ do
    output <- emitText "main.tynix" "[1 \"x\"]" >>= expectRight
    program <- expectRight (parseDecl "main.d.tynix" output)
    programAmbient program
      `shouldBe` [ AmbientDecl
                     "./main.nix"
                     [AmbientEntry "default" (TApp (TCon "Tuple") (TTypeList [TLit (LInt 1), TLit (LString "x")]))]
                 ]

  it "round-trips higher-rank tensor declarations through the emitter" $ do
    output <- emitText "main.tynix" "[[[1] [2]] [[3] [4]]]" >>= expectRight
    program <- expectRight (parseDecl "main.d.tynix" output)
    programAmbient program
      `shouldBe` [ AmbientDecl
                     "./main.nix"
                     [ AmbientEntry
                         "default"
                         (TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 2), TLit (LInt 2), TLit (LInt 1)])) (TUnion [TLit (LInt 1), TLit (LInt 2), TLit (LInt 3), TLit (LInt 4)]))
                     ]
                 ]

  it "emits ragged nested list roots as structural list declarations" $ do
    output <- emitText "main.tynix" "[[1] [2 3]]" >>= expectRight
    Text.isInfixOf "default :: List (Vec (1 | 2) (1 | 2 | 3));" output `shouldBe` True

  it "emits linear function arrows in declaration output" $ do
    output <-
      emitText
        "main.tynix"
        ( source
            [ "let",
              "  consume :: Int %1 -> Int;",
              "  consume = x: x;",
              "in consume"
            ]
        )
        >>= expectRight
    Text.isInfixOf "default :: Int %1 -> Int;" output `shouldBe` True

  it "emits numeric validation and unit wrappers in declaration output" $ do
    output <-
      emitText
        "main.tynix"
        ( source
            [ "let",
              "  timeout :: Unit \"ms\" (Range 0 5000 Nat);",
              "  timeout = 2500;",
              "in timeout"
            ]
        )
        >>= expectRight
    Text.isInfixOf "default :: Unit \"ms\" (Range 0 5000 Nat);" output `shouldBe` True

  it "emits any and unknown annotations distinctly" $ do
    anyOutput <-
      emitText
        "main.tynix"
        ( source
            [ "let",
              "  value :: any;",
              "  value = 1;",
              "in value"
            ]
        )
        >>= expectRight
    unknownOutput <-
      emitText
        "main.tynix"
        ( source
            [ "let",
              "  value :: unknown;",
              "  value = 1;",
              "in value"
            ]
        )
        >>= expectRight
    Text.isInfixOf "default :: any;" anyOutput `shouldBe` True
    Text.isInfixOf "default :: unknown;" unknownOutput `shouldBe` True

  it "erases numeric and unit annotations when compiling executable output" $ do
    output <-
      compileText
        "main.tynix"
        ( source
            [ "let",
              "  timeout :: Unit \"ms\" (Range 0 5000 Nat);",
              "  timeout = 2500;",
              "in timeout"
            ]
        )
        >>= expectRight
    "Unit" `Text.isInfixOf` output `shouldBe` False
    "Range" `Text.isInfixOf` output `shouldBe` False
    "timeout = 2500;" `Text.isInfixOf` output `shouldBe` True

  it "emits bounded indexed annotations without erasing dependent shape constraints" $ do
    output <-
      emitText
        "main.tynix"
        ( source
            [ "let",
              "  grid :: Matrix (Range 1 2 Nat) 2 Int;",
              "  grid = [[1 2] [3 4]];",
              "in grid"
            ]
        )
        >>= expectRight
    Text.isInfixOf "default :: Matrix (Range 1 2 Nat) 2 Int;" output `shouldBe` True

  it "emits exact-zero and refined tensor annotations precisely" $ do
    zeroOutput <-
      emitText
        "main.tynix"
        ( source
            [ "let",
              "  xs :: Vec (Range 0 0 Nat) Int;",
              "  xs = [];",
              "in xs"
            ]
        )
        >>= expectRight
    tensorOutput <-
      emitText
        "main.tynix"
        ( source
            [ "let",
              "  cube :: Tensor [2 (Range 1 2 Nat) 1] Int;",
              "  cube = [[[1] [2]] [[3] [4]]];",
              "in cube"
            ]
        )
        >>= expectRight
    Text.isInfixOf "default :: Vec (Range 0 0 Nat) Int;" zeroOutput `shouldBe` True
    Text.isInfixOf "default :: Tensor [ 2 (Range 1 2 Nat) 1 ] Int;" tensorOutput `shouldBe` True

  it "emits float-based numeric validators in declaration output" $ do
    output <-
      emitText
        "main.tynix"
        ( source
            [ "let",
              "  ratio :: Range 0.0 1.0 Float;",
              "  ratio = 0.5;",
              "in ratio"
            ]
        )
        >>= expectRight
    Text.isInfixOf "default :: Range 0.0 1.0 Float;" output `shouldBe` True

  it "emits declarations relative to the source basename" $ do
    output <- emitText "nested/app/main.tynix" "{ value = 1; }" >>= expectRight
    Text.isInfixOf "declare \"./main.nix\"" output `shouldBe` True

  it "rewrites declaration targets relative to an explicit output path" $
    withTempTree
      [("src/main.tynix", "1")]
      ( \root -> do
          output <- emitFileTo (root <> "/src/main.tynix") (root <> "/types/generated/main.d.tynix") >>= expectRight
          Text.isInfixOf "declare \"../../src/main.nix\"" output `shouldBe` True
      )

  describe "compiled output is a fixed point" $ do
    -- Compilation renders the erased AST back to Nix source. Two things have
    -- to hold for that to be trustworthy, and neither is implied by the exact
    -- output tests above: the result must re-parse, and compiling it again
    -- must change nothing. The second is what catches layout that leaks into
    -- verbatim string bodies, where the drift compounds per compile.
    it "re-parses and compiles to itself for every corpus entry" $
      mapM_ expectCompileFixedPoint compileCorpus

    it "erases every type-only construct" $
      mapM_ expectNoTypeSyntax compileCorpus

  describe "golden regressions" $
    mapM_ goldenFixtureSpec compileEmitFixtures
  where
    parseDecl = parseText

expectCompileFixedPoint :: (String, Text) -> Expectation
expectCompileFixedPoint (label, input) = do
  once <- compileOrFail label input
  twice <- compileOrFail (label <> " (second pass)") once
  when (twice /= once) $
    expectationFailure
      (label <> ": compiling twice changed the output\nfirst:\n" <> Text.unpack once <> "\nsecond:\n" <> Text.unpack twice)

expectNoTypeSyntax :: (String, Text) -> Expectation
expectNoTypeSyntax (label, input) = do
  compiled <- compileOrFail label input
  mapM_
    ( \needle ->
        when (needle `Text.isInfixOf` compiled) $
          expectationFailure (label <> ": compiled output still contains " <> show needle <> "\n" <> Text.unpack compiled)
    )
    [" :: ", "type ", "declare ", " as "]

compileOrFail :: String -> Text -> IO Text
compileOrFail label input =
  case parseProgram "corpus.tynix" input of
    Left err -> expectationFailure (label <> ": parse failed: " <> Text.unpack err) >> fail "parse failed"
    Right program ->
      case compileProgram program of
        Left err -> expectationFailure (label <> ": compile failed: " <> Text.unpack err) >> fail "compile failed"
        Right compiled -> pure compiled

-- | Sources chosen to cover the executable grammar, with the type-only syntax
-- that has to be erased woven through them.
compileCorpus :: [(String, Text)]
compileCorpus =
  [ ("literals", "{ int = 1; float = 1.5; neg = -2; str = \"a\"; nul = null; yes = true; }"),
    ("escaped strings", "{ quote = \"say \\\"hi\\\"\"; slash = \"a\\\\b\"; dollar = \"a\\${b}\"; }"),
    ("indented string", source ["{", "  hook = ''", "    echo 'go'", "    export DIR=\"$HOME\"", "  '';", "}"]),
    ("nested indented string", source ["{", "  outer = {", "    hook = ''", "      line one", "      line two", "    '';", "  };", "}"]),
    ("interpolation", "let name = \"world\"; in \"hello ${name} and ${\"more\"}\""),
    ("indented interpolation", source ["let name = \"x\";", "in ''", "  value ${name}", "''"]),
    ("lambdas and application", "let add = a: b: a + b; in add 1 2"),
    ("annotated lambdas", "let f :: Int -> Int; f = (x :: Int): x + 1; in f 1"),
    ("attrset patterns", "{ self, nixpkgs, ... }: { inherit self; }"),
    ("operators", "let a = 1; b = 2; in (a + b) * 3 - 4 == 5 && !(a < b) || a >= b"),
    ("concat and update", "{ xs = [ 1 2 ] ++ [ 3 ]; merged = { a = 1; } // { b = 2; }; }"),
    ("has-attr", "let r = { a = 1; }; in r ? a"),
    ("control flow", "if 1 < 2 then (assert true; 1) else (with { x = 1; }; x)"),
    ("rec and inherit", "let outer = 1; in rec { inner = outer; alias = inner; inherit outer; }"),
    ("selections", "let r = { a = { b = 1; }; }; k = \"a\"; in [ r.a.b r.${k} ]"),
    ("quoted attribute names", "{ \"with space\" = 1; \"0lead\" = 2; }"),
    ("paths", "{ here = ./lib.nix; up = ../shared.nix; root = /etc/hosts; }"),
    ("casts", "let value = ({ a = 1; } as { a :: Int; }); in value"),
    ( "aliases and ambient declarations",
      source
        [ "type Option a = { tag :: \"some\"; value :: a; } | { tag :: \"none\"; };",
          "declare \"./legacy.nix\" { mkPkg :: { name :: String; } -> dynamic; };",
          "let",
          "  opt :: Option Int;",
          "  opt = { tag = \"some\"; value = 1; };",
          "in opt"
        ]
    ),
    ( "indexed annotations",
      source
        [ "let",
          "  xs :: Vec (Range 2 4 Nat) Int;",
          "  xs = [ 1 2 3 ];",
          "  grid :: Matrix 2 2 Int;",
          "  grid = [ [ 1 2 ] [ 3 4 ] ];",
          "in { inherit xs grid; }"
        ]
    )
  ]

compileEmitFixtures :: [FilePath]
compileEmitFixtures =
  [ "record-root",
    "poly-id",
    "scalar-cast",
    "float-root",
    "linear-fn"
  ]

goldenFixtureSpec :: FilePath -> Spec
goldenFixtureSpec name =
  it ("matches the stored compile and emit output for " <> name) $ do
    fixtureRoot <-
      fixturePathCandidates
        [ "packages/tynix-core/fixtures/compile-emit/" <> name,
          "fixtures/compile-emit/" <> name
        ]
    withCopiedFixtureTree fixtureRoot $ \isolatedRoot -> do
      let sourceFile = isolatedRoot </> "main.tynix"
          expectedNixFile = isolatedRoot </> "expected.nix"
          expectedDeclFile = isolatedRoot </> "expected.d.tynix"
      compiled <- Text.stripEnd <$> (compileFile sourceFile >>= expectRight)
      expectedNix <- Text.stripEnd <$> TextIO.readFile expectedNixFile
      emitted <- Text.stripEnd <$> (emitFile sourceFile >>= expectRight)
      expectedDecl <- Text.stripEnd <$> TextIO.readFile expectedDeclFile
      compiled `shouldBe` expectedNix
      emitted `shouldBe` expectedDecl
