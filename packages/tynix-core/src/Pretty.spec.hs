{-# LANGUAGE OverloadedStrings #-}

-- | Round-trip and shape tests for the shared renderer.
--
-- Every surface `tynix` prints is also a surface it parses, so the strongest
-- statement this module can make is that rendering never changes meaning:
-- parse . render must be the identity on programs, expressions, and types.
-- The string cases matter most, because a missing escape there silently
-- miscompiles `.tynix` into `.nix` that no longer holds the same text.
module Main (main) where

import Control.Monad.Reader (runReaderT)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Parser (parseProgram)
import ParserLexer (sc)
import ParserType (typeParser)
import Pretty
import Syntax
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec (eof, errorBundlePretty, runParser)
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "string escaping" $ do
    it "round-trips double-quoted text through every escape-worthy character" $
      mapM_
        expectStringRoundTrip
        [ "",
          "plain",
          "with \"quotes\"",
          "back\\slash",
          "trailing backslash \\",
          "line\nbreak",
          "carriage\rreturn",
          "tab\there",
          "dollar $ alone",
          "interp ${not-really}",
          "$",
          "${",
          "}",
          "mixed \"$\\{\" \n end",
          "unicode: \12354\12356\12358 \128512"
        ]

    it "round-trips indented text through every escape-worthy character" $
      mapM_
        expectIndentedRoundTrip
        [ "",
          "plain",
          "'",
          "''",
          "'''",
          "''''",
          "leading ' quote",
          "trailing quote '",
          "'leading",
          "double '' inside",
          "${interp}",
          "'${",
          "$",
          "''$",
          "shell: echo 'hello' && echo \"world\"",
          "\n  multi\n  line\n"
        ]

    it "keeps a lone quote unescaped so embedded shell stays readable" $
      renderExpr (EString (Indented "echo 'hi' now"))
        `shouldBe` "''echo 'hi' now''"

    it "escapes an antiquotation opener in indented text" $
      renderExpr (EString (Indented "${x}"))
        `shouldBe` "''''${x}''"

    it "escapes a dollar only when it would open an antiquotation" $ do
      renderExpr (EString (DoubleQuoted "a $ b")) `shouldBe` "\"a $ b\""
      renderExpr (EString (DoubleQuoted "a ${b}")) `shouldBe` "\"a \\${b}\""

    it "round-trips interpolated strings whose literal runs need escaping" $ do
      expectExprRoundTrip
        (EInterp InterpDouble [StrText "a\"b", StrExpr (EVar "x"), StrText "c\\d"])
      expectExprRoundTrip
        (EInterp InterpDouble [StrExpr (EVar "x"), StrText "${literal}"])
      expectExprRoundTrip
        (EInterp InterpIndented [StrText "'", StrExpr (EVar "x"), StrText "'"])
      expectExprRoundTrip
        (EInterp InterpIndented [StrText "a'", StrExpr (EVar "x"), StrText "'b"])
      expectExprRoundTrip
        (EInterp InterpIndented [StrText "${", StrExpr (EVar "x"), StrText "}"])

    it "round-trips generated nasty double-quoted text" $
      property $ \(NastyText raw) ->
        roundTripsAsExpr (EString (DoubleQuoted raw))

    it "round-trips generated nasty indented text" $
      property $ \(NastyText raw) ->
        roundTripsAsExpr (EString (Indented raw))

    it "round-trips generated nasty text on both sides of an antiquotation" $
      property $ \(NastyText leading) (NastyText trailing) (NastyForm form) ->
        let parts =
              [StrText leading | not (Text.null leading)]
                <> [StrExpr (EVar "x")]
                <> [StrText trailing | not (Text.null trailing)]
         in roundTripsAsExpr (EInterp form parts)

  describe "multi-line string bodies" $ do
    it "does not pick up the enclosing indentation" $ do
      let rendered =
            renderExpr
              (EAttrSet [AttrField "hook" (EString (Indented "\nline one\nline two\n"))])
      rendered `shouldBe` "{\n  hook = ''\nline one\nline two\n'';\n}"

    it "round-trips a multi-line indented string nested in an attribute set" $
      expectExprRoundTrip
        ( EAttrSet
            [ AttrField "hook" (EString (Indented "\n  echo 'go'\n  export DIR=\"$HOME\"\n"))
            ]
        )

    it "round-trips a multi-line indented string nested two levels deep" $
      expectExprRoundTrip
        ( EAttrSet
            [ AttrField
                "outer"
                (EAttrSet [AttrField "hook" (EString (Indented "\n  a\n  b\n"))])
            ]
        )

    it "round-trips a multi-line interpolated indented string" $
      expectExprRoundTrip
        ( EAttrSet
            [ AttrField
                "hook"
                ( EInterp
                    InterpIndented
                    [StrText "\n  echo ", StrExpr (EVar "name"), StrText "\n  done\n"]
                )
            ]
        )

    it "keeps compilation idempotent for indented strings" $ do
      -- Re-rendering an already-rendered program must not keep adding
      -- indentation to the string body.
      let program = EAttrSet [AttrField "hook" (EString (Indented "\n  body\n"))]
          once = renderExpr program
      reparsed <- parseExprOrFail once
      renderExpr reparsed `shouldBe` once

  describe "attribute names" $ do
    it "quotes and escapes names that are not bare identifiers" $ do
      renderExpr (EAttrSet [AttrField "with space" (EInt 1)])
        `shouldSatisfy` Text.isInfixOf "\"with space\""
      renderExpr (EAttrSet [AttrField "quote\"inside" (EInt 1)])
        `shouldSatisfy` Text.isInfixOf "\"quote\\\"inside\""

    it "round-trips attribute names that need quoting" $
      mapM_
        (\name -> expectExprRoundTrip (EAttrSet [AttrField name (EInt 1)]))
        [ "plain",
          "with space",
          "quote\"inside",
          "back\\slash",
          "0leading",
          "",
          "inherit"
        ]

    it "keeps `inherit` quoted so it is not read back as a keyword" $
      renderExpr (EAttrSet [AttrField "inherit" (EInt 1)])
        `shouldBe` "{\n  \"inherit\" = 1;\n}"

  describe "renderProgram" $ do
    it "is meaning-preserving and idempotent on a representative program" $
      expectFormatterStable
        [ "type Option a = { tag :: \"some\"; value :: a; } | { tag :: \"none\"; };",
          "declare \"./legacy.nix\" { mkPkg :: { name :: String; } -> dynamic; };",
          "let",
          "  greeting :: String;",
          "  greeting = \"hello \\\"world\\\"\";",
          "  script = ''echo 'hi' && test ${greeting}'';",
          "  add = a: b: a + b;",
          "in { inherit greeting; total = add 1 2; }"
        ]

    it "is meaning-preserving on operator-heavy input" $
      expectFormatterStable
        [ "let",
          "  value = (1 + 2) * 3 - 4;",
          "  flag = !(1 < 2) || 3 >= 4 && 5 == 6;",
          "  merged = { a = 1; } // { b = 2; };",
          "  items = [ 1 2 ] ++ [ 3 ];",
          "in { inherit value flag merged items; }"
        ]

    it "renders declaration-only programs without a root expression" $
      expectFormatterStable
        [ "type Id a = a;",
          "declare \"./lib.nix\" { default :: Id Int; };"
        ]

  describe "renderType" $ do
    it "round-trips representative types through the type parser" $
      mapM_
        expectTypeRoundTrip
        [ tInt,
          tDynamic,
          tUnknown,
          tAny,
          TLit (LString "with \"quote\""),
          TLit (LInt (-3)),
          TLit (LBool True),
          tList tInt,
          TFun Many tInt tString,
          TFun One tInt (TFun Many tString tBool),
          TRecord (Map.fromList [("a", tInt), ("b space", tString)]),
          TUnion [tInt, tString, tNull],
          TForall ["a"] (TFun Many (TVar "a") (TVar "a")),
          TApp (TApp (TCon "Vec") (TLit (LInt 3))) tInt,
          TApp (TApp (TApp (TCon "Matrix") (TLit (LInt 2))) (TLit (LInt 3))) tInt,
          TApp
            (TApp (TCon "Tensor") (TTypeList [TLit (LInt 2), TLit (LInt 3), TLit (LInt 4)]))
            tInt,
          TApp (TCon "Tuple") (TTypeList [tInt, tString]),
          TConditional (tList tInt) (tList (TInfer "a")) (TVar "a") tDynamic
        ]

    it "parenthesizes nested arrows on the argument side" $
      renderType (TFun Many (TFun Many tInt tInt) tInt)
        `shouldBe` "(Int -> Int) -> Int"

    it "keeps right-nested arrows unparenthesized" $
      renderType (TFun Many tInt (TFun Many tInt tInt))
        `shouldBe` "Int -> Int -> Int"

    it "spells linear arrows with the multiplicity marker" $
      renderType (TFun One tInt tInt) `shouldBe` "Int %1 -> Int"

  describe "renderScheme" $ do
    it "omits the quantifier for monomorphic schemes" $
      renderScheme (Scheme [] tInt) `shouldBe` "Int"

    it "prints the quantifier for polymorphic schemes" $
      renderScheme (Scheme ["a"] (TFun Many (TVar "a") (TVar "a")))
        `shouldBe` "forall a. a -> a"

  describe "renderKind" $ do
    it "renders base and higher kinds with the expected associativity" $ do
      renderKind KType `shouldBe` "Type"
      renderKind (KFun KType KType) `shouldBe` "Type -> Type"
      renderKind (KFun (KFun KType KType) KType) `shouldBe` "(Type -> Type) -> Type"
      renderKind (KFun KType (KFun KType KType)) `shouldBe` "Type -> Type -> Type"

  describe "float rendering" $
    it "always emits a decimal point so the value re-parses as a float" $
      mapM_
        (expectExprRoundTrip . EFloat)
        [0.0, 1.0, -1.0, 0.5, -0.25, 1.0e10, 1.0e-10, 123456.789]

-- | Text drawn from an alphabet of exactly the characters that make string
-- rendering hard, so the generator spends its budget on the interesting cases
-- instead of on ordinary letters.
newtype NastyText = NastyText Text
  deriving (Show)

instance Arbitrary NastyText where
  arbitrary = NastyText . Text.pack <$> listOf (elements nastyAlphabet)
  shrink (NastyText raw) = NastyText . Text.pack <$> shrink (Text.unpack raw)

-- | Wrapper so the generator for the two string forms stays local instead of
-- becoming an orphan instance on 'InterpForm'.
newtype NastyForm = NastyForm InterpForm
  deriving (Show)

instance Arbitrary NastyForm where
  arbitrary = NastyForm <$> elements [InterpDouble, InterpIndented]

nastyAlphabet :: [Char]
nastyAlphabet = ['\'', '"', '\\', '$', '{', '}', '\n', '\t', 'a', ' ']

parseExprOrFail :: Text -> IO Expr
parseExprOrFail input =
  case parseProgram "pretty.tynix" input of
    Left err -> expectationFailure (Text.unpack err) >> fail "parse failed"
    Right program ->
      case markedValue <$> programExpr program of
        Just expr -> pure expr
        Nothing -> expectationFailure "expected a root expression" >> fail "no root"

expectStringRoundTrip :: Text -> Expectation
expectStringRoundTrip raw = expectExprRoundTrip (EString (DoubleQuoted raw))

expectIndentedRoundTrip :: Text -> Expectation
expectIndentedRoundTrip raw = expectExprRoundTrip (EString (Indented raw))

-- | Render an expression, parse it back, and require the same tree.
expectExprRoundTrip :: Expr -> Expectation
expectExprRoundTrip expr =
  case parseProgram "pretty.tynix" rendered of
    Left err ->
      expectationFailure
        ("rendered " <> show rendered <> " failed to parse: " <> Text.unpack err)
    Right program ->
      (markedValue <$> programExpr program) `shouldBe` Just expr
  where
    rendered = renderExpr expr

-- | 'expectExprRoundTrip' as a QuickCheck property.
roundTripsAsExpr :: Expr -> Property
roundTripsAsExpr expr =
  let rendered = renderExpr expr
   in case parseProgram "pretty.tynix" rendered of
        Left err ->
          counterexample
            ("rendered " <> show rendered <> " failed to parse: " <> Text.unpack err)
            False
        Right program ->
          counterexample ("rendered as " <> show rendered) $
            (markedValue <$> programExpr program) === Just expr

-- | Formatting must not change what a file means, and formatting an already
-- formatted file must not change it again.
expectFormatterStable :: [Text] -> Expectation
expectFormatterStable sourceLines = do
  let input = Text.unlines sourceLines
  original <- parseOrFail input
  let formatted = renderProgram original
  reparsed <- parseOrFail formatted
  reparsed `shouldBe` original
  renderProgram reparsed `shouldBe` formatted
  where
    parseOrFail text =
      case parseProgram "format.tynix" text of
        Left err -> expectationFailure (Text.unpack err) >> fail "parse failed"
        Right program -> pure program

expectTypeRoundTrip :: Type -> Expectation
expectTypeRoundTrip ty =
  case parseType (renderType ty) of
    Left err ->
      expectationFailure
        ("rendered " <> show (renderType ty) <> " failed to parse: " <> err)
    Right parsed -> parsed `shouldBe` ty

parseType :: Text -> Either String Type
parseType input =
  case runParser (runReaderT (sc *> typeParser <* eof) mempty) "type.tynix" input of
    Left bundle -> Left (errorBundlePretty bundle)
    Right ty -> Right ty
