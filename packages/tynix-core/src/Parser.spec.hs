{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Either (isRight)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Parser (ParseError (..), parseProgram, parseProgramDetailed)
import Pretty (renderExpr)
import Syntax
import Test.Hspec
import Test.QuickCheck
import TestSupport (expectRight, source)
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "parseProgram" $ do
  it "parses aliases, ambient declarations, and a root expression" $ do
    program <-
      expectRight $
        parseProgram "main.tynix" $
          source
            [ "type Box t = { value :: t; };",
              "declare \"./lib.nix\" { default :: Box Int; };",
              "let box = import ./lib.nix; in box.value"
            ]
    programAliases program
      `shouldBe` [plainAlias "Box" ["t"] (TRecord (Map.fromList [("value", TVar "t")]))]
    programAmbient program
      `shouldBe` [AmbientDecl "./lib.nix" [AmbientEntry "default" (TApp (TCon "Box") tInt)]]
    programExpr program
      `shouldBe` Just (plain (ELet [plain (LetBinding "box" (EApp (EVar "import") (EPath "./lib.nix")))] (ESelect (EVar "box") [SelectName "value"])))

  it "parses typed lambdas and nested selections without changing nix shape" $ do
    program <- expectRight $ parseProgram "main.tynix" "{ nested = { value = 1; }; }.nested.value"
    programExpr program
      `shouldBe` Just
        ( plain
            ( ESelect
                (EAttrSet [AttrField "nested" (EAttrSet [AttrField "value" (EInt 1)])])
                [SelectName "nested", SelectName "value"]
            )
        )

  it "parses infix addition inside lambdas and attrsets" $ do
    program <- expectRight $ parseProgram "main.nix" "{ inc = x: x + 1; }"
    programExpr program
      `shouldBe` Just
        ( plain
            ( EAttrSet
                [ AttrField
                    "inc"
                    (ELambda (PVar "x" Nothing) (EBinaryOp OpAdd (EVar "x") (EInt 1)))
                ]
            )
        )

  it "parses conditional types with infer binders" $ do
    program <- expectRight $ parseProgram "main.tynix" "type Elem t = t extends List (infer u) ? u : dynamic;"
    programAliases program
      `shouldBe` [plainAlias "Elem" ["t"] (TConditional (TVar "t") (TApp (TCon "List") (TInfer "u")) (TVar "u") TDynamic)]

  it "parses indexed container types and tensor shapes" $ do
    program <- expectRight $ parseProgram "main.tynix" "type Grid t = Matrix 2 3 t; type Cube t = Tensor [2 3 4] t;"
    programAliases program
      `shouldBe` [ plainAlias "Grid" ["t"] (TApp (TApp (TApp (TCon "Matrix") (TLit (LInt 2))) (TLit (LInt 3))) (TVar "t")),
                   plainAlias
                     "Cube"
                     ["t"]
                     (TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt 2), TLit (LInt 3), TLit (LInt 4)])) (TVar "t"))
                 ]

  it "parses float literals together with range and unit types" $ do
    program <- expectRight $ parseProgram "main.tynix" "type Timeout = Unit \"ms\" (Range 0 5000 Nat); 1.5"
    programAliases program
      `shouldBe` [ plainAlias
                     "Timeout"
                     []
                     (TApp (TApp (TCon "Unit") (TLit (LString "ms"))) (TApp (TApp (TApp (TCon "Range") (TLit (LInt 0))) (TLit (LInt 5000))) tNat))
                 ]
    programExpr program `shouldBe` Just (plain (EFloat 1.5))

  it "parses negative numeric literals in expressions and type shapes" $ do
    program <-
      expectRight $
        parseProgram
          "main.tynix"
          ( source
              [ "type Signed = Range -1 1 Int;",
                "type Shape = Tensor [-1 -2.5] Float;",
                "-1.5"
              ]
          )
    programAliases program
      `shouldBe` [ plainAlias "Signed" [] (TApp (TApp (TApp (TCon "Range") (TLit (LInt (-1)))) (TLit (LInt 1))) tInt),
                   plainAlias "Shape" [] (TApp (TApp (TCon "Tensor") (TTypeList [TLit (LInt (-1)), TLit (LFloat (-2.5))])) tFloat)
                 ]
    programExpr program `shouldBe` Just (plain (EFloat (-1.5)))

  it "parses relational and equality operators producing comparisons" $ do
    program <- expectRight $ parseProgram "main.tynix" "1 < 2"
    programExpr program `shouldBe` Just (plain (EBinaryOp OpLt (EInt 1) (EInt 2)))
    leq <- expectRight $ parseProgram "main.tynix" "1 <= 2"
    programExpr leq `shouldBe` Just (plain (EBinaryOp OpLe (EInt 1) (EInt 2)))
    eq <- expectRight $ parseProgram "main.tynix" "1 == 2"
    programExpr eq `shouldBe` Just (plain (EBinaryOp OpEq (EInt 1) (EInt 2)))
    neq <- expectRight $ parseProgram "main.tynix" "1 != 2"
    programExpr neq `shouldBe` Just (plain (EBinaryOp OpNeq (EInt 1) (EInt 2)))

  it "parses additive and multiplicative arithmetic with correct precedence" $ do
    -- a + b * c  ==>  a + (b * c)
    program <- expectRight $ parseProgram "main.tynix" "a + b * c"
    programExpr program
      `shouldBe` Just (plain (EBinaryOp OpAdd (EVar "a") (EBinaryOp OpMul (EVar "b") (EVar "c"))))
    -- a - b - c  ==>  (a - b) - c (left-associative)
    sub <- expectRight $ parseProgram "main.tynix" "a - b - c"
    programExpr sub
      `shouldBe` Just (plain (EBinaryOp OpSub (EBinaryOp OpSub (EVar "a") (EVar "b")) (EVar "c")))

  it "parses right-associative list concatenation" $ do
    program <- expectRight $ parseProgram "main.tynix" "xs ++ ys ++ zs"
    programExpr program
      `shouldBe` Just (plain (EBinaryOp OpConcat (EVar "xs") (EBinaryOp OpConcat (EVar "ys") (EVar "zs"))))

  it "parses recursive attribute sets and reserves rec" $ do
    program <- expectRight $ parseProgram "main.tynix" "rec { a = 1; b = a; }"
    programExpr program
      `shouldBe` Just (plain (ERec [AttrField "a" (EInt 1), AttrField "b" (EVar "a")]))
    parseProgram "main.tynix" "let rec = 1; in rec" `shouldSatisfy` isLeft

  it "parses with expressions and reserves the keyword" $ do
    program <- expectRight $ parseProgram "main.tynix" "with scope; body"
    programExpr program `shouldBe` Just (plain (EWith (EVar "scope") (EVar "body")))
    parseProgram "main.tynix" "let with = 1; in with" `shouldSatisfy` isLeft

  it "parses assert expressions and reserves the keyword" $ do
    program <- expectRight $ parseProgram "main.tynix" "assert x; y"
    programExpr program `shouldBe` Just (plain (EAssert (EVar "x") (EVar "y")))
    parseProgram "main.tynix" "let assert = 1; in assert" `shouldSatisfy` isLeft

  it "parses the attribute-presence test with a dotted attribute path" $ do
    program <- expectRight $ parseProgram "main.tynix" "a ? b"
    programExpr program `shouldBe` Just (plain (EHasAttr (EVar "a") [SelectName "b"]))
    nested <- expectRight $ parseProgram "main.tynix" "a.b ? c.d"
    programExpr nested
      `shouldBe` Just (plain (EHasAttr (ESelect (EVar "a") [SelectName "b"]) [SelectName "c", SelectName "d"]))

  it "parses right-associative attribute-set update binding tighter than comparison" $ do
    program <- expectRight $ parseProgram "main.tynix" "a // b // c"
    programExpr program
      `shouldBe` Just (plain (EBinaryOp OpUpdate (EVar "a") (EBinaryOp OpUpdate (EVar "b") (EVar "c"))))
    -- a // b == c  ==>  (a // b) == c
    cmp <- expectRight $ parseProgram "main.tynix" "a // b == c"
    programExpr cmp
      `shouldBe` Just (plain (EBinaryOp OpEq (EBinaryOp OpUpdate (EVar "a") (EVar "b")) (EVar "c")))

  it "parses boolean connectives and prefix negation" $ do
    program <- expectRight $ parseProgram "main.tynix" "true && false"
    programExpr program `shouldBe` Just (plain (EBinaryOp OpAnd (EBool True) (EBool False)))
    notProgram <- expectRight $ parseProgram "main.tynix" "!true"
    programExpr notProgram `shouldBe` Just (plain (EUnaryOp OpNot (EBool True)))

  it "honors operator precedence: || looser than && looser than == looser than < looser than + looser than !" $ do
    -- a + 1 < limit && ok || done  ==>  (((a + 1) < limit) && ok) || done
    program <- expectRight $ parseProgram "main.tynix" "a + 1 < limit && ok || done"
    programExpr program
      `shouldBe` Just
        ( plain
            ( EBinaryOp
                OpOr
                ( EBinaryOp
                    OpAnd
                    (EBinaryOp OpLt (EBinaryOp OpAdd (EVar "a") (EInt 1)) (EVar "limit"))
                    (EVar "ok")
                )
                (EVar "done")
            )
        )
    -- !a == b  ==>  (!a) == b ; ! a + b ==> !(a + b)
    notEq <- expectRight $ parseProgram "main.tynix" "!a == b"
    programExpr notEq `shouldBe` Just (plain (EBinaryOp OpEq (EUnaryOp OpNot (EVar "a")) (EVar "b")))
    notAdd <- expectRight $ parseProgram "main.tynix" "!a + b"
    programExpr notAdd `shouldBe` Just (plain (EUnaryOp OpNot (EBinaryOp OpAdd (EVar "a") (EVar "b"))))

  it "parses string interpolation in double-quoted and indented strings" $ do
    dq <- expectRight $ parseProgram "main.tynix" "\"a${x}b\""
    programExpr dq
      `shouldBe` Just (plain (EInterp InterpDouble [StrText "a", StrExpr (EVar "x"), StrText "b"]))
    leading <- expectRight $ parseProgram "main.tynix" "\"${x}\""
    programExpr leading `shouldBe` Just (plain (EInterp InterpDouble [StrExpr (EVar "x")]))
    indented <- expectRight $ parseProgram "main.tynix" "''a${x}b''"
    programExpr indented
      `shouldBe` Just (plain (EInterp InterpIndented [StrText "a", StrExpr (EVar "x"), StrText "b"]))

  it "keeps non-interpolated strings as plain string literals" $ do
    program <- expectRight $ parseProgram "main.tynix" "\"abc\""
    programExpr program `shouldBe` Just (plain (EString (DoubleQuoted "abc")))

  it "parses interpolation containing a full expression" $ do
    program <- expectRight $ parseProgram "main.tynix" "\"sum ${a + b}\""
    programExpr program
      `shouldBe` Just (plain (EInterp InterpDouble [StrText "sum ", StrExpr (EBinaryOp OpAdd (EVar "a") (EVar "b"))]))

  it "parses any and unknown as distinct built-in gradual types" $ do
    program <- expectRight $ parseProgram "main.tynix" "type Loose = any; type Opaque = unknown;"
    programAliases program
      `shouldBe` [plainAlias "Loose" [] tAny, plainAlias "Opaque" [] tUnknown]

  it "parses parenthesized refinements and unions inside tensor shapes" $ do
    program <- expectRight $ parseProgram "main.tynix" "type Batch t = Tensor [(Range 0 2 Nat) (1 | 2) 4] t;"
    programAliases program
      `shouldBe` [ plainAlias
                     "Batch"
                     ["t"]
                     ( TApp
                         ( TApp
                             (TCon "Tensor")
                             ( TTypeList
                                 [ TApp (TApp (TApp (TCon "Range") (TLit (LInt 0))) (TLit (LInt 2))) tNat,
                                   TUnion [TLit (LInt 1), TLit (LInt 2)],
                                   TLit (LInt 4)
                                 ]
                             )
                         )
                         (TVar "t")
                     )
                 ]

  it "parses tuple types as type-only heterogeneous sequences" $ do
    program <- expectRight $ parseProgram "main.tynix" "type Pair = Tuple [Int String];"
    programAliases program
      `shouldBe` [plainAlias "Pair" [] (TApp (TCon "Tuple") (TTypeList [tInt, tString]))]

  it "reserves Tuple at the type level while letting terms rebind import, as Nix does" $ do
    program <- expectRight $ parseProgram "main.tynix" "type Pair = Tuple [Int String]; import \"./lib.nix\""
    programAliases program
      `shouldBe` [plainAlias "Pair" [] (TApp (TCon "Tuple") (TTypeList [tInt, tString]))]
    programExpr program
      `shouldBe` Just (plain (EApp (EVar "import") (EString (DoubleQuoted "./lib.nix"))))
    parseProgram "main.tynix" "let import = 1; in import" `shouldSatisfy` isRight
    parseProgram "main.tynix" "type Tuple = Int;" `shouldSatisfy` isLeft

  it "parses linear function arrows alongside ordinary arrows" $ do
    program <- expectRight $ parseProgram "main.tynix" "type Consume a = a %1 -> a; type Endo a = a -> a;"
    programAliases program
      `shouldBe` [ plainAlias "Consume" ["a"] (TFun One (TVar "a") (TVar "a")),
                   plainAlias "Endo" ["a"] (TFun Many (TVar "a") (TVar "a"))
                 ]

  it "parses typed lambda binders" $ do
    program <- expectRight $ parseProgram "main.tynix" "(x :: Int): x"
    programExpr program `shouldBe` Just (plain (ELambda (PVar "x" (Just tInt)) (EVar "x")))

  it "parses attrset lambda binders used by flakes" $ do
    program <- expectRight $ parseProgram "main.tynix" "{ self, nixpkgs, ... }: self"
    programExpr program `shouldBe` Just (plain (ELambda (PAttrSet [PatternField "self" Nothing Nothing, PatternField "nixpkgs" Nothing Nothing] True Nothing) (EVar "self")))

  it "parses comments, inherit clauses, and list conditionals" $ do
    program <-
      expectRight $
        parseProgram
          "main.tynix"
          ( source
              [ "# line comment",
                "/* block comment */",
                "let",
                "  value = 1;",
                "in { inherit value; nested = [ (if true then 1 else 2) ../lib.nix ]; }"
              ]
          )
    programExpr program
      `shouldBe` Just
        ( plain
            ( ELet
                [plain (LetBinding "value" (EInt 1))]
                ( EAttrSet
                    [ AttrInherit ["value"],
                      AttrField
                        "nested"
                        (EList [EIf (EBool True) (EInt 1) (EInt 2), EPath "../lib.nix"])
                    ]
                )
            )
        )

  it "treats uppercase type heads as constructors in generic applications" $ do
    program <- expectRight $ parseProgram "main.tynix" "type Use t = Result t;"
    programAliases program `shouldBe` [plainAlias "Use" ["t"] (TApp (TCon "Result") (TVar "t"))]

  it "parses absolute path imports and nested field selections in applications" $ do
    program <- expectRight $ parseProgram "main.tynix" "(import /etc/hosts).meta.value"
    programExpr program
      `shouldBe` Just (plain (ESelect (EApp (EVar "import") (EPath "/etc/hosts")) [SelectName "meta", SelectName "value"]))

  it "allows reserved keywords in field and selector positions" $ do
    program <- expectRight $ parseProgram "main.tynix" "{ any = 1; }.any"
    programExpr program
      `shouldBe` Just (plain (ESelect (EAttrSet [AttrField "any" (EInt 1)]) [SelectName "any"]))

  it "parses quoted attribute names and dynamic selections" $ do
    quotedProgram <- expectRight $ parseProgram "main.tynix" "{ \"aarch64-darwin\" = 1; }.\"aarch64-darwin\""
    dynamicProgram <- expectRight $ parseProgram "main.tynix" "self.packages.${system}.default"
    quotedProgram
      `shouldSatisfy` ( \program ->
                          programExpr program
                            == Just
                              ( plain
                                  ( ESelect
                                      (EAttrSet [AttrField "aarch64-darwin" (EInt 1)])
                                      [SelectName "aarch64-darwin"]
                                  )
                              )
                      )
    dynamicProgram
      `shouldSatisfy` ( \program ->
                          programExpr program
                            == Just
                              ( plain
                                  ( ESelect
                                      (EVar "self")
                                      [ SelectName "packages",
                                        SelectDynamic (EVar "system"),
                                        SelectName "default"
                                      ]
                                  )
                              )
                      )

  it "parses indented strings as executable literals" $ do
    program <-
      expectRight $
        parseProgram
          "main.tynix"
          (source ["''", "hello", "world", "''"])
    programExpr program `shouldBe` Just (plain (EString (Indented "\nhello\nworld\n")))

  it "reads the Nix escapes an indented string can contain" $ do
    expectStringBody "'' ''$ ''" " $ "
    expectStringBody "'' ''${ ''" " ${ "
    expectStringBody "'' ''' ''" " '' "
    expectStringBody "'' ''\\' ''" " ' "
    expectStringBody "'' ''\\x ''" " x "

  it "keeps whitespace escapes verbatim so indentation stripping is preserved" $ do
    -- Nix computes the indentation to strip from literal lines only, so an
    -- escaped newline must not be turned into a real one on output.
    program <- expectRight (parseProgram "main.tynix" "'' a''\\nb ''\\t ''")
    (markedValue <$> programExpr program)
      `shouldBe` Just (EInterp InterpIndented [StrText " a", StrEscape 'n', StrText "b ", StrEscape 't', StrText " "])

  it "keeps an escaped antiquotation out of the interpolation parts" $ do
    program <- expectRight (parseProgram "main.tynix" "'' ''${value} ''")
    (markedValue <$> programExpr program)
      `shouldBe` Just (EString (Indented " ${value} "))

  it "still splits a real antiquotation in an indented string" $ do
    program <- expectRight (parseProgram "main.tynix" "'' ${value} ''")
    (markedValue <$> programExpr program)
      `shouldBe` Just (EInterp InterpIndented [StrText " ", StrExpr (EVar "value"), StrText " "])

  it "reads the escapes a double-quoted string can contain" $ do
    expectStringBody "\"a\\\"b\"" "a\"b"
    expectStringBody "\"a\\\\b\"" "a\\b"
    expectStringBody "\"a\\nb\"" "a\nb"
    expectStringBody "\"a\\$b\"" "a$b"
    expectStringBody "\"a\\${b}\"" "a${b}"

  it "parses as-casts after selections and inside list items" $ do
    castProgram <- expectRight $ parseProgram "main.tynix" "{ as = 1; }.as as Int as Number"
    listProgram <- expectRight $ parseProgram "main.tynix" "[value as Int]"
    programExpr castProgram
      `shouldBe` Just
        ( plain
            ( ECast
                (ECast (ESelect (EAttrSet [AttrField "as" (EInt 1)]) [SelectName "as"]) tInt)
                tNumber
            )
        )
    programExpr listProgram `shouldBe` Just (plain (EList [ECast (EVar "value") tInt]))

  it "attaches tynix diagnostic directives to the next root expression or let item" $ do
    program <-
      expectRight $
        parseProgram
          "main.tynix"
          ( source
              [ "# @tynix-expected",
                "let",
                "  # @tynix-ignore",
                "  value = missing;",
                "in value"
              ]
          )
    programExpr program
      `shouldBe` Just
        ( Marked
            (Just TynixExpected)
            (ELet [Marked (Just TynixIgnore) (LetBinding "value" (EVar "missing"))] (EVar "value"))
        )

  it "rejects dangling tynix directives without a following line of code" $
    parseProgram "main.tynix" "# @tynix-ignore" `shouldSatisfy` isLeft

  it "rejects reserved words as identifiers" $
    parseProgram "main.tynix" "let if = 1; in if" `shouldSatisfy` isLeft

  it "surfaces 1-based line/column on parser failure via parseProgramDetailed" $ do
    case parseProgramDetailed "main.tynix" "let x = ;" of
      Left err -> do
        parseErrorLine err `shouldBe` 1
        parseErrorColumn err `shouldSatisfy` (> 0)
      Right _ -> expectationFailure "expected a parse failure for `let x = ;`"

  it "points the directive-balance error at the file boundary" $ do
    case parseProgramDetailed "main.tynix" "# @tynix-ignore" of
      Left err -> do
        parseErrorLine err `shouldBe` 1
        parseErrorMessage err
          `shouldSatisfy` (\msg -> "dangling tynix diagnostic directive" `Text.isInfixOf` msg)
      Right _ -> expectationFailure "expected a dangling-directive failure"

  it "back-compat parseProgram preserves the line:column prefix expected by the LSP" $
    case parseProgram "main.tynix" "let x = ;" of
      Left message ->
        let header = Text.takeWhile (/= ':') message
         in case Text.unpack header of
              digits | all (`elem` ("0123456789" :: String)) digits, not (null digits) -> pure ()
              _ -> expectationFailure ("expected a numeric line prefix, got " <> Text.unpack header)
      Right _ -> expectationFailure "expected a parse failure"

  it "round-trips generated executable expressions through the pretty-printer" $
    property $ \(RoundTripExpr expr) ->
      case parseProgram "roundtrip.tynix" (renderExpr expr) of
        Right program -> programExpr program === Just (plain expr)
        Left err -> counterexample (Text.unpack err) False
  where
    plain = Marked Nothing
    isLeft (Left _) = True
    isLeft _ = False

-- | Parse a source that is exactly one string literal and check its content.
expectStringBody :: Text.Text -> Text.Text -> Expectation
expectStringBody input expected = do
  program <- expectRight (parseProgram "main.tynix" input)
  case markedValue <$> programExpr program of
    Just (EString literal) -> stringLiteralText literal `shouldBe` expected
    other -> expectationFailure ("expected a plain string literal, got " <> show other)

newtype RoundTripExpr = RoundTripExpr Expr
  deriving (Show)

instance Arbitrary RoundTripExpr where
  arbitrary = sized (fmap RoundTripExpr . genExpr)
  shrink (RoundTripExpr expr) = RoundTripExpr <$> shrinkExpr expr

-- | Depth-limited generator that nests operators, applications, and
-- control-flow inside one another so the round-trip property exercises
-- precedence-sensitive parenthesization (e.g. control flow as an operator
-- operand, or an application inside a list element).
genExpr :: Int -> Gen Expr
genExpr depth
  | depth <= 0 = genLeafExpr
  | otherwise =
      oneof
        [ genLeafExpr,
          EBinaryOp <$> genBinOp <*> sub <*> sub,
          EUnaryOp OpNot <$> sub,
          EApp <$> sub <*> sub,
          EIf <$> sub <*> sub <*> sub,
          EWith <$> sub <*> sub,
          EList <$> resize 3 (listOf sub),
          EAttrSet <$> resize 3 (listOf genAttr),
          EInterp <$> elements [InterpDouble, InterpIndented] <*> genInterpParts sub
        ]
  where
    sub = genExpr (depth `div` 2)
    genAttr = AttrField <$> genName <*> sub

-- | Generate a canonical interpolated string: non-empty literal text on either
-- side of a single antiquotation. This guarantees the segments round-trip (no
-- adjacent or empty text runs, and at least one expression so the form is not
-- collapsed back to a plain string).
genInterpParts :: Gen Expr -> Gen [StringPart]
genInterpParts sub =
  (\a expr b -> [StrText a, StrExpr expr, StrText b]) <$> genName <*> sub <*> genName

genBinOp :: Gen BinOp
genBinOp = elements [OpAdd, OpSub, OpMul, OpConcat, OpUpdate, OpEq, OpNeq, OpLt, OpGt, OpLe, OpGe, OpAnd, OpOr]

genLeafExpr :: Gen Expr
genLeafExpr =
  oneof
    [ EInt <$> chooseInteger (0, 1000),
      EBool <$> arbitrary,
      pure ENull,
      EString . DoubleQuoted <$> genStringBody,
      EString . Indented <$> genStringBody
    ]

-- | String bodies drawn from an alphabet weighted towards the characters that
-- have to survive escaping: quotes, backslashes, and antiquotation markers.
genStringBody :: Gen Text.Text
genStringBody = Text.pack <$> listOf (elements (['a' .. 'e'] <> ['\'', '"', '\\', '$', '{', '}', ' ']))

genName :: Gen Text.Text
genName = Text.pack <$> ((:) <$> elements ['a' .. 'z'] <*> listOf (elements (['a' .. 'z'] <> ['0' .. '9'])))

shrinkExpr :: Expr -> [Expr]
shrinkExpr = \case
  EBinaryOp _ left right -> [left, right]
  EUnaryOp _ operand -> [operand]
  EApp f x -> [f, x]
  EWith scope body -> [scope, body]
  EIf cond yes no -> [cond, yes, no]
  EList items -> items
  EAttrSet items -> [expr | AttrField _ expr <- items]
  EInterp _ parts -> [expr | StrExpr expr <- parts]
  _ -> []
