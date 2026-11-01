{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end specs for the scope- and type-aware LSP features, driven
-- through the same 'Session' entry points the server uses and the real
-- checker ('Driver.analyzeText') on temporary workspaces.
module Main (main) where

import Control.Exception (bracket)
import Control.Monad ((>=>))
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Driver (Analysis, analyzeText, analyzeTextForEditor)
import Session
import SessionScan (scanDocument)
import SessionSemanticTokens (semanticTokensFor)
import SessionTypes (SemanticToken (..))
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    getTemporaryDirectory,
    removeFile,
    removePathForcibly,
  )
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "completion" $ do
    it "lists record fields with their types after a dot"
      $ withDoc
        ( Text.unlines
            [ "let",
              "  pkg :: { name :: String; version :: String; };",
              "  pkg = { name = \"a\"; version = \"1\"; };",
              "in pkg.name"
            ]
        )
      $ \run -> do
        items <- run completionDocument (at 3 7)
        map (\i -> (label i, detail i)) (completionItems items)
          `shouldBe` [("name", Just "String"), ("version", Just "String")]

    it
      "types lambda parameters from the enclosing function signature, even while the buffer is broken"
      $ withDoc
        ( Text.unlines
            ["let", "  f :: { a :: Int; b :: String; } -> Int;", "  f = r: r.", "in f"]
        )
      $ \run -> do
        items <- run completionDocument (at 2 11)
        map label (completionItems items) `shouldBe` ["a", "b"]

    it "types destructured pattern fields from the signature"
      $ withDoc
        ( Text.unlines
            [ "let",
              "  f :: { cfg :: { port :: Int; }; } -> Int;",
              "  f = { cfg }: cfg.",
              "in f"
            ]
        )
      $ \run -> do
        items <- run completionDocument (at 2 19)
        map label (completionItems items) `shouldBe` ["port"]

    it "offers the expected record's missing fields inside an argument attrset"
      $ withDoc
        ( Text.unlines
            [ "let",
              "  mk :: { name :: String; version :: String; } -> String;",
              "  mk = a: a.name;",
              "in mk { name = \"x\"; }"
            ]
        )
      $ \run -> do
        items <- run completionDocument (at 3 19)
        let fields = filter ((== Just 10) . kind) (completionItems items)
        map label fields `shouldBe` ["version"]
        map insertText fields `shouldBe` [Just "version = $1;"]

    it "completes type names, aliases, and type parameters in type positions"
      $ withDoc
        ( Text.unlines
            ["type Box = { v :: Int; };", "let", "  x :: B", "  x = 1;", "in x"]
        )
      $ \run -> do
        items <- run completionDocument (at 2 8)
        map label (completionItems items) `shouldSatisfy` \ls -> "Box" `elem` ls && "Bool" `elem` ls && "let" `notElem` ls

    it "lists names in scope innermost first, plus keywords and snippets"
      $ withDoc
        (Text.unlines ["let", "  outer = 1;", "  f = inner: [ inner  ];", "in f outer"])
      $ \run -> do
        items <- run completionDocument (at 2 21)
        let labels = map label (completionItems items)
        take 3 labels `shouldBe` ["inner", "outer", "f"]
        labels `shouldSatisfy` \ls -> "if" `elem` ls && "let … in" `elem` ls

    it "does not leak lambda parameters outside their body" $
      withDoc (Text.unlines ["let", "  f = inner: inner;", "  g = 1;", "in f g"]) $ \run -> do
        items <- run completionDocument (at 3 5)
        map label (completionItems items) `shouldSatisfy` notElem "inner"

    it "completes directory entries inside path literals"
      $ withTree
        [("main.tynix", "import ./sub/"), ("sub/a.nix", "1"), ("sub/nested/b.nix", "1")]
      $ \root -> do
        let file = root </> "main.tynix"
        items <-
          completionDocument
            readFileStub
            analyzeText
            mempty
            (positionMessage file (0, 13))
        map label (completionItems items) `shouldBe` ["nested/", "a.nix"]

    it "attaches declaration comments as documentation and supports resolve"
      $ withTree
        [ ( "builtins.d.tynix",
            Text.unlines
              [ "declare \"builtins\" {",
                "  # Number of list elements.",
                "  length :: List Int -> Int;",
                "};"
              ]
          ),
          ("main.tynix", "builtins.length")
        ]
      $ \root -> do
        let file = root </> "main.tynix"
        items <-
          completionDocument readFileStub analyzeText mempty (positionMessage file (0, 9))
        case filter ((== "length") . label) (completionItems items) of
          [item] -> do
            documentation item `shouldBe` Just "Number of list elements."
            let stripped = case item of
                  Object o -> Object (KeyMap.delete "documentation" o)
                  other -> other
            resolved <-
              completionResolveDocument
                readFileStub
                analyzeText
                mempty
                (object ["params" .= stripped])
            documentation resolved `shouldBe` Just "Number of list elements."
          other -> expectationFailure ("expected one length item, got " <> show (length other))

  describe "hover" $ do
    it "shows parameter types and documentation comments"
      $ withDoc
        ( Text.unlines
            [ "let",
              "  # Greets someone.",
              "  greet :: String -> String;",
              "  greet = who: who;",
              "in greet \"x\""
            ]
        )
      $ \run -> do
        paramHover <- run hoverDocument (at 3 11)
        hoverValue paramHover `shouldBe` "```tynix\n(parameter) who :: String\n```"
        fnHover <- run hoverDocument (at 4 4)
        hoverValue fnHover
          `shouldBe` "```tynix\ngreet :: String -> String\n```\n\n---\n\nGreets someone."

    it "explains keywords and type aliases"
      $ withDoc
        ( Text.unlines
            ["type Pair = { a :: Int; };", "let x :: Pair; x = { a = 1; }; in x"]
        )
      $ \run -> do
        kw <- run hoverDocument (at 1 1)
        hoverValue kw `shouldSatisfy` ("Local bindings" `Text.isInfixOf`)
        alias <- run hoverDocument (at 1 10)
        hoverValue alias
          `shouldSatisfy` ("type Pair = " `Text.isPrefixOf`) . Text.drop (Text.length "```tynix\n")

  describe "signature help"
    $ it "tracks the active parameter of a curried call"
    $ withDoc
      ( Text.unlines
          ["let", "  add :: Int -> Int -> Int;", "  add = a: b: a;", "in add 1 "]
      )
    $ \run -> do
      help <- run signatureHelpDocument (at 3 9)
      field' "activeParameter" help `shouldBe` Just (Number 1)
      (field' "signatures" help >>= firstOf >>= field' "label")
        `shouldBe` Just (String "add :: Int -> Int -> Int")

  describe "diagnostics" $ do
    it
      "reports unbound names with a span, code link, suggestion, and related declaration (published)"
      $ withDiagnostics (Text.unlines ["let", "  greet = x: x;", "in gret 1"])
      $ \diags -> do
        let errors = filter ((== Just (String "TC0001")) . field' "code") diags
        case errors of
          [d] -> do
            rangeOf d `shouldBe` Just (2, 3, 2, 7)
            (field' "codeDescription" d >>= field' "href")
              `shouldBe` Just (String "https://tynix.dev/reference/diagnostics#tc0001")
            field' "message" d
              `shouldBe` Just (String "unbound name: `gret` Did you mean `greet`?")
            (field' "relatedInformation" d >>= firstOf >>= field' "location" >>= rangeOf)
              `shouldBe` Just (1, 2, 1, 7)
          other -> expectationFailure ("expected one TC0001, got " <> show other)

    it "points missing fields at the selected field"
      $ withDiagnostics
        (Text.unlines ["let", "  box = { alpha = 1; };", "in box.alpah"])
      $ \diags -> do
        let errors = filter ((== Just (String "TC0009")) . field' "code") diags
        map rangeOf errors `shouldBe` [Just (2, 7, 2, 12)]

    it "localises location-less type errors to the offending binding value"
      $ withDiagnostics
        ( Text.unlines
            [ "let",
              "  greet :: String -> String;",
              "  greet = who: who;",
              "in {",
              "  ok = greet \"x\";",
              "  bad = greet 1;",
              "}"
            ]
        )
      $ \diags -> do
        let errors = filter ((== Just (Number 1)) . field' "severity") diags
        -- The checker points call mismatches at the offending argument.
        map rangeOf errors `shouldBe` [Just (5, 14, 5, 15)]

    it "adds unused-binding hints tagged as unnecessary"
      $ withDiagnostics
        ( Text.unlines
            ["let", "  used = 1;", "  spare = 2;", "in { f = { a, b }: a; v = used; }"]
        )
      $ \diags -> do
        let hints = filter ((== Just (String "TL0001")) . field' "code") diags
        map rangeOf hints `shouldBe` [Just (2, 2, 2, 7), Just (3, 14, 3, 15)]
        map (field' "tags") hints `shouldBe` replicate 2 (Just (toJSONList [1]))
        map (field' "severity") hints `shouldBe` replicate 2 (Just (Number 4))

    it "flags uses of declarations documented as deprecated"
      $ withDiagnostics
        (Text.unlines ["let", "  # @deprecated use next instead", "  old = 1;", "in old"])
      $ \diags -> do
        let hints = filter ((== Just (String "TL0002")) . field' "code") diags
        map rangeOf hints `shouldBe` [Just (3, 3, 3, 6)]
        map (field' "tags") hints `shouldBe` [Just (toJSONList [2])]

    it "keeps parse errors on the parser's coordinates without the source excerpt" $
      withDiagnostics (Text.unlines ["let", "  x = ;", "in x"]) $ \diags -> do
        let errors = filter ((== Just (String "TP0004")) . field' "code") diags
        map rangeOf errors `shouldBe` [Just (1, 6, 1, 7)]
        map (fmap (Text.isInfixOf "|") . (field' "message" >=> asText)) errors
          `shouldBe` [Just False]

  describe "code actions" $ do
    it "suggests close field names and adding the missing field"
      $ withDoc
        (Text.unlines ["let", "  box = {", "    alpha = 1;", "  };", "in box.alpah"])
      $ \run -> do
        let diagnostic =
              object
                [ "message" .= ("missing field `alpah` on { alpha :: Int; }" :: Text),
                  "code" .= ("TC0009" :: Text),
                  "range" .= rangeJson 4 7 4 12
                ]
        actions <- run codeActionsDocument (codeActionParams (4, 7) [diagnostic])
        titles actions `shouldSatisfy` \ts ->
          "Did you mean `alpha`?" `elem` ts
            && "Add missing field `alpah` to `box`" `elem` ts

    it "removes unused let bindings and prefixes unused parameters" $
      withDoc (Text.unlines ["let", "  spare = 2;", "  f = x: 1;", "in f"]) $ \run -> do
        let unusedLet =
              object
                [ "message" .= ("`spare` is declared but never used." :: Text),
                  "code" .= ("TL0001" :: Text),
                  "range" .= rangeJson 1 2 1 7
                ]
            unusedParam =
              object
                [ "message" .= ("`x` is a parameter but never used." :: Text),
                  "code" .= ("TL0001" :: Text),
                  "range" .= rangeJson 2 6 2 7
                ]
        actions <-
          run codeActionsDocument (codeActionParams (1, 2) [unusedLet, unusedParam])
        titles actions `shouldSatisfy` \ts -> "Remove unused binding `spare`" `elem` ts && "Prefix `x` with `_`" `elem` ts

    it "inserts an inferred type signature for an unannotated binding" $
      withDoc (Text.unlines ["let", "  count = 1;", "in count"]) $ \run -> do
        actions <- run codeActionsDocument (codeActionParams (1, 3) [])
        titles actions `shouldBe` ["Add type signature `count :: 1`"]

  describe "navigation" $ do
    it "jumps from a use to the binding lambda parameter" $
      withDoc (Text.unlines ["let x = 1; f = x: x; in f x"]) $ \run -> do
        loc <- run definitionDocument (at 0 18)
        rangeOf loc `shouldBe` Just (0, 15, 0, 16)

    it "renames only the shadowing binder's occurrences" $
      withDoc (Text.unlines ["let x = 1; f = x: x; in f x"]) $ \run -> do
        edit <- run renameDocument (renameAt 0 15 "y")
        editRanges edit `shouldBe` [(0, 15, 0, 16), (0, 18, 0, 19)]

    it "refuses to prepare a rename on a keyword" $
      withDoc "let x = 1; in x" $ \run -> do
        result <- run prepareRenameDocument (at 0 1)
        result `shouldBe` Null

    it "returns nested selection ranges" $
      withDoc "let x = { a = [ 1 2 ]; }; in x" $ \run -> do
        result <-
          run (\r _ d m -> selectionRangeDocument r d m) (positionsParams [(0, 16)])
        selectionDepth result `shouldSatisfy` (>= 4)

    it "builds a hierarchical outline"
      $ withDoc
        (Text.unlines ["let", "  pkg = { meta = { license = \"MIT\"; }; };", "in pkg"])
      $ \run -> do
        result <- run documentSymbolsHierarchicalDocument (at 0 0)
        outline result `shouldBe` [("pkg", [("meta", [("license", [])])])]

  describe "semantic tokens" $
    it "classifies parameters, type parameters, and declarations" $ do
      let content = "let id :: forall a. a -> a; id = x: x; in id 1"
          tokens = semanticTokensFor content (Left "no analysis")
          at' col =
            [ (semanticTokenType t, semanticTokenModifiers t)
            | t <- tokens,
              semanticTokenStart t == col
            ]
      at' 17 `shouldBe` [(9, 1)] -- `a` declared by `forall a.`
      at' 20 `shouldBe` [(9, 0)] -- `a` used in the quantified type
      at' 33 `shouldBe` [(8, 1)] -- `x` parameter declaration
      at' 36 `shouldBe` [(8, 0)] -- `x` parameter use
      at' 28 `shouldBe` [(2, 1)] -- `id = x: …` function declaration
  describe "document store" $
    it "updates text without analysing and only stores results for current text" $ do
      let docs = documentsFromList [("/tmp/main.tynix", "1")]
          change =
            object
              [ "params"
                  .= object
                    [ "textDocument" .= object ["uri" .= ("file:///tmp/main.tynix" :: Text)],
                      "contentChanges" .= [object ["text" .= ("2" :: Text)]]
                    ]
              ]
      case updateDocumentText docs change of
        Right (docs', file) -> do
          lookupDocumentText file docs' `shouldBe` Just "2"
          storeDocumentAnalysis file "stale" (Left "x") docs' `shouldBe` docs'
        Left err -> expectationFailure err

  describe "scanner robustness" $
    it "scans a large generated document quickly" $ do
      let content =
            Text.unlines
              ( ["let"]
                  <> [ Text.pack ("  v" <> show i <> " = " <> show i <> ";") | i <- [1 :: Int .. 3000]
                     ]
                  <> ["in v1"]
              )
          scan = scanDocument content
      length (show (length (semanticTokensFor content (Left "none"))))
        `shouldSatisfy` (> 0)
      scan `seq` pure ()

-- Helpers -----------------------------------------------------------------------------------

type Reader = FilePath -> IO (Either String Text)

type Analyzer = FilePath -> Text -> IO (Either String Analysis)

type Handler = Reader -> Analyzer -> Documents -> Value -> IO Value

type Run = Handler -> RequestShape -> IO Value

at :: Int -> Int -> RequestShape
at = AtPos

-- | Run handlers against one document in a fresh workspace.
withDoc :: Text -> (Run -> IO ()) -> IO ()
withDoc content body =
  withTree [("main.tynix", content)] $ \root -> do
    let file = root </> "main.tynix"
        docs = documentsFromList [(file, content)]
        run handler shape = handler readFileStub analyzeText docs (shapeMessage file shape)
    body run

withDiagnostics :: Text -> ([Value] -> IO ()) -> IO ()
withDiagnostics content body =
  withTree [("main.tynix", content)] $ \root -> do
    let file = root </> "main.tynix"
        docs = documentsFromList [(file, content)]
    result <- analyzeTextForEditor file content
    diags <-
      documentDiagnostics readFileStub analyzeTextForEditor docs file content result
    body diags

data RequestShape
  = AtPos Int Int
  | RenameAt Int Int Text
  | CodeAction (Int, Int) [Value]
  | Positions [(Int, Int)]

shapeMessage :: FilePath -> RequestShape -> Value
shapeMessage file shape = case shape of
  AtPos l c -> positionMessage file (l, c)
  RenameAt l c newName ->
    object
      [ "id" .= (1 :: Int),
        "params"
          .= object
            ["textDocument" .= uriObj file, "position" .= posObj l c, "newName" .= newName]
      ]
  CodeAction (l, c) diags ->
    object
      [ "id" .= (1 :: Int),
        "params"
          .= object
            [ "textDocument" .= uriObj file,
              "range" .= rangeJson l c l c,
              "context" .= object ["diagnostics" .= diags]
            ]
      ]
  Positions ps ->
    object
      [ "id" .= (1 :: Int),
        "params"
          .= object
            ["textDocument" .= uriObj file, "positions" .= [posObj l c | (l, c) <- ps]]
      ]

renameAt :: Int -> Int -> Text -> RequestShape
renameAt = RenameAt

codeActionParams :: (Int, Int) -> [Value] -> RequestShape
codeActionParams = CodeAction

positionsParams :: [(Int, Int)] -> RequestShape
positionsParams = Positions

positionMessage :: FilePath -> (Int, Int) -> Value
positionMessage file (l, c) =
  object
    [ "id" .= (1 :: Int),
      "params" .= object ["textDocument" .= uriObj file, "position" .= posObj l c]
    ]

uriObj :: FilePath -> Value
uriObj file = object ["uri" .= ("file://" <> Text.pack file)]

posObj :: Int -> Int -> Value
posObj l c = object ["line" .= l, "character" .= c]

rangeJson :: Int -> Int -> Int -> Int -> Value
rangeJson sl sc el ec = object ["start" .= posObj sl sc, "end" .= posObj el ec]

readFileStub :: FilePath -> IO (Either String Text)
readFileStub = fmap Right . TextIO.readFile

withTree :: [(FilePath, Text)] -> (FilePath -> IO a) -> IO a
withTree files action =
  bracket createRoot removePathForcibly (\root -> writeTree root >> action root)
  where
    createRoot = do
      tmp <- getTemporaryDirectory
      (path, handle) <- openTempFile tmp "tynix-lsp-features"
      hClose handle
      removeFile path
      createDirectory path
      TextIO.writeFile (path </> "flake.nix") "{}\n"
      pure path
    writeTree root =
      mapM_
        ( \(relative, content) -> do
            let path = root </> relative
            createDirectoryIfMissing True (takeDirectory path)
            TextIO.writeFile path content
        )
        files

field' :: Text -> Value -> Maybe Value
field' key (Object o) = KeyMap.lookup (Key.fromText key) o
field' _ _ = Nothing

asText :: Value -> Maybe Text
asText (String t) = Just t
asText _ = Nothing

firstOf :: Value -> Maybe Value
firstOf (Array xs) = case toList xs of
  x : _ -> Just x
  [] -> Nothing
firstOf _ = Nothing

toJSONList :: [Int] -> Value
toJSONList = Array . foldr (\x acc -> pure (Number (fromIntegral x)) <> acc) mempty

completionItems :: Value -> [Value]
completionItems v = case field' "items" v of
  Just (Array xs) -> toList xs
  _ -> []

label :: Value -> Text
label v = fromMaybe "" (field' "label" v >>= asText)

detail :: Value -> Maybe Text
detail v = field' "detail" v >>= asText

kind :: Value -> Maybe Int
kind v = case field' "kind" v of
  Just (Number n) -> Just (round n)
  _ -> Nothing

insertText :: Value -> Maybe Text
insertText v = field' "textEdit" v >>= field' "newText" >>= asText

documentation :: Value -> Maybe Text
documentation v = field' "documentation" v >>= field' "value" >>= asText

hoverValue :: Value -> Text
hoverValue v = fromMaybe "" (field' "contents" v >>= field' "value" >>= asText)

rangeOf :: Value -> Maybe (Int, Int, Int, Int)
rangeOf v = do
  r <- field' "range" v
  s <- field' "start" r
  e <- field' "end" r
  let n k o = case field' k o of
        Just (Number x) -> Just (round x)
        _ -> Nothing
  (,,,) <$> n "line" s <*> n "character" s <*> n "line" e <*> n "character" e

titles :: Value -> [Text]
titles (Array xs) = mapMaybe (field' "title" >=> asText) (toList xs)
titles _ = []

editRanges :: Value -> [(Int, Int, Int, Int)]
editRanges v = case field' "changes" v of
  Just (Object o) -> concat [mapMaybe rangeOf (toList edits) | Array edits <- KeyMap.elems o]
  _ -> []

selectionDepth :: Value -> Int
selectionDepth v = maybe 0 go (firstOf v)
  where
    go node = 1 + maybe 0 go (field' "parent" node)

outline :: Value -> [(Text, [(Text, [(Text, [()])])])]
outline (Array xs) =
  [ (nameOf n, [(nameOf c, [(nameOf g, []) | g <- children c]) | c <- children n])
  | n <- toList xs
  ]
  where
    nameOf n = fromMaybe "" (field' "name" n >>= asText)
    children n = case field' "children" n of
      Just (Array cs) -> toList cs
      _ -> []
outline _ = []
