{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Driver (Analysis (..))
import Server
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO
  ( Handle,
    SeekMode (AbsoluteSeek),
    hClose,
    hFlush,
    hSeek,
    hSetBinaryMode,
    hWaitForInput,
    openTempFile,
  )
import System.Posix.IO (createPipe, fdToHandle)
import Test.Hspec
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "clientCapabilities" $ do
    it "advertises editor features needed for an interactive tynix workflow" $ do
      let caps = field "capabilities" clientCapabilities
          provider name = caps >>= field name
      mapM_
        (\name -> provider name `shouldSatisfy` (/= Nothing))
        [ "hoverProvider",
          "completionProvider",
          "signatureHelpProvider",
          "definitionProvider",
          "declarationProvider",
          "referencesProvider",
          "renameProvider",
          "documentSymbolProvider",
          "workspaceSymbolProvider",
          "codeActionProvider",
          "documentHighlightProvider",
          "documentFormattingProvider",
          "documentLinkProvider",
          "foldingRangeProvider",
          "selectionRangeProvider",
          "inlayHintProvider",
          "semanticTokensProvider",
          "textDocumentSync"
        ]
      (provider "completionProvider" >>= field "resolveProvider")
        `shouldBe` Just (Bool True)
      (provider "completionProvider" >>= field "triggerCharacters")
        `shouldBe` Just (toJSON ["." :: String, "/"])
      (provider "renameProvider" >>= field "prepareProvider")
        `shouldBe` Just (Bool True)
      (provider "semanticTokensProvider" >>= field "range")
        `shouldBe` Just (Bool True)
      provider "diagnosticProvider" `shouldBe` Nothing

    it "keeps the original semantic token legend indices and appends new types" $ do
      let legend =
            field "capabilities" clientCapabilities
              >>= field "semanticTokensProvider"
              >>= field "legend"
      (legend >>= field "tokenTypes")
        `shouldBe` Just
          ( toJSON
              ( [ "keyword",
                  "type",
                  "function",
                  "variable",
                  "property",
                  "string",
                  "number",
                  "operator",
                  "parameter",
                  "typeParameter",
                  "comment",
                  "namespace",
                  "decorator"
                ] ::
                  [String]
              )
          )

    it "advertises pull diagnostics only when asked to" $
      (field "capabilities" (serverCapabilities True) >>= field "diagnosticProvider")
        `shouldBe` Just (object ["interFileDependencies" .= True, "workspaceDiagnostics" .= False])

  describe "respondError" $ do
    it "answers a request id with a JSON-RPC error object" $
      withTempHandle $ \handle -> do
        respondError
          handle
          ( object
              [ "jsonrpc" .= ("2.0" :: String),
                "id" .= (7 :: Int),
                "method" .= ("textDocument/unknown" :: String)
              ]
          )
          (-32601)
          "method not found: textDocument/unknown"
        hSeek handle AbsoluteSeek 0
        payload <- BS.hGetContents handle
        let body = BS.drop 4 (snd (BS.breakSubstring "\r\n\r\n" payload))
        decodeStrict' body
          `shouldBe` Just
            ( object
                [ "jsonrpc" .= ("2.0" :: String),
                  "id" .= (7 :: Int),
                  "error"
                    .= object
                      [ "code" .= (-32601 :: Int),
                        "message" .= ("method not found: textDocument/unknown" :: String)
                      ]
                ]
            )

    it "stays silent for a notification with no id" $
      withTempHandle $ \handle -> do
        respondError
          handle
          ( object
              ["jsonrpc" .= ("2.0" :: String), "method" .= ("$/cancelRequest" :: String)]
          )
          (-32601)
          "method not found"
        hSeek handle AbsoluteSeek 0
        payload <- BS.hGetContents handle
        BS.null payload `shouldBe` True

  describe "contentLengthFromHeaders" $ do
    it "finds content length regardless of header order or casing" $
      contentLengthFromHeaders
        [ "Content-Type: application/vscode-jsonrpc; charset=utf-8",
          "content-length: 42"
        ]
        `shouldBe` Just 42

    it "rejects missing or malformed lengths" $ do
      contentLengthFromHeaders ["X-Test: 1"] `shouldBe` Nothing
      contentLengthFromHeaders ["Content-Length: nope"] `shouldBe` Nothing

  describe "uri helpers" $
    it "round-trips file uris" $ do
      let path = "/tmp/project/main.tynix"
      uriPath (pathUri path) `shouldBe` path

  describe "uri encoding" $ do
    it "percent-encodes spaces and unicode in file uris" $
      pathUri "/tmp/type space/\x578b.tynix"
        `shouldBe` "file:///tmp/type%20space/%E5%9E%8B.tynix"

    it "decodes percent-encoded file uris back to filesystem paths" $
      uriPath "file:///tmp/type%20space/%E5%9E%8B.tynix"
        `shouldBe` "/tmp/type space/\x578b.tynix"

    it "accepts localhost authorities emitted by some LSP clients" $
      uriPath "file://localhost/tmp/main.tynix"
        `shouldBe` "/tmp/main.tynix"

  describe "documentPath" $
    it "extracts the path from textDocument params" $
      documentPath
        ( Just
            ( object
                ["textDocument" .= object ["uri" .= ("file:///tmp/main.tynix" :: String)]]
            )
        )
        `shouldBe` Just "/tmp/main.tynix"

  describe "documentPath localhost" $
    it "normalizes localhost authorities in textDocument params" $
      documentPath
        ( Just
            ( object
                [ "textDocument"
                    .= object ["uri" .= ("file://localhost/tmp/main.tynix" :: String)]
                ]
            )
        )
        `shouldBe` Just "/tmp/main.tynix"

  describe "applyContentChanges" $ do
    it "replaces the whole document when a change omits range information" $
      applyContentChanges
        "before"
        (Just (object ["contentChanges" .= [object ["text" .= ("after" :: String)]]]))
        `shouldBe` Right "after"

    it "applies incremental edits in order" $
      applyContentChanges
        "let value = 1;\n"
        ( Just
            ( object
                [ "contentChanges"
                    .= [ object
                           [ "range"
                               .= object
                                 [ "start" .= object ["line" .= (0 :: Int), "character" .= (4 :: Int)],
                                   "end" .= object ["line" .= (0 :: Int), "character" .= (9 :: Int)]
                                 ],
                             "text" .= ("result" :: String)
                           ],
                         object
                           [ "range"
                               .= object
                                 [ "start" .= object ["line" .= (0 :: Int), "character" .= (13 :: Int)],
                                   "end" .= object ["line" .= (0 :: Int), "character" .= (14 :: Int)]
                                 ],
                             "text" .= ("2" :: String)
                           ]
                       ]
                ]
            )
        )
        `shouldBe` Right "let result = 2;\n"

    it "applies incremental edits using UTF-16 character offsets" $
      applyContentChanges
        "\x1f600 abc"
        ( Just
            ( object
                [ "contentChanges"
                    .= [ object
                           [ "range"
                               .= object
                                 [ "start" .= object ["line" .= (0 :: Int), "character" .= (3 :: Int)],
                                   "end" .= object ["line" .= (0 :: Int), "character" .= (6 :: Int)]
                                 ],
                             "text" .= ("xyz" :: String)
                           ]
                       ]
                ]
            )
        )
        `shouldBe` Right "\x1f600 xyz"

    it "rejects invalid incremental ranges" $
      applyContentChanges
        "x"
        ( Just
            ( object
                [ "contentChanges"
                    .= [ object
                           [ "range"
                               .= object
                                 [ "start" .= object ["line" .= (0 :: Int), "character" .= (2 :: Int)],
                                   "end" .= object ["line" .= (0 :: Int), "character" .= (3 :: Int)]
                                 ],
                             "text" .= ("y" :: String)
                           ]
                       ]
                ]
            )
        )
        `shouldBe` Left "content change character is out of bounds"

    it "rejects incremental edits inside a UTF-16 surrogate pair" $
      applyContentChanges
        "\x1f600 x"
        ( Just
            ( object
                [ "contentChanges"
                    .= [ object
                           [ "range"
                               .= object
                                 [ "start" .= object ["line" .= (0 :: Int), "character" .= (1 :: Int)],
                                   "end" .= object ["line" .= (0 :: Int), "character" .= (1 :: Int)]
                                 ],
                             "text" .= ("!" :: String)
                           ]
                       ]
                ]
            )
        )
        `shouldBe` Left "content change character is out of bounds"

  describe "wordAt"
    $ it
      "extracts identifiers around the cursor including punctuation used by nix names"
    $ do
      wordAt 0 4 "lib.attr-set" `shouldBe` "lib.attr-set"
      wordAt 1 0 "x\ny" `shouldBe` "y"
      wordAt 0 4 "\x1f600 b more" `shouldBe` "b"

  describe "publishDiagnostics" $ do
    it
      "emits an empty diagnostics list on success and an error diagnostic on failure"
      $ do
        publishDiagnostics "/tmp/main.tynix" (Right successAnalysis)
          `shouldBe` object
            [ "uri" .= ("file:///tmp/main.tynix" :: String),
              "diagnostics" .= ([] :: [Value])
            ]
        case publishDiagnostics "/tmp/main.tynix" (Left "boom") of
          Object obj ->
            KeyMap.lookup "diagnostics" obj `shouldBe` Just (toJSON [diag "boom"])
          other -> expectationFailure ("expected object, got " <> show other)

    it "builds an explicit clear-diagnostics notification body" $
      clearDiagnostics "/tmp/main.tynix"
        `shouldBe` object
          [ "uri" .= ("file:///tmp/main.tynix" :: String),
            "diagnostics" .= ([] :: [Value])
          ]

  describe "publishDiagnosticsWithContent" $ do
    it "highlights unbound names using the token span in the current buffer" $
      diagnosticSpan
        ( publishDiagnosticsWithContent
            "/tmp/main.tynix"
            "missing"
            (Left "unbound name: \"missing\"")
        )
        `shouldBe` Just (0, 0, 7)

    it "reports token ranges using UTF-16 columns after non-BMP text" $
      diagnosticSpan
        ( publishDiagnosticsWithContent
            "/tmp/main.tynix"
            "\x1f600 missing"
            (Left "unbound name: \"missing\"")
        )
        `shouldBe` Just (0, 3, 10)

    it "uses parser coordinates when a parse error includes line and column data" $
      diagnosticSpan
        ( publishDiagnosticsWithContent
            "/tmp/main.tynix"
            "ok\n  broken"
            (Left "/tmp/main.tynix:2:3:\n  |\n2 |   broken\n  |   ^")
        )
        `shouldBe` Just (1, 2, 8)

  describe "range helpers" $
    it "returns UTF-16 columns for symbols after non-BMP text" $ do
      findWordRange "\x1f600 missing" "missing" `shouldBe` Just (0, 3, 10)
      findFieldRange "\x1f600 box.alpha" "alpha" `shouldBe` Just (0, 7, 12)
      findDefinitionRange "\x1f600 value = 1" "value" `shouldBe` Just (0, 3, 8)

  describe "hoverResult" $
    it "prefers the hovered symbol type and falls back to the root type" $ do
      hoverResult (Right successAnalysis) "box" 0 1
        `shouldBe` object
          [ "contents"
              .= object
                [ "kind" .= ("markdown" :: String),
                  "value" .= ("```tynix\nString\n```" :: String)
                ]
          ]
      hoverResult (Right builtinsAnalysis) "builtins.map" 0 2
        `shouldBe` object
          [ "contents"
              .= object
                [ "kind" .= ("markdown" :: String),
                  "value"
                    .= ("```tynix\n{\n  map :: Int -> String;\n}\n```" :: String)
                ]
          ]
      hoverResult (Right builtinsAnalysis) "builtins.map" 0 10
        `shouldBe` object
          [ "contents"
              .= object
                [ "kind" .= ("markdown" :: String),
                  "value" .= ("```tynix\nInt -> String\n```" :: String)
                ]
          ]
      hoverResult (Right rootOnlyAnalysis) "?" 0 0
        `shouldBe` object
          [ "contents"
              .= object
                ["kind" .= ("markdown" :: String), "value" .= ("```tynix\nInt\n```" :: String)]
          ]
      hoverResult (Left "type mismatch") "box" 0 0
        `shouldBe` object
          [ "contents"
              .= object
                [ "kind" .= ("markdown" :: String),
                  "value" .= ("```tynix\ntype mismatch\n```" :: String)
                ]
          ]

  describe "completionResult" $ do
    it "offers top-level symbols, builtins, and protocol-safe metadata" $
      completionLabels (completionResult (Right completionAnalysis) "b" 0 1)
        `shouldBe` ["box", "builtins"]

    it "offers root attribute-set fields as top-level completions" $
      completionLabels (completionResult (Right rootFieldCompletionAnalysis) "in" 0 2)
        `shouldBe` ["inc"]

    it "interprets completion positions as UTF-16 character offsets" $
      completionLabels
        (completionResult (Right completionAnalysis) "\x1f600 b more" 0 4)
        `shouldBe` ["box", "builtins"]

    it "offers record fields after a dotted access and filters by prefix" $ do
      completionLabels (completionResult (Right completionAnalysis) "box." 0 4)
        `shouldBe` ["alpha", "beta"]
      completionLabels (completionResult (Right completionAnalysis) "box.al" 0 6)
        `shouldBe` ["alpha"]

  describe "framing" $ do
    it "writes a Content-Length header followed by a JSON body" $
      withTempHandle $ \handle -> do
        send
          handle
          (object ["jsonrpc" .= ("2.0" :: String), "method" .= ("initialized" :: String)])
        hSeek handle AbsoluteSeek 0
        payload <- BS.hGetContents handle
        let body = snd (BS.breakSubstring "\r\n\r\n" payload)
        "Content-Length: " `BS.isPrefixOf` payload `shouldBe` True
        decodeStrict' (BS.drop 4 body)
          `shouldBe` Just
            (object ["jsonrpc" .= ("2.0" :: String), "method" .= ("initialized" :: String)])

    it "flushes framed messages so stdio clients can read them immediately" $
      withPipeHandles $ \readHandle writeHandle -> do
        send
          writeHandle
          (object ["jsonrpc" .= ("2.0" :: String), "method" .= ("initialized" :: String)])
        hWaitForInput readHandle 100 `shouldReturn` True
        payload <- BS.hGetSome readHandle 4096
        "Content-Length: " `BS.isPrefixOf` payload `shouldBe` True

    it "reads a framed message payload from a handle" $
      withTempHandle $ \handle -> do
        B8.hPutStr handle "Content-Length: 17\r\nX-Test: 1\r\n\r\n{\"jsonrpc\":\"2.0\"}"
        hFlush handle
        hSeek handle AbsoluteSeek 0
        readMessage handle `shouldReturn` Just (object ["jsonrpc" .= ("2.0" :: String)])

    it "tags clean end-of-stream as ReadEof" $
      withTempHandle $ \handle -> do
        outcome <- readMessageOutcome handle
        case outcome of
          ReadEof -> pure ()
          other -> expectationFailure ("expected ReadEof, got " <> describeOutcome other)

    it "reports malformed Content-Length headers as ReadError" $
      withTempHandle $ \handle -> do
        B8.hPutStr handle "Content-Length: nope\r\n\r\n"
        hFlush handle
        hSeek handle AbsoluteSeek 0
        outcome <- readMessageOutcome handle
        case outcome of
          ReadError _ -> pure ()
          other -> expectationFailure ("expected ReadError, got " <> describeOutcome other)

    it "reports unparseable JSON bodies as ReadError" $
      withTempHandle $ \handle -> do
        B8.hPutStr handle "Content-Length: 7\r\n\r\nnot{jso"
        hFlush handle
        hSeek handle AbsoluteSeek 0
        outcome <- readMessageOutcome handle
        case outcome of
          ReadError _ -> pure ()
          other -> expectationFailure ("expected ReadError, got " <> describeOutcome other)

successAnalysis :: Analysis
successAnalysis =
  Analysis
    { analysisProgram = error "unused in server tests",
      analysisRoot = Just (Scheme [] tInt),
      analysisBindings = Map.fromList [("box", Scheme [] tString)],
      analysisAliases = mempty,
      analysisAmbient = mempty
    }

rootOnlyAnalysis :: Analysis
rootOnlyAnalysis =
  Analysis
    { analysisProgram = error "unused in server tests",
      analysisRoot = Just (Scheme [] tInt),
      analysisBindings = Map.empty,
      analysisAliases = mempty,
      analysisAmbient = mempty
    }

builtinsAnalysis :: Analysis
builtinsAnalysis =
  Analysis
    { analysisProgram = error "unused in server tests",
      analysisRoot = Just (Scheme [] (tList tString)),
      analysisBindings = Map.empty,
      analysisAliases = mempty,
      analysisAmbient =
        Map.fromList
          [ ( "builtins",
              Scheme
                []
                ( TRecord
                    ( Map.fromList
                        [ ("map", TFun Many tInt tString)
                        ]
                    )
                )
            )
          ]
    }

completionAnalysis :: Analysis
completionAnalysis =
  Analysis
    { analysisProgram = error "unused in server tests",
      analysisRoot = Just (Scheme [] tInt),
      analysisBindings =
        Map.fromList
          [ ( "box",
              Scheme
                []
                ( TRecord
                    ( Map.fromList
                        [ ("alpha", tInt),
                          ("beta", tString)
                        ]
                    )
                )
            )
          ],
      analysisAliases = mempty,
      analysisAmbient =
        Map.fromList
          [ ( "builtins",
              Scheme
                []
                ( TRecord
                    ( Map.fromList
                        [ ( "map",
                            TForall
                              ["a", "b"]
                              ( TFun
                                  Many
                                  (TFun Many (TVar "a") (TVar "b"))
                                  (TFun Many (tList (TVar "a")) (tList (TVar "b")))
                              )
                          )
                        ]
                    )
                )
            )
          ]
    }

rootFieldCompletionAnalysis :: Analysis
rootFieldCompletionAnalysis =
  Analysis
    { analysisProgram = error "unused in server tests",
      analysisRoot =
        Just
          ( Scheme
              []
              ( TRecord
                  ( Map.fromList
                      [ ("inc", TFun Many tInt tInt),
                        ("twice", TFun Many tInt tInt)
                      ]
                  )
              )
          ),
      analysisBindings = Map.empty,
      analysisAliases = mempty,
      analysisAmbient = mempty
    }

withTempHandle :: (Handle -> IO a) -> IO a
withTempHandle action = do
  tmp <- getTemporaryDirectory
  (path, handle) <- openTempFile tmp "tynix-lsp-spec"
  result <- action handle
  hClose handle
  removeFile path
  pure result

withPipeHandles :: (Handle -> Handle -> IO a) -> IO a
withPipeHandles action =
  bracket open close (uncurry action)
  where
    open = do
      (readFd, writeFd) <- createPipe
      readHandle <- fdToHandle readFd
      writeHandle <- fdToHandle writeFd
      hSetBinaryMode readHandle True
      hSetBinaryMode writeHandle True
      pure (readHandle, writeHandle)
    close (readHandle, writeHandle) = hClose readHandle >> hClose writeHandle

describeOutcome :: ReadOutcome -> String
describeOutcome ReadEof = "ReadEof"
describeOutcome (ReadMessage value) = "ReadMessage " <> show value
describeOutcome (ReadError reason) = "ReadError " <> show reason

completionLabels :: Value -> [Text]
completionLabels value =
  case value of
    Object obj ->
      case KeyMap.lookup "items" obj of
        Just (Array items) ->
          [ label
          | Object item <- toList items,
            Just (String label) <- [KeyMap.lookup "label" item]
          ]
        _ -> []
    _ -> []

diagnosticSpan :: Value -> Maybe (Int, Int, Int)
diagnosticSpan value = do
  Object obj <- pure value
  Array diagnostics <- KeyMap.lookup "diagnostics" obj
  Object diagnostic <- listToMaybe (toList diagnostics)
  Object range <- KeyMap.lookup "range" diagnostic
  Object start <- KeyMap.lookup "start" range
  Object end <- KeyMap.lookup "end" range
  Number startLine <- KeyMap.lookup "line" start
  Number startChar <- KeyMap.lookup "character" start
  Number endChar <- KeyMap.lookup "character" end
  pure (floor startLine, floor startChar, floor endChar)
