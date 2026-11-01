{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import SessionDiagnostics
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "diagnosticPayloads" $ do
    it "returns an empty list when the message has no diagnostics array" $
      diagnosticPayloads (object ["params" .= object []]) `shouldBe` []

    it "extracts the diagnostics array from params.context.diagnostics" $ do
      let diag = object ["message" .= ("oops" :: Text)]
          msg =
            object
              [ "params"
                  .= object
                    [ "context" .= object ["diagnostics" .= [diag]]
                    ]
              ]
      diagnosticPayloads msg `shouldBe` [diag]

  describe "diagnosticRange" $ do
    it "extracts (line, startChar, endChar) from a well-formed diagnostic" $ do
      let d =
            object
              [ "range"
                  .= object
                    [ "start" .= object ["line" .= (3 :: Int), "character" .= (5 :: Int)],
                      "end" .= object ["line" .= (3 :: Int), "character" .= (9 :: Int)]
                    ]
              ]
      diagnosticRange d `shouldBe` Just (3, 5, 9)

    it "returns Nothing when range fields are missing" $
      diagnosticRange (object ["range" .= object []]) `shouldBe` Nothing

  describe "diagnosticSymbolName" $ do
    it "extracts the first quoted identifier from a diagnostic message" $
      diagnosticSymbolName "unbound name: \"value\"" `shouldBe` Just "value"

    it "returns Nothing when there is no quoted identifier" $
      diagnosticSymbolName "type mismatch in lambda" `shouldBe` Nothing

    it "returns Nothing when the opening quote has no matching close" $
      diagnosticSymbolName "missing field \"value" `shouldBe` Nothing

  describe "rangeValue" $
    it "renders an LSP range from line + start/end columns" $
      rangeValue 2 4 9
        `shouldBe` object
          [ "start" .= object ["line" .= (2 :: Int), "character" .= (4 :: Int)],
            "end" .= object ["line" .= (2 :: Int), "character" .= (9 :: Int)]
          ]

  describe "textEdit" $
    it "renders a TextEdit value with the right range and replacement" $
      textEdit 5 1 4 "tynix"
        `shouldBe` object
          [ "range" .= rangeValue 5 1 4,
            "newText" .= ("tynix" :: Text)
          ]

  describe "insertLineEdit" $
    it "renders a zero-width edit at the start of a line" $
      insertLineEdit 7 "# @tynix-ignore\n"
        `shouldBe` object
          [ "range"
              .= object
                [ "start" .= object ["line" .= (7 :: Int), "character" .= (0 :: Int)],
                  "end" .= object ["line" .= (7 :: Int), "character" .= (0 :: Int)]
                ],
            "newText" .= ("# @tynix-ignore\n" :: Text)
          ]

  describe "lineHasDirective" $ do
    let body = "let\n# @tynix-ignore\nvalue = missing;\n"
    it "detects a directive immediately above the target line" $
      lineHasDirective "# @tynix-ignore" 2 body `shouldBe` True

    it "returns False when no directive is on the previous line" $
      lineHasDirective "# @tynix-ignore" 1 body `shouldBe` False

  describe "quickFixAction" $
    it "wraps edits in a `quickfix` workspace-edit envelope" $ do
      let action = quickFixAction "Replace" "/tmp/x.tynix" [textEdit 0 0 1 "y"]
      case action of
        Object obj -> do
          KeyMap.lookup (Key.fromText "title") obj `shouldBe` Just (String "Replace")
          KeyMap.lookup (Key.fromText "kind") obj `shouldBe` Just (String "quickfix")
          KeyMap.member (Key.fromText "edit") obj `shouldBe` True
        _ -> expectationFailure "expected an Object payload"

  describe "directiveActions" $ do
    it "offers both directive quick fixes for a clean line" $ do
      let diag =
            object
              [ "range"
                  .= object
                    [ "start" .= object ["line" .= (1 :: Int), "character" .= (0 :: Int)],
                      "end" .= object ["line" .= (1 :: Int), "character" .= (10 :: Int)]
                    ]
              ]
      length
        (directiveActions "/tmp/x.tynix" "let\nvalue = missing;\nin value\n" diag)
        `shouldBe` 2

    it "skips both quick fixes when the previous line already has @tynix-ignore" $ do
      let diag =
            object
              [ "range"
                  .= object
                    [ "start" .= object ["line" .= (2 :: Int), "character" .= (0 :: Int)],
                      "end" .= object ["line" .= (2 :: Int), "character" .= (10 :: Int)]
                    ]
              ]
          body = "let\n# @tynix-ignore\nvalue = missing;\nin value\n"
      directiveActions "/tmp/x.tynix" body diag `shouldBe` []

  describe "closestCandidate / nameDistance" $ do
    it "returns the closest candidate within the edit-distance budget" $
      closestCandidate ["values", "validate", "vector"] "valeu"
        `shouldBe` Just "values"

    it "returns Nothing when no candidate is close enough" $
      closestCandidate ["alpha", "beta", "gamma"] "valuesomething" `shouldBe` Nothing

    it "filters out the needle itself before ranking" $
      closestCandidate ["value", "values"] "value" `shouldBe` Just "values"

    it "treats case differences as zero-cost edits" $
      nameDistance "Value" "value" `shouldBe` 0
