{-# LANGUAGE OverloadedStrings #-}

-- | Diagnostic-aware helpers used by `textDocument/codeAction` and the
-- editing-related JSON value constructors that go with them.
--
-- The functions split into two halves: pure inspectors over the diagnostic
-- payload (`diagnosticPayloads`, `diagnosticRange`, `diagnosticSymbolName`)
-- and the LSP-flavoured value builders for quick-fix workspace edits
-- (`textEdit`, `rangeValue`, `workspaceEdit`, `insertLineEdit`,
-- `quickFixAction`, `directiveActions`).
--
-- A small Levenshtein-style closest-candidate helper lives here too because
-- the rename quick fix is the only consumer.
module SessionDiagnostics
  ( -- * Diagnostic inspection
    diagnosticPayloads,
    diagnosticRange,
    diagnosticSymbolName,

    -- * Quick-fix construction
    directiveActions,
    insertLineEdit,
    lineHasDirective,
    quickFixAction,
    rangeValue,
    textEdit,
    workspaceEdit,

    -- * Rename suggestion
    closestCandidate,
    nameDistance,
  )
where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as Text
import Server (asInt, field, pathUri)

-- | Pull the @params.context.diagnostics@ array out of a `codeAction`
-- request body.
diagnosticPayloads :: Value -> [Value]
diagnosticPayloads msg =
  case field "params" msg >>= field "context" >>= field "diagnostics" of
    Just (Array diagnostics) -> toList diagnostics
    _ -> []
  where
    toList = foldr (:) []

-- | Extract the (line, startChar, endChar) range from one diagnostic.
diagnosticRange :: Value -> Maybe (Int, Int, Int)
diagnosticRange diagnostic = do
  range <- field "range" diagnostic
  start <- field "start" range
  ending <- field "end" range
  lineNo <- field "line" start
  startChar <- field "character" start
  endChar <- field "character" ending
  pure (asInt lineNo, asInt startChar, asInt endChar)

-- | Pull the first double-quoted identifier out of a diagnostic message.
--
-- Used by the rename quick fix to figure out which symbol the diagnostic
-- is talking about.
diagnosticSymbolName :: Text -> Maybe Text
diagnosticSymbolName message =
  case [ (Text.length before, name)
       | q <- ["\"", "`"],
         before : name : _ : _ <- [Text.splitOn q message]
       ] of
    [] -> Nothing
    found -> Just (snd (minimum found))

-- | The two `tynix-ignore` / `tynix-expected` quick fixes for a diagnostic.
--
-- Skips both when the offending line already carries a `# @tynix-ignore`
-- directive so the menu doesn't suggest no-ops.
directiveActions :: FilePath -> Text -> Value -> [Value]
directiveActions file content diagnostic =
  case diagnosticRange diagnostic of
    Just (lineNo, _, _)
      | not (lineHasDirective "# @tynix-ignore" lineNo content) ->
          [ quickFixAction
              "Add `# @tynix-ignore`"
              file
              [insertLineEdit lineNo "# @tynix-ignore\n"],
            quickFixAction
              "Add `# @tynix-expected`"
              file
              [insertLineEdit lineNo "# @tynix-expected\n"]
          ]
      | otherwise -> []
    Nothing -> []

-- | Quick-fix action helper. The result plugs straight into the LSP
-- `textDocument/codeAction` response.
quickFixAction :: Text -> FilePath -> [Value] -> Value
quickFixAction title file edits =
  object
    [ "title" .= title,
      "kind" .= ("quickfix" :: Text),
      "edit" .= workspaceEdit [(file, edits)]
    ]

-- | True if the line immediately above @lineNo@ already starts with the
-- given directive.
lineHasDirective :: Text -> Int -> Text -> Bool
lineHasDirective directive lineNo content =
  case if lineNo <= 0 then [] else drop (lineNo - 1) (Text.lines content) of
    previous : _ -> directive `Text.isPrefixOf` Text.stripStart previous
    [] -> False

-- | Build a `TextEdit` value at @(line, startChar, endChar)@.
textEdit :: Int -> Int -> Int -> Text -> Value
textEdit lineNo startChar endChar newText =
  object
    [ "range" .= rangeValue lineNo startChar endChar,
      "newText" .= newText
    ]

-- | Build a single-line `Range` value used by `textEdit` / `Location`.
rangeValue :: Int -> Int -> Int -> Value
rangeValue lineNo startChar endChar =
  object
    [ "start" .= object ["line" .= lineNo, "character" .= startChar],
      "end" .= object ["line" .= lineNo, "character" .= endChar]
    ]

-- | Build an insert-at-line-start edit. Used to prepend `# @tynix-*`
-- directive lines.
insertLineEdit :: Int -> Text -> Value
insertLineEdit lineNo newText =
  object
    [ "range"
        .= object
          [ "start" .= object ["line" .= lineNo, "character" .= (0 :: Int)],
            "end" .= object ["line" .= lineNo, "character" .= (0 :: Int)]
          ],
      "newText" .= newText
    ]

-- | Bundle per-file edits into a `WorkspaceEdit.changes` map.
workspaceEdit :: [(FilePath, [Value])] -> Value
workspaceEdit edits =
  object
    [ "changes"
        .= Object
          ( KeyMap.fromList
              [ (Key.fromText (pathUri file), toJSON fileEdits)
              | (file, fileEdits) <- edits
              ]
          )
    ]

-- | Closest in-scope name to a typo, returning 'Nothing' if no candidate
-- is close enough to be useful.
--
-- "Close enough" means edit distance ≤ @max 2 (length needle / 2)@.
closestCandidate :: [Text] -> Text -> Maybe Text
closestCandidate candidates needle =
  case sortOn
    (\candidate -> (nameDistance needle candidate, candidate))
    (filter (/= needle) candidates) of
    candidate : _
      | nameDistance needle candidate <= max 2 (Text.length needle `div` 2) ->
          Just candidate
    _ -> Nothing

-- | Plain Levenshtein distance, case-insensitive, used only for the rename
-- quick-fix ranking.
nameDistance :: Text -> Text -> Int
nameDistance left right = last (foldl' step [0 .. length rightChars] (zip [1 ..] leftChars))
  where
    leftChars = map toLower (Text.unpack left)
    rightChars = map toLower (Text.unpack right)
    step previousRow (rowIndex, leftChar) =
      scanl
        ( \leftCost (columnIndex, rightChar) ->
            minimum
              [ leftCost + 1,
                previousRow !! columnIndex + 1,
                previousRow !! (columnIndex - 1) + substitutionCost leftChar rightChar
              ]
        )
        rowIndex
        (zip [1 ..] rightChars)
    substitutionCost leftChar rightChar
      | leftChar == rightChar = 0
      | otherwise = 1
