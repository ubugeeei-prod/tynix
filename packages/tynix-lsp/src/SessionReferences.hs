{-# LANGUAGE OverloadedStrings #-}

-- | Reference- and definition-resolution layer used by `textDocument/definition`,
-- `textDocument/references`, and `textDocument/rename`.
--
-- The layer is purely a query on the symbol index produced by
-- 'SessionSymbols'. It does no IO and no analysis itself; it just picks
-- the right `ReferenceTarget` or location based on the cursor word, the
-- per-file analysis result, and the workspace document set.
module SessionReferences
  ( -- * Filtering workspace docs for a target
    workspaceDocumentsForTarget,

    -- * Lifting symbol indexes into ranges
    allDefinitionRanges,
    allFieldRanges,
    allWordRanges,
    symbolRanges,

    -- * Per-cursor resolution
    resolveDefinitionLocation,
    resolveReferenceTarget,
  )
where

import Control.Applicative ((<|>))
import Data.List (find, isSuffixOf, nub, sortOn)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Driver (Analysis)
import Server
  ( findDefinitionRange,
    findFieldRange,
    textRangeToUtf16Columns,
    wordAt,
  )
import SessionSymbols (documentIndexedSymbols, workspaceIndexedSymbols)
import SessionText (definitionSpans, fieldSpans, wordSpans)
import SessionTypes
  ( IndexedSymbol (..),
    MatchMode (..),
    ReferenceTarget (..),
    WorkspaceDocument (..),
  )

-- | Filter the workspace document list to only those mentioned by the
-- reference target. Useful when iterating files to compute references /
-- rename edits.
workspaceDocumentsForTarget ::
  [WorkspaceDocument] -> ReferenceTarget -> [WorkspaceDocument]
workspaceDocumentsForTarget workspace target =
  filter
    (\doc -> workspaceDocumentFile doc `elem` referenceTargetFiles target)
    workspace

-- | All ranges in @content@ matching @symbol@ under the given match mode.
--
-- For 'WholeWordMatch' this lights up whole-identifier references; for
-- 'FieldWordMatch' it also lights up dot-prefixed selections so
-- @record.foo@ shows up when searching for @foo@.
symbolRanges :: Text -> Text -> MatchMode -> [(Int, Int, Int)]
symbolRanges content symbol mode =
  nub . sortOn id $
    case mode of
      WholeWordMatch -> allWordRanges content symbol
      FieldWordMatch -> allDefinitionRanges content symbol <> allFieldRanges content symbol

allDefinitionRanges :: Text -> Text -> [(Int, Int, Int)]
allDefinitionRanges content symbol =
  concatMap
    ( \(lineNo, line) ->
        map
          ( \(startOffset, endOffset) ->
              let (startChar, endChar) = textRangeToUtf16Columns line startOffset endOffset
               in (lineNo, startChar, endChar)
          )
          (definitionSpans line symbol)
    )
    (zip [0 ..] (Text.lines content))

allFieldRanges :: Text -> Text -> [(Int, Int, Int)]
allFieldRanges content symbol =
  concatMap
    ( \(lineNo, line) ->
        map
          ( \startOffset ->
              let endOffset = startOffset + Text.length symbol
                  (startChar, endChar) = textRangeToUtf16Columns line startOffset endOffset
               in (lineNo, startChar, endChar)
          )
          (fieldSpans line symbol)
    )
    (zip [0 ..] (Text.lines content))

allWordRanges :: Text -> Text -> [(Int, Int, Int)]
allWordRanges content symbol =
  concatMap
    ( \(lineNo, line) ->
        map
          ( \startOffset ->
              let endOffset = startOffset + Text.length symbol
                  (startChar, endChar) = textRangeToUtf16Columns line startOffset endOffset
               in (lineNo, startChar, endChar)
          )
          (wordSpans line symbol)
    )
    (zip [0 ..] (Text.lines content))

-- | Resolve the cursor position to a `Location` for @textDocument/definition@.
--
-- The strategy is intentionally simple:
--
-- 1. `builtins.foo` always jumps into the discovered `builtins.d.tynix`.
-- 2. Bare identifiers prefer the current document, then the unique
--    workspace match.
-- 3. Dotted selections fall back to the last segment as a field name.
resolveDefinitionLocation ::
  FilePath ->
  Text ->
  [WorkspaceDocument] ->
  Maybe FilePath ->
  Either String Analysis ->
  Int ->
  Int ->
  Maybe (FilePath, Int, Int, Int)
resolveDefinitionLocation file content workspace builtinsFile result lineNo charNo =
  let symbol = wordAt lineNo charNo content
      parts = filter (not . Text.null) (Text.splitOn "." symbol)
      workspaceSymbols = workspaceIndexedSymbols workspace
      currentDefinitions = documentIndexedSymbols file content result
      currentFieldTarget name =
        (\(targetLine, startChar, endChar) -> (file, targetLine, startChar, endChar))
          <$> (findFieldRange content name <|> findDefinitionRange content name)
      uniqueWorkspaceTarget name =
        case filter (\entry -> indexedSymbolName entry == name) workspaceSymbols of
          [entry] ->
            let (targetLine, startChar, endChar) = indexedSymbolRange entry
             in Just (indexedSymbolFile entry, targetLine, startChar, endChar)
          entries ->
            case filter (\entry -> indexedSymbolFile entry == file) entries of
              current : _ ->
                let (targetLine, startChar, endChar) = indexedSymbolRange current
                 in Just (indexedSymbolFile current, targetLine, startChar, endChar)
              [] -> Nothing
   in case parts of
        ["builtins", member] ->
          builtinsFile >>= \targetFile ->
            let targetDoc = find (\doc -> workspaceDocumentFile doc == targetFile) workspace
             in case targetDoc of
                  Just doc ->
                    ( \(targetLine, startChar, endChar) -> (targetFile, targetLine, startChar, endChar)
                    )
                      <$> findDefinitionRange (workspaceDocumentContent doc) member
                  Nothing -> Nothing
        [name]
          | any (\entry -> indexedSymbolName entry == name) currentDefinitions ->
              (\(targetLine, startChar, endChar) -> (file, targetLine, startChar, endChar))
                <$> findDefinitionRange content name
          | otherwise -> uniqueWorkspaceTarget name
        _
          | Just fieldName <- listToMaybe (reverse parts) ->
              currentFieldTarget fieldName <|> uniqueWorkspaceTarget fieldName
        _ -> Nothing

-- | Resolve the cursor position to a 'ReferenceTarget' for
-- `textDocument/references` and `textDocument/rename`.
resolveReferenceTarget ::
  FilePath ->
  Text ->
  [WorkspaceDocument] ->
  Maybe FilePath ->
  Either String Analysis ->
  Int ->
  Int ->
  Maybe ReferenceTarget
resolveReferenceTarget file content workspace builtinsFile result lineNo charNo =
  let symbol = wordAt lineNo charNo content
      parts = filter (not . Text.null) (Text.splitOn "." symbol)
      currentSymbols = documentIndexedSymbols file content result
      workspaceSymbols = workspaceIndexedSymbols workspace
      currentFileOnly name =
        Just
          ReferenceTarget
            { referenceTargetFiles = [file],
              referenceTargetNeedle = name,
              referenceTargetMode = WholeWordMatch
            }
      workspaceField name =
        Just
          ReferenceTarget
            { referenceTargetFiles = map workspaceDocumentFile workspace,
              referenceTargetNeedle = name,
              referenceTargetMode = FieldWordMatch
            }
      uniqueWorkspace name =
        case filter (\entry -> indexedSymbolName entry == name) workspaceSymbols of
          [_] -> workspaceField name
          _ -> Nothing
   in case parts of
        ["builtins", member] ->
          case builtinsFile of
            Just _ -> workspaceField member
            Nothing -> Nothing
        [name]
          | any (\entry -> indexedSymbolName entry == name) currentSymbols ->
              currentFileOnly name
          | ".d.tynix" `isSuffixOf` file -> uniqueWorkspace name <|> currentFileOnly name
          | otherwise -> currentFileOnly name
        _
          | Just fieldName <- listToMaybe (reverse parts) -> workspaceField fieldName
        _ -> Nothing
