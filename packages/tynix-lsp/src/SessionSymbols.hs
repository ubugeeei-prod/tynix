{-# LANGUAGE OverloadedStrings #-}

-- | Symbol indexing for documentSymbol / workspaceSymbol and the
-- definition / references handlers.
--
-- This module owns the conversion from a tynix `Analysis` to the
-- LSP-flavoured 'IndexedSymbol' records, plus the per-file `declare ".."`
-- range lookup used to highlight ambient module headers.
module SessionSymbols
  ( -- * Symbol index
    documentIndexedSymbols,
    workspaceIndexedSymbols,
    indexedSymbolInformation,

    -- * Completion + ranking inputs
    documentCandidateNames,

    -- * Range helpers
    findDeclareRange,

    -- * LSP value helpers
    kindForType,
    locationFromRange,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (Value, object, (.=))
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Driver (Analysis (..))
import Server
  ( findDefinitionRange,
    findFieldRange,
    location,
    textRangeToUtf16Columns,
  )
import SessionTypes (IndexedSymbol (..), WorkspaceDocument (..))
import Subtyping (resolveType)
import Syntax
  ( AmbientDecl (ambientEntries, ambientPath),
    AmbientEntry (ambientEntryName, ambientEntryType),
    Program (programAliases, programAmbient),
  )
import Type
  ( LiteralType (..),
    Scheme (schemeType),
    Type (..),
    TypeAlias (typeAliasName),
  )

-- | All editor-visible symbols defined inside a single document.
--
-- Bindings, top-level aliases, ambient declarations / entries, and the
-- fields of a record root each contribute. The result is sorted by source
-- position so 'workspace/symbol' picks a stable order.
documentIndexedSymbols ::
  FilePath -> Text -> Either String Analysis -> [IndexedSymbol]
documentIndexedSymbols file content result =
  case result of
    Left _ -> []
    Right analysis ->
      sortOn indexedSymbolRange $
        aliasSymbols analysis
          <> ambientModuleSymbols analysis
          <> ambientEntrySymbols analysis
          <> bindingSymbols analysis
          <> rootExportSymbols analysis
  where
    aliasSymbols analysis =
      mapMaybe
        ( \alias -> do
            range <- findDefinitionRange content (typeAliasName alias)
            pure
              IndexedSymbol
                { indexedSymbolName = typeAliasName alias,
                  indexedSymbolKind = 23,
                  indexedSymbolFile = file,
                  indexedSymbolRange = range,
                  indexedSymbolContainer = Just "type"
                }
        )
        (programAliases (analysisProgram analysis))

    ambientModuleSymbols analysis =
      mapMaybe
        ( \decl -> do
            range <- findDeclareRange content (ambientPath decl)
            pure
              IndexedSymbol
                { indexedSymbolName = Text.pack (ambientPath decl),
                  indexedSymbolKind = 2,
                  indexedSymbolFile = file,
                  indexedSymbolRange = range,
                  indexedSymbolContainer = Nothing
                }
        )
        (programAmbient (analysisProgram analysis))

    ambientEntrySymbols analysis =
      concatMap
        ( \decl ->
            mapMaybe
              ( \entry -> do
                  range <- findDefinitionRange content (ambientEntryName entry)
                  pure
                    IndexedSymbol
                      { indexedSymbolName = ambientEntryName entry,
                        indexedSymbolKind = kindForType (ambientEntryType entry),
                        indexedSymbolFile = file,
                        indexedSymbolRange = range,
                        indexedSymbolContainer = Just (Text.pack (ambientPath decl))
                      }
              )
              (ambientEntries decl)
        )
        (programAmbient (analysisProgram analysis))

    bindingSymbols analysis =
      mapMaybe
        ( \(name, scheme) -> do
            range <- findDefinitionRange content name
            pure
              IndexedSymbol
                { indexedSymbolName = name,
                  indexedSymbolKind = kindForType (schemeType scheme),
                  indexedSymbolFile = file,
                  indexedSymbolRange = range,
                  indexedSymbolContainer = Just "let"
                }
        )
        (Map.toList (analysisBindings analysis))

    rootExportSymbols analysis =
      case analysisRoot analysis of
        Nothing -> []
        Just scheme ->
          case resolveType (analysisAliases analysis) (schemeType scheme) of
            TRecord fields ->
              mapMaybe
                ( \(name, fieldTy) -> do
                    range <- findFieldRange content name <|> findDefinitionRange content name
                    pure
                      IndexedSymbol
                        { indexedSymbolName = name,
                          indexedSymbolKind = kindForType fieldTy,
                          indexedSymbolFile = file,
                          indexedSymbolRange = range,
                          indexedSymbolContainer = Just "default"
                        }
                )
                (Map.toList fields)
            _ -> []

-- | Aggregate every symbol across the workspace, sorted by name + location
-- so 'workspace/symbol' picks a deterministic order.
workspaceIndexedSymbols :: [WorkspaceDocument] -> [IndexedSymbol]
workspaceIndexedSymbols workspace =
  sortOn
    ( \symbol ->
        (indexedSymbolName symbol, indexedSymbolFile symbol, indexedSymbolRange symbol)
    )
    $ concatMap
      ( \doc ->
          documentIndexedSymbols
            (workspaceDocumentFile doc)
            (workspaceDocumentContent doc)
            (workspaceDocumentAnalysis doc)
      )
      workspace

-- | Convert a single 'IndexedSymbol' into the JSON shape LSP expects from
-- 'workspace/symbol' and 'textDocument/documentSymbol'.
indexedSymbolInformation :: IndexedSymbol -> Value
indexedSymbolInformation symbol =
  object $
    [ "name" .= indexedSymbolName symbol,
      "kind" .= indexedSymbolKind symbol,
      "location"
        .= locationFromRange (indexedSymbolFile symbol) (indexedSymbolRange symbol)
    ]
      <> maybe
        []
        (\container -> ["containerName" .= container])
        (indexedSymbolContainer symbol)

-- | Candidate name list used by completion / rename to score against the
-- current buffer.
documentCandidateNames :: Either String Analysis -> [Text]
documentCandidateNames result =
  case result of
    Left _ -> []
    Right analysis ->
      nub $
        Map.keys (analysisBindings analysis)
          <> map typeAliasName (programAliases (analysisProgram analysis))
          <> concatMap
            (map ambientEntryName . ambientEntries)
            (programAmbient (analysisProgram analysis))

-- | Locate the @declare "..."@ header for a given ambient path.
--
-- Returned as @(line, startUtf16, endUtf16)@ so the LSP layer can plug
-- the range directly into a `Location`.
findDeclareRange :: Text -> FilePath -> Maybe (Int, Int, Int)
findDeclareRange content path =
  listToMaybe $
    mapMaybe
      ( \(lineNo, line) ->
          let needles =
                [ "declare \"" <> Text.pack path <> "\"",
                  "declare " <> Text.pack path
                ]
           in listToMaybe
                [ let (startColumn, endColumn) =
                        textRangeToUtf16Columns
                          line
                          (startOffset + 8)
                          (startOffset + 8 + Text.length (Text.pack path))
                   in (lineNo, startColumn, endColumn)
                | needle <- needles,
                  (prefix, suffix) <- Text.breakOnAll needle line,
                  not (Text.null suffix),
                  let startOffset = Text.length prefix
                ]
      )
      (zip [0 ..] (Text.lines content))

-- | LSP `SymbolKind` mapping for a tynix type.
--
-- The numbering matches the
-- [`SymbolKind`](https://microsoft.github.io/language-server-protocol/specification#symbolKind)
-- enum so callers can hand the value straight to the editor.
kindForType :: Type -> Int
kindForType ty =
  case ty of
    TFun{} -> 12
    TRecord{} -> 19
    TLit (LString _) -> 15
    TLit (LInt _) -> 16
    TLit (LFloat _) -> 16
    TLit (LBool _) -> 17
    _ -> 13

-- | Build a `Location` value from a `(line, startChar, endChar)` triple.
--
-- Thin convenience over `Server.location` so call sites don't have to
-- destructure the tuple every time.
locationFromRange :: FilePath -> (Int, Int, Int) -> Value
locationFromRange file (lineNo, startChar, endChar) = location file lineNo startChar endChar
