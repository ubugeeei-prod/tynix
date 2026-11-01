{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Testable document-session helpers for the tynix language server.
--
-- The executable keeps only the stdio loop and an 'IORef' cache. All document
-- lifecycle behavior lives here so specs can exercise the same update/hover
-- logic that the real server uses.
module Session
  ( Documents,
    closeDocuments,
    codeActionsDocument,
    completionDocument,
    completionResolveDocument,
    definitionDocument,
    documentDiagnostics,
    documentSymbolsHierarchicalDocument,
    prepareRenameDocument,
    pullDiagnosticsDocument,
    selectionRangeDocument,
    storeDocumentAnalysis,
    updateDocumentText,
    documentHighlightsDocument,
    documentLinksDocument,
    documentsFromList,
    documentSymbolsDocument,
    foldingRangesDocument,
    formattingDocument,
    hoverDocument,
    inlayHintsDocument,
    lookupDocumentText,
    referencesDocument,
    renameDocument,
    semanticTokensDocument,
    signatureHelpDocument,
    updateDocuments,
    workspaceSymbolsDocument,
  )
where

import Check qualified
import Control.Applicative ((<|>))
import Control.Exception (IOException, try)
import Control.Monad (forM, (>=>))
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (isPrefixOf, isSuffixOf, nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Driver (Analysis (..), parseText)
import Pretty (renderProgram, renderScheme)
import Server
  ( asInt,
    asText,
    diag,
    documentHighlight,
    field,
    hoverResult,
    location,
    pathUri,
    signatureHelpResult,
    uriPath,
  )
import SessionCompletion
import SessionDiagnostics
  ( closestCandidate,
    diagnosticPayloads,
    diagnosticRange,
    diagnosticSymbolName,
    directiveActions,
    quickFixAction,
    textEdit,
    workspaceEdit,
  )
import SessionDocuments
  ( closeDocuments,
    documentsFromList,
    loadDocumentAnalysis,
    loadDocumentContent,
    loadWorkspaceDocuments,
    lookupDocumentText,
    storeDocumentAnalysis,
    updateDocumentText,
    updateDocuments,
    workspaceSeedFile,
  )
import SessionFolding (encodeFoldingRanges, foldingRangesFor)
import SessionHover (hoverAt, signatureHelpAt)
import SessionInlayHints (encodeInlayHints, inlayHintsFor)
import SessionLinks (encodeDocumentLinks, findDocumentLinks)
import SessionNavigation
import SessionPublish
import SessionReferences
  ( resolveDefinitionLocation,
    resolveReferenceTarget,
    symbolRanges,
    workspaceDocumentsForTarget,
  )
import SessionRequest
import SessionResolve
import SessionScan
import SessionSemanticTokens (encodeSemanticTokens, semanticTokensForScan)
import SessionSymbols
  ( documentCandidateNames,
    documentIndexedSymbols,
    indexedSymbolInformation,
    workspaceIndexedSymbols,
  )
import SessionTypes
  ( Documents (..),
    IndexedSymbol (..),
    ReferenceTarget (..),
    SemanticToken (..),
    WorkspaceDocument (..),
  )
import SessionWorkspace (findBuiltinsFile, workspaceFilesFor)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getHomeDirectory,
    listDirectory,
  )
import System.FilePath (isAbsolute, normalise, takeDirectory, (</>))

-- | Compute hover information for the requested position.
--
-- Hover prefers the cached document text so editors see immediate results after
-- unsaved edits. When the file is not cached yet, the helper falls back to
-- disk and renders a readable error when loading fails.
legacyHoverDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
legacyHoverDocument readDocument analyze docs msg = do
  let params = field "params" msg
      position = params >>= field "position"
      file = documentFile msg
      (lineNo, charNo) = positionOf position
  contentResult <- loadDocumentContent readDocument docs file
  case contentResult of
    Left err -> pure (hoverResult (Left err) "" lineNo charNo)
    Right content -> do
      result <- loadDocumentAnalysis readDocument analyze docs file
      pure (hoverResult result content lineNo charNo)

-- | Compute signature help for the requested position.
--
-- Reuses the cached analysis to render the parameters of the function being
-- applied at the cursor. Mirrors 'hoverDocument' so unsaved edits are honored.
legacySignatureHelpDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
legacySignatureHelpDocument readDocument analyze docs msg = do
  let params = field "params" msg
      position = params >>= field "position"
      file = documentFile msg
      (lineNo, charNo) = positionOf position
  contentResult <- loadDocumentContent readDocument docs file
  case contentResult of
    Left _ -> pure Null
    Right content -> do
      result <- loadDocumentAnalysis readDocument analyze docs file
      pure (signatureHelpResult result content lineNo charNo)

-- | Format a whole document by re-rendering its parsed program.
--
-- Formatting is intentionally conservative so it can never corrupt source: it
-- is a no-op (returns no edits) when the document contains comments — which the
-- AST does not preserve — when it fails to parse, when re-rendering does not
-- round-trip to the same program, or when it is already formatted.
formattingDocument ::
  (FilePath -> IO (Either String Text)) ->
  Documents ->
  Value ->
  IO Value
formattingDocument readDocument docs msg = do
  let file = documentFile msg
  contentResult <- loadDocumentContent readDocument docs file
  pure $ case contentResult of
    Left _ -> emptyEdits
    Right content -> formattingEdits file content

emptyEdits :: Value
emptyEdits = toJSON ([] :: [Value])

formattingEdits :: FilePath -> Text -> Value
formattingEdits file content
  | hasComment content = emptyEdits
  | otherwise =
      case parseText file content of
        Left _ -> emptyEdits
        Right program ->
          let formatted = renderProgram program <> "\n"
           in if formatted == content
                then emptyEdits
                else case parseText file formatted of
                  Right reparsed | reparsed == program -> toJSON [fullDocumentEdit content formatted]
                  _ -> emptyEdits

-- | Whether the document contains comment syntax. Conservative: a @#@ or @/*@
-- inside a string literal also disables formatting, which is safe (no-op).
hasComment :: Text -> Bool
hasComment content = "#" `Text.isInfixOf` content || "/*" `Text.isInfixOf` content

-- | A single edit replacing the entire document. The end position uses a line
-- past the last so clients clamp it to the true document end.
fullDocumentEdit :: Text -> Text -> Value
fullDocumentEdit content newText =
  object
    [ "range"
        .= object
          [ "start" .= object ["line" .= (0 :: Int), "character" .= (0 :: Int)],
            "end"
              .= object ["line" .= (length (Text.lines content) + 1), "character" .= (0 :: Int)]
          ],
      "newText" .= newText
    ]

-- | Resolve a definition/declaration jump for the requested position.
--
-- Top-level names resolve in the current buffer first and then fall back to a
-- workspace-wide symbol index. Dotted selections additionally search field
-- declarations so ambient APIs such as `builtins.map` and local attrset fields
-- behave like editor users expect.
legacyDefinitionDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
legacyDefinitionDocument readDocument analyze docs msg = do
  (file, lineNo, charNo, contentResult) <- requestDocument readDocument docs msg
  case contentResult of
    Left _ -> pure Null
    Right content -> do
      result <- loadDocumentAnalysis readDocument analyze docs file
      workspace <- loadWorkspaceDocuments readDocument analyze docs file
      builtinsFile <- findBuiltinsFile file
      pure $
        maybe
          Null
          ( \(targetFile, targetLine, startChar, endChar) -> location targetFile targetLine startChar endChar
          )
          ( resolveDefinitionLocation
              file
              content
              workspace
              builtinsFile
              result
              lineNo
              charNo
          )

-- | Compute references for the selected symbol.
--
-- Local names stay scoped to the active buffer, while dotted members search the
-- workspace so shared ambient surfaces and record-field APIs are discoverable.
legacyReferencesDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
legacyReferencesDocument readDocument analyze docs msg = do
  (file, lineNo, charNo, contentResult) <- requestDocument readDocument docs msg
  case contentResult of
    Left _ -> pure (toJSON ([] :: [Value]))
    Right content -> do
      result <- loadDocumentAnalysis readDocument analyze docs file
      workspace <- loadWorkspaceDocuments readDocument analyze docs file
      builtinsFile <- findBuiltinsFile file
      pure . toJSON $
        case resolveReferenceTarget file content workspace builtinsFile result lineNo charNo of
          Nothing -> []
          Just target ->
            [ location path foundLine startChar endChar
            | doc <- workspaceDocumentsForTarget workspace target,
              let path = workspaceDocumentFile doc,
              (foundLine, startChar, endChar) <-
                symbolRanges
                  (workspaceDocumentContent doc)
                  (referenceTargetNeedle target)
                  (referenceTargetMode target)
            ]

-- | Highlight occurrences of the selected symbol inside the active buffer.
--
-- Unlike 'referencesDocument', the response is scoped to the current document
-- so editors can paint quick same-file occurrences without paying for the
-- workspace-wide scan. The symbol resolution itself still goes through the
-- shared reference machinery so dotted selections (e.g. @record.foo@) light
-- up the same matches we would jump or rename to.
legacyDocumentHighlightsDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
legacyDocumentHighlightsDocument readDocument analyze docs msg = do
  (file, lineNo, charNo, contentResult) <- requestDocument readDocument docs msg
  case contentResult of
    Left _ -> pure (toJSON ([] :: [Value]))
    Right content -> do
      result <- loadDocumentAnalysis readDocument analyze docs file
      workspace <- loadWorkspaceDocuments readDocument analyze docs file
      builtinsFile <- findBuiltinsFile file
      pure . toJSON $
        case resolveReferenceTarget file content workspace builtinsFile result lineNo charNo of
          Nothing -> []
          Just target ->
            [ documentHighlight foundLine startChar endChar
            | (foundLine, startChar, endChar) <-
                symbolRanges content (referenceTargetNeedle target) (referenceTargetMode target)
            ]

-- | Produce a workspace edit that renames the selected symbol.
--
-- The rename strategy mirrors 'referencesDocument': plain local names stay in
-- one file, while member-style names update dotted usages plus declaration
-- sites across the workspace.
legacyRenameDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
legacyRenameDocument readDocument analyze docs msg = do
  (file, lineNo, charNo, contentResult) <- requestDocument readDocument docs msg
  case (contentResult, field "params" msg >>= field "newName" >>= asText) of
    (Right content, Just newName)
      | not (Text.null newName) -> do
          result <- loadDocumentAnalysis readDocument analyze docs file
          workspace <- loadWorkspaceDocuments readDocument analyze docs file
          builtinsFile <- findBuiltinsFile file
          pure $
            case resolveReferenceTarget file content workspace builtinsFile result lineNo charNo of
              Nothing -> Null
              Just target ->
                let edits =
                      [ ( path,
                          map
                            (\(foundLine, startChar, endChar) -> textEdit foundLine startChar endChar newName)
                            ranges
                        )
                      | doc <- workspaceDocumentsForTarget workspace target,
                        let path = workspaceDocumentFile doc,
                        let ranges =
                              symbolRanges
                                (workspaceDocumentContent doc)
                                (referenceTargetNeedle target)
                                (referenceTargetMode target),
                        not (null ranges)
                      ]
                 in workspaceEdit edits
    _ -> pure Null

-- | List document symbols in a flat, editor-friendly shape.
documentSymbolsDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
documentSymbolsDocument readDocument analyze docs msg = do
  (file, _, _, contentResult) <- requestDocument readDocument docs msg
  case contentResult of
    Left _ -> pure (toJSON ([] :: [Value]))
    Right content -> do
      result <- loadDocumentAnalysis readDocument analyze docs file
      pure
        ( toJSON
            (map indexedSymbolInformation (documentIndexedSymbols file content result))
        )

-- | Search symbols across the surrounding workspace.
workspaceSymbolsDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
workspaceSymbolsDocument readDocument analyze docs msg =
  case workspaceSeedFile docs of
    Nothing -> pure (toJSON ([] :: [Value]))
    Just seedFile -> do
      workspace <- loadWorkspaceDocuments readDocument analyze docs seedFile
      let query =
            Text.toCaseFold (fromMaybe "" (field "params" msg >>= field "query" >>= asText))
          matches symbol =
            Text.null query
              || query `Text.isInfixOf` Text.toCaseFold (indexedSymbolName symbol)
              || maybe
                False
                ((query `Text.isInfixOf`) . Text.toCaseFold)
                (indexedSymbolContainer symbol)
          symbols = take 200 (filter matches (workspaceIndexedSymbols workspace))
      pure (toJSON (map indexedSymbolInformation symbols))

-- | Offer quick fixes for current diagnostics.
--
-- The server surfaces lightweight escape hatches (`@tynix-ignore`,
-- `@tynix-expected`) and, for obvious misspellings, a rename replacement based
-- on nearby in-scope symbol names.
legacyCodeActionsDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
legacyCodeActionsDocument readDocument analyze docs msg = do
  (file, _, _, contentResult) <- requestDocument readDocument docs msg
  case contentResult of
    Left _ -> pure (toJSON ([] :: [Value]))
    Right content -> do
      result <- loadDocumentAnalysis readDocument analyze docs file
      workspace <- loadWorkspaceDocuments readDocument analyze docs file
      let diagnostics = diagnosticPayloads msg
          candidates =
            nub
              ( documentCandidateNames result
                  <> map indexedSymbolName (workspaceIndexedSymbols workspace)
                  <> ["builtins", "import"]
              )
          actionsFor diagnostic =
            directiveActions file content diagnostic <> renameFix diagnostic
          renameFix diagnostic =
            case suggestion diagnostic of
              Just ((lineNo, startChar, endChar), current, replacement)
                | current /= replacement ->
                    [ quickFixAction
                        ("Replace with `" <> replacement <> "`")
                        file
                        [textEdit lineNo startChar endChar replacement]
                    ]
              _ -> []
          suggestion diagnostic = do
            range <- diagnosticRange diagnostic
            let message = fromMaybe "" (field "message" diagnostic >>= asText)
            current <- diagnosticSymbolName message
            replacement <- closestCandidate candidates current
            pure (range, current, replacement)
          actions = concatMap actionsFor diagnostics
      pure (toJSON actions)

-- | Return LSP folding ranges for one document.
--
-- The provider is text-driven so it keeps emitting folds even when the buffer
-- has type errors or fails to parse cleanly.
foldingRangesDocument ::
  (FilePath -> IO (Either String Text)) ->
  Documents ->
  Value ->
  IO Value
foldingRangesDocument readDocument docs msg = do
  let file = documentFile msg
  contentResult <- loadDocumentContent readDocument docs file
  pure $ case contentResult of
    Left _ -> toJSON ([] :: [Value])
    Right content -> toJSON (encodeFoldingRanges (foldingRangesFor content))

-- | Surface every @import@ / @declare@ target inside the document as a
-- clickable LSP DocumentLink.
--
-- The scan is text-driven (see 'SessionLinks.findDocumentLinks') and
-- resolves relative paths against the source file's directory so editors
-- can jump straight to the referenced @.nix@ / @.tynix@ / @.d.tynix@ file.
documentLinksDocument ::
  (FilePath -> IO (Either String Text)) ->
  Documents ->
  Value ->
  IO Value
documentLinksDocument readDocument docs msg = do
  let file = documentFile msg
  contentResult <- loadDocumentContent readDocument docs file
  pure $ case contentResult of
    Left _ -> toJSON ([] :: [Value])
    Right content ->
      let baseDir = takeDirectory file
       in toJSON (encodeDocumentLinks baseDir (findDocumentLinks content))

-- | Surface inferred-type inlay hints for unannotated top-level @let@
-- bindings.
--
-- The hint position is the UTF-16 column right after the bound name, and
-- the label is @:: Scheme@ rendered through the shared 'Pretty' helpers
-- so editors render the same text the hover already shows.
inlayHintsDocument ::
  (FilePath -> IO (Either String Text)) ->
  (FilePath -> Text -> IO (Either String Analysis)) ->
  Documents ->
  Value ->
  IO Value
inlayHintsDocument readDocument analyze docs msg = do
  let file = documentFile msg
  contentResult <- loadDocumentContent readDocument docs file
  case contentResult of
    Left _ -> pure (toJSON ([] :: [Value]))
    Right content -> do
      result <- loadDocumentAnalysis readDocument analyze docs file
      pure (toJSON (encodeInlayHints (inlayHintsFor content result)))

-- Declaration index ----------------------------------------------------------------

-- | Every documented @declare@ entry and alias around a document.
data DeclarationIndex = DeclarationIndex
  { declEntries :: [(FilePath, Scan, AmbientDoc)],
    declAliasDocs :: Map.Map Text Text
  }

loadDeclarationIndex ::
  DocumentReader -> Documents -> FilePath -> Text -> IO DeclarationIndex
loadDeclarationIndex readDocument docs file content = do
  builtinsFile <- findBuiltinsFile file
  files <- workspaceFilesFor file
  let declFiles =
        take
          200
          (nub (maybe [] pure builtinsFile <> filter (".d.tynix" `isSuffixOf`) files))
  scans <- forM (filter (/= file) declFiles) $ \path -> do
    loaded <- loadDocumentContent readDocument docs path
    pure (either (const Nothing) (\text -> Just (path, scanDocument text)) loaded)
  let own = (file, scanDocument content)
      allScans = own : catMaybes scans
  pure
    DeclarationIndex
      { declEntries = [(path, sc, d) | (path, sc) <- allScans, d <- ambientDocs sc],
        declAliasDocs =
          Map.fromList
            [ (name, doc)
            | (_, sc) <- catMaybes scans,
              (name, (doc, _)) <- Map.toList (aliasDocs sc)
            ]
      }

completionEnvOf :: DeclarationIndex -> CompletionEnv
completionEnvOf index =
  CompletionEnv
    { envBuiltinDocs =
        Map.fromList
          [ (ambientDocName d, d)
          | (_, _, d) <- declEntries index,
            ambientDocTarget d == "builtins"
          ],
      envAmbientDocs =
        Map.fromListWith
          (flip (<>))
          [ (Check.resolvePath path (Text.unpack (ambientDocTarget d)), [d])
          | (path, _, d) <- declEntries index,
            ambientDocTarget d /= "builtins"
          ],
      envAliasDocs = declAliasDocs index
    }

loadEnv :: DocumentReader -> Documents -> Request -> IO CompletionEnv
loadEnv readDocument docs r =
  completionEnvOf
    <$> loadDeclarationIndex readDocument docs (requestFile r) (requestContent r)

-- Completion -------------------------------------------------------------------------

emptyCompletionList :: Value
emptyCompletionList = object ["isIncomplete" .= False, "items" .= ([] :: [Value])]

-- | Context-aware completion (see 'SessionCompletion').
completionDocument ::
  DocumentReader -> DocumentAnalyzer -> Documents -> Value -> IO Value
completionDocument readDocument analyze docs msg = do
  loaded <- loadRequest readDocument analyze docs msg
  case loaded of
    Left _ -> pure emptyCompletionList
    Right r -> do
      env <- loadEnv readDocument docs r
      completionFor env r

completionFor :: CompletionEnv -> Request -> IO Value
completionFor env r =
  case completeAt env (requestContext r) (requestOffset r) of
    Items range items ->
      pure
        (encodeCompletionList (slice range) (lineRangeOf scan range) dataPairs items)
    PathEntries dir partial range -> do
      base <- resolveCompletionDir (requestFile r) dir
      entries <- listEntries base
      pure
        ( encodeCompletionList
            partial
            (lineRangeOf scan range)
            []
            (pathCompletionItems partial entries)
        )
  where
    scan = requestScan r
    slice (s, e) = Text.take (e - s) (Text.drop s (requestContent r))
    (lineNo, charNo) = offsetToPosition (scanLineIndex scan) (requestOffset r)
    dataPairs = ["uri" .= pathUri (requestFile r), "line" .= lineNo, "character" .= charNo]

resolveCompletionDir :: FilePath -> FilePath -> IO FilePath
resolveCompletionDir file dir
  | "~/" `isPrefixOf` dir = (</> drop 2 dir) <$> getHomeDirectory
  | isAbsolute dir = pure dir
  | otherwise = pure (takeDirectory file </> dir)

listEntries :: FilePath -> IO [(FilePath, Bool)]
listEntries dir = do
  listed <- try @IOException (listDirectory dir)
  case listed of
    Left _ -> pure []
    Right names -> forM (take 500 (sortOn id names)) $ \name -> do
      isDir <- doesDirectoryExist (dir </> name)
      pure (name, isDir)

-- | @completionItem/resolve@: recompute the item at its recorded position to
-- attach documentation that large lists omit.
completionResolveDocument ::
  DocumentReader -> DocumentAnalyzer -> Documents -> Value -> IO Value
completionResolveDocument readDocument analyze docs msg = do
  let itemValue = fromMaybe Null (field "params" msg)
      dat = field "data" itemValue
      file = maybe "" (normalise . uriPath) (dat >>= field "uri" >>= asText)
      lineNo = maybe 0 asInt (dat >>= field "line")
      charNo = maybe 0 asInt (dat >>= field "character")
      label = fromMaybe "" (field "label" itemValue >>= asText)
  case (itemValue, dat) of
    (Object obj, Just _) | not (KeyMap.member "documentation" obj) -> do
      loaded <- loadRequestAt readDocument analyze docs file lineNo charNo
      case loaded of
        Left _ -> pure itemValue
        Right r -> do
          env <- loadEnv readDocument docs r
          pure $ case completeAt env (requestContext r) (requestOffset r) of
            Items _ items
              | it : _ <- filter ((== label) . itemLabel) items,
                Just doc <- itemDoc it ->
                  Object
                    ( KeyMap.insert
                        "documentation"
                        (object ["kind" .= ("markdown" :: Text), "value" .= doc])
                        obj
                    )
            _ -> itemValue
    _ -> pure itemValue

-- Hover / signature help ---------------------------------------------------------------

hoverDocument ::
  DocumentReader -> DocumentAnalyzer -> Documents -> Value -> IO Value
hoverDocument readDocument analyze docs msg = do
  loaded <- loadRequest readDocument analyze docs msg
  case loaded of
    Left _ -> legacyHoverDocument readDocument analyze docs msg
    Right r -> do
      env <- loadEnv readDocument docs r
      case hoverAt env (requestContext r) (requestOffset r) of
        Just (markdown, range) ->
          pure $
            object
              [ "contents" .= object ["kind" .= ("markdown" :: Text), "value" .= markdown],
                "range" .= rangeValueOf (requestScan r) range
              ]
        Nothing -> legacyHoverDocument readDocument analyze docs msg

signatureHelpDocument ::
  DocumentReader -> DocumentAnalyzer -> Documents -> Value -> IO Value
signatureHelpDocument readDocument analyze docs msg = do
  loaded <- loadRequest readDocument analyze docs msg
  case loaded of
    Left _ -> pure Null
    Right r -> do
      env <- loadEnv readDocument docs r
      case signatureHelpAt env (requestContext r) (requestOffset r) of
        Null -> legacySignatureHelpDocument readDocument analyze docs msg
        help -> pure help

-- Definition / references / rename ---------------------------------------------------

definitionDocument ::
  DocumentReader -> DocumentAnalyzer -> Documents -> Value -> IO Value
definitionDocument readDocument analyze docs msg = do
  loaded <- loadRequest readDocument analyze docs msg
  case loaded of
    Left _ -> legacyDefinitionDocument readDocument analyze docs msg
    Right r -> do
      let scan = requestScan r
      case localSymbolAt scan (requestOffset r) of
        Just sym -> pure (locationOf (requestFile r) scan (declarationRange sym))
        Nothing -> do
          target <-
            firstLocation [pathDefinition r, importedMemberDefinition readDocument docs r]
          maybe (legacyDefinitionDocument readDocument analyze docs msg) pure target
  where
    firstLocation [] = pure Nothing
    firstLocation (candidate : rest) = do
      target <- candidate
      maybe (firstLocation rest) (pure . Just) target

declarationRange :: LocalSymbol -> (Int, Int)
declarationRange sym = case sym of
  LocalValue b -> (binderNameStart b, binderNameEnd b)
  LocalType b -> (binderNameStart b, binderNameEnd b)

-- | Jump from a path literal to the file (or its @default.nix@).
pathDefinition :: Request -> IO (Maybe Value)
pathDefinition r =
  case codeTokenIndexAt scan (requestOffset r) >>= codeToken scan of
    Just t
      | tokKind t == KPath,
        any (`Text.isPrefixOf` tokText t) ["./", "../", "/"] -> do
          let target = Check.resolvePath (requestFile r) (Text.unpack (tokText t))
          isDir <- doesDirectoryExist target
          let candidate = if isDir then target </> "default.nix" else target
          exists <- doesFileExist candidate
          pure $
            if exists
              then
                Just
                  ( object
                      [ "uri" .= pathUri candidate,
                        "range" .= object ["start" .= zeroPos, "end" .= zeroPos]
                      ]
                  )
              else Nothing
    _ -> pure Nothing
  where
    scan = requestScan r
    zeroPos = object ["line" .= (0 :: Int), "character" .= (0 :: Int)]

-- | @m.name@ where @m = import ./file.nix@: jump to the @declare@ entry that
-- types it, or to the binding in the imported file.
importedMemberDefinition ::
  DocumentReader -> Documents -> Request -> IO (Maybe Value)
importedMemberDefinition readDocument docs r =
  case importedTarget of
    Nothing -> pure Nothing
    Just (target, name) -> do
      index <-
        loadDeclarationIndex readDocument docs (requestFile r) (requestContent r)
      case [ locationOf path sc (ambientDocOffset d, ambientDocOffset d + Text.length name)
           | (path, sc, d) <- declEntries index,
             ambientDocName d == name,
             ambientDocTarget d /= "builtins",
             Check.resolvePath path (Text.unpack (ambientDocTarget d)) == target
           ] of
        loc : _ -> pure (Just loc)
        [] -> do
          loaded <- loadDocumentContent readDocument docs target
          pure $ case loaded of
            Right text ->
              let sc = scanDocument text
               in case [ (tokStart t, tokEnd t)
                       | i <- [0 .. codeTokenCount sc - 1],
                         Just t <- [codeToken sc i],
                         tokKind t == KIdent,
                         tokText t == name,
                         maybe False ((== "=") . tokText) (codeToken sc (i + 1))
                       ] of
                    range : _ -> Just (locationOf target sc range)
                    [] -> Nothing
            Left _ -> Nothing
  where
    scan = requestScan r
    importedTarget = do
      i <- codeTokenIndexAt scan (requestOffset r)
      t <- codeToken scan i
      dot <- codeToken scan (i - 1)
      headTok <- codeToken scan (i - 2)
      if tokText dot == "." && tokKind headTok == KIdent then Just () else Nothing
      b <- resolveNameAt scan (tokText headTok) (tokStart headTok)
      (from, _) <- binderValue b
      importTok <- codeToken scan from
      pathTok <- codeToken scan (from + 1)
      if tokText importTok == "import" && tokKind pathTok == KPath
        then
          Just
            (Check.resolvePath (requestFile r) (Text.unpack (tokText pathTok)), tokText t)
        else Nothing

referencesDocument ::
  DocumentReader -> DocumentAnalyzer -> Documents -> Value -> IO Value
referencesDocument readDocument analyze docs msg = do
  loaded <- loadRequest readDocument analyze docs msg
  case loaded of
    Right r
      | Just sym <- localSymbolAt (requestScan r) (requestOffset r) -> do
          let includeDecl =
                fromMaybe
                  True
                  (field "params" msg >>= field "context" >>= field "includeDeclaration" >>= asBool)
              decl = declarationRange sym
              ranges =
                [ range
                | range <- localOccurrences (requestScan r) sym,
                  includeDecl || range /= decl
                ]
          pure (toJSON (map (locationOf (requestFile r) (requestScan r)) ranges))
    _ -> legacyReferencesDocument readDocument analyze docs msg
  where
    asBool (Bool b) = Just b
    asBool _ = Nothing

documentHighlightsDocument ::
  DocumentReader -> DocumentAnalyzer -> Documents -> Value -> IO Value
documentHighlightsDocument readDocument analyze docs msg = do
  loaded <- loadRequest readDocument analyze docs msg
  case loaded of
    Right r
      | Just sym <- localSymbolAt (requestScan r) (requestOffset r) ->
          pure . toJSON $
            [ object ["range" .= rangeValueOf (requestScan r) range, "kind" .= (1 :: Int)]
            | range <- localOccurrences (requestScan r) sym
            ]
    _ -> legacyDocumentHighlightsDocument readDocument analyze docs msg

renameDocument ::
  DocumentReader -> DocumentAnalyzer -> Documents -> Value -> IO Value
renameDocument readDocument analyze docs msg = do
  loaded <- loadRequest readDocument analyze docs msg
  let newName = fromMaybe "" (field "params" msg >>= field "newName" >>= asText)
  case loaded of
    Right r
      | validIdentifier newName,
        Just sym <- localSymbolAt (requestScan r) (requestOffset r) ->
          pure
            ( workspaceEdit
                [ ( requestFile r,
                    [ editOf (requestScan r) range newName
                    | range <- localOccurrences (requestScan r) sym
                    ]
                  )
                ]
            )
    _ -> legacyRenameDocument readDocument analyze docs msg

validIdentifier :: Text -> Bool
validIdentifier name = case Text.uncons name of
  Just (c, rest) ->
    (c == '_' || c `elem` ['a' .. 'z'] || c `elem` ['A' .. 'Z'])
      && Text.all
        ( \x ->
            x `elem` ("_'-" :: String)
              || x `elem` ['a' .. 'z']
              || x `elem` ['A' .. 'Z']
              || x `elem` ['0' .. '9']
        )
        rest
      && name
        `notElem` [ "let",
                    "in",
                    "if",
                    "then",
                    "else",
                    "assert",
                    "with",
                    "rec",
                    "inherit",
                    "or",
                    "true",
                    "false",
                    "null"
                  ]
  Nothing -> False

-- | @textDocument/prepareRename@: the identifier range and its text, or
-- 'Null' when the cursor is not on something renameable.
prepareRenameDocument ::
  DocumentReader -> DocumentAnalyzer -> Documents -> Value -> IO Value
prepareRenameDocument readDocument analyze docs msg = do
  loaded <- loadRequest readDocument analyze docs msg
  pure $ case loaded of
    Right r
      | Just (range, current) <- renameableAt (requestScan r) (requestOffset r) ->
          object ["range" .= rangeValueOf (requestScan r) range, "placeholder" .= current]
    _ -> Null

-- Symbols / selection --------------------------------------------------------------------

-- | Hierarchical @DocumentSymbol[]@ outline.
documentSymbolsHierarchicalDocument ::
  DocumentReader -> DocumentAnalyzer -> Documents -> Value -> IO Value
documentSymbolsHierarchicalDocument readDocument analyze docs msg = do
  loaded <- loadRequest readDocument analyze docs msg
  pure $ case loaded of
    Right r ->
      toJSON
        (encodeSymbolTree (requestScan r) (documentSymbolTree (requestContext r)))
    Left _ -> toJSON ([] :: [Value])

selectionRangeDocument :: DocumentReader -> Documents -> Value -> IO Value
selectionRangeDocument readDocument docs msg = do
  let file = documentFile msg
      positions = case field "params" msg >>= field "positions" of
        Just (Array xs) -> foldr (:) [] xs
        _ -> []
  loaded <- loadDocumentContent readDocument docs file
  pure $ case loaded of
    Left _ -> toJSON ([] :: [Value])
    Right content ->
      let scan = scanDocument content
          toOffset p = positionToOffset (scanLineIndex scan) (positionOf (Just p))
          build [] = Null
          build (range : outer) =
            object $
              ["range" .= rangeValueOf scan range]
                <> ["parent" .= build outer | not (null outer)]
       in toJSON [build (selectionRangesAt scan (toOffset p)) | p <- positions]

-- Semantic tokens ---------------------------------------------------------------------

semanticTokensDocument ::
  DocumentReader -> DocumentAnalyzer -> Documents -> Value -> IO Value
semanticTokensDocument readDocument analyze docs msg = do
  loaded <- loadRequest readDocument analyze docs msg
  pure $ case loaded of
    Left _ -> object ["data" .= ([] :: [Int])]
    Right r ->
      let tokens = semanticTokensForScan (requestContext r)
          inRange = case field "params" msg >>= field "range" of
            Just range ->
              let startLine = maybe 0 asInt (field "start" range >>= field "line")
                  endLine = maybe maxBound asInt (field "end" range >>= field "line")
               in filter
                    (\t -> semanticTokenLine t >= startLine && semanticTokenLine t <= endLine)
                    tokens
            Nothing -> tokens
       in object ["data" .= encodeSemanticTokens inRange]

-- Diagnostics -------------------------------------------------------------------------

-- | Full diagnostic list for a document: the analysis error (span-accurate,
-- coded, with related information) plus lint hints.
--
-- When an error message carries no location, the binding value that causes
-- it is found by re-checking with each candidate value (smallest first)
-- replaced by an @any@-typed placeholder. Once the core reports @line:col@ for
-- every checker error this step never runs.
documentDiagnostics ::
  DocumentReader ->
  DocumentAnalyzer ->
  Documents ->
  FilePath ->
  Text ->
  Either String Analysis ->
  IO [Value]
documentDiagnostics readDocument analyze docs file content result = do
  index <- loadDeclarationIndex readDocument docs file content
  let env = completionEnvOf index
      ctx = mkCtx file content result
      scan = ctxScan ctx
  localized <- case result of
    Left err
      | Nothing <- errorRange scan (Text.pack err),
        maybe True (not . ("TP" `Text.isPrefixOf`)) (errorCode (Text.pack err)) ->
          localize scan err (localizationCandidates scan)
    _ -> pure Nothing
  pure
    ( diagnosticValues
        file
        scan
        (analysisDiagnostics ctx result localized <> lintDiagnostics env ctx)
    )
  where
    localize _ _ [] = pure Nothing
    localize scan err (candidate : rest) = do
      probe <- analyze file (replaceWithDynamic scan candidate)
      let suppressed = case probe of
            Right _ -> True
            Left other ->
              other /= err
                && maybe True (not . ("TP" `Text.isPrefixOf`)) (errorCode (Text.pack other))
      if suppressed
        then pure (Just candidate)
        else localize scan err rest

-- | Pull-model @textDocument/diagnostic@ report. Always analyses the current
-- text (never the last good analysis).
pullDiagnosticsDocument ::
  DocumentReader -> DocumentAnalyzer -> Documents -> Value -> IO Value
pullDiagnosticsDocument readDocument analyze docs msg = do
  let file = documentFile msg
  contentResult <- loadDocumentContent readDocument docs file
  case contentResult of
    Left err -> pure (object ["kind" .= ("full" :: Text), "items" .= [diag err]])
    Right content -> do
      result <- analyze file content
      items <- documentDiagnostics readDocument analyze docs file content result
      pure (object ["kind" .= ("full" :: Text), "items" .= items])

-- Code actions ---------------------------------------------------------------------------

-- | Quick fixes and refactorings.
--
-- On top of the directive escape hatches and the legacy rename fix this
-- offers scope-aware "did you mean" replacements for unbound names and
-- missing fields, "add missing field" for local attrset literals, removal or
-- @_@-prefixing of unused bindings, and "add type signature" for
-- unannotated @let@ bindings whose type is known.
codeActionsDocument ::
  DocumentReader -> DocumentAnalyzer -> Documents -> Value -> IO Value
codeActionsDocument readDocument analyze docs msg = do
  legacy <- legacyCodeActionsDocument readDocument analyze docs msg
  let params = field "params" msg
      start = params >>= field "range" >>= field "start"
      (lineNo, charNo) = positionOf start
      file = documentFile msg
  loaded <- loadRequestAt readDocument analyze docs file lineNo charNo
  pure $ case (legacy, loaded) of
    (Array existing, Right r) ->
      let legacyActions = foldr (:) [] existing
          titles = mapMaybe (field "title" >=> asText) legacyActions
          fieldDiagnostic =
            any
              (\d -> (field "code" d >>= asText) == Just "TC0009")
              (diagnosticPayloads msg)
          keptLegacy =
            [ a
            | a <- legacyActions,
              not
                ( fieldDiagnostic
                    && maybe False ("Replace with `" `Text.isPrefixOf`) (field "title" a >>= asText)
                )
            ]
          renamed = [t | t <- titles, "Replace with `" `Text.isPrefixOf` t, not fieldDiagnostic]
          extra = richCodeActions r (diagnosticPayloads msg)
          fresh =
            [ a
            | a <- extra,
              maybe
                True
                (\t -> t `notElem` titles && not (duplicateSuggestion renamed t))
                (field "title" a >>= asText)
            ]
       in toJSON (keptLegacy <> fresh)
    _ -> legacy
  where
    duplicateSuggestion renamed title =
      any
        ( \t ->
            Text.drop (Text.length "Replace with ") t
              == Text.dropEnd 1 (Text.drop (Text.length "Did you mean ") title)
        )
        renamed

richCodeActions :: Request -> [Value] -> [Value]
richCodeActions r diagnostics = concatMap forDiagnostic diagnostics <> signatureActions
  where
    scan = requestScan r
    ctx = requestContext r
    file = requestFile r
    idx = scanLineIndex scan
    blank = Text.all (`elem` (" \t" :: String))
    action :: Text -> Text -> Bool -> Maybe Value -> [Value] -> Value
    action title kind preferred diagnostic edits =
      object $
        [ "title" .= (title :: Text),
          "kind" .= (kind :: Text),
          "edit" .= workspaceEdit [(file, edits)]
        ]
          <> ["isPreferred" .= True | preferred]
          <> maybe [] (\d -> ["diagnostics" .= [d]]) diagnostic
    diagnosticOffsets d = do
      range <- field "range" d
      s <- field "start" range
      e <- field "end" range
      let toOff p = positionToOffset idx (positionOf (Just p))
      pure (toOff s, toOff e)
    forDiagnostic d =
      let message = fromMaybe "" (field "message" d >>= asText)
          code = (field "code" d >>= asText) <|> errorCode message
          offsets = diagnosticOffsets d
       in case (code, offsets) of
            (Just "TL0001", Just (s, _)) -> unusedActions d s
            (Just "TC0001", Just range) -> didYouMean d range message (scopeNames (fst range))
            (Just "TC0009", Just range) ->
              didYouMean d range message (recordFieldNamesInMessage message)
                <> addMissingField d range message
            (_, Just range)
              | "unbound name" `Text.isInfixOf` message ->
                  didYouMean d range message (scopeNames (fst range))
            _ -> []
    scopeNames off =
      map binderName (bindersInScopeAt scan off)
        <> maybe [] (Map.keys . analysisBindings) (ctxAnalysis ctx)
        <> ["builtins", "import"]
    didYouMean d range message candidates =
      case quoted message of
        Just current ->
          [ action
              ("Did you mean `" <> s <> "`?")
              "quickfix"
              (ix == 0)
              (Just d)
              [editOf scan range s]
          | (ix, s) <- zip [0 :: Int ..] (take 3 (suggestNames current candidates))
          ]
        Nothing -> []
    quoted = quotedName
    addMissingField d (s, _) message = fromMaybe [] $ do
      fieldName <- quoted message
      i <- codeTokenIndexAt scan s
      headTok <- codeToken scan (i - 2)
      b <- resolveNameAt scan (tokText headTok) (tokStart headTok)
      (from, _) <- binderValue b
      opener <- codeToken scan from
      if tokText opener == "{" then Just () else Nothing
      closerIx <- matchingCloser scan from
      closer <- codeToken scan closerIx
      let (closerLine, _) = offsetToPosition idx (tokStart closer)
          closerLineStart = positionToOffset idx (closerLine, 0)
          ownLine =
            blank (Text.take (tokStart closer - closerLineStart) (lineText idx closerLine))
          indent = Text.takeWhile (`elem` (" \t" :: String)) (lineText idx closerLine)
          edit
            | ownLine =
                editOf
                  scan
                  (closerLineStart, closerLineStart)
                  (indent <> "  " <> fieldName <> " = null;\n")
            | otherwise =
                editOf scan (tokStart closer, tokStart closer) (fieldName <> " = null; ")
      pure
        [ action
            ("Add missing field `" <> fieldName <> "` to `" <> binderName b <> "`")
            "quickfix"
            False
            (Just d)
            [edit]
        ]
    unusedActions d off = case [b | b <- unusedBinders scan, binderNameStart b == off] of
      b : _ -> case binderKind b of
        BindParam ->
          [ action
              ("Prefix `" <> binderName b <> "` with `_`")
              "quickfix"
              True
              (Just d)
              [editOf scan (binderNameStart b, binderNameStart b) "_"]
          ]
        BindLet ->
          [ action
              ("Remove unused binding `" <> binderName b <> "`")
              "quickfix"
              True
              (Just d)
              (removeBinding b)
          ]
        BindPatternField ->
          [ action
              ("Remove `" <> binderName b <> "` from the pattern")
              "quickfix"
              True
              (Just d)
              [editOf scan (patternFieldRange b) ""]
          ]
        _ -> []
      [] -> []
    removeBinding b =
      [editOf scan (wholeLines (binderDeclStart b) (binderDeclEnd b)) ""]
        <> [editOf scan (wholeLines s e) "" | Just (s, e) <- [binderSignature b]]
    wholeLines s e =
      let (sl, _) = offsetToPosition idx s
          (el, _) = offsetToPosition idx e
          lineStart = positionToOffset idx (sl, 0)
          endLineStart = positionToOffset idx (el, 0)
          before = Text.take (s - lineStart) (lineText idx sl)
          after = Text.drop (e - endLineStart) (lineText idx el)
       in if blank before && blank after
            then (lineStart, endLineStart + Text.length (lineText idx el) + 1)
            else (s, e)
    patternFieldRange b =
      let i = binderToken b
          endIx = maybe (i + 1) snd (binderValue b)
       in case codeToken scan endIx of
            Just t
              | tokText t == "," ->
                  (binderNameStart b, maybe (tokEnd t) tokStart (codeToken scan (endIx + 1)))
            _ -> case codeToken scan (i - 1) of
              Just prev
                | tokText prev == "," ->
                    (tokStart prev, maybe (binderNameEnd b) tokEnd (codeToken scan (endIx - 1)))
              _ -> (binderNameStart b, binderNameEnd b)
    signatureActions =
      [ action
          ("Add type signature `" <> binderName b <> " :: " <> rendered <> "`")
          "refactor.rewrite"
          False
          Nothing
          [ editOf
              scan
              (lineStart, lineStart)
              (indent <> binderName b <> " :: " <> rendered <> ";\n")
          ]
      | b <- scanBinders scan,
        binderKind b == BindLet,
        isNothing (binderSignature b),
        binderNameStart b <= requestOffset r,
        requestOffset r <= binderNameEnd b,
        let (l, _) = offsetToPosition idx (binderDeclStart b)
            lineStart = positionToOffset idx (l, 0)
            indent = Text.takeWhile (`elem` (" \t" :: String)) (lineText idx l),
        blank (Text.take (binderDeclStart b - lineStart) (lineText idx l)),
        Just rendered <- [signatureText b]
      ]
    signatureText b
      | binderRootLevel b,
        Just scheme <- ctxAnalysis ctx >>= Map.lookup (binderName b) . analysisBindings =
          Just (Text.unwords (Text.words (renderScheme scheme)))
      | otherwise = compactType <$> binderType ctx b
