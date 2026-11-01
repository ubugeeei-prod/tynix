{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | JSON-RPC/LSP bridge for tynix.
--
-- The server keeps protocol framing and stdio orchestration here while pushing
-- semantic behavior into the core driver and the testable 'Session' helpers.
--
-- Concurrency model: a reader thread decodes framed messages from stdin into
-- a queue, and a single worker (the main thread) handles them in order. The
-- split buys two things without any locking around the document store:
--
-- * @$/cancelRequest@ is applied by the reader as soon as it arrives, so a
--   request still waiting in the queue is answered with @RequestCancelled@
--   instead of being computed;
-- * @didChange@ only updates the text and schedules a debounced
--   re-analysis: a timer thread enqueues an internal message after a quiet
--   period, and the worker drops it if newer edits arrived meanwhile. Typing
--   bursts therefore cost one analysis, not one per keystroke.
module Main (main) where

import AnalysisCache
  ( AnalysisCache,
    accessAnalysisCache,
    emptyAnalysisCache,
    insertAnalysisCache,
  )
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception
  ( IOException,
    SomeException,
    fromException,
    throwIO,
    try,
  )
import Control.Monad (forM_, void, when)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy.Char8 qualified as LB8
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Version (showVersion)
import Driver
  ( Analysis (..),
    SupportCache,
    analyzeTextForEditorWith,
    newSupportCache,
  )
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Paths_tynix_lsp qualified as PackageInfo
import Server
  ( asText,
    clearDiagnostics,
    field,
    pathUri,
    respond,
    respondError,
    serverCapabilities,
  )
import ServerProtocol (ReadOutcome (..), notify, readMessageOutcome)
import Session qualified
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.IO
  ( BufferMode (LineBuffering, NoBuffering),
    IOMode (AppendMode),
    hPutStrLn,
    hSetBinaryMode,
    hSetBuffering,
    openFile,
    stderr,
    stdin,
    stdout,
  )

-- | Start the stdio event loop and keep the latest document text in memory.
main :: IO ()
main = do
  -- Source files are UTF-8 whatever the locale says (e.g. a C locale).
  setLocaleEncoding utf8
  args <- getArgs
  handleArgs args

handleArgs :: [String] -> IO ()
handleArgs args
  | any (`elem` ["--help", "-h"]) args = putStrLn helpText
  | any (`elem` ["--version", "-v"]) args = putStrLn versionText
  | otherwise = case parseServerArgs args of
      Left err -> do
        hPutStrLn stderr ("tynix-lsp: " <> err)
        hPutStrLn stderr "Use --stdio, --log-file PATH, --version, or --help."
        exitFailure
      Right logFile -> runServer logFile

-- | Parse the supported server flags. Recognizes @--stdio@ (the implicit
-- default) and an optional @--log-file PATH@ / @--log-file=PATH@; anything
-- else is rejected so typos do not silently start a misconfigured server.
parseServerArgs :: [String] -> Either String (Maybe FilePath)
parseServerArgs = go Nothing
  where
    go logFile [] = Right logFile
    go logFile ("--stdio" : rest) = go logFile rest
    go _ ("--log-file" : path : rest) = go (Just path) rest
    go _ ["--log-file"] = Left "--log-file requires a path argument"
    go _ (arg : rest)
      | Just path <- stripPrefix' "--log-file=" arg = go (Just path) rest
      | otherwise = Left ("unsupported argument: " <> arg)
    stripPrefix' prefix value =
      if prefix == take (length prefix) value
        then Just (drop (length prefix) value)
        else Nothing

-- | A logging sink. Diagnostics always reach stderr and, when @--log-file@ is
-- configured, are also appended to that file.
type Logger = Text -> IO ()

-- | Messages consumed by the worker.
data Incoming
  = FromClient Value
  | -- | debounce timer fired for a document at an edit generation
    Debounced FilePath Int
  | EndOfInput

-- | Mutable server state, touched only by the worker thread (except
-- 'stCancelled', which the reader updates).
data ServerState = ServerState
  { stLogger :: Logger,
    stDocs :: IORef Session.Documents,
    stCache :: IORef AnalysisCache,
    stShutdown :: IORef Bool,
    stQueue :: Chan Incoming,
    stCancelled :: IORef (Set Text),
    stGenerations :: IORef (Map FilePath Int),
    stPullDiagnostics :: IORef Bool,
    stHierarchicalSymbols :: IORef Bool
  }

-- | Quiet period before re-analysing after an edit.
debounceMicros :: Int
debounceMicros = 200000

runServer :: Maybe FilePath -> IO ()
runServer logFile = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  logHandle <- traverse openLogHandle logFile
  st <-
    ServerState (makeLogger logHandle)
      <$> newIORef mempty
      <*> newIORef emptyAnalysisCache
      <*> newIORef False
      <*> newChan
      <*> newIORef Set.empty
      <*> newIORef Map.empty
      <*> newIORef False
      <*> newIORef False
  void (forkIO (reader st))
  worker st
  where
    openLogHandle path = do
      h <- openFile path AppendMode
      hSetBuffering h LineBuffering
      pure h
    makeLogger logHandle message = do
      hPutStrLn stderr ("tynix-lsp: " <> T.unpack message)
      case logHandle of
        Just h -> hPutStrLn h ("tynix-lsp: " <> T.unpack message)
        Nothing -> pure ()

-- | Decode messages from stdin. Cancellations are applied immediately.
reader :: ServerState -> IO ()
reader st = do
  outcome <- readMessageOutcome stdin
  case outcome of
    ReadEof -> writeChan (stQueue st) EndOfInput
    ReadError reason -> stLogger st reason >> reader st
    ReadMessage msg -> do
      case field "method" msg >>= asText of
        Just "$/cancelRequest" ->
          forM_ (field "params" msg >>= field "id") $ \ident ->
            atomicModifyIORef' (stCancelled st) (\s -> (Set.insert (idKey ident) s, ()))
        _ -> writeChan (stQueue st) (FromClient msg)
      reader st

idKey :: Value -> Text
idKey = T.pack . LB8.unpack . encode

worker :: ServerState -> IO ()
worker st = do
  incoming <- readChan (stQueue st)
  case incoming of
    EndOfInput -> pure ()
    FromClient msg -> do
      cancelled <- case field "id" msg of
        Just ident -> do
          let key = idKey ident
          atomicModifyIORef' (stCancelled st) (\s -> (Set.delete key s, Set.member key s))
        Nothing -> pure False
      if cancelled
        then respondError stdout msg (-32800) "request cancelled"
        else safeHandle st msg
      worker st
    Debounced file generation -> do
      current <- Map.findWithDefault 0 file <$> readIORef (stGenerations st)
      when (current == generation) $
        guarded st Nothing (analyzeAndPublish st file)
      worker st

-- | Run one handler, isolating crashes so a single bad document or a partial
-- function deep in the checker cannot take down the whole session.
--
-- 'ExitCode' (raised by @exit@/@shutdown@ handling) is re-thrown so the server
-- can still terminate; any other exception is logged, surfaced to the client
-- via @window/logMessage@, and—if the failing message was a request—answered
-- with a JSON-RPC internal error so the client is never left waiting.
safeHandle :: ServerState -> Value -> IO ()
safeHandle st msg = guarded st (Just msg) (handle st msg)

guarded :: ServerState -> Maybe Value -> IO () -> IO ()
guarded st msg action = do
  result <- try action
  case result of
    Right () -> pure ()
    Left err
      | Just code <- fromException err -> throwIO (code :: ExitCode)
      | otherwise -> do
          let detail = T.pack (show (err :: SomeException))
          stLogger st ("handler error: " <> detail)
          notify
            stdout
            "window/logMessage"
            ( object
                ["type" .= (1 :: Int), "message" .= ("tynix-lsp internal error: " <> detail)]
            )
          case msg of
            Just m
              | Just _ <- field "id" m ->
                  respondError stdout m (-32603) ("internal error: " <> detail)
            _ -> pure ()

-- | The cached analyzer. A fresh declaration-support cache is used per call
-- so edited `.d.tynix` files are never served stale.
analyzer :: ServerState -> IO AnalyzeFn
analyzer st = cachedAnalyzeText (stCache st) <$> newSupportCache

-- | Wrap the driver with the workspace-wide analysis cache so repeated
-- hover / workspace-symbol / definition requests against unchanged content
-- collapse to a single driver invocation.
cachedAnalyzeText ::
  IORef AnalysisCache ->
  SupportCache ->
  FilePath ->
  Text ->
  IO (Either String Analysis)
cachedAnalyzeText cacheRef supportCache file content = do
  cache <- readIORef cacheRef
  case accessAnalysisCache (file, content) cache of
    (Just result, touchedCache) -> do
      writeIORef cacheRef touchedCache
      pure result
    (Nothing, _) -> do
      -- Checker failures carry a full source range; encode it as
      -- `line:col:endLine:endCol:` so diagnostics underline the exact span.
      result <- analyzeTextForEditorWith supportCache file content
      modifyIORef' cacheRef (insertAnalysisCache (file, content) result)
      pure result

helpText :: String
helpText =
  unlines
    [ "tynix-lsp",
      "",
      "Usage:",
      "  tynix-lsp [--stdio] [--log-file PATH]",
      "  tynix-lsp --version",
      "  tynix-lsp --help"
    ]

versionText :: String
versionText = "tynix-lsp " <> showVersion PackageInfo.version

-- | Type alias for the cached analyzer threaded through every handler.
type AnalyzeFn = FilePath -> Text -> IO (Either String Analysis)

-- | Dispatch one incoming JSON-RPC message.
handle :: ServerState -> Value -> IO ()
handle st msg = do
  analyze <- analyzer st
  let docsRef = stDocs st
      query run = do
        docs <- readIORef docsRef
        run readFileSafe analyze docs msg >>= respond stdout msg
  case field "method" msg >>= asText of
    Just "initialize" -> do
      let caps = field "params" msg >>= field "capabilities" >>= field "textDocument"
          pull = isJust (caps >>= field "diagnostic")
          hierarchical = caps >>= field "documentSymbol" >>= field "hierarchicalDocumentSymbolSupport"
      writeIORef (stPullDiagnostics st) pull
      writeIORef (stHierarchicalSymbols st) (hierarchical == Just (Bool True))
      respond stdout msg (withServerInfo (serverCapabilities pull))
    Just "initialized" -> pure ()
    Just "shutdown" -> writeIORef (stShutdown st) True >> respond stdout msg Null
    -- Per the LSP spec, `exit` returns code 0 only when a `shutdown` request
    -- preceded it, and 1 otherwise.
    Just "exit" -> do
      didShutdown <- readIORef (stShutdown st)
      if didShutdown then exitSuccess else exitWith (ExitFailure 1)
    Just "textDocument/didOpen" -> updateNow st analyze msg
    Just "textDocument/didSave" -> updateNow st analyze msg
    Just "textDocument/didChange" -> do
      docs <- readIORef docsRef
      case Session.updateDocumentText docs msg of
        Right (docs', file) -> do
          writeIORef docsRef docs'
          scheduleAnalysis st file
        -- not open yet (or a malformed edit): fall back to analysing now
        Left _ -> updateNow st analyze msg
    Just "textDocument/didClose" -> closeDocument st msg
    Just "textDocument/hover" -> query Session.hoverDocument
    Just "textDocument/signatureHelp" -> query Session.signatureHelpDocument
    Just "textDocument/completion" -> query Session.completionDocument
    Just "completionItem/resolve" -> query Session.completionResolveDocument
    Just "textDocument/definition" -> query Session.definitionDocument
    Just "textDocument/declaration" -> query Session.definitionDocument
    Just "textDocument/references" -> query Session.referencesDocument
    Just "textDocument/documentHighlight" -> query Session.documentHighlightsDocument
    Just "textDocument/prepareRename" -> query Session.prepareRenameDocument
    Just "textDocument/rename" -> query Session.renameDocument
    Just "textDocument/documentSymbol" -> do
      hierarchical <- readIORef (stHierarchicalSymbols st)
      query
        ( if hierarchical
            then Session.documentSymbolsHierarchicalDocument
            else Session.documentSymbolsDocument
        )
    Just "workspace/symbol" -> query Session.workspaceSymbolsDocument
    Just "textDocument/codeAction" -> query Session.codeActionsDocument
    Just "textDocument/semanticTokens/full" -> query Session.semanticTokensDocument
    Just "textDocument/semanticTokens/range" -> query Session.semanticTokensDocument
    Just "textDocument/diagnostic" -> query Session.pullDiagnosticsDocument
    Just "textDocument/formatting" -> query (\r _ d m -> Session.formattingDocument r d m)
    Just "textDocument/foldingRange" -> query (\r _ d m -> Session.foldingRangesDocument r d m)
    Just "textDocument/selectionRange" -> query (\r _ d m -> Session.selectionRangeDocument r d m)
    Just "textDocument/documentLink" -> query (\r _ d m -> Session.documentLinksDocument r d m)
    Just "textDocument/inlayHint" -> query Session.inlayHintsDocument
    Just "workspace/didChangeWatchedFiles" -> writeIORef (stCache st) emptyAnalysisCache
    Just "workspace/didChangeConfiguration" -> writeIORef (stCache st) emptyAnalysisCache
    -- A request (carries an `id`) for a method we do not implement must be
    -- answered with MethodNotFound; unknown notifications are ignored.
    method -> case field "id" msg of
      Just _ ->
        respondError
          stdout
          msg
          (-32601)
          (maybe "method not found" ("method not found: " <>) method)
      Nothing -> pure ()

withServerInfo :: Value -> Value
withServerInfo (Object obj) = Object (obj <> KeyMap.singleton "serverInfo" serverInfo)
  where
    serverInfo =
      object
        ["name" .= ("tynix-lsp" :: Text), "version" .= showVersion PackageInfo.version]
withServerInfo other = other

-- | Analyse a document now (open / save / unopened change) and publish.
updateNow :: ServerState -> AnalyzeFn -> Value -> IO ()
updateNow st analyze msg = do
  docs <- readIORef (stDocs st)
  (docs', file, result) <- Session.updateDocuments readFileSafe analyze docs msg
  writeIORef (stDocs st) docs'
  -- a direct analysis supersedes any pending debounced one
  modifyIORef' (stGenerations st) (Map.adjust (+ 1) file)
  case Session.lookupDocumentText file docs' of
    Just content -> publish st analyze file content result
    Nothing -> pure ()

-- | Bump the edit generation and enqueue a debounced re-analysis.
scheduleAnalysis :: ServerState -> FilePath -> IO ()
scheduleAnalysis st file = do
  generation <-
    atomicModifyIORef'
      (stGenerations st)
      (\m -> let g = Map.findWithDefault 0 file m + 1 in (Map.insert file g m, g))
  void . forkIO $ do
    threadDelay debounceMicros
    writeChan (stQueue st) (Debounced file generation)

-- | Analyse the current text of a document, store the result, and publish.
analyzeAndPublish :: ServerState -> FilePath -> IO ()
analyzeAndPublish st file = do
  analyze <- analyzer st
  docs <- readIORef (stDocs st)
  case Session.lookupDocumentText file docs of
    Nothing -> pure ()
    Just content -> do
      result <- analyze file content
      modifyIORef' (stDocs st) (Session.storeDocumentAnalysis file content result)
      publish st analyze file content result

-- | Publish diagnostics unless the client pulls them.
publish ::
  ServerState -> AnalyzeFn -> FilePath -> Text -> Either String Analysis -> IO ()
publish st analyze file content result = do
  pull <- readIORef (stPullDiagnostics st)
  if pull
    then pure ()
    else do
      docs <- readIORef (stDocs st)
      items <-
        Session.documentDiagnostics readFileSafe analyze docs file content result
      notify
        stdout
        "textDocument/publishDiagnostics"
        (object ["uri" .= pathUri file, "diagnostics" .= items])

-- | Drop a document from the in-memory cache and clear its diagnostics.
closeDocument :: ServerState -> Value -> IO ()
closeDocument st msg = do
  docs <- readIORef (stDocs st)
  let (docs', closed) = Session.closeDocuments docs msg
  writeIORef (stDocs st) docs'
  case closed of
    Just file -> do
      modifyIORef' (stGenerations st) (Map.adjust (+ 1) file)
      notify stdout "textDocument/publishDiagnostics" (clearDiagnostics file)
    Nothing -> pure ()

readFileSafe :: FilePath -> IO (Either String Text)
readFileSafe file = do
  result <- try @IOException (TIO.readFile file)
  pure $
    case result of
      Left err -> Left ("failed to read " <> file <> ": " <> show err)
      Right content -> Right content
