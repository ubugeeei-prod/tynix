{-# LANGUAGE OverloadedStrings #-}

-- | Decode document requests, load their analysis, and encode source positions.
module SessionRequest
  ( DocumentReader,
    DocumentAnalyzer,
    Request (..),
    documentFile,
    positionOf,
    requestDocument,
    loadRequest,
    loadRequestAt,
    requestScan,
    rangeValueOf,
    lineRangeOf,
    locationOf,
    editOf,
  )
where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Driver (Analysis)
import Server (asInt, asText, field, pathUri, uriPath)
import SessionDocuments (loadDocumentAnalysis, loadDocumentContent)
import SessionResolve (Ctx (..), mkCtx)
import SessionScan (Scan (..), offsetToPosition, positionToOffset)
import SessionTypes (Documents)
import System.FilePath (normalise)

-- | Decode the document URI, preserving the empty-path fallback for malformed requests.
documentFile :: Value -> FilePath
documentFile message =
  maybe "" (normalise . uriPath) (field "params" message >>= field "textDocument" >>= field "uri" >>= asText)

-- | Decode a UTF-16 position, defaulting missing coordinates to zero.
positionOf :: Maybe Value -> (Int, Int)
positionOf position =
  ( maybe 0 asInt (position >>= field "line"),
    maybe 0 asInt (position >>= field "character")
  )

requestDocument ::
  (FilePath -> IO (Either String Text)) ->
  Documents ->
  Value ->
  IO (FilePath, Int, Int, Either String Text)
requestDocument readDocument docs msg = do
  let params = field "params" msg
      position = params >>= field "position"
      file = documentFile msg
      (lineNo, charNo) = positionOf position
  contentResult <- loadDocumentContent readDocument docs file
  pure (file, lineNo, charNo, contentResult)

type DocumentReader = FilePath -> IO (Either String Text)

type DocumentAnalyzer = FilePath -> Text -> IO (Either String Analysis)

-- | One positional request with everything handlers usually need.
data Request = Request
  { requestFile :: FilePath,
    requestContent :: Text,
    requestContext :: Ctx,
    requestOffset :: Int
  }

loadRequestAt :: DocumentReader -> DocumentAnalyzer -> Documents -> FilePath -> Int -> Int -> IO (Either String Request)
loadRequestAt readDocument analyze docs file lineNo charNo = do
  contentResult <- loadDocumentContent readDocument docs file
  case contentResult of
    Left err -> pure (Left err)
    Right content -> do
      result <- loadDocumentAnalysis readDocument analyze docs file
      let ctx = mkCtx file content result
          off = positionToOffset (scanLineIndex (ctxScan ctx)) (lineNo, charNo)
      pure (Right Request{requestFile = file, requestContent = content, requestContext = ctx, requestOffset = off})

loadRequest :: DocumentReader -> DocumentAnalyzer -> Documents -> Value -> IO (Either String Request)
loadRequest readDocument analyze docs msg = do
  let params = field "params" msg
      position = params >>= field "position"
      file = documentFile msg
      (lineNo, charNo) = positionOf position
  loadRequestAt readDocument analyze docs file lineNo charNo

requestScan :: Request -> Scan
requestScan = ctxScan . requestContext

posValue :: Scan -> Int -> Value
posValue scan off =
  let (l, c) = offsetToPosition (scanLineIndex scan) off
   in object ["line" .= l, "character" .= c]

rangeValueOf :: Scan -> (Int, Int) -> Value
rangeValueOf scan (s, e) = object ["start" .= posValue scan s, "end" .= posValue scan e]

lineRangeOf :: Scan -> (Int, Int) -> ((Int, Int), (Int, Int))
lineRangeOf scan (s, e) = (offsetToPosition (scanLineIndex scan) s, offsetToPosition (scanLineIndex scan) e)

locationOf :: FilePath -> Scan -> (Int, Int) -> Value
locationOf file scan range = object ["uri" .= pathUri file, "range" .= rangeValueOf scan range]

editOf :: Scan -> (Int, Int) -> Text -> Value
editOf scan range newText = object ["range" .= rangeValueOf scan range, "newText" .= newText]
