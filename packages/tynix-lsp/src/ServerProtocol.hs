{-# LANGUAGE OverloadedStrings #-}

-- | JSON-RPC framing layer used by the tynix LSP.
--
-- The LSP wire format is JSON-RPC over @Content-Length@-framed messages.
-- This module owns the framing primitives so the rest of `Server` can
-- focus on the semantic LSP method bodies.
--
-- 'readMessageOutcome' is the preferred read primitive: it differentiates
-- clean EOF (no log) from protocol decode errors (which should surface on
-- stderr) so the event loop can stay alive on a malformed message instead
-- of silently spinning. 'readMessage' is the back-compat wrapper that
-- flattens the outcome into 'Maybe'.
module ServerProtocol
  ( -- * Read path
    ReadOutcome (..),
    contentLengthFromHeaders,
    readHeaders,
    readMessage,
    readMessageOutcome,

    -- * Write path
    notify,
    send,
  )
where

import Data.Aeson (Value, decodeStrict', encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.IO (Handle, hFlush, hIsEOF)

-- | Result of attempting to read one JSON-RPC framed message from a handle.
data ReadOutcome
  = -- | The peer closed the stream cleanly.
    ReadEof
  | -- | A well-formed message was decoded.
    ReadMessage Value
  | -- | The framing or payload could not be parsed. The text describes the
    --   failure so callers can log it without blowing up the event loop.
    ReadError Text

-- | Pick the @Content-Length:@ value out of a list of header lines.
--
-- The lookup is case-insensitive per RFC 7230, since some clients send
-- @content-length@ in lowercase.
contentLengthFromHeaders :: [BS.ByteString] -> Maybe Int
contentLengthFromHeaders headers =
  listToMaybe $
    mapMaybe parseHeader headers
  where
    parseHeader header = do
      value <- stripPrefixCI "content-length:" (B8.unpack header)
      case reads (dropWhile (== ' ') value) of
        [(len, "")] -> Just len
        _ -> Nothing
    stripPrefixCI needle haystack =
      stripPrefix (map toLower needle) (map toLower haystack)

-- | Read one JSON-RPC framed message, returning a tagged outcome.
readMessageOutcome :: Handle -> IO ReadOutcome
readMessageOutcome handle = do
  eof <- hIsEOF handle
  if eof
    then pure ReadEof
    else do
      headers <- readHeaders handle
      case contentLengthFromHeaders headers of
        Nothing ->
          pure
            ( ReadError
                ( "missing or malformed Content-Length header (got "
                    <> renderHeaders headers
                    <> ")"
                )
            )
        Just len -> do
          body <- BS.hGet handle len
          case decodeStrict' body of
            Just value -> pure (ReadMessage value)
            Nothing ->
              pure
                ( ReadError
                    ( "failed to decode JSON-RPC body of "
                        <> T.pack (show len)
                        <> " bytes"
                    )
                )
  where
    renderHeaders headers =
      if null headers
        then "no headers"
        else T.intercalate "; " (map (T.pack . B8.unpack) headers)

-- | Backwards-compatible wrapper that flattens the outcome into 'Maybe'.
--
-- Treats every non-message outcome as 'Nothing'. New callers should prefer
-- 'readMessageOutcome' so they can log protocol errors.
readMessage :: Handle -> IO (Maybe Value)
readMessage handle = do
  outcome <- readMessageOutcome handle
  pure $ case outcome of
    ReadMessage value -> Just value
    _ -> Nothing

-- | Read a sequence of CRLF-terminated header lines, stopping at the blank
-- line that separates the headers from the body.
readHeaders :: Handle -> IO [BS.ByteString]
readHeaders handle = go []
  where
    go acc = do
      line <- B8.hGetLine handle
      let trimmed = B8.filter (/= '\r') line
      if BS.null trimmed
        then pure (reverse acc)
        else go (trimmed : acc)

-- | Frame a JSON value as a single LSP message and write it to a handle.
send :: Handle -> Value -> IO ()
send handle payload = do
  let body = encode payload
  B8.hPutStr
    handle
    ("Content-Length: " <> B8.pack (show (LBS.length body)) <> "\r\n\r\n")
  LBS.hPutStr handle body
  hFlush handle

-- | Send a JSON-RPC notification (no @id@, fire-and-forget) with the given
-- method name and params payload.
notify :: Handle -> Text -> Value -> IO ()
notify handle method params =
  send
    handle
    (object ["jsonrpc" .= ("2.0" :: Text), "method" .= method, "params" .= params])
