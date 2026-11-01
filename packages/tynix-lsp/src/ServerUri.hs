{-# LANGUAGE OverloadedStrings #-}

-- | URI / path utilities used by the LSP framing layer.
--
-- The LSP spec exchanges paths as `file://` URIs with percent encoding.
-- Editors are inconsistent about whether they include an authority
-- (`file://localhost/...`), what they percent-encode, and how they treat
-- non-ASCII characters. These helpers normalise both directions so the
-- rest of the server can work with plain `FilePath`/`Text` values.
module ServerUri
  ( -- * Conversion
    pathUri,
    uriPath,

    -- * Percent encoding (exposed for tests)
    percentDecode,
    percentEncode,
  )
where

import Data.ByteString qualified as BS
import Data.Char
  ( digitToInt,
    isAsciiLower,
    isAsciiUpper,
    isDigit,
    isHexDigit,
    toUpper,
  )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TextEncoding
import Numeric (showHex)

-- | Encode a 'FilePath' as a `file://` LSP URI.
--
-- Path segments are percent-encoded as per RFC 3986 while @/@ separators
-- are preserved verbatim so the URI stays readable.
pathUri :: FilePath -> Text
pathUri file = "file://" <> percentEncode (T.pack file)

-- | Decode an LSP URI back into a 'FilePath'.
--
-- Tolerates the optional @localhost@ authority some clients emit
-- (`file://localhost/...`).
uriPath :: Text -> FilePath
uriPath text =
  T.unpack
    (percentDecode (stripAuthority (fromMaybe text (T.stripPrefix "file://" text))))

percentEncode :: Text -> Text
percentEncode =
  T.concatMap encodeChar
  where
    encodeChar char
      | isUnreserved char || char == '/' = T.singleton char
      | otherwise =
          T.concat
            (map encodeByte (BS.unpack (TextEncoding.encodeUtf8 (T.singleton char))))
    encodeByte byte = T.pack ('%' : map toUpper (pad (showHex byte "")))
    -- Percent-encoding always spells a byte with two hex digits.
    pad [digit] = ['0', digit]
    pad digits = digits
    isUnreserved char =
      isAsciiLower char
        || isAsciiUpper char
        || isDigit char
        || char `elem` ['-', '.', '_', '~']

percentDecode :: Text -> Text
percentDecode text =
  case TextEncoding.decodeUtf8' (BS.pack (decodeBytes (T.unpack text))) of
    Left _ -> text
    Right decoded -> decoded
  where
    decodeBytes ('%' : a : b : rest)
      | isHexDigit a && isHexDigit b =
          fromIntegral (digitToInt a * 16 + digitToInt b) : decodeBytes rest
    decodeBytes (char : rest) = BS.unpack (TextEncoding.encodeUtf8 (T.singleton char)) <> decodeBytes rest
    decodeBytes [] = []

stripAuthority :: Text -> Text
stripAuthority path =
  fromMaybe path (T.stripPrefix "localhost" path)
