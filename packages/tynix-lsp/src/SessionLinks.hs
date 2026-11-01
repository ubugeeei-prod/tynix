{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Document-link provider for the tynix LSP.
--
-- Finds every importable path mentioned in a source buffer and produces
-- LSP 'DocumentLink' payloads so editors can navigate from
--
-- @
-- import ./legacy/default.nix
-- declare \"./registry/builtins.d.tynix\" { ... }
-- @
--
-- straight into the referenced file. The scan is text-driven so it
-- keeps working even when the buffer has type errors and no AST is
-- available.
--
-- 'findDocumentLinks' is the pure entry point so spec tests can
-- exercise the scanner without touching disk. 'encodeDocumentLinks'
-- formats the LSP wire object once the caller has resolved relative
-- paths against the source file's directory.
module SessionLinks
  ( DocumentLink (..),
    encodeDocumentLink,
    encodeDocumentLinks,
    findDocumentLinks,
    resolveLinkTarget,
  )
where

import Data.Aeson (Value, object, (.=))
import Data.Char (isAlphaNum, isDigit, isLetter)
import Data.Text (Text)
import Data.Text qualified as Text
import Server (pathUri, textRangeToUtf16Columns)
import System.FilePath
  ( isAbsolute,
    joinPath,
    normalise,
    splitDirectories,
    (</>),
  )

-- | Internal document-link value type.
--
-- @documentLinkStartChar@ / @documentLinkEndChar@ are UTF-16 columns so
-- the encoded LSP payload matches the protocol's character index model.
data DocumentLink = DocumentLink
  { documentLinkLine :: !Int,
    documentLinkStartChar :: !Int,
    documentLinkEndChar :: !Int,
    documentLinkPath :: !Text
  }
  deriving (Eq, Show)

-- | Encode a list of document links as the LSP wire array, resolving any
-- relative paths against the source file's directory.
encodeDocumentLinks :: FilePath -> [DocumentLink] -> [Value]
encodeDocumentLinks baseDir = map (encodeDocumentLink baseDir)

-- | Encode one document link, resolving its path target against the
-- source file's directory.
encodeDocumentLink :: FilePath -> DocumentLink -> Value
encodeDocumentLink baseDir link =
  object
    [ "range"
        .= object
          [ "start"
              .= object
                ["line" .= documentLinkLine link, "character" .= documentLinkStartChar link],
            "end"
              .= object
                ["line" .= documentLinkLine link, "character" .= documentLinkEndChar link]
          ],
      "target"
        .= pathUri (resolveLinkTarget baseDir (Text.unpack (documentLinkPath link)))
    ]

-- | Combine a base directory with a possibly-relative link path. Absolute
-- paths are kept as-is so a leading @/@ stays untouched. @..@ segments
-- are collapsed lexically so editors receive a clean filesystem target.
resolveLinkTarget :: FilePath -> FilePath -> FilePath
resolveLinkTarget baseDir path
  | isAbsolute path = collapseDotDot (normalise path)
  | otherwise = collapseDotDot (normalise (baseDir </> path))

-- | Lexically collapse @..@ and @.@ segments without touching the
-- filesystem. Avoids @System.Directory.canonicalizePath@ so the helper
-- stays pure for unit tests.
collapseDotDot :: FilePath -> FilePath
collapseDotDot = joinPath . reverse . foldl' step [] . splitDirectories
  where
    step acc ".." = case acc of
      (top : tops) | not (isAnchor top) && top /= ".." -> tops
      _ -> ".." : acc
    step acc "." = acc
    step acc s = s : acc
    isAnchor s = s == "/" || s == "" || s == "."

-- | Scan @content@ and return one 'DocumentLink' per @import@ path
-- literal or @declare@ string literal.
--
-- The scanner walks each line as a state machine so it correctly
-- ignores keywords inside line comments (@# ...@) and double-quoted
-- strings, and so identifiers that merely have @import@ or @declare@
-- as a prefix (e.g. @importHelper@) are not matched.
findDocumentLinks :: Text -> [DocumentLink]
findDocumentLinks content =
  concatMap scanLine (zip [0 ..] (Text.lines content))

scanLine :: (Int, Text) -> [DocumentLink]
scanLine (lineNo, line) = go 0 False False []
  where
    len = Text.length line
    go ix inDQuote escaped acc
      | ix >= len = reverse acc
      | otherwise =
          let c = Text.index line ix
           in case c of
                '\\'
                  | inDQuote && not escaped ->
                      go (ix + 1) inDQuote True acc
                '"'
                  | not escaped ->
                      go (ix + 1) (not inDQuote) False acc
                '#'
                  | not inDQuote ->
                      reverse acc
                _
                  | inDQuote ->
                      go (ix + 1) inDQuote False acc
                _
                  | isKeywordAt "import" line ix ->
                      resumeLink inDQuote (ix + 6) acc (scanPathLiteralAt line (ix + 6))
                  | isKeywordAt "declare" line ix ->
                      resumeLink inDQuote (ix + 7) acc (scanQuotedStringAt line (ix + 7))
                  | isIdentChar c ->
                      let identLen = Text.length (Text.takeWhile isIdentChar (Text.drop ix line))
                       in go (ix + identLen) inDQuote False acc
                  | otherwise -> go (ix + 1) inDQuote False acc

    resumeLink inDQuote next acc = \case
      Just (pathStart, pathEnd, pathText) ->
        let (startColumn, endColumn) = textRangeToUtf16Columns line pathStart pathEnd
            link = DocumentLink lineNo startColumn endColumn pathText
         in go pathEnd inDQuote False (link : acc)
      Nothing -> go next inDQuote False acc

-- | True when @line@ contains @keyword@ at position @ix@ with a word
-- boundary on the right (start-of-line on the left is guaranteed by the
-- scan loop, which only invokes this after a non-identifier char or
-- after consuming a full identifier).
isKeywordAt :: Text -> Text -> Int -> Bool
isKeywordAt keyword line ix =
  let kwLen = Text.length keyword
      end = ix + kwLen
   in end <= Text.length line
        && Text.take kwLen (Text.drop ix line) == keyword
        && (end == Text.length line || not (isIdentChar (Text.index line end)))

-- | Match an unquoted path literal starting at @startIx@ after skipping
-- intervening spaces / tabs.
scanPathLiteralAt :: Text -> Int -> Maybe (Int, Int, Text)
scanPathLiteralAt line startIx =
  let rest0 = Text.drop startIx line
      ws = Text.length (Text.takeWhile isHSpace rest0)
      offset = startIx + ws
      rest1 = Text.drop ws rest0
   in if hasPathPrefix rest1
        then
          let pathText = Text.takeWhile isPathChar rest1
           in if Text.length pathText > prefixWidth rest1
                then Just (offset, offset + Text.length pathText, pathText)
                else Nothing
        else Nothing

-- | Match a double-quoted string literal starting at @startIx@ after
-- skipping intervening spaces / tabs.
scanQuotedStringAt :: Text -> Int -> Maybe (Int, Int, Text)
scanQuotedStringAt line startIx =
  let rest0 = Text.drop startIx line
      ws = Text.length (Text.takeWhile isHSpace rest0)
      offset = startIx + ws
      rest1 = Text.drop ws rest0
   in case Text.uncons rest1 of
        Just ('"', rest2) ->
          let pathText = Text.takeWhile (\c -> c /= '"' && c /= '\n') rest2
           in if Text.null pathText
                then Nothing
                else Just (offset + 1, offset + 1 + Text.length pathText, pathText)
        _ -> Nothing

-- | True when @text@ starts with @./@, @../@, or @/@.
hasPathPrefix :: Text -> Bool
hasPathPrefix text =
  "./" `Text.isPrefixOf` text
    || "../" `Text.isPrefixOf` text
    || "/" `Text.isPrefixOf` text

-- | Width of the recognised path prefix.
prefixWidth :: Text -> Int
prefixWidth text
  | "../" `Text.isPrefixOf` text = 3
  | "./" `Text.isPrefixOf` text = 2
  | "/" `Text.isPrefixOf` text = 1
  | otherwise = 0

isHSpace :: Char -> Bool
isHSpace c = c == ' ' || c == '\t'

isPathChar :: Char -> Bool
isPathChar c = isAlphaNum c || c `elem` ("._/-+" :: String)

isIdentChar :: Char -> Bool
isIdentChar c =
  isLetter c
    || isDigit c
    || c == '_'
    || c == '\''
    || c == '-'
    || c == '?'
    || c == '!'
