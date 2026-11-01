{-# LANGUAGE OverloadedStrings #-}

-- | Rich diagnostics for @textDocument/publishDiagnostics@ and pull-model
-- @textDocument/diagnostic@.
--
-- Checker failures become span-accurate LSP diagnostics with a stable
-- @code@, a @codeDescription@ link into the diagnostics reference, mapped
-- severity, "did you mean" suggestions, and @relatedInformation@ pointing at
-- the declarations involved. On top of that the scanner contributes lint
-- hints that do not need a successful analysis: unused bindings (tagged
-- @Unnecessary@ so editors fade them) and uses of declarations documented as
-- @\@deprecated@ (tagged @Deprecated@).
module SessionPublish
  ( -- * Diagnostics
    DiagnosticInfo (..),
    analysisDiagnostics,
    diagnosticValues,
    errorCode,
    stripErrorPrefixes,

    -- * Error localisation
    errorRange,
    localizationCandidates,
    replaceWithDynamic,

    -- * Lint
    lintDiagnostics,

    -- * Suggestions
    suggestNames,
    nameDistance',
    recordFieldNamesInMessage,
    quotedName,
    codeHref,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (Value, object, (.=))
import Data.Char (isAlphaNum, isDigit, toLower)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Driver (Analysis (..))
import Server (pathUri)
import SessionCompletion (CompletionEnv (..))
import SessionResolve
import SessionScan

-- | Structured form of one diagnostic before JSON encoding.
data DiagnosticInfo = DiagnosticInfo
  { diagRange :: (Int, Int),
    diagSeverity :: Int,
    diagCode :: Maybe Text,
    diagMessage :: Text,
    diagTags :: [Int],
    -- | (offset range, message) pairs within the same document
    diagRelated :: [((Int, Int), Text)],
    diagData :: Maybe Value
  }

codeHref :: Text -> Text
codeHref code = "https://tynix.dev/reference/diagnostics#" <> Text.toLower code

-- | Encode diagnostics for a document.
diagnosticValues :: FilePath -> Scan -> [DiagnosticInfo] -> [Value]
diagnosticValues file scan = map encode
  where
    idx = scanLineIndex scan
    pos off =
      let (l, c) = offsetToPosition idx off in object ["line" .= l, "character" .= c]
    rangeV (s, e) = object ["start" .= pos s, "end" .= pos e]
    encode d =
      object $
        [ "range" .= rangeV (diagRange d),
          "severity" .= diagSeverity d,
          "source" .= ("tynix" :: Text),
          "message" .= diagMessage d
        ]
          <> catMaybes
            [ ("code" .=) <$> diagCode d,
              (\c -> "codeDescription" .= object ["href" .= codeHref c]) <$> diagCode d,
              if null (diagTags d) then Nothing else Just ("tags" .= diagTags d),
              relatedInformation d,
              ("data" .=) <$> diagData d
            ]

    relatedInformation diagnostic
      | null (diagRelated diagnostic) = Nothing
      | otherwise =
          Just ("relatedInformation" .= map relatedLocation (diagRelated diagnostic))
    relatedLocation (range, message) =
      object
        [ "location" .= object ["uri" .= pathUri file, "range" .= rangeV range],
          "message" .= message
        ]

-- | The @[Txxxxx]@ code embedded in a checker message.
errorCode :: Text -> Maybe Text
errorCode msg = do
  (_, rest) <- nonEmptyBreak (Text.breakOn "[T" msg)
  let candidate = Text.takeWhile (/= ']') (Text.drop 1 rest)
  if Text.length candidate == 6 && Text.all isAlphaNum candidate
    then Just candidate
    else Nothing
  where
    nonEmptyBreak (a, b) = if Text.null b then Nothing else Just (a, b)

-- | Drop a leading @line:col:@ location and @[CODE]@ tag from a message,
-- and the source excerpt Megaparsec embeds in parse errors.
stripErrorPrefixes :: Text -> Text
stripErrorPrefixes msg =
  let firstLine = Text.strip (dropExcerpt msg)
      -- Drop a leading `line:col:` or `line:col:endLine:endCol:` location.
      segments = Text.splitOn ":" firstLine
      numeric t = not (Text.null (Text.strip t)) && Text.all isDigit (Text.strip t)
      locationParts = length (takeWhile numeric (take 4 segments))
      noLoc
        | locationParts >= 2 && length segments > locationParts =
            Text.stripStart (Text.intercalate ":" (drop locationParts segments))
        | otherwise = firstLine
      noCode = case Text.stripPrefix "[" noLoc of
        Just rest
          | Text.length (Text.takeWhile (/= ']') rest) == 6 ->
              Text.stripStart (Text.drop 7 rest)
        _ -> noLoc
   in noCode

dropExcerpt :: Text -> Text
dropExcerpt msg =
  case Text.lines msg of
    header : rest
      | any isGutter rest ->
          let body = filter (not . isGutter) rest
           in Text.intercalate "\n" (filter (not . Text.null) (codeOf header : body))
    _ -> msg
  where
    isGutter l =
      "|"
        `Text.isPrefixOf` Text.stripStart
          (Text.dropWhile (`elem` ("0123456789" :: String)) (Text.stripStart l))
    -- keep a "[TP0004]"-style tag that lives on the header line
    codeOf header = case errorCode header of
      Just c -> "[" <> c <> "]"
      Nothing -> ""

-- | The first name quoted with backticks (the checker's style) or double
-- quotes in a message.
quotedName :: Text -> Maybe Text
quotedName text =
  case [ (Text.length before, name)
       | q <- ["`", "\""],
         before : name : _ : _ <- [Text.splitOn q text]
       ] of
    [] -> Nothing
    found -> Just (snd (minimum found))

-- | Offset range of a checker error: an explicit @line:col@ prefix wins,
-- then the token named by the message.
errorRange :: Scan -> Text -> Maybe (Int, Int)
errorRange scan msg = narrowed <|> located <|> named
  where
    -- A field or name error inside a reported span is best underlined at the
    -- named token itself (`box.alpah` -> `alpah`).
    narrowed = do
      (start, end) <- located
      (nameStart, nameEnd) <- named
      if code `elem` ["TC0001", "TC0009"] && nameStart >= start && nameEnd <= end
        then Just (nameStart, nameEnd)
        else Nothing
    idx = scanLineIndex scan
    code = fromMaybe "" (errorCode msg)
    located = do
      firstLine <- listToMaybe (Text.lines msg)
      case mapMaybe
        readInt
        (takeWhile (isJust . readInt) (take 4 (Text.splitOn ":" firstLine))) of
        [l, c, el, ec] -> do
          -- A full `line:col:endLine:endCol:` range from the checker.
          let start = positionToOffset idx (max 0 (l - 1), max 0 (c - 1))
              end = positionToOffset idx (max 0 (el - 1), max 0 (ec - 1))
          pure (start, max (start + 1) end)
        l : c : _ -> do
          let off = positionToOffset idx (max 0 (l - 1), max 0 (c - 1))
          pure $ case codeTokenIndexAt scan off >>= codeToken scan of
            Just t | tokStart t == off -> (tokStart t, tokEnd t)
            _ -> (off, off + 1)
        _ -> Nothing
    readInt t = case reads (Text.unpack (Text.strip t)) of
      [(n, "")] -> Just (n :: Int)
      _ -> Nothing
    named = do
      name <- quotedName msg
      let toks = [(i, t) | i <- [0 .. codeTokenCount scan - 1], Just t <- [codeToken scan i]]
          textAt k = maybe "" tokText (codeToken scan k)
          span' t = (tokStart t, tokEnd t)
          idents = [(i, t) | (i, t) <- toks, tokKind t == KIdent, tokText t == name]
          pick xs = span' . snd <$> listToMaybe xs
      case code of
        "TC0001" ->
          pick
            [ (i, t)
            | (i, t) <- idents,
              not (isReferenceToken scan i),
              not (isTypeToken scan i),
              textAt (i - 1) /= "."
            ]
            <|> pick idents
        "TC0009" -> pick [(i, t) | (i, t) <- idents, textAt (i - 1) == "."] <|> pick idents
        c
          | c `elem` ["TC0002", "TC0004", "TC0007"] ->
              pick (drop 1 idents) <|> pick idents
          | otherwise -> pick idents

-- | Diagnostics for a failed (or successful) analysis.
--
-- @localized@ is an optional offset range found by re-checking with
-- suppression directives when the message itself carries no location.
analysisDiagnostics ::
  Ctx -> Either String Analysis -> Maybe (Int, Int) -> [DiagnosticInfo]
analysisDiagnostics _ (Right _) _ = []
analysisDiagnostics ctx (Left err) localized =
  [ DiagnosticInfo
      { diagRange = range,
        diagSeverity = if code == Just "TC0006" then 2 else 1,
        diagCode = code,
        diagMessage = message <> suggestionText,
        diagTags = [],
        diagRelated = related,
        diagData =
          if null suggestions
            then Nothing
            else Just (object ["suggestions" .= suggestions])
      }
  ]
  where
    scan = ctxScan ctx
    raw = Text.pack err
    code = errorCode raw
    message = stripErrorPrefixes raw
    range = fromMaybe fallbackRange (errorRange scan raw <|> localized)
    fallbackRange = case codeToken scan (fromMaybe 0 (scanRootStart scan)) of
      Just t -> (tokStart t, tokEnd t)
      Nothing -> (0, 0)
    name = quotedName message
    suggestions = case (code, name) of
      (Just "TC0001", Just n) -> take 3 (suggestNames n (scopeCandidates ctx (fst range)))
      (Just "TC0009", Just n) -> take 3 (suggestNames n (recordFieldNamesInMessage message))
      _ -> []
    suggestionText = case suggestions of
      s : _ -> " Did you mean `" <> s <> "`?"
      [] -> ""
    related = suggestionRelated <> selectionRelated <> signatureRelated
    suggestionRelated = case (code, suggestions) of
      (Just "TC0001", s : _) ->
        [ ((binderNameStart b, binderNameEnd b), "`" <> s <> "` is declared here")
        | Just b <- [resolveNameAt scan s (fst range)]
        ]
      _ -> []
    selectionRelated = case code of
      Just "TC0009" ->
        [ ( (binderNameStart b, binderNameEnd b),
            "`" <> binderName b <> "` is declared here"
          )
        | Just i <- [codeTokenIndexAt scan (fst range)],
          let path = selectionHead i,
          Just headName <- [listToMaybe path],
          Just b <- [resolveNameAt scan headName (fst range)]
        ]
      _ -> []
    selectionHead i =
      let textAt k = maybe "" tokText (codeToken scan k)
          go k = if textAt (k - 1) == "." then go (k - 2) else k
       in [textAt (go i)]
    signatureRelated
      | code
          `elem` map Just ["TC0013", "TC0014", "TC0015", "TC0019", "TC0020", "TC0021"] =
          [ (sig, "Expected type of `" <> binderName b <> "` is declared here")
          | b <- enclosingBindings scan (fst range),
            Just sig <- [binderSignature b]
          ]
      | otherwise = []

-- | Let bindings whose declaration contains the offset, innermost first.
enclosingBindings :: Scan -> Int -> [Binder]
enclosingBindings scan off =
  sortOn
    (\b -> binderDeclEnd b - binderDeclStart b)
    [ b
    | b <- scanBinders scan,
      binderKind b == BindLet,
      binderDeclStart b <= off,
      off <= binderDeclEnd b
    ]

-- | Names visible at an offset (scope-aware) plus the always-available ones.
scopeCandidates :: Ctx -> Int -> [Text]
scopeCandidates ctx off =
  map binderName (bindersInScopeAt (ctxScan ctx) off)
    <> maybe [] (Map.keys . analysisBindings) (ctxAnalysis ctx)
    <> ["builtins", "import", "true", "false", "null"]

-- | Field names listed in a rendered record type inside a message
-- (@{ name :: String; version :: String; }@).
recordFieldNamesInMessage :: Text -> [Text]
recordFieldNamesInMessage msg =
  [ Text.takeWhileEnd isNameChar (Text.stripEnd before)
  | (before, _) <- Text.breakOnAll "::" msg,
    let name = Text.takeWhileEnd isNameChar (Text.stripEnd before),
    not (Text.null name)
  ]
  where
    isNameChar c = isAlphaNum c || c `elem` ("_'-" :: String)

-- | Candidates within edit distance, closest first.
suggestNames :: Text -> [Text] -> [Text]
suggestNames needle candidates =
  map snd $
    sortOn
      fst
      [ (d, c)
      | c <- dedupe candidates,
        c /= needle,
        let d = nameDistance' needle c,
        d <= max 2 (Text.length needle `div` 2)
      ]
  where
    dedupe = Set.toList . Set.fromList

-- | Case-insensitive Levenshtein distance.
nameDistance' :: Text -> Text -> Int
nameDistance' a b = last (foldl step [0 .. length bs] (zip [1 ..] as))
  where
    as = map toLower (Text.unpack a)
    bs = map toLower (Text.unpack b)
    step prev (i, ca) =
      scanl
        ( \left (j, cb) ->
            minimum [left + 1, prev !! j + 1, prev !! (j - 1) + if ca == cb then 0 else 1]
        )
        i
        (zip [1 ..] bs)

-- * Localisation ----------------------------------------------------------------

-- | Candidate value spans (binding right-hand sides of @let@, @rec@, and
-- attribute sets), smallest first. Replacing one with a value of type
-- @any@ and re-checking tells whether a location-less error lives in it.
localizationCandidates :: Scan -> [(Int, Int)]
localizationCandidates scan =
  take 40 . sortOn (\(s, e) -> e - s) $
    [ (tokStart first, tokEnd lastTok)
    | k <- [1 .. n - 1],
      textAt k == "=",
      kindAt k == Just KSymbol,
      not (isTypeToken scan k),
      kindAt (k - 1) `elem` [Just KIdent, Just KString],
      let stop = valueEnd (k + 1) (0 :: Int),
      stop > k + 1,
      Just first <- [codeToken scan (k + 1)],
      Just lastTok <- [codeToken scan (stop - 1)]
    ]
  where
    n = codeTokenCount scan
    textAt i = maybe "" tokText (codeToken scan i)
    kindAt i = tokKind <$> codeToken scan i
    valueEnd i lets
      | i >= n = n
      | kindAt i == Just KSymbol && textAt i `elem` ["{", "(", "[", "${"] =
          maybe n (\c -> valueEnd (c + 1) lets) (matchingCloser scan i)
      | kindAt i == Just KSymbol && textAt i `elem` ["}", ")", "]"] = i
      | textAt i == "let" = valueEnd (i + 1) (lets + 1)
      | textAt i == "in" = if lets > 0 then valueEnd (i + 1) (lets - 1) else i
      | textAt i == ";" && lets == 0 = i
      | otherwise = valueEnd (i + 1) lets

-- | Source with the given span replaced by an expression of type @dynamic@
-- (@null as any@, which flows into every expected type).
replaceWithDynamic :: Scan -> (Int, Int) -> Text
replaceWithDynamic scan (s, e) =
  let content = scanContent scan
   in Text.take s content <> "(null as any)" <> Text.drop e content

-- * Lint ----------------------------------------------------------------------------

-- | Unused-binding and deprecated-use hints.
lintDiagnostics :: CompletionEnv -> Ctx -> [DiagnosticInfo]
lintDiagnostics env ctx = unused <> deprecated
  where
    scan = ctxScan ctx
    unused =
      [ DiagnosticInfo
          { diagRange = (binderNameStart b, binderNameEnd b),
            diagSeverity = 4,
            diagCode = Just "TL0001",
            diagMessage = "`" <> binderName b <> "` is " <> what b <> " but never used.",
            diagTags = [1],
            diagRelated = [],
            diagData = Just (object ["kind" .= kindTag b, "name" .= binderName b])
          }
      | b <- unusedBinders scan
      ]
    what b = case binderKind b of
      BindParam -> "a parameter"
      BindPatternField -> "destructured"
      BindPatternAlias -> "bound"
      _ -> "declared"
    kindTag b = case binderKind b of
      BindParam -> "param" :: Text
      BindPatternField -> "patternField"
      BindPatternAlias -> "patternAlias"
      BindInherit -> "inherit"
      _ -> "let"
    deprecatedBinders =
      Map.fromList
        [ (binderToken b, reason)
        | b <- scanBinders scan,
          Just doc <- [binderDoc ctx b],
          Just reason <- [deprecationReason doc]
        ]
    deprecated =
      [ deprecatedDiag (tokStart t, tokEnd t) (tokText t) reason
      | i <- [0 .. codeTokenCount scan - 1],
        isReferenceToken scan i,
        Just t <- [codeToken scan i],
        Just b <- [resolveNameAt scan (tokText t) (tokStart t)],
        Just reason <- [Map.lookup (binderToken b) deprecatedBinders]
      ]
        <> [ deprecatedDiag (tokStart t, tokEnd t) ("builtins." <> tokText t) reason
           | i <- [2 .. codeTokenCount scan - 1],
             Just t <- [codeToken scan i],
             tokKind t == KIdent,
             maybe False ((== ".") . tokText) (codeToken scan (i - 1)),
             maybe False ((== "builtins") . tokText) (codeToken scan (i - 2)),
             Just entry <- [Map.lookup (tokText t) (envBuiltinDocs env)],
             Just reason <- [ambientDocText entry >>= deprecationReason]
           ]
    deprecatedDiag range name reason =
      DiagnosticInfo
        { diagRange = range,
          diagSeverity = 4,
          diagCode = Just "TL0002",
          diagMessage =
            "`"
              <> name
              <> "` is deprecated"
              <> (if Text.null reason then "." else ": " <> reason),
          diagTags = [2],
          diagRelated = [],
          diagData = Nothing
        }

-- | @\@deprecated reason@ inside a documentation comment.
deprecationReason :: Text -> Maybe Text
deprecationReason doc =
  case Text.breakOn "@deprecated" doc of
    (_, rest)
      | Text.null rest -> Nothing
      | otherwise ->
          Just
            ( Text.strip
                (Text.takeWhile (/= '\n') (Text.drop (Text.length "@deprecated") rest))
            )
