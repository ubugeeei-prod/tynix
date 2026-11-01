{-# LANGUAGE OverloadedStrings #-}

-- | Error-tolerant lexical and scope scanner used by the richer LSP features.
--
-- The core parser is all-or-nothing and its AST carries no source spans, so
-- editor features that need positions (scope-aware completion, unused-binding
-- hints, local rename, semantic tokens, selection ranges, …) work from this
-- independent token stream instead. The scanner never fails: unterminated
-- strings, unbalanced brackets, and half-typed bindings all degrade to the
-- best structure that can be recovered, which is exactly what an editor needs
-- while the user is mid-edit.
--
-- The scanner deliberately does not depend on 'Syntax', so changes to the
-- core AST never ripple into the language server's positional features.
module SessionScan
  ( -- * Tokens
    Token (..),
    TokenKind (..),
    tokenize,

    -- * Scanned document
    Scan (..),
    scanDocument,
    codeToken,
    codeTokenCount,

    -- * Binders and scopes
    Binder (..),
    BinderKind (..),
    Owner (..),
    isValueBinder,
    bindersInScopeAt,
    resolveNameAt,
    binderAtToken,
    binderReferences,
    unusedBinders,

    -- * Positional queries
    codeTokenIndexAt,
    codeTokenBefore,
    enclosingOpeners,
    matchingCloser,
    isTypeToken,
    isReferenceToken,
    insideCommentOrString,
    rootAttrsetOpener,
    attrsetFieldNames,

    -- * Documentation comments
    docCommentAtLine,
    docCommentForOffset,

    -- * Line index / positions
    LineIndex,
    mkLineIndex,
    offsetToPosition,
    positionToOffset,
    lineOfOffset,
    lineText,
    lineCount,

    -- * Keywords
    hardKeywords,
    isHardKeyword,
  )
where

import Control.Applicative ((<|>))
import Data.Bifunctor (bimap)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Ord (Down (..))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

-- * Tokens ------------------------------------------------------------------

-- | Coarse lexical class. Contextual keywords (`type`, `declare`, `as`, …)
-- are left as identifiers and classified by consumers with context.
data TokenKind
  = KIdent
  | KKeyword
  | KString
  | KNumber
  | KPath
  | KSymbol
  | KComment
  | KUnknown
  deriving (Eq, Show)

-- | One token. Offsets are character offsets into the whole document.
data Token = Token
  { tokKind :: !TokenKind,
    tokText :: !Text,
    tokStart :: !Int,
    tokEnd :: !Int
  }
  deriving (Eq, Show)

-- | Nix keywords that can never be identifiers.
hardKeywords :: [Text]
hardKeywords = ["let", "in", "if", "then", "else", "assert", "with", "rec", "inherit"]

isHardKeyword :: Text -> Bool
isHardKeyword = (`elem` hardKeywords)

data LexMode
  = LCode !Int
  | LDouble
  | LIndented

data StrEnd = StrClosed | StrInterp | StrEof

-- | Tokenize a whole document. Never fails.
tokenize :: Text -> [Token]
tokenize = go [LCode 0] 0
  where
    go [] off txt = go [LCode 0] off txt
    go (mode : rest) off txt =
      case mode of
        LDouble -> stringPiece LDouble scanDouble rest off txt
        LIndented -> stringPiece LIndented scanIndented rest off txt
        LCode depth -> code depth rest off txt

    stringPiece strMode scanner rest off txt =
      let (len, ending) = scanner txt
          piece = [Token KString (Text.take len txt) off (off + len) | len > 0]
          after = Text.drop len txt
          off' = off + len
       in piece <> case ending of
            StrClosed -> go rest off' after
            StrEof -> []
            StrInterp ->
              Token KSymbol "${" off' (off' + 2)
                : go (LCode 0 : strMode : rest) (off' + 2) (Text.drop 2 after)

    code depth rest off txt =
      case Text.uncons txt of
        Nothing -> []
        Just (c, r)
          | isSpace c ->
              let n = Text.length (Text.takeWhile isSpace txt)
               in go (LCode depth : rest) (off + n) (Text.drop n txt)
          | c == '#' ->
              let body = Text.takeWhile (/= '\n') txt
                  n = Text.length body
               in Token KComment body off (off + n)
                    : go (LCode depth : rest) (off + n) (Text.drop n txt)
          | c == '/' && "*" `Text.isPrefixOf` r ->
              let (inside, after) = Text.breakOn "*/" (Text.drop 2 txt)
                  n = 2 + Text.length inside + (if Text.null after then 0 else 2)
               in Token KComment (Text.take n txt) off (off + n)
                    : go (LCode depth : rest) (off + n) (Text.drop n txt)
          | c == '"' ->
              let (len, ending) = scanDouble r
                  n = len + 1
                  tok = Token KString (Text.take n txt) off (off + n)
                  after = Text.drop n txt
               in tok : case ending of
                    StrClosed -> go (LCode depth : rest) (off + n) after
                    StrEof -> []
                    StrInterp ->
                      Token KSymbol "${" (off + n) (off + n + 2)
                        : go (LCode 0 : LDouble : LCode depth : rest) (off + n + 2) (Text.drop 2 after)
          | c == '\'' && "'" `Text.isPrefixOf` r ->
              let (len, ending) = scanIndented (Text.drop 1 r)
                  n = len + 2
                  tok = Token KString (Text.take n txt) off (off + n)
                  after = Text.drop n txt
               in tok : case ending of
                    StrClosed -> go (LCode depth : rest) (off + n) after
                    StrEof -> []
                    StrInterp ->
                      Token KSymbol "${" (off + n) (off + n + 2)
                        : go (LCode 0 : LIndented : LCode depth : rest) (off + n + 2) (Text.drop 2 after)
          | Just n <- pathLength txt ->
              Token KPath (Text.take n txt) off (off + n)
                : go (LCode depth : rest) (off + n) (Text.drop n txt)
          | c == '<',
            Just n <- searchPathLength txt ->
              Token KPath (Text.take n txt) off (off + n)
                : go (LCode depth : rest) (off + n) (Text.drop n txt)
          | isDigit c ->
              let n = numberLength txt
               in Token KNumber (Text.take n txt) off (off + n)
                    : go (LCode depth : rest) (off + n) (Text.drop n txt)
          | identStart c ->
              let word = Text.takeWhile identChar txt
                  n = Text.length word
                  kind = if isHardKeyword word then KKeyword else KIdent
               in Token kind word off (off + n)
                    : go (LCode depth : rest) (off + n) (Text.drop n txt)
          | c == '}' ->
              let tok = Token KSymbol "}" off (off + 1)
               in case rest of
                    (stringMode : outer)
                      | depth == 0,
                        isStringMode stringMode ->
                          tok : go (stringMode : outer) (off + 1) r
                    _ -> tok : go (LCode (max 0 (depth - 1)) : rest) (off + 1) r
          | otherwise ->
              case [s | s <- symbols, s `Text.isPrefixOf` txt] of
                sym : _ ->
                  let n = Text.length sym
                      depth' = if sym == "{" || sym == "${" then depth + 1 else depth
                   in Token KSymbol sym off (off + n)
                        : go (LCode depth' : rest) (off + n) (Text.drop n txt)
                [] ->
                  Token KUnknown (Text.singleton c) off (off + 1)
                    : go (LCode depth : rest) (off + 1) r

    isStringMode LDouble = True
    isStringMode LIndented = True
    isStringMode _ = False

symbols :: [Text]
symbols =
  [ "...",
    "${",
    "->",
    "|>",
    "<|",
    "==",
    "!=",
    "<=",
    ">=",
    "&&",
    "||",
    "++",
    "//",
    "::",
    "%1",
    "{",
    "}",
    "(",
    ")",
    "[",
    "]",
    ";",
    ",",
    ":",
    "@",
    ".",
    "?",
    "=",
    "+",
    "-",
    "*",
    "/",
    "<",
    ">",
    "!",
    "|",
    "&",
    "%"
  ]

-- | Scan the body of a double-quoted string (after the opening quote).
scanDouble :: Text -> (Int, StrEnd)
scanDouble = loop 0
  where
    loop n t = case Text.uncons t of
      Nothing -> (n, StrEof)
      Just ('\\', r) -> case Text.uncons r of
        Nothing -> (n + 1, StrEof)
        Just (_, r') -> loop (n + 2) r'
      Just ('"', _) -> (n + 1, StrClosed)
      Just ('$', r)
        | "$" `Text.isPrefixOf` r -> loop (n + 2) (Text.drop 1 r)
        | "{" `Text.isPrefixOf` r -> (n, StrInterp)
      Just (_, r) -> loop (n + 1) r

-- | Scan the body of an indented string (after the opening @''@).
scanIndented :: Text -> (Int, StrEnd)
scanIndented = loop 0
  where
    loop n t
      | Text.null t = (n, StrEof)
      | "'''" `Text.isPrefixOf` t = loop (n + 3) (Text.drop 3 t)
      | "''$" `Text.isPrefixOf` t = loop (n + 3) (Text.drop 3 t)
      | "''\\" `Text.isPrefixOf` t = loop (n + 4) (Text.drop 4 t)
      | "''" `Text.isPrefixOf` t = (n + 2, StrClosed)
      | "$$" `Text.isPrefixOf` t = loop (n + 2) (Text.drop 2 t)
      | "${" `Text.isPrefixOf` t = (n, StrInterp)
      | otherwise = loop (n + 1) (Text.drop 1 t)

pathChar :: Char -> Bool
pathChar c = isAlphaNum c || c `elem` ("._-+" :: String)

-- | Length of a Nix path literal at the start of the text, if any.
--
-- Accepts @./x@, @../x@, @~/x@, @/abs@, @a/b@ and — so completion can react
-- while the user is typing — a bare trailing slash after @.@, @..@, or @~@.
pathLength :: Text -> Maybe Int
pathLength txt =
  let prefix = if "~" `Text.isPrefixOf` txt then "~" else Text.takeWhile pathChar txt
      afterPrefix = Text.drop (Text.length prefix) txt
      bareSlashOk = prefix `elem` [".", "..", "~"]
   in case Text.uncons afterPrefix of
        Just ('/', r)
          | startsSegment r -> Just (Text.length prefix + segments afterPrefix)
          | bareSlashOk && not ("/" `Text.isPrefixOf` r) -> Just (Text.length prefix + 1)
        _ -> Nothing
  where
    startsSegment r = case Text.uncons r of
      Just (c, _) -> pathChar c
      Nothing -> False
    segments t = case Text.uncons t of
      Just ('/', r)
        | startsSegment r ->
            let seg = Text.takeWhile pathChar r
             in 1 + Text.length seg + segments (Text.drop (Text.length seg) r)
        | not ("/" `Text.isPrefixOf` r) && not ("*" `Text.isPrefixOf` r) -> 1
      _ -> 0

-- | Length of a @<nixpkgs>@-style search path at the start of the text.
searchPathLength :: Text -> Maybe Int
searchPathLength txt =
  let body = Text.takeWhile (\c -> pathChar c || c == '/') (Text.drop 1 txt)
      after = Text.drop (1 + Text.length body) txt
   in if not (Text.null body)
        && ">" `Text.isPrefixOf` after
        && maybe False (isAlpha . fst) (Text.uncons body)
        then Just (Text.length body + 2)
        else Nothing

numberLength :: Text -> Int
numberLength txt =
  let intPart = Text.takeWhile isDigit txt
      rest = Text.drop (Text.length intPart) txt
      fracPart = case Text.uncons rest of
        Just ('.', r)
          | maybe False (isDigit . fst) (Text.uncons r) ->
              1 + Text.length (Text.takeWhile isDigit r)
        _ -> 0
      rest' = Text.drop fracPart rest
      expPart = case Text.uncons rest' of
        Just (e, r)
          | e == 'e' || e == 'E' ->
              let signLen =
                    if maybe False ((`elem` ("+-" :: String)) . fst) (Text.uncons r) then 1 else 0
                  digits = Text.takeWhile isDigit (Text.drop signLen r)
               in if Text.null digits then 0 else 1 + signLen + Text.length digits
        _ -> 0
   in Text.length intPart + fracPart + expPart

identStart :: Char -> Bool
identStart c = isAlpha c || c == '_'

identChar :: Char -> Bool
identChar c = isAlphaNum c || c == '_' || c == '\'' || c == '-'

-- * Line index ----------------------------------------------------------------

-- | Fast offset ↔ (line, UTF-16 column) conversion for one document.
data LineIndex = LineIndex
  { lineStarts :: !(IntMap Int),
    lineStartSeq :: !(Seq Int),
    lineTexts :: !(Seq Text)
  }

mkLineIndex :: Text -> LineIndex
mkLineIndex content =
  let ls = Text.splitOn "\n" content
      starts = init (scanl (\acc l -> acc + Text.length l + 1) 0 ls)
   in LineIndex
        { lineStarts = IntMap.fromList (zip starts [0 ..]),
          lineStartSeq = Seq.fromList starts,
          lineTexts = Seq.fromList ls
        }

lineCount :: LineIndex -> Int
lineCount = Seq.length . lineTexts

lineText :: LineIndex -> Int -> Text
lineText idx n = fromMaybe "" (Seq.lookup n (lineTexts idx))

lineOfOffset :: LineIndex -> Int -> Int
lineOfOffset idx off = maybe 0 snd (IntMap.lookupLE off (lineStarts idx))

-- | Convert a character offset into an LSP @(line, utf16Column)@ pair.
offsetToPosition :: LineIndex -> Int -> (Int, Int)
offsetToPosition idx off =
  case IntMap.lookupLE off (lineStarts idx) of
    Nothing -> (0, 0)
    Just (start, lineNo) ->
      let line = lineText idx lineNo
       in (lineNo, utf16Width (Text.take (off - start) line))

-- | Convert an LSP position into a character offset, clamping to the line.
positionToOffset :: LineIndex -> (Int, Int) -> Int
positionToOffset idx (lineNo, column)
  | lineNo < 0 = 0
  | lineNo >= lineCount idx =
      let lastLine = lineCount idx - 1
       in fromMaybe 0 (Seq.lookup lastLine (lineStartSeq idx))
            + Text.length (lineText idx lastLine)
  | otherwise =
      fromMaybe 0 (Seq.lookup lineNo (lineStartSeq idx))
        + utf16ToChars (lineText idx lineNo) column

utf16Width :: Text -> Int
utf16Width = Text.foldl' (\acc c -> acc + if fromEnum c > 0xffff then 2 else 1) 0

utf16ToChars :: Text -> Int -> Int
utf16ToChars line target = go 0 0 (Text.unpack line)
  where
    go chars _ [] = chars
    go chars units (c : cs)
      | units >= target = chars
      | otherwise = go (chars + 1) (units + if fromEnum c > 0xffff then 2 else 1) cs

-- * Scanned document ----------------------------------------------------------

-- | What introduced a binder.
data BinderKind
  = -- | @name = …;@ inside @let@
    BindLet
  | -- | @inherit name;@ inside @let@
    BindInherit
  | -- | @x: …@ or @(x :: T): …@
    BindParam
  | -- | @{ x, … }: …@
    BindPatternField
  | -- | @args\@{ … }: …@
    BindPatternAlias
  | -- | @rec { name = …; }@
    BindRecField
  | -- | @type Name … = …;@
    BindTypeAlias
  | -- | alias parameters and @forall@ variables
    BindTypeParam
  deriving (Eq, Ord, Show)

-- | The function a lambda parameter belongs to, used to recover the
-- parameter's type from the function's signature.
data Owner
  = -- | a @let@ / @rec@ binder, by the code-token index of its name
    OwnerBinding !Int
  | -- | a field of the root attribute set
    OwnerRootField !Text
  | -- | the root expression itself
    OwnerRoot
  deriving (Eq, Show)

data Binder = Binder
  { binderName :: !Text,
    binderKind :: !BinderKind,
    -- | code-token index of the binder's name
    binderToken :: !Int,
    binderNameStart :: !Int,
    binderNameEnd :: !Int,
    -- | offsets of the region in which the name is visible
    binderScopeStart :: !Int,
    binderScopeEnd :: !Int,
    -- | offsets of the whole declaration (binding statement, pattern, …)
    binderDeclStart :: !Int,
    binderDeclEnd :: !Int,
    -- | code-token range @[from, to)@ of the bound value or pattern default
    binderValue :: !(Maybe (Int, Int)),
    -- | source text of an explicit type annotation (signature or typed param)
    binderAnnotation :: !(Maybe Text),
    -- | offsets of the signature statement, when there is one
    binderSignature :: !(Maybe (Int, Int)),
    -- | owning function and parameter position for lambda parameters
    binderOwner :: !(Maybe (Owner, Int)),
    -- | True for bindings of the root @let@ (the ones the checker reports)
    binderRootLevel :: !Bool
  }
  deriving (Eq, Show)

isValueBinder :: Binder -> Bool
isValueBinder b = binderKind b `notElem` [BindTypeAlias, BindTypeParam]

data Scan = Scan
  { scanContent :: !Text,
    scanLineIndex :: !LineIndex,
    scanAllTokens :: ![Token],
    scanCode :: !(Seq Token),
    scanMatches :: !(IntMap Int),
    scanBinders :: ![Binder],
    scanBindersByName :: !(Map Text [Binder]),
    scanTypeTokens :: !(Set Int),
    scanNonRefs :: !(Set Int),
    scanOuterRefs :: !(Set Int),
    scanRootStart :: !(Maybe Int),
    scanRootAttrset :: !(Maybe Int),
    scanLineComments :: !(IntMap Text),
    -- | binder token → number of resolved references
    scanUsage :: !(IntMap Int),
    -- | reference token → binder token it resolves to
    scanResolved :: !(IntMap Int)
  }

codeToken :: Scan -> Int -> Maybe Token
codeToken scan i = Seq.lookup i (scanCode scan)

codeTokenCount :: Scan -> Int
codeTokenCount = Seq.length . scanCode

data Stmt
  = StmtBind !Int !Int !Int [Int]
  | StmtInherit !(Maybe Int) [Int] !Int
  | StmtSig !Int !Int
  | StmtOther

data Acc = Acc
  { accBinders :: [Binder],
    accOwners :: IntMap (Owner, Int),
    accNonRefs :: Set Int,
    accOuter :: Set Int,
    accTypes :: Set Int
  }

emptyAcc :: Acc
emptyAcc = Acc [] IntMap.empty Set.empty Set.empty Set.empty

-- | Scan a document into tokens, binders, scopes, and resolved references.
scanDocument :: Text -> Scan
scanDocument content =
  Scan
    { scanContent = content,
      scanLineIndex = lineIndex,
      scanAllTokens = allTokens,
      scanCode = code,
      scanMatches = matches,
      scanBinders = binders,
      scanBindersByName = byName,
      scanTypeTokens = typeTokens,
      scanNonRefs = nonRefs,
      scanOuterRefs = outerRefs,
      scanRootStart = rootStart,
      scanRootAttrset = rootAttrset,
      scanLineComments = lineComments,
      scanUsage = usage,
      scanResolved = resolved
    }
  where
    lineIndex = mkLineIndex content
    textLen = Text.length content
    allTokens = tokenize content
    code = Seq.fromList (filter ((/= KComment) . tokKind) allTokens)
    n = Seq.length code

    at i = if i < 0 then Nothing else Seq.lookup i code
    textAt i = maybe "" tokText (at i)
    kindAt i = tokKind <$> at i
    isSym s i = kindAt i == Just KSymbol && textAt i == s
    isKw s i = kindAt i == Just KKeyword && textAt i == s
    isIdent i = kindAt i == Just KIdent
    isIdentNamed s i = isIdent i && textAt i == s
    isString i = kindAt i == Just KString
    startOf i = maybe textLen tokStart (at i)
    endOf i = maybe textLen tokEnd (at i)
    slice a b = Text.strip (Text.take (b - a) (Text.drop a content))
    isOpener i = kindAt i == Just KSymbol && textAt i `elem` ["{", "(", "[", "${"]
    isCloser i = kindAt i == Just KSymbol && textAt i `elem` ["}", ")", "]"]

    matches = matchBrackets code
    closerOf i = IntMap.lookup i matches
    -- index just past a group starting at opener @i@ (n when unterminated)
    skipGroup i = maybe n (+ 1) (closerOf i)

    -- End of a lambda / let body starting at token @start@: the first
    -- top-level @;@, @,@, closer, or unbalanced @in@/@then@/@else@.
    bodyEnd start = go start (0 :: Int) (0 :: Int) (0 :: Int)
      where
        go i lets ifs pend
          | i >= n = n
          | isOpener i = go (skipGroup i) lets ifs pend
          | isCloser i = i
          | isSym ";" i =
              if lets > 0
                then go (i + 1) lets ifs pend
                else if pend > 0 then go (i + 1) lets ifs (pend - 1) else i
          | isSym "," i && lets == 0 = i
          | isKw "let" i = go (i + 1) (lets + 1) ifs pend
          | isKw "in" i = if lets > 0 then go (i + 1) (lets - 1) ifs pend else i
          | isKw "if" i = go (i + 1) lets (ifs + 1) pend
          | isKw "then" i = if ifs > 0 then go (i + 1) lets ifs pend else i
          | isKw "else" i = if ifs > 0 then go (i + 1) lets (ifs - 1) pend else i
          | isKw "with" i || isKw "assert" i = go (i + 1) lets ifs (pend + 1)
          | otherwise = go (i + 1) lets ifs pend

    -- End of a binding statement: the next top-level @;@ before @limit@.
    stmtEnd from limit = go from (0 :: Int) (0 :: Int)
      where
        go i lets pend
          | i >= limit = limit
          | isOpener i = go (min limit (skipGroup i)) lets pend
          | isKw "let" i = go (i + 1) (lets + 1) pend
          | isKw "in" i = go (i + 1) (max 0 (lets - 1)) pend
          | isKw "with" i || isKw "assert" i = go (i + 1) lets (pend + 1)
          | isSym ";" i =
              if lets > 0
                then go (i + 1) lets pend
                else if pend > 0 then go (i + 1) lets (pend - 1) else i
          | otherwise = go (i + 1) lets pend

    statements a b
      | a >= b = []
      | otherwise = let e = stmtEnd a b in (a, e) : statements (e + 1) b

    -- The token index of the @in@ closing a @let@ at @i@, or of the
    -- enclosing closer / end of input when the block is unterminated.
    findIn i = go (i + 1) (0 :: Int)
      where
        go k depth
          | k >= n = n
          | isOpener k = go (skipGroup k) depth
          | isCloser k = k
          | isKw "let" k = go (k + 1) (depth + 1)
          | isKw "in" k = if depth == 0 then k else go (k + 1) (depth - 1)
          | otherwise = go (k + 1) depth

    attrPath k
      | isIdent k || isString k = continuePath [k] (k + 1)
      | isSym "${" k = continuePath [] (skipGroup k)
      | otherwise = Nothing
      where
        continuePath acc next
          | isSym "." next = case attrPath (next + 1) of
              Just (rest, after) -> Just (acc <> rest, after)
              Nothing -> Just (acc, next)
          | otherwise = Just (acc, next)

    classify (s, e)
      | isKw "inherit" s =
          if isSym "(" (s + 1)
            then
              let c = fromMaybe e (closerOf (s + 1))
               in StmtInherit
                    (Just (s + 1))
                    [k | k <- [c + 1 .. e - 1], isIdent k || isString k]
                    e
            else StmtInherit Nothing [k | k <- [s + 1 .. e - 1], isIdent k] e
      | (isIdent s || isString s) && isSym "::" (s + 1) = StmtSig s e
      | Just (path, after) <- attrPath s,
        isSym "=" after =
          StmtBind s after e path
      | otherwise = StmtOther

    -- Lambda headers -------------------------------------------------------

    patternBody i
      | not (isSym "{" i) = Nothing
      | otherwise = do
          c <- closerOf i
          if isSym ":" (c + 1)
            then
              Just
                (c + 2, if isSym "@" (i - 1) && isIdent (i - 2) then Just (i - 2) else Nothing)
            else
              if isSym "@" (c + 1) && isIdent (c + 2) && isSym ":" (c + 3)
                then Just (c + 4, Just (c + 2))
                else Nothing

    typedParam i
      | isSym "(" i && isIdent (i + 1) && isSym "::" (i + 2) = do
          c <- closerOf i
          if isSym ":" (c + 1) then Just c else Nothing
      | otherwise = Nothing

    simpleParam i =
      isIdent i
        && isSym ":" (i + 1)
        && not (isSym "." (i - 1))
        && not (isSym "@" (i - 1))
        && not (adjacentSlash (i + 2))

    adjacentSlash j = case (at (j - 1), at j) of
      (Just colon, Just next) -> tokEnd colon == tokStart next && "/" `Text.isPrefixOf` tokText next
      _ -> False

    -- A lambda header starting at @k@: its key token and body start.
    headerAt k
      | simpleParam k = Just (k, k + 2)
      | Just (body, _) <- patternBody k = Just (k, body)
      | isIdent k && isSym "@" (k + 1),
        Just (body, _) <- patternBody (k + 2) =
          Just (k, body)
      | Just c <- typedParam k = Just (k, c + 2)
      | otherwise = Nothing

    ownerChain owner k idx = case headerAt k of
      Just (key, next) -> (key, (owner, idx)) : ownerChain owner next (idx + 1 :: Int)
      Nothing -> []

    -- Top-level declarations ------------------------------------------------

    (declTypeBinders, declTypeTokens, rootStart) = topLevel 0 [] Set.empty
      where
        topLevel k accB accT
          | k >= n = (accB, accT, Nothing)
          | isIdentNamed "type" k && isIdent (k + 1) && not (isSym "=" (k + 1)) =
              let e = stmtEnd k n
                  eqIx = case [j | j <- [k + 2 .. e - 1], isSym "=" j] of
                    j : _ -> j
                    [] -> e
                  stmtSpan = (startOf k, endOf e)
                  alias =
                    (mkBinder (k + 1) BindTypeAlias (0, textLen) stmtSpan)
                      { binderAnnotation = Just (slice (endOf eqIx) (startOf e))
                      }
                  params =
                    [ mkBinder j BindTypeParam stmtSpan stmtSpan
                    | j <- [k + 2 .. eqIx - 1],
                      isIdent j
                    ]
               in topLevel (e + 1) (accB <> (alias : params)) (accT <> Set.fromList [k + 1 .. e])
          | isIdentNamed "declare" k
              && (isString (k + 1) || kindAt (k + 1) == Just KPath)
              && isSym "{" (k + 2) =
              let c = fromMaybe n (closerOf (k + 2))
                  e = if isSym ";" (c + 1) then c + 1 else c
               in topLevel (e + 1) accB (accT <> Set.fromList [k + 1 .. e])
          | otherwise = (accB, accT, Just k)

    notPattern i = isNothing (patternBody i)

    rootAttrset = do
      r <- rootStart
      if isSym "{" r && notPattern r
        then Just r
        else
          if isKw "rec" r && isSym "{" (r + 1)
            then Just (r + 1)
            else
              if isKw "let" r
                then
                  let j = findIn r
                   in if isKw "in" j && isSym "{" (j + 1) then Just (j + 1) else Nothing
                else Nothing

    -- Pass 1: let blocks and attribute sets ---------------------------------

    acc1 = foldl' step emptyAcc [0 .. n - 1]
      where
        step acc i
          | isKw "let" i && not (isSym "{" (i + 1)) = letBlock acc i
          | isSym "{" i && notPattern i && Set.notMember i declTypeTokens =
              attrBlock acc i
          | isIdentNamed "as" i
              && Set.notMember i declTypeTokens
              && valueBefore (i - 1)
              && not (isSym "=" (i + 1)) =
              acc{accTypes = accTypes acc <> Set.fromList [i + 1 .. bodyEnd (i + 1) - 1]}
          | otherwise = acc

    valueBefore j = case at j of
      Just t ->
        tokKind t `elem` [KIdent, KString, KNumber, KPath]
          || tokText t `elem` [")", "}", "]"]
      Nothing -> False

    letBlock acc i =
      let j = findIn i
          bodyStop = if isKw "in" j then bodyEnd (j + 1) else j
          scope = (startOf i, if bodyStop >= n then textLen else startOf bodyStop)
          stmts = map classify (statements (i + 1) (min j n))
          rootLevel = rootStart == Just i
          sigs = Map.fromList [(textAt s, (s, e)) | StmtSig s e <- stmts]
          bindBinders =
            dedupeFirst
              [ (mkBinder s BindLet scope (startOf s, endOf e))
                  { binderValue = if length path == 1 then Just (eq + 1, e) else Nothing,
                    binderAnnotation =
                      (\(ss, se) -> slice (endOf (ss + 1)) (startOf se))
                        <$> Map.lookup (textAt s) sigs,
                    binderSignature = bimap startOf endOf <$> Map.lookup (textAt s) sigs,
                    binderRootLevel = rootLevel
                  }
              | StmtBind s eq e path <- stmts,
                isIdent s
              ]
          inheritBinders =
            [ (mkBinder k BindInherit scope (startOf k, endOf e)){binderRootLevel = rootLevel}
            | StmtInherit _ names e <- stmts,
              k <- names,
              isIdent k
            ]
          owners =
            concat
              [ ownerChain (OwnerBinding s) (eq + 1) 0
              | StmtBind s eq _ path <- stmts,
                length path == 1
              ]
          nonRefsHere =
            Set.fromList (concat [path | StmtBind _ _ _ path <- stmts])
              <> Set.fromList [s | StmtSig s _ <- stmts]
              <> Set.fromList (concat [names | StmtInherit (Just _) names _ <- stmts])
          outer = Set.fromList (concat [names | StmtInherit Nothing names _ <- stmts])
          typesHere = Set.fromList (concat [[s + 2 .. e - 1] | StmtSig s e <- stmts])
       in acc
            { accBinders = accBinders acc <> bindBinders <> inheritBinders,
              accOwners = accOwners acc <> IntMap.fromList owners,
              accNonRefs = accNonRefs acc <> nonRefsHere,
              accOuter = accOuter acc <> outer,
              accTypes = accTypes acc <> typesHere
            }

    attrBlock acc i =
      let c = fromMaybe n (closerOf i)
          isRec = isKw "rec" (i - 1)
          scope = (startOf i, endOf c)
          stmts = map classify (statements (i + 1) c)
          sigOnly = not (null [() | StmtSig _ _ <- stmts])
          recBinders =
            if isRec
              then
                dedupeFirst
                  [ mkBinder s BindRecField scope (startOf s, endOf e)
                  | StmtBind s _ e path <- stmts,
                    length path == 1,
                    isIdent s
                  ]
              else []
          owners =
            concat
              [ ownerChain owner (eq + 1) 0
              | StmtBind s eq _ path <- stmts,
                length path == 1,
                Just owner <-
                  [ if isRec
                      then Just (OwnerBinding s)
                      else if rootAttrset == Just i then Just (OwnerRootField (textAt s)) else Nothing
                  ]
              ]
          nonRefsHere =
            Set.fromList (concat [path | StmtBind _ _ _ path <- stmts])
              <> Set.fromList (concat [names | StmtInherit (Just _) names _ <- stmts])
              <> Set.fromList [s | StmtSig s _ <- stmts]
          typesHere = if sigOnly then Set.fromList [i .. c] else Set.empty
       in acc
            { accBinders = accBinders acc <> recBinders,
              accOwners = accOwners acc <> IntMap.fromList owners,
              accNonRefs = accNonRefs acc <> nonRefsHere,
              accTypes = accTypes acc <> typesHere
            }

    rootOwners = maybe [] (\r -> ownerChain OwnerRoot r 0) rootStart
    ownerMap = accOwners acc1 <> IntMap.fromList rootOwners
    typeTokens0 = declTypeTokens <> accTypes acc1

    -- Pass 2: lambdas and forall binders -----------------------------------

    lambdaBinders = concatMap lambdaAt [0 .. n - 1]
    lambdaAt i
      | Set.member i typeTokens0 =
          if isIdentNamed "forall" i
            then
              let stop = bodyEnd i
                  vars = takeWhile isIdent [i + 1 .. stop - 1]
                  scope = (startOf i, if stop >= n then textLen else startOf stop)
               in [mkBinder v BindTypeParam scope scope | v <- vars]
            else []
      | simpleParam i =
          let stop = bodyEnd (i + 2)
              scope = (startOf i, if stop >= n then textLen else startOf stop)
           in [ (mkBinder i BindParam scope (startOf i, endOf (i + 1)))
                  { binderOwner = IntMap.lookup i ownerMap
                  }
              ]
      | Just (body, alias) <- patternBody i =
          let c = fromMaybe n (closerOf i)
              stop = bodyEnd body
              headStart = maybe (startOf i) (\a -> startOf (min a i)) alias
              scope = (headStart, if stop >= n then textLen else startOf stop)
              decl = (headStart, endOf (body - 1))
              key = maybe i (min i) alias
              owner = IntMap.lookup key ownerMap
              fields =
                [ (mkBinder s BindPatternField scope decl)
                    { binderValue = if isSym "?" (s + 1) then Just (s + 2, e) else Nothing,
                      binderOwner = owner
                    }
                | (s, e) <- splitElements (i + 1) c,
                  isIdent s
                ]
              aliasBinder =
                [ (mkBinder a BindPatternAlias scope decl){binderOwner = owner}
                | Just a <- [alias]
                ]
           in fields <> aliasBinder
      | Just c <- typedParam i =
          let stop = bodyEnd (c + 2)
              scope = (startOf i, if stop >= n then textLen else startOf stop)
           in [ (mkBinder (i + 1) BindParam scope (startOf i, endOf (c + 1)))
                  { binderAnnotation = Just (slice (endOf (i + 2)) (startOf c)),
                    binderOwner = IntMap.lookup i ownerMap
                  }
              ]
      | otherwise = []

    typedParamTypes =
      Set.fromList
        (concat [[i + 3 .. c - 1] | i <- [0 .. n - 1], Just c <- [typedParam i]])
    typeTokens = typeTokens0 <> typedParamTypes

    splitElements a b = go a
      where
        go i
          | i >= b = []
          | otherwise =
              let e = elementEnd i
               in (i, e) : go (e + 1)
        elementEnd i
          | i >= b = b
          | isOpener i = elementEnd (min b (skipGroup i))
          | isSym "," i = i
          | otherwise = elementEnd (i + 1)

    patternDefaultStarts =
      Set.fromList
        [ s
        | b <- lambdaBinders,
          binderKind b == BindPatternField,
          Just (s, _) <- [binderValue b]
        ]

    mkBinder k kind (scopeStart, scopeEnd) (declStart, declEnd) =
      Binder
        { binderName = textAt k,
          binderKind = kind,
          binderToken = k,
          binderNameStart = startOf k,
          binderNameEnd = endOf k,
          binderScopeStart = scopeStart,
          binderScopeEnd = scopeEnd,
          binderDeclStart = declStart,
          binderDeclEnd = declEnd,
          binderValue = Nothing,
          binderAnnotation = Nothing,
          binderSignature = Nothing,
          binderOwner = Nothing,
          binderRootLevel = False
        }

    binders = declTypeBinders <> accBinders acc1 <> lambdaBinders
    byName = Map.fromListWith (flip (<>)) [(binderName b, [b]) | b <- binders]
    binderTokens = Set.fromList (map binderToken binders)
    outerRefs = accOuter acc1
    nonRefs = accNonRefs acc1

    referenceIdx =
      [ i
      | i <- [0 .. n - 1],
        isIdent i,
        Set.notMember i typeTokens,
        Set.notMember i nonRefs,
        Set.notMember i binderTokens || Set.member i outerRefs,
        not (isSym "." (i - 1)),
        not (isSym "?" (i - 1)) || Set.member i patternDefaultStarts,
        not (isSym "@" (i + 1) && isSym "{" (i + 2))
      ]

    resolvedPairs =
      [ (i, binderToken b)
      | i <- referenceIdx,
        Just t <- [at i],
        Just b <-
          [ resolveIn
              byName
              (tokText t)
              (tokStart t)
              (if Set.member i outerRefs then Just i else Nothing)
          ]
      ]
    resolved = IntMap.fromList resolvedPairs
    usage = IntMap.fromListWith (+) [(b, 1 :: Int) | (_, b) <- resolvedPairs]

    lineComments =
      IntMap.fromList
        [ (lineNo, tokText t)
        | t <- allTokens,
          tokKind t == KComment,
          "#" `Text.isPrefixOf` tokText t,
          let lineNo = lineOfOffset lineIndex (tokStart t),
          Text.all
            isSpace
            ( Text.take
                (tokStart t - positionToOffset lineIndex (lineNo, 0))
                (lineText lineIndex lineNo)
            )
        ]

dedupeFirst :: [Binder] -> [Binder]
dedupeFirst = go Set.empty
  where
    go _ [] = []
    go seen (b : bs)
      | Set.member (binderName b) seen = go seen bs
      | otherwise = b : go (Set.insert (binderName b) seen) bs

-- | Pair openers with closers. Mismatched closers pop back to the nearest
-- compatible opener so one stray bracket does not unbalance the whole file.
matchBrackets :: Seq Token -> IntMap Int
matchBrackets code = go (0 :: Int) [] IntMap.empty (toList code)
  where
    go _ _ acc [] = acc
    go i stack acc (t : ts)
      | tokKind t /= KSymbol = go (i + 1) stack acc ts
      | tokText t `elem` ["{", "(", "[", "${"] =
          go (i + 1) ((i, tokText t) : stack) acc ts
      | tokText t `elem` ["}", ")", "]"] =
          case break (compatible (tokText t) . snd) stack of
            (_, (o, _) : rest) -> go (i + 1) rest (IntMap.insert o i (IntMap.insert i o acc)) ts
            (_, []) -> go (i + 1) stack acc ts
      | otherwise = go (i + 1) stack acc ts
    compatible :: Text -> Text -> Bool
    compatible "}" o = o == "{" || o == "${"
    compatible ")" o = o == "("
    compatible "]" o = o == "["
    compatible _ _ = False

resolveIn :: Map Text [Binder] -> Text -> Int -> Maybe Int -> Maybe Binder
resolveIn byName name off exclude =
  case sortOn (\b -> Down (binderScopeStart b, binderToken b)) candidates of
    b : _ -> Just b
    [] -> Nothing
  where
    candidates =
      [ b
      | b <- Map.findWithDefault [] name byName,
        isValueBinder b,
        binderScopeStart b <= off,
        off <= binderScopeEnd b,
        Just (binderToken b) /= exclude
      ]

-- | Resolve a value name visible at the given offset.
resolveNameAt :: Scan -> Text -> Int -> Maybe Binder
resolveNameAt scan name off = resolveIn (scanBindersByName scan) name off Nothing

-- | Every value binder visible at an offset, innermost first, one per name.
bindersInScopeAt :: Scan -> Int -> [Binder]
bindersInScopeAt scan off =
  dedupeFirst $
    sortOn
      (\b -> (Down (binderScopeStart b), binderToken b))
      [ b
      | b <- scanBinders scan,
        isValueBinder b,
        binderScopeStart b <= off,
        off <= binderScopeEnd b
      ]

-- | The binder whose name sits at the given code token, if any.
binderAtToken :: Scan -> Int -> Maybe Binder
binderAtToken scan i =
  case [b | b <- scanBinders scan, binderToken b == i] of
    b : _ -> Just b
    [] -> Nothing

-- | Code-token indices of every reference that resolves to the binder.
binderReferences :: Scan -> Binder -> [Int]
binderReferences scan b =
  [i | (i, target) <- IntMap.toList (scanResolved scan), target == binderToken b]

-- | Value binders that are never referenced. Names starting with @_@ and
-- exported @rec@ fields are exempt.
unusedBinders :: Scan -> [Binder]
unusedBinders scan =
  [ b
  | b <- scanBinders scan,
    binderKind b
      `elem` [BindLet, BindInherit, BindParam, BindPatternField, BindPatternAlias],
    not ("_" `Text.isPrefixOf` binderName b),
    IntMap.findWithDefault 0 (binderToken b) (scanUsage scan) == 0
  ]

isTypeToken :: Scan -> Int -> Bool
isTypeToken scan i = Set.member i (scanTypeTokens scan)

isReferenceToken :: Scan -> Int -> Bool
isReferenceToken scan i = IntMap.member i (scanResolved scan)

-- | The code token containing (or touching) the offset. A token starting at
-- the offset wins over one that merely ends there.
codeTokenIndexAt :: Scan -> Int -> Maybe Int
codeTokenIndexAt scan off =
  let startingHere = case codeTokenBefore scan off of
        Just i -> next (i + 1)
        Nothing -> next 0
      next j = case codeToken scan j of
        Just t | tokStart t <= off && off < tokEnd t -> Just j
        _ -> Nothing
      endingHere = case codeTokenBefore scan off of
        Just i | Just t <- codeToken scan i, tokEnd t == off -> Just i
        _ -> Nothing
   in startingHere <|> endingHere

-- | Index of the last code token that ends at or before the offset.
codeTokenBefore :: Scan -> Int -> Maybe Int
codeTokenBefore scan off = search 0 (Seq.length code - 1) Nothing
  where
    code = scanCode scan
    search lo hi best
      | lo > hi = best
      | otherwise =
          let mid = (lo + hi) `div` 2
           in case Seq.lookup mid code of
                Just t
                  | tokEnd t <= off -> search (mid + 1) hi (Just mid)
                  | otherwise -> search lo (mid - 1) best
                Nothing -> best

-- | Openers whose group contains the offset, innermost first.
enclosingOpeners :: Scan -> Int -> [Int]
enclosingOpeners scan off = go (0 :: Int) [] (toList (scanCode scan))
  where
    go _ stack [] = stack
    go i stack (t : ts)
      | tokStart t >= off = stack
      | tokKind t == KSymbol && tokText t `elem` ["{", "(", "[", "${"] =
          go (i + 1) (i : stack) ts
      | tokKind t == KSymbol && tokText t `elem` ["}", ")", "]"] =
          case IntMap.lookup i (scanMatches scan) of
            Just o | o `elem` stack -> go (i + 1) (drop 1 (dropWhile (/= o) stack)) ts
            _ -> go (i + 1) stack ts
      | otherwise = go (i + 1) stack ts

matchingCloser :: Scan -> Int -> Maybe Int
matchingCloser scan i = IntMap.lookup i (scanMatches scan)

-- | True when the offset sits inside a comment or a string literal piece.
insideCommentOrString :: Scan -> Int -> Bool
insideCommentOrString scan off = any inside (scanAllTokens scan)
  where
    inside t = case tokKind t of
      KComment ->
        tokStart t < off
          && (off < tokEnd t || (off == tokEnd t && "#" `Text.isPrefixOf` tokText t))
      KString ->
        tokStart t < off
          && (off < tokEnd t || (off == tokEnd t && not (closedString (tokText t))))
      _ -> False
    closedString s =
      ( Text.length s >= 2
          && ("\"" `Text.isSuffixOf` s && not ("\\\"" `Text.isSuffixOf` s))
      )
        || (Text.length s >= 4 && "''" `Text.isSuffixOf` s)

rootAttrsetOpener :: Scan -> Maybe Int
rootAttrsetOpener = scanRootAttrset

-- | Field names already written in the attrset / pattern opened at @i@.
attrsetFieldNames :: Scan -> Int -> [Text]
attrsetFieldNames scan i =
  let c = fromMaybe (codeTokenCount scan) (matchingCloser scan i)
      textAt k = maybe "" tokText (codeToken scan k)
      fieldLike k t =
        tokKind t == KIdent
          && (textAt (k + 1) `elem` ["=", ".", ",", "?", "}"] || k + 1 == c)
          && (textAt (k - 1) `elem` ["{", ";", ","] || k - 1 == i)
      inherited k = case [j | j <- [k - 1, k - 2 .. i + 1], textAt j `elem` [";", "inherit", "{"]] of
        j : _ -> textAt j == "inherit"
        [] -> False
   in [ tokText t
      | k <- [i + 1 .. c - 1],
        Just t <- [codeToken scan k],
        fieldLike k t || (tokKind t == KIdent && inherited k)
      ]

-- * Documentation comments ---------------------------------------------------

-- | The @#@ comment block directly above a line, with directive lines
-- (@# \@tynix-…@) skipped and comment markers stripped.
docCommentAtLine :: Scan -> Int -> Maybe Text
docCommentAtLine scan lineNo =
  let collected = go (lineNo - 1) []
      cleaned = filter (not . ("@tynix-" `Text.isPrefixOf`)) collected
   in if null cleaned then Nothing else Just (Text.intercalate "\n" cleaned)
  where
    go l acc = case IntMap.lookup l (scanLineComments scan) of
      Just comment -> go (l - 1) (stripMarker comment : acc)
      Nothing -> acc
    stripMarker t =
      let body = Text.dropWhile (== '#') t
       in Text.stripEnd (fromMaybe body (Text.stripPrefix " " body))

docCommentForOffset :: Scan -> Int -> Maybe Text
docCommentForOffset scan off = docCommentAtLine scan (lineOfOffset (scanLineIndex scan) off)
