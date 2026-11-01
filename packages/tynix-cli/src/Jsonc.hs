{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A small, order-preserving JSON-with-comments (JSONC) reader and writer.
--
-- Editor settings files (VS Code, Cursor, VSCodium, Zed) are JSONC: they may
-- carry @//@ and @/* */@ comments and trailing commas. Aeson rejects both and
-- reorders object keys, which would scramble a user's hand-maintained settings
-- file. This module keeps key order intact, reports whether comments were seen
-- (so callers can refuse to silently drop them), and applies a handful of
-- idempotent edits.
module Jsonc
  ( JValue (..),
    JsonEdit (..),
    ParsedJson (..),
    applyJsonEdits,
    detectIndent,
    parseJsonc,
    renderJson,
  )
where

import Data.Bifunctor (first)
import Data.Char (isDigit, isHexDigit, isSpace, ord)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric (readHex, showHex)

-- | A JSON value whose objects remember the order their keys were written in.
data JValue
  = JObject [(Text, JValue)]
  | JArray [JValue]
  | JString Text
  | JNumber Text
  | JBool Bool
  | JNull
  deriving (Eq, Show)

data ParsedJson = ParsedJson
  { parsedValue :: JValue,
    -- | Whether the source contained comments that a rewrite would drop.
    parsedHasComments :: Bool
  }
  deriving (Eq, Show)

-- | Idempotent edits applied to a settings document.
data JsonEdit
  = -- | Set the value at a key path, creating intermediate objects.
    SetKey [Text] JValue
  | -- | Ensure the array at a key path contains a value, creating it if absent.
    AppendUnique [Text] JValue
  deriving (Eq, Show)

-- Parsing -----------------------------------------------------------------

newtype P a = P {runP :: (Text, Bool) -> Either String (a, (Text, Bool))}

instance Functor P where
  fmap f (P p) = P $ fmap (first f) . p

instance Applicative P where
  pure a = P $ \s -> Right (a, s)
  P pf <*> P pa = P $ \s -> do
    (f, s') <- pf s
    (a, s'') <- pa s'
    pure (f a, s'')

instance Monad P where
  P p >>= k = P $ \s -> do
    (a, s') <- p s
    runP (k a) s'

failP :: String -> P a
failP msg = P $ \(rest, _) -> Left (msg <> " near " <> show (Text.take 20 rest))

peek :: P (Maybe Char)
peek = P $ \s@(rest, _) -> Right (fst <$> Text.uncons rest, s)

advance :: Int -> P ()
advance n = P $ \(rest, comments) -> Right ((), (Text.drop n rest, comments))

remaining :: P Text
remaining = P $ \s@(rest, _) -> Right (rest, s)

markComment :: P ()
markComment = P $ \(rest, _) -> Right ((), (rest, True))

-- | Parse a JSONC document. An empty (or whitespace/comment-only) document is
-- read as an empty object, which is how editors treat a blank settings file.
parseJsonc :: Text -> Either String ParsedJson
parseJsonc input = do
  (value, (rest, comments)) <-
    runP document (Text.dropWhile (== '\xFEFF') input, False)
  if Text.null rest
    then Right (ParsedJson value comments)
    else Left ("unexpected trailing content near " <> show (Text.take 20 rest))
  where
    document = do
      skipTrivia
      next <- peek
      case next of
        Nothing -> pure (JObject [])
        Just _ -> do
          value <- jvalue
          skipTrivia
          pure value

skipTrivia :: P ()
skipTrivia = do
  rest <- remaining
  case Text.uncons rest of
    Just (c, _) | isSpace c -> advance 1 >> skipTrivia
    _
      | "//" `Text.isPrefixOf` rest -> do
          markComment
          advance (Text.length (Text.takeWhile (/= '\n') rest))
          skipTrivia
      | "/*" `Text.isPrefixOf` rest -> do
          markComment
          let (_, after) = Text.breakOn "*/" (Text.drop 2 rest)
          if Text.null after
            then failP "unterminated block comment"
            else advance (Text.length rest - Text.length after + 2) >> skipTrivia
      | otherwise -> pure ()

jvalue :: P JValue
jvalue = do
  next <- peek
  case next of
    Just '{' -> advance 1 >> object []
    Just '[' -> advance 1 >> array []
    Just '"' -> JString <$> stringLit
    Just c | c == '-' || isDigit c -> JNumber <$> number
    _ -> keyword
  where
    object acc = do
      skipTrivia
      next <- peek
      case next of
        Just '}' -> advance 1 >> pure (JObject (reverse acc))
        Just '"' -> do
          key <- stringLit
          skipTrivia
          expect ':'
          skipTrivia
          value <- jvalue
          skipTrivia
          separator '}'
          object ((key, value) : acc)
        _ -> failP "expected object key"
    array acc = do
      skipTrivia
      next <- peek
      case next of
        Just ']' -> advance 1 >> pure (JArray (reverse acc))
        _ -> do
          value <- jvalue
          skipTrivia
          separator ']'
          array (value : acc)
    -- A comma or the closing bracket; trailing commas are tolerated because
    -- the closing bracket is checked again by the caller's loop.
    separator close = do
      next <- peek
      case next of
        Just ',' -> advance 1
        Just c | c == close -> pure ()
        _ -> failP ("expected ',' or '" <> [close] <> "'")
    keyword = do
      rest <- remaining
      case () of
        _
          | "true" `Text.isPrefixOf` rest -> advance 4 >> pure (JBool True)
          | "false" `Text.isPrefixOf` rest -> advance 5 >> pure (JBool False)
          | "null" `Text.isPrefixOf` rest -> advance 4 >> pure JNull
          | otherwise -> failP "unexpected token"

expect :: Char -> P ()
expect c = do
  next <- peek
  if next == Just c then advance 1 else failP ("expected '" <> [c] <> "'")

number :: P Text
number = do
  rest <- remaining
  let lexeme = Text.takeWhile (\c -> isDigit c || c `elem` ("+-.eE" :: String)) rest
  if Text.null lexeme
    then failP "expected number"
    else advance (Text.length lexeme) >> pure lexeme

stringLit :: P Text
stringLit = expect '"' >> go []
  where
    go acc = do
      rest <- remaining
      let (chunk, after) = Text.break (\c -> c == '"' || c == '\\') rest
      advance (Text.length chunk)
      case Text.uncons after of
        Nothing -> failP "unterminated string"
        Just ('"', _) -> advance 1 >> pure (Text.concat (reverse (chunk : acc)))
        Just (_, escaped) ->
          case Text.uncons escaped of
            Just ('u', hex)
              | Text.length (Text.takeWhile isHexDigit (Text.take 4 hex)) == 4,
                [(code, "")] <- readHex (Text.unpack (Text.take 4 hex)) -> do
                  advance 6
                  go (Text.singleton (toEnum code) : chunk : acc)
            Just (e, _)
              | Just c <- lookup e simpleEscapes ->
                  advance 2 >> go (Text.singleton c : chunk : acc)
            _ -> failP "invalid string escape"
    simpleEscapes =
      [ ('"', '"'),
        ('\\', '\\'),
        ('/', '/'),
        ('b', '\b'),
        ('f', '\f'),
        ('n', '\n'),
        ('r', '\r'),
        ('t', '\t')
      ]

-- Editing -----------------------------------------------------------------

-- | Apply edits in order. Fails when a path runs through a non-object value or
-- an append targets something that is not an array, since overwriting those
-- would destroy user data.
applyJsonEdits :: [JsonEdit] -> JValue -> Either String JValue
applyJsonEdits edits root = List.foldl' (\acc edit -> acc >>= applyOne edit) (Right root) edits
  where
    applyOne = \case
      SetKey path value -> updateAt path (const (Right value))
      AppendUnique path value ->
        updateAt path $ \case
          Nothing -> Right (JArray [value])
          Just (JArray items)
            | value `elem` items -> Right (JArray items)
            | otherwise -> Right (JArray (items <> [value]))
          Just _ -> Left ("expected an array at " <> renderPath path)

updateAt ::
  [Text] ->
  (Maybe JValue -> Either String JValue) ->
  JValue ->
  Either String JValue
updateAt path f = go [] path
  where
    go seen keys value =
      case (keys, value) of
        ([], _) -> f (Just value)
        (key : more, JObject fields) ->
          case lookup key fields of
            Just existing -> do
              updated <- go (seen <> [key]) more existing
              Right (JObject [(k, if k == key then updated else v) | (k, v) <- fields])
            Nothing -> do
              created <- build more
              Right (JObject (fields <> [(key, created)]))
        (_, _) -> Left ("expected an object at " <> renderPath seen)
    build keys =
      case keys of
        [] -> f Nothing
        key : more -> (\v -> JObject [(key, v)]) <$> build more

renderPath :: [Text] -> String
renderPath [] = "the document root"
renderPath keys = Text.unpack (Text.intercalate " > " keys)

-- Rendering ---------------------------------------------------------------

-- | Guess the indentation unit of an existing JSON document (defaults to two
-- spaces) so rewrites keep the file's style.
detectIndent :: Text -> Text
detectIndent content =
  case [ Text.takeWhile isIndent line
       | line <- Text.lines content,
         let ws = Text.takeWhile isIndent line,
         not (Text.null ws),
         not (Text.null (Text.strip line))
       ] of
    indentation : _ -> indentation
    [] -> "  "
  where
    isIndent c = c == ' ' || c == '\t'

-- | Render a value with one key per line and a trailing newline.
renderJson :: Text -> JValue -> Text
renderJson unit value = go 0 value <> "\n"
  where
    pad n = Text.replicate n unit
    go depth = \case
      JObject [] -> "{}"
      JArray [] -> "[]"
      JObject fields ->
        "{\n"
          <> Text.intercalate
            ",\n"
            [pad (depth + 1) <> quote k <> ": " <> go (depth + 1) v | (k, v) <- fields]
          <> "\n"
          <> pad depth
          <> "}"
      JArray items ->
        "[\n"
          <> Text.intercalate ",\n" [pad (depth + 1) <> go (depth + 1) v | v <- items]
          <> "\n"
          <> pad depth
          <> "]"
      JString s -> quote s
      JNumber n -> n
      JBool True -> "true"
      JBool False -> "false"
      JNull -> "null"

quote :: Text -> Text
quote s = "\"" <> Text.concatMap escape s <> "\""
  where
    escape = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      c
        | ord c < 0x20 ->
            "\\u" <> Text.justifyRight 4 '0' (Text.pack (showHex (ord c) ""))
        | otherwise -> Text.singleton c
