{-# LANGUAGE OverloadedStrings #-}

-- | Top-level parsing entry point.
--
-- This wrapper converts Megaparsec's rich diagnostic bundle into plain text so
-- the rest of the pipeline can forward parse errors through the CLI, LSP, and
-- tests without depending on parser-specific types.
--
-- 'parseProgramDetailed' also exposes a small structured error type so editors
-- can attach diagnostics to the exact line/column instead of column 0.
module Parser
  ( ParseError (..),
    parseProgram,
    parseProgramDetailed,
    parseProgramLocated,
    parseProgramLocatedDetailed,
    stripProgramLocations,
  )
where

import Control.Monad.Reader (runReaderT)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Diagnostics (DiagnosticCode (..), withCode)
import Macro (strayPlaceholders)
import ParserExpr
import ParserLexer
import Syntax
import Text.Megaparsec
  ( ParseErrorBundle,
    PosState (..),
    ShowErrorComponent,
    SourcePos (..),
    TraversableStream,
    VisualStream,
    bundleErrors,
    bundlePosState,
    eof,
    errorBundlePretty,
    errorOffset,
    parseErrorTextPretty,
    reachOffset,
    runParser,
    unPos,
  )

-- | Structured parser diagnostic.
--
-- All offsets are 1-based to match what editors and humans expect; the LSP
-- conversion subtracts 1 when it builds a 0-based LSP `Range`.
data ParseError = ParseError
  { parseErrorLine :: Int,
    parseErrorColumn :: Int,
    parseErrorMessage :: Text
  }
  deriving (Eq, Show)

-- | Parse a complete tynix program, returning a pretty-printed error message
-- on failure. Kept as the back-compatible entry point used by the rest of
-- the pipeline.
parseProgram :: FilePath -> Text -> Either Text Program
parseProgram path = either (Left . renderParseError) Right . parseProgramDetailed path

-- | Parse a program keeping 'ELoc' source spans on expressions. The checker
-- uses these to attach diagnostics to precise source regions.
parseProgramLocated :: FilePath -> Text -> Either Text Program
parseProgramLocated path = either (Left . renderParseError) Right . parseProgramLocatedDetailed path

-- | Remove every 'ELoc' wrapper from a program.
stripProgramLocations :: Program -> Program
stripProgramLocations program =
  program{programExpr = fmap stripLocations <$> programExpr program}

renderParseError :: ParseError -> Text
renderParseError err =
  Text.pack (show (parseErrorLine err))
    <> ":"
    <> Text.pack (show (parseErrorColumn err))
    <> ": "
    <> parseErrorMessage err

-- | Parse a complete tynix program with structured location info on failure.
parseProgramDetailed :: FilePath -> Text -> Either ParseError Program
parseProgramDetailed path input = stripProgramLocations <$> parseProgramLocatedDetailed path input

-- | 'parseProgramDetailed' without stripping source spans.
parseProgramLocatedDetailed :: FilePath -> Text -> Either ParseError Program
parseProgramLocatedDetailed path input = do
  directives <- mapDirectiveError input (scanDiagnosticDirectives input)
  case runParser (runReaderT (sc *> programParser <* eof) mempty{envDirectives = directives}) path input of
    Left bundle -> Left (megaparsecError bundle)
    Right program ->
      case foldMap (strayPlaceholders . markedValue) (programExpr program) of
        [] -> Right program
        stray : _ ->
          Left
            ParseError
              { parseErrorLine = 1,
                parseErrorColumn = 1,
                parseErrorMessage =
                  withCodeText TX0006StrayMacroVariable ("macro metavariable `" <> stray <> "` used outside a macro template")
              }

mapDirectiveError :: Text -> Either Text a -> Either ParseError a
mapDirectiveError _ (Right a) = Right a
mapDirectiveError _ (Left message) =
  Left ParseError{parseErrorLine = 1, parseErrorColumn = 1, parseErrorMessage = message}

-- | Lift the first error in a Megaparsec bundle into our structured form,
-- recovering the offending source line/column from the parser's PosState.
megaparsecError :: (TraversableStream s, VisualStream s, ShowErrorComponent e) => ParseErrorBundle s e -> ParseError
megaparsecError bundle =
  let firstError :| _ = bundleErrors bundle
      (_, finalState) = reachOffset (errorOffset firstError) (bundlePosState bundle)
      SourcePos _ srcLine srcColumn = pstateSourcePos finalState
      humanMessage = Text.pack (errorBundlePretty bundle)
      compactMessage = Text.pack (parseErrorTextPretty firstError)
      message
        -- Macro expansion failures already carry their own code.
        | "[TX" `Text.isPrefixOf` compactMessage = Text.strip compactMessage
        | otherwise =
            withCodeText TP0004ParseError $
              if Text.null humanMessage
                then compactMessage
                else humanMessage
   in ParseError
        { parseErrorLine = unPos srcLine,
          parseErrorColumn = unPos srcColumn,
          parseErrorMessage = message
        }

scanDiagnosticDirectives :: Text -> Either Text DirectiveTargets
scanDiagnosticDirectives input = go 1 Nothing Map.empty (Text.lines input)
  where
    go _ Nothing acc [] = Right acc
    go lineNo (Just _) _ [] =
      Left (withCodeText TP0001DanglingDirective ("dangling tynix diagnostic directive before end of file at line " <> Text.pack (show (lineNo - 1))))
    go lineNo pending acc (line : rest) =
      case parseDirective line of
        Just directive ->
          case pending of
            Just _ ->
              Left (withCodeText TP0002DuplicateDirectiveSameLine ("multiple tynix diagnostic directives target the same next line before line " <> Text.pack (show lineNo)))
            Nothing -> go (lineNo + 1) (Just directive) acc rest
        Nothing
          | isSkippableLine line -> go (lineNo + 1) pending acc rest
          | otherwise ->
              case pending of
                Nothing -> go (lineNo + 1) Nothing acc rest
                Just directive ->
                  case Map.lookup lineNo acc of
                    Just _ ->
                      Left (withCodeText TP0003DuplicateDirectivePerLine ("duplicate tynix diagnostic directives for line " <> Text.pack (show lineNo)))
                    Nothing -> go (lineNo + 1) Nothing (Map.insert lineNo directive acc) rest

parseDirective :: Text -> Maybe DiagnosticDirective
parseDirective raw =
  case Text.stripStart raw of
    trimmed
      | Just rest <- Text.stripPrefix "#" trimmed ->
          let body = Text.stripStart rest
           in if "@tynix-ignore" `Text.isPrefixOf` body
                then Just TynixIgnore
                else
                  if "@tynix-expected" `Text.isPrefixOf` body
                    then Just TynixExpected
                    else Nothing
      | otherwise -> Nothing

isSkippableLine :: Text -> Bool
isSkippableLine raw =
  case Text.strip raw of
    "" -> True
    trimmed -> "#" `Text.isPrefixOf` trimmed

-- | 'Diagnostics.withCode' for the 'Text'-shaped messages the parser produces.
withCodeText :: DiagnosticCode -> Text -> Text
withCodeText code message = Text.pack (withCode code "") <> message
