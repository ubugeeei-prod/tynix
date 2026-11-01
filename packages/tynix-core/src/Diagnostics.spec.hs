{-# LANGUAGE OverloadedStrings #-}

-- | Catalogue tests for the stable diagnostic codes.
--
-- Codes are a public contract: editors match on them, CI logs quote them, and
-- `docs/diagnostics.md` explains them. The interesting failures are therefore
-- not about any single message but about the catalogue drifting — a code that
-- is documented but never produced, produced but never documented, or that
-- quietly changes its identifier.
module Main (main) where

import Check (CheckContext (..), CheckResult, checkProgram)
import Control.Monad (forM, forM_)
import Data.Char (isDigit, isUpper)
import Data.List (sort)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Diagnostics
import Kind (inferAliasKinds, inferTypeKind, validateProgramKinds)
import Parser (parseProgram)
import Syntax (Program (programAliases))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Hspec
import TestSupport (fixturePathCandidates, source)
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "code identifiers" $ do
    it "gives every catalogued code a distinct identifier" $ do
      let identifiers = map diagnosticCodeText allDiagnosticCodes
      length (Set.fromList identifiers) `shouldBe` length identifiers

    it "spells every identifier as two phase letters and four digits" $
      forM_ allDiagnosticCodes $ \code -> do
        let identifier = diagnosticCodeText code
        (code, length identifier) `shouldBe` (code, 6)
        (code, all isUpper (take 2 identifier)) `shouldBe` (code, True)
        (code, all isDigit (drop 2 identifier)) `shouldBe` (code, True)

    it "uses only the documented phase prefixes" $
      forM_ allDiagnosticCodes $ \code ->
        (code, take 2 (diagnosticCodeText code)) `shouldSatisfy` \(_, prefix) ->
          prefix `elem` ["TP", "TK", "TC", "TD", "TX"]

    it "names each constructor after the identifier it renders" $
      forM_ allDiagnosticCodes $ \code ->
        (code, take 6 (show code)) `shouldBe` (code, diagnosticCodeText code)

    it "prefixes messages in the canonical bracketed shape" $
      withCode TC0013TypeMismatch "type mismatch: Int vs String"
        `shouldBe` "[TC0013] type mismatch: Int vs String"

  describe "catalogue" $ do
    it "documents exactly the codes the compiler defines" $ do
      documented <- documentedCodes
      let defined = sort (map diagnosticCodeText allDiagnosticCodes)
      sort documented `shouldBe` defined

    it "emits every code it documents" $ do
      sources <- packageSources
      bodies <- forM sources Text.readFile
      let mentioned constructor = any (Text.isInfixOf constructor) bodies
          missing = [show code | code <- allDiagnosticCodes, not (mentioned (Text.pack (show code)))]
      missing `shouldBe` []

  describe "emitted messages" $ do
    it "tags parser directive failures" $ do
      expectCode "TP0001" (parseFailure (source ["let x = 1; in x", "# @tynix-ignore"]))
      expectCode
        "TP0002"
        (parseFailure (source ["# @tynix-ignore", "# @tynix-expected", "let x = 1; in x"]))

    it "tags plain syntax errors" $
      expectCode "TP0004" (parseFailure "let x = ;")

    it "tags kind mismatches" $ do
      expectCodeIn "TK0001" (inferAliasKinds [plainAlias "Bad" ["a"] (TApp tInt (TVar "a"))])
      expectCodeIn "TK0001" (inferTypeKind mempty (TApp (tList tInt) tString))

    it "tags the kind occurs check" $
      expectCodeIn "TK0002" (inferAliasKinds [plainAlias "Loop" ["f"] (TApp (TVar "f") (TVar "f"))])

    it "tags term annotations that do not resolve to Type" $ do
      program <-
        either (\err -> expectationFailure (Text.unpack err) >> fail "parse failed") pure $
          parseProgram "kinds.tynix" (source ["let value :: List;", "    value = [];", "in value"])
      expectCodeIn "TK0003" (validateProgramKinds (programAliases program) program)

    it "tags type-checker failures" $
      expectCodeIn "TC0001" (checkSource (source ["missing"]))

expectCode :: Text -> Maybe Text -> Expectation
expectCode code failure =
  case failure of
    Nothing -> expectationFailure ("expected a failure carrying " <> Text.unpack code)
    Just message -> message `shouldSatisfy` Text.isInfixOf ("[" <> code <> "]")

expectCodeIn :: Text -> Either String a -> Expectation
expectCodeIn code result =
  case result of
    Right _ -> expectationFailure ("expected a failure carrying " <> Text.unpack code)
    Left message -> Text.pack message `shouldSatisfy` Text.isInfixOf ("[" <> code <> "]")

-- | Parse and check a buffer with no ambient world, returning the first error.
checkSource :: Text -> Either String CheckResult
checkSource input = do
  program <- either (Left . Text.unpack) Right (parseProgram "codes.tynix" input)
  checkProgram
    CheckContext
      { checkAliases = mempty,
        checkAmbient = mempty,
        checkFile = "codes.tynix",
        checkOpenScope = False,
        checkPureEval = False
      }
    program

parseFailure :: Text -> Maybe Text
parseFailure input = either Just (const Nothing) (parseProgram "codes.tynix" input)

-- | Read the code identifiers documented in @docs/diagnostics.md@.
--
-- Entries look like @### `TC0013` — type mismatch@, so the identifier is the
-- backtick-delimited run right after the heading marker.
documentedCodes :: IO [String]
documentedCodes = do
  path <- fixturePathCandidates ["docs/diagnostics.md"]
  contents <- Text.readFile path
  pure
    [ Text.unpack identifier
    | line <- Text.lines contents,
      Just rest <- [Text.stripPrefix "### `" line],
      let identifier = Text.takeWhile (/= '`') rest
    ]

-- | Every non-test Haskell source that could emit a diagnostic, minus the
-- catalogue itself.
--
-- Scanning the sources is what makes "no dead diagnostics" checkable: a code
-- can be defined and documented while nothing ever produces it, which is
-- exactly the drift this suite exists to catch. Only `tynix-core` builds
-- diagnostics, and it is the one tree reachable from every place these tests
-- run, including the sandboxed `nix flake check` build.
packageSources :: IO [FilePath]
packageSources = do
  root <- fixturePathCandidates ["src", "packages/tynix-core/src"]
  paths <- walk root
  pure
    [ path
    | path <- paths,
      takeExtension path == ".hs",
      not (".spec.hs" `isSuffixOfPath` path),
      not ("Diagnostics.hs" `isSuffixOfPath` path)
    ]
  where
    isSuffixOfPath needle path = needle `Text.isSuffixOf` Text.pack path
    walk dir = do
      names <- listDirectory dir
      fmap concat $ forM names $ \name -> do
        let path = dir </> name
        isDir <- doesDirectoryExist path
        if isDir then walk path else pure [path]
