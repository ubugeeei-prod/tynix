{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the checker's own surface.
--
-- End-to-end behaviour lives in `Driver.spec`; this suite covers the pieces
-- that are easier to pin down directly: import path resolution (which the
-- driver must agree with exactly, or ambient declarations stop matching the
-- imports they describe) and the shape of a 'CheckResult'.
module Main (main) where

import Check
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Parser (parseProgram)
import Pretty (renderScheme)
import System.FilePath (isAbsolute, splitDirectories)
import Test.Hspec
import Test.QuickCheck
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "collapseParentSegments" $ do
    it "drops redundant current-directory segments" $ do
      collapseParentSegments "./a/./b" `shouldBe` "a/b"
      collapseParentSegments "a/././b" `shouldBe` "a/b"

    it "cancels a parent segment against the segment before it" $ do
      collapseParentSegments "a/b/../c" `shouldBe` "a/c"
      collapseParentSegments "a/b/c/../.." `shouldBe` "a"
      collapseParentSegments "/root/a/../b" `shouldBe` "/root/b"

    it "never climbs above an absolute root" $ do
      collapseParentSegments "/.." `shouldBe` "/"
      collapseParentSegments "/../.." `shouldBe` "/"
      collapseParentSegments "/a/../../.." `shouldBe` "/"

    it "keeps a leading parent segment on a relative path" $
      collapseParentSegments "../a" `shouldBe` "../a"

    it "is idempotent" $
      property $ \(PathSegments segments) ->
        let path = joinSegments segments
         in collapseParentSegments (collapseParentSegments path) === collapseParentSegments path

    it "leaves no resolvable parent segment in an absolute result" $
      property $ \(PathSegments segments) ->
        let path = "/" <> joinSegments segments
            collapsed = collapseParentSegments path
         in counterexample collapsed $
              splitDirectories collapsed `shouldNotContain` [".."]

    it "keeps absolute paths absolute and relative paths relative" $
      property $ \(PathSegments segments) ->
        let relative = joinSegments segments
         in isAbsolute (collapseParentSegments ("/" <> relative))
              .&&. not (isAbsolute (collapseParentSegments relative))

  describe "resolvePath" $ do
    it "resolves a relative import against the importing file's directory" $
      resolvePath "src/app/main.tynix" "./lib.nix" `shouldBe` "src/app/lib.nix"

    it "walks out of the importing directory for parent-relative imports" $
      resolvePath "src/app/main.tynix" "../shared/lib.nix" `shouldBe` "src/shared/lib.nix"

    it "normalizes an absolute import without consulting the importer" $
      resolvePath "src/app/main.tynix" "/opt/pkgs/./lib.nix" `shouldBe` "/opt/pkgs/lib.nix"

    it "agrees for a file and the same file reached through a parent segment" $
      resolvePath "src/app/main.tynix" "./lib.nix"
        `shouldBe` resolvePath "src/app/nested/../main.tynix" "./lib.nix"

  describe "checkProgram" $ do
    it "returns no root for a declaration-only program" $ do
      result <- checkSource (source ["type Id a = a;", "declare \"./lib.nix\" { default :: Id Int; };"])
      resultRoot result `shouldBe` Nothing
      resultBindings result `shouldBe` Map.empty

    it "reports the final type of every let binding" $ do
      result <- checkSource (source ["let", "  name = \"tynix\";", "  count = 1 + 1;", "in count"])
      fmap renderScheme (Map.lookup "name" (resultBindings result)) `shouldBe` Just "\"tynix\""
      fmap renderScheme (Map.lookup "count" (resultBindings result)) `shouldBe` Just "Int"

    it "keeps a declared signature rather than the inferred body type" $ do
      result <- checkSource (source ["let", "  id :: forall a. a -> a;", "  id = x: x;", "in id"])
      fmap renderScheme (Map.lookup "id" (resultBindings result)) `shouldBe` Just "forall a. a -> a"

    it "leaves bindings empty when the root is not a let expression" $ do
      result <- checkSource "{ value = 1; }"
      resultBindings result `shouldBe` Map.empty
      fmap renderScheme (resultRoot result) `shouldBe` Just "{\n  value :: 1;\n}"

    it "treats unresolved names as dynamic inside an open scope" $ do
      result <- checkSourceWith (defaultContext{checkOpenScope = True}) "mystery"
      fmap renderScheme (resultRoot result) `shouldBe` Just "dynamic"

    it "still rejects unresolved names in a closed scope" $
      checkEither defaultContext "mystery"
        `shouldSatisfy` either (Text.isInfixOf "[TC0001]" . Text.pack) (const False)

    it "reads the builtins scheme out of the ambient world" $ do
      let ambient = Map.fromList [("builtins", Scheme [] (TRecord (Map.fromList [("length", TFun Many (tList tInt) tInt)])))]
      result <- checkSourceWith defaultContext{checkAmbient = ambient} "builtins.length"
      fmap renderScheme (resultRoot result) `shouldBe` Just "List Int -> Int"

defaultContext :: CheckContext
defaultContext =
  CheckContext
    { checkAliases = mempty,
      checkAmbient = mempty,
      checkFile = "check.tynix",
      checkOpenScope = False,
      checkPureEval = False
    }

checkEither :: CheckContext -> Text -> Either String CheckResult
checkEither ctx input = do
  program <- either (Left . Text.unpack) Right (parseProgram (checkFile ctx) input)
  checkProgram ctx program

checkSourceWith :: CheckContext -> Text -> IO CheckResult
checkSourceWith ctx input =
  case checkEither ctx input of
    Left err -> expectationFailure err >> fail "check failed"
    Right result -> pure result

checkSource :: Text -> IO CheckResult
checkSource = checkSourceWith defaultContext

source :: [Text] -> Text
source = Text.unlines

-- | Path segments drawn from a pool that makes `.` and `..` common, so the
-- generated paths actually exercise the collapsing rules.
newtype PathSegments = PathSegments [String]
  deriving (Show)

instance Arbitrary PathSegments where
  arbitrary = PathSegments <$> resize 8 (listOf (elements ["a", "b", "c", ".", "..", "dir"]))
  shrink (PathSegments segments) = PathSegments <$> shrinkList (const []) segments

joinSegments :: [String] -> FilePath
joinSegments [] = "."
joinSegments segments = concatWith "/" segments
  where
    concatWith _ [] = ""
    concatWith _ [x] = x
    concatWith sep (x : xs) = x <> sep <> concatWith sep xs
