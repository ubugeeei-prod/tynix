{-# LANGUAGE OverloadedStrings #-}

module TestSupport
  ( expectLeftContaining,
    expectRight,
    fixturePath,
    fixturePathCandidates,
    source,
    withCopiedFixtureTree,
    withTempTree,
  )
where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory
  ( copyFile,
    createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    getTemporaryDirectory,
    listDirectory,
    removeFile,
    removePathForcibly,
  )
import System.FilePath (normalise, takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import Test.Hspec (Expectation, expectationFailure)

expectRight :: (Show e) => Either e a -> IO a
expectRight (Right value) = pure value
expectRight (Left err) =
  expectationFailure ("expected Right, got Left: " <> show err)
    >> fail "expected Right"

expectLeftContaining :: Either String a -> String -> Expectation
expectLeftContaining result needle =
  case result of
    Left err | Text.pack needle `Text.isInfixOf` Text.pack err -> pure ()
    Left err ->
      expectationFailure
        ("expected error containing " <> show needle <> ", got " <> show err)
    Right _ ->
      expectationFailure ("expected Left containing " <> show needle <> ", got Right")

source :: [Text] -> Text
source = Text.unlines

fixturePath :: FilePath -> IO FilePath
fixturePath relative = fixturePathCandidates [relative]

fixturePathCandidates :: [FilePath] -> IO FilePath
fixturePathCandidates candidates = do
  cwd <- getCurrentDirectory
  findFixtureRoot (normalise cwd)
  where
    findFixtureRoot dir = do
      existing <- firstExistingPath dir candidates
      case existing of
        Just path -> pure path
        Nothing ->
          let parent = normalise (takeDirectory dir)
           in if parent == dir
                then
                  expectationFailure
                    ("could not locate fixture path in ancestors: " <> show candidates)
                    >> fail "missing fixture path"
                else findFixtureRoot parent

    firstExistingPath _ [] = pure Nothing
    firstExistingPath dir (candidate : rest) = do
      let path = dir </> candidate
      fileExists <- doesFileExist path
      dirExists <- doesDirectoryExist path
      if fileExists || dirExists
        then pure (Just path)
        else firstExistingPath dir rest

withTempTree :: [(FilePath, Text)] -> (FilePath -> IO a) -> IO a
withTempTree files action =
  bracket createRoot removePathForcibly (\root -> writeTree root >> action root)
  where
    createRoot = do
      tmp <- getTemporaryDirectory
      (path, handle) <- openTempFile tmp "tynix-test"
      hClose handle
      removeFile path
      createDirectory path
      pure path
    writeTree root =
      forM_ files $ \(relative, content) -> do
        let path = root </> relative
        createDirectoryIfMissing True (takeDirectory path)
        Text.writeFile path content

withCopiedFixtureTree :: FilePath -> (FilePath -> IO a) -> IO a
withCopiedFixtureTree fixtureRoot action =
  withTempTree [("flake.nix", "{}")] $ \root -> do
    copyTree fixtureRoot root
    action root
  where
    copyTree sourceDir destDir = do
      names <- listDirectory sourceDir
      forM_ names $ \name -> do
        let sourcePath = sourceDir </> name
            destPath = destDir </> name
        isDir <- doesDirectoryExist sourcePath
        if isDir
          then do
            createDirectoryIfMissing True destPath
            copyTree sourcePath destPath
          else do
            createDirectoryIfMissing True (takeDirectory destPath)
            copyFile sourcePath destPath
