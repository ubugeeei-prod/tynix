{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.List (sort)
import SessionWorkspace
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    getTemporaryDirectory,
    removeFile,
    removePathForcibly,
  )
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ignoredDirectory" $ do
    it "skips conventional infrastructure directories" $ do
      ignoredDirectory ".git" `shouldBe` True
      ignoredDirectory ".direnv" `shouldBe` True
      ignoredDirectory ".devenv" `shouldBe` True
      ignoredDirectory "node_modules" `shouldBe` True
      ignoredDirectory "dist" `shouldBe` True

    it "treats any `result*` symlink as a Nix build artifact" $ do
      ignoredDirectory "result" `shouldBe` True
      ignoredDirectory "result-doc" `shouldBe` True
      ignoredDirectory "result-1" `shouldBe` True

    it "allows ordinary project directories" $ do
      ignoredDirectory "src" `shouldBe` False
      ignoredDirectory "lib" `shouldBe` False
      ignoredDirectory "tests" `shouldBe` False

  describe "isSourceFile" $ do
    it "accepts .tynix and .d.tynix files" $ do
      isSourceFile "main.tynix" `shouldBe` True
      isSourceFile "lib.d.tynix" `shouldBe` True

    it "rejects unrelated extensions" $ do
      isSourceFile "main.nix" `shouldBe` False
      isSourceFile "README.md" `shouldBe` False

  describe "hasWorkspaceMarker" $ do
    it "returns True when any project marker is present" $
      withTempTree
        [("flake.nix", "")]
        (\root -> hasWorkspaceMarker root `shouldReturn` True)

    it "returns True for tynix.config.tynix" $
      withTempTree
        [("tynix.config.tynix", "{}")]
        (\root -> hasWorkspaceMarker root `shouldReturn` True)

    it "returns False for an unmarked directory" $
      withTempTree
        [("hello.txt", "")]
        (\root -> hasWorkspaceMarker root `shouldReturn` False)

  describe "findWorkspaceRoot" $ do
    it "walks up to the marker directory" $
      withTempTree
        [("flake.nix", ""), ("nested/inner/main.tynix", "1")]
        ( \root -> do
            actual <- findWorkspaceRoot (root </> "nested/inner/main.tynix")
            actual `shouldBe` root
        )

    it "falls back to the file's directory when no marker is found" $
      withTempTree
        [("orphan/main.tynix", "1")]
        ( \root -> do
            actual <- findWorkspaceRoot (root </> "orphan/main.tynix")
            actual `shouldBe` (root </> "orphan")
        )

  describe "findBuiltinsFile" $ do
    it "finds builtins.d.tynix in the same directory" $
      withTempTree
        [("project/builtins.d.tynix", ""), ("project/main.tynix", "1")]
        ( \root -> do
            actual <- findBuiltinsFile (root </> "project/main.tynix")
            actual `shouldBe` Just (root </> "project/builtins.d.tynix")
        )

    it "walks up parent directories to find builtins.d.tynix" $
      withTempTree
        [("workspace/builtins.d.tynix", ""), ("workspace/src/main.tynix", "1")]
        ( \root -> do
            actual <- findBuiltinsFile (root </> "workspace/src/main.tynix")
            actual `shouldBe` Just (root </> "workspace/builtins.d.tynix")
        )

    it "returns Nothing when no builtins.d.tynix exists anywhere on the path" $
      withTempTree
        [("workspace/src/main.tynix", "1")]
        ( \root -> do
            actual <- findBuiltinsFile (root </> "workspace/src/main.tynix")
            actual `shouldBe` Nothing
        )

  describe "workspaceFilesFor" $ do
    it "returns only the requested file when the workspace has no marker" $
      withTempTree
        [("orphan/main.tynix", "1"), ("orphan/lib.tynix", "2")]
        ( \root -> do
            files <- workspaceFilesFor (root </> "orphan/main.tynix")
            files `shouldBe` [root </> "orphan/main.tynix"]
        )

    it "walks the workspace when a marker is present, skipping ignored dirs" $
      withTempTree
        [ ("flake.nix", ""),
          ("src/a.tynix", "1"),
          ("src/nested/b.tynix", "2"),
          ("dist/built.tynix", "ignored"),
          ("node_modules/dep.tynix", "ignored"),
          ("result-doc/inside.tynix", "ignored")
        ]
        ( \root -> do
            files <- workspaceFilesFor (root </> "src/a.tynix")
            sort files
              `shouldBe` sort
                [ root </> "src/a.tynix",
                  root </> "src/nested/b.tynix"
                ]
        )

withTempTree :: [(FilePath, String)] -> (FilePath -> IO a) -> IO a
withTempTree files action =
  bracket createRoot removePathForcibly (\root -> writeTree root >> action root)
  where
    createRoot = do
      tmp <- getTemporaryDirectory
      (path, handle) <- openTempFile tmp "tynix-session-workspace-spec"
      hClose handle
      removeFile path
      createDirectory path
      pure path
    writeTree root =
      forM_ files $ \(relative, content) -> do
        let path = root </> relative
        createDirectoryIfMissing True (takeDirectory path)
        writeFile path content
