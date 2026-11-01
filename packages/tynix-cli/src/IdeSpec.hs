{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Specs for `tynix ide` and `tynix doctor`. Every external process goes
-- through a fake 'IdeEnv', so no real editor is touched.
module IdeSpec (ideSpec) where

import Control.Exception (bracket)
import Data.Either (fromLeft)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Doctor
import Ide
import Jsonc
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    doesFileExist,
    getTemporaryDirectory,
    removeFile,
    removePathForcibly,
  )
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import Test.Hspec

ideSpec :: Spec
ideSpec = do
  describe "Jsonc" $ do
    it "reads comments and trailing commas and remembers that comments were present" $ do
      let parsed =
            parseJsonc
              "// header\n{\n  \"b\": 1, /* inline */\n  \"a\": [true, null,],\n}\n"
      parsed
        `shouldBe` Right
          ( ParsedJson
              (JObject [("b", JNumber "1"), ("a", JArray [JBool True, JNull])])
              True
          )

    it
      "reads plain JSON without flagging comments, and comment markers inside strings are text"
      $ parseJsonc "{\"url\": \"http://x//y\", \"s\": \"a\\\"b\\u0041\"}"
        `shouldBe` Right
          ( ParsedJson
              (JObject [("url", JString "http://x//y"), ("s", JString "a\"bA")])
              False
          )

    it "treats an empty document as an empty object" $
      parsedValue <$> parseJsonc "  \n" `shouldBe` Right (JObject [])

    it "rejects malformed documents" $ do
      parseJsonc "{\"a\": }" `shouldSatisfy` isLeft
      parseJsonc "{\"a\": 1} trailing" `shouldSatisfy` isLeft
      parseJsonc "{ /* never closed" `shouldSatisfy` isLeft

    it "renders with stable key order and escapes strings" $
      renderJson
        "  "
        ( JObject
            [("z", JString "a\"b\n"), ("a", JArray [JNumber "1"]), ("e", JObject [])]
        )
        `shouldBe` "{\n  \"z\": \"a\\\"b\\n\",\n  \"a\": [\n    1\n  ],\n  \"e\": {}\n}\n"

    it "round-trips its own output" $ do
      let value = JObject [("k", JObject [("n", JNumber "-1.5e3"), ("t", JString "ü")])]
      parsedValue <$> parseJsonc (renderJson "\t" value) `shouldBe` Right value

    it "detects tab and space indentation" $ do
      detectIndent "{\n\t\"a\": 1\n}" `shouldBe` "\t"
      detectIndent "{\n    \"a\": 1\n}" `shouldBe` "    "
      detectIndent "{}" `shouldBe` "  "

  describe "applyJsonEdits" $ do
    it "creates nested objects and keeps unrelated keys in place" $
      applyJsonEdits
        [SetKey ["files.associations", "*.tynix"] (JString "tynix")]
        ( JObject
            [("x", JNumber "1"), ("files.associations", JObject [("*.foo", JString "bar")])]
        )
        `shouldBe` Right
          ( JObject
              [ ("x", JNumber "1"),
                ( "files.associations",
                  JObject [("*.foo", JString "bar"), ("*.tynix", JString "tynix")]
                )
              ]
          )

    it "appends to arrays only once" $ do
      let edit = [AppendUnique ["recommendations"] (JString "ubugeeei.tynix")]
          once = applyJsonEdits edit (JObject [("recommendations", JArray [JString "other"])])
      once
        `shouldBe` Right
          ( JObject
              [("recommendations", JArray [JString "other", JString "ubugeeei.tynix"])]
          )
      (once >>= applyJsonEdits edit) `shouldBe` once

    it "refuses to overwrite values of the wrong shape" $ do
      applyJsonEdits
        [SetKey ["lsp", "tynix-lsp"] JNull]
        (JObject [("lsp", JString "oops")])
        `shouldSatisfy` isLeft
      applyJsonEdits
        [AppendUnique ["recommendations"] JNull]
        (JObject [("recommendations", JString "x")])
        `shouldSatisfy` isLeft

  describe "settings content" $ do
    it "uses the VS Code extension's real configuration keys" $ do
      vscodeSettingsEdits (Just "/bin/tynix-lsp")
        `shouldBe` [ SetKey ["tynix.server.path"] (JString "/bin/tynix-lsp"),
                     SetKey ["files.associations", "*.tynix"] (JString "tynix"),
                     SetKey ["files.associations", "*.d.tynix"] (JString "tynix")
                   ]
      vscodeSettingsEdits Nothing `shouldSatisfy` (not . any isServerPath)

    it "targets the Zed language-server id from extension.toml" $
      zedSettingsEdits (Just "/bin/tynix-lsp")
        `shouldBe` [ SetKey ["auto_install_extensions", "tynix"] (JBool True),
                     SetKey ["lsp", "tynix-lsp", "binary", "path"] (JString "/bin/tynix-lsp")
                   ]

    it "generates a Neovim config that registers the filetype and the server" $ do
      let lua = neovimSnippet (Just "/bin/tynix-lsp")
      lua `shouldSatisfy` Text.isInfixOf "extension = { tynix = \"tynix\" }"
      lua `shouldSatisfy` Text.isInfixOf "local cmd = { \"/bin/tynix-lsp\" }"
      lua `shouldSatisfy` Text.isInfixOf "vim.lsp.enable(\"tynix\")"

    it "generates Helix language and server tables" $ do
      let (language, server) = helixSnippet "/bin/tynix-lsp"
      language `shouldSatisfy` Text.isInfixOf "language-servers = [\"tynix-lsp\"]"
      server
        `shouldSatisfy` Text.isInfixOf "[language-server.tynix-lsp]\ncommand = \"/bin/tynix-lsp\""

  describe "planJsonFile" $ do
    let edits = vscodeSettingsEdits (Just "/bin/tynix-lsp")
    it "creates a missing file" $
      case planJsonFile False "s.json" Nothing edits of
        FileCreate _ content ->
          content
            `shouldSatisfy` Text.isInfixOf "\"tynix.server.path\": \"/bin/tynix-lsp\""
        other -> expectationFailure (show other)

    it
      "merges into an existing file, keeping its keys and indentation, then is a no-op"
      $ do
        let existing =
              "{\n\t\"editor.fontSize\": 14,\n\t\"files.associations\": {\"*.foo\": \"bar\"},\n}\n"
        case planJsonFile False "s.json" (Just existing) edits of
          FileUpdate _ old new Nothing -> do
            old `shouldBe` existing
            new
              `shouldSatisfy` Text.isPrefixOf
                "{\n\t\"editor.fontSize\": 14,\n\t\"files.associations\": {\n\t\t\"*.foo\": \"bar\",\n\t\t\"*.tynix\": \"tynix\""
            planJsonFile False "s.json" (Just new) edits `shouldBe` FileUnchanged "s.json"
          other -> expectationFailure (show other)

    it "leaves commented files alone unless forced, and backs them up when forced" $ do
      let existing = "// mine\n{\"a\": 1}\n"
      case planJsonFile False "s.json" (Just existing) edits of
        FileManual _ reason snippet -> do
          reason `shouldSatisfy` Text.isInfixOf "comments"
          snippet `shouldSatisfy` Text.isInfixOf "tynix.server.path"
        other -> expectationFailure (show other)
      case planJsonFile True "s.json" (Just existing) edits of
        FileUpdate _ _ _ backup -> backup `shouldBe` Just "s.json.bak"
        other -> expectationFailure (show other)

    it "never rewrites a commented file that already has the settings" $ do
      let existing =
            "// mine\n{\"tynix.server.path\": \"/bin/tynix-lsp\", \"files.associations\": {\"*.tynix\": \"tynix\", \"*.d.tynix\": \"tynix\"}}"
      planJsonFile False "s.json" (Just existing) edits
        `shouldBe` FileUnchanged "s.json"

    it "falls back to a manual snippet for unparseable files" $
      case planJsonFile True "s.json" (Just "{ not json") edits of
        FileManual _ reason _ -> reason `shouldSatisfy` Text.isInfixOf "could not parse"
        other -> expectationFailure (show other)

  describe "planHelixFile" $ do
    it "creates languages.toml with both tables" $
      case planHelixFile "languages.toml" Nothing "tynix-lsp" of
        FileCreate _ content -> do
          content `shouldSatisfy` Text.isInfixOf "name = \"tynix\""
          content `shouldSatisfy` Text.isInfixOf "[language-server.tynix-lsp]"
        other -> expectationFailure (show other)

    it "appends only the missing table and is idempotent" $ do
      let existing =
            "[[language]]\nname = \"nix\"\n\n[language-server.tynix-lsp]\ncommand = \"custom\"\n"
      case planHelixFile "languages.toml" (Just existing) "tynix-lsp" of
        FileUpdate _ _ new _ -> do
          new `shouldSatisfy` Text.isPrefixOf existing
          Text.count "[language-server.tynix-lsp]" new `shouldBe` 1
          new `shouldSatisfy` Text.isInfixOf "name = \"tynix\""
          planHelixFile "languages.toml" (Just new) "tynix-lsp"
            `shouldBe` FileUnchanged "languages.toml"
        other -> expectationFailure (show other)

    it "recognises an inline server definition under [language-server]" $
      planHelixFile
        "l.toml"
        ( Just
            "[[language]]\nname = \"tynix\" # mine\n[language-server]\ntynix-lsp = { command = \"x\" }\n"
        )
        "tynix-lsp"
        `shouldBe` FileUnchanged "l.toml"

  describe "planManagedFile" $ do
    it "creates, refreshes its own files, and protects user files" $ do
      let content = neovimSnippet Nothing
      planManagedFile False "t.lua" Nothing content
        `shouldBe` FileCreate "t.lua" content
      planManagedFile False "t.lua" (Just content) content
        `shouldBe` FileUnchanged "t.lua"
      planManagedFile False "t.lua" (Just (neovimSnippet (Just "/old"))) content
        `shouldBe` FileUpdate "t.lua" (neovimSnippet (Just "/old")) content Nothing
      planManagedFile False "t.lua" (Just "-- mine") content `shouldSatisfy` isManual
      planManagedFile True "t.lua" (Just "-- mine") content
        `shouldBe` FileUpdate "t.lua" "-- mine" content (Just "t.lua.bak")

  describe "lineDiff" $
    it "marks additions and removals with one line of context" $
      lineDiff "a\nb\nc\nd\ne\n" "a\nb\nC\nd\ne\n"
        `shouldBe` ["  ...", "  b", "- c", "+ C", "  d", "  ..."]

  describe "planInstall" $ do
    it "installs the VS Code extension through the code CLI when it is missing"
      $ withFakeEnv
        [("code", "/bin/code"), ("tynix-lsp", "/opt/bin/tynix-lsp")]
        Map.empty
      $ \env calls -> do
        (_, steps) <- planInstall env (defaultInstallOptions VSCode)
        steps
          `shouldSatisfy` elem
            ( StepRun
                "/bin/code"
                ["--install-extension", "ubugeeei.tynix"]
                "install extension ubugeeei.tynix"
            )
        readIORef calls `shouldReturn` [("/bin/code", ["--list-extensions"])]

    it "skips the install when the extension is already present"
      $ withFakeEnv
        [("cursor", "/bin/cursor")]
        (Map.fromList [("/bin/cursor", (ExitSuccess, "foo.bar\nUbugeeei.tynix\n"))])
      $ \env _ -> do
        (_, steps) <- planInstall env (defaultInstallOptions Cursor)
        steps
          `shouldSatisfy` elem (StepOk "extension ubugeeei.tynix is already installed")
        steps `shouldSatisfy` (not . any isRun)

    it "explains how to install manually when the editor CLI is missing" $
      withFakeEnv [] Map.empty $ \env _ -> do
        (_, steps) <- planInstall env (defaultInstallOptions VSCodium)
        [msg | StepWarn msg <- steps]
          `shouldSatisfy` any (Text.isInfixOf "open-vsx.org")

    it "does not touch the editor CLI with --no-extension" $
      withFakeEnv [("code", "/bin/code")] Map.empty $ \env calls -> do
        _ <- planInstall env (defaultInstallOptions VSCode){installExtension = False}
        readIORef calls `shouldReturn` []

    it "writes project files for VS Code and user files with --global" $
      withFakeEnv [] Map.empty $ \env _ -> do
        (_, project) <-
          planInstall env (defaultInstallOptions VSCode){installExtension = False}
        filePaths project
          `shouldBe` [ ideCwd env </> ".vscode/settings.json",
                       ideCwd env </> ".vscode/extensions.json"
                     ]
        (_, global) <-
          planInstall
            env
            (defaultInstallOptions VSCode)
              { installExtension = False,
                installScope = GlobalScope
              }
        filePaths global `shouldBe` [ideHome env </> ".config/Code/User/settings.json"]
        (_, mac) <-
          planInstall
            env{ideOs = "darwin"}
            (defaultInstallOptions Cursor)
              { installExtension = False,
                installScope = GlobalScope
              }
        filePaths mac
          `shouldBe` [ideHome env </> "Library/Application Support/Cursor/User/settings.json"]

    it "defaults Neovim to the user config and the others to the project" $
      withFakeEnv [] Map.empty $ \env _ -> do
        (_, nvim) <- planInstall env (defaultInstallOptions Neovim)
        filePaths nvim `shouldBe` [ideConfigHome env </> "nvim/after/plugin/tynix.lua"]
        (_, zed) <- planInstall env (defaultInstallOptions Zed)
        filePaths zed `shouldBe` [ideCwd env </> ".zed/settings.json"]
        (_, helix) <-
          planInstall env (defaultInstallOptions Helix){installScope = GlobalScope}
        filePaths helix `shouldBe` [ideConfigHome env </> "helix/languages.toml"]

    it "pins the resolved tynix-lsp path and warns when it cannot be found" $
      withFakeEnv [("tynix-lsp", "/opt/bin/tynix-lsp")] Map.empty $ \env _ -> do
        resolveLspPath env Nothing `shouldReturn` Just "/opt/bin/tynix-lsp"
        resolveLspPath env (Just "") `shouldReturn` Nothing
        resolveLspPath env (Just "bin/lsp")
          `shouldReturn` Just (ideCwd env </> "bin/lsp")
        (_, steps) <-
          planInstall
            env{ideFindExecutable = const (pure Nothing)}
            (defaultInstallOptions Zed)
        [msg | StepWarn msg <- steps]
          `shouldSatisfy` any (Text.isInfixOf "tynix-lsp was not found")

  describe "runInstall" $ do
    it "writes VS Code settings, then reports everything unchanged on a second run"
      $ withFakeEnv
        [("code", "/bin/code"), ("tynix-lsp", "/opt/bin/tynix-lsp")]
        Map.empty
      $ \env calls -> do
        let settings = ideCwd env </> ".vscode/settings.json"
        createDirectoryIfMissing True (takeDirectory settings)
        TextIO.writeFile settings "{\n  \"editor.tabSize\": 2\n}\n"
        report <-
          runInstall env (defaultInstallOptions VSCode) >>= either (fail . show) pure
        report `shouldSatisfy` Text.isInfixOf "✓ install extension ubugeeei.tynix"
        written <- TextIO.readFile settings
        parsedValue <$> parseJsonc written
          `shouldBe` Right
            ( JObject
                [ ("editor.tabSize", JNumber "2"),
                  ("tynix.server.path", JString "/opt/bin/tynix-lsp"),
                  ( "files.associations",
                    JObject [("*.tynix", JString "tynix"), ("*.d.tynix", JString "tynix")]
                  )
                ]
            )
        TextIO.readFile (ideCwd env </> ".vscode/extensions.json")
          `shouldReturn` "{\n  \"recommendations\": [\n    \"ubugeeei.tynix\"\n  ]\n}\n"
        readIORef calls
          `shouldReturn` [ ("/bin/code", ["--list-extensions"]),
                           ("/bin/code", ["--install-extension", "ubugeeei.tynix"])
                         ]
        again <-
          runInstall env (defaultInstallOptions VSCode){installExtension = False}
            >>= either (fail . show) pure
        Text.count "already up to date" again `shouldBe` 2
        TextIO.readFile settings `shouldReturn` written

    it "changes nothing on a dry run" $
      withFakeEnv [("code", "/bin/code")] Map.empty $ \env calls -> do
        report <-
          runInstall env (defaultInstallOptions VSCode){installDryRun = True}
            >>= either (fail . show) pure
        report
          `shouldSatisfy` Text.isInfixOf "would run: /bin/code --install-extension ubugeeei.tynix"
        report `shouldSatisfy` Text.isInfixOf "would create "
        doesFileExist (ideCwd env </> ".vscode/settings.json") `shouldReturn` False
        readIORef calls `shouldReturn` [("/bin/code", ["--list-extensions"])]

    it "fails when the extension install fails"
      $ withFakeEnv
        [("code", "/bin/code")]
        (Map.fromList [("/bin/code", (ExitFailure 2, ""))])
      $ \env _ -> do
        result <- runInstall env (defaultInstallOptions VSCode)
        fromLeft "" result
          `shouldSatisfy` (\err -> "exited with 2" `Text.isInfixOf` Text.pack err)

  describe "doctor" $ do
    it "passes when tynix-lsp starts and matches the CLI version"
      $ withFakeEnv
        [("tynix", "/bin/tynix"), ("tynix-lsp", "/bin/tynix-lsp"), ("nix", "/bin/nix")]
        ( Map.fromList
            [ ("/bin/tynix", (ExitSuccess, "tynix 1.2.3\n")),
              ("/bin/tynix-lsp", (ExitSuccess, "tynix-lsp 1.2.3\n")),
              ("/bin/nix", (ExitSuccess, "nix (Nix) 2.30\n"))
            ]
        )
      $ \env _ -> do
        checks <- doctorChecks env "1.2.3"
        doctorSucceeded checks `shouldBe` True
        [checkId c | c <- checks, checkStatus c == CheckOk]
          `shouldSatisfy` (\ids -> all (`elem` ids) ["tynix-lsp.starts", "tynix-lsp.version", "nix"])

    it "fails when tynix-lsp is missing or out of sync" $ do
      withFakeEnv [] Map.empty $ \env _ -> do
        checks <- doctorChecks env "1.2.3"
        doctorSucceeded checks `shouldBe` False
        renderDoctor False checks
          `shouldSatisfy` Text.isInfixOf "✗ tynix-lsp is not on PATH"
      withFakeEnv
        [("tynix-lsp", "/bin/tynix-lsp")]
        (Map.fromList [("/bin/tynix-lsp", (ExitSuccess, "tynix-lsp 0.9\n"))])
        $ \env _ -> do
          checks <- doctorChecks env "1.2.3"
          [checkStatus c | c <- checks, checkId c == "tynix-lsp.version"]
            `shouldBe` [CheckFail]

    it "reports an editor whose extension is missing and renders json"
      $ withFakeEnv
        [("code", "/bin/code"), ("tynix-lsp", "/bin/tynix-lsp")]
        (Map.fromList [("/bin/tynix-lsp", (ExitSuccess, "tynix-lsp 1.2.3"))])
      $ \env _ -> do
        checks <- doctorChecks env "1.2.3"
        [checkStatus c | c <- checks, checkId c == "editor.vscode"]
          `shouldBe` [CheckWarn]
        let json = renderDoctorJson 1 checks
        json `shouldSatisfy` Text.isInfixOf "\"action\":\"doctor\""
        json `shouldSatisfy` Text.isInfixOf "\"success\":true"

    it "parses version lines" $ do
      parseVersionLine "tynix-lsp 0.5.0.0\n" `shouldBe` Just "0.5.0.0"
      parseVersionLine "garbage" `shouldBe` Nothing

-- Helpers -------------------------------------------------------------------

type Calls = IORef [(FilePath, [String])]

-- | A fake environment rooted in a fresh temp directory. @executables@ maps
-- names to fake paths; @outputs@ maps a path to the exit code and stdout of
-- every invocation (default: success with no output).
withFakeEnv ::
  [(String, FilePath)] ->
  Map.Map FilePath (ExitCode, String) ->
  (IdeEnv -> Calls -> IO a) ->
  IO a
withFakeEnv executables outputs action =
  withTempDir $ \root -> do
    calls <- newIORef []
    let home = root </> "home"
        cwd = root </> "project"
    createDirectoryIfMissing True home
    createDirectoryIfMissing True cwd
    let env =
          IdeEnv
            { ideFindExecutable = \name -> pure (lookup name executables),
              ideRunProcess = \cmd args -> do
                modifyIORef' calls (<> [(cmd, args)])
                let (code, out) = Map.findWithDefault (ExitSuccess, "") cmd outputs
                pure (code, out, ""),
              ideHome = home,
              ideConfigHome = home </> ".config",
              ideAppData = Nothing,
              ideOs = "linux",
              ideCwd = cwd
            }
    action env calls

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = bracket create removePathForcibly
  where
    create = do
      tmp <- getTemporaryDirectory
      (path, handle) <- openTempFile tmp "tynix-ide-spec"
      hClose handle
      removeFile path
      createDirectory path
      pure path

filePaths :: [Step] -> [FilePath]
filePaths steps = [changePath change | StepFile change <- steps]
  where
    changePath = \case
      FileCreate path _ -> path
      FileUpdate path _ _ _ -> path
      FileUnchanged path -> path
      FileManual path _ _ -> path

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

isManual :: FileChange -> Bool
isManual = \case
  FileManual{} -> True
  _ -> False

isRun :: Step -> Bool
isRun = \case
  StepRun{} -> True
  _ -> False

isServerPath :: JsonEdit -> Bool
isServerPath = \case
  SetKey ["tynix.server.path"] _ -> True
  _ -> False
