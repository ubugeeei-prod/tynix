{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cli
  ( Command (..),
    OutputFormat (..),
    commandOutputFormat,
    commandOutputPath,
    commandParser,
    executeCommand,
    lspCommandArgs,
    renderAnalysis,
    renderVersion,
    writeOutput,
  )
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.List (intercalate, isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Driver (Analysis (..))
import Ide (Editor (..), InstallOptions (..), Scope (..), defaultInstallOptions)
import IdeSpec (ideSpec)
import Options.Applicative
  ( ParserPrefs,
    ParserResult (..),
    defaultPrefs,
    execParserPure,
    getParseResult,
    info,
    renderFailure,
  )
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    createDirectoryLink,
    doesFileExist,
    getTemporaryDirectory,
    listDirectory,
    removeFile,
    removePathForcibly,
  )
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)
import Test.Hspec
import Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  ideSpec
  describe "commandParser" $ do
    it "parses compile with output" $
      parse ["compile", "main.tynix", "-o", "dist/main.nix"]
        `shouldBe` Just (Compile "main.tynix" (Just "dist/main.nix") False)

    it "parses check, emit, init, scaffold, and project commands" $ do
      parse ["check", "main.tynix"] `shouldBe` Just (Check "main.tynix" TextFormat)
      parse ["check", "main.tynix", "--format", "json"]
        `shouldBe` Just (Check "main.tynix" JsonFormat)
      parse ["emit", "main.tynix"] `shouldBe` Just (Emit "main.tynix" Nothing)
      parse ["init"] `shouldBe` Just (Init Nothing [])
      parse ["scaffold", "demo"] `shouldBe` Just (Scaffold (Just "demo"))
      parse ["check-project"] `shouldBe` Just (CheckProject Nothing TextFormat)
      parse ["build", "demo", "--format", "json"]
        `shouldBe` Just (BuildProject (Just "demo") JsonFormat)
      parse ["emit-project", "demo"]
        `shouldBe` Just (EmitProject (Just "demo") TextFormat)
      parse ["version"] `shouldBe` Just (Version TextFormat)
      parse ["version", "--format", "json"] `shouldBe` Just (Version JsonFormat)
      parse ["lsp"] `shouldBe` Just (Lsp Nothing)
      parse ["lsp", "--log-file", "tynix-lsp.log"]
        `shouldBe` Just (Lsp (Just "tynix-lsp.log"))

    it "parses ide, doctor, and init --editor" $ do
      parse ["ide", "install", "vscode"]
        `shouldBe` Just (IdeInstall (defaultInstallOptions VSCode))
      parse
        [ "ide",
          "install",
          "nvim",
          "--global",
          "--dry-run",
          "--no-extension",
          "--force",
          "--lsp-path",
          "/bin/tynix-lsp"
        ]
        `shouldBe` Just
          ( IdeInstall
              (defaultInstallOptions Neovim)
                { installScope = GlobalScope,
                  installDryRun = True,
                  installExtension = False,
                  installForce = True,
                  installLspPath = Just "/bin/tynix-lsp"
                }
          )
      parse ["ide", "install", "zed", "--project", "demo"]
        `shouldBe` Just
          (IdeInstall (defaultInstallOptions Zed){installScope = ProjectScope "demo"})
      parse ["ide", "install", "helix", "--project", "demo", "--global"]
        `shouldBe` Nothing
      parse ["ide", "install", "emacs"] `shouldBe` Nothing
      parse ["ide", "list"] `shouldBe` Just IdeList
      parse ["doctor"] `shouldBe` Just (Doctor TextFormat)
      parse ["doctor", "--format", "json"] `shouldBe` Just (Doctor JsonFormat)
      parse ["init", "demo", "--editor", "vscode", "--editor", "helix"]
        `shouldBe` Just (Init (Just "demo") [VSCode, Helix])

    it "reports an error for missing subcommands" $ do
      case parserResult [] of
        Failure failure ->
          let (message, _) = renderFailure failure ""
           in message `shouldSatisfy` ("Missing: COMMAND" `isInfixOf`)
        other -> expectationFailure ("expected parse failure, got " <> show other)

  describe "renderAnalysis" $
    it "renders root schemes before named bindings" $ do
      let analysis =
            Analysis
              { analysisProgram = error "unused in cli tests",
                analysisLocatedProgram = error "unused in cli tests",
                analysisRoot = Just (Scheme [] tInt),
                analysisBindings =
                  Map.fromList
                    [ ("box", Scheme [] tString),
                      ("id", Scheme ["a"] (TFun Many (TVar "a") (TVar "a")))
                    ],
                analysisAliases = mempty,
                analysisAmbient = mempty
              }
      renderAnalysis analysis
        `shouldBe` Text.unlines ["root: Int", "box :: String", "id :: forall a. a -> a"]

  describe "commandOutputPath" $
    it "tracks explicit destinations only for write commands" $ do
      commandOutputPath (Compile "main.tynix" (Just "dist/main.nix") False)
        `shouldBe` Just "dist/main.nix"
      commandOutputPath (Emit "main.tynix" (Just "types/main.d.tynix"))
        `shouldBe` Just "types/main.d.tynix"
      commandOutputPath (Check "main.tynix" TextFormat) `shouldBe` Nothing
      commandOutputPath (Init Nothing []) `shouldBe` Nothing
      commandOutputPath (Scaffold Nothing) `shouldBe` Nothing
      commandOutputPath (Version TextFormat) `shouldBe` Nothing
      commandOutputPath (Lsp Nothing) `shouldBe` Nothing

  describe "commandOutputFormat" $
    it "tracks commands that can emit machine-readable reports" $ do
      commandOutputFormat (Check "main.tynix" JsonFormat) `shouldBe` Just JsonFormat
      commandOutputFormat (CheckProject Nothing TextFormat) `shouldBe` Just TextFormat
      commandOutputFormat (BuildProject Nothing JsonFormat) `shouldBe` Just JsonFormat
      commandOutputFormat (EmitProject Nothing JsonFormat) `shouldBe` Just JsonFormat
      commandOutputFormat (Version JsonFormat) `shouldBe` Just JsonFormat
      commandOutputFormat (Compile "main.tynix" Nothing False) `shouldBe` Nothing
      commandOutputFormat (Init Nothing []) `shouldBe` Nothing
      commandOutputFormat (Lsp Nothing) `shouldBe` Nothing

  describe "lspCommandArgs" $
    it "delegates to the standalone server over stdio" $ do
      lspCommandArgs Nothing `shouldBe` ["--stdio"]
      lspCommandArgs (Just "tynix-lsp.log")
        `shouldBe` ["--stdio", "--log-file", "tynix-lsp.log"]

  describe "renderVersion" $ do
    it "renders text and json version output" $ do
      renderVersion "1.2.3" TextFormat `shouldBe` "tynix 1.2.3"
      let json = renderVersion "1.2.3" JsonFormat
      Text.isInfixOf "\"schemaVersion\":1" json `shouldBe` True
      Text.isInfixOf "\"action\":\"version\"" json `shouldBe` True
      Text.isInfixOf "\"version\":\"1.2.3\"" json `shouldBe` True

  describe "executeCommand" $ do
    it "compiles source files through the driver end-to-end" $
      withTempTree
        [ ( "main.tynix",
            source
              [ "let",
                "  value :: Int;",
                "  value = 1;",
                "in value"
              ]
          )
        ]
        ( \root -> do
            output <-
              executeCommand (Compile (root <> "/main.tynix") Nothing False) >>= expectRight
            output `shouldBe` Text.stripEnd (source ["let", "  value = 1;", "in value"])
        )

    it "renders check output for analyzed files" $
      withTempTree
        [ ( "main.tynix",
            source
              [ "let",
                "  id :: forall a. a -> a;",
                "  id = x: x;",
                "in id"
              ]
          )
        ]
        ( \root -> do
            output <-
              executeCommand (Check (root <> "/main.tynix") TextFormat) >>= expectRight
            Text.lines output
              `shouldBe` ["root: forall t0. t0 -> t0", "id :: forall a. a -> a"]
        )

    it "renders json check output for analyzed files" $
      withTempTree
        [("main.tynix", "1")]
        ( \root -> do
            output <-
              executeCommand (Check (root <> "/main.tynix") JsonFormat) >>= expectRight
            Text.isInfixOf "\"schemaVersion\":1" output `shouldBe` True
            Text.isInfixOf "\"success\":true" output `shouldBe` True
            Text.isInfixOf "\"root\":\"1\"" output `shouldBe` True
        )

    it "emits declaration files through the driver end-to-end" $
      withTempTree [("main.tynix", "{ value = 1; }")] $
        \root -> do
          output <- executeCommand (Emit (root <> "/main.tynix") Nothing) >>= expectRight
          output
            `shouldBe` Text.stripEnd
              (source ["declare \"./main.nix\" {", "  value :: 1;", "};"])

    it "surfaces driver failures without writing partial output" $
      withTempTree [("types.d.tynix", "declare \"./lib.nix\" { default :: Int; };")] $
        \root ->
          executeCommand (Compile (root <> "/types.d.tynix") Nothing False)
            >>= (`expectLeftContaining` "declaration-only")

    it "surfaces missing source files as user-facing errors" $
      executeCommand (Check "/tmp/tynix-cli-missing/main.tynix" TextFormat)
        >>= (`expectLeftContaining` "failed to read")

    it "renders check failures as json when json output is requested" $
      executeCommand (Check "/tmp/tynix-cli-missing/main.tynix" JsonFormat)
        >>= (`expectLeftContaining` "\"success\":false")

    it "initializes a project with tynix.config.tynix and starter files" $
      withTempTree [] $ \root -> do
        output <- executeCommand (Init (Just root) []) >>= expectRight
        let configPath = root </> "tynix.config.tynix"
            configDeclPath = root </> "tynix.config.d.tynix"
            entryPath = root </> "src/main.tynix"
            builtinsPath = root </> "types/builtins.d.tynix"
        doesFileExist configPath `shouldReturn` True
        doesFileExist configDeclPath `shouldReturn` True
        doesFileExist entryPath `shouldReturn` True
        doesFileExist builtinsPath `shouldReturn` True
        -- Atomic write should leave no `*.tmp*` siblings around once init succeeds.
        rootEntries <- listDirectory root
        any (".tmp" `isInfixOf`) rootEntries `shouldBe` False
        srcEntries <- listDirectory (root </> "src")
        any (".tmp" `isInfixOf`) srcEntries `shouldBe` False
        typesEntries <- listDirectory (root </> "types")
        any (".tmp" `isInfixOf`) typesEntries `shouldBe` False
        config <- TextIO.readFile configPath
        configDecl <- TextIO.readFile configDeclPath
        entry <- TextIO.readFile entryPath
        Text.isInfixOf "sourceDir = ./src;" config `shouldBe` True
        Text.isInfixOf "declarationPacks = [];" config `shouldBe` True
        Text.isInfixOf "declare \"./tynix.config.tynix\"" configDecl `shouldBe` True
        Text.isInfixOf "declarationPacks :: List TynixProjectPath;" configDecl
          `shouldBe` True
        Text.isInfixOf "Hello from" entry `shouldBe` True
        Text.isInfixOf "tynix.config.tynix" output `shouldBe` True

    it "escapes generated string literals during project initialization" $
      withTempTree [] $ \root -> do
        let projectRoot = root </> "quote\"and\\slash"
        _ <- executeCommand (Init (Just projectRoot) []) >>= expectRight
        config <- TextIO.readFile (projectRoot </> "tynix.config.tynix")
        entry <- TextIO.readFile (projectRoot </> "src/main.tynix")
        Text.isInfixOf "name = \"quote\\\"and\\\\slash\";" config `shouldBe` True
        Text.isInfixOf "greeting = \"Hello from quote\\\"and\\\\slash\";" entry
          `shouldBe` True
        _ <- executeCommand (Scaffold (Just projectRoot)) >>= expectRight
        _ <-
          executeCommand (Check (projectRoot </> "src/main.tynix") TextFormat)
            >>= expectRight
        pure ()

    it
      "scaffolds from tynix.config.tynix path overrides without overwriting existing files"
      $ withTempTree
        [ ( "tynix.config.tynix",
            source
              [ "{",
                "  name = \"demo\";",
                "  sourceDir = ./app;",
                "  entry = ./app/custom.tynix;",
                "  declarationDir = \"./decls\";",
                "  builtins = true;",
                "}"
              ]
          ),
          ("app/custom.tynix", "existing")
        ]
      $ \root -> do
        output <- executeCommand (Scaffold (Just root)) >>= expectRight
        doesFileExist (root </> "tynix.config.d.tynix") `shouldReturn` True
        TextIO.readFile (root </> "app/custom.tynix") `shouldReturn` "existing"
        doesFileExist (root </> "decls/builtins.d.tynix") `shouldReturn` True
        Text.isInfixOf "skipped" output `shouldBe` True
        Text.isInfixOf "builtins.d.tynix" output `shouldBe` True
        Text.isInfixOf "tynix.config.d.tynix" output `shouldBe` True

    it "respects builtins = false when scaffolding"
      $ withTempTree
        [ ( "tynix.config.tynix",
            source
              [ "{",
                "  name = \"demo\";",
                "  builtins = false;",
                "}"
              ]
          )
        ]
      $ \root -> do
        _ <- executeCommand (Scaffold (Just root)) >>= expectRight
        doesFileExist (root </> "src/main.tynix") `shouldReturn` True
        doesFileExist (root </> "types/builtins.d.tynix") `shouldReturn` False

    it "reports missing or invalid scaffold configs" $
      withTempTree
        []
        ( \root ->
            executeCommand (Scaffold (Just root))
              >>= (`expectLeftContaining` "missing tynix.config.tynix")
        )
        >> withTempTree
          [("tynix.config.tynix", "{ builtins = 1; }")]
          ( \root ->
              executeCommand (Scaffold (Just root))
                >>= (`expectLeftContaining` "expected Bool for builtins")
          )

    it "checks every discovered source file in a project"
      $ withTempTree
        [ ( "tynix.config.tynix",
            source
              [ "{",
                "  name = \"demo\";",
                "  sourceDir = ./src;",
                "  exclude = [ ./src/skip ];",
                "}"
              ]
          ),
          ("src/main.tynix", "1"),
          ("src/lib/util.tynix", "\"ok\""),
          ("src/skip/ignored.tynix", "missing")
        ]
      $ \root -> do
        output <- executeCommand (CheckProject (Just root) TextFormat) >>= expectRight
        Text.isInfixOf "src/main.tynix" output `shouldBe` True
        Text.isInfixOf "src/lib/util.tynix" output `shouldBe` True
        Text.isInfixOf "ignored.tynix" output `shouldBe` False

    it "builds a project into compiled nix and generated declarations"
      $ withTempTree
        [ ( "tynix.config.tynix",
            source
              [ "{",
                "  name = \"demo\";",
                "  sourceDir = ./src;",
                "  buildDir = ./artifacts;",
                "  generatedDeclarationDir = ./artifacts/types;",
                "}"
              ]
          ),
          ( "src/main.tynix",
            source
              [ "let",
                "  value :: Int;",
                "  value = 1;",
                "in value"
              ]
          )
        ]
      $ \root -> do
        output <- executeCommand (BuildProject (Just root) TextFormat) >>= expectRight
        doesFileExist (root </> "artifacts/main.nix") `shouldReturn` True
        doesFileExist (root </> "artifacts/types/main.d.tynix") `shouldReturn` True
        declaration <- TextIO.readFile (root </> "artifacts/types/main.d.tynix")
        Text.isInfixOf "declare \"../main.nix\"" declaration `shouldBe` True
        Text.isInfixOf "artifacts/main.nix" output `shouldBe` True

    it "does not write project build outputs when any source fails"
      $ withTempTree
        [ ( "tynix.config.tynix",
            source
              [ "{",
                "  name = \"demo\";",
                "  sourceDir = ./src;",
                "  buildDir = ./artifacts;",
                "  generatedDeclarationDir = ./artifacts/types;",
                "}"
              ]
          ),
          ("src/a.tynix", "1"),
          ("src/b.tynix", "missing")
        ]
      $ \root -> do
        result <- executeCommand (BuildProject (Just root) TextFormat)
        case result of
          Left report -> do
            Text.isInfixOf "- ok src/a.tynix" (Text.pack report) `shouldBe` True
            Text.isInfixOf "- error src/b.tynix" (Text.pack report) `shouldBe` True
          Right output -> expectationFailure ("expected build failure, got: " <> Text.unpack output)
        doesFileExist (root </> "artifacts/a.nix") `shouldReturn` False
        doesFileExist (root </> "artifacts/types/a.d.tynix") `shouldReturn` False
        doesFileExist (root </> "artifacts/b.nix") `shouldReturn` False
        doesFileExist (root </> "artifacts/types/b.d.tynix") `shouldReturn` False

    it "does not write project declaration outputs when any source fails"
      $ withTempTree
        [ ( "tynix.config.tynix",
            source
              [ "{",
                "  name = \"demo\";",
                "  sourceDir = ./src;",
                "  generatedDeclarationDir = ./generated;",
                "}"
              ]
          ),
          ("src/a.tynix", "{ value = 1; }"),
          ("src/b.tynix", "missing")
        ]
      $ \root -> do
        result <- executeCommand (EmitProject (Just root) TextFormat)
        case result of
          Left report -> do
            Text.isInfixOf "- ok src/a.tynix" (Text.pack report) `shouldBe` True
            Text.isInfixOf "- error src/b.tynix" (Text.pack report) `shouldBe` True
          Right output ->
            expectationFailure
              ("expected emit-project failure, got: " <> Text.unpack output)
        doesFileExist (root </> "generated/a.d.tynix") `shouldReturn` False
        doesFileExist (root </> "generated/b.d.tynix") `shouldReturn` False

    it "emits project declarations as json"
      $ withTempTree
        [ ( "tynix.config.tynix",
            source
              [ "{",
                "  name = \"demo\";",
                "  sourceDir = ./src;",
                "}"
              ]
          ),
          ("src/main.tynix", "{ value = 1; }")
        ]
      $ \root -> do
        output <- executeCommand (EmitProject (Just root) JsonFormat) >>= expectRight
        Text.isInfixOf "\"schemaVersion\":1" output `shouldBe` True
        Text.isInfixOf "\"action\":\"emit-project\"" output `shouldBe` True
        Text.isInfixOf "\"success\":true" output `shouldBe` True

  describe "writeOutput" $
    it "creates parent directories before writing files" $
      withTempTree [] $ \root -> do
        let path = root <> "/dist/nested/out.txt"
        writeOutput (Just path) "hello"
        doesFileExist path `shouldReturn` True

  describe "check-project against pathological filesystems" $ do
    it "terminates on a directory symlink loop without hanging"
      $ withTempTree
        [ ( "tynix.config.tynix",
            source
              [ "{",
                "  name = \"loop\";",
                "  sourceDir = ./src;",
                "}"
              ]
          ),
          ("src/main.tynix", "1")
        ]
      $ \root -> do
        -- Create src/loop -> src so a naive walker would recurse forever.
        createDirectoryLink (root </> "src") (root </> "src/loop")
        result <-
          timeout
            (5 * 1000 * 1000)
            (executeCommand (CheckProject (Just root) TextFormat))
        case result of
          Just (Right report) ->
            Text.isInfixOf "ok src/main.tynix" report `shouldBe` True
          Just (Left err) ->
            expectationFailure ("expected ok, got error: " <> err)
          Nothing -> expectationFailure "check-project hung on symlink loop"

    it "stops walking when the directory depth exceeds the budget"
      $ withTempTree
        [ ( "tynix.config.tynix",
            source
              [ "{",
                "  name = \"deep\";",
                "  sourceDir = ./src;",
                "}"
              ]
          ),
          ("src/main.tynix", "1")
        ]
      $ \root -> do
        -- Create a deep nested layout below the configured source dir.
        let deep = root </> "src" </> deepPath 80
        createDirectoryIfMissing True deep
        TextIO.writeFile (deep </> "leaf.tynix") "1"
        result <- executeCommand (CheckProject (Just root) TextFormat)
        case result of
          Right report ->
            Text.isInfixOf "ok src/main.tynix" report `shouldBe` True
          Left err ->
            expectationFailure ("expected ok, got error: " <> err)

  describe "project discovery" $ do
    it "honours include and exclude filters" $
      withTempTree
        [ ( "tynix.config.tynix",
            projectConfig
              ["include = [ ./src/keep ];", "exclude = [ ./src/keep/skipped.tynix ];"]
          ),
          ("flake.nix", "{}"),
          ("src/keep/kept.tynix", "1"),
          ("src/keep/skipped.tynix", "1"),
          ("src/dropped/other.tynix", "1")
        ]
        ( \root -> do
            report <- executeCommand (CheckProject (Just root) JsonFormat) >>= expectRight
            report `shouldSatisfy` Text.isInfixOf "kept.tynix"
            report `shouldSatisfy` (not . Text.isInfixOf "skipped.tynix")
            report `shouldSatisfy` (not . Text.isInfixOf "other.tynix")
        )

    it "never treats a declaration file as a project source" $
      withTempTree
        [ ("tynix.config.tynix", projectConfig []),
          ("flake.nix", "{}"),
          ("src/main.tynix", "1"),
          ("src/types.d.tynix", "declare \"./lib.nix\" { default :: Int; };")
        ]
        ( \root -> do
            report <- executeCommand (CheckProject (Just root) JsonFormat) >>= expectRight
            report `shouldSatisfy` Text.isInfixOf "main.tynix"
            report `shouldSatisfy` (not . Text.isInfixOf "types.d.tynix")
        )

    it "reports a project with no discovered sources as an error" $
      withTempTree
        [ ("tynix.config.tynix", projectConfig []),
          ("flake.nix", "{}")
        ]
        ( \root -> do
            result <- executeCommand (CheckProject (Just root) TextFormat)
            expectLeftContaining result "no project source files discovered"
        )

    it "reports a missing config rather than guessing a layout" $
      withTempTree
        [("flake.nix", "{}")]
        ( \root -> do
            result <- executeCommand (CheckProject (Just root) TextFormat)
            expectLeftContaining result "missing tynix.config.tynix"
        )

  describe "config decoding" $ do
    it "rejects a duplicate field" $
      expectConfigError
        (source ["{", "  name = \"a\";", "  name = \"b\";", "}"])
        "duplicate config field: name"

    it "rejects a root that is not an attribute set" $
      expectConfigError "1" "must evaluate to an attrset"

    it "rejects inherit in the root attribute set" $
      expectConfigError
        (source ["{", "  inherit name;", "}"])
        "does not support inherit"

    it "rejects a field whose type does not match" $ do
      expectConfigError (source ["{ name = 1; }"]) "expected string field"
      expectConfigError (source ["{ builtins = 1; }"]) "expected Bool for builtins"
      expectConfigError
        (source ["{ sourceDir = 1; }"])
        "expected path-like field for sourceDir"
      expectConfigError
        (source ["{ entries = 1; }"])
        "expected list of path-like values for entries"
      expectConfigError
        (source ["{ entries = [ 1 ]; }"])
        "expected path-like item in entries"

  describe "json reports" $ do
    it "stamps every machine-readable payload with the schema version" $
      withTempTree
        [ ("tynix.config.tynix", projectConfig []),
          ("flake.nix", "{}"),
          ("src/main.tynix", "1")
        ]
        ( \root -> do
            forM_
              [ CheckProject (Just root) JsonFormat,
                BuildProject (Just root) JsonFormat,
                EmitProject (Just root) JsonFormat
              ]
              $ \command -> do
                report <- executeCommand command >>= expectRight
                report `shouldSatisfy` Text.isInfixOf "\"schemaVersion\":1"
            renderVersion "1.2.3" JsonFormat
              `shouldSatisfy` Text.isInfixOf "\"schemaVersion\":1"
        )

    it "names the action that produced each report" $
      withTempTree
        [ ("tynix.config.tynix", projectConfig []),
          ("flake.nix", "{}"),
          ("src/main.tynix", "1")
        ]
        ( \root -> do
            check <- executeCommand (CheckProject (Just root) JsonFormat) >>= expectRight
            build <- executeCommand (BuildProject (Just root) JsonFormat) >>= expectRight
            emit <- executeCommand (EmitProject (Just root) JsonFormat) >>= expectRight
            check `shouldSatisfy` Text.isInfixOf "\"action\":\"check-project\""
            build `shouldSatisfy` Text.isInfixOf "\"action\":\"build\""
            emit `shouldSatisfy` Text.isInfixOf "\"action\":\"emit-project\""
        )

    it "summarises how many files passed and failed" $
      withTempTree
        [ ("tynix.config.tynix", projectConfig []),
          ("flake.nix", "{}"),
          ("src/ok.tynix", "1"),
          ("src/bad.tynix", "missing")
        ]
        ( \root -> do
            result <- executeCommand (CheckProject (Just root) JsonFormat)
            case result of
              Right report -> expectationFailure ("expected a failing report, got " <> Text.unpack report)
              Left report -> do
                report `shouldSatisfy` isInfixOf "\"total\":2"
                report `shouldSatisfy` isInfixOf "\"ok\":1"
                report `shouldSatisfy` isInfixOf "\"failed\":1"
        )

  describe "writeOutput" $ do
    it "creates missing parent directories" $
      withTempTree
        []
        ( \root -> do
            let target = root </> "a/b/c/out.nix"
            writeOutput (Just target) "value"
            -- File output is written verbatim; only stdout gets a trailing
            -- newline, because the content is the artifact.
            readFileText target `shouldReturn` "value"
        )

    it "leaves no temporary file behind" $
      withTempTree
        []
        ( \root -> do
            let target = root </> "out.nix"
            writeOutput (Just target) "value"
            entries <- listDirectory root
            entries `shouldBe` ["out.nix"]
        )

    it "replaces existing content rather than appending to it" $
      withTempTree
        [("out.nix", "old\n")]
        ( \root -> do
            let target = root </> "out.nix"
            writeOutput (Just target) "new"
            readFileText target `shouldReturn` "new"
        )
  where
    parserInfo = info commandParser mempty
    parserPrefs :: ParserPrefs
    parserPrefs = defaultPrefs
    parse = getParseResult . execParserPure parserPrefs parserInfo
    parserResult = execParserPure parserPrefs parserInfo

-- | A `tynix.config.tynix` with the standard layout plus any extra fields.
projectConfig :: [Text] -> Text
projectConfig extra =
  source $
    [ "{",
      "  name = \"spec\";",
      "  sourceDir = ./src;",
      "  entry = ./src/main.tynix;",
      "  builtins = false;"
    ]
      <> map ("  " <>) extra
      <> ["}"]

-- | Load a project whose config is expected to be rejected, and check the
-- reason surfaced to the user.
expectConfigError :: Text -> String -> Expectation
expectConfigError config needle =
  withTempTree
    [("tynix.config.tynix", config), ("flake.nix", "{}")]
    ( \root -> do
        result <- executeCommand (CheckProject (Just root) TextFormat)
        expectLeftContaining result needle
    )

readFileText :: FilePath -> IO Text
readFileText = TextIO.readFile

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

deepPath :: Int -> FilePath
deepPath n = intercalate "/" (replicate n "deep")

withTempTree :: [(FilePath, Text)] -> (FilePath -> IO a) -> IO a
withTempTree files action =
  bracket createRoot removePathForcibly (\root -> writeTree root >> action root)
  where
    createRoot = do
      tmp <- getTemporaryDirectory
      (path, handle) <- openTempFile tmp "tynix-cli-spec"
      hClose handle
      removeFile path
      createDirectory path
      pure path
    writeTree root =
      forM_ files $ \(relative, content) -> do
        let path = root </> relative
        createDirectoryIfMissing True (takeDirectory path)
        TextIO.writeFile path content
