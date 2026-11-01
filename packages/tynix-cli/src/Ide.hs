{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | One-shot editor setup for tynix (`tynix ide install` / `tynix ide list`).
--
-- Everything that touches the outside world goes through 'IdeEnv' so the
-- planning logic can be exercised in tests without real editors on PATH.
-- Installation is split into two phases: 'planInstall' reads the current state
-- and computes a list of 'Step's, then 'applySteps' performs them. A dry run
-- simply renders the plan.
module Ide
  ( Editor (..),
    FileChange (..),
    IdeEnv (..),
    InstallOptions (..),
    Scope (..),
    Step (..),
    allEditors,
    applySteps,
    defaultIdeEnv,
    defaultInstallOptions,
    detectEditor,
    editorIntegrationStatus,
    editorCliCandidates,
    editorFromName,
    editorName,
    extensionId,
    helixSnippet,
    lineDiff,
    listEditors,
    neovimSnippet,
    planHelixFile,
    planInstall,
    planJsonFile,
    planManagedFile,
    renderPlan,
    renderReport,
    resolveLspPath,
    runInstall,
    vscodeSettingsEdits,
    zedSettingsEdits,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (filterM, forM, (>=>))
import Data.Array (Array, listArray, (!))
import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Jsonc
import Project (writeFileAtomic)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    findExecutable,
    getCurrentDirectory,
    getHomeDirectory,
    makeAbsolute,
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.Info qualified as SystemInfo
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

-- | Editors `tynix ide` knows how to configure.
data Editor
  = VSCode
  | Cursor
  | VSCodium
  | Zed
  | Neovim
  | Helix
  deriving (Eq, Show, Enum, Bounded)

allEditors :: [Editor]
allEditors = [minBound .. maxBound]

editorName :: Editor -> Text
editorName = \case
  VSCode -> "vscode"
  Cursor -> "cursor"
  VSCodium -> "vscodium"
  Zed -> "zed"
  Neovim -> "neovim"
  Helix -> "helix"

editorFromName :: String -> Either String Editor
editorFromName raw =
  case map toLower raw of
    "vscode" -> Right VSCode
    "code" -> Right VSCode
    "cursor" -> Right Cursor
    "vscodium" -> Right VSCodium
    "codium" -> Right VSCodium
    "zed" -> Right Zed
    "neovim" -> Right Neovim
    "nvim" -> Right Neovim
    "helix" -> Right Helix
    "hx" -> Right Helix
    _ ->
      Left
        ( "unknown editor '"
            <> raw
            <> "'; expected one of: "
            <> Text.unpack (Text.intercalate ", " (map editorName allEditors))
        )

-- | Executables that identify an editor, in lookup order.
editorCliCandidates :: Editor -> [String]
editorCliCandidates = \case
  VSCode -> ["code"]
  Cursor -> ["cursor"]
  VSCodium -> ["codium"]
  Zed -> ["zed", "zeditor"]
  Neovim -> ["nvim"]
  Helix -> ["hx", "helix"]

-- | Marketplace (VS Code) and Open VSX (Cursor, VSCodium) share one id.
extensionId :: Text
extensionId = "ubugeeei.tynix"

isVSCodeFamily :: Editor -> Bool
isVSCodeFamily editor = editor `elem` [VSCode, Cursor, VSCodium]

-- | Where configuration should be written.
data Scope
  = -- | The editor's natural default: the current project for editors with
    -- project settings, the user config for Neovim.
    DefaultScope
  | ProjectScope FilePath
  | GlobalScope
  deriving (Eq, Show)

data InstallOptions = InstallOptions
  { installEditor :: Editor,
    installScope :: Scope,
    installDryRun :: Bool,
    installExtension :: Bool,
    -- | Overwrite files that cannot be merged safely (after taking a backup).
    installForce :: Bool,
    -- | Explicit language-server path; @Just ""@ disables pinning a path.
    installLspPath :: Maybe FilePath
  }
  deriving (Eq, Show)

defaultInstallOptions :: Editor -> InstallOptions
defaultInstallOptions editor =
  InstallOptions
    { installEditor = editor,
      installScope = DefaultScope,
      installDryRun = False,
      installExtension = True,
      installForce = False,
      installLspPath = Nothing
    }

-- | Side effects needed to plan an install, injectable for tests.
data IdeEnv = IdeEnv
  { ideFindExecutable :: String -> IO (Maybe FilePath),
    ideRunProcess :: FilePath -> [String] -> IO (ExitCode, String, String),
    ideHome :: FilePath,
    -- | @$XDG_CONFIG_HOME@, falling back to @~/.config@.
    ideConfigHome :: FilePath,
    -- | @%APPDATA%@ on Windows.
    ideAppData :: Maybe FilePath,
    ideOs :: String,
    ideCwd :: FilePath
  }

defaultIdeEnv :: IO IdeEnv
defaultIdeEnv = do
  home <- getHomeDirectory
  xdg <- lookupEnv "XDG_CONFIG_HOME"
  appData <- lookupEnv "APPDATA"
  cwd <- getCurrentDirectory
  pure
    IdeEnv
      { ideFindExecutable = findExecutable >=> traverse makeAbsolute,
        ideRunProcess = runProcessWithTimeout,
        ideHome = home,
        ideConfigHome = fromMaybe (home </> ".config") (nonEmpty xdg),
        ideAppData = nonEmpty appData,
        ideOs = SystemInfo.os,
        ideCwd = cwd
      }
  where
    nonEmpty = \case
      Just value | not (null value) -> Just value
      _ -> Nothing

-- | Run a process with closed stdin and a 60 second ceiling so a wedged editor
-- CLI cannot hang the installer.
runProcessWithTimeout :: FilePath -> [String] -> IO (ExitCode, String, String)
runProcessWithTimeout cmd args = do
  result <-
    try @IOException (timeout 60000000 (readProcessWithExitCode cmd args ""))
  pure $ case result of
    Left err -> (ExitFailure 127, "", show err)
    Right Nothing -> (ExitFailure 124, "", cmd <> " timed out")
    Right (Just output) -> output

-- Plans ---------------------------------------------------------------------

-- | The outcome planned for one configuration file.
data FileChange
  = FileCreate FilePath Text
  | -- | Path, previous content, new content, optional backup path.
    FileUpdate FilePath Text Text (Maybe FilePath)
  | FileUnchanged FilePath
  | -- | The file could not be edited safely; show the user what to add.
    FileManual FilePath Text Text
  deriving (Eq, Show)

data Step
  = StepRun FilePath [String] Text
  | StepFile FileChange
  | StepOk Text
  | StepWarn Text
  | StepNote Text
  deriving (Eq, Show)

-- | Resolve where tynix-lsp lives: an explicit override, PATH, then the common
-- Nix profile locations the editor extensions also probe.
resolveLspPath :: IdeEnv -> Maybe FilePath -> IO (Maybe FilePath)
resolveLspPath env override =
  case override of
    Just "" -> pure Nothing
    Just path -> Just <$> makeAbsoluteFrom (ideCwd env) path
    Nothing -> do
      onPath <- ideFindExecutable env "tynix-lsp"
      case onPath of
        Just path -> pure (Just path)
        Nothing -> find' candidates
  where
    home = ideHome env
    candidates =
      [ home </> ".nix-profile/bin/tynix-lsp",
        home </> ".local/state/nix/profiles/profile/bin/tynix-lsp",
        "/run/current-system/sw/bin/tynix-lsp",
        "/nix/var/nix/profiles/default/bin/tynix-lsp"
      ]
    find' = \case
      [] -> pure Nothing
      candidate : rest -> do
        exists <- doesFileExist candidate
        if exists then pure (Just candidate) else find' rest

makeAbsoluteFrom :: FilePath -> FilePath -> IO FilePath
makeAbsoluteFrom base path =
  pure (if "/" `isPrefixOf` path then path else base </> path)

data ResolvedScope = AtProject FilePath | AtGlobal
  deriving (Eq, Show)

resolveScope :: IdeEnv -> Editor -> Scope -> IO ResolvedScope
resolveScope env editor = \case
  GlobalScope -> pure AtGlobal
  ProjectScope dir -> AtProject <$> makeAbsoluteFrom (ideCwd env) dir
  DefaultScope
    | editor == Neovim -> pure AtGlobal
    | otherwise -> pure (AtProject (ideCwd env))

-- | Compute every step of an install without changing anything on disk.
planInstall :: IdeEnv -> InstallOptions -> IO (Text, [Step])
planInstall env opts = do
  let editor = installEditor opts
  scope <- resolveScope env editor (installScope opts)
  lsp <- resolveLspPath env (installLspPath opts)
  let header =
        "tynix ide install "
          <> editorName editor
          <> (if installDryRun opts then " (dry run)" else "")
          <> " -> "
          <> case scope of
            AtProject dir -> "project " <> Text.pack dir
            AtGlobal -> "user settings"
      lspSteps = case (lsp, installLspPath opts) of
        (Nothing, Just "") -> []
        (Nothing, _) ->
          [ StepWarn
              "tynix-lsp was not found on PATH or in a Nix profile; the editor \
              \will look it up at startup. Install it with `nix profile install \
              \github:ubugeeei-prod/tynix#tynix-lsp`."
          ]
        (Just path, _)
          | "/nix/store/" `isPrefixOf` path ->
              [ StepWarn
                  ( "tynix-lsp resolves to "
                      <> Text.pack path
                      <> ", a Nix store path that disappears after garbage collection; \
                         \prefer a profile install or pass --lsp-path."
                  )
              ]
          | otherwise -> []
  extensionSteps <-
    if installExtension opts && isVSCodeFamily editor
      then planExtension env editor
      else pure []
  fileSteps <- planEditorFiles env opts scope lsp
  pure
    (header, lspSteps <> extensionSteps <> fileSteps <> editorNotes editor scope)

planExtension :: IdeEnv -> Editor -> IO [Step]
planExtension env editor = do
  cli <- firstExecutable env (editorCliCandidates editor)
  case cli of
    Nothing ->
      pure
        [ StepWarn $
            "`"
              <> Text.pack (fromMaybe "code" (listToMaybe (editorCliCandidates editor)))
              <> "` is not on PATH, so the extension was not installed. "
              <> manualExtensionHint editor
        ]
    Just path -> do
      installed <- extensionInstalledWith env path
      pure $
        if installed
          then [StepOk ("extension " <> extensionId <> " is already installed")]
          else
            [ StepRun
                path
                ["--install-extension", Text.unpack extensionId]
                ("install extension " <> extensionId)
            ]

manualExtensionHint :: Editor -> Text
manualExtensionHint = \case
  VSCode ->
    "Install it from https://marketplace.visualstudio.com/items?itemName=ubugeeei.tynix, or run \"Shell Command: Install 'code' command in PATH\" from the command palette and re-run."
  Cursor ->
    "Install it from the Extensions view (search \"ubugeeei.tynix\", \
    \served by https://open-vsx.org/extension/ubugeeei/tynix), or add \
    \the `cursor` shell command and re-run."
  VSCodium ->
    "Install it from https://open-vsx.org/extension/ubugeeei/tynix, or \
    \put `codium` on PATH and re-run."
  _ -> ""

firstExecutable :: IdeEnv -> [String] -> IO (Maybe FilePath)
firstExecutable env = \case
  [] -> pure Nothing
  name : rest ->
    ideFindExecutable env name >>= maybe (firstExecutable env rest) (pure . Just)

extensionInstalledWith :: IdeEnv -> FilePath -> IO Bool
extensionInstalledWith env cli = do
  (code, out, _) <- ideRunProcess env cli ["--list-extensions"]
  pure $
    code == ExitSuccess
      && any
        ((== Text.toLower extensionId) . Text.toLower . Text.strip)
        (Text.lines (Text.pack out))

planEditorFiles ::
  IdeEnv -> InstallOptions -> ResolvedScope -> Maybe FilePath -> IO [Step]
planEditorFiles env opts scope lsp =
  case installEditor opts of
    editor
      | isVSCodeFamily editor -> do
          let settingsPath = case scope of
                AtProject dir -> dir </> ".vscode" </> "settings.json"
                AtGlobal -> vscodeUserDir env editor </> "settings.json"
          settings <- jsonStep settingsPath (vscodeSettingsEdits lsp)
          extensions <- case scope of
            AtProject dir ->
              pure
                <$> jsonStep
                  (dir </> ".vscode" </> "extensions.json")
                  [AppendUnique ["recommendations"] (JString extensionId)]
            AtGlobal -> pure []
          pure (settings : extensions)
    Zed -> do
      let path = case scope of
            AtProject dir -> dir </> ".zed" </> "settings.json"
            AtGlobal -> zedConfigDir env </> "settings.json"
      pure <$> jsonStep path (zedSettingsEdits lsp)
    Neovim -> do
      let path = case scope of
            AtProject dir -> dir </> ".nvim.lua"
            AtGlobal -> neovimConfigDir env </> "after" </> "plugin" </> "tynix.lua"
      existing <- readIfExists path
      pure
        [ StepFile (planManagedFile (installForce opts) path existing (neovimSnippet lsp))
        ]
    Helix -> do
      let path = case scope of
            AtProject dir -> dir </> ".helix" </> "languages.toml"
            AtGlobal -> helixConfigDir env </> "languages.toml"
      existing <- readIfExists path
      pure [StepFile (planHelixFile path existing (fromMaybe "tynix-lsp" lsp))]
    _ -> pure []
  where
    jsonStep path edits = do
      existing <- readIfExists path
      pure (StepFile (planJsonFile (installForce opts) path existing edits))

editorNotes :: Editor -> ResolvedScope -> [Step]
editorNotes editor scope =
  case editor of
    Zed ->
      [ StepNote
          "Zed installs the tynix extension on its next start via \
          \auto_install_extensions. Until tynix is listed in the Zed \
          \extension registry, install it with \"zed: install dev extension\" \
          \pointing at editors/zed (or run `vp ide` in a tynix checkout)."
      ]
    Neovim
      | AtProject _ <- scope ->
          [ StepNote
              "Project-local .nvim.lua files only load with `:set exrc` (Neovim \
              \0.9+ asks you to trust the file once)."
          ]
    Helix ->
      [ StepNote
          "Helix reuses its built-in Nix tree-sitter grammar for tynix; run \
          \`hx --health tynix` to confirm the language server is found."
      ]
    _ -> []

readIfExists :: FilePath -> IO (Maybe Text)
readIfExists path = do
  exists <- doesFileExist path
  if exists
    then either (const Nothing) Just <$> try @IOException (TextIO.readFile path)
    else pure Nothing

-- Paths ---------------------------------------------------------------------

vscodeUserDir :: IdeEnv -> Editor -> FilePath
vscodeUserDir env editor =
  case ideOs env of
    "darwin" -> ideHome env </> "Library" </> "Application Support" </> app </> "User"
    "mingw32" ->
      fromMaybe (ideHome env </> "AppData" </> "Roaming") (ideAppData env)
        </> app
        </> "User"
    _ -> ideConfigHome env </> app </> "User"
  where
    app = case editor of
      Cursor -> "Cursor"
      VSCodium -> "VSCodium"
      _ -> "Code"

zedConfigDir :: IdeEnv -> FilePath
zedConfigDir env =
  case ideOs env of
    "mingw32" ->
      fromMaybe (ideHome env </> "AppData" </> "Roaming") (ideAppData env) </> "Zed"
    _ -> ideConfigHome env </> "zed"

zedSupportDir :: IdeEnv -> FilePath
zedSupportDir env =
  case ideOs env of
    "darwin" -> ideHome env </> "Library" </> "Application Support" </> "Zed"
    "mingw32" -> ideHome env </> "AppData" </> "Local" </> "Zed"
    _ -> ideHome env </> ".local" </> "share" </> "zed"

neovimConfigDir :: IdeEnv -> FilePath
neovimConfigDir env =
  case ideOs env of
    "mingw32" -> ideHome env </> "AppData" </> "Local" </> "nvim"
    _ -> ideConfigHome env </> "nvim"

helixConfigDir :: IdeEnv -> FilePath
helixConfigDir env =
  case ideOs env of
    "mingw32" ->
      fromMaybe (ideHome env </> "AppData" </> "Roaming") (ideAppData env) </> "helix"
    _ -> ideConfigHome env </> "helix"

-- Settings content -----------------------------------------------------------

-- | Settings for the VS Code family. Keys come from the extension's
-- @contributes.configuration@ (editors/vscode/package.json).
vscodeSettingsEdits :: Maybe FilePath -> [JsonEdit]
vscodeSettingsEdits lsp =
  [SetKey ["tynix.server.path"] (JString (Text.pack path)) | Just path <- [lsp]]
    <> [ SetKey ["files.associations", "*.tynix"] (JString "tynix"),
         SetKey ["files.associations", "*.d.tynix"] (JString "tynix")
       ]

-- | Zed settings. The language-server id comes from editors/zed/extension.toml
-- (@[language_servers.tynix-lsp]@); the extension honours @lsp.<id>.binary@.
zedSettingsEdits :: Maybe FilePath -> [JsonEdit]
zedSettingsEdits lsp =
  SetKey ["auto_install_extensions", "tynix"] (JBool True)
    : [ SetKey ["lsp", "tynix-lsp", "binary", "path"] (JString (Text.pack path))
      | Just path <- [lsp]
      ]

-- | Plan a JSONC settings merge. Existing keys are preserved; when the file
-- holds comments (which a rewrite would drop) the change is only made with
-- @force@, after a backup.
planJsonFile :: Bool -> FilePath -> Maybe Text -> [JsonEdit] -> FileChange
planJsonFile force path existing edits =
  case existing of
    Nothing ->
      either
        (\err -> FileManual path (Text.pack err) "")
        (FileCreate path . renderJson "  ")
        (applyJsonEdits edits (JObject []))
    Just content ->
      case mergeExisting content of
        Left reason -> FileManual path reason snippet
        Right (parsed, updated)
          | updated == parsedValue parsed -> FileUnchanged path
          | parsedHasComments parsed && not force ->
              FileManual path commentsWarning snippet
          | otherwise ->
              FileUpdate
                path
                content
                (renderJson (detectIndent content) updated)
                (if parsedHasComments parsed then Just (path <> ".bak") else Nothing)
  where
    snippet = either (const "") (renderJson "  ") (applyJsonEdits edits (JObject []))
    mergeExisting content = do
      parsed <- either (Left . parseFailure) Right (parseJsonc content)
      updated <-
        either (Left . mergeFailure) Right (applyJsonEdits edits (parsedValue parsed))
      pure (parsed, updated)
    parseFailure err =
      "could not parse the existing file ("
        <> Text.pack err
        <> "); add these settings by hand"
    mergeFailure err =
      "could not merge safely (" <> Text.pack err <> "); add these settings by hand"
    commentsWarning =
      "the file contains comments that a rewrite would drop; "
        <> "add these settings by hand or re-run with --force (a .bak backup is written first)"

-- | Marker identifying files generated (and therefore owned) by this command.
managedMarker :: Text
managedMarker = "Managed by `tynix ide install`"

-- | Plan a whole-file write for generated config (Neovim). A file without the
-- marker belongs to the user and is only replaced with @force@.
planManagedFile :: Bool -> FilePath -> Maybe Text -> Text -> FileChange
planManagedFile force path existing content =
  case existing of
    Nothing -> FileCreate path content
    Just current
      | current == content -> FileUnchanged path
      | managedMarker `Text.isInfixOf` current ->
          FileUpdate path current content Nothing
      | force -> FileUpdate path current content (Just (path <> ".bak"))
      | otherwise ->
          FileManual
            path
            "the file exists and was not generated by tynix; merge this by hand \
            \or re-run with --force (a .bak backup is written first)"
            content

neovimSnippet :: Maybe FilePath -> Text
neovimSnippet lsp =
  Text.unlines
    [ "-- " <> managedMarker <> "; re-running it refreshes this file.",
      "-- Remove this header line to take ownership of the file.",
      "vim.filetype.add({",
      "  extension = { tynix = \"tynix\" },",
      "  pattern = { [\".*%.d%.tynix\"] = \"tynix\" },",
      "})",
      "",
      "local cmd = { " <> luaString (Text.pack (fromMaybe "tynix-lsp" lsp)) <> " }",
      "local root_markers = { \"tynix.config.tynix\", \"flake.nix\", \".git\" }",
      "-- Drop \"nix\" to leave plain .nix buffers to another Nix language server.",
      "local filetypes = { \"tynix\", \"nix\" }",
      "",
      "local ok, tynix = pcall(require, \"tynix\")",
      "if ok and type(tynix.setup) == \"function\" then",
      "  -- The editors/neovim plugin is on the runtimepath: let it drive.",
      "  tynix.setup({ cmd = cmd, root_markers = root_markers, filetypes = filetypes })",
      "elseif vim.lsp.config and vim.lsp.enable then",
      "  vim.lsp.config(\"tynix\", { cmd = cmd, filetypes = filetypes, \
      \root_markers = root_markers })",
      "  vim.lsp.enable(\"tynix\")",
      "else",
      "  vim.api.nvim_create_autocmd(\"FileType\", {",
      "    pattern = filetypes,",
      "    callback = function(ev)",
      "      vim.lsp.start({",
      "        name = \"tynix\",",
      "        cmd = cmd,",
      "        root_dir = vim.fs.root(ev.buf, root_markers) or vim.loop.cwd(),",
      "      })",
      "    end,",
      "  })",
      "end"
    ]

luaString :: Text -> Text
luaString s = "\"" <> Text.concatMap escape s <> "\""
  where
    escape = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      c -> Text.singleton c

-- | The two TOML tables tynix needs in Helix's languages.toml.
helixSnippet :: FilePath -> (Text, Text)
helixSnippet lsp =
  ( Text.unlines
      [ "[[language]]",
        "name = \"tynix\"",
        "scope = \"source.tynix\"",
        "injection-regex = \"tynix\"",
        "file-types = [\"tynix\"]",
        "roots = [\"tynix.config.tynix\", \"flake.nix\"]",
        "comment-token = \"#\"",
        "block-comment-tokens = { start = \"/*\", end = \"*/\" }",
        "indent = { tab-width = 2, unit = \"  \" }",
        "language-servers = [\"tynix-lsp\"]",
        "grammar = \"nix\""
      ],
    Text.unlines
      [ "[language-server.tynix-lsp]",
        "command = " <> tomlString (Text.pack lsp),
        "args = [\"--stdio\"]"
      ]
  )

tomlString :: Text -> Text
tomlString s = "\"" <> Text.concatMap escape s <> "\""
  where
    escape = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      c -> Text.singleton c

-- | Append whichever tynix tables are missing from languages.toml. Existing
-- definitions are never edited, so a user's customisations survive re-runs.
planHelixFile :: FilePath -> Maybe Text -> FilePath -> FileChange
planHelixFile path existing lsp =
  case existing of
    Nothing -> FileCreate path (languageBlock <> "\n" <> serverBlock)
    Just current ->
      let (hasLanguage, hasServer) = scanHelix current
          additions = [languageBlock | not hasLanguage] <> [serverBlock | not hasServer]
       in if null additions
            then FileUnchanged path
            else FileUpdate path current (appendBlocks current additions) Nothing
  where
    (languageBlock, serverBlock) = helixSnippet lsp
    appendBlocks current blocks =
      let base = Text.dropWhileEnd isSpace current
       in (if Text.null base then "" else base <> "\n\n") <> Text.intercalate "\n" blocks

-- | Detect an existing tynix @[[language]]@ entry and @tynix-lsp@ server table.
scanHelix :: Text -> (Bool, Bool)
scanHelix content = go Nothing (False, False) (map Text.strip (Text.lines content))
  where
    go _ acc [] = acc
    go table (lang, server) (line : rest)
      | "[" `Text.isPrefixOf` line =
          let header = Text.filter (not . isSpace) (Text.takeWhile (/= '#') line)
              server' =
                server
                  || header `elem` ["[language-server.tynix-lsp]", "[language-server.\"tynix-lsp\"]"]
           in go (Just header) (lang, server') rest
      | otherwise =
          let compact = Text.filter (not . isSpace) (Text.takeWhile (/= '#') line)
              lang' = lang || (table == Just "[[language]]" && compact == "name=\"tynix\"")
              server' =
                server
                  || ( table == Just "[language-server]"
                         && ( "tynix-lsp=" `Text.isPrefixOf` compact
                                || "\"tynix-lsp\"=" `Text.isPrefixOf` compact
                            )
                     )
           in go table (lang', server') rest

-- Applying ------------------------------------------------------------------

-- | Perform the planned steps. Returns the per-step report and whether every
-- step succeeded.
applySteps :: IdeEnv -> [Step] -> IO ([Text], Bool)
applySteps env steps = do
  results <- forM steps $ \case
    StepRun cmd args label -> do
      (code, out, err) <- ideRunProcess env cmd args
      pure $ case code of
        ExitSuccess -> (["✓ " <> label <> " (" <> Text.pack cmd <> ")"], True)
        ExitFailure n ->
          ( [ "✗ "
                <> label
                <> " failed: `"
                <> Text.unwords (map Text.pack (cmd : args))
                <> "` exited with "
                <> Text.pack (show n)
            ]
              <> map ("    " <>) (Text.lines (Text.strip (Text.pack (err <> out)))),
            False
          )
    StepFile change -> applyFileChange change
    other -> pure (renderStatic other, True)
  pure (concatMap fst results, all snd results)

applyFileChange :: FileChange -> IO ([Text], Bool)
applyFileChange change =
  case change of
    FileCreate path content -> write path content >>= \r -> pure (r ("✓ created " <> Text.pack path))
    FileUpdate path old new backup -> do
      backedUp <-
        maybe (pure (Right ())) (\b -> try @IOException (TextIO.writeFile b old)) backup
      case backedUp of
        Left err ->
          pure
            ( [ "✗ could not write backup for "
                  <> Text.pack path
                  <> ": "
                  <> Text.pack (show err)
              ],
              False
            )
        Right () ->
          write path new >>= \r ->
            pure
              ( r
                  ( "✓ updated "
                      <> Text.pack path
                      <> maybe "" (\b -> " (backup: " <> Text.pack b <> ")") backup
                  )
              )
    FileUnchanged path -> pure (["= " <> Text.pack path <> " already up to date"], True)
    FileManual{} -> pure (renderFileChange False change, True)
  where
    write path content = do
      result <- try @IOException $ do
        createDirectoryIfMissing True (takeDirectory path)
        writeFileAtomic path content
      pure $ \okLine -> case result of
        Left err ->
          ( ["✗ could not write " <> Text.pack path <> ": " <> Text.pack (show err)],
            False
          )
        Right () -> ([okLine], True)

renderStatic :: Step -> [Text]
renderStatic = \case
  StepOk msg -> ["✓ " <> msg]
  StepWarn msg -> ["! " <> msg]
  StepNote msg -> ["note: " <> msg]
  _ -> []

-- | Render a plan for `--dry-run`, with a line diff for each file.
renderPlan :: Text -> [Step] -> Text
renderPlan header steps =
  Text.unlines (header : map ("  " <>) (concatMap renderStep steps))
  where
    renderStep = \case
      StepRun cmd args _ -> ["would run: " <> Text.unwords (map Text.pack (cmd : args))]
      StepFile change -> renderFileChange True change
      other -> renderStatic other

renderFileChange :: Bool -> FileChange -> [Text]
renderFileChange dry = \case
  FileCreate path content ->
    [(if dry then "would create " else "created ") <> Text.pack path]
      <> indent (map ("+ " <>) (Text.lines content))
  FileUpdate path old new backup ->
    [ (if dry then "would update " else "updated ")
        <> Text.pack path
        <> maybe "" (\b -> " (backup: " <> Text.pack b <> ")") backup
    ]
      <> indent (lineDiff old new)
  FileUnchanged path -> ["= " <> Text.pack path <> " already up to date"]
  FileManual path reason snippet ->
    ["! " <> Text.pack path <> ": " <> reason] <> indent (Text.lines snippet)
  where
    indent = map ("    " <>)

renderReport :: Text -> [Text] -> Text
renderReport header lines' = Text.unlines (header : map ("  " <>) lines')

-- | Plan and (unless dry-running) apply an install. 'Left' carries a report
-- when any step failed.
runInstall :: IdeEnv -> InstallOptions -> IO (Either String Text)
runInstall env opts = do
  (header, steps) <- planInstall env opts
  if installDryRun opts
    then pure (Right (renderPlan header steps))
    else do
      (lines', ok) <- applySteps env steps
      let report = renderReport header lines'
      pure (if ok then Right report else Left (Text.unpack (Text.stripEnd report)))

-- | A compact line diff: changed lines with one line of context, LCS based.
lineDiff :: Text -> Text -> [Text]
lineDiff old new = collapse (diffLines (Text.lines old) (Text.lines new))
  where
    collapse ops =
      let tagged = zip [0 :: Int ..] ops
          changed = [i | (i, (tag, _)) <- tagged, tag /= ' ']
          near i = any (\c -> abs (c - i) <= 1) changed
          go _ [] = []
          go skipped ((i, (tag, line)) : rest)
            | near i = (Text.singleton tag <> " " <> line) : go False rest
            | skipped = go True rest
            | otherwise = "  ..." : go True rest
       in go False tagged

diffLines :: [Text] -> [Text] -> [(Char, Text)]
diffLines xs ys = walk 0 0
  where
    n = length xs
    m = length ys
    xa = listArray (0, n - 1) xs :: Array Int Text
    ya = listArray (0, m - 1) ys :: Array Int Text
    table :: Array (Int, Int) Int
    table =
      listArray
        ((0, 0), (n, m))
        [ if i == n || j == m
            then 0
            else
              if xa ! i == ya ! j
                then 1 + table ! (i + 1, j + 1)
                else max (table ! (i + 1, j)) (table ! (i, j + 1))
        | i <- [0 .. n],
          j <- [0 .. m]
        ]
    walk i j
      | i < n && j < m && xa ! i == ya ! j = (' ', xa ! i) : walk (i + 1) (j + 1)
      | i < n && (j == m || table ! (i + 1, j) >= table ! (i, j + 1)) =
          ('-', xa ! i) : walk (i + 1) j
      | j < m = ('+', ya ! j) : walk i (j + 1)
      | otherwise = []

-- Detection -----------------------------------------------------------------

-- | Whether an editor looks installed: its CLI on PATH or its config/support
-- directory present. Returns the evidence found.
detectEditor :: IdeEnv -> Editor -> IO (Maybe Text)
detectEditor env editor = do
  cli <- firstExecutable env (editorCliCandidates editor)
  case cli of
    Just path -> pure (Just (Text.pack path))
    Nothing -> do
      dirs <- filterM doesDirectoryExist (configDirs editor)
      pure (Text.pack . displayPath <$> listToMaybe dirs)
  where
    displayPath path = "~" </> makeRelative (ideHome env) path
    configDirs = \case
      VSCode -> [takeDirectory (vscodeUserDir env VSCode)]
      Cursor -> [takeDirectory (vscodeUserDir env Cursor)]
      VSCodium -> [takeDirectory (vscodeUserDir env VSCodium)]
      Zed -> [zedConfigDir env, zedSupportDir env]
      Neovim -> [neovimConfigDir env]
      Helix -> [helixConfigDir env]

-- | Render `tynix ide list`.
listEditors :: IdeEnv -> IO Text
listEditors env = do
  rows <- forM allEditors $ \editor -> do
    detected <- detectEditor env editor
    pure (editor, detected)
  let width = maximum (map (Text.length . editorName) allEditors)
      row (editor, detected) =
        "  "
          <> Text.justifyLeft width ' ' (editorName editor)
          <> "  "
          <> maybe "not detected" ("detected: " <>) detected
  pure $
    Text.unlines $
      ["supported editors (install with `tynix ide install <editor>`):"]
        <> map row rows

-- | Whether tynix is wired into an editor: the extension for the VS Code
-- family and Zed, a tynix config for Neovim and Helix. 'Nothing' when it cannot
-- be determined (for example the editor CLI is missing).
editorIntegrationStatus :: IdeEnv -> Editor -> IO (Maybe Bool)
editorIntegrationStatus env editor
  | isVSCodeFamily editor = do
      cli <- firstExecutable env (editorCliCandidates editor)
      traverse (extensionInstalledWith env) cli
  | otherwise =
      case editor of
        Zed ->
          Just
            <$> doesDirectoryExist
              (zedSupportDir env </> "extensions" </> "installed" </> "tynix")
        Neovim ->
          Just
            <$> doesFileExist (neovimConfigDir env </> "after" </> "plugin" </> "tynix.lua")
        Helix -> do
          content <- readIfExists (helixConfigDir env </> "languages.toml")
          pure (Just (maybe False (uncurry (&&) . scanHelix) content))
        _ -> pure Nothing
