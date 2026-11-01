{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Testable CLI helpers for tynix.
module Cli
  ( Command (..),
    OutputFormat (..),
    commandOutputFormat,
    commandOutputPath,
    commandParser,
    executeCommand,
    ideInstallParser,
    lspCommandArgs,
    renderAnalysis,
    renderVersion,
    runDoctor,
    writeOutput,
  )
where

import Control.Monad (void)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.IO qualified as TextIO
import Doctor (doctorChecks, doctorSucceeded, renderDoctor, renderDoctorJson)
import Driver
  ( Analysis (..),
    SupportCache,
    analyzeFile,
    analyzeFileWith,
    compileFile,
    compileFileUnchecked,
    compileFileWith,
    emitFile,
    emitFileAsWith,
    newSupportCache,
  )
import Ide
  ( Editor (..),
    InstallOptions (..),
    Scope (..),
    defaultIdeEnv,
    defaultInstallOptions,
    editorFromName,
    listEditors,
    runInstall,
  )
import Options.Applicative
import Pretty (renderScheme)
import Project
import System.Directory (createDirectoryIfMissing)
import System.FilePath (makeRelative, takeDirectory)

data OutputFormat
  = TextFormat
  | JsonFormat
  deriving (Eq, Show)

-- | Supported subcommands.
data Command
  = Compile FilePath (Maybe FilePath) Bool
  | Check FilePath OutputFormat
  | Emit FilePath (Maybe FilePath)
  | Init (Maybe FilePath) [Editor]
  | Scaffold (Maybe FilePath)
  | CheckProject (Maybe FilePath) OutputFormat
  | BuildProject (Maybe FilePath) OutputFormat
  | EmitProject (Maybe FilePath) OutputFormat
  | Version OutputFormat
  | Lsp (Maybe FilePath)
  | IdeInstall InstallOptions
  | IdeList
  | Doctor OutputFormat
  deriving (Eq, Show)

data PlannedWrite = PlannedWrite FilePath Text

type ProjectOutputPlan =
  (ProjectSource, FilePath, FilePath, Either String [PlannedWrite])

-- | Command-line parser definition.
commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "compile" (info compileP (progDesc "Compile .tynix to .nix"))
        <> command "check" (info checkP (progDesc "Type-check a .tynix file"))
        <> command "emit" (info emitP (progDesc "Emit a .d.tynix declaration file"))
        <> command
          "init"
          (info initP (progDesc "Create tynix.config.tynix and starter files"))
        <> command
          "scaffold"
          (info scaffoldP (progDesc "Create project files from tynix.config.tynix"))
        <> command
          "check-project"
          (info checkProjectP (progDesc "Type-check every discovered project source file"))
        <> command
          "build"
          ( info
              buildProjectP
              (progDesc "Compile project sources and emit generated declarations")
          )
        <> command
          "emit-project"
          ( info
              emitProjectP
              (progDesc "Emit declaration files for every discovered project source file")
          )
        <> command "version" (info versionP (progDesc "Show the tynix version"))
        <> command
          "lsp"
          (info lspP (progDesc "Launch the tynix language server over stdio"))
        <> command
          "ide"
          (info ideP (progDesc "Set up editor integrations (install, list)"))
        <> command
          "doctor"
          (info doctorP (progDesc "Check the tynix toolchain, project, and editor setup"))
    )
  where
    fileArg = strArgument (metavar "FILE")
    dirArg = optional (strArgument (metavar "DIRECTORY"))
    outputOpt = optional (strOption (short 'o' <> long "output" <> metavar "OUTPUT"))
    formatOpt =
      option
        formatReader
        ( long "format"
            <> short 'f'
            <> value TextFormat
            <> showDefaultWith renderFormat
            <> metavar "text|json"
        )
    compileP =
      Compile
        <$> fileArg
        <*> outputOpt
        <*> switch
          ( long "no-check"
              <> help "Erase types without type-checking (parse errors still fail)"
          )
    checkP = Check <$> fileArg <*> formatOpt
    emitP = Emit <$> fileArg <*> outputOpt
    initP =
      Init
        <$> dirArg
        <*> many
          ( option
              editorReader
              ( long "editor"
                  <> metavar "EDITOR"
                  <> help "Also run `tynix ide install EDITOR` for the new project (repeatable)"
              )
          )
    scaffoldP = Scaffold <$> dirArg
    checkProjectP = CheckProject <$> dirArg <*> formatOpt
    buildProjectP = BuildProject <$> dirArg <*> formatOpt
    emitProjectP = EmitProject <$> dirArg <*> formatOpt
    versionP = Version <$> formatOpt
    lspP = Lsp <$> optional (strOption (long "log-file" <> metavar "PATH"))
    ideP =
      hsubparser
        ( command
            "install"
            ( info
                (IdeInstall <$> ideInstallParser)
                (progDesc "Install the tynix extension and write editor config")
            )
            <> command
              "list"
              ( info
                  (pure IdeList)
                  (progDesc "List supported editors and whether they are detected")
              )
        )
    doctorP = Doctor <$> formatOpt

-- | Options for `tynix ide install`.
ideInstallParser :: Parser InstallOptions
ideInstallParser =
  InstallOptions
    <$> argument editorReader (metavar "vscode|cursor|vscodium|zed|neovim|helix")
    <*> scopeP
    <*> switch
      ( long "dry-run"
          <> short 'n'
          <> help "Print the planned changes without touching anything"
      )
    <*> ( not
            <$> switch
              ( long "no-extension"
                  <> help "Only write config; skip installing the editor extension"
              )
        )
    <*> switch
      ( long "force"
          <> help "Rewrite files that cannot be merged safely, keeping a .bak backup"
      )
    <*> optional
      ( strOption
          ( long "lsp-path"
              <> metavar "PATH"
              <> help
                "tynix-lsp path to pin in editor settings (default: resolved from \
                \PATH; \"\" to leave unset)"
          )
      )
  where
    scopeP =
      ( ProjectScope
          <$> strOption
            ( long "project"
                <> metavar "DIR"
                <> help "Write project settings under DIR (default: current directory)"
            )
      )
        <|> flag'
          GlobalScope
          ( long "global"
              <> short 'g'
              <> help "Write user-level settings instead of project settings"
          )
        <|> pure DefaultScope

editorReader :: ReadM Editor
editorReader = eitherReader editorFromName

-- | Extract the explicit destination path carried by a command, if any.
commandOutputPath :: Command -> Maybe FilePath
commandOutputPath cmd =
  case cmd of
    Compile _ output _ -> output
    Emit _ output -> output
    Check _ _ -> Nothing
    Init _ _ -> Nothing
    Scaffold _ -> Nothing
    CheckProject _ _ -> Nothing
    BuildProject _ _ -> Nothing
    EmitProject _ _ -> Nothing
    Version _ -> Nothing
    Lsp _ -> Nothing
    IdeInstall _ -> Nothing
    IdeList -> Nothing
    Doctor _ -> Nothing

-- | Extract the requested machine-readable output format, if a command has one.
commandOutputFormat :: Command -> Maybe OutputFormat
commandOutputFormat cmd =
  case cmd of
    Check _ format -> Just format
    CheckProject _ format -> Just format
    BuildProject _ format -> Just format
    EmitProject _ format -> Just format
    Version format -> Just format
    Doctor format -> Just format
    Compile{} -> Nothing
    Emit _ _ -> Nothing
    Init _ _ -> Nothing
    Scaffold _ -> Nothing
    Lsp _ -> Nothing
    IdeInstall _ -> Nothing
    IdeList -> Nothing

-- | Execute one CLI command and return the rendered text payload.
executeCommand :: Command -> IO (Either String Text)
executeCommand cmd =
  case cmd of
    Compile input _ False -> compileFile input
    Compile input _ True -> compileFileUnchecked input
    Check input format -> do
      result <- analyzeFile input
      pure (renderSingleCheck input format result)
    Emit input _ -> emitFile input
    Init target editors -> initWithEditors target editors
    Scaffold target -> scaffoldProject target
    CheckProject target format -> executeProjectCheck target format
    BuildProject target format -> executeProjectBuild target format
    EmitProject target format -> executeProjectEmit target format
    Version format -> pure (Right (renderVersion "unknown" format))
    Lsp _ -> pure (Left "tynix lsp must be launched by the executable entry point")
    IdeInstall opts -> defaultIdeEnv >>= \env -> runInstall env opts
    IdeList -> defaultIdeEnv >>= fmap Right . listEditors
    Doctor format -> do
      (_, report) <- runDoctor "unknown" False format
      pure (Right report)

-- | Run `tynix doctor`, returning whether it passed and the rendered report.
runDoctor :: Text -> Bool -> OutputFormat -> IO (Bool, Text)
runDoctor version color format = do
  env <- defaultIdeEnv
  checks <- doctorChecks env version
  pure
    ( doctorSucceeded checks,
      case format of
        TextFormat -> renderDoctor color checks
        JsonFormat -> renderDoctorJson cliSchemaVersion checks
    )

-- | `tynix init`, optionally chaining into `tynix ide install` for each editor.
-- Editors with project settings are configured inside the new project;
-- Neovim keeps its user-level default.
initWithEditors :: Maybe FilePath -> [Editor] -> IO (Either String Text)
initWithEditors target editors = do
  initResult <- initProject target
  case initResult of
    Left err -> pure (Left err)
    Right summary -> go [summary] editors
  where
    go acc [] = pure (Right (Text.intercalate "\n" (reverse acc)))
    go acc (editor : rest) = do
      env <- defaultIdeEnv
      let scope = case (editor, target) of
            (Neovim, _) -> DefaultScope
            (_, Just dir) -> ProjectScope dir
            (_, Nothing) -> DefaultScope
      result <- runInstall env ((defaultInstallOptions editor){installScope = scope})
      case result of
        Left err ->
          pure (Left (Text.unpack (Text.intercalate "\n" (reverse acc)) <> "\n" <> err))
        Right report -> go (report : acc) rest

-- | Pretty-print the inferred root and bindings for `tynix check`.
renderAnalysis :: Analysis -> Text
renderAnalysis analysis =
  Text.unlines $
    maybe [] (\root -> ["root: " <> renderScheme root]) (analysisRoot analysis)
      <> [ name <> " :: " <> renderScheme scheme
         | (name, scheme) <- Map.toList (analysisBindings analysis)
         ]

-- | Render the CLI version in the same text/json formats used by reports.
renderVersion :: Text -> OutputFormat -> Text
renderVersion version format =
  case format of
    TextFormat -> "tynix " <> version
    JsonFormat ->
      jsonText $
        object
          [ "schemaVersion" .= cliSchemaVersion,
            "action" .= ("version" :: Text),
            "success" .= True,
            "version" .= version
          ]

-- | Arguments used when `tynix lsp` delegates to the standalone LSP executable.
lspCommandArgs :: Maybe FilePath -> [String]
lspCommandArgs logFile =
  ["--stdio"] <> maybe [] (\path -> ["--log-file", path]) logFile

-- | Write command output either to stdout or an explicit file.
writeOutput :: Maybe FilePath -> Text -> IO ()
writeOutput output content =
  case output of
    Nothing -> TextIO.putStrLn content
    Just path -> do
      createDirectoryIfMissing True (takeDirectory path)
      writeFileAtomic path content

executeProjectCheck :: Maybe FilePath -> OutputFormat -> IO (Either String Text)
executeProjectCheck target format = do
  configResult <- loadProject target
  case configResult of
    Left err -> pure (renderedFailure format (projectErrorJson "check" Nothing err) err)
    Right config -> do
      sources <- discoverProjectSources config
      if null sources
        then
          pure
            ( renderedFailure
                format
                (projectErrorJson "check" (Just config) "no project source files discovered")
                "no project source files discovered"
            )
        else do
          cache <- newSupportCache
          entries <-
            traverse
              (\source -> (source,) <$> analyzeFileWith cache (projectSourcePath source))
              sources
          pure (renderProjectCheck format config entries)

executeProjectBuild :: Maybe FilePath -> OutputFormat -> IO (Either String Text)
executeProjectBuild target format = do
  configResult <- loadProject target
  case configResult of
    Left err -> pure (renderedFailure format (projectErrorJson "build" Nothing err) err)
    Right config -> do
      sources <- discoverProjectSources config
      if null sources
        then
          pure
            ( renderedFailure
                format
                (projectErrorJson "build" (Just config) "no project source files discovered")
                "no project source files discovered"
            )
        else do
          cache <- newSupportCache
          plans <- traverse (planBuildOne cache config) sources
          case renderProjectBuild format config (reportEntries plans) of
            Left err -> pure (Left err)
            Right report -> do
              writeOutputPlans plans
              pure (Right report)

executeProjectEmit :: Maybe FilePath -> OutputFormat -> IO (Either String Text)
executeProjectEmit target format = do
  configResult <- loadProject target
  case configResult of
    Left err ->
      pure (renderedFailure format (projectErrorJson "emit-project" Nothing err) err)
    Right config -> do
      sources <- discoverProjectSources config
      if null sources
        then
          pure
            ( renderedFailure
                format
                ( projectErrorJson
                    "emit-project"
                    (Just config)
                    "no project source files discovered"
                )
                "no project source files discovered"
            )
        else do
          cache <- newSupportCache
          plans <- traverse (planEmitOne cache config) sources
          case renderProjectEmit format config (reportEntries plans) of
            Left err -> pure (Left err)
            Right report -> do
              writeOutputPlans plans
              pure (Right report)

renderSingleCheck ::
  FilePath -> OutputFormat -> Either String Analysis -> Either String Text
renderSingleCheck input format result =
  case (format, result) of
    (TextFormat, _) -> fmap renderAnalysis result
    (JsonFormat, Left _) -> Left (Text.unpack rendered)
    (JsonFormat, Right _) -> Right rendered
  where
    rendered = jsonText (object (commonFields <> either failureFields successFields result))
    commonFields =
      [ "schemaVersion" .= cliSchemaVersion,
        "action" .= ("check" :: Text),
        "file" .= input
      ]
    failureFields err =
      [ "success" .= False,
        "root" .= (Nothing :: Maybe Text),
        "bindings" .= (Map.empty :: Map.Map Text Text),
        "error" .= err
      ]
    successFields analysis =
      [ "success" .= True,
        "root" .= fmap renderScheme (analysisRoot analysis),
        "bindings" .= Map.map renderScheme (analysisBindings analysis),
        "error" .= (Nothing :: Maybe Text)
      ]

renderProjectCheck ::
  OutputFormat ->
  ProjectConfig ->
  [(ProjectSource, Either String Analysis)] ->
  Either String Text
renderProjectCheck format config entries =
  if any (either (const True) (const False) . snd) entries
    then renderedFailure format payload (Text.unpack textReport)
    else Right (if format == JsonFormat then jsonText payload else textReport)
  where
    payload =
      object
        [ "schemaVersion" .= cliSchemaVersion,
          "action" .= ("check-project" :: Text),
          "projectRoot" .= configRoot config,
          "projectName" .= configName config,
          "summary" .= summaryJson entries,
          "files" .= map fileJson entries
        ]
    textReport =
      Text.unlines $
        [ "checked project " <> configName config,
          "root: " <> Text.pack (configRoot config)
        ]
          <> concatMap fileLines entries
    fileLines (source, result) =
      [ "- " <> statusLabel result <> " " <> displaySourcePath config source
      ]
        <> case result of
          Left err -> ["  " <> Text.pack err]
          Right analysis -> map ("  " <>) (Text.lines (Text.stripEnd (renderAnalysis analysis)))
    fileJson (source, result) =
      object
        [ "source" .= projectSourcePath source,
          "relative" .= projectSourceRelative source,
          "success" .= either (const False) (const True) result,
          "root" .= either (const Nothing) (fmap renderScheme . analysisRoot) result,
          "bindings"
            .= either (const Map.empty) (Map.map renderScheme . analysisBindings) result,
          "error" .= either Just (const Nothing) result
        ]

planBuildOne ::
  SupportCache -> ProjectConfig -> ProjectSource -> IO ProjectOutputPlan
planBuildOne cache config source = do
  let runtimeOutput = projectBuildOutputPath config source
      declarationOutput = projectDeclarationOutputPath config source
  compileResult <- compileFileWith cache (projectSourcePath source)
  case compileResult of
    Left err -> pure (source, runtimeOutput, declarationOutput, Left err)
    Right compiled -> do
      emitResult <-
        emitFileAsWith cache (projectSourcePath source) runtimeOutput declarationOutput
      case emitResult of
        Left err -> pure (source, runtimeOutput, declarationOutput, Left err)
        Right declaration ->
          pure
            ( source,
              runtimeOutput,
              declarationOutput,
              Right
                [ PlannedWrite runtimeOutput compiled,
                  PlannedWrite declarationOutput declaration
                ]
            )

planEmitOne ::
  SupportCache -> ProjectConfig -> ProjectSource -> IO ProjectOutputPlan
planEmitOne cache config source = do
  let runtimeOutput = projectBuildOutputPath config source
      declarationOutput = projectDeclarationOutputPath config source
  result <-
    emitFileAsWith cache (projectSourcePath source) runtimeOutput declarationOutput
  case result of
    Left err -> pure (source, runtimeOutput, declarationOutput, Left err)
    Right declaration ->
      pure
        ( source,
          runtimeOutput,
          declarationOutput,
          Right [PlannedWrite declarationOutput declaration]
        )

reportEntries ::
  [ProjectOutputPlan] -> [(ProjectSource, FilePath, FilePath, Either String ())]
reportEntries =
  map
    ( \(source, runtimeOutput, declarationOutput, result) -> (source, runtimeOutput, declarationOutput, void result)
    )

writeOutputPlans :: [ProjectOutputPlan] -> IO ()
writeOutputPlans plans =
  if any (either (const True) (const False) . fourth) plans
    then pure ()
    else
      mapM_
        writePlannedWrite
        [planned | (_, _, _, Right plannedWrites) <- plans, planned <- plannedWrites]

writePlannedWrite :: PlannedWrite -> IO ()
writePlannedWrite (PlannedWrite path content) =
  writeTextFile path content

renderProjectBuild ::
  OutputFormat ->
  ProjectConfig ->
  [(ProjectSource, FilePath, FilePath, Either String ())] ->
  Either String Text
renderProjectBuild format config entries =
  if any (either (const True) (const False) . fourth) entries
    then renderedFailure format payload (Text.unpack textReport)
    else Right (if format == JsonFormat then jsonText payload else textReport)
  where
    payload =
      object
        [ "schemaVersion" .= cliSchemaVersion,
          "action" .= ("build" :: Text),
          "projectRoot" .= configRoot config,
          "projectName" .= configName config,
          "summary" .= buildSummaryJson entries,
          "files" .= map buildJson entries
        ]
    textReport =
      Text.unlines $
        [ "built project " <> configName config,
          "root: " <> Text.pack (configRoot config)
        ]
          <> concatMap buildLines entries
    buildLines (source, runtimeOutput, declarationOutput, result) =
      [ "- " <> statusLabel result <> " " <> displaySourcePath config source
      ]
        <> case result of
          Left err -> ["  " <> Text.pack err]
          Right () ->
            [ "  nix -> " <> Text.pack runtimeOutput,
              "  decl -> " <> Text.pack declarationOutput
            ]
    buildJson (source, runtimeOutput, declarationOutput, result) =
      object
        [ "source" .= projectSourcePath source,
          "relative" .= projectSourceRelative source,
          "runtimeOutput" .= runtimeOutput,
          "declarationOutput" .= declarationOutput,
          "success" .= either (const False) (const True) result,
          "error" .= either Just (const Nothing) result
        ]

renderProjectEmit ::
  OutputFormat ->
  ProjectConfig ->
  [(ProjectSource, FilePath, FilePath, Either String ())] ->
  Either String Text
renderProjectEmit format config entries =
  if any (either (const True) (const False) . fourth) entries
    then renderedFailure format payload (Text.unpack textReport)
    else Right (if format == JsonFormat then jsonText payload else textReport)
  where
    payload =
      object
        [ "schemaVersion" .= cliSchemaVersion,
          "action" .= ("emit-project" :: Text),
          "projectRoot" .= configRoot config,
          "projectName" .= configName config,
          "summary" .= buildSummaryJson entries,
          "files" .= map emitJson entries
        ]
    textReport =
      Text.unlines $
        [ "emitted declarations for project " <> configName config,
          "root: " <> Text.pack (configRoot config)
        ]
          <> concatMap emitLines entries
    emitLines (source, _, declarationOutput, result) =
      [ "- " <> statusLabel result <> " " <> displaySourcePath config source
      ]
        <> case result of
          Left err -> ["  " <> Text.pack err]
          Right () -> ["  decl -> " <> Text.pack declarationOutput]
    emitJson (source, runtimeOutput, declarationOutput, result) =
      object
        [ "source" .= projectSourcePath source,
          "relative" .= projectSourceRelative source,
          "runtimeOutput" .= runtimeOutput,
          "declarationOutput" .= declarationOutput,
          "success" .= either (const False) (const True) result,
          "error" .= either Just (const Nothing) result
        ]

summaryJson :: [(ProjectSource, Either String Analysis)] -> Value
summaryJson entries =
  object
    [ "total" .= length entries,
      "ok" .= length [() | (_, Right _) <- entries],
      "failed" .= length [() | (_, Left _) <- entries]
    ]

buildSummaryJson ::
  [(ProjectSource, FilePath, FilePath, Either String ())] -> Value
buildSummaryJson entries =
  object
    [ "total" .= length entries,
      "ok" .= length [() | (_, _, _, Right ()) <- entries],
      "failed" .= length [() | (_, _, _, Left _) <- entries]
    ]

projectErrorJson :: Text -> Maybe ProjectConfig -> String -> Value
projectErrorJson actionName maybeConfig err =
  object
    [ "schemaVersion" .= cliSchemaVersion,
      "action" .= actionName,
      "projectRoot" .= fmap configRoot maybeConfig,
      "projectName" .= fmap configName maybeConfig,
      "success" .= False,
      "error" .= err
    ]

renderedFailure :: OutputFormat -> Value -> String -> Either String Text
renderedFailure format payload err =
  case format of
    TextFormat -> Left err
    JsonFormat -> Left (Text.unpack (jsonText payload))

statusLabel :: Either a b -> Text
statusLabel = either (const "error") (const "ok")

jsonText :: Value -> Text
jsonText = TextEncoding.decodeUtf8 . LBS.toStrict . encode

cliSchemaVersion :: Int
cliSchemaVersion = 1

writeTextFile :: FilePath -> Text -> IO ()
writeTextFile path content = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFileAtomic path content

displaySourcePath :: ProjectConfig -> ProjectSource -> Text
displaySourcePath config source =
  Text.pack (makeRelative (configRoot config) (projectSourcePath source))

renderFormat :: OutputFormat -> String
renderFormat = \case
  TextFormat -> "text"
  JsonFormat -> "json"

formatReader :: ReadM OutputFormat
formatReader =
  eitherReader $ \case
    "text" -> Right TextFormat
    "json" -> Right JsonFormat
    _ -> Left "expected one of: text, json"

fourth :: (a, b, c, d) -> d
fourth (_, _, _, item) = item
