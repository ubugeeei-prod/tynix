{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | `tynix.config.tynix` loading, source discovery, and scaffolding helpers.
--
-- The config format intentionally stays small and executable-free: it is a root
-- attribute set parsed with the ordinary tynix frontend, then decoded into a
-- concrete project plan.
module Project
  ( ProjectConfig (..),
    ProjectSource (..),
    discoverProjectSources,
    initProject,
    loadProject,
    projectBuildOutputPath,
    projectDeclarationOutputPath,
    scaffoldProject,
    writeFileAtomic,
  )
where

import BuiltinPrelude (builtinPreludeSource)
import Control.Exception (IOException, bracketOnError, try)
import Control.Monad (forM)
import Data.Either (fromRight)
import Data.List (group, isPrefixOf, nub, partition, sort)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Parser (parseProgram)
import Syntax
import System.Directory
  ( canonicalizePath,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    listDirectory,
    makeAbsolute,
    removeFile,
    renameFile,
  )
import System.FilePath
  ( addTrailingPathSeparator,
    isAbsolute,
    makeRelative,
    normalise,
    replaceExtension,
    takeBaseName,
    takeDirectory,
    takeFileName,
    (</>),
  )
import System.IO (hClose, hFlush, openTempFile)

data ProjectConfig = ProjectConfig
  { configRoot :: FilePath,
    configName :: Text,
    configSourceDir :: FilePath,
    configEntry :: FilePath,
    configDeclarationDir :: FilePath,
    configDeclarationPacks :: [FilePath],
    configBuildDir :: FilePath,
    configGeneratedDeclarationDir :: FilePath,
    configEntries :: [FilePath],
    configInclude :: [FilePath],
    configExclude :: [FilePath],
    configBuiltins :: Bool
  }
  deriving (Eq, Show)

data ProjectSource = ProjectSource
  { projectSourcePath :: FilePath,
    projectSourceRelative :: FilePath
  }
  deriving (Eq, Show)

data PlannedFile = PlannedFile
  { plannedPath :: FilePath,
    plannedContent :: Text
  }

initProject :: Maybe FilePath -> IO (Either String Text)
initProject target = do
  root <- resolveRoot target
  createDirectoryIfMissing True root
  let configPath = root </> configFileName
  configExists <- doesFileExist configPath
  if configExists
    then pure (Left ("tynix.config.tynix already exists in " <> root))
    else do
      let config = defaultConfig root
      writeFileAtomic configPath (renderConfig config)
      scaffoldResult <- scaffoldFromConfig config
      pure $
        fmap
          ( \summary ->
              Text.unlines ("created: " : ("- " <> Text.pack configPath) : Text.lines summary)
          )
          scaffoldResult

scaffoldProject :: Maybe FilePath -> IO (Either String Text)
scaffoldProject target = do
  root <- resolveRoot target
  loadProjectConfig (root </> configFileName)
    >>= either (pure . Left) scaffoldFromConfig

loadProject :: Maybe FilePath -> IO (Either String ProjectConfig)
loadProject target = do
  root <- resolveRoot target
  loadProjectConfig (root </> configFileName)

configFileName :: FilePath
configFileName = "tynix.config.tynix"

resolveRoot :: Maybe FilePath -> IO FilePath
resolveRoot = maybe getCurrentDirectory makeAbsolute

defaultConfig :: FilePath -> ProjectConfig
defaultConfig root =
  let name = Text.pack (takeBaseName root)
      sourceDir = root </> "src"
      entry = sourceDir </> "main.tynix"
      declarationDir = root </> "types"
      buildDir = root </> "dist"
      generatedDeclarationDir = buildDir </> "types"
   in ProjectConfig
        { configRoot = root,
          configName = if Text.null name then "tynix-app" else name,
          configSourceDir = sourceDir,
          configEntry = entry,
          configDeclarationDir = declarationDir,
          configDeclarationPacks = [],
          configBuildDir = buildDir,
          configGeneratedDeclarationDir = generatedDeclarationDir,
          configEntries = [],
          configInclude = [],
          configExclude = [],
          configBuiltins = True
        }

renderConfig :: ProjectConfig -> Text
renderConfig config =
  Text.unlines
    [ "{",
      "  name = " <> quoted (configName config) <> ";",
      "  sourceDir = " <> prettyPath (configSourceDir config) <> ";",
      "  entry = " <> prettyPath (configEntry config) <> ";",
      "  declarationDir = " <> prettyPath (configDeclarationDir config) <> ";",
      "  declarationPacks = " <> prettyPathList (configDeclarationPacks config) <> ";",
      "  buildDir = " <> prettyPath (configBuildDir config) <> ";",
      "  generatedDeclarationDir = "
        <> prettyPath (configGeneratedDeclarationDir config)
        <> ";",
      "  entries = " <> prettyPathList (configEntries config) <> ";",
      "  include = " <> prettyPathList (configInclude config) <> ";",
      "  exclude = " <> prettyPathList (configExclude config) <> ";",
      "  builtins = " <> boolLiteral (configBuiltins config) <> ";",
      "}"
    ]
  where
    prettyPath path = Text.pack (relativizeFromRoot (configRoot config) path)
    prettyPathList paths =
      case paths of
        [] -> "[]"
        _ -> "[ " <> Text.intercalate " " (map prettyPath paths) <> " ]"

scaffoldFromConfig :: ProjectConfig -> IO (Either String Text)
scaffoldFromConfig config = do
  results <- traverse materializeFile (plannedFiles config)
  let (created, skipped) = partition fst results
      renderEntry (createdFile, path) =
        (if createdFile then "- created " else "- skipped ") <> Text.pack path
  pure $
    Right $
      Text.unlines $
        ["scaffolded " <> configName config]
          <> map renderEntry created
          <> map renderEntry skipped

plannedFiles :: ProjectConfig -> [PlannedFile]
plannedFiles config =
  [ PlannedFile
      (configRoot config </> "tynix.config.d.tynix")
      (renderConfigDeclarationFile config),
    PlannedFile (configEntry config) (renderEntryFile config)
  ]
    <> [ PlannedFile
           (configDeclarationDir config </> "builtins.d.tynix")
           builtinsTemplate
       | configBuiltins config
       ]

materializeFile :: PlannedFile -> IO (Bool, FilePath)
materializeFile planned = do
  exists <- doesFileExist (plannedPath planned)
  if exists
    then pure (False, plannedPath planned)
    else do
      createDirectoryIfMissing True (takeDirectory (plannedPath planned))
      writeFileAtomic (plannedPath planned) (plannedContent planned)
      pure (True, plannedPath planned)

-- | Write a file by staging the content in a sibling temporary file and then
-- atomically renaming it into place.
--
-- The rename is atomic on POSIX file systems (and on NTFS via Windows
-- semantics), so a crash mid-write can never leave a half-populated file at
-- the target path. On exception the temporary file is removed.
writeFileAtomic :: FilePath -> Text -> IO ()
writeFileAtomic target content =
  bracketOnError
    (openTempFile (takeDirectory target) (takeFileName target <> ".tmp"))
    cleanupOnError
    writeAndCommit
  where
    writeAndCommit (tmpPath, handle) = do
      TextIO.hPutStr handle content
      hFlush handle
      hClose handle
      renameFile tmpPath target
    cleanupOnError (tmpPath, handle) = do
      _ <- try @IOException (hClose handle)
      _ <- try @IOException (removeFile tmpPath)
      pure ()

discoverProjectSources :: ProjectConfig -> IO [ProjectSource]
discoverProjectSources config = do
  explicit <- expandConfiguredPaths (configRoot config) (configEntries config)
  discovered <-
    if null explicit
      then walkTynixFiles (configSourceDir config)
      else pure explicit
  let filtered =
        [ path
        | path <- nub (sort (map normalise discovered)),
          isSourceFile path,
          passesInclude path,
          not (isExcluded path)
        ]
  pure (map toProjectSource filtered)
  where
    includePaths = map (resolveConfigPath (configRoot config)) (configInclude config)
    excludePaths = map (resolveConfigPath (configRoot config)) (configExclude config)
    isSourceFile path =
      ".tynix" `Text.isSuffixOf` Text.pack path
        && not (".d.tynix" `Text.isSuffixOf` Text.pack path)
    passesInclude path =
      null includePaths || any (matchesConfiguredPath path) includePaths
    isExcluded path =
      any (matchesConfiguredPath path) excludePaths
    toProjectSource path =
      let relative =
            if isPathPrefixOf (configSourceDir config) path
              then makeRelative (configSourceDir config) path
              else makeRelative (configRoot config) path
       in ProjectSource
            { projectSourcePath = path,
              projectSourceRelative = relative
            }

projectBuildOutputPath :: ProjectConfig -> ProjectSource -> FilePath
projectBuildOutputPath config source =
  configBuildDir config </> replaceExtension (projectSourceRelative source) "nix"

projectDeclarationOutputPath :: ProjectConfig -> ProjectSource -> FilePath
projectDeclarationOutputPath config source =
  configGeneratedDeclarationDir config
    </> replaceExtension (projectSourceRelative source) "d.tynix"

expandConfiguredPaths :: FilePath -> [FilePath] -> IO [FilePath]
expandConfiguredPaths root =
  fmap concat . traverse expandOne
  where
    expandOne raw = do
      let path = normalise (resolveConfigPath root raw)
      isDir <- doesDirectoryExist path
      if isDir
        then walkTynixFiles path
        else pure [path]

-- | Maximum depth a source-tree walk is allowed to recurse before bailing out.
--
-- Picked generously so realistic project layouts never hit it, but small enough
-- that a misconfigured or maliciously-symlinked workspace still terminates.
walkTynixFilesMaxDepth :: Int
walkTynixFilesMaxDepth = 64

walkTynixFiles :: FilePath -> IO [FilePath]
walkTynixFiles = walkTynixFilesWithLimit walkTynixFilesMaxDepth Set.empty

-- | Cycle-safe, depth-bounded directory walk.
--
-- 'canonicalizePath' is used to detect already-visited directories so symlink
-- loops terminate. When the depth or visited set guard kicks in the helper
-- silently returns no files for the offending branch — callers see a clean
-- short list instead of a hang.
walkTynixFilesWithLimit :: Int -> Set FilePath -> FilePath -> IO [FilePath]
walkTynixFilesWithLimit remaining visited root
  | remaining <= 0 = pure []
  | otherwise = do
      exists <- doesDirectoryExist root
      if exists then walkExisting else pure []
  where
    walkExisting = do
      canonical <- canonicalizePathSafe root
      if Set.member canonical visited
        then pure []
        else walkChildren (Set.insert canonical visited)

    walkChildren seen = do
      names <- sort <$> listDirectory root
      concat <$> traverse (visitChild seen) names

    visitChild seen name = do
      let path = root </> name
      isDir <- doesDirectoryExist path
      if isDir
        then walkTynixFilesWithLimit (remaining - 1) seen path
        else pure [path]

canonicalizePathSafe :: FilePath -> IO FilePath
canonicalizePathSafe path = do
  result <- try @IOException (canonicalizePath path)
  pure (fromRight (normalise path) result)

loadProjectConfig :: FilePath -> IO (Either String ProjectConfig)
loadProjectConfig configPath = do
  exists <- doesFileExist configPath
  if not exists
    then pure (Left ("missing tynix.config.tynix in " <> takeDirectory configPath))
    else do
      inputResult <- readTextFileSafe configPath
      pure $ do
        input <- inputResult
        program <-
          firstTextError
            ("failed to parse " <> configPath <> ": ")
            (parseProgram configPath input)
        expr <-
          maybe
            (Left "tynix.config.tynix must contain a root attribute set")
            (Right . markedValue)
            (programExpr program)
        fields <- decodeAttrSet expr
        let root = takeDirectory configPath
            fallbackName = Text.pack (takeBaseName root)
        name <- maybe (Right fallbackName) decodeStringField (Map.lookup "name" fields)
        sourceDir <-
          maybe
            (Right (root </> "src"))
            (decodePathField root "sourceDir")
            (Map.lookup "sourceDir" fields)
        entry <-
          maybe
            (Right (sourceDir </> "main.tynix"))
            (decodePathField root "entry")
            (Map.lookup "entry" fields)
        declarationDir <-
          maybe
            (Right (root </> "types"))
            (decodePathField root "declarationDir")
            (Map.lookup "declarationDir" fields)
        declarationPacks <-
          maybe
            (Right [])
            (decodePathListField root "declarationPacks")
            (Map.lookup "declarationPacks" fields)
        buildDir <-
          maybe
            (Right (root </> "dist"))
            (decodePathField root "buildDir")
            (Map.lookup "buildDir" fields)
        generatedDeclarationDir <-
          maybe
            (Right (buildDir </> "types"))
            (decodePathField root "generatedDeclarationDir")
            (Map.lookup "generatedDeclarationDir" fields)
        entries <-
          maybe
            (Right [])
            (decodePathListField root "entries")
            (Map.lookup "entries" fields)
        include <-
          maybe
            (Right [])
            (decodePathListField root "include")
            (Map.lookup "include" fields)
        exclude <-
          maybe
            (Right [])
            (decodePathListField root "exclude")
            (Map.lookup "exclude" fields)
        builtins <-
          maybe (Right True) (decodeBoolField "builtins") (Map.lookup "builtins" fields)
        pure
          ProjectConfig
            { configRoot = root,
              configName = if Text.null name then "tynix-app" else name,
              configSourceDir = sourceDir,
              configEntry = entry,
              configDeclarationDir = declarationDir,
              configDeclarationPacks = declarationPacks,
              configBuildDir = buildDir,
              configGeneratedDeclarationDir = generatedDeclarationDir,
              configEntries = entries,
              configInclude = include,
              configExclude = exclude,
              configBuiltins = builtins
            }

decodeAttrSet :: Expr -> Either String (Map.Map Text Expr)
decodeAttrSet = \case
  EAttrSet items ->
    let fields = [name | AttrField name _ <- items]
        duplicates = duplicateNames fields
     in case duplicates of
          duplicate : _ -> Left ("duplicate config field: " <> Text.unpack duplicate)
          [] ->
            fmap Map.fromList $
              forM items $ \case
                AttrField name expr -> pure (name, expr)
                AttrInherit _ -> Left "tynix.config.tynix does not support inherit in the root attrset"
                AttrInheritFrom _ _ -> Left "tynix.config.tynix does not support inherit in the root attrset"
                AttrPath _ _ ->
                  Left
                    "tynix.config.tynix does not support nested attribute paths in the root attrset"
  _ -> Left "tynix.config.tynix must evaluate to an attrset"

decodeStringField :: Expr -> Either String Text
decodeStringField = \case
  EString text -> Right (stringLiteralText text)
  other -> Left ("expected string field in tynix.config.tynix, but got " <> show other)

decodeBoolField :: Text -> Expr -> Either String Bool
decodeBoolField name = \case
  EBool value -> Right value
  other ->
    Left ("expected Bool for " <> Text.unpack name <> ", but got " <> show other)

decodePathField :: FilePath -> Text -> Expr -> Either String FilePath
decodePathField root name = \case
  EPath path -> Right (resolveConfigPath root path)
  EString text -> Right (resolveConfigPath root (Text.unpack (stringLiteralText text)))
  other ->
    Left
      ( "expected path-like field for "
          <> Text.unpack name
          <> ", but got "
          <> show other
      )

decodePathListField :: FilePath -> Text -> Expr -> Either String [FilePath]
decodePathListField root name = \case
  EList items -> traverse decodeItem items
  other ->
    Left
      ( "expected list of path-like values for "
          <> Text.unpack name
          <> ", but got "
          <> show other
      )
  where
    decodeItem = \case
      EPath path -> Right (resolveConfigPath root path)
      EString text -> Right (resolveConfigPath root (Text.unpack (stringLiteralText text)))
      item ->
        Left
          ("expected path-like item in " <> Text.unpack name <> ", but got " <> show item)

resolveConfigPath :: FilePath -> FilePath -> FilePath
resolveConfigPath root path
  | isAbsolute path = normalise path
  | otherwise = normalise (root </> dropDotSlash path)

dropDotSlash :: FilePath -> FilePath
dropDotSlash path =
  case path of
    '.' : '/' : rest -> rest
    _ -> path

relativizeFromRoot :: FilePath -> FilePath -> FilePath
relativizeFromRoot root path = "./" <> makeRelative root path

isPathPrefixOf :: FilePath -> FilePath -> Bool
isPathPrefixOf parent child =
  let normalizedParent = normalise parent
      normalizedChild = normalise child
   in normalizedParent == normalizedChild
        || addTrailingPathSeparator normalizedParent
          `isPrefixOf` addTrailingPathSeparator normalizedChild

matchesConfiguredPath :: FilePath -> FilePath -> Bool
matchesConfiguredPath candidate configured =
  let normalizedCandidate = normalise candidate
      normalizedConfigured = normalise configured
   in normalizedCandidate == normalizedConfigured
        || isPathPrefixOf normalizedConfigured normalizedCandidate

quoted :: Text -> Text
quoted text = "\"" <> Text.concatMap escapeDoubleQuoted text <> "\""

escapeDoubleQuoted :: Char -> Text
escapeDoubleQuoted = \case
  '"' -> "\\\""
  '\\' -> "\\\\"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  char -> Text.singleton char

boolLiteral :: Bool -> Text
boolLiteral True = "true"
boolLiteral False = "false"

renderEntryFile :: ProjectConfig -> Text
renderEntryFile config =
  Text.unlines
    [ "let",
      "  greeting :: String;",
      "  greeting = " <> quoted ("Hello from " <> configName config) <> ";",
      "in greeting"
    ]

renderConfigDeclarationFile :: ProjectConfig -> Text
renderConfigDeclarationFile _ =
  Text.unlines
    [ "type TynixProjectPath = Path | String;",
      "",
      "type TynixProjectConfig = {",
      "  name :: String;",
      "  sourceDir :: TynixProjectPath;",
      "  entry :: TynixProjectPath;",
      "  declarationDir :: TynixProjectPath;",
      "  declarationPacks :: List TynixProjectPath;",
      "  buildDir :: TynixProjectPath;",
      "  generatedDeclarationDir :: TynixProjectPath;",
      "  entries :: List TynixProjectPath;",
      "  include :: List TynixProjectPath;",
      "  exclude :: List TynixProjectPath;",
      "  builtins :: Bool;",
      "};",
      "",
      "declare " <> quoted "./tynix.config.tynix" <> " {",
      "  default :: TynixProjectConfig;",
      "};"
    ]

-- | An editable copy of the bundled `builtins` declarations. Every tynix
-- binary already embeds the same declarations as its prelude; a project file
-- takes precedence, so delete it (or set `builtins = false`) to keep tracking
-- the bundled version.
builtinsTemplate :: Text
builtinsTemplate =
  "# Generated by `tynix init`: an editable copy of the bundled builtins\n"
    <> "# declarations. Delete this file to use the version built into tynix.\n"
    <> builtinPreludeSource

firstTextError :: String -> Either Text a -> Either String a
firstTextError prefix = either (Left . (prefix <>) . Text.unpack) Right

readTextFileSafe :: FilePath -> IO (Either String Text)
readTextFileSafe path = do
  result <- try @IOException (TextIO.readFile path)
  pure $
    case result of
      Left err -> Left ("failed to read " <> path <> ": " <> show err)
      Right input -> Right input

duplicateNames :: (Ord a) => [a] -> [a]
duplicateNames = foldr step [] . group . sort
  where
    step values acc =
      case values of
        first : _ | length values > 1 -> first : acc
        _ -> acc
