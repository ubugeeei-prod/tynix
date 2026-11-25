{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | High-level entry points that stitch parsing, checking, compilation, ambient
-- declaration discovery, and emission together.
--
-- The rest of the repo treats this module as the main service layer. CLI
-- commands call it directly, the LSP keeps analyzed results from it in memory,
-- and tests use it to exercise end-to-end behavior.
module Driver
  ( Analysis (..),
    SupportCache,
    analyzeFile,
    analyzeFileWith,
    analyzeText,
    analyzeTextWith,
    compileFile,
    compileFileUnchecked,
    compileFileWith,
    compileText,
    emitFileAs,
    emitFileAsWith,
    emitFile,
    emitFileTo,
    emitText,
    emitTextAs,
    emitTextTo,
    lookupSymbolType,
    newSupportCache,
    parseText,
    AnalysisError (..),
    analyzeTextDetailedWith,
    renderAnalysisError,
    renderAnalysisErrorWithRange,
    analyzeTextForEditor,
    analyzeTextForEditorWith,
    spanToRange,
  )
where

import Alias
import BuiltinPrelude (builtinPreludePath, builtinPreludeSource)
import Check hiding (resolvePath)
import Check qualified
import Compile
import Control.Applicative ((<|>))
import Control.Exception (IOException, bracket_, displayException, try)
import Control.Monad (foldM, forM, forM_)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (group, intercalate, isSuffixOf, nub, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Diagnostics (DiagnosticCode (..), withCode)
import Emit
import Indexed
import Kind
import Parser
import Pretty (renderExpr)
import Syntax
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    listDirectory,
    pathIsSymbolicLink,
  )
import System.FilePath
  ( isAbsolute,
    joinPath,
    normalise,
    replaceExtension,
    splitDirectories,
    takeDirectory,
    takeExtension,
    takeFileName,
    (</>),
  )
import Type

-- | End-to-end analysis result for one file.
--
-- Keeping the parsed program alongside inferred schemes lets downstream tools
-- answer both syntactic and semantic questions without reparsing.
data Analysis = Analysis
  { -- | The parsed program with source locations stripped, so structural
    -- consumers can pattern-match on it directly.
    analysisProgram :: Program,
    -- | The same program with 'ELoc' source spans retained.
    analysisLocatedProgram :: Program,
    analysisRoot :: Maybe Scheme,
    analysisBindings :: Map Name Scheme,
    analysisAliases :: AliasEnv,
    analysisAmbient :: Map FilePath Scheme
  }
  deriving (Eq, Show)

-- | Parse a source buffer and normalize parser errors to plain strings.
parseText :: FilePath -> Text -> Either String Program
parseText path = either (Left . Text.unpack) Right . parseProgram path

-- | Analyze an in-memory source buffer, loading nearby declaration support.
analyzeText :: FilePath -> Text -> IO (Either String Analysis)
analyzeText path input = newSupportCache >>= \cache -> analyzeTextWith cache path input

-- | A failed analysis: the coded message and, when known, the 1-based
-- @(startLine, startColumn, endLine, endColumn)@ source range it refers to.
data AnalysisError = AnalysisError
  { analysisErrorMessage :: String,
    analysisErrorRange :: Maybe (Int, Int, Int, Int)
  }
  deriving (Eq, Show)

-- | Render an analysis error as text, prefixing `line:column:` when the
-- error carries a location (matching the parser's convention).
renderAnalysisError :: AnalysisError -> String
renderAnalysisError err =
  case analysisErrorRange err of
    Just (line, column, _, _) -> show line <> ":" <> show column <> ": " <> analysisErrorMessage err
    Nothing -> analysisErrorMessage err

-- | Render an analysis error with its full 1-based range as a
-- `line:col:endLine:endCol:` prefix, for editors that underline the span.
renderAnalysisErrorWithRange :: AnalysisError -> String
renderAnalysisErrorWithRange err =
  case analysisErrorRange err of
    Just (line, column, endLine, endColumn) ->
      show line
        <> ":"
        <> show column
        <> ":"
        <> show endLine
        <> ":"
        <> show endColumn
        <> ": "
        <> analysisErrorMessage err
    Nothing -> analysisErrorMessage err

-- | 'analyzeText' with errors rendered by 'renderAnalysisErrorWithRange'.
analyzeTextForEditor :: FilePath -> Text -> IO (Either String Analysis)
analyzeTextForEditor path input = newSupportCache >>= \cache -> analyzeTextForEditorWith cache path input

analyzeTextForEditorWith ::
  SupportCache -> FilePath -> Text -> IO (Either String Analysis)
analyzeTextForEditorWith cache path input =
  either (Left . renderAnalysisErrorWithRange) Right
    <$> analyzeTextDetailedWith cache path input

-- | Convert a span of character offsets into a 1-based line/column range.
-- Trailing whitespace absorbed by the lexer is trimmed from the end.
spanToRange :: Text -> SrcSpan -> (Int, Int, Int, Int)
spanToRange input (SrcSpan start end) =
  let clampedEnd = max start (min end (Text.length input))
      trimmedEnd =
        start
          + Text.length
            (Text.stripEnd (Text.take (clampedEnd - start) (Text.drop start input)))
      (startLine, startColumn) = position start
      (endLine, endColumn) = position (max start trimmedEnd)
   in (startLine, startColumn, endLine, endColumn)
  where
    position offset =
      let before = Text.take offset input
          lineNo = Text.count "\n" before + 1
          column = Text.length (snd (Text.breakOnEnd "\n" before)) + 1
       in (lineNo, column)

-- | 'analyzeText' reusing declaration support already loaded into @cache@.
analyzeTextWith ::
  SupportCache -> FilePath -> Text -> IO (Either String Analysis)
analyzeTextWith cache path input =
  either (Left . renderAnalysisError) Right
    <$> analyzeTextDetailedWith cache path input

-- | Analyze a buffer, keeping structured location information on failure.
--
-- The path is pushed for the duration of the call so that a compiled-source
-- import of this same file is reported as a cycle instead of re-entering it.
analyzeTextDetailedWith ::
  SupportCache -> FilePath -> Text -> IO (Either AnalysisError Analysis)
analyzeTextDetailedWith cache path input =
  let file = normalizeSourcePath path
   in bracket_ (pushCompiled cache file) (popCompiled cache) $ do
        support <- loadSupportWith cache file
        case prepareAnalysis file input support of
          Left err -> pure (Left err)
          Right prepared -> do
            imported <- resolveCompiledImports cache file input (preparedExpr prepared)
            pure $ case imported of
              Left err -> Left err
              Right sourceAmbient -> finishAnalysis file input prepared sourceAmbient

-- | Check each macro template once, attributing failures to its definition.
checkMacroTemplates ::
  CheckContext -> Text -> Program -> Either AnalysisError ()
checkMacroTemplates context input program =
  forM_ (programMacros program) $ \macro ->
    forM_ (zip [1 :: Int ..] (macroRules macro)) (checkRule macro)
  where
    checkRule macro (index, rule) =
      case checkProgramDetailed
        context
        (Program [] [] (Just (Marked Nothing (ruleCheck rule))) []) of
        Right _ -> Right ()
        Left err ->
          Left
            ( AnalysisError
                (templateMessage macro index err)
                (Just (spanToRange input (macroSpan macro)))
            )
    templateMessage macro index err =
      withCode
        TX0004InvalidTemplate
        ( "rule "
            <> show index
            <> " of macro `"
            <> Text.unpack (macroName macro)
            <> "` does not type-check: "
            <> checkErrorMessage err
        )

plain :: Either String a -> Either AnalysisError a
plain = either (\message -> Left (AnalysisError message Nothing)) Right

-- | The parsed file, ready to check once compiled-source imports are resolved.
data PreparedAnalysis = PreparedAnalysis
  { preparedProgram :: Program,
    preparedLocated :: Program,
    preparedAliases :: AliasEnv,
    preparedAmbient :: Map FilePath Scheme,
    preparedExpr :: Expr
  }

prepareAnalysis ::
  FilePath -> Text -> Either String World -> Either AnalysisError PreparedAnalysis
prepareAnalysis file input support = do
  supportWorld <- plain support
  located <- case parseProgramLocatedDetailed file input of
    Left err ->
      Left
        ( AnalysisError
            (Text.unpack (parseErrorMessage err))
            ( Just
                ( parseErrorLine err,
                  parseErrorColumn err,
                  parseErrorLine err,
                  parseErrorColumn err
                )
            )
        )
    Right program -> Right program
  let program = stripProgramLocations located
  _ <-
    plain
      ( validateProgramKinds
          (programAliases program <> worldAliases supportWorld)
          program
      )
  _ <- plain (validateProgramIndexedTypes program)
  localAmbient <- plain (collectAmbient file program)
  pure
    PreparedAnalysis
      { preparedProgram = program,
        preparedLocated = located,
        preparedAliases =
          mkAliasEnv (programAliases program <> worldAliases supportWorld),
        preparedAmbient = localAmbient <> worldAmbient supportWorld,
        preparedExpr = maybe (EAttrSet []) markedValue (programExpr located)
      }

-- | Check a prepared file. Schemes from sibling `.tynix` sources override
-- declarations for the same compiled path: the source is newer than the last
-- emit, and a handwritten `declare` must not hide the implementation.
finishAnalysis ::
  FilePath ->
  Text ->
  PreparedAnalysis ->
  Map FilePath Scheme ->
  Either AnalysisError Analysis
finishAnalysis file input prepared sourceAmbient = do
  let aliases = preparedAliases prepared
      ambient = Map.union sourceAmbient (preparedAmbient prepared)
      located = preparedLocated prepared
      context =
        CheckContext
          { checkAliases = aliases,
            checkAmbient = ambient,
            checkFile = file,
            checkOpenScope = False,
            -- Flakes are evaluated in pure mode.
            checkPureEval = takeFileName file == "flake.tynix"
          }
  -- Each macro rule's template is checked once, here, rather than at every
  -- expansion. Sibling imports run this for the file they read from disk.
  checkMacroTemplates context input located
  result <- case checkProgramDetailed context located of
    Left err ->
      Left
        (AnalysisError (checkErrorMessage err) (spanToRange input <$> checkErrorSpan err))
    Right ok -> Right ok
  pure
    Analysis
      { analysisProgram = preparedProgram prepared,
        analysisLocatedProgram = preparedLocated prepared,
        analysisRoot = resultRoot result,
        analysisBindings = resultBindings result,
        analysisAliases = aliases,
        analysisAmbient = ambient
      }

-- | One `import` of a path or string literal, with the span of that call.
data ImportSite = ImportSite
  { importSiteSpan :: Maybe SrcSpan,
    importSiteTarget :: FilePath
  }

-- | Resolve every static `import ./file.nix` whose sibling `file.tynix` exists.
--
-- Nix evaluates the `.nix`. The `.tynix` beside it is the implementation, so
-- its checked root scheme is the type of that import. A path that is not a
-- `.nix` literal, or whose sibling is absent, keeps the declaration world
-- (or `dynamic`, when nothing declares it). `import` shadowed by a local
-- binding is ignored, matching the checker. `with` is not treated as binding
-- `import`: its scope is a value, and enumerating it would guess.
resolveCompiledImports ::
  SupportCache ->
  FilePath ->
  Text ->
  Expr ->
  IO (Either AnalysisError (Map FilePath Scheme))
resolveCompiledImports cache file input expr =
  foldM step (Right Map.empty) (importSites False Nothing expr)
  where
    step (Left err) _ = pure (Left err)
    step (Right found) site = do
      loaded <- loadCompiledImport cache file input site
      pure $ case loaded of
        Left err -> Left err
        Right Nothing -> Right found
        Right (Just (key, scheme)) -> Right (Map.insert key scheme found)

loadCompiledImport ::
  SupportCache ->
  FilePath ->
  Text ->
  ImportSite ->
  IO (Either AnalysisError (Maybe (FilePath, Scheme)))
loadCompiledImport cache file input site =
  let nixPath = resolvePath file (importSiteTarget site)
   in case compiledSourceOf nixPath of
        Nothing -> pure (Right Nothing)
        Just sourcePath -> do
          exists <- doesFileExist sourcePath
          if not exists
            then pure (Right Nothing)
            else do
              analysis <- analyzeCompiledSource cache sourcePath
              pure $ case analysis of
                Left err -> Left (attributeCompiledImport input site sourcePath err)
                Right found ->
                  Right $
                    fmap
                      (\scheme -> (nixPath, expandScheme (analysisAliases found) scheme))
                      (analysisRoot found)

-- | Read and analyze a `.tynix` file that backs a compiled `.nix` import.
--
-- The result is memoized for this cache. Re-entering a file that is already
-- on the stack is a cycle; the entry point's unsaved buffer is not reused,
-- because a sibling import names the file on disk.
analyzeCompiledSource ::
  SupportCache -> FilePath -> IO (Either AnalysisError Analysis)
analyzeCompiledSource cache path = do
  let file = normalizeSourcePath path
  cached <- Map.lookup file <$> readIORef (cacheCompiled cache)
  case cached of
    Just hit -> pure hit
    Nothing -> do
      stack <- readIORef (cacheCompiledStack cache)
      if file `elem` stack
        then pure (Left (cyclicCompiledImport file stack))
        else do
          readResult <- readTextFile file
          result <- case readResult of
            Left err -> pure (Left (AnalysisError err Nothing))
            Right input -> analyzeTextDetailedWith cache file input
          modifyIORef' (cacheCompiled cache) (Map.insert file result)
          pure result

attributeCompiledImport ::
  Text -> ImportSite -> FilePath -> AnalysisError -> AnalysisError
attributeCompiledImport input site sourcePath err =
  err
    { analysisErrorMessage =
        "compiled source "
          <> sourcePath
          <> " failed to check: "
          <> renderAnalysisError err,
      analysisErrorRange = spanToRange input <$> importSiteSpan site
    }

cyclicCompiledImport :: FilePath -> [FilePath] -> AnalysisError
cyclicCompiledImport file stack =
  AnalysisError
    { analysisErrorMessage =
        withCode
          TD0009CyclicCompiledImport
          ( "cyclic compiled source import: "
              <> formatCycle (reverse stack ++ [file])
          ),
      analysisErrorRange = Nothing
    }

formatCycle :: [FilePath] -> String
formatCycle = intercalate " -> "

-- | The `.tynix` implementation of a compiled `.nix` path, when the import
-- target is itself a `.nix` file. `foo.nix` maps to `foo.tynix`, never to
-- `foo.d.tynix`.
compiledSourceOf :: FilePath -> Maybe FilePath
compiledSourceOf nixPath
  | takeExtension nixPath == ".nix" = Just (replaceExtension nixPath ".tynix")
  | otherwise = Nothing

expandScheme :: AliasEnv -> Scheme -> Scheme
expandScheme env (Scheme vars ty) = Scheme vars (expandAliases env ty)

normalizeSourcePath :: FilePath -> FilePath
normalizeSourcePath = collapseParentSegments . normalise

pushCompiled :: SupportCache -> FilePath -> IO ()
pushCompiled cache file = modifyIORef' (cacheCompiledStack cache) (file :)

popCompiled :: SupportCache -> IO ()
popCompiled cache =
  modifyIORef' (cacheCompiledStack cache) $ \case
    [] -> []
    _ : rest -> rest

-- | Static `import` calls of a path or string literal.
--
-- @shadowed@ is true when a local binding named `import` hides the builtin in
-- this scope. Nix `let` is recursive, and a lambda's formals are in scope in
-- every default, so the flag covers the whole binding group.
importSites :: Bool -> Maybe SrcSpan -> Expr -> [ImportSite]
importSites shadowed here = \case
  ELoc region inner -> importSites shadowed (Just region) inner
  EApp fun arg
    | not shadowed,
      EVar "import" <- peel fun,
      Just target <- literalImportTarget (peel arg) ->
        [ImportSite here target]
  EApp fun arg -> nested [fun, arg]
  ELambda pattern' body ->
    patternImportSites shadowed here pattern'
      ++ importSites (shadowed || patternBindsImport pattern') here body
  EBinaryOp _ left right -> nested [left, right]
  EUnaryOp _ operand -> nested [operand]
  ELet items body ->
    let shadowed' = shadowed || any (letBindsImport . markedValue) items
        inItems item = case markedValue item of
          LetBinding _ expr -> importSites shadowed' here expr
          LetInherit source _ -> maybe [] (importSites shadowed' here) source
          LetPath steps expr ->
            concatMap (stepSites shadowed' here) steps ++ importSites shadowed' here expr
          LetSignature _ _ -> []
     in concatMap inItems items ++ importSites shadowed' here body
  EAttrSet items -> concatMap (attrSites shadowed here) items
  -- `rec` fields are in scope for every field body and for `inherit (source)`,
  -- same as `let`. A non-recursive attrset does not bind its own fields.
  ERec items ->
    let shadowed' = shadowed || any attrBindsImport items
     in concatMap (attrSites shadowed' here) items
  ESelect base steps -> importSites shadowed here base ++ concatMap (stepSites shadowed here) steps
  EHasAttr base steps -> importSites shadowed here base ++ concatMap (stepSites shadowed here) steps
  EAssert cond body -> nested [cond, body]
  EWith scope body -> nested [scope, body]
  EIf cond yesExpr noExpr -> nested [cond, yesExpr, noExpr]
  EList members -> concatMap (importSites shadowed here) members
  ECast inner _ -> importSites shadowed here inner
  -- An ascription is erased. The import inside it is still the import Nix
  -- evaluates, so it has to be typed from the sibling source.
  EAscribe inner _ -> importSites shadowed here inner
  EInterp _ parts -> concatMap (partSites shadowed here) parts
  ESelectOr base steps fallback ->
    importSites shadowed here base
      ++ concatMap (stepSites shadowed here) steps
      ++ importSites shadowed here fallback
  EPathInterp parts -> concatMap (partSites shadowed here) parts
  EVar _ -> []
  EString _ -> []
  EFloat _ -> []
  EInt _ -> []
  EBool _ -> []
  ENull -> []
  EPath _ -> []
  ESearchPath _ -> []
  where
    nested = concatMap (importSites shadowed here)

patternImportSites :: Bool -> Maybe SrcSpan -> Pattern -> [ImportSite]
patternImportSites shadowed here = \case
  PVar _ _ -> []
  PAttrSet fields _ binder ->
    let shadowed' = shadowed || fieldsBindImport fields || binderBindsImport binder
     in concat
          [ importSites shadowed' here expr
          | field <- fields,
            Just expr <- [patternFieldDefault field]
          ]

attrSites :: Bool -> Maybe SrcSpan -> AttrItem -> [ImportSite]
attrSites shadowed here = \case
  AttrField _ expr -> importSites shadowed here expr
  AttrInherit _ -> []
  AttrInheritFrom source _ -> importSites shadowed here source
  AttrPath steps expr -> concatMap (stepSites shadowed here) steps ++ importSites shadowed here expr

stepSites :: Bool -> Maybe SrcSpan -> SelectStep -> [ImportSite]
stepSites shadowed here = \case
  SelectName _ -> []
  SelectDynamic expr -> importSites shadowed here expr

partSites :: Bool -> Maybe SrcSpan -> StringPart -> [ImportSite]
partSites shadowed here = \case
  StrExpr expr -> importSites shadowed here expr
  StrText _ -> []
  StrEscape _ -> []

letBindsImport :: LetItem -> Bool
letBindsImport = \case
  LetBinding name _ -> name == "import"
  LetInherit _ names -> "import" `elem` names
  LetPath (SelectName name : _) _ -> name == "import"
  LetPath _ _ -> False
  LetSignature _ _ -> False

-- | A `rec` attribute introduces `import` when its head name is `import`.
-- Bare `inherit` names count: they are fields of the rec set even though the
-- inherited value itself is resolved in the enclosing scope.
attrBindsImport :: AttrItem -> Bool
attrBindsImport = \case
  AttrField name _ -> name == "import"
  AttrInherit names -> "import" `elem` names
  AttrInheritFrom _ names -> "import" `elem` names
  AttrPath (SelectName name : _) _ -> name == "import"
  AttrPath _ _ -> False

patternBindsImport :: Pattern -> Bool
patternBindsImport = \case
  PVar "import" _ -> True
  PVar _ _ -> False
  PAttrSet fields _ binder -> fieldsBindImport fields || binderBindsImport binder

fieldsBindImport :: [PatternField] -> Bool
fieldsBindImport = any ((== "import") . patternFieldName)

binderBindsImport :: Maybe PatternBinder -> Bool
binderBindsImport = \case
  Just (BinderBefore "import") -> True
  Just (BinderAfter "import") -> True
  _ -> False

peel :: Expr -> Expr
peel = \case
  ELoc _ inner -> peel inner
  other -> other

literalImportTarget :: Expr -> Maybe FilePath
literalImportTarget = \case
  EPath raw -> Just raw
  EString literal -> Just (Text.unpack (stringLiteralText literal))
  ELoc _ inner -> literalImportTarget inner
  _ -> Nothing

-- | Read and analyze a file from disk.
analyzeFile :: FilePath -> IO (Either String Analysis)
analyzeFile path = newSupportCache >>= \cache -> analyzeFileWith cache path

-- | 'analyzeFile' reusing declaration support already loaded into @cache@.
analyzeFileWith :: SupportCache -> FilePath -> IO (Either String Analysis)
analyzeFileWith cache path = readTextFile path >>= either (pure . Left) (analyzeTextWith cache path)

-- | Compile an in-memory `.tynix` buffer into `.nix` text.
compileText :: FilePath -> Text -> IO (Either String Text)
compileText path input = newSupportCache >>= \cache -> compileTextWith cache path input

compileTextWith :: SupportCache -> FilePath -> Text -> IO (Either String Text)
compileTextWith cache path input = do
  checked <- analyzeTextWith cache path input
  pure $
    checked >>= \analysis ->
      either (Left . Text.unpack) Right (compileProgram (analysisProgram analysis))

-- | Compile a file from disk.
compileFile :: FilePath -> IO (Either String Text)
compileFile path = newSupportCache >>= \cache -> compileFileWith cache path

-- | Erase types without type-checking (like TypeScript's transpile-only
-- mode): parse errors still fail, type errors do not.
compileFileUnchecked :: FilePath -> IO (Either String Text)
compileFileUnchecked path = do
  inputResult <- readTextFile path
  pure $ do
    input <- inputResult
    program <- parseText path input
    either (Left . Text.unpack) Right (compileProgram program)

-- | 'compileFile' reusing declaration support already loaded into @cache@.
compileFileWith :: SupportCache -> FilePath -> IO (Either String Text)
compileFileWith cache path = readTextFile path >>= either (pure . Left) (compileTextWith cache path)

-- | Emit a declaration file for an in-memory source buffer.
emitText :: FilePath -> Text -> IO (Either String Text)
emitText path input = do
  checked <- analyzeText path input
  pure $ do
    analysis <- checked
    root <-
      maybe
        ( Left
            ( withCode
                TD0008DeclarationOnlyEmit
                "cannot emit declarations from a declaration-only file"
            )
        )
        Right
        (analysisRoot analysis)
    pure (emitDeclarationFile path (analysisProgram analysis) root)

emitTextTo :: FilePath -> FilePath -> Text -> IO (Either String Text)
emitTextTo source declarationPath input = do
  let runtimeTarget = replaceExtension source "nix"
  emitTextAs source runtimeTarget declarationPath input

emitTextAs ::
  FilePath -> FilePath -> FilePath -> Text -> IO (Either String Text)
emitTextAs source runtimeTarget declarationPath input =
  newSupportCache >>= \cache -> emitTextAsWith cache source runtimeTarget declarationPath input

emitTextAsWith ::
  SupportCache ->
  FilePath ->
  FilePath ->
  FilePath ->
  Text ->
  IO (Either String Text)
emitTextAsWith cache source runtimeTarget declarationPath input = do
  checked <- analyzeTextWith cache source input
  pure $ do
    analysis <- checked
    root <-
      maybe
        ( Left
            ( withCode
                TD0008DeclarationOnlyEmit
                "cannot emit declarations from a declaration-only file"
            )
        )
        Right
        (analysisRoot analysis)
    pure
      ( emitDeclarationFileFor
          runtimeTarget
          declarationPath
          (analysisProgram analysis)
          root
      )

-- | Emit a declaration file for a source file on disk.
emitFile :: FilePath -> IO (Either String Text)
emitFile path = readTextFile path >>= either (pure . Left) (emitText path)

emitFileTo :: FilePath -> FilePath -> IO (Either String Text)
emitFileTo source declarationPath =
  readTextFile source >>= either (pure . Left) (emitTextTo source declarationPath)

emitFileAs :: FilePath -> FilePath -> FilePath -> IO (Either String Text)
emitFileAs source runtimeTarget declarationPath =
  newSupportCache >>= \cache -> emitFileAsWith cache source runtimeTarget declarationPath

-- | 'emitFileAs' reusing declaration support already loaded into @cache@.
emitFileAsWith ::
  SupportCache -> FilePath -> FilePath -> FilePath -> IO (Either String Text)
emitFileAsWith cache source runtimeTarget declarationPath =
  readTextFile source
    >>= either (pure . Left) (emitTextAsWith cache source runtimeTarget declarationPath)

-- | Look up a top-level symbol type exposed by an analysis result.
--
-- `default` is synthesized from the root expression so editor tooling can show
-- something useful even when the file does not bind a name explicitly.
lookupSymbolType :: Analysis -> Name -> Maybe Scheme
lookupSymbolType analysis name =
  Map.lookup name (analysisBindings analysis)
    <|> ( analysisRoot analysis >>= \root -> if name == "default" then Just root else Nothing
        )

data World = World
  { worldAliases :: [TypeAlias],
    worldAmbient :: Map FilePath Scheme
  }

data DeclarationSupportFile = DeclarationSupportFile
  { declarationLoadPath :: FilePath,
    declarationResolvePath :: FilePath
  }
  deriving (Eq, Ord, Show)

builtinsAmbientKey :: FilePath
builtinsAmbientKey = "builtins"

-- | Declaration support discovered for one workspace root.
--
-- Discovery, reading, and parsing depend only on the root, so the expensive
-- part is shared by every source file under it. Only the final filter — a
-- declaration file never contributes support to itself — is per-source, and
-- that is cheap enough to redo.
data SupportBundle = SupportBundle
  { bundleFiles :: [DeclarationSupportFile],
    bundleWorlds :: [Either String World]
  }

-- | Memo tables shared by every file analyzed in one command or request.
--
-- Declaration discovery is keyed by workspace root. Compiled-source analysis
-- is keyed by the normalized `.tynix` path, so a diamond of `import ./x.nix`
-- reads `x.tynix` once. The stack is the files whose analysis is in progress;
-- it exists only to report a cycle.
--
-- The cache holds no invalidation logic on purpose: its scope is one CLI
-- command or one LSP request, so it can never serve a stale read. The entry
-- point's buffer is not stored here, because that buffer may differ from disk.
data SupportCache = SupportCache
  { cacheSupport :: IORef (Map FilePath (Either String SupportBundle)),
    cacheCompiled :: IORef (Map FilePath (Either AnalysisError Analysis)),
    cacheCompiledStack :: IORef [FilePath]
  }

-- | Create an empty 'SupportCache'.
newSupportCache :: IO SupportCache
newSupportCache =
  SupportCache <$> newIORef Map.empty <*> newIORef Map.empty <*> newIORef []

loadSupportWith :: SupportCache -> FilePath -> IO (Either String World)
loadSupportWith cache path = do
  root <- findSupportRoot path
  cached <- Map.lookup root <$> readIORef (cacheSupport cache)
  bundle <- case cached of
    Just hit -> pure hit
    Nothing -> do
      loaded <- loadSupportBundle root
      modifyIORef' (cacheSupport cache) (Map.insert root loaded)
      pure loaded
  pure (withBuiltinPrelude <$> (bundle >>= supportWorldFor path))

-- | The `builtins` declarations embedded in every tynix binary.
--
-- They are parsed once (lazily) and form the base of every world, so
-- `builtins.*` and global builtins such as `toString` are typed with no
-- project setup. A workspace that declares `builtins` itself takes precedence.
builtinPreludeWorld :: Either String World
builtinPreludeWorld = do
  program <- parseText builtinPreludePath builtinPreludeSource
  ambient <- collectAmbient builtinPreludePath program
  pure World{worldAliases = programAliases program, worldAmbient = ambient}
{-# NOINLINE builtinPreludeWorld #-}

withBuiltinPrelude :: World -> World
withBuiltinPrelude world =
  case builtinPreludeWorld of
    Left _ -> world
    Right prelude ->
      World
        { -- Prelude aliases come first so project aliases of the same name win.
          worldAliases = worldAliases prelude <> worldAliases world,
          worldAmbient = Map.union (worldAmbient world) (worldAmbient prelude)
        }

loadSupportBundle :: FilePath -> IO (Either String SupportBundle)
loadSupportBundle root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure (Right (SupportBundle [] []))
    else do
      workspaceFiles <-
        map (\file -> DeclarationSupportFile file file)
          <$> findWorkspaceDeclarationFiles root
      configuredFilesResult <- loadConfiguredDeclarationFiles root
      case configuredFilesResult of
        Left err -> pure (Left err)
        Right configuredFiles -> do
          let declarationFiles = dedupeDeclarationFiles (workspaceFiles <> configuredFiles)
          worlds <- traverse loadDeclarationFile declarationFiles
          pure (Right (SupportBundle declarationFiles worlds))

-- | Assemble the world visible from @source@, dropping the entry for the file
-- being analyzed so a declaration file never declares itself.
supportWorldFor :: FilePath -> SupportBundle -> Either String World
supportWorldFor source bundle = do
  let kept =
        filter
          ((/= normalise source) . declarationLoadPath . fst)
          (zip (bundleFiles bundle) (bundleWorlds bundle))
  loaded <- traverse snd kept
  mergeLoadedWorlds (map fst kept) loaded

dedupeDeclarationFiles :: [DeclarationSupportFile] -> [DeclarationSupportFile]
dedupeDeclarationFiles =
  nub
    . sort
    . map normalizeDeclarationSupportFile

normalizeDeclarationSupportFile ::
  DeclarationSupportFile -> DeclarationSupportFile
normalizeDeclarationSupportFile file =
  file
    { declarationLoadPath = normalise (declarationLoadPath file),
      declarationResolvePath = normalise (declarationResolvePath file)
    }

loadConfiguredDeclarationFiles ::
  FilePath -> IO (Either String [DeclarationSupportFile])
loadConfiguredDeclarationFiles root = do
  let configPath = root </> "tynix.config.tynix"
  exists <- doesFileExist configPath
  if not exists
    then pure (Right [])
    else do
      inputResult <- readTextFile configPath
      case inputResult of
        Left err -> pure (Left err)
        Right input ->
          case parseText configPath input of
            Left err ->
              pure
                ( Left
                    ( withCode
                        TD0004ConfigDecodeError
                        ("failed to parse " <> configPath <> ": " <> err)
                    )
                )
            Right program ->
              case configuredDeclarationPackPaths configPath program of
                Left err -> pure (Left err)
                Right packs -> do
                  expanded <- traverse (expandDeclarationPackPath root) packs
                  pure (fmap concat (sequence expanded))

configuredDeclarationPackPaths ::
  FilePath -> Program -> Either String [FilePath]
configuredDeclarationPackPaths configPath program = do
  expr <-
    maybe
      ( Left
          ( withCode
              TD0004ConfigDecodeError
              ("tynix.config.tynix must contain a root attribute set: " <> configPath)
          )
      )
      (Right . markedValue)
      (programExpr program)
  decodeDeclarationPackField (takeDirectory configPath) expr

decodeDeclarationPackField :: FilePath -> Expr -> Either String [FilePath]
decodeDeclarationPackField root = \case
  EAttrSet items ->
    case [expr | AttrField "declarationPacks" expr <- items] of
      [] -> Right []
      [expr] -> decodePathList root "declarationPacks" expr
      _ ->
        Left
          (withCode TD0004ConfigDecodeError "duplicate config field: declarationPacks")
  _ ->
    Left
      ( withCode
          TD0004ConfigDecodeError
          "tynix.config.tynix must evaluate to an attrset"
      )

decodePathList :: FilePath -> Text -> Expr -> Either String [FilePath]
decodePathList root label = \case
  EList items -> traverse decodeItem items
  other ->
    Left
      ( withCode
          TD0005ConfigBadList
          ( "expected list of path-like values for "
              <> Text.unpack label
              <> ", but got "
              <> Text.unpack (renderExpr other)
          )
      )
  where
    decodeItem = \case
      EPath path -> Right (resolveConfigPath root path)
      EString text -> Right (resolveConfigPath root (Text.unpack (stringLiteralText text)))
      item ->
        Left
          ( withCode
              TD0006ConfigBadItem
              ( "expected path-like item in "
                  <> Text.unpack label
                  <> ", but got "
                  <> Text.unpack (renderExpr item)
              )
          )

expandDeclarationPackPath ::
  FilePath -> FilePath -> IO (Either String [DeclarationSupportFile])
expandDeclarationPackPath root path = do
  let normalized = normalise path
  isDir <- doesDirectoryExist normalized
  fileExists <- doesFileExist normalized
  case (isDir, fileExists) of
    (True, _) -> do
      files <- findDeclarationFiles normalized
      pure (Right (map (mkDeclarationSupportFile root) files))
    (False, True)
      | ".d.tynix" `isSuffixOf` normalized ->
          pure (Right [mkDeclarationSupportFile root normalized])
      | otherwise ->
          pure (Left (withCode TD0006ConfigBadItem (invalidEntry normalized)))
    (False, False) ->
      pure
        ( Left
            ( withCode
                TD0006ConfigBadItem
                ("declarationPacks entry does not exist: " <> normalized)
            )
        )
  where
    invalidEntry normalized =
      "declarationPacks entries must point to .d.tynix files or directories, but got "
        <> normalized

mkDeclarationSupportFile :: FilePath -> FilePath -> DeclarationSupportFile
mkDeclarationSupportFile root loadPath =
  DeclarationSupportFile
    { declarationLoadPath = loadPath,
      declarationResolvePath =
        maybe
          (normalise loadPath)
          (normalise . (root </>) . joinPath)
          (workspacePackRelativeParts loadPath)
    }

workspacePackRelativeParts :: FilePath -> Maybe [FilePath]
workspacePackRelativeParts path =
  findWorkspacePackSuffix (splitDirectories (normalise path))
  where
    findWorkspacePackSuffix parts@("registry" : "workspace" : _) = Just parts
    findWorkspacePackSuffix (_ : rest) = findWorkspacePackSuffix rest
    findWorkspacePackSuffix [] = Nothing

resolveConfigPath :: FilePath -> FilePath -> FilePath
resolveConfigPath root target
  | isAbsolute target = normalise target
  | otherwise = normalise (root </> dropDotSlash target)

dropDotSlash :: FilePath -> FilePath
dropDotSlash path =
  case path of
    '.' : '/' : rest -> rest
    _ -> path

mergeLoadedWorlds :: [DeclarationSupportFile] -> [World] -> Either String World
mergeLoadedWorlds files loaded = do
  ambient <-
    mergeAmbientWorlds
      (zip (map declarationLoadPath files) (map worldAmbient loaded))
  pure World{worldAliases = concatMap worldAliases loaded, worldAmbient = ambient}

loadDeclarationFile :: DeclarationSupportFile -> IO (Either String World)
loadDeclarationFile file = do
  let path = declarationLoadPath file
  inputResult <- readTextFile path
  pure $ do
    input <- inputResult
    program <-
      firstError
        ("failed to load declaration file " <> path <> ": ")
        (parseText path input)
    case markedValue <$> programExpr program of
      Just _ ->
        Left
          ( withCode
              TD0007DeclarationOnlyCompile
              ("declaration files must not contain executable expressions: " <> path)
          )
      Nothing -> do
        _ <- validateProgramKinds (programAliases program) program
        _ <- validateProgramIndexedTypes program
        ambient <- collectAmbientWithBase path (declarationResolvePath file) program
        pure World{worldAliases = programAliases program, worldAmbient = ambient}

collectAmbient :: FilePath -> Program -> Either String (Map FilePath Scheme)
collectAmbient file = collectAmbientWithBase file file

collectAmbientWithBase ::
  FilePath -> FilePath -> Program -> Either String (Map FilePath Scheme)
collectAmbientWithBase file resolveBase program = do
  let duplicates =
        duplicateNames
          (map (resolvePath resolveBase . ambientPath) (programAmbient program))
  case duplicates of
    dup : _ ->
      Left
        ( withCode
            TD0002DuplicateAmbientDeclaration
            ("duplicate ambient declarations for target `" <> dup <> "` in " <> file)
        )
    [] -> Map.fromList <$> traverse toPair (programAmbient program)
  where
    toPair decl = do
      scheme <- schemeFromEntries (ambientEntries decl)
      pure (resolvePath resolveBase (ambientPath decl), scheme)
    schemeFromEntries entries =
      case duplicateNames (map ambientEntryName entries) of
        dup : _ ->
          Left
            ( withCode
                TD0003DuplicateAmbientEntry
                ("duplicate ambient entry `" <> Text.unpack dup <> "` in " <> file)
            )
        [] ->
          Right $
            case entries of
              [AmbientEntry "default" ty] -> schemeFromAnnotation ty
              _ ->
                Scheme
                  []
                  ( TRecord
                      ( Map.fromList
                          [(ambientEntryName entry, ambientEntryType entry) | entry <- entries]
                      )
                  )

findDeclarationFiles :: FilePath -> IO [FilePath]
findDeclarationFiles dir = do
  names <- sort <$> listDirectory dir
  fmap concat $
    forM names $ \name -> do
      let path = dir </> name
      isDir <- doesDirectoryExist path
      if isDir
        then findDeclarationFiles path
        else pure [normalise path | ".d.tynix" `isSuffixOf` name]

-- | Find the `.d.tynix` files a workspace contributes.
--
-- A real workspace (one with a marker such as `tynix.config.tynix`,
-- `flake.nix`, or `.git`) is searched to any depth; a bare directory only a
-- few levels deep, so running tynix in `$HOME` or a temp directory never walks
-- an unbounded tree. Hidden directories, build outputs, and symlinked
-- directories (e.g. Nix `result` links into the store) are always skipped.
findWorkspaceDeclarationFiles :: FilePath -> IO [FilePath]
findWorkspaceDeclarationFiles root = do
  marked <- hasWorkspaceMarker root
  go (if marked then maxBound else unmarkedSearchDepth) root
  where
    unmarkedSearchDepth = 4 :: Int
    go depth dir = do
      names <- sort <$> listDirectory dir
      fmap concat $
        forM names $ \name -> do
          let path = dir </> name
          isDir <- doesDirectoryExist path
          isLink <- pathIsSymbolicLink path
          if isDir
            then
              if depth <= 0 || isLink || ignoredDirectory name
                then pure []
                else do
                  nestedWorkspace <-
                    if normalise path == normalise root then pure False else hasWorkspaceMarker path
                  if nestedWorkspace
                    then pure []
                    else go (depth - 1) path
            else pure [normalise path | ".d.tynix" `isSuffixOf` name]
    ignoredDirectory name =
      take 1 name == "."
        || name `elem` ["node_modules", "dist-newstyle", "dist", "target", "result"]
        || take 7 name == "result-"

-- | Resolve an ambient declaration target.
--
-- Import resolution must agree exactly with the checker, so this delegates to
-- 'Check.resolvePath' rather than keeping a second copy of the same rules. The
-- one addition is the synthetic `builtins` target, which names the ambient
-- world rather than a file on disk.
resolvePath :: FilePath -> FilePath -> FilePath
resolvePath _ "builtins" = builtinsAmbientKey
resolvePath from target = Check.resolvePath from target

findSupportRoot :: FilePath -> IO FilePath
findSupportRoot path = go start
  where
    start = normalise (takeDirectory path)
    go dir = do
      marked <- hasWorkspaceMarker dir
      let parent = normalise (takeDirectory dir)
      if marked
        then pure dir
        else
          if parent == dir
            then pure start
            else go parent

hasWorkspaceMarker :: FilePath -> IO Bool
hasWorkspaceMarker dir =
  or
    <$> sequence
      [ doesFileExist (dir </> "flake.nix"),
        doesFileExist (dir </> "cabal.project"),
        doesFileExist (dir </> "pnpm-workspace.yaml"),
        doesFileExist (dir </> "tynix.config.tynix"),
        doesDirectoryExist (dir </> ".git")
      ]

mergeAmbientWorlds ::
  [(FilePath, Map FilePath Scheme)] -> Either String (Map FilePath Scheme)
mergeAmbientWorlds = fmap snd . foldM step (Map.empty, Map.empty)
  where
    step (sources, ambient) (file, additions) =
      foldM (insertOne file) (sources, ambient) (Map.toList additions)
    insertOne file (sources, ambient) (target, scheme) =
      case Map.lookup target sources of
        Just firstSource ->
          Left
            ( withCode
                TD0002DuplicateAmbientDeclaration
                ( "duplicate ambient declarations for target "
                    <> show target
                    <> " in "
                    <> firstSource
                    <> " and "
                    <> file
                )
            )
        Nothing ->
          Right (Map.insert target file sources, Map.insert target scheme ambient)

duplicateNames :: (Ord a) => [a] -> [a]
duplicateNames = foldr step [] . group . sort
  where
    step xs acc =
      case xs of
        first : _ | length xs > 1 -> first : acc
        _ -> acc

firstError :: String -> Either String a -> Either String a
firstError prefix = either (Left . (prefix <>)) Right

-- | Read a source file as UTF-8 regardless of the process locale (Nix build
-- sandboxes run with a C locale). Invalid bytes become U+FFFD instead of
-- aborting the read.
readTextFile :: FilePath -> IO (Either String Text)
readTextFile path = do
  result <- try @IOException (decodeUtf8Lenient <$> ByteString.readFile path)
  pure $
    case result of
      Left err ->
        Left
          ( withCode
              TD0001ReadFailed
              ("failed to read " <> path <> ": " <> displayException err)
          )
      Right input -> Right input
