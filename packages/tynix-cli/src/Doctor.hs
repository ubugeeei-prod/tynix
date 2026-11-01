{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | `tynix doctor`: a quick health report of the toolchain and editor wiring.
module Doctor
  ( Check (..),
    CheckStatus (..),
    doctorChecks,
    doctorSucceeded,
    parseVersionLine,
    renderDoctor,
    renderDoctorJson,
  )
where

import Control.Monad (forM)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isDigit)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Ide
import Project (loadProject)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))

data CheckStatus
  = CheckOk
  | CheckWarn
  | CheckFail
  | CheckSkip
  deriving (Eq, Show)

data Check = Check
  { checkId :: Text,
    checkStatus :: CheckStatus,
    checkMessage :: Text,
    -- | A follow-up hint shown under failing or warning checks.
    checkHint :: Maybe Text
  }
  deriving (Eq, Show)

-- | Pull the version number out of a `tool --version` line such as
-- "tynix-lsp 0.5.0.0".
parseVersionLine :: Text -> Maybe Text
parseVersionLine output =
  case [word | word <- Text.words output, Just (c, _) <- [Text.uncons word], isDigit c] of
    version : _ -> Just version
    [] -> Nothing

-- | Run every check. @selfVersion@ is the version of the running `tynix`.
doctorChecks :: IdeEnv -> Text -> IO [Check]
doctorChecks env selfVersion = do
  tynixCheck <-
    checkTool
      "tynix"
      "tynix"
      ( Just
          "install it with `nix profile install \
          \github:ubugeeei-prod/tynix#tynix` so editors and scripts find the \
          \same version"
      )
      CheckWarn
  lspCheck <-
    checkTool
      "tynix-lsp"
      "tynix-lsp"
      ( Just
          "install it with `nix profile install github:ubugeeei-prod/tynix#tynix-lsp`"
      )
      CheckFail
  projectCheck <- checkProject
  editorChecks <- checkEditors
  nixCheck <- checkNix
  pure (concat [tynixCheck, lspCheck, [projectCheck], editorChecks, [nixCheck]])
  where
    run = ideRunProcess env
    -- A missing or mismatched `tynix` on PATH only warns (the running binary
    -- may come from `nix run`); a missing or mismatched `tynix-lsp` breaks
    -- every editor integration, so it fails.
    checkTool :: Text -> String -> Maybe Text -> CheckStatus -> IO [Check]
    checkTool name exe missingHint severity = do
      found <- ideFindExecutable env exe
      case found of
        Nothing ->
          pure [Check (name <> ".path") severity (name <> " is not on PATH") missingHint]
        Just path -> do
          (code, out, err) <- run path ["--version"]
          let reported = parseVersionLine (Text.pack out)
              located =
                Check (name <> ".path") CheckOk (name <> " found at " <> Text.pack path) Nothing
          pure $ case code of
            ExitFailure n ->
              [ located,
                Check
                  (name <> ".starts")
                  CheckFail
                  (Text.pack exe <> " --version exited with " <> Text.pack (show n))
                  (nonEmpty (Text.strip (Text.pack err)))
              ]
            ExitSuccess ->
              [ located,
                Check (name <> ".starts") CheckOk (Text.strip (Text.pack out)) Nothing,
                versionCheck name severity reported
              ]
    versionCheck name severity reported =
      case reported of
        Just version
          | version == selfVersion ->
              Check
                (name <> ".version")
                CheckOk
                (name <> " " <> version <> " matches tynix " <> selfVersion)
                Nothing
          | otherwise ->
              Check
                (name <> ".version")
                severity
                (name <> " " <> version <> " does not match tynix " <> selfVersion)
                ( Just
                    "upgrade both from the same release so the CLI and editors agree on diagnostics"
                )
        Nothing ->
          Check
            (name <> ".version")
            CheckWarn
            ("could not read the " <> name <> " version")
            Nothing
    checkProject = do
      found <- findUp (ideCwd env)
      case found of
        Nothing ->
          pure
            ( Check
                "project.config"
                CheckWarn
                "no tynix.config.tynix in this directory or its parents"
                (Just "run `tynix init` to create one")
            )
        Just root -> do
          loaded <- loadProject (Just root)
          pure $ case loaded of
            Left err ->
              Check
                "project.config"
                CheckFail
                ("tynix.config.tynix in " <> Text.pack root <> " is invalid")
                (Just (Text.pack err))
            Right _ ->
              Check
                "project.config"
                CheckOk
                ("project config " <> Text.pack (root </> "tynix.config.tynix"))
                Nothing
    findUp dir = do
      exists <- doesFileExist (dir </> "tynix.config.tynix")
      if exists
        then pure (Just dir)
        else
          let parent = takeDirectory dir
           in if parent == dir then pure Nothing else findUp parent
    checkEditors = do
      results <- forM allEditors $ \editor -> do
        detected <- detectEditor env editor
        case detected of
          Nothing -> pure Nothing
          Just evidence -> do
            status <- editorIntegrationStatus env editor
            let name = editorName editor
                key = "editor." <> name
                hint = Just ("run `tynix ide install " <> name <> "`")
            pure . Just $ case status of
              Just True -> Check key CheckOk (name <> ": tynix is set up (" <> evidence <> ")") Nothing
              Just False ->
                Check
                  key
                  CheckWarn
                  (name <> ": detected (" <> evidence <> ") but tynix is not set up")
                  hint
              Nothing ->
                Check
                  key
                  CheckWarn
                  ( name
                      <> ": detected ("
                      <> evidence
                      <> "); could not tell whether tynix is set up"
                  )
                  hint
      let found = catMaybes results
      pure $
        if null found
          then
            [ Check
                "editor"
                CheckSkip
                "no supported editor detected"
                (Just "see `tynix ide list`")
            ]
          else found
    checkNix = do
      found <- ideFindExecutable env "nix"
      case found of
        Nothing ->
          pure
            ( Check
                "nix"
                CheckWarn
                "nix is not on PATH"
                ( Just
                    "compiled .nix output needs Nix to evaluate; see https://nixos.org/download"
                )
            )
        Just path -> do
          (code, out, _) <- run path ["--version"]
          pure $
            if code == ExitSuccess
              then Check "nix" CheckOk (Text.strip (Text.pack out)) Nothing
              else
                Check
                  "nix"
                  CheckWarn
                  ("nix found at " <> Text.pack path <> " but `nix --version` failed")
                  Nothing
    nonEmpty t = if Text.null t then Nothing else Just t

-- | Doctor succeeds unless some check failed outright.
doctorSucceeded :: [Check] -> Bool
doctorSucceeded = all ((/= CheckFail) . checkStatus)

renderDoctor :: Bool -> [Check] -> Text
renderDoctor color checks =
  Text.unlines $
    concatMap line checks
      <> ["", summary]
  where
    line check =
      [symbol (checkStatus check) <> " " <> checkMessage check]
        <> [ paint dim ("  " <> hint)
           | checkStatus check /= CheckOk,
             Just hint <- [checkHint check]
           ]
    symbol = \case
      CheckOk -> paint green "✓"
      CheckWarn -> paint yellow "!"
      CheckFail -> paint red "✗"
      CheckSkip -> paint dim "-"
    count status = length (filter ((== status) . checkStatus) checks)
    summary =
      if doctorSucceeded checks
        then paint green "tynix doctor: no problems found" <> warnings
        else
          paint
            red
            ("tynix doctor: " <> Text.pack (show (count CheckFail)) <> " problem(s) found")
            <> warnings
    warnings =
      if count CheckWarn > 0
        then " (" <> Text.pack (show (count CheckWarn)) <> " warning(s))"
        else ""
    paint code text = if color then "\ESC[" <> code <> "m" <> text <> "\ESC[0m" else text
    green = "32"
    yellow = "33"
    red = "31"
    dim = "2"

renderDoctorJson :: Int -> [Check] -> Text
renderDoctorJson schemaVersion checks =
  TextEncoding.decodeUtf8 . LBS.toStrict . encode $
    object
      [ "schemaVersion" .= schemaVersion,
        "action" .= ("doctor" :: Text),
        "success" .= doctorSucceeded checks,
        "checks" .= map checkJson checks
      ]
  where
    checkJson :: Check -> Value
    checkJson check =
      object
        [ "id" .= checkId check,
          "status" .= statusText (checkStatus check),
          "message" .= checkMessage check,
          "hint" .= checkHint check
        ]
    statusText = \case
      CheckOk -> "ok" :: Text
      CheckWarn -> "warn"
      CheckFail -> "fail"
      CheckSkip -> "skip"
