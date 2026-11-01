{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Command-line entry point for tynix tooling.
--
-- The CLI intentionally stays small: it exposes the three capabilities that are
-- useful both for humans and for editor integration tests, namely compile,
-- check, and declaration emission.
module Main (main) where

import Cli qualified
import Control.Monad (unless)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Version (showVersion)
import GHC.IO.Encoding (setLocaleEncoding)
import Options.Applicative
import Paths_tynix_cli qualified as PackageInfo
import System.Environment (lookupEnv)
import System.Exit (die, exitFailure)
import System.IO (hIsTerminalDevice, hSetEncoding, stderr, stdout, utf8)
import System.Process (callProcess)

-- | Parse arguments and execute the requested command.
main :: IO ()
main = do
  -- Reports use ✓/✗ marks; do not let a C locale turn them into a crash.
  mapM_ (`hSetEncoding` utf8) [stdout, stderr]
  -- Source and config files are UTF-8 whatever the locale says.
  setLocaleEncoding utf8
  execParser opts >>= run
  where
    opts =
      info
        (Cli.commandParser <**> helper <**> versionOption)
        (fullDesc <> progDesc "Compile, check, emit, and scaffold tynix projects")
    versionOption =
      infoOption
        ("tynix " <> showVersion PackageInfo.version)
        (long "version" <> short 'v' <> help "Show the tynix version")

-- | Execute one CLI command.
run :: Cli.Command -> IO ()
run (Cli.Version format) =
  putPayload
    (Cli.renderVersion (Text.pack (showVersion PackageInfo.version)) format)
run (Cli.Lsp logFile) =
  callProcess "tynix-lsp" (Cli.lspCommandArgs logFile)
run (Cli.Doctor format) = do
  tty <- hIsTerminalDevice stdout
  noColor <- lookupEnv "NO_COLOR"
  let color = tty && maybe True null noColor
  (ok, report) <-
    Cli.runDoctor (Text.pack (showVersion PackageInfo.version)) color format
  putPayload report
  unless ok exitFailure
run cmd =
  Cli.executeCommand cmd >>= \case
    Left err ->
      case Cli.commandOutputFormat cmd of
        Just Cli.JsonFormat -> TextIO.putStrLn (Text.pack err) >> exitFailure
        _ -> die err
    Right content ->
      case Cli.commandOutputPath cmd of
        Just output -> Cli.writeOutput (Just output) content
        Nothing -> putPayload content

putPayload :: Text.Text -> IO ()
putPayload content =
  if "\n" `Text.isSuffixOf` content
    then TextIO.putStr content
    else TextIO.putStrLn content
