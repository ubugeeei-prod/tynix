{-# LANGUAGE OverloadedStrings #-}

-- | Declaration-file emitter.
--
-- Emission is intentionally structural: attribute-set roots become named
-- members, while everything else is exposed as `default`. This mirrors how
-- ambient declarations describe existing `.nix` modules.
module Emit
  ( emitDeclarationFile,
    emitDeclarationFileFor,
  )
where

import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Pretty (renderDeclarationFile)
import Syntax
import System.FilePath (isRelative, joinPath, normalise, replaceExtension, splitDirectories, takeDirectory)
import Type

-- | Emit a `.d.tynix`-style declaration text for a source file.
--
-- Local aliases are preserved verbatim so downstream files can reuse the same
-- abstractions rather than only seeing fully-expanded record shapes.
emitDeclarationFile :: FilePath -> Program -> Scheme -> Text
emitDeclarationFile source =
  emitDeclarationFileFor (replaceExtension source "nix") (replaceExtension source "d.tynix")

emitDeclarationFileFor :: FilePath -> FilePath -> Program -> Scheme -> Text
emitDeclarationFileFor runtimeTarget declarationPath program scheme =
  renderDeclarationFile (declareTarget runtimeTarget declarationPath) (programAliases program) entries
  where
    entries =
      case (stripCasts . markedValue <$> programExpr program, schemeType scheme, schemeVars scheme) of
        (Just (EAttrSet _), TRecord fields, []) -> Map.toList fields
        (Just (ERec _), TRecord fields, []) -> Map.toList fields
        _ -> [("default", quantified)]
    quantified =
      case schemeVars scheme of
        [] -> schemeType scheme
        _ -> TForall (schemeVars scheme) (schemeType scheme)

declareTarget :: FilePath -> FilePath -> FilePath
declareTarget runtimeTarget declarationPath =
  let relativePath = relativeBetween (normalise (takeDirectory declarationPath)) (normalise runtimeTarget)
   in if isRelative relativePath && not ("./" `isPrefixOf` relativePath || "../" `isPrefixOf` relativePath)
        then "./" <> relativePath
        else relativePath

relativeBetween :: FilePath -> FilePath -> FilePath
relativeBetween base target =
  let baseParts = splitDirectories base
      targetParts = splitDirectories target
      shared = sharedPrefixLength baseParts targetParts
   in if shared == 0
        then target
        else
          let ups = replicate (length baseParts - shared) ".."
              downs = drop shared targetParts
           in case ups <> downs of
                [] -> "."
                parts -> joinPath parts

sharedPrefixLength :: (Eq a) => [a] -> [a] -> Int
sharedPrefixLength left right =
  length (takeWhile id (zipWith (==) left right))

stripCasts :: Expr -> Expr
stripCasts (ECast expr _) = stripCasts expr
stripCasts (EAscribe expr _) = stripCasts expr
stripCasts expr = expr
