{-# LANGUAGE OverloadedStrings #-}

-- | Consistent rendering of names and types in checker diagnostics.
module Check.Diagnostics
  ( quoteName,
    quoteNames,
    showType,
    showRecord,
  )
where

import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Text qualified as T
import Pretty (renderType)
import Type

-- | Render a name inside backticks so diagnostics stay readable.
quoteName :: Name -> String
quoteName name = "`" <> T.unpack name <> "`"

-- | Render a list of names as a comma-separated, backtick-quoted sequence.
quoteNames :: [Name] -> String
quoteNames = intercalate ", " . map quoteName

-- | Render a type using the surface syntax produced by 'Pretty'.
showType :: Type -> String
showType = T.unpack . T.unwords . map T.strip . T.lines . renderType

-- | Render a record's field map as the equivalent record type.
showRecord :: Map Name Type -> String
showRecord = showType . TRecord
