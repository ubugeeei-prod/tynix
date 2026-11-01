{-# LANGUAGE OverloadedStrings #-}

-- | Type and documentation lookup that combines the core analysis with the
-- positional information recovered by 'SessionScan'.
--
-- The checker only reports types for root-level @let@ bindings, so local
-- names (lambda parameters, nested @let@s, pattern fields) are typed here from
-- what the source says about them: explicit annotations, the signature of the
-- function a parameter belongs to, ambient declarations for @import@ targets,
-- and — for the conventional @lib@ / @pkgs@ / @stdenv@ arguments — the
-- registry aliases when a project has loaded them.
module SessionResolve
  ( -- * Context
    Ctx (..),
    mkCtx,
    ctxAliases,

    -- * Types
    binderType,
    nameType,
    exprPathType,
    recordFields,
    paramDomain,
    functionParams,
    parseTypeText,
    conventionalAlias,

    -- * Literal structure
    binderLiteralFields,

    -- * Documentation
    binderDoc,
    AmbientDoc (..),
    ambientDocs,
    aliasDocs,
  )
where

import Check qualified
import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Driver (Analysis (..), lookupSymbolType, parseText)
import SessionScan
import Subtyping (lookupRecordField, resolveType)
import Syntax (Program (programAliases))
import Type
  ( Multiplicity (..),
    Scheme (..),
    Type (..),
    TypeAlias (..),
    tDynamic,
    tPath,
  )

-- | Everything needed to answer type questions about one document.
data Ctx = Ctx
  { ctxFile :: FilePath,
    ctxScan :: Scan,
    ctxAnalysis :: Maybe Analysis
  }

mkCtx :: FilePath -> Text -> Either String Analysis -> Ctx
mkCtx file content result =
  Ctx
    { ctxFile = file,
      ctxScan = scanDocument content,
      ctxAnalysis = either (const Nothing) Just result
    }

ctxAliases :: Ctx -> Map Text TypeAlias
ctxAliases ctx = maybe Map.empty analysisAliases (ctxAnalysis ctx)

-- | Parse a type annotation written in source.
--
-- The core parser exposes only whole-program entry points, so the type is
-- wrapped in a throwaway alias declaration. This keeps the LSP independent of
-- the parser's internal combinators.
parseTypeText :: Text -> Maybe Type
parseTypeText source
  | Text.null (Text.strip source) = Nothing
  | otherwise =
      case parseText "<annotation>" ("type TynixLspProbe__ = " <> source <> ";") of
        Right program -> case programAliases program of
          [alias] -> Just (typeAliasBody alias)
          _ -> Nothing
        Left _ -> Nothing

-- | Registry alias conventionally used for a well-known argument name.
conventionalAlias :: Text -> Maybe Text
conventionalAlias name =
  lookup
    name
    [("lib", "NixpkgsLib"), ("pkgs", "NixpkgsPkgs"), ("stdenv", "NixpkgsStdenv")]

-- | Best-effort type of a binder.
binderType :: Ctx -> Binder -> Maybe Type
binderType = binderTypeAt (8 :: Int)

binderTypeAt :: Int -> Ctx -> Binder -> Maybe Type
binderTypeAt fuel ctx b
  | fuel <= 0 = Nothing
  | otherwise =
      annotated
        <|> checked
        <|> fromValue
        <|> fromOwner
        <|> fromRootField
        <|> conventional
  where
    analysis = ctxAnalysis ctx
    scan = ctxScan ctx
    annotated = binderAnnotation b >>= parseTypeText
    checked
      | binderKind b `elem` [BindLet, BindInherit] && binderRootLevel b =
          schemeType <$> (analysis >>= Map.lookup (binderName b) . analysisBindings)
      | otherwise = Nothing
    fromValue = do
      (from, to) <- binderValue b
      valueType (fuel - 1) ctx from to
    fromOwner = do
      (owner, ix) <- binderOwner b
      fnTy <- ownerType (fuel - 1) ctx owner
      dom <- paramDomain (ctxAliases ctx) ix fnTy
      case binderKind b of
        BindPatternField -> lookupRecordField (ctxAliases ctx) dom (binderName b)
        _ -> Just dom
    fromRootField
      | binderKind b == BindRecField,
        Just root <- rootAttrsetOpener scan,
        binderScopeStart b == maybe (-1) tokStart (codeToken scan root) =
          do
            scheme <- analysis >>= analysisRoot
            lookupRecordField (ctxAliases ctx) (schemeType scheme) (binderName b)
      | otherwise = Nothing
    conventional
      | binderKind b `elem` [BindParam, BindPatternField] = do
          alias <- conventionalAlias (binderName b)
          if Map.member alias (ctxAliases ctx) then Just (TCon alias) else Nothing
      | otherwise = Nothing

ownerType :: Int -> Ctx -> Owner -> Maybe Type
ownerType fuel ctx owner = case owner of
  OwnerBinding tok -> binderAtToken (ctxScan ctx) tok >>= binderTypeAt fuel ctx
  OwnerRootField name -> do
    analysis <- ctxAnalysis ctx
    root <- analysisRoot analysis
    lookupRecordField (analysisAliases analysis) (schemeType root) name
  OwnerRoot -> schemeType <$> (ctxAnalysis ctx >>= analysisRoot)

-- | Type of a value expression spanning code tokens @[from, to)@, for the
-- simple shapes that matter for navigation: @import ./path@, a bare name, or
-- a dotted selection.
valueType :: Int -> Ctx -> Int -> Int -> Maybe Type
valueType fuel ctx from to
  | fuel <= 0 = Nothing
  | otherwise =
      case toks of
        [t1, t2]
          | tokText t1 == "import",
            tokKind t2 == KPath ->
              importType ctx (tokText t2)
        t1 : rest
          | tokKind t1 == KIdent,
            Just names <- selection rest ->
              exprPathTypeAt fuel ctx (tokStart t1) (tokText t1 : names)
        _ -> Nothing
  where
    scan = ctxScan ctx
    toks = mapMaybe (codeToken scan) [from .. to - 1]
    selection [] = Just []
    selection (dot : name : more)
      | tokText dot == "." && tokKind name == KIdent =
          (tokText name :) <$> selection more
    selection _ = Nothing

importType :: Ctx -> Text -> Maybe Type
importType ctx path = do
  analysis <- ctxAnalysis ctx
  let target = Check.resolvePath (ctxFile ctx) (Text.unpack path)
  scheme <- Map.lookup target (analysisAmbient analysis)
  pure (schemeType scheme)

-- | Type of a free name that is not bound locally.
nameType :: Ctx -> Text -> Maybe Type
nameType ctx name
  | name == "import" = Just (TFun Many tPath tDynamic)
  | otherwise = do
      analysis <- ctxAnalysis ctx
      if name == "builtins"
        then schemeType <$> Map.lookup "builtins" (analysisAmbient analysis)
        else schemeType <$> lookupSymbolType analysis name

-- | Type of @head.field1.field2…@ as seen from the given offset.
exprPathType :: Ctx -> Int -> [Text] -> Maybe Type
exprPathType = exprPathTypeAt 8

exprPathTypeAt :: Int -> Ctx -> Int -> [Text] -> Maybe Type
exprPathTypeAt _ _ _ [] = Nothing
exprPathTypeAt fuel ctx off (headName : rest) = do
  headTy <- case resolveNameAt (ctxScan ctx) headName off of
    Just b -> binderTypeAt fuel ctx b
    Nothing -> nameType ctx headName
  foldM (lookupRecordField (ctxAliases ctx)) headTy rest

-- | Fields reachable on a value of the given type (records and unions of
-- records).
recordFields :: Map Text TypeAlias -> Type -> [(Text, Type)]
recordFields aliases ty =
  case resolveType aliases ty of
    TRecord fields -> Map.toList fields
    TUnion members ->
      let names = nub (concatMap (map fst . recordFields aliases) members)
       in mapMaybe (\name -> (,) name <$> lookupRecordField aliases ty name) names
    _ -> []

-- | Domain of the @ix@-th curried parameter of a function type.
paramDomain :: Map Text TypeAlias -> Int -> Type -> Maybe Type
paramDomain aliases ix ty =
  case functionParams aliases ty of
    params | ix < length params -> Just (params !! ix)
    _ -> Nothing

-- | Parameter types of a (possibly quantified, possibly aliased) function.
functionParams :: Map Text TypeAlias -> Type -> [Type]
functionParams aliases = go (0 :: Int)
  where
    go depth ty
      | depth > 32 = []
      | otherwise = case resolveType aliases ty of
          TFun _ dom cod -> dom : go (depth + 1) cod
          _ -> []

-- | Field names written in a binder's attrset literal value, for completion
-- when no type is known.
binderLiteralFields :: Ctx -> Binder -> [Text]
binderLiteralFields ctx b = fromMaybe [] $ do
  (from, _) <- binderValue b
  let scan = ctxScan ctx
      opener = case codeToken scan from of
        Just t | tokText t == "rec" -> from + 1
        _ -> from
  t <- codeToken scan opener
  if tokText t == "{" then Just (nub (attrsetFieldNames scan opener)) else Nothing

-- | Documentation comment attached to a binder's declaration.
binderDoc :: Ctx -> Binder -> Maybe Text
binderDoc ctx b =
  let scan = ctxScan ctx
      anchor = case binderKind b of
        BindPatternField -> binderNameStart b
        BindParam -> binderNameStart b
        _ ->
          maybe (binderDeclStart b) (min (binderDeclStart b) . fst) (binderSignature b)
   in docCommentForOffset scan anchor

-- | One documented entry of a @declare@ block or a type alias.
data AmbientDoc = AmbientDoc
  { ambientDocTarget :: Text,
    ambientDocName :: Text,
    ambientDocType :: Text,
    ambientDocText :: Maybe Text,
    ambientDocOffset :: Int
  }
  deriving (Eq, Show)

-- | Every @declare "target" { name :: T; }@ entry in a document, with its
-- documentation comment.
ambientDocs :: Scan -> [AmbientDoc]
ambientDocs scan = concatMap declareAt [0 .. codeTokenCount scan - 1]
  where
    tok = codeToken scan
    textAt i = maybe "" tokText (tok i)
    declareAt i
      | textAt i == "declare",
        Just target <- tok (i + 1),
        tokKind target `elem` [KString, KPath],
        textAt (i + 2) == "{",
        Just c <- matchingCloser scan (i + 2) =
          entries (unquote (tokText target)) (i + 3) c
      | otherwise = []
    entries target from to = go from
      where
        go k
          | k >= to = []
          | textAt (k + 1) == "::",
            Just nameTok <- tok k =
              let e = stmtStop (k + 2)
                  tyText = sliceTokens (k + 2) e
               in AmbientDoc
                    { ambientDocTarget = target,
                      ambientDocName = tokText nameTok,
                      ambientDocType = tyText,
                      ambientDocText = docCommentForOffset scan (tokStart nameTok),
                      ambientDocOffset = tokStart nameTok
                    }
                    : go (e + 1)
          | otherwise = go (k + 1)
        stmtStop k
          | k >= to = to
          | textAt k `elem` ["{", "(", "["] =
              maybe to (stmtStop . (+ 1)) (matchingCloser scan k)
          | textAt k == ";" = k
          | otherwise = stmtStop (k + 1)
    sliceTokens a b = case (tok a, tok (b - 1)) of
      (Just s, Just e) ->
        Text.strip
          (Text.take (tokEnd e - tokStart s) (Text.drop (tokStart s) (scanContent scan)))
      _ -> ""
    unquote t = fromMaybe t (Text.stripPrefix "\"" t >>= Text.stripSuffix "\"")

-- | Documentation for top-level @type@ aliases, keyed by alias name.
aliasDocs :: Scan -> Map Text (Text, Int)
aliasDocs scan =
  Map.fromList
    [ (binderName b, (doc, binderNameStart b))
    | b <- scanBinders scan,
      binderKind b == BindTypeAlias,
      Just doc <- [docCommentForOffset scan (binderDeclStart b)]
    ]
