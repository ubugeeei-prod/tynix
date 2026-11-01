{-# LANGUAGE OverloadedStrings #-}

-- | Context-aware completion for tynix.
--
-- The engine classifies the cursor position from the error-tolerant token
-- stream ('SessionScan') and then draws candidates from the best available
-- type information ('SessionResolve'):
--
-- * @expr.@ — fields of the expression's record type (or of an attrset
--   literal when no type is known), including @builtins.@ and members of
--   imported files typed by @declare@ blocks;
-- * type positions (after @::@, @as@, inside @type X = …@) — builtin type
--   constructors, aliases, and type parameters in scope;
-- * attribute-set keys and lambda patterns whose expected record type is
--   known (function arguments, signatures) — the missing fields;
-- * @./@ path literals — directory entries (resolved by the caller, since
--   that needs IO);
-- * everything else — names in scope (innermost first), top-level and
--   ambient names, keywords, and snippets.
--
-- Items carry @detail@ (the type), markdown documentation from comments and
-- declarations, a @sortText@ that encodes relevance, and a @textEdit@ that
-- replaces exactly the identifier being typed.
module SessionCompletion
  ( CompletionEnv (..),
    emptyCompletionEnv,
    CompletionItem (..),
    CompletionOutcome (..),
    completeAt,
    encodeCompletionItem,
    encodeCompletionList,
    pathCompletionItems,
    matchesPrefix,
    memberDoc,
    calleeBefore,
    valueKeywords,
    builtinTypeNames,
    typeKeywords,
    renderAlias,
    isFunction,
    compactType,
  )
where

import Check qualified
import Control.Applicative ((<|>))
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Char (toLower)
import Data.List (isSubsequenceOf, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Driver (Analysis (..))
import Pretty (renderType)
import SessionResolve
import SessionScan
import Subtyping (lookupRecordField, resolveType)
import Type (Scheme (..), Type (..), TypeAlias (..))

-- | Documentation gathered from declaration files around the document.
data CompletionEnv = CompletionEnv
  { -- | @builtins.<name>@ documentation
    envBuiltinDocs :: Map Text AmbientDoc,
    -- | resolved @declare@ target path → documented entries
    envAmbientDocs :: Map FilePath [AmbientDoc],
    -- | type alias documentation from declaration files
    envAliasDocs :: Map Text Text
  }

emptyCompletionEnv :: CompletionEnv
emptyCompletionEnv = CompletionEnv Map.empty Map.empty Map.empty

data CompletionItem = CompletionItem
  { itemLabel :: Text,
    itemKind :: Int,
    itemDetail :: Maybe Text,
    itemDoc :: Maybe Text,
    -- | relevance group; lower sorts first
    itemGroup :: Int,
    -- | snippet body (insertTextFormat 2) or plain insert text
    itemInsert :: Maybe Text,
    itemSnippet :: Bool,
    itemFilter :: Maybe Text,
    -- | retrigger completion after accepting (directories)
    itemRetrigger :: Bool
  }
  deriving (Eq, Show)

-- | Result of classifying the cursor.
data CompletionOutcome
  = -- | items plus the offset range they replace
    Items (Int, Int) [CompletionItem]
  | -- | complete entries of a directory: (directory relative to the
    -- document's folder, partial name, replaced range)
    PathEntries FilePath Text (Int, Int)
  deriving (Eq, Show)

item :: Text -> Int -> Int -> CompletionItem
item label kind grp =
  CompletionItem
    { itemLabel = label,
      itemKind = kind,
      itemDetail = Nothing,
      itemDoc = Nothing,
      itemGroup = grp,
      itemInsert = Nothing,
      itemSnippet = False,
      itemFilter = Nothing,
      itemRetrigger = False
    }

-- LSP CompletionItemKind values
kMethod, kFunction, kField, kVariable, kModule, kProperty, kKeyword, kSnippet, kFile, kFolder, kTypeParameter, kStruct, kClass :: Int
kMethod = 2
kFunction = 3
kField = 5
kVariable = 6
kClass = 7
kModule = 9
kProperty = 10
kKeyword = 14
kSnippet = 15
kFile = 17
kFolder = 19
kStruct = 22
kTypeParameter = 25

-- | Classify the cursor at a character offset and produce candidates.
completeAt :: CompletionEnv -> Ctx -> Int -> CompletionOutcome
completeAt env ctx off
  | Just (dir, partial, range) <- pathContext scan off = PathEntries dir partial range
  | insideCommentOrString scan off = Items (off, off) []
  | not (null dotPath) = Items wordRange (filterItems prefix (memberItems env ctx off dotPath))
  | typeContext scan wordStart = Items wordRange (filterItems prefix (typeItems env ctx off))
  | Just fieldItems <- attrKeyContext ctx wordStart = Items wordRange (filterItems prefix fieldItems)
  | otherwise = Items wordRange (filterItems prefix (expressionItems env ctx off wordStart))
  where
    scan = ctxScan ctx
    (dotPath, prefix) = dottedFragment scan off
    wordStart = off - Text.length prefix
    wordRange = (wordStart, off)

-- | The @a.b.@ chain and partial identifier right before the cursor.
dottedFragment :: Scan -> Int -> ([Text], Text)
dottedFragment scan off =
  let idx = scanLineIndex scan
      (lineNo, _) = offsetToPosition idx off
      lineStart = positionToOffset idx (lineNo, 0)
      before = Text.take (off - lineStart) (lineText idx lineNo)
      fragment = Text.takeWhileEnd (\c -> identCharC c || c == '.') before
      parts = Text.splitOn "." fragment
   in case reverse parts of
        [] -> ([], "")
        [partial] -> ([], partial)
        partial : revPath
          | any Text.null revPath -> ([], partial)
          | otherwise -> (reverse revPath, partial)
  where
    identCharC c = c == '_' || c == '\'' || c == '-' || c `elem` ['0' .. '9'] || c `elem` ['a' .. 'z'] || c `elem` ['A' .. 'Z']

-- | Path literal under the cursor, split into directory and partial name.
pathContext :: Scan -> Int -> Maybe (FilePath, Text, (Int, Int))
pathContext scan off = do
  i <- codeTokenIndexAt scan off
  t <- codeToken scan i
  if tokKind t == KPath && tokStart t < off && any (`Text.isPrefixOf` tokText t) ["./", "../", "/", "~/"]
    then
      let typed = Text.take (off - tokStart t) (tokText t)
          (dir, partial) = Text.breakOnEnd "/" typed
       in Just (Text.unpack dir, partial, (off - Text.length partial, off))
    else Nothing

-- | Turn directory entries into completion items. Directories get a trailing
-- slash and retrigger completion so users can keep drilling down.
pathCompletionItems :: Text -> [(FilePath, Bool)] -> [CompletionItem]
pathCompletionItems partial entries =
  [ (item label (if isDir then kFolder else kFile) (if isDir then 0 else 1))
      { itemInsert = Just label,
        itemRetrigger = isDir,
        itemDetail = Just (if isDir then "directory" else "file")
      }
  | (name, isDir) <- entries,
    let label = Text.pack name <> (if isDir then "/" else ""),
    not ("." `Text.isPrefixOf` Text.pack name) || "." `Text.isPrefixOf` partial,
    matchesPrefix partial label
  ]

-- Member access ---------------------------------------------------------------

memberItems :: CompletionEnv -> Ctx -> Int -> [Text] -> [CompletionItem]
memberItems env ctx off path =
  case exprPathType ctx off path of
    Just ty ->
      [ fieldItem name fieldTy (docFor name)
      | (name, fieldTy) <- recordFields aliases ty
      ]
    Nothing -> literalFallback
  where
    aliases = ctxAliases ctx
    scan = ctxScan ctx
    headBinder = case path of
      [h] -> resolveNameAt scan h off
      _ -> Nothing
    literalFallback = case headBinder of
      Just b -> [item name kField 2 | name <- binderLiteralFields ctx b]
      Nothing -> []
    docFor = memberDoc env ctx off path

-- | Documentation for @path.name@: @builtins@ members come from the nearest
-- @builtins.d.tynix@, members of @import@ed files from matching @declare@
-- blocks.
memberDoc :: CompletionEnv -> Ctx -> Int -> [Text] -> Text -> Maybe Text
memberDoc env ctx off path name
  | path == ["builtins"] = Map.lookup name (envBuiltinDocs env) >>= ambientDocText
  | otherwise = do
      [h] <- Just path
      b <- resolveNameAt scan h off
      (from, _) <- binderValue b
      importTok <- codeToken scan from
      pathTok <- codeToken scan (from + 1)
      if tokText importTok == "import" && tokKind pathTok == KPath
        then do
          let target = resolveImport (ctxFile ctx) (tokText pathTok)
          docs <- Map.lookup target (envAmbientDocs env)
          entry <- case filter ((== name) . ambientDocName) docs of
            d : _ -> Just d
            [] -> Nothing
          ambientDocText entry
        else Nothing
  where
    scan = ctxScan ctx

fieldItem :: Text -> Type -> Maybe Text -> CompletionItem
fieldItem name ty doc =
  (item name (if isFunction ty then kMethod else kField) 2)
    { itemDetail = Just (compactType ty),
      itemDoc = doc
    }

-- | One-line rendering for completion details and outlines.
compactType :: Type -> Text
compactType ty =
  let flat = Text.unwords (Text.words (renderType ty))
   in if Text.length flat > 120 then Text.take 117 flat <> "..." else flat

isFunction :: Type -> Bool
isFunction ty = case ty of
  TFun{} -> True
  TForall _ body -> isFunction body
  _ -> False

resolveImport :: FilePath -> Text -> FilePath
resolveImport file path = Check.resolvePath file (Text.unpack path)

-- Type positions --------------------------------------------------------------

-- | True when the cursor is inside a type annotation.
typeContext :: Scan -> Int -> Bool
typeContext scan wordStart =
  case codeTokenBefore scan wordStart of
    Nothing -> False
    Just prev
      | isTypeToken scan prev && textAt prev /= ";" -> True
      | otherwise -> walk prev (200 :: Int)
  where
    textAt i = maybe "" tokText (codeToken scan i)
    kindAt i = tokKind <$> codeToken scan i
    walk k fuel
      | k < 0 || fuel <= 0 = False
      | textAt k `elem` ["}", ")", "]"] && kindAt k == Just KSymbol =
          case matchingCloser scan k of
            Just o -> walk (o - 1) (fuel - 1)
            Nothing -> False
      | textAt k == "::" = True
      | textAt k == "as" && kindAt k == Just KIdent = True
      | textAt k == "=" = statementIsTypeAlias (k - 1) (50 :: Int)
      | textAt k `elem` [";", "{", "(", "[", ",", ":", "${", "?"] = False
      | kindAt k == Just KKeyword = False
      | otherwise = walk (k - 1) (fuel - 1)
    statementIsTypeAlias k fuel
      | k < 0 || fuel <= 0 = False
      | textAt k == "type" && kindAt k == Just KIdent && (k == 0 || textAt (k - 1) `elem` [";", "}"]) = True
      | textAt k `elem` [";", "{", "(", "=", ":"] || kindAt k == Just KKeyword = False
      | otherwise = statementIsTypeAlias (k - 1) (fuel - 1)

builtinTypeNames :: [(Text, Text)]
builtinTypeNames =
  [ ("Int", "Integers"),
    ("Float", "Floating-point numbers"),
    ("Number", "Int | Float"),
    ("Nat", "Non-negative integers"),
    ("String", "Strings"),
    ("Bool", "Booleans"),
    ("Path", "Nix path literals"),
    ("Null", "The null type"),
    ("List", "List a — homogeneous lists"),
    ("Vec", "Vec n a — lists of known length"),
    ("Matrix", "Matrix r c a — rectangular nested lists"),
    ("Tensor", "Tensor [d1 d2 …] a — n-dimensional nested lists"),
    ("Range", "Range lo hi T — bounded numbers"),
    ("Unit", "Unit u T — numbers tagged with a unit"),
    ("Tuple", "Tuple [a b …] — fixed-shape heterogeneous lists"),
    ("Get", "Get r k — the type of field k of record r"),
    ("KeyOf", "KeyOf r — the union of a record's field names"),
    ("Length", "Length xs — the length of an exact sequence"),
    ("Add", "Add a b — integer literal addition"),
    ("Sub", "Sub a b — integer literal subtraction"),
    ("Mul", "Mul a b — integer literal multiplication")
  ]

typeKeywords :: [(Text, Text)]
typeKeywords =
  [ ("dynamic", "Gradual escape hatch: unchecked in both directions"),
    ("any", "Unsound escape hatch: assignable to and from everything"),
    ("unknown", "Top type: must be narrowed before use"),
    ("forall", "Universal quantification: forall a. …"),
    ("infer", "Bind a type inside a conditional type isPattern"),
    ("extends", "Conditional type: A extends B ? C : D")
  ]

typeItems :: CompletionEnv -> Ctx -> Int -> [CompletionItem]
typeItems env ctx off =
  [ (item name kStruct 1){itemDetail = Just "builtin type", itemDoc = Just doc}
  | (name, doc) <- builtinTypeNames
  ]
    <> [ (item name kClass 0)
           { itemDetail = Just (renderAlias alias),
             itemDoc = Map.lookup name localAliasDocs <|> Map.lookup name (envAliasDocs env)
           }
       | (name, alias) <- Map.toList (ctxAliases ctx),
         name /= "TynixLspProbe__"
       ]
    <> [ (item (binderName b) kClass 0)
           { itemDetail = ("type " <>) . (binderName b <>) . (" = " <>) <$> binderAnnotation b,
             itemDoc = binderDoc ctx b
           }
       | b <- scanBinders scan,
         binderKind b == BindTypeAlias,
         not (Map.member (binderName b) (ctxAliases ctx))
       ]
    <> [ (item (binderName b) kTypeParameter 0){itemDetail = Just "type parameter"}
       | b <- scanBinders scan,
         binderKind b == BindTypeParam,
         binderScopeStart b <= off,
         off <= binderScopeEnd b
       ]
    <> [(item name kKeyword 3){itemDoc = Just doc} | (name, doc) <- typeKeywords]
  where
    scan = ctxScan ctx
    localAliasDocs = Map.map fst (aliasDocs scan)

renderAlias :: TypeAlias -> Text
renderAlias alias =
  "type "
    <> Text.unwords (typeAliasName alias : typeAliasParams alias)
    <> " = "
    <> renderType (typeAliasBody alias)

-- Attribute keys / lambda patterns ---------------------------------------------

-- | Field candidates when the cursor is at a key position of an attrset or
-- lambda isPattern whose expected record type is known.
attrKeyContext :: Ctx -> Int -> Maybe [CompletionItem]
attrKeyContext ctx wordStart = do
  opener : _ <- Just (enclosingOpeners scan wordStart)
  openTok <- codeToken scan opener
  if tokText openTok /= "{" then Nothing else Just ()
  prev <- codeTokenBefore scan wordStart
  let prevText = maybe "" tokText (codeToken scan prev)
  if prev == opener || (prevText `elem` [";", ","] && prev > opener) then Just () else Nothing
  let isPattern = isPatternGroup scan opener
  expected <- expectedRecord ctx opener (6 :: Int)
  let present = Set.fromList (attrsetFieldNames scan opener)
      fields =
        [ (fieldItem name fieldTy Nothing)
            { itemKind = kProperty,
              itemGroup = 0,
              itemInsert = Just (if isPattern then name else name <> " = $1;"),
              itemSnippet = not isPattern
            }
        | (name, fieldTy) <- recordFields (ctxAliases ctx) expected,
          Set.notMember name present
        ]
  pure $
    fields
      <> [ (item "..." kKeyword 4){itemDetail = Just "accept additional attributes"}
         | isPattern,
           "..." `notElem` map tokText (mapMaybe (codeToken scan) [opener + 1 .. fromMaybe opener (matchingCloser scan opener)])
         ]
      <> [(item "inherit" kKeyword 4){itemInsert = Just "inherit $1;", itemSnippet = True} | not isPattern]
  where
    scan = ctxScan ctx

-- | Whether the brace group at @i@ is (or is becoming) a lambda isPattern.
isPatternGroup :: Scan -> Int -> Bool
isPatternGroup scan i =
  case matchingCloser scan i of
    Just c ->
      let after = maybe "" tokText (codeToken scan (c + 1))
       in after == ":" || after == "@" || hasComma c
    Nothing -> hasComma (codeTokenCount scan)
  where
    hasComma c = any ((== ",") . tokText) (mapMaybe (codeToken scan) [i + 1 .. c - 1])

-- | The record type an attrset / isPattern opened at @i@ is expected to have.
expectedRecord :: Ctx -> Int -> Int -> Maybe Type
expectedRecord ctx i fuel
  | fuel <= 0 = Nothing
  | otherwise =
      asRecord =<< (fromBinding <|> fromArgument <|> fromRoot)
  where
    scan = ctxScan ctx
    aliases = ctxAliases ctx
    textAt k = maybe "" tokText (codeToken scan k)
    kindAt k = tokKind <$> codeToken scan k
    isPattern = isPatternGroup scan i
    asRecord ty =
      let target = if isPattern then paramDomain aliases 0 ty else Just ty
       in case resolveType aliases <$> target of
            Just r@(TRecord _) -> Just r
            Just u@(TUnion _) | not (null (recordFields aliases u)) -> Just u
            _ -> Nothing
    -- name = { … }   (let binding, rec field, or nested attrset field)
    fromBinding
      | textAt (i - 1) == "=" && kindAt (i - 2) == Just KIdent = do
          let nameIx = i - 2
              name = textAt nameIx
          case binderAtToken scan nameIx of
            Just b -> binderType ctx b
            Nothing ->
              case enclosingOpeners scan (maybe 0 tokStart (codeToken scan nameIx)) of
                outer : _
                  | textAt outer == "{" ->
                      if rootAttrsetOpener scan == Just outer
                        then rootField name
                        else do
                          outerTy <- expectedRecord ctx outer (fuel - 1)
                          lookupRecordField aliases outerTy name
                _ -> Nothing
      | otherwise = Nothing
    rootField name = do
      analysis <- ctxAnalysis ctx
      root <- analysisRoot analysis
      lookupRecordField aliases (schemeType root) name
    -- f a { … }
    fromArgument = do
      (calleePath, calleeStart, argIx) <- calleeBefore scan i
      fnTy <- exprPathType ctx calleeStart calleePath
      dom <- paramDomain aliases argIx fnTy
      if isPattern then Nothing else Just dom
    -- the root lambda isPattern of the file
    fromRoot
      | scanRootStart scan == Just i = schemeType <$> (ctxAnalysis ctx >>= analysisRoot)
      | otherwise = Nothing

-- | Find the function being applied to the argument starting at token @i@:
-- returns the callee's name path, its start offset, and the argument index.
calleeBefore :: Scan -> Int -> Maybe ([Text], Int, Int)
calleeBefore scan i = go (i - 1) (0 :: Int) (12 :: Int)
  where
    textAt k = maybe "" tokText (codeToken scan k)
    kindAt k = tokKind <$> codeToken scan k
    go k args fuel
      | k < 0 || fuel <= 0 = Nothing
      | kindAt k == Just KSymbol && textAt k `elem` ["}", ")", "]"] =
          case matchingCloser scan k of
            Just o -> go (o - 1) (args + 1) (fuel - 1)
            Nothing -> Nothing
      | kindAt k `elem` map Just [KString, KNumber, KPath] = go (k - 1) (args + 1) (fuel - 1)
      | kindAt k == Just KIdent =
          let (path, start) = selectPath k
           in case codeToken scan start of
                Just t
                  | args == 0 || True ->
                      -- the left-most atom of an application is the callee
                      case go (start - 1) (args + 1) (fuel - 1) of
                        Just found -> Just found
                        Nothing -> Just (path, tokStart t, args)
                _ -> Nothing
      | otherwise = Nothing
    selectPath k
      | textAt (k - 1) == "." && kindAt (k - 2) == Just KIdent =
          let (path, start) = selectPath (k - 2)
           in (path <> [textAt k], start)
      | otherwise = ([textAt k], k)

-- Expression positions ---------------------------------------------------------

expressionItems :: CompletionEnv -> Ctx -> Int -> Int -> [CompletionItem]
expressionItems _env ctx off wordStart =
  localItems
    <> topItems
    <> ambientItems
    <> keywordItems
    <> snippetItems ctx wordStart
  where
    scan = ctxScan ctx
    analysis = ctxAnalysis ctx
    typedNow = maybe "" tokText (codeTokenIndexAt scan wordStart >>= codeToken scan)
    locals =
      [ b
      | b <- bindersInScopeAt scan off,
        not (binderNameStart b == wordStart && binderName b == typedNow)
      ]
    localNames = Set.fromList (map binderName locals)
    localItems =
      [ let ty = binderType ctx b
         in (item (binderName b) (maybe kVariable (\t -> if isFunction t then kFunction else kVariable) ty) 0)
              { itemDetail = Just (maybe (kindLabel b) compactType ty),
                itemDoc = binderDoc ctx b,
                itemFilter = Nothing
              }
      | b <- locals
      ]
    rootFields = case analysis >>= analysisRoot of
      Just scheme -> recordFields (ctxAliases ctx) (schemeType scheme)
      Nothing -> []
    topItems =
      [ (item name (if isFunction (schemeType scheme) then kFunction else kVariable) 1)
          { itemDetail = Just (compactType (schemeType scheme))
          }
      | Just a <- [analysis],
        (name, scheme) <- Map.toList (analysisBindings a),
        Set.notMember name localNames
      ]
        <> [ (item name kField 2){itemDetail = Just (compactType ty)}
           | (name, ty) <- rootFields,
             Set.notMember name localNames,
             not (maybe False (Map.member name . analysisBindings) analysis)
           ]
        <> [ (item "default" kVariable 2){itemDetail = Just (compactType (schemeType scheme)), itemDoc = Just "Type of this file's root expression."}
           | Just scheme <- [analysis >>= analysisRoot]
           ]
    ambientItems =
      [ (item "builtins" kModule 3)
          { itemDetail = Just "module",
            itemDoc = Just "The Nix `builtins` attribute set."
          }
      | Set.notMember "builtins" localNames
      ]
        <> [ (item "import" kFunction 3)
               { itemDetail = Just "Path -> dynamic",
                 itemDoc = Just "Load and evaluate a Nix file. Typed by matching `declare` blocks when available."
               }
           | Set.notMember "import" localNames
           ]
    keywordItems =
      [ (item kw kKeyword 5){itemDoc = Just doc}
      | (kw, doc) <- valueKeywords
      ]
    kindLabel b = case binderKind b of
      BindParam -> "parameter"
      BindPatternField -> "isPattern field"
      BindPatternAlias -> "argument set"
      BindInherit -> "inherited"
      BindRecField -> "rec field"
      _ -> "let binding"

valueKeywords :: [(Text, Text)]
valueKeywords =
  [ ("let", "Local bindings: `let x = …; in body`"),
    ("in", "Ends a `let` block"),
    ("if", "Conditional: `if c then a else b`"),
    ("then", "Branch of an `if`"),
    ("else", "Branch of an `if`"),
    ("with", "Bring an attrset's fields into scope: `with e; body`"),
    ("inherit", "Copy names into an attrset / let: `inherit x;` or `inherit (e) x;`"),
    ("rec", "Recursive attrset whose fields can refer to each other"),
    ("assert", "`assert cond; body` — evaluation fails when cond is false"),
    ("or", "Default for a missing attribute: `a.b or fallback`"),
    ("true", "Boolean literal"),
    ("false", "Boolean literal"),
    ("null", "The null value")
  ]

snippetItems :: Ctx -> Int -> [CompletionItem]
snippetItems ctx wordStart =
  [ snippet "let … in" "let" "let\n  ${1:name} = ${2:value};\nin\n${0:body}" "Local bindings",
    snippet "if … then … else" "if" "if ${1:condition} then ${2:yes} else ${0:no}" "Conditional expression",
    snippet "{ … }: lambda" "{" "{ ${1:lib}, ${2:pkgs}, ... }:\n$0" "Function taking an attribute set",
    snippet "with … ;" "with" "with ${1:pkgs}; ${0}" "Bring fields into scope"
  ]
    <> [ snippet
           "package skeleton"
           "package"
           "{ lib, stdenv, fetchurl, ... }:\n\nstdenv.mkDerivation {\n  pname = \"${1:name}\";\n  version = \"${2:0.1.0}\";\n\n  src = ${3:./.};\n\n  meta = {\n    description = \"${4:description}\";\n    license = lib.licenses.${5:mit};\n  };\n}\n"
           "`{ ... }:` package function calling `stdenv.mkDerivation`"
       | atTopLevel
       ]
    <> [ snippet "declare block" "declare" "declare \"${1:./file.nix}\" {\n  ${2:name} :: ${3:Type};\n};\n$0" "Describe the types of an existing .nix file"
       | atTopLevel
       ]
    <> [ snippet "type alias" "type" "type ${1:Name} = ${0:Type};" "Declare a type alias"
       | atTopLevel
       ]
    <> [ snippet "opaque type" "opaque" "opaque type ${1:Name} = ${0:String};" "Declare a nominal type"
       | atTopLevel
       ]
    <> [ snippet "macro" "macro" "macro ${1:name} {\n  (\\$${2:x}:expr) => (${0:\\$${2:x}});\n};\n" "Declare a hygienic macro"
       | atTopLevel
       ]
    <> [ snippet "signature + binding" "sig" "${1:name} :: ${2:Type};\n${1:name} = ${0:value};" "Typed let binding"
       | inLetBindings
       ]
  where
    scan = ctxScan ctx
    snippet label filterText body doc =
      (item label kSnippet 6)
        { itemInsert = Just body,
          itemSnippet = True,
          itemFilter = Just filterText,
          itemDetail = Just "snippet",
          itemDoc = Just doc
        }
    atTopLevel = case scanRootStart scan of
      Nothing -> True
      Just r -> maybe True ((wordStart <=) . tokStart) (codeToken scan r)
    inLetBindings = case codeTokenBefore scan wordStart >>= codeToken scan of
      Just t -> tokText t `elem` ["let", ";"]
      Nothing -> False

-- Filtering / encoding -----------------------------------------------------------

-- | Case-insensitive subsequence match; an empty prefix matches everything.
matchesPrefix :: Text -> Text -> Bool
matchesPrefix prefix label =
  Text.null prefix || Text.unpack (Text.toLower prefix) `isSubsequenceOf` Text.unpack (Text.toLower label)

filterItems :: Text -> [CompletionItem] -> [CompletionItem]
filterItems prefix items =
  [ it
  | it <- dedupe items,
    matchesPrefix prefix (fromMaybe (itemLabel it) (itemFilter it))
  ]
  where
    dedupe = go Set.empty
    go _ [] = []
    go seen (x : xs)
      | Set.member (itemLabel x, itemKind x == kSnippet) seen = go seen xs
      | otherwise = x : go (Set.insert (itemLabel x, itemKind x == kSnippet) seen) xs

-- | Encode one item. @range@ is the replaced range in LSP coordinates.
encodeCompletionItem :: Text -> ((Int, Int), (Int, Int)) -> Bool -> [Pair] -> Int -> CompletionItem -> Value
encodeCompletionItem prefix ((sl, sc), (el, ec)) includeDocs extraData ix it =
  object $
    [ "label" .= itemLabel it,
      "kind" .= itemKind it,
      "sortText" .= sortKey,
      "textEdit"
        .= object
          [ "range"
              .= object
                [ "start" .= object ["line" .= sl, "character" .= sc],
                  "end" .= object ["line" .= el, "character" .= ec]
                ],
            "newText" .= fromMaybe (itemLabel it) (itemInsert it)
          ],
      "insertTextFormat" .= (if itemSnippet it then 2 else 1 :: Int)
    ]
      <> catMaybes
        [ ("detail" .=) <$> itemDetail it,
          (\d -> "labelDetails" .= object ["description" .= shorten d]) <$> (itemDetail it >>= \d -> if itemKind it == kSnippet then Nothing else Just d),
          ("filterText" .=) <$> itemFilter it,
          if includeDocs then (\d -> "documentation" .= object ["kind" .= ("markdown" :: Text), "value" .= d]) <$> itemDoc it else Nothing,
          if itemRetrigger it then Just ("command" .= object ["title" .= ("Suggest" :: Text), "command" .= ("editor.action.triggerSuggest" :: Text)]) else Nothing,
          if null extraData then Nothing else Just ("data" .= object (extraData <> ["label" .= itemLabel it]))
        ]
  where
    exact = Text.toLower prefix `Text.isPrefixOf` Text.toLower (fromMaybe (itemLabel it) (itemFilter it))
    sortKey =
      Text.pack (show (itemGroup it))
        <> (if exact then "0" else "1")
        <> Text.justifyRight 5 '0' (Text.pack (show ix))
    shorten d = if Text.length d > 60 then Text.take 57 d <> "..." else d

-- | Encode a whole completion list, sorted by relevance.
encodeCompletionList :: Text -> ((Int, Int), (Int, Int)) -> [Pair] -> [CompletionItem] -> Value
encodeCompletionList prefix range extraData items =
  let ordered = sortOn (\(ix, it) -> (itemGroup it, not (exactPrefix it), ix)) (zip [0 :: Int ..] (orderWithinGroups items))
      includeDocs = length items <= 150
   in object
        [ "isIncomplete" .= False,
          "items" .= zipWith (encodeCompletionItem prefix range includeDocs extraData) [0 ..] (map snd ordered)
        ]
  where
    exactPrefix it = Text.toLower prefix `Text.isPrefixOf` Text.toLower (fromMaybe (itemLabel it) (itemFilter it))
    -- locals keep their scope order; every other group is alphabetical
    orderWithinGroups xs =
      let (locals, others) = span' ((== 0) . itemGroup) xs
       in locals <> sortOn (\it -> (itemGroup it, Text.map toLower (itemLabel it), itemLabel it)) others
    span' p xs = (filter p xs, filter (not . p) xs)
