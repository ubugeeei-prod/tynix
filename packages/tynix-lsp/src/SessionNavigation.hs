{-# LANGUAGE OverloadedStrings #-}

-- | Scope-aware navigation: local definitions, references, rename, selection
-- ranges, and hierarchical document symbols.
--
-- Everything here works from the scanner, so it keeps answering while the
-- buffer has type or syntax errors, and it understands shadowing: renaming a
-- lambda parameter never touches an unrelated binding with the same name, and
-- field selections (@x.name@) or attribute keys are never mistaken for uses of
-- a variable called @name@.
module SessionNavigation
  ( -- * Local symbols
    LocalSymbol (..),
    localSymbolAt,
    localOccurrences,
    renameableAt,

    -- * Selection ranges
    selectionRangesAt,

    -- * Document symbols
    SymbolNode (..),
    documentSymbolTree,
    encodeSymbolTree,
  )
where

import Data.Aeson (Value, object, (.=))
import Data.Bifunctor (second)
import Data.List (nub, sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import SessionCompletion (compactType, isFunction)
import SessionResolve
import SessionScan

-- | The symbol under the cursor, resolved to its declaration.
data LocalSymbol
  = -- | a value binder (let, parameter, pattern field, …)
    LocalValue Binder
  | -- | a type alias or type parameter
    LocalType Binder
  deriving (Eq, Show)

-- | Resolve the identifier at an offset to a binder in this document.
localSymbolAt :: Scan -> Int -> Maybe LocalSymbol
localSymbolAt scan off = do
  i <- codeTokenIndexAt scan off
  t <- codeToken scan i
  if tokKind t /= KIdent then Nothing else Just ()
  case binderAtToken scan i of
    Just b
      | isValueBinder b -> Just (LocalValue b)
      | otherwise -> Just (LocalType b)
    Nothing
      | isTypeToken scan i -> LocalType <$> typeBinderFor (tokText t) (tokStart t)
      | isReferenceToken scan i ->
          LocalValue <$> resolveNameAt scan (tokText t) (tokStart t)
      | otherwise -> Nothing
  where
    typeBinderFor name at' =
      case sortOn
        (negate . binderScopeStart)
        [ b
        | b <- scanBinders scan,
          not (isValueBinder b),
          binderName b == name,
          binderScopeStart b <= at',
          at' <= binderScopeEnd b
        ] of
        b : _ -> Just b
        [] -> Nothing

-- | Every occurrence (declaration, signature, references) of a local symbol
-- as offset ranges, sorted.
localOccurrences :: Scan -> LocalSymbol -> [(Int, Int)]
localOccurrences scan sym = case sym of
  LocalValue b ->
    nub . sortOn fst $
      (binderNameStart b, binderNameEnd b)
        : signatureName b
          <> [ (tokStart t, tokEnd t)
             | i <- binderReferences scan b,
               Just t <- [codeToken scan i]
             ]
  LocalType b ->
    nub . sortOn fst $
      [ (tokStart t, tokEnd t)
      | i <- [0 .. codeTokenCount scan - 1],
        isTypeToken scan i || i == binderToken b,
        Just t <- [codeToken scan i],
        tokKind t == KIdent,
        tokText t == binderName b,
        binderScopeStart b <= tokStart t,
        tokStart t <= binderScopeEnd b
      ]
  where
    signatureName b = case binderSignature b of
      Just (s, _) -> case codeTokenIndexAt scan s >>= codeToken scan of
        Just t | tokText t == binderName b -> [(tokStart t, tokEnd t)]
        _ -> []
      Nothing -> []

-- | Range and current name for @textDocument/prepareRename@.
renameableAt :: Scan -> Int -> Maybe ((Int, Int), Text)
renameableAt scan off = do
  i <- codeTokenIndexAt scan off
  t <- codeToken scan i
  if tokKind t == KIdent
    && tokText t `notElem` ["true", "false", "null", "builtins", "import"]
    then Just ((tokStart t, tokEnd t), tokText t)
    else Nothing

-- | Nested ranges around an offset, innermost first: the token, enclosing
-- bracket contents and groups, the binding statement, and the document.
selectionRangesAt :: Scan -> Int -> [(Int, Int)]
selectionRangesAt scan off = chain (sortOn size (nub candidates))
  where
    content = scanContent scan
    whole = (0, Text.length content)
    size (s, e) = e - s
    tokenRange = case codeTokenIndexAt scan off >>= codeToken scan of
      Just t -> [(tokStart t, tokEnd t)]
      Nothing -> []
    groups =
      concat
        [ catMaybes
            [ do
                o <- codeToken scan opener
                c <- matchingCloser scan opener >>= codeToken scan
                if tokEnd o < tokStart c then Just (tokEnd o, tokStart c) else Nothing,
              do
                o <- codeToken scan opener
                c <- matchingCloser scan opener >>= codeToken scan
                Just (tokStart o, tokEnd c)
            ]
        | opener <- enclosingOpeners scan off
        ]
    declarations =
      [ (binderDeclStart b, binderDeclEnd b)
      | b <- scanBinders scan,
        binderKind b `elem` [BindLet, BindRecField, BindTypeAlias],
        binderDeclStart b <= off,
        off <= binderDeclEnd b
      ]
        <> [ (binderScopeStart b, binderScopeEnd b)
           | b <- scanBinders scan,
             binderKind b `elem` [BindLet, BindParam],
             binderScopeStart b <= off,
             off <= binderScopeEnd b
           ]
    candidates =
      filter
        (\(s, e) -> s <= off && off <= e)
        (tokenRange <> groups <> declarations <> [whole])
    chain [] = []
    chain (r : rs) = r : chain (filter (\x -> contains x r && x /= r) rs)
    contains (s1, e1) (s2, e2) = s1 <= s2 && e2 <= e1

-- Document symbols -------------------------------------------------------------

data SymbolNode = SymbolNode
  { symbolName :: Text,
    symbolDetail :: Maybe Text,
    symbolKind :: Int,
    symbolRange :: (Int, Int),
    symbolSelection :: (Int, Int),
    symbolChildren :: [SymbolNode]
  }
  deriving (Eq, Show)

-- | Hierarchical outline: type aliases, @declare@ blocks with their entries,
-- @let@ bindings, and attribute-set fields nested by containment.
documentSymbolTree :: Ctx -> [SymbolNode]
documentSymbolTree ctx = nest (sortOn (second negate . symbolRange) flat)
  where
    scan = ctxScan ctx
    flat = aliases <> declares <> lets <> fields
    aliases =
      [ SymbolNode
          { symbolName = binderName b,
            symbolDetail = binderAnnotation b,
            symbolKind = 11,
            symbolRange = (binderDeclStart b, binderDeclEnd b),
            symbolSelection = (binderNameStart b, binderNameEnd b),
            symbolChildren = []
          }
      | b <- scanBinders scan,
        binderKind b == BindTypeAlias
      ]
    declares = mapMaybe declareNode [0 .. codeTokenCount scan - 1]
    declareNode i = do
      kw <- codeToken scan i
      target <- codeToken scan (i + 1)
      brace <- codeToken scan (i + 2)
      if tokText kw == "declare"
        && tokKind target `elem` [KString, KPath]
        && tokText brace == "{"
        then Just ()
        else Nothing
      c <- matchingCloser scan (i + 2)
      closeTok <- codeToken scan c
      let entries =
            [ SymbolNode
                { symbolName = ambientDocName d,
                  symbolDetail = Just (ambientDocType d),
                  symbolKind = if "->" `Text.isInfixOf` ambientDocType d then 12 else 13,
                  symbolRange =
                    ( ambientDocOffset d,
                      ambientDocOffset d
                        + Text.length (ambientDocName d)
                        + 4
                        + Text.length (ambientDocType d)
                    ),
                  symbolSelection =
                    (ambientDocOffset d, ambientDocOffset d + Text.length (ambientDocName d)),
                  symbolChildren = []
                }
            | d <- ambientDocs scan,
              ambientDocOffset d > tokStart brace,
              ambientDocOffset d < tokEnd closeTok
            ]
      pure
        SymbolNode
          { symbolName = "declare " <> tokText target,
            symbolDetail = Nothing,
            symbolKind = 2,
            symbolRange = (tokStart kw, tokEnd closeTok),
            symbolSelection = (tokStart target, tokEnd target),
            symbolChildren = entries
          }
    lets =
      [ SymbolNode
          { symbolName = binderName b,
            symbolDetail = compactType <$> ty,
            symbolKind = if maybe (startsLambda b) isFunction ty then 12 else 13,
            symbolRange = (binderDeclStart b, binderDeclEnd b),
            symbolSelection = (binderNameStart b, binderNameEnd b),
            symbolChildren = []
          }
      | b <- scanBinders scan,
        binderKind b `elem` [BindLet, BindRecField],
        let ty = binderType ctx b
      ]
    startsLambda b = case binderValue b of
      Just (from, _) -> maybe False ((== ":") . tokText) (codeToken scan (from + 1))
      Nothing -> False
    letNames =
      [ binderNameStart b
      | b <- scanBinders scan,
        binderKind b `elem` [BindLet, BindRecField]
      ]
    fields =
      [ SymbolNode
          { symbolName = tokText t,
            symbolDetail = Nothing,
            symbolKind = 8,
            symbolRange = (tokStart t, endOf e),
            symbolSelection = (tokStart t, tokEnd t),
            symbolChildren = []
          }
      | (k, e) <- attrFieldStatements scan,
        Just t <- [codeToken scan k],
        tokStart t `notElem` letNames
      ]
    endOf e = maybe (Text.length (scanContent scan)) tokEnd (codeToken scan e)
    nest [] = []
    nest (x : xs) =
      let (inside, rest) = span (\y -> within (symbolRange y) (symbolRange x)) xs
       in x{symbolChildren = symbolChildren x <> nest inside} : nest rest
    within (s1, e1) (s2, e2) = s2 <= s1 && e1 <= e2

-- | @(keyToken, terminatorToken)@ for every @key = value;@ statement directly
-- inside a non-pattern, non-type brace group.
attrFieldStatements :: Scan -> [(Int, Int)]
attrFieldStatements scan =
  concat
    [ fieldsIn i c
    | i <- [0 .. n - 1],
      textAt i == "{",
      not (isTypeToken scan i),
      Just c <- [matchingCloser scan i],
      textAt (c + 1) `notElem` [":", "@"],
      textAt (i - 1) /= "@"
    ]
  where
    n = codeTokenCount scan
    textAt k = maybe "" tokText (codeToken scan k)
    kindAt k = tokKind <$> codeToken scan k
    fieldsIn i c = go (i + 1)
      where
        go k
          | k >= c = []
          | kindAt k `elem` [Just KIdent, Just KString] && textAt (k + 1) `elem` ["=", "."] =
              let e = stmtStop k (0 :: Int)
               in (k, e) : go (e + 1)
          | otherwise = go (stmtStop k (0 :: Int) + 1)
        stmtStop k lets
          | k >= c = c
          | textAt k `elem` ["{", "(", "[", "${"] =
              maybe c (\cl -> stmtStop (cl + 1) lets) (matchingCloser scan k)
          | textAt k == "let" = stmtStop (k + 1) (lets + 1)
          | textAt k == "in" = stmtStop (k + 1) (max 0 (lets - 1))
          | textAt k == ";" && lets == 0 = k
          | otherwise = stmtStop (k + 1) lets

-- | Encode the outline as LSP @DocumentSymbol@ values.
encodeSymbolTree :: Scan -> [SymbolNode] -> [Value]
encodeSymbolTree scan = map encode
  where
    idx = scanLineIndex scan
    pos off =
      let (l, c) = offsetToPosition idx off in object ["line" .= l, "character" .= c]
    rangeV (s, e) = object ["start" .= pos s, "end" .= pos e]
    encode node =
      object $
        [ "name" .= symbolName node,
          "kind" .= symbolKind node,
          "range" .= rangeV (symbolRange node),
          "selectionRange" .= rangeV (symbolSelection node),
          "children" .= map encode (symbolChildren node)
        ]
          <> maybe [] (\d -> ["detail" .= d]) (symbolDetail node)
