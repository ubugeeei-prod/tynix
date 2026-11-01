{-# LANGUAGE OverloadedStrings #-}

-- | Semantic-tokens provider for tynix LSP.
--
-- Tokens come from the error-tolerant scanner, so multi-line strings and
-- comments, string interpolation, and half-typed code all highlight
-- correctly. Identifiers are classified with scope and type information:
-- type names vs. type parameters inside annotations, functions vs. plain
-- values (by inferred or declared type), parameters, attribute keys and
-- selected fields as properties, and @builtins@ / @import@ as default-library
-- symbols. Declarations carry the @declaration@ modifier.
--
-- The legend (see 'semanticTokenTypes' / 'semanticTokenModifiers') keeps the
-- original eight types at their original indices and appends the new ones.
module SessionSemanticTokens
  ( encodeSemanticTokens,
    semanticTokensFor,
    semanticTokensForScan,
    semanticTokenTypes,
    semanticTokenModifierNames,
  )
where

import Data.Bits ((.|.))
import Data.Char (isUpper)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Driver (Analysis (..))
import SessionCompletion (isFunction)
import SessionResolve
import SessionScan
import SessionTypes (SemanticToken (..))
import Type (Scheme (..))

semanticTokenTypes :: [Text]
semanticTokenTypes =
  [ "keyword",
    "type",
    "function",
    "variable",
    "property",
    "string",
    "number",
    "operator",
    "parameter",
    "typeParameter",
    "comment",
    "namespace",
    "decorator"
  ]

semanticTokenModifierNames :: [Text]
semanticTokenModifierNames = ["declaration", "readonly", "defaultLibrary", "deprecated"]

tKeyword, tType, tFunction, tVariable, tProperty, tString, tNumber, tOperator, tParameter, tTypeParameter, tComment, tNamespace, tDecorator :: Int
tKeyword = 0
tType = 1
tFunction = 2
tVariable = 3
tProperty = 4
tString = 5
tNumber = 6
tOperator = 7
tParameter = 8
tTypeParameter = 9
tComment = 10
tNamespace = 11
tDecorator = 12

mDeclaration, mDefaultLibrary :: Int
mDeclaration = 1
mDefaultLibrary = 4

-- | Produce the per-document token list ready for `encodeSemanticTokens`.
semanticTokensFor :: Text -> Either String Analysis -> [SemanticToken]
semanticTokensFor content result = semanticTokensForScan (mkCtx "" content result)

semanticTokensForScan :: Ctx -> [SemanticToken]
semanticTokensForScan ctx =
  concatMap splitLines (concatMap classify (zip [0 :: Int ..] (scanAllTokens scan)))
  where
    scan = ctxScan ctx
    idx = scanLineIndex scan
    analysis = ctxAnalysis ctx
    aliases = ctxAliases ctx
    codeStarts = Map.fromList [(tokStart t, i) | i <- [0 .. codeTokenCount scan - 1], Just t <- [codeToken scan i]]
    textAt k = maybe "" tokText (codeToken scan k)
    kindAt k = tokKind <$> codeToken scan k
    binderTokens = Map.fromList [(binderToken b, b) | b <- scanBinders scan]
    typeParams = [b | b <- scanBinders scan, binderKind b == BindTypeParam]
    localAliases = [binderName b | b <- scanBinders scan, binderKind b == BindTypeAlias]
    topFunctions = case analysis of
      Just a -> Map.keysSet (Map.filter (isFunction . schemeType) (analysisBindings a))
      Nothing -> mempty

    classify (_, t) = case tokKind t of
      KComment
        | "@tynix-" `Text.isInfixOf` tokText t -> [(t, tDecorator, 0)]
        | otherwise -> [(t, tComment, 0)]
      KString -> [(t, tString, 0)]
      KPath -> [(t, tString, 0)]
      KNumber -> [(t, tNumber, 0)]
      KKeyword -> [(t, tKeyword, 0)]
      KSymbol
        | tokText t `elem` operators -> [(t, tOperator, 0)]
        | otherwise -> []
      KIdent -> case Map.lookup (tokStart t) codeStarts of
        Just i -> [(t, ty, mods) | (ty, mods) <- [identifier i t]]
        Nothing -> []
      KUnknown -> []

    operators = ["::", "->", "|>", "<|", "==", "!=", "<=", ">=", "&&", "||", "++", "//", "%1", "=", "+", "-", "*", "/", "<", ">", "!", "|", "?", ":", ".", "@", "..."]

    identifier i t
      | name `elem` ["true", "false", "null"] = (tKeyword, 0)
      | name == "or" && textAt (i - 2) == "." = (tKeyword, 0)
      | isTypeToken scan i = typeIdentifier i t
      | name `elem` ["type", "declare"] && kindAt (i + 1) `elem` [Just KIdent, Just KString, Just KPath] && textAt (i + 1) /= "=" && atStatementStart i = (tKeyword, 0)
      | name == "opaque" && textAt (i + 1) == "type" && atStatementStart i = (tKeyword, 0)
      | name == "macro" && kindAt (i + 1) == Just KIdent && textAt (i + 2) == "{" && atStatementStart i = (tKeyword, 0)
      -- `name!( ... )` invokes a macro.
      | textAt (i + 1) == "!" && textAt (i + 2) == "(" && tokEnd t == maybe (-1) tokStart (codeToken scan (i + 1)) = (tFunction, 0)
      | name == "as" && isTypeToken scan (i + 1) = (tKeyword, 0)
      | Just b <- Map.lookup i binderTokens = binderToken' b mDeclaration
      | textAt (i - 1) == "." =
          if textAt (i - 2) == "builtins" then (memberKind i, mDefaultLibrary) else (memberKind i, 0)
      | isAttrKey i = (tProperty, mDeclaration)
      | Just b <- resolvedBinder i t = binderToken' b 0
      | name == "builtins" = (tNamespace, mDefaultLibrary)
      | name == "import" = (tFunction, mDefaultLibrary)
      | name `elem` topFunctions = (tFunction, 0)
      | otherwise = (tVariable, 0)
      where
        name = tokText t

    atStatementStart i = i == 0 || textAt (i - 1) `elem` [";", "}", "opaque"]

    typeIdentifier i t
      | name `elem` ["forall", "infer", "extends"] = (tKeyword, 0)
      | name `elem` ["dynamic", "any", "unknown"] = (tType, mDefaultLibrary)
      | textAt (i + 1) == "::" = (tProperty, mDeclaration)
      | any (\b -> binderName b == name && binderScopeStart b <= tokStart t && tokStart t <= binderScopeEnd b) typeParams = (tTypeParameter, if Map.member i binderTokens then mDeclaration else 0)
      | Map.member name aliases || name `elem` localAliases = (tType, if Map.member i binderTokens then mDeclaration else 0)
      | maybe False (isUpper . fst) (Text.uncons name) = (tType, if name `elem` builtinTypes then mDefaultLibrary else 0)
      | otherwise = (tTypeParameter, 0)
      where
        name = tokText t

    builtinTypes = ["Int", "Float", "Number", "Nat", "String", "Bool", "Path", "Null", "List", "Vec", "Matrix", "Tensor", "Range", "Unit", "Tuple"]

    resolvedBinder _ t = resolveNameAt scan (tokText t) (tokStart t)

    binderToken' b mods = case binderKind b of
      BindParam -> (tParameter, mods)
      BindPatternField -> (tParameter, mods)
      BindPatternAlias -> (tParameter, mods)
      BindRecField -> (tProperty, mods)
      _ -> (if valueIsFunction b then tFunction else tVariable, mods)

    valueIsFunction b =
      case binderType ctx b of
        Just ty -> isFunction ty
        Nothing -> case binderValue b of
          Just (from, _) -> textAt (from + 1) == ":" || (textAt from == "{" && closesIntoLambda from)
          Nothing -> False
    closesIntoLambda from = case matchingCloser scan from of
      Just c -> textAt (c + 1) `elem` [":", "@"]
      Nothing -> False

    memberKind i =
      let path = selection i
       in case exprPathType ctx (maybe 0 tokStart (codeToken scan (i - 2 * (length path - 1)))) path of
            Just ty | isFunction ty -> tFunction
            _ -> tProperty
    selection i
      | textAt (i - 1) == "." && kindAt (i - 2) == Just KIdent = selection (i - 2) <> [textAt i]
      | otherwise = [textAt i]

    -- attribute keys: `key =` / `key.sub =` at a statement start of an attrset
    isAttrKey i =
      (textAt (i + 1) `elem` ["=", "."])
        && (textAt (i - 1) `elem` ["{", ";", "."])
        && not (isReferenceToken scan i)

    -- split multi-line tokens so clients without multiline support render them
    splitLines (t, ty, mods) =
      let pieces = zip [0 :: Int ..] (Text.splitOn "\n" (tokText t))
          startOffsets = scanl (\acc (_, piece) -> acc + Text.length piece + 1) (tokStart t) pieces
       in [ SemanticToken
              { semanticTokenLine = line,
                semanticTokenStart = col,
                semanticTokenLength = endCol - col,
                semanticTokenType = ty,
                semanticTokenModifiers = mods
              }
          | ((_, piece), off) <- zip pieces startOffsets,
            not (Text.null piece),
            let (line, col) = offsetToPosition idx off
                (_, endCol) = offsetToPosition idx (off + Text.length piece),
            endCol > col
          ]

-- | Convert the per-token list into the LSP integer delta stream that the
-- spec requires.
encodeSemanticTokens :: [SemanticToken] -> [Int]
encodeSemanticTokens tokens = concat (reverse (snd (foldl step (Nothing, []) (sortOn (\token -> (semanticTokenLine token, semanticTokenStart token)) tokens))))
  where
    step (previous, acc) token =
      let deltaLine = maybe (semanticTokenLine token) (\prev -> semanticTokenLine token - semanticTokenLine prev) previous
          deltaStart =
            case previous of
              Just prev
                | semanticTokenLine prev == semanticTokenLine token ->
                    semanticTokenStart token - semanticTokenStart prev
              _ -> semanticTokenStart token
          encoded =
            [ deltaLine,
              deltaStart,
              semanticTokenLength token,
              semanticTokenType token,
              semanticTokenModifiers token .|. 0
            ]
       in (Just token, encoded : acc)
