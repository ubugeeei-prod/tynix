{-# LANGUAGE OverloadedStrings #-}

-- | Hover and signature help built on the scope-aware scanner.
--
-- Hover renders a markdown card: a @tynix@ code block with the symbol's kind,
-- name, and type, followed by its documentation comment (from the source,
-- the nearest @builtins.d.tynix@, or the @declare@ block typing an import).
module SessionHover
  ( hoverAt,
    signatureHelpAt,
    selectionPathAt,
  )
where

import Data.Aeson (Value (..), object, (.=))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Pretty (renderType)
import SessionCompletion
import SessionResolve
import SessionScan

-- | Markdown hover text and the offset range it applies to.
hoverAt :: CompletionEnv -> Ctx -> Int -> Maybe (Text, (Int, Int))
hoverAt env ctx off = do
  i <- codeTokenIndexAt scan off
  t <- codeToken scan i
  let range = (tokStart t, tokEnd t)
  card <- case tokKind t of
    KKeyword -> keywordCard (tokText t)
    KIdent
      | isTypeToken scan i -> typeCard t
      | isSelected i -> selectionCard i t
      | otherwise -> valueCard i t
    KPath -> Just (code (tokText t <> " :: Path"), Nothing)
    _ -> Nothing
  pure (render card, range)
  where
    scan = ctxScan ctx
    aliases = ctxAliases ctx
    textAt k = maybe "" tokText (codeToken scan k)
    isSelected k = textAt (k - 1) == "."
    render (sig, doc) = sig <> maybe "" ("\n\n---\n\n" <>) doc
    code body = "```tynix\n" <> body <> "\n```"
    withType label name ty = code (label <> name <> maybe "" ((" :: " <>) . renderType) ty)

    keywordCard kw = do
      doc <- lookup kw valueKeywords
      Just (code kw, Just doc)

    typeCard t
      | Just alias <- Map.lookup (tokText t) aliases =
          Just (code (renderAlias alias), localAliasDoc (tokText t))
      | Just b <- typeBinder (tokText t) BindTypeAlias =
          Just
            ( code ("type " <> binderName b <> maybe "" (" = " <>) (binderAnnotation b)),
              binderDoc ctx b
            )
      | Just _ <- typeBinder (tokText t) BindTypeParam =
          Just (code ("(type parameter) " <> tokText t), Nothing)
      | Just doc <- lookup (tokText t) builtinTypeNames =
          Just (code (tokText t), Just doc)
      | Just doc <- lookup (tokText t) typeKeywords =
          Just (code (tokText t), Just doc)
      | otherwise = Nothing
    typeBinder name kind =
      case [ b
           | b <- scanBinders scan,
             binderName b == name,
             binderKind b == kind,
             binderScopeStart b <= off,
             off <= binderScopeEnd b
           ] of
        b : _ -> Just b
        [] -> Nothing
    localAliasDoc name = case [b | b <- scanBinders scan, binderKind b == BindTypeAlias, binderName b == name] of
      b : _ -> binderDoc ctx b
      [] -> Nothing

    selectionCard i t = do
      let path = selectionPathAt scan i
          parent = take (length path - 1) path
      start <- codeToken scan (i - 2 * (length path - 1))
      ty <- exprPathType ctx (tokStart start) path
      Just
        ( withType "(field) " (tokText t) (Just ty),
          memberDoc env ctx (tokStart start) parent (tokText t)
        )

    valueCard i t =
      case binderAtToken scan i of
        Just b | isValueBinder b -> Just (binderCard b)
        _ -> case resolveNameAt scan (tokText t) (tokStart t) of
          Just b -> Just (binderCard b)
          Nothing -> do
            ty <- nameType ctx (tokText t)
            Just (withType "" (tokText t) (Just ty), freeDoc (tokText t))
    binderCard b = (withType (kindLabel b) (binderName b) (binderType ctx b), binderDoc ctx b)
    freeDoc name
      | name == "builtins" = Just "The Nix `builtins` attribute set."
      | name == "import" =
          Just
            "Load and evaluate a Nix file. Typed by matching `declare` blocks when available."
      | otherwise = Nothing

kindLabel :: Binder -> Text
kindLabel b = case binderKind b of
  BindParam -> "(parameter) "
  BindPatternField -> "(parameter) "
  BindPatternAlias -> "(argument set) "
  BindInherit -> "(inherited) "
  BindRecField -> "(field) "
  _ -> ""

-- | The selection chain @a.b.c@ that ends at code token @i@ (the token of
-- @c@), as names.
selectionPathAt :: Scan -> Int -> [Text]
selectionPathAt scan i = reverse (go i)
  where
    textAt k = maybe "" tokText (codeToken scan k)
    kindAt k = tokKind <$> codeToken scan k
    go k
      | textAt (k - 1) == "." && kindAt (k - 2) == Just KIdent = textAt k : go (k - 2)
      | otherwise = [textAt k]

-- | LSP @SignatureHelp@ for the application around the cursor, or 'Null'.
signatureHelpAt :: CompletionEnv -> Ctx -> Int -> Value
signatureHelpAt env ctx off =
  fromMaybe Null $ do
    k <- codeTokenBefore scan off
    lastTok <- codeToken scan k
    (path, calleeStart, argIx) <- calleeBefore scan (k + 1)
    fnTy <- exprPathType ctx calleeStart path
    let params = functionParams (ctxAliases ctx) fnTy
        midWord =
          tokEnd lastTok == off
            && tokKind lastTok `elem` [KIdent, KNumber, KString, KPath]
        active = if midWord then argIx - 1 else argIx
    if null params || active < 0 then Nothing else Just ()
    let name = Text.intercalate "." path
        names = paramNames path calleeStart
        paramLabel ix ty = maybe "" (<> " :: ") (lookupIx ix names) <> renderType ty
        doc = case path of
          [single] -> resolveNameAt scan single calleeStart >>= binderDoc ctx
          _ -> memberDoc env ctx calleeStart (init path) (last path)
    pure $
      object
        [ "signatures"
            .= [ object $
                   [ "label" .= (name <> " :: " <> renderType fnTy),
                     "parameters"
                       .= [object ["label" .= paramLabel ix ty] | (ix, ty) <- zip [0 :: Int ..] params]
                   ]
                     <> maybe
                       []
                       (\d -> ["documentation" .= object ["kind" .= ("markdown" :: Text), "value" .= d]])
                       doc
               ],
          "activeSignature" .= (0 :: Int),
          "activeParameter" .= min active (length params - 1)
        ]
  where
    scan = ctxScan ctx
    lookupIx ix xs = if ix < length xs then Just (xs !! ix) else Nothing
    -- names of simple curried parameters (`f = x: y: …`)
    paramNames [single] start = case resolveNameAt scan single start of
      Just b | Just (from, _) <- binderValue b -> lambdaNames from
      _ -> []
    paramNames _ _ = []
    lambdaNames k =
      case (codeToken scan k, codeToken scan (k + 1)) of
        (Just a, Just colon)
          | tokKind a == KIdent && tokText colon == ":" -> tokText a : lambdaNames (k + 2)
        _ -> []
