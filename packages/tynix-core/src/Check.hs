{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Gradual type checker and local inference engine for tynix.
--
-- The checker intentionally aims for useful incremental feedback rather than
-- whole-program soundness. `dynamic` is built in, imports can be typed from
-- ambient declarations, and remaining inference variables are surfaced as
-- polymorphic schemes instead of forcing runtime evidence.
module Check
  ( CheckContext (..),
    CheckError (..),
    CheckResult (..),
    checkProgram,
    checkProgramDetailed,
    collapseParentSegments,
    resolvePath,
  )
where

import Alias
import Check.Constraints
import Check.Diagnostics
import Check.Effects
import Check.Expression
import Check.Inference
import Control.Applicative ((<|>))
import Control.Monad (foldM, forM, forM_, unless, void, when)
import Data.Functor (($>), (<&>))
import Data.Graph (flattenSCC, stronglyConnComp)
import Data.List (group, intercalate, isInfixOf, nub, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text qualified as T
import Diagnostics (DiagnosticCode (..), withCode)
import Indexed
import Subtyping
import Syntax
import System.FilePath (isAbsolute, joinPath, normalise, splitDirectories, takeDirectory, (</>))
import Type

-- | Check a parsed program and infer its public types.
--
-- The built-in environment is intentionally tiny: `builtins` is left fully
-- dynamic and `import` only promises that a path yields something. Ambient
-- declarations refine imports when they are available.
--
-- The checker follows a "best effort but explicit" policy:
--
-- * annotations are respected when they are structurally satisfied,
-- * gradual behavior only kicks in when `dynamic` is genuinely involved,
-- * unresolved inference variables are closed into stable schemes before
--   results escape the module.
--
-- Representative examples:
--
-- @
-- let id = x: x; in id
--   => forall a. a -> a
--
-- let xs :: Vec (Range 2 4 Nat) Int; xs = [1 2 3]; in xs
--   => accepted
--
-- let xs :: Vec (2 | Range 4 8 Nat) Int; xs = [1 2 3]; in xs
--   => rejected
-- @
checkProgram :: CheckContext -> Program -> Either String CheckResult
checkProgram ctx = either (Left . checkErrorMessage) Right . checkProgramDetailed ctx

-- | Like 'checkProgram', but keeps the source span of the failure.
checkProgramDetailed :: CheckContext -> Program -> Either CheckError CheckResult
checkProgramDetailed ctx program =
  runInference (inferTop ctx (globalEnvironment ctx))
  where
    inferTop local env = case programExpr program of
      Nothing -> pure (CheckResult Nothing Map.empty)
      Just markedExpr -> inferRootExpression local env markedExpr

-- | The names Nix puts in scope without a `builtins.` prefix.
--
-- Each global shares its type with the matching `builtins` member when the
-- ambient `builtins` declaration provides one, so richer declarations flow to
-- both spellings automatically. Missing members fall back to a conservative
-- built-in signature or `dynamic`.
globalEnvironment :: CheckContext -> TypeEnv
globalEnvironment ctx =
  Map.fromList $
    [ ("builtins", builtinsScheme),
      ("import", Scheme [] (TFun Many tPath tDynamic))
    ]
      <> [(name, memberScheme name) | name <- globalBuiltinNames]
  where
    builtinsScheme = Map.findWithDefault (Scheme [] tDynamic) "builtins" (checkAmbient ctx)
    memberScheme name =
      case lookupRecordField (checkAliases ctx) (schemeType builtinsScheme) name of
        Just fieldTy -> schemeFromAnnotation fieldTy
        Nothing -> fallbackGlobal name
    fallbackGlobal = \case
      "throw" -> Scheme ["a"] (TArrow (effectfulArrow ["Throw"]) tString (TVar "a"))
      "abort" -> Scheme ["a"] (TArrow (effectfulArrow ["Abort"]) tString (TVar "a"))
      "toString" -> Scheme [] (TFun Many tDynamic tString)
      "isNull" -> Scheme [] (TFun Many tDynamic tBool)
      "baseNameOf" -> Scheme [] (TFun Many tDynamic tString)
      "map" -> Scheme ["a", "b"] (TFun Many (TFun Many (TVar "a") (TVar "b")) (TFun Many (tList (TVar "a")) (tList (TVar "b"))))
      _ -> Scheme [] tDynamic

-- | Builtins that Nix also exposes as top-level identifiers.
globalBuiltinNames :: [Name]
globalBuiltinNames =
  [ "abort",
    "baseNameOf",
    "break",
    "derivation",
    "derivationStrict",
    "dirOf",
    "fetchGit",
    "fetchMercurial",
    "fetchTarball",
    "fetchTree",
    "fromTOML",
    "isNull",
    "map",
    "placeholder",
    "removeAttrs",
    "scopedImport",
    "throw",
    "toString"
  ]

-- | Infer the type of one expression under the current local environment.
--
-- The function is syntax-directed except for applications and annotations,
-- where it consults `constrain`/`unify` to reconcile inferred and expected
-- structure. List literals delegate to `inferListType`, which means exact
-- vector, matrix, or tensor shapes can be recovered directly from surface list
-- syntax.
--
-- Representative examples:
--
-- @
-- inferExpr [] [1 2]
--   => Vec 2 (1 | 2)
--
-- inferExpr [] [[1 2] [3 4]]
--   => Matrix 2 2 (1 | 2 | 3 | 4)
--
-- inferExpr [] (import ./unknown.nix)
--   => dynamic
--
-- inferExpr [] ({ value = 1; } as { value :: Int; })
--   => { value :: Int; }
-- @
inferExpr :: CheckContext -> TypeEnv -> Expr -> InferM Type
inferExpr ctx env = \case
  ELoc region inner -> withSpan region (inferExpr ctx env inner)
  EVar name ->
    case Map.lookup name env of
      Just scheme -> do
        noteUse name
        -- A binder checked against a dependent arrow holds a singleton; used
        -- as an ordinary value it is just its base type.
        instantiate scheme >>= zonkHead >>= \case
          TSingleton _ base -> pure base
          -- A higher-rank parameter (`f :: forall a. a -> a`) is
          -- instantiated afresh at every use.
          TForall vars body -> instantiate (Scheme vars body)
          ty -> pure ty
      Nothing
        | checkOpenScope ctx -> pure tDynamic
        | otherwise -> throwCheck (withCode TC0001UnboundName ("unbound name: " <> quoteName name))
  EString text -> pure (TLit (LString (stringLiteralText text)))
  EInterp _ parts -> do
    mapM_ (\case StrExpr expr -> void (inferExpr ctx env expr); _ -> pure ()) parts
    pure tString
  EFloat n -> pure (TLit (LFloat n))
  EInt n -> pure (TLit (LInt n))
  EBool b -> pure (TLit (LBool b))
  ENull -> pure tNull
  EPath _ -> pure tPath
  ESearchPath _ -> pure tPath
  EPathInterp parts -> do
    mapM_ (\case StrExpr expr -> void (inferExpr ctx env expr); _ -> pure ()) parts
    pure tPath
  ELambda pattern' body -> inferLambda ctx env pattern' body Nothing
  EApp fun arg
    | EVar "import" <- unloc fun,
      Just target <- importTarget (unloc arg),
      -- Only the builtin `import` resolves ambient declarations; a local
      -- binding named `import` shadows it, as in Nix.
      Map.lookup "import" env == Map.lookup "import" (globalEnvironment ctx) ->
        maybe (pure tDynamic) instantiate (Map.lookup (resolvePath (checkFile ctx) target) (checkAmbient ctx))
  EApp fun arg -> do
    -- Only the head is zonked so that metas inside the parameter type keep
    -- their identity (needed to widen `max 1 2` to `1 | 2`).
    funTy <- inferExpr ctx env fun >>= zonkHead
    let resolvedFunTy = resolveHead (checkAliases ctx) funTy
        -- Only a linear function consumes its argument exactly once.
        argScale = case resolvedFunTy of
          TFun One _ _ -> UOne
          _ -> UMany
    -- A known parameter type is pushed into a lambda argument before its
    -- body is inferred (bidirectional checking), so `f (ps: [ ps.x ])`
    -- sees what `ps` is.
    argTy <- withScale argScale . handleEffects ctx (handledEffects fun) $ case resolvedFunTy of
      TFun _ domTy _ -> checkAgainst ctx env arg domTy
      _ -> inferExpr ctx env arg
    case resolvedFunTy of
      TDynamic -> pure tDynamic
      TAny -> pure tAny
      TArrow arrow domTy outTy -> do
        _ <- atSpanOf arg (constrain ctx argTy domTy)
        perform ctx (arrowEffects arrow)
        result <- case arrowBinder arrow of
          Nothing -> pure outTy
          Just binder -> do
            index <- dependentIndex env arg argTy domTy
            reduceOperators (checkAliases ctx) <$> zonk (substituteTypeVars (Map.singleton binder index) outTy)
        result' <- zonkHead result
        -- A type-level operator in the result (`Get r k`) can be
        -- reduced once the arguments have pinned its inputs.
        reduced <-
          if isOperatorApp result'
            then reduceOperators (checkAliases ctx) <$> zonk result'
            else pure result'
        instantiateRank reduced
      _
        | definitelyNotCallable resolvedFunTy ->
            throwCheck (withCode TC0018NotCallable ("cannot call " <> describeNonCallable resolvedFunTy <> " as a function"))
        | TMeta n <- resolvedFunTy -> do
            soft <- isSoftMeta n
            if soft
              then do
                -- An injected dependency: stay gradual instead of
                -- fixing its type from this one call site.
                _ <- bindMeta n (TFun Many tDynamic tDynamic)
                pure tDynamic
              else applyUnknown ctx funTy argTy
        | otherwise -> applyUnknown ctx funTy argTy
  EBinaryOp op left right -> inferBinaryOp ctx env op left right
  EUnaryOp OpNot operand -> do
    operandTy <- inferExpr ctx env operand
    _ <- constrain ctx operandTy tBool
    pure tBool
  EUnaryOp OpNeg operand -> do
    operandTy <- inferExpr ctx env operand >>= zonk
    let resolved = resolveHead (checkAliases ctx) operandTy
    if resolved == tAny || resolved == tDynamic
      then pure resolved
      else do
        _ <- constrain ctx operandTy tNumber
        pure (maybe tNumber widenSingleNumericFamily (numericFamily resolved))
  EHasAttr base steps -> do
    _ <- inferExpr ctx env base
    forM_ [key | SelectDynamic key <- steps] $ \key -> do
      keyTy <- inferExpr ctx env key
      constrain ctx keyTy tString
    pure tBool
  EAssert cond body -> do
    condTy <- inferExpr ctx env cond
    _ <- constrain ctx condTy tBool
    -- A failed assertion throws (and `tryEval` can catch it).
    perform ctx (effectRow ["Throw"] Nothing)
    inferExpr ctx env body
  EWith scope body -> do
    scopeTy <- inferExpr ctx env scope >>= zonk
    case resolveHead (checkAliases ctx) scopeTy of
      -- A known record contributes its fields (lexical bindings still win), and
      -- truly-unbound names in the body remain errors.
      TRecord fields -> inferExpr ctx (Map.union env (Map.map (Scheme []) fields)) body
      -- Any other scope (dynamic, unknown, ...) cannot be enumerated, so the
      -- body is checked with an open scope where unresolved names are dynamic.
      _ -> inferExpr ctx{checkOpenScope = True} env body
  ELet items body -> hideNames (letBoundNames items) $ do
    (env', _) <- inferLet ctx env items body
    inferExpr ctx env' body
  EAttrSet rawItems -> do
    (items, dynamicEntries) <- normalizeAttrItems rawItems
    fields <- concat <$> traverse inferAttr items
    case duplicateNames (map fst fields) of
      dup : _ -> throwCheck (withCode TC0002DuplicateAttribute ("duplicate attribute: " <> quoteName dup))
      [] -> finishAttrSet ctx env dynamicEntries (Map.fromList fields)
    where
      inferAttr = \case
        AttrField name expr -> do
          ty <- inferExpr ctx env expr
          pure [(name, ty)]
        AttrInherit names ->
          traverse (\name -> inferExpr ctx env (EVar name) >>= \ty -> pure (name, ty)) names
        AttrInheritFrom source names -> inferInheritFrom ctx env source names
        AttrPath _ _ -> pure []
  ERec items -> inferRecAttrSet ctx env items
  ESelectOr base fields fallback -> do
    fallbackTy <- inferExpr ctx env fallback
    baseTy <- inferExpr ctx env base >>= zonk
    -- `x.a or d` must not *require* `a`: when the path runs into a value whose
    -- shape is still unknown, the result is just the fallback's type joined
    -- with a fresh unknown, and no row requirement is recorded.
    known <- pathIsKnown ctx env baseTy fields
    attempt <-
      if known
        then catchInfer (inferExpr ctx env (ESelect base fields))
        else Right <$> freshMeta
    case attempt of
      Right selectedTy -> do
        selected <- zonk selectedTy
        fallback' <- zonk fallbackTy
        pure (joinTypes (checkAliases ctx) selected fallback')
      Left err
        | isMissingFieldError err -> do
            -- The base must still type-check on its own; only the missing
            -- path is covered by the default.
            _ <- inferExpr ctx env base
            pure fallbackTy
        | otherwise -> throwCheckError err
  ESelect base fields -> do
    -- Reading the clock, the host platform, or the search path is impure.
    case (unloc base, fields) of
      (EVar "builtins", SelectName name : _)
        | name `elem` impureValues,
          Map.lookup "builtins" env == Map.lookup "builtins" (globalEnvironment ctx) ->
            perform ctx (effectRow ["Impure"] Nothing)
      _ -> pure ()
    baseTy <- inferExpr ctx env base
    foldM step baseTy fields
    where
      step ty field =
        case field of
          SelectName name -> inferStaticSelect ctx ty name
          SelectDynamic expr -> do
            keyTy <- inferExpr ctx env expr
            inferDynamicSelect ctx ty keyTy
  EIf cond yesExpr noExpr -> do
    condTy <- inferExpr ctx env cond
    _ <- constrain ctx condTy tBool
    -- Only one branch runs, so each must consume a linear binder the same
    -- number of times.
    (yesTy, noTy) <-
      branchUsages
        (inferExpr ctx env yesExpr >>= zonk)
        (inferExpr ctx env noExpr >>= zonk)
    joinBranches ctx yesTy noTy
  EList members ->
    traverse (inferExpr ctx env) members
      <&> inferListType (joinTypes (checkAliases ctx))
  ECast expr assertedTy -> do
    actualTy <- inferExpr ctx env expr
    checkCast ctx actualTy assertedTy
  EAscribe expr ascribedTy -> do
    actualTy <- checkAgainst ctx env expr ascribedTy
    _ <- constrain ctx actualTy ascribedTy
    pure ascribedTy

-- | Whether every step of a selection path can be resolved without guessing:
-- the base (and each intermediate value) has a known shape.
pathIsKnown :: CheckContext -> TypeEnv -> Type -> [SelectStep] -> InferM Bool
pathIsKnown _ _ _ [] = pure True
pathIsKnown ctx env ty (step : rest) = do
  ty' <- zonk ty
  case resolveHead (checkAliases ctx) ty' of
    TMeta _ -> pure False
    TOpenRecord fields (TMeta _)
      | SelectName name <- step,
        not (Map.member name fields) ->
          pure False
    resolved
      | SelectName name <- step,
        Just fieldTy <- lookupRecordField (checkAliases ctx) resolved name ->
          pathIsKnown ctx env fieldTy rest
      | otherwise -> pure True

-- | Join the two branches of a conditional. While metas are involved the
-- branches are unified (after widening literals, so `true`/`false` meet at
-- `Bool`), which is what lets recursive definitions such as mutually
-- recursive `even`/`odd` solve; fully known branches keep their precise join.
joinBranches :: CheckContext -> Type -> Type -> InferM Type
joinBranches ctx yesTy noTy
  | hasUnresolvedMetas yesTy noTy = do
      attempt <- catchInfer (unify ctx (widenLiterals yesTy) (widenLiterals noTy))
      case attempt of
        Right ty -> zonk ty
        Left _ -> pure (joinTypes (checkAliases ctx) yesTy noTy)
  | otherwise = pure (joinTypes (checkAliases ctx) yesTy noTy)

isNullLiteral :: Expr -> Bool
isNullLiteral expr =
  case unloc expr of
    ENull -> True
    _ -> False

-- | Infer an expression while a target type is known. Lambdas take their
-- parameter type from the target before the body is inferred, and their body
-- is checked against the target result; everything else is plain inference.
-- Callers still constrain the result against the target.
checkAgainst :: CheckContext -> TypeEnv -> Expr -> Type -> InferM Type
checkAgainst ctx env expr expected =
  case expr of
    ELoc region inner -> withSpan region (checkAgainst ctx env inner expected)
    ELambda pattern' body -> do
      expected' <- zonk expected
      target <- skolemize expected'
      case functionTarget (resolveHead (checkAliases ctx) target) of
        Just arrowTarget -> do
          checked <- inferLambda ctx env pattern' body (Just arrowTarget)
          -- A lambda checked against fresh skolems works at every instance,
          -- so it has the polymorphic type itself.
          pure $ case expected' of
            TForall _ _ -> expected'
            _ -> checked
        Nothing -> inferExpr ctx env expr
    _ -> inferExpr ctx env expr
  where
    functionTarget = \case
      TArrow arrow domTy codTy -> Just (arrow, domTy, codTy)
      TUnion members ->
        case [(arrow, d, c) | TArrow arrow d c <- map (resolveHead (checkAliases ctx)) members] of
          [single] -> Just single
          _ -> Nothing
      _ -> Nothing

-- | Whether a type is an application of a built-in type-level operator.
isOperatorApp :: Type -> Bool
isOperatorApp ty =
  case collectApps ty of
    (TCon name, _ : _) -> name `elem` ["Get", "KeyOf", "Add", "Sub", "Mul", "Length"]
    _ -> False

-- | Infer a lambda, optionally against a known arrow (bidirectional
-- checking). Besides the argument and result types this determines the
-- arrow's
--
-- * multiplicity — linear when the binder is consumed exactly once;
-- * latent effects — the effect row its body performs;
-- * captures — the effectful capabilities the closure closes over;
--
-- and, when an expected arrow is given, enforces each of them: a `%1`
-- arrow demands a linear body, `! { ... }` bounds the effects, `->{...}`
-- bounds the captures, and a dependent arrow `(n :: Nat) -> B` gives the
-- binder the singleton type `n`, so the body is checked against `B` itself.
inferLambda :: CheckContext -> TypeEnv -> Pattern -> Expr -> Maybe (Arrow, Type, Type) -> InferM Type
inferLambda ctx env pattern' body expected = do
  (argTy, patternEnv0) <- case (pattern', expected) of
    -- An unannotated binder simply takes the expected parameter type, which
    -- keeps a higher-rank parameter (`forall a. a -> a`) polymorphic.
    (PVar name Nothing, Just (_, domTy, _)) -> pure (domTy, Map.singleton name (Scheme [] domTy))
    _ -> do
      bound <- inferPatternBindings ctx env pattern'
      forM_ expected $ \(_, domTy, _) ->
        -- The expected parameter flows *into* the pattern (contravariance).
        catchInfer (constrain ctx domTy (fst bound))
      pure bound
  let (patternEnv, bodyTarget, dependentOn) =
        case (expected, pattern') of
          (Just (arrow, domTy, codTy), PVar name _)
            | Just binder <- arrowBinder arrow ->
                let singleton = TSingleton name domTy
                 in ( Map.insert name (Scheme [] singleton) patternEnv0,
                      Just (substituteTypeVars (Map.singleton binder singleton) codTy),
                      Just name
                    )
          (Just (arrow, domTy, codTy), _) -> (patternEnv0, Just (eraseBinder arrow domTy codTy), Nothing)
          (Nothing, _) -> (patternEnv0, Nothing, Nothing)
      bodyEnv = patternEnv <> env
  latent <- freshMeta
  (bodyTy, usage) <-
    withEffects latent . trackPattern pattern' . inLambdaBody $
      case bodyTarget of
        Just codTy -> checkAgainst ctx bodyEnv body codTy
        Nothing -> inferExpr ctx bodyEnv body
  captures <- lambdaCaptures ctx env pattern' body
  forM_ expected $ \(arrow, _, _) -> do
    when (arrowMult arrow == One && usage /= UOne) $
      throwCheck (withCode TC0026LinearityViolation (linearityMessage pattern' usage))
    constrainEffects latent (arrowEffects arrow)
    forM_ (arrowCaptures arrow) $ \allowed ->
      case filter (`notElem` allowed) captures of
        [] -> pure ()
        extra ->
          throwCheck
            ( withCode
                TC0025CaptureNotAllowed
                ("closure captures " <> quoteNames extra <> ", but its type only allows " <> captureSetText allowed)
            )
  let arrow =
        Arrow
          { arrowMult = if usage == UOne then One else Many,
            arrowEffects = latent,
            arrowCaptures = Just captures,
            arrowBinder = dependentOn
          }
  pure (TArrow arrow argTy (maybe id abstractSingleton dependentOn bodyTy))
  where
    captureSetText allowed = "{" <> intercalate ", " (map T.unpack allowed) <> "}"

-- | Turn the singleton of a dependent binder back into the binder's name, so
-- an inferred dependent lambda reads `(n :: Nat) -> Vec n a`.
abstractSingleton :: Name -> Type -> Type
abstractSingleton name = go
  where
    go = \case
      TSingleton other _ | other == name -> TVar name
      TArrow arrow a b -> TArrow arrow (go a) (go b)
      TTypeList items -> TTypeList (map go items)
      TRecord fields -> TRecord (fmap go fields)
      TOpenRecord fields tail' -> TOpenRecord (fmap go fields) (go tail')
      TOptional inner -> TOptional (go inner)
      TUnion members -> TUnion (map go members)
      TApp f x -> TApp (go f) (go x)
      other -> other

linearityMessage :: Pattern -> Usage -> String
linearityMessage pattern' usage =
  "linear binder " <> binder <> " " <> problem
  where
    binder = case pattern' of
      PVar name _ -> quoteName name
      _ -> "of an attribute-set pattern"
    problem = case usage of
      UZero -> "is never used, but a `%1 ->` function must consume its argument exactly once"
      UMany -> "is used more than once (directly, inside a closure, or as the argument of an unrestricted function)"
      UMixed -> "is not used exactly once on every branch"
      UOne -> "is used exactly once"

-- | The effectful capabilities a lambda closes over: free variables bound
-- in the enclosing (non-global) scope whose type is a function with a known,
-- non-empty effect row.
lambdaCaptures :: CheckContext -> TypeEnv -> Pattern -> Expr -> InferM [Name]
lambdaCaptures ctx env pattern' body = do
  let globals = globalEnvironment ctx
      candidates =
        [ (name, scheme)
        | name <- Set.toList (freeVariables (ELambda pattern' body)),
          Just scheme <- [Map.lookup name env],
          Map.lookup name globals /= Just scheme
        ]
  fmap concat . forM candidates $ \(name, Scheme _ ty) -> do
    ty' <- zonk ty
    pure [name | isCapability ty']
  where
    isCapability ty =
      case resolveHead (checkAliases ctx) ty of
        TArrow arrow _ _ -> effectful (arrowEffects arrow)
        _ -> False
    effectful = \case
      TRecord fields -> not (Map.null fields)
      TOpenRecord fields tail' -> not (Map.null fields) || isRowVar tail'
      TVar _ -> True
      _ -> False
    isRowVar = \case
      TVar _ -> True
      _ -> False

-- | Apply a callee whose type is not known yet. Its parameter type is
-- inferred from this argument, widened so one call site's literal does not
-- pin the parameter to a singleton.
applyUnknown :: CheckContext -> Type -> Type -> InferM Type
applyUnknown ctx funTy argTy = do
  outTy <- freshMeta
  effects <- freshMeta
  argTy' <- widenLiterals <$> zonk argTy
  _ <- unify ctx funTy (TArrow (plainArrow Many){arrowEffects = effects} argTy' outTy)
  -- Whatever the callee turns out to perform happens here.
  perform ctx effects
  zonk outTy

-- | Recognize `import <path>` targets that can be resolved statically.
importTarget :: Expr -> Maybe FilePath
importTarget = \case
  EPath raw -> Just raw
  EString raw -> Just (T.unpack (stringLiteralText raw))
  _ -> Nothing

isMissingFieldError :: CheckError -> Bool
isMissingFieldError err =
  any (`isInfixOf` checkErrorMessage err) ["TC0009", "TC0010"]

-- | Infer `inherit (source) a b;` as selections from @source@.
inferInheritFrom :: CheckContext -> TypeEnv -> Expr -> [Name] -> InferM [(Name, Type)]
inferInheritFrom ctx env source names = do
  sourceTy <- inferExpr ctx env source
  traverse (\name -> (,) name <$> inferStaticSelect ctx sourceTy name) names

-- | Fold nested attribute paths into ordinary fields.
--
-- `a.b = 1; a.c = 2;` becomes `a = { b = 1; c = 2; };`, merging with a literal
-- attribute set written for the same key, exactly as Nix does. Entries whose
-- first key is dynamic (`${k} = v;`) cannot be folded statically and are
-- returned separately as key/value expression pairs.
normalizeAttrItems :: [AttrItem] -> InferM ([AttrItem], [(Expr, Expr)])
normalizeAttrItems items = do
  let entries = concatMap entry items
      staticKeys = nub [name | Left (name, _, _) <- entries]
      dynamicEntries = [(key, value) | Right (key, value) <- entries]
      inherits = filter isInherit items
  merged <- traverse (\name -> mergeKey name [(path, value) | Left (key, path, value) <- entries, key == name]) staticKeys
  pure (inherits <> merged, dynamicEntries)
  where
    entry = \case
      AttrField name value -> [Left (name, [], value)]
      AttrPath (SelectName name : rest) value -> [Left (name, rest, value)]
      AttrPath (SelectDynamic key : rest) value -> [Right (key, nestValue rest value)]
      _ -> []
    isInherit = \case
      AttrInherit _ -> True
      AttrInheritFrom _ _ -> True
      _ -> False
    mergeKey name = \case
      [([], value)] -> pure (AttrField name value)
      group' -> AttrField name . EAttrSet . concat <$> traverse (subItems name) group'
    subItems name = \case
      ([], value) ->
        case unloc value of
          EAttrSet nested -> pure nested
          _ -> throwCheck (withCode TC0002DuplicateAttribute ("duplicate attribute: " <> quoteName name))
      (path, value) -> pure [pathItem path value]
    nestValue [] value = value
    nestValue path value = EAttrSet [pathItem path value]
    pathItem [SelectName name] value = AttrField name value
    pathItem path value = AttrPath path value

-- | Close an attribute set's type, accounting for dynamically-named entries.
--
-- Keys must be string-like. An attribute set built only from computed keys is
-- a dictionary (`AttrsOf v`); one mixing static and computed keys keeps its
-- static fields and gets a `dynamic` row for the rest.
finishAttrSet :: CheckContext -> TypeEnv -> [(Expr, Expr)] -> Map Name Type -> InferM Type
finishAttrSet ctx env dynamicEntries fields
  | null dynamicEntries = pure (TRecord fields)
  | otherwise = do
      valueTys <- forM dynamicEntries $ \(key, value) -> do
        keyTy <- inferExpr ctx env key
        _ <- constrain ctx keyTy tString
        inferExpr ctx env value >>= zonk
      let aliases = checkAliases ctx
      pure $
        if Map.null fields
          then -- Only computed keys: a dictionary of the joined value type.
            case valueTys of
              x : xs -> tAttrsOf (foldRight1 (joinTypes aliases) x xs)
              [] -> tAttrsOf tDynamic
          else -- Known fields plus computed keys: keep what is known, open the rest.
            TOpenRecord fields tDynamic

-- | Infer a mutually recursive `let` group.
--
-- The algorithm proceeds in three phases:
--
-- * collect user signatures,
-- * allocate placeholders so recursive bindings may refer to one another,
-- * infer each body and constrain it against its placeholder/signature.
--
-- This arrangement keeps explicit signatures authoritative while still allowing
-- recursive inference for unannotated bindings.
--
-- Representative examples:
--
-- @
-- let
--   id :: forall a. a -> a;
--   id = x: x;
-- in id
--   => keeps the declared polymorphic scheme for `id`
--
-- let
--   value = value;
-- in value
--   => allocates a placeholder first, then constrains recursively
-- @
inferLet :: CheckContext -> TypeEnv -> [Marked LetItem] -> Expr -> InferM (TypeEnv, Map Name Scheme)
inferLet ctx env items letBody = do
  let itemDirectives =
        Map.fromListWith
          (\_ earlier -> earlier)
          [(name, directive) | Marked (Just directive) item <- items, name <- letItemNames item]
      inheritedNames = concat [names | Marked _ (LetInherit Nothing names) <- items]
      attrItems = concatMap (letItemAttrs . markedValue) items
  let simpleNames = [name | Marked _ (LetBinding name _) <- items] <> concat [names | Marked _ (LetInherit _ names) <- items]
  rejectDuplicates TC0004DuplicateBinding "duplicate bindings: " simpleNames
  (normalized, dynamicEntries) <- normalizeAttrItems attrItems
  unless (null dynamicEntries) (throwCheck (withCode TC0022DynamicLetBinding "dynamic attributes are not allowed in let"))
  let bindings =
        [(name, expr, Map.lookup name itemDirectives) | AttrField name expr <- normalized]
          <> [(name, ESelect source [SelectName name], Map.lookup name itemDirectives) | AttrInheritFrom source names <- normalized, name <- names]
      bindingNames = [name | (name, _, _) <- bindings]
      allNames = bindingNames <> inheritedNames
      missing = filter (`notElem` allNames) (Map.keys signatures)
      inheritedScheme name =
        case Map.lookup name env of
          Just scheme -> pure (name, scheme)
          Nothing
            | checkOpenScope ctx -> pure (name, Scheme [] tDynamic)
            | otherwise -> throwCheck (withCode TC0001UnboundName ("unbound name: " <> quoteName name))
  rejectDuplicates TC0003DuplicateSignature "duplicate signatures: " [name | Marked _ (LetSignature name _) <- items]
  rejectDuplicates TC0004DuplicateBinding "duplicate bindings: " allNames
  unless (null missing) (throwCheck (withCode TC0005MissingBindingForSignature ("missing bindings for signatures: " <> quoteNames missing)))
  inherited <- Map.fromList <$> traverse inheritedScheme inheritedNames
  -- Signed bindings are known up front (enabling polymorphic recursion);
  -- unsigned ones are inferred one dependency group at a time, in
  -- topological order, and generalized as soon as their group is solved.
  -- That is what gives `let` Hindley-Milner polymorphism: a helper used at
  -- two different types later in the same `let` is instantiated freshly.
  let bindingNameSet = Set.fromList bindingNames
      signedEnv = Map.restrictKeys signatures bindingNameSet
      baseEnv = signedEnv <> inherited <> env
      bindingBodies = Map.fromList [(name, (expr, directive)) | (name, expr, directive) <- bindings]
      unsignedNames = Set.fromList [name | name <- bindingNames, not (Map.member name signatures)]
      dependencyGroups =
        stronglyConnComp
          [ (name, name, Set.toList (Set.intersection (exprFreeNames expr) bindingNameSet))
          | (name, expr, _) <- bindings
          ]
  (finalEnv, inferredList) <- foldM (inferGroup unsignedNames bindingBodies) (baseEnv, []) (map flattenSCC dependencyGroups)
  let finalBindings = Map.fromList inferredList <> inherited
  pure (finalBindings <> finalEnv, finalBindings)
  where
    signatures = Map.fromList [(name, schemeFromAnnotation ty) | Marked _ (LetSignature name ty) <- items]
    signatureDirectives = Map.fromList [(name, directive) | Marked (Just directive) (LetSignature name _) <- items]

    inferGroup unsignedNames bindingBodies (currentEnv, inferredBindings) members = do
      placeholders <-
        Map.fromList
          <$> traverse (\name -> (,) name . Scheme [] <$> freshMeta) (filter (`Set.member` unsignedNames) members)
      let groupEnv = placeholders <> currentEnv
      results <- traverse (inferBinding bindingBodies groupEnv) members
      -- Generalize against the environment *outside* this group, so metas
      -- still shared with enclosing lambdas stay monomorphic.
      generalized <- forM results $ \(name, ty) ->
        case Map.lookup name signatures of
          Just scheme -> pure (name, scheme)
          Nothing -> (,) name <$> generalize currentEnv ty
      pure (Map.fromList generalized <> currentEnv, inferredBindings <> generalized)

    inferBinding bindingBodies groupEnv name = do
      (expr, inlineDirective) <-
        maybe (throwCheck (withCode TC0017MissingPlaceholder ("internal: missing binding " <> show name))) pure (Map.lookup name bindingBodies)
      -- A signed binding is checked against its signature with the
      -- quantified variables held *rigid* (skolemized): `forall a. a -> a`
      -- must work for every `a`, so a body returning `1` is rejected.
      -- Other bindings still use the signature polymorphically.
      expected <- case (Map.lookup name signatures, Map.lookup name groupEnv) of
        (Just (Scheme _ signatureTy), _) -> pure signatureTy
        (Nothing, Just scheme) -> instantiate scheme
        (Nothing, Nothing) -> throwCheck (withCode TC0017MissingPlaceholder ("internal: missing placeholder for binding " <> show name))
      let directive = inlineDirective <|> Map.lookup name signatureDirectives
      -- A binding read more than once duplicates whatever it consumed, so
      -- (for linearity) its right-hand side counts as used many times.
      let readCount = usageCount name letBody + sum [usageCount name other | (otherName, (other, _)) <- Map.toList bindingBodies, otherName /= name]
      attempt <-
        catchInfer . withScale (if readCount == 1 then UOne else UMany) $ do
          actual <- checkAgainst ctx groupEnv expr expected
          _ <- atSpanOf expr (constrain ctx actual expected)
          zonk expected
      resolved <-
        case (directive, attempt) of
          (Nothing, Right ty) -> pure ty
          (Nothing, Left err) -> throwCheckError err
          (Just TynixIgnore, Right ty) -> pure ty
          (Just TynixIgnore, Left _) -> recoverSuppressedType ctx expected
          (Just TynixExpected, Left _) -> recoverSuppressedType ctx expected
          (Just TynixExpected, Right _) ->
            throwCheck (withCode TC0006UnusedExpectedDirective ("unused @tynix-expected directive on binding " <> quoteName name))
      pure (name, resolved)

    letItemAttrs = \case
      LetBinding name expr -> [AttrField name expr]
      LetPath steps expr -> [AttrPath steps expr]
      LetInherit (Just source) names -> [AttrInheritFrom source names]
      _ -> []
    letItemNames = \case
      LetBinding name _ -> [name]
      LetPath (SelectName name : _) _ -> [name]
      LetInherit _ names -> names
      _ -> []

-- | Infer a recursive attribute set (`rec { ... }`).
--
-- Fields may refer to one another, so — exactly like `let` — they are
-- inferred one dependency group at a time and generalized as soon as their
-- group is solved. A polymorphic field keeps its quantifiers in the record
-- type (`{ id :: forall t0. t0 -> t0; }`) and is instantiated afresh at each
-- selection, so `rec { id = x: x; a = id 1; b = id "s"; }` checks.
-- `inherit x;` resolves against the enclosing scope, not the rec scope.
inferRecAttrSet :: CheckContext -> TypeEnv -> [AttrItem] -> InferM Type
inferRecAttrSet ctx env rawItems = do
  (items, dynamicEntries) <- normalizeAttrItems rawItems
  let plainInherited = concat [names | AttrInherit names <- items]
      binds =
        [(name, expr) | AttrField name expr <- items]
          <> [(name, ESelect source [SelectName name]) | AttrInheritFrom source names <- items, name <- names]
      bindNames = map fst binds
  case duplicateNames (bindNames <> plainInherited) of
    dup : _ -> throwCheck (withCode TC0002DuplicateAttribute ("duplicate attribute: " <> quoteName dup))
    [] -> pure ()
  hideNames (bindNames <> plainInherited) $ do
    inherited <- forM plainInherited $ \name -> (,) name <$> (inferExpr ctx env (EVar name) >>= zonk)
    let inheritedEnv = Map.fromList [(name, Scheme [] ty) | (name, ty) <- inherited]
        bindMap = Map.fromList binds
        bindSet = Set.fromList bindNames
        groups =
          stronglyConnComp
            [(name, name, Set.toList (Set.intersection (exprFreeNames expr) bindSet)) | (name, expr) <- binds]
        inferGroup (currentEnv, acc) members = do
          placeholders <- Map.fromList <$> traverse (\name -> (,) name . Scheme [] <$> freshMeta) members
          let groupEnv = placeholders <> currentEnv
          results <- forM members $ \name -> do
            expr <- maybe (throwCheck (withCode TC0017MissingPlaceholder ("internal: missing rec field " <> show name))) pure (Map.lookup name bindMap)
            expected <- maybe (throwCheck (withCode TC0017MissingPlaceholder ("internal: missing placeholder for rec field " <> show name))) instantiate (Map.lookup name placeholders)
            actual <- checkAgainst ctx groupEnv expr expected
            _ <- atSpanOf expr (constrain ctx actual expected)
            (,) name <$> zonk expected
          generalized <- forM results $ \(name, ty) -> (,) name <$> generalize currentEnv ty
          pure (Map.fromList generalized <> currentEnv, acc <> generalized)
    (finalEnv, inferred) <- foldM inferGroup (inheritedEnv <> env, []) (map flattenSCC groups)
    let fields =
          Map.fromList
            ( [(name, schemeType' scheme) | (name, scheme) <- inferred]
                <> inherited
            )
    finishAttrSet ctx finalEnv dynamicEntries fields
  where
    schemeType' = \case
      Scheme [] ty -> ty
      Scheme vars ty -> TForall vars ty

-- | Bind the names introduced by a lambda pattern.
--
-- Attribute-set patterns produce a record argument type. Fields with a default
-- are checked against their (possibly annotated) field type; defaults may
-- refer to the other pattern names, as in Nix.
inferPatternBindings :: CheckContext -> TypeEnv -> Pattern -> InferM (Type, TypeEnv)
inferPatternBindings ctx env = \case
  PVar name ann -> do
    argTy <- maybe freshMeta pure ann
    pure (argTy, Map.singleton name (Scheme [] argTy))
  PAttrSet patternFields openPattern binder -> do
    let names = patternFieldNames patternFields <> maybe [] (pure . binderName) binder
        dups = duplicateNames names
    unless (null dups) (throwCheck (withCode TC0007DuplicatePatternBinding ("duplicate pattern bindings: " <> quoteNames dups)))
    fieldTys <- traverse (maybe freshSoftMeta pure . patternFieldType) patternFields
    rowTail <- freshMeta
    let markOptional field ty = if isJust (patternFieldDefault field) then TOptional ty else ty
        fields = Map.fromList [(patternFieldName field, markOptional field ty) | (field, ty) <- zip patternFields fieldTys]
        -- `...` admits further arguments, which the `@` binder may select.
        argTy = if openPattern then TOpenRecord fields rowTail else TRecord fields
        fieldEnv = Map.fromList (zip (patternFieldNames patternFields) (map (Scheme []) fieldTys))
        patternEnv = fieldEnv <> maybe Map.empty (\b -> Map.singleton (binderName b) (Scheme [] argTy)) binder
    forM_ (zip patternFields fieldTys) $ \(field, fieldTy) ->
      forM_ (patternFieldDefault field) $ \fallback -> do
        fallbackTy <- inferExpr ctx (patternEnv <> env) fallback
        -- `x ? null` is Nix's idiom for "optional": it says nothing about the
        -- type of a supplied value, so it does not constrain the field.
        unless (isNullLiteral fallback) $ do
          -- An unannotated defaulted argument takes the default's *widened*
          -- type: `b ? 2` accepts any Int, not just the literal 2.
          let target = if isJust (patternFieldType field) then fallbackTy else widenLiterals fallbackTy
          void (constrain ctx target fieldTy)
    pure (argTy, patternEnv)
  where
    binderName = \case
      BinderBefore name -> name
      BinderAfter name -> name

inferStaticSelect :: CheckContext -> Type -> Name -> InferM Type
inferStaticSelect ctx ty field =
  zonk ty >>= \resolvedTy ->
    let base' = resolveHead (checkAliases ctx) resolvedTy
     in case base' of
          -- Selecting from a not-yet-known value: record the requirement as an
          -- open row, so later selections extend it (row polymorphism).
          TMeta n -> do
            fieldTy <- freshSoftMeta
            rowTail <- freshMeta
            _ <- bindMeta n (TOpenRecord (Map.singleton field fieldTy) rowTail)
            pure fieldTy
          TOpenRecord fields (TMeta n)
            | not (Map.member field fields) -> do
                fieldTy <- freshSoftMeta
                rowTail <- freshMeta
                _ <- bindMeta n (TOpenRecord (Map.singleton field fieldTy) rowTail)
                pure fieldTy
          _ -> inferStaticSelectKnown ctx resolvedTy base' field

inferStaticSelectKnown :: CheckContext -> Type -> Type -> Name -> InferM Type
inferStaticSelectKnown ctx resolvedTy base' field
  | base' == tAny = pure tAny
  | base' == tDynamic = pure tDynamic
  | base' == tUnknown = throwCheck (withCode TC0008SelectOnUnknown ("cannot select field " <> quoteName field <> " from unknown"))
  | isOpaqueHead (checkAliases ctx) base' =
      throwCheck (withCode TC0027OpaqueType ("cannot select field " <> quoteName field <> " from opaque type " <> showType base' <> "; cast it to its representation with `as` first"))
  | otherwise =
      case lookupRecordField (checkAliases ctx) resolvedTy field of
        Just fieldTy -> instantiate (schemeFromAnnotation fieldTy)
        Nothing -> throwCheck (withCode TC0009MissingField ("missing field " <> quoteName field <> " on " <> showType resolvedTy))

inferDynamicSelect :: CheckContext -> Type -> Type -> InferM Type
inferDynamicSelect ctx baseTy keyTy = do
  resolvedBaseTy <- zonk baseTy
  resolvedKeyTy <- zonk keyTy
  let aliases = checkAliases ctx
      base' = resolveHead aliases resolvedBaseTy
      key' = resolveHead aliases resolvedKeyTy
  case () of
    _
      | base' == tAny || key' == tAny -> pure tAny
      | base' == tDynamic || key' == tDynamic || isMeta base' -> pure tDynamic
      | Just valueTy <- attrsOfView base' -> constrain ctx resolvedKeyTy tString $> valueTy
      | base' == tUnknown -> throwCheck (withCode TC0008SelectOnUnknown "cannot select dynamic field from unknown")
      | key' == tUnknown -> throwCheck (withCode TC0011DynamicKeyTypeMismatch "dynamic field selection expects a string-like key, but got unknown")
      | Just names <- selectionKeyNames key' ->
          case traverse (lookupRecordField aliases base') names of
            Just fieldTypes -> do
              instantiated <- traverse (instantiate . schemeFromAnnotation) fieldTypes
              case instantiated of
                x : xs -> pure (foldRight1 (joinTypes aliases) x xs)
                [] -> pure tDynamic
            Nothing -> throwCheck (withCode TC0010DynamicKeyMissingField ("missing field selected by dynamic key of type " <> showType key'))
      | isSubtype aliases key' tString || isConsistent aliases key' tString || isMeta key' -> pure tDynamic
      | otherwise -> throwCheck (withCode TC0012DynamicKeyNotStringLike ("dynamic field selection expects a string-like key, but got " <> showType key'))

selectionKeyNames :: Type -> Maybe [Name]
selectionKeyNames = \case
  TLit (LString name) -> Just [name]
  TUnion members -> concat <$> traverse selectionKeyNames members
  _ -> Nothing

-- * Dependent application

-- | The index a dependent arrow's binder stands for at one call: the
-- singleton of a dependent variable, or the argument's precise type when it
-- is fully known, and otherwise just the parameter type.
dependentIndex :: TypeEnv -> Expr -> Type -> Type -> InferM Type
dependentIndex env arg argTy domTy =
  case unloc arg of
    EVar name
      | Just (Scheme [] singleton@(TSingleton _ _)) <- Map.lookup name env -> pure singleton
    _ -> do
      argTy' <- zonk argTy
      pure $
        if Set.null (freeTypeMetas argTy') && not (hasDynamic argTy')
          then argTy'
          else domTy

-- * Linearity

inferRootExpression :: CheckContext -> TypeEnv -> Marked Expr -> InferM CheckResult
inferRootExpression ctx env (Marked directive expr) = do
  attempt <-
    catchInfer $
      case unloc expr of
        ELet items body -> do
          (env', bindings) <- inferLet ctx env items body
          ty <- inferExpr ctx env' body >>= zonk
          displayed <- traverse displayScheme bindings
          pure (CheckResult (Just (hideSingletonRows (closeMetas ty))) displayed)
        _ -> do
          ty <- inferExpr ctx env expr >>= zonk
          pure (CheckResult (Just (hideSingletonRows (closeMetas ty))) Map.empty)
  case (directive, attempt) of
    (Nothing, Right result) -> pure result
    (Nothing, Left err) -> throwCheckError err
    (Just TynixIgnore, Right result) -> pure result
    (Just TynixIgnore, Left _) -> pure (CheckResult (Just (Scheme [] tDynamic)) Map.empty)
    (Just TynixExpected, Left _) -> pure (CheckResult (Just (Scheme [] tDynamic)) Map.empty)
    (Just TynixExpected, Right _) -> throwCheck (withCode TC0006UnusedExpectedDirective "unused @tynix-expected directive on root expression")

recoverSuppressedType :: CheckContext -> Type -> InferM Type
recoverSuppressedType ctx expected = constrain ctx tDynamic expected *> zonk expected

-- | Infer the result type of a binary operator application.
--
-- Numeric `+` keeps its dedicated coercion rules; structural equality accepts
-- any operands and yields `Bool`; ordered comparisons require comparable
-- (numeric or string) operands; boolean connectives require `Bool` operands.
inferBinaryOp :: CheckContext -> TypeEnv -> BinOp -> Expr -> Expr -> InferM Type
inferBinaryOp ctx env op left right =
  case op of
    OpAdd -> inferArithmetic ctx env op left right
    OpSub -> inferArithmetic ctx env op left right
    OpMul -> inferArithmetic ctx env op left right
    OpConcat -> inferConcat ctx env left right
    OpUpdate -> inferUpdate ctx env left right
    OpEq -> inferEquality ctx env left right
    OpNeq -> inferEquality ctx env left right
    OpLt -> inferRelational ctx env left right
    OpGt -> inferRelational ctx env left right
    OpLe -> inferRelational ctx env left right
    OpGe -> inferRelational ctx env left right
    OpAnd -> inferLogical ctx env left right
    OpOr -> inferLogical ctx env left right
    OpImpl -> inferLogical ctx env left right
    OpDiv -> inferArithmetic ctx env op left right
    OpPipeRight -> inferExpr ctx env (EApp right left)
    OpPipeLeft -> inferExpr ctx env (EApp left right)

inferArithmetic :: CheckContext -> TypeEnv -> BinOp -> Expr -> Expr -> InferM Type
inferArithmetic ctx env op left right = do
  leftTy <- inferExpr ctx env left >>= zonk
  rightTy <- inferExpr ctx env right >>= zonk
  let aliases = checkAliases ctx
      leftResolved = resolveHead aliases leftTy
      rightResolved = resolveHead aliases rightTy
  if leftResolved == tAny || rightResolved == tAny
    then pure tAny
    else
      if leftResolved == tDynamic || rightResolved == tDynamic
        then pure tDynamic
        else case textConcatTarget aliases op leftResolved rightResolved of
          -- `+` also concatenates strings and paths, as in Nix.
          Just (leftExpected, rightExpected, result) -> do
            _ <- constrain ctx leftTy leftExpected
            _ <- constrain ctx rightTy rightExpected
            pure result
          Nothing -> do
            let expected = arithmeticTarget op leftResolved rightResolved
            _ <- constrain ctx leftTy expected
            _ <- constrain ctx rightTy expected
            zonk expected

-- | Decide whether `+` is string/path concatenation. The result follows the
-- left operand: `path + string` is a path, `string + path` a string.
textConcatTarget :: AliasEnv -> BinOp -> Type -> Type -> Maybe (Type, Type, Type)
textConcatTarget aliases op left right
  | op /= OpAdd = Nothing
  | isPathLike left, isTextLike right || isMeta right = Just (tPath, tString `orPath` right, tPath)
  | isStringLike left, isTextLike right || isMeta right = Just (tString, tString `orPath` right, tString)
  | isMeta left, isStringLike right = Just (tString, tString, tString)
  | isMeta left, isPathLike right = Just (tPath, tPath, tPath)
  | otherwise = Nothing
  where
    isStringLike ty = isSubtype aliases ty tString
    isPathLike ty = isSubtype aliases ty tPath
    isTextLike ty = isStringLike ty || isPathLike ty
    orPath base ty = if isPathLike ty then tPath else base

-- | Pick the numeric result family for an arithmetic operator. Subtraction can
-- yield negative results, so a `Nat`-only operand pair widens to `Int`.
arithmeticTarget :: BinOp -> Type -> Type -> Type
arithmeticTarget op left right =
  let base = additionTarget left right
   in if op == OpSub && base == tNat then tInt else base

-- | Structural equality (`==`/`!=`) accepts any pair of operands and always
-- produces `Bool`, mirroring Nix's value-level equality.
inferEquality :: CheckContext -> TypeEnv -> Expr -> Expr -> InferM Type
inferEquality ctx env left right = do
  _ <- inferExpr ctx env left
  _ <- inferExpr ctx env right
  pure tBool

-- | Ordered comparisons require comparable operands (numeric or string) on both
-- sides, unless a gradual boundary connects them. The result is always `Bool`.
inferRelational :: CheckContext -> TypeEnv -> Expr -> Expr -> InferM Type
inferRelational ctx env left right = do
  leftTy <- inferExpr ctx env left >>= zonk
  rightTy <- inferExpr ctx env right >>= zonk
  let aliases = checkAliases ctx
      leftResolved = resolveHead aliases leftTy
      rightResolved = resolveHead aliases rightTy
      gradual ty = ty == tAny || ty == tDynamic
      comparable ty = isSubtype aliases ty tNumber || isSubtype aliases ty tString || isSubtype aliases ty tPath
      comparisonBase ty
        | Just family <- numericFamily ty = widenSingleNumericFamily family
        | isSubtype aliases ty tNumber = tNumber
        | isSubtype aliases ty tPath = tPath
        | otherwise = tString
  if gradual leftResolved || gradual rightResolved || (comparable leftResolved && comparable rightResolved)
    then pure tBool
    else
      if isMeta leftResolved && comparable rightResolved
        then constrain ctx leftTy (comparisonBase rightResolved) $> tBool
        else
          if isMeta rightResolved && comparable leftResolved
            then constrain ctx rightTy (comparisonBase leftResolved) $> tBool
            else
              if hasUnresolvedMetas leftResolved rightResolved
                then pure tBool
                else
                  throwCheck
                    ( withCode
                        TC0019NotComparable
                        ("cannot compare " <> showType leftResolved <> " with " <> showType rightResolved)
                    )

-- | Boolean connectives (`&&`/`||`) require `Bool` operands and yield `Bool`.
inferLogical :: CheckContext -> TypeEnv -> Expr -> Expr -> InferM Type
inferLogical ctx env left right = do
  leftTy <- inferExpr ctx env left
  _ <- constrain ctx leftTy tBool
  rightTy <- inferExpr ctx env right
  _ <- constrain ctx rightTy tBool
  pure tBool

-- | List concatenation (`++`) requires list-like operands on both sides and
-- produces a plain @List@ whose element type joins the two element types.
-- Fixed-shape sequences (vectors, tuples) participate via their structural
-- list view; precise length information is intentionally not tracked here.
inferConcat :: CheckContext -> TypeEnv -> Expr -> Expr -> InferM Type
inferConcat ctx env left right = do
  leftTy <- inferExpr ctx env left >>= zonk
  rightTy <- inferExpr ctx env right >>= zonk
  let aliases = checkAliases ctx
      leftResolved = resolveHead aliases leftTy
      rightResolved = resolveHead aliases rightTy
  if leftResolved == tAny || rightResolved == tAny
    then pure tAny
    else
      if leftResolved == tDynamic || rightResolved == tDynamic
        then pure tDynamic
        else do
          left' <- listIfMeta leftResolved
          right' <- listIfMeta rightResolved
          case (listElementType left', listElementType right') of
            (Just leftElem, Just rightElem) -> do
              leftElem' <- zonk leftElem
              rightElem' <- zonk rightElem
              if hasUnresolvedMetas leftElem' rightElem'
                then tList <$> joinBranches ctx leftElem' rightElem'
                else pure (tList (joinTypes aliases leftElem' rightElem'))
            _ | hasUnresolvedMetas left' right' -> pure tDynamic
            _ ->
              throwCheck
                ( withCode
                    TC0020NotConcatenable
                    ("cannot concatenate " <> showType leftResolved <> " with " <> showType rightResolved)
                )

-- | Attribute-set update (`//`) merges two record types, with the right-hand
-- side overriding fields present on both. A gradual operand on either side
-- yields a gradual result; non-record operands raise TC0021.
inferUpdate :: CheckContext -> TypeEnv -> Expr -> Expr -> InferM Type
inferUpdate ctx env left right = do
  leftTy <- inferExpr ctx env left >>= zonk
  rightTy <- inferExpr ctx env right >>= zonk
  let aliases = checkAliases ctx
      leftResolved = resolveHead aliases leftTy
      rightResolved = resolveHead aliases rightTy
  if leftResolved == tAny || rightResolved == tAny
    then pure tAny
    else
      if leftResolved == tDynamic || rightResolved == tDynamic
        then pure tDynamic
        else do
          left' <- openIfMeta leftResolved
          right' <- openIfMeta rightResolved
          case (left', right') of
            _
              | Just (leftFields, leftTail) <- recordView left',
                Just (rightFields, rightTail) <- recordView right' ->
                  -- Right-hand fields win. An open right side may override any
                  -- left field with an unknown type, so its row stays open.
                  pure $ case (leftTail, rightTail) of
                    (Nothing, Nothing) -> TRecord (Map.union rightFields leftFields)
                    (_, Just tail') -> mkOpenRecord (Map.union rightFields leftFields) tail'
                    (Just tail', Nothing) -> mkOpenRecord (Map.union rightFields leftFields) tail'
              | Just leftValue <- attrsOfView left',
                Just rightValue <- attrsOfView right' ->
                  pure (tAttrsOf (joinTypes aliases leftValue rightValue))
              | Just leftValue <- attrsOfView left',
                Just (rightFields, _) <- recordView right' ->
                  pure (tAttrsOf (foldr (joinTypes aliases . unOptional) leftValue (Map.elems rightFields)))
              | Just (leftFields, _) <- recordView left',
                Just rightValue <- attrsOfView right' ->
                  pure (tAttrsOf (foldr (joinTypes aliases . unOptional) rightValue (Map.elems leftFields)))
              -- A shape that is still partly unknown (say, a union with an
              -- unsolved member) cannot be judged yet; stay gradual.
              | hasUnresolvedMetas left' right' -> pure tDynamic
            _ ->
              throwCheck
                ( withCode
                    TC0021NotUpdatable
                    ("cannot update " <> showType leftResolved <> " with " <> showType rightResolved)
                )

-- | A `++` operand whose type is still unknown must be a list.
listIfMeta :: Type -> InferM Type
listIfMeta = \case
  TMeta n -> do
    elemTy <- freshMeta
    bindMeta n (tList elemTy)
  other -> pure other

-- | An attribute-set operand whose type is still unknown becomes an open row,
-- so `//` can proceed and later uses refine it.
openIfMeta :: Type -> InferM Type
openIfMeta = \case
  TMeta n -> do
    rowTail <- freshMeta
    bindMeta n (TOpenRecord Map.empty rowTail)
  other -> pure other

-- | Extract the element type of a list-like type: a plain @List a@ directly,
-- or any fixed-shape sequence (vector, tuple) via its structural list view.
listElementType :: Type -> Maybe Type
listElementType ty =
  case plainListElement ty of
    Just elemTy -> Just elemTy
    Nothing -> sequenceListView ty >>= plainListElement
  where
    plainListElement candidate =
      case collectApps candidate of
        (TCon "List", [elemTy]) -> Just elemTy
        _ -> Nothing

additionTarget :: Type -> Type -> Type
additionTarget left right =
  case (numericFamily left, numericFamily right) of
    (Just leftBase, Just rightBase) -> joinNumericFamilies leftBase rightBase
    (Just knownBase, Nothing)
      | hasUnresolvedMetas left right -> widenSingleNumericFamily knownBase
    (Nothing, Just knownBase)
      | hasUnresolvedMetas left right -> widenSingleNumericFamily knownBase
    _ -> tNumber

numericFamily :: Type -> Maybe Type
numericFamily = \case
  ty
    | ty == tNat -> Just tNat
    | ty == tInt -> Just tInt
    | ty == tFloat -> Just tFloat
    | ty == tNumber -> Just tNumber
  TLit (LInt _) -> Just tInt
  TLit (LFloat _) -> Just tFloat
  _ -> Nothing

joinNumericFamilies :: Type -> Type -> Type
joinNumericFamilies left right
  | left == right = left
  | left == tNat, right == tInt = tInt
  | left == tInt, right == tNat = tInt
  | otherwise = tNumber

widenSingleNumericFamily :: Type -> Type
widenSingleNumericFamily ty
  | ty == tNat = tInt
  | ty == tInt = tInt
  | otherwise = tNumber

-- | Resolve an import path relative to the current source file.
--
-- Absolute paths are normalized but otherwise preserved. Relative paths are
-- interpreted against the directory containing the current file, mirroring how
-- Nix imports behave.
resolvePath :: FilePath -> FilePath -> FilePath
resolvePath from target
  | isAbsolute target = collapseParentSegments target
  | otherwise = collapseParentSegments (takeDirectory from </> target)

-- | Normalize `.` and `..` path segments without escaping an absolute root.
--
-- Segments accumulate in reverse so the most recent one is the head of the
-- list: dropping a segment for `..` is then O(1) instead of the O(n) reverse
-- and append the forward-ordered version needed on every step.
collapseParentSegments :: FilePath -> FilePath
collapseParentSegments = joinPath . reverse . foldl step [] . splitDirectories . normalise
  where
    step acc "." = acc
    step acc ".." =
      case acc of
        [] -> [".."]
        [root] | isAbsoluteRoot root -> acc
        _ : rest -> rest
    step acc part = part : acc
    isAbsoluteRoot part = part == "/"

-- | Reject duplicate declarations while keeping their diagnostic order stable.
rejectDuplicates :: DiagnosticCode -> String -> [Name] -> InferM ()
rejectDuplicates code message names =
  case duplicateNames names of
    [] -> pure ()
    duplicates -> throwCheck (withCode code (message <> quoteNames duplicates))

duplicateNames :: (Ord a) => [a] -> [a]
duplicateNames = foldr step [] . group . sort
  where
    step xs acc =
      case xs of
        first : _ | length xs > 1 -> first : acc
        _ -> acc

-- | Types that can never be applied as a function. Deliberately conservative:
-- only structurally-concrete non-functions (literals, records, lists, and the
-- base scalar constructors) are flagged. Gradual types (dynamic, any, unknown),
-- unresolved metas, and type applications fall through so the gradual boundary
-- and inference behavior are preserved.
definitelyNotCallable :: Type -> Bool
definitelyNotCallable ty = case ty of
  TLit _ -> True
  TRecord _ -> True
  TTypeList _ -> True
  TCon name -> name `elem` ["String", "Int", "Float", "Number", "Nat", "Bool", "Null", "Path"]
  _ -> False

-- | Human-readable noun phrase for a non-callable value, used in TC0018.
describeNonCallable :: Type -> String
describeNonCallable ty = case ty of
  TRecord _ -> "an attribute set"
  TTypeList _ -> "a list"
  TLit (LString _) -> "a string"
  TLit (LInt _) -> "an integer"
  TLit (LFloat _) -> "a float"
  TLit (LBool _) -> "a boolean"
  TCon name -> "a value of type " <> T.unpack name
  _ -> "a non-function value"
