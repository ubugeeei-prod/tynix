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
import Control.Applicative ((<|>))
import Control.Monad (foldM, forM, forM_, unless, void, when, zipWithM)
import Control.Monad.State.Strict
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
import Pretty (renderType)
import Subtyping
import Syntax
import System.FilePath (isAbsolute, joinPath, normalise, splitDirectories, takeDirectory, (</>))
import Type

-- | Inputs required to analyze one file.
--
-- `checkAmbient` represents the ambient world visible from the current file,
-- while `checkAliases` contains both local aliases and any imported declaration
-- aliases that were discovered by the driver.
data CheckContext = CheckContext
  { checkAliases :: AliasEnv,
    checkAmbient :: Map FilePath Scheme,
    checkFile :: FilePath,
    -- | True inside the body of a `with` whose scope type is not a known
    -- record, so unresolved names degrade to `dynamic` instead of erroring.
    checkOpenScope :: Bool,
    -- | Pure evaluation (flakes): performing the `Impure` effect is an error.
    checkPureEval :: Bool
  }

-- | User-visible results produced by checking a program.
--
-- The root scheme describes the file's resulting expression, while
-- `resultBindings` records the final types of `let`-bound names for CLI output
-- and LSP hover.
data CheckResult = CheckResult
  { resultRoot :: Maybe Scheme,
    resultBindings :: Map Name Scheme
  }
  deriving (Eq, Show)

-- | A checker failure: the coded message plus, when known, the innermost
-- source region whose inference raised it.
data CheckError = CheckError
  { checkErrorMessage :: String,
    checkErrorSpan :: Maybe SrcSpan
  }
  deriving (Eq, Show)

-- | Inference state: the meta supply, solved metas, and the set of "soft"
-- metas. A soft meta stands for an injected dependency whose real type is
-- unknown and usually polymorphic (an attrset-pattern argument such as
-- `fetchFromGitHub`, or a field selected from an unknown record such as
-- `lib.mkOption`). Calling one is gradual rather than pinning it to the
-- monotype of the first call site.
--
-- Besides solving types, inference tracks two facts about the computation it
-- is walking:
--
-- * 'currentEffects' is the effect row of the innermost enclosing lambda body
--   (or 'TDynamic' at the top level). Applying a function adds the callee's
--   latent effects to it, so a lambda's own latent effects are inferred from
--   its body.
-- * 'usages' counts how often each lambda binder is consumed, which decides
--   whether the lambda is linear (`%1 ->`) and lets a linear signature be
--   enforced. A use under a nested lambda, or as the argument of an
--   unrestricted function ('usageScale'), counts as many.
data InferState = InferState
  { nextMeta :: Int,
    substitutions :: Map Int Type,
    softMetas :: Set.Set Int,
    currentEffects :: Type,
    usages :: Map Name (Int, Usage),
    lambdaDepth :: Int,
    usageScale :: Usage
  }

-- | How many times a binder is consumed: never, exactly once, more than once,
-- or a different number of times on different branches.
data Usage = UZero | UOne | UMany | UMixed
  deriving (Eq, Show)

addUsage :: Usage -> Usage -> Usage
addUsage UZero u = u
addUsage u UZero = u
addUsage UMixed _ = UMixed
addUsage _ UMixed = UMixed
addUsage _ _ = UMany

joinUsage :: Usage -> Usage -> Usage
joinUsage a b
  | a == b = a
  | otherwise = UMixed

initialInferState :: InferState
initialInferState =
  InferState
    { nextMeta = 0,
      substitutions = Map.empty,
      softMetas = Set.empty,
      currentEffects = TDynamic,
      usages = Map.empty,
      lambdaDepth = 0,
      usageScale = UOne
    }

type InferM = StateT InferState (Either CheckError)

type TypeEnv = Map Name Scheme

-- | Abort inference with a (coded) message. The span is attached by the
-- nearest enclosing 'ELoc' as the error propagates outward.
throwCheck :: String -> InferM a
throwCheck message = lift (Left (CheckError message Nothing))

-- | Run an inference step, attributing any span-less failure to @region@.
withSpan :: SrcSpan -> InferM a -> InferM a
withSpan region action =
  StateT $ \st ->
    case runStateT action st of
      Left err@CheckError{checkErrorSpan = Nothing} -> Left err{checkErrorSpan = Just region}
      other -> other

-- | Attribute failures of @action@ to @expr@'s source span, if it has one.
atSpanOf :: Expr -> InferM a -> InferM a
atSpanOf = \case
  ELoc region _ -> withSpan region
  _ -> id

-- | Strip the outermost location wrappers to inspect an expression's shape.
unloc :: Expr -> Expr
unloc = \case
  ELoc _ inner -> unloc inner
  other -> other

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
  evalStateT (inferTop ctx (globalEnvironment ctx)) initialInferState
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
    if resolvedFunTy == tDynamic
      then pure tDynamic
      else
        if resolvedFunTy == tAny
          then pure tAny
          else case resolvedFunTy of
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
        | otherwise -> lift (Left err)
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
    before <- gets usages
    yesTy <- inferExpr ctx env yesExpr >>= zonk
    afterYes <- gets usages
    modify' (\st -> st{usages = before})
    noTy <- inferExpr ctx env noExpr >>= zonk
    afterNo <- gets usages
    modify' (\st -> st{usages = Map.unionWith (\(depth, a) (_, b) -> (depth, joinUsage a b)) afterYes afterNo})
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

-- | Replace singleton literal types by their primitive base.
widenLiterals :: Type -> Type
widenLiterals = \case
  TLit (LInt _) -> tInt
  TLit (LFloat _) -> tFloat
  TLit (LString _) -> tString
  TLit (LBool _) -> tBool
  TRecord fields -> TRecord (fmap widenLiterals fields)
  TOpenRecord fields tail' -> TOpenRecord (fmap widenLiterals fields) tail'
  TOptional inner -> TOptional (widenLiterals inner)
  TArrow arrow a b -> TArrow arrow (widenLiterals a) (widenLiterals b)
  TApp f x -> TApp f (widenLiterals x)
  TUnion members -> case nubOrdTypes (map widenLiterals members) of
    [single] -> single
    many' -> TUnion many'
  other -> other
  where
    nubOrdTypes = Set.toList . Set.fromList

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

-- | Replace the quantified variables of a polymorphic expectation by fresh
-- rigid variables, so a lambda checked against `forall a. a -> a` must work
-- for an arbitrary `a` (and cannot confuse it with an `a` in scope).
skolemize :: Type -> InferM Type
skolemize = \case
  TForall vars body -> do
    n <- gets nextMeta
    modify' (\st -> st{nextMeta = nextMeta st + 1})
    let rigid var = TVar (var <> "'" <> T.pack (show n))
    skolemize (substituteTypeVars (Map.fromList [(var, rigid var) | var <- vars]) body)
  other -> pure other

-- | Whether a type is an application of a built-in type-level operator.
isOperatorApp :: Type -> Bool
isOperatorApp ty =
  case collectApps ty of
    (TCon name, _ : _) -> name `elem` ["Get", "KeyOf", "Add", "Sub", "Mul", "Length"]
    _ -> False

-- | Instantiate a result whose type is itself polymorphic (`Int -> forall a.
-- a -> a`), as a higher-rank signature may produce.
instantiateRank :: Type -> InferM Type
instantiateRank = \case
  TForall vars body -> instantiate (Scheme vars body)
  other -> pure other

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
  outerEffects <- gets currentEffects
  latent <- freshMeta
  modify' (\st -> st{currentEffects = latent})
  (bodyTy, usage) <-
    trackPattern pattern' . inLambdaBody $
      case bodyTarget of
        Just codTy -> checkAgainst ctx bodyEnv body codTy
        Nothing -> inferExpr ctx bodyEnv body
  modify' (\st -> st{currentEffects = outerEffects})
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
  let sigs = Map.fromList [(name, schemeFromAnnotation ty) | Marked _ (LetSignature name ty) <- items]
      itemDirectives =
        Map.fromListWith
          (\_ earlier -> earlier)
          [(name, directive) | Marked (Just directive) item <- items, name <- letItemNames item]
      plainInherits = concat [names | Marked _ (LetInherit Nothing names) <- items]
      attrItems = concatMap (letItemAttrs . markedValue) items
  let simpleNames = [name | Marked _ (LetBinding name _) <- items] <> concat [names | Marked _ (LetInherit _ names) <- items]
  unless (null (duplicateNames simpleNames)) (throwCheck (withCode TC0004DuplicateBinding ("duplicate bindings: " <> quoteNames (duplicateNames simpleNames))))
  (normalized, dynamicEntries) <- normalizeAttrItems attrItems
  unless (null dynamicEntries) (throwCheck (withCode TC0022DynamicLetBinding "dynamic attributes are not allowed in let"))
  let binds =
        [(name, expr, Map.lookup name itemDirectives) | AttrField name expr <- normalized]
          <> [(name, ESelect source [SelectName name], Map.lookup name itemDirectives) | AttrInheritFrom source names <- normalized, name <- names]
      bindNames = [name | (name, _, _) <- binds]
      allNames = bindNames <> plainInherits
      missing = filter (`notElem` allNames) (Map.keys sigs)
      duplicateSigs = duplicateNames [name | Marked _ (LetSignature name _) <- items]
      duplicateBinds = duplicateNames (bindNames <> plainInherits)
      inheritedScheme name =
        case Map.lookup name env of
          Just scheme -> pure (name, scheme)
          Nothing
            | checkOpenScope ctx -> pure (name, Scheme [] tDynamic)
            | otherwise -> throwCheck (withCode TC0001UnboundName ("unbound name: " <> quoteName name))
  unless (null duplicateSigs) (throwCheck (withCode TC0003DuplicateSignature ("duplicate signatures: " <> quoteNames duplicateSigs)))
  unless (null duplicateBinds) (throwCheck (withCode TC0004DuplicateBinding ("duplicate bindings: " <> quoteNames duplicateBinds)))
  unless (null missing) (throwCheck (withCode TC0005MissingBindingForSignature ("missing bindings for signatures: " <> quoteNames missing)))
  inherited <- Map.fromList <$> traverse inheritedScheme plainInherits
  -- Signed bindings are known up front (enabling polymorphic recursion);
  -- unsigned ones are inferred one dependency group at a time, in
  -- topological order, and generalized as soon as their group is solved.
  -- That is what gives `let` Hindley-Milner polymorphism: a helper used at
  -- two different types later in the same `let` is instantiated freshly.
  let signedEnv = Map.restrictKeys sigs (Set.fromList bindNames)
      baseEnv = signedEnv <> inherited <> env
      bindMap = Map.fromList [(name, (expr, directive)) | (name, expr, directive) <- binds]
      unsigned = Set.fromList [name | name <- bindNames, not (Map.member name sigs)]
      groups =
        stronglyConnComp
          [ (name, name, Set.toList (Set.intersection (exprFreeNames expr) (Set.fromList bindNames)))
          | (name, expr, _) <- binds
          ]
  (finalEnv, inferredList) <- foldM (inferGroup unsigned bindMap) (baseEnv, []) (map flattenSCC groups)
  let finals = Map.fromList inferredList <> inherited
  pure (finals <> finalEnv, finals)
  where
    inferGroup unsigned bindMap (currentEnv, acc) members = do
      placeholders <-
        Map.fromList
          <$> traverse (\name -> (,) name . Scheme [] <$> freshMeta) (filter (`Set.member` unsigned) members)
      let groupEnv = placeholders <> currentEnv
      results <- forM members $ \name -> do
        (expr, inlineDirective) <-
          maybe (throwCheck (withCode TC0017MissingPlaceholder ("internal: missing binding " <> show name))) pure (Map.lookup name bindMap)
        -- A signed binding is checked against its signature with the
        -- quantified variables held *rigid* (skolemized): `forall a. a -> a`
        -- must work for every `a`, so a body returning `1` is rejected.
        -- Other bindings still use the signature polymorphically.
        expected <- case (Map.lookup name (signaturesOf items), Map.lookup name groupEnv) of
          (Just (Scheme _ signatureTy), _) -> pure signatureTy
          (Nothing, Just scheme) -> instantiate scheme
          (Nothing, Nothing) -> throwCheck (withCode TC0017MissingPlaceholder ("internal: missing placeholder for binding " <> show name))
        let directive = inlineDirective <|> Map.lookup name (sigDirectivesOf items)
        -- A binding read more than once duplicates whatever it consumed, so
        -- (for linearity) its right-hand side counts as used many times.
        let readCount = usageCount name letBody + sum [usageCount name other | (otherName, (other, _)) <- Map.toList bindMap, otherName /= name]
        attempt <-
          catchInfer . withScale (if readCount == 1 then UOne else UMany) $ do
            actual <- checkAgainst ctx groupEnv expr expected
            _ <- atSpanOf expr (constrain ctx actual expected)
            zonk expected
        resolved <-
          case (directive, attempt) of
            (Nothing, Right ty) -> pure ty
            (Nothing, Left err) -> lift (Left err)
            (Just TynixIgnore, Right ty) -> pure ty
            (Just TynixIgnore, Left _) -> recoverSuppressedType ctx expected
            (Just TynixExpected, Left _) -> recoverSuppressedType ctx expected
            (Just TynixExpected, Right _) ->
              throwCheck (withCode TC0006UnusedExpectedDirective ("unused @tynix-expected directive on binding " <> quoteName name))
        pure (name, resolved)
      -- Generalize against the environment *outside* this group, so metas
      -- still shared with enclosing lambdas stay monomorphic.
      generalized <- forM results $ \(name, ty) ->
        case Map.lookup name (signaturesOf items) of
          Just scheme -> pure (name, scheme)
          Nothing -> (,) name <$> generalize currentEnv ty
      pure (Map.fromList generalized <> currentEnv, acc <> generalized)
    sigDirectivesOf xs = Map.fromList [(name, directive) | Marked (Just directive) (LetSignature name _) <- xs]
    signaturesOf xs = Map.fromList [(name, schemeFromAnnotation ty) | Marked _ (LetSignature name ty) <- xs]
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

isMeta :: Type -> Bool
isMeta = \case
  TMeta _ -> True
  _ -> False

selectionKeyNames :: Type -> Maybe [Name]
selectionKeyNames = \case
  TLit (LString name) -> Just [name]
  TUnion members -> concat <$> traverse selectionKeyNames members
  _ -> Nothing

-- | Generalize a type over the metas that do not occur free in @env@.
--
-- This is the Hindley-Milner `gen` step. Metas that also appear in the
-- environment belong to an enclosing binder (a lambda parameter, say) and must
-- stay shared, so only the remaining ones become quantified variables.
generalize :: TypeEnv -> Type -> InferM Scheme
generalize env ty = do
  subst <- gets substitutions
  let zonked0 = substituteMetas subst ty
      envMetas = foldMap (freeMetas . substituteMetas subst . schemeType) (Map.elems env)
      -- A latent effect variable nobody else mentions means "pure".
      lone = loneEffectMetas zonked0 `Set.difference` envMetas
  let zonked = substituteMetas (Map.fromSet (const pureEffects) lone) zonked0
      quantifiable = sort (Set.toList (freeMetas zonked `Set.difference` envMetas))
      effectsOnly = effectOnlyMetas zonked
      taken = freeTypeVars zonked
      fresh prefix = [name | i <- [0 :: Int ..], let name = T.pack (prefix <> show i), not (Set.member name taken)]
      typeMetas = filter (not . (`Set.member` effectsOnly)) quantifiable
      effectMetas = filter (`Set.member` effectsOnly) quantifiable
      renaming = Map.fromList (zip typeMetas (TVar <$> fresh "t") <> zip effectMetas (TVar <$> fresh "e"))
      names = take (length typeMetas) (fresh "t") <> take (length effectMetas) (fresh "e")
  pure (Scheme names (substituteMetas renaming zonked))

-- | Names a binding's expression refers to (an over-approximation that
-- ignores shadowing), used to order `let` bindings by dependency.
exprFreeNames :: Expr -> Set.Set Name
exprFreeNames = go
  where
    go = \case
      EVar name -> Set.singleton name
      ELoc _ inner -> go inner
      ELambda pat body -> goPat pat <> go body
      EApp f x -> go f <> go x
      EBinaryOp _ l r -> go l <> go r
      EUnaryOp _ x -> go x
      ELet items body -> foldMap (goLet . markedValue) items <> go body
      EAttrSet items -> foldMap goAttr items
      ERec items -> foldMap goAttr items
      ESelect base steps -> go base <> foldMap goStep steps
      ESelectOr base steps def -> go base <> foldMap goStep steps <> go def
      EHasAttr base steps -> go base <> foldMap goStep steps
      EAssert c b -> go c <> go b
      EWith sc b -> go sc <> go b
      EIf c a b -> go c <> go a <> go b
      EList xs -> foldMap go xs
      ECast e _ -> go e
      EAscribe e _ -> go e
      EInterp _ parts -> foldMap goPart parts
      EPathInterp parts -> foldMap goPart parts
      _ -> Set.empty
    goPart = \case
      StrExpr e -> go e
      _ -> Set.empty
    goStep = \case
      SelectDynamic e -> go e
      SelectName _ -> Set.empty
    goPat = \case
      PAttrSet fields _ _ -> foldMap (foldMap go . patternFieldDefault) fields
      PVar _ _ -> Set.empty
    goLet = \case
      LetBinding _ e -> go e
      LetInherit (Just src) _ -> go src
      LetInherit Nothing names -> Set.fromList names
      LetPath steps e -> foldMap goStep steps <> go e
      LetSignature _ _ -> Set.empty
    goAttr = \case
      AttrField _ e -> go e
      AttrInherit names -> Set.fromList names
      AttrInheritFrom src _ -> go src
      AttrPath steps e -> foldMap goStep steps <> go e

-- * Effects

-- | An arrow whose latent effect row is exactly these labels.
effectfulArrow :: [Name] -> Arrow
effectfulArrow labels = (plainArrow Many){arrowEffects = effectRow labels Nothing}

-- | `builtins` values (not functions) whose mere evaluation is impure.
impureValues :: [Name]
impureValues = ["currentTime", "currentSystem", "nixPath"]

-- | Record that the current computation performs an effect row: a callee's
-- latent effects, or a primitive effect such as a failed `assert`.
perform :: CheckContext -> Type -> InferM ()
perform ctx row = do
  row' <- zonk row
  when (checkPureEval ctx && "Impure" `elem` effectLabels row') $
    throwCheck (withCode TC0024ImpureInPureEval "impure operation in pure evaluation: flakes cannot read the environment, the clock, or the host platform")
  ambient <- gets currentEffects
  subsumeEffects row' ambient

-- | Check a latent effect row against an expected one. Untracked rows on
-- either side are the gradual case.
constrainEffects :: Type -> Type -> InferM ()
constrainEffects actual expected = do
  actual' <- zonk actual
  expected' <- zonk expected
  unless (actual' == TDynamic || expected' == TDynamic) (subsumeEffects actual' expected')

-- | Make @target@ admit every effect of @actual@: each label must be present
-- (an open tail is extended), and an effect variable of @actual@ must be the
-- target's own. An unsolved actual tail is unified with the target, which is
-- how a call to an unknown function inherits the effects of its context.
subsumeEffects :: Type -> Type -> InferM ()
subsumeEffects actual target = do
  actual' <- zonk actual
  case actual' of
    TDynamic -> pure ()
    TRecord fields -> mapM_ (requireLabel target) (Map.keys fields)
    TOpenRecord fields tail' -> mapM_ (requireLabel target) (Map.keys fields) *> requireTail tail'
    other -> requireTail other
  where
    requireTail = \case
      TMeta m -> do
        target' <- zonk target
        unless (Set.member m (freeMetas target')) (void (bindMeta m target'))
      TVar var -> requireVar var target
      _ -> pure ()

requireLabel :: Type -> Name -> InferM ()
requireLabel row label =
  zonk row >>= \case
    TDynamic -> pure ()
    TMeta n -> do
      rest <- freshMeta
      void (bindMeta n (effectRow [label] (Just rest)))
    TOpenRecord fields rest
      | Map.member label fields -> pure ()
      | otherwise -> requireLabel rest label
    TRecord fields
      | Map.member label fields -> pure ()
    other ->
      throwCheck
        ( withCode
            TC0023EffectNotAllowed
            ("effect `" <> T.unpack label <> "` is not allowed here: the expected effects are " <> showEffects other)
        )

requireVar :: Name -> Type -> InferM ()
requireVar var row =
  zonk row >>= \case
    TDynamic -> pure ()
    TMeta n -> void (bindMeta n (TVar var))
    TOpenRecord _ rest -> requireVar var rest
    TVar other | other == var -> pure ()
    other ->
      throwCheck
        ( withCode
            TC0023EffectNotAllowed
            ("effects `" <> T.unpack var <> "` of a polymorphic callee are not allowed here: the expected effects are " <> showEffects other)
        )

showEffects :: Type -> String
showEffects = \case
  TRecord fields
    | Map.null fields -> "{} (pure)"
    | otherwise -> labels fields ""
  TOpenRecord fields (TVar var) -> labels fields (" | " <> T.unpack var)
  TOpenRecord fields _ -> labels fields ", ..."
  TVar var -> T.unpack var
  other -> showType other
  where
    labels fields rest = "{" <> intercalate ", " (map T.unpack (Map.keys fields)) <> rest <> "}"

-- | Effects a handler discharges from its argument: `builtins.tryEval`
-- catches `throw` and failed assertions.
handledEffects :: Expr -> [Name]
handledEffects fun =
  case unloc fun of
    ESelect base [SelectName "tryEval"]
      | EVar "builtins" <- unloc base -> ["Throw"]
    _ -> []

-- | Run @action@ in its own effect scope, then perform what it performed
-- minus the @handled@ labels.
handleEffects :: CheckContext -> [Name] -> InferM a -> InferM a
handleEffects _ [] action = action
handleEffects ctx handled action = do
  outer <- gets currentEffects
  inner <- freshMeta
  modify' (\st -> st{currentEffects = inner})
  result <- action
  modify' (\st -> st{currentEffects = outer})
  performed <- zonk inner
  let remaining = case performed of
        TRecord fields -> TRecord (foldr Map.delete fields handled)
        TOpenRecord fields tail' -> mkOpenRecord (foldr Map.delete fields handled) tail'
        other -> other
  perform ctx remaining
  pure result

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

-- | Count one consumption of @name@ if it is a tracked lambda binder.
noteUse :: Name -> InferM ()
noteUse name =
  modify' $ \st ->
    case Map.lookup name (usages st) of
      Just (depth, usage) ->
        let amount = if lambdaDepth st > depth then UMany else usageScale st
         in st{usages = Map.insert name (depth, addUsage usage amount) (usages st)}
      Nothing -> st

-- | Scale the consumptions inside @action@: under an unrestricted function
-- every use counts as many.
withScale :: Usage -> InferM a -> InferM a
withScale scale action = do
  outer <- gets usageScale
  let scaled = if outer == UMany || scale == UMany then UMany else UOne
  modify' (\st -> st{usageScale = scaled})
  result <- action
  modify' (\st -> st{usageScale = outer})
  pure result

-- | Enter a lambda body: one level deeper, consumed once per call.
inLambdaBody :: InferM a -> InferM a
inLambdaBody action = do
  st <- get
  put st{lambdaDepth = lambdaDepth st + 1, usageScale = UOne}
  result <- action
  modify' (\st' -> st'{lambdaDepth = lambdaDepth st, usageScale = usageScale st})
  pure result

-- | Track the binder of a lambda pattern while its body is inferred, and
-- report how it was consumed. Attribute-set patterns are never linear.
trackPattern :: Pattern -> InferM a -> InferM (a, Usage)
trackPattern pattern' action =
  case pattern' of
    PVar name _ -> do
      st <- get
      let saved = Map.lookup name (usages st)
      put st{usages = Map.insert name (lambdaDepth st + 1, UZero) (usages st)}
      result <- action
      usage <- gets (maybe UZero snd . Map.lookup name . usages)
      modify' (\st' -> st'{usages = maybe (Map.delete name) (Map.insert name) saved (usages st')})
      pure (result, usage)
    _ -> (,UMany) <$> hideNames (patternBoundNames pattern') action

-- | Stop tracking names that a binding form shadows.
hideNames :: [Name] -> InferM a -> InferM a
hideNames names action = do
  saved <- gets (\st -> Map.restrictKeys (usages st) (Set.fromList names))
  if Map.null saved
    then action
    else do
      modify' (\st -> st{usages = Map.withoutKeys (usages st) (Set.fromList names)})
      result <- action
      modify' (\st -> st{usages = Map.union saved (usages st)})
      pure result

-- | Names a `let` block binds.
letBoundNames :: [Marked LetItem] -> [Name]
letBoundNames = concatMap (names . markedValue)
  where
    names = \case
      LetBinding name _ -> [name]
      LetPath (SelectName name : _) _ -> [name]
      LetInherit _ inherited -> inherited
      _ -> []

-- | Free variables of an expression, respecting lambda, `let`, and `rec`
-- scoping (unlike 'exprFreeNames', which over-approximates).
freeVariables :: Expr -> Set.Set Name
freeVariables = go
  where
    go = \case
      EVar name -> Set.singleton name
      ELoc _ inner -> go inner
      ELambda pat body ->
        (goPat pat <> go body) `Set.difference` Set.fromList (patternBoundNames pat)
      EApp f x -> go f <> go x
      EBinaryOp _ l r -> go l <> go r
      EUnaryOp _ x -> go x
      ELet items body ->
        (foldMap (goLet . markedValue) items <> go body) `Set.difference` Set.fromList (letBoundNames items)
      EAttrSet items -> foldMap goAttr items
      ERec items -> foldMap goAttr items `Set.difference` Set.fromList (concatMap attrNames items)
      ESelect base steps -> go base <> foldMap goStep steps
      ESelectOr base steps def -> go base <> foldMap goStep steps <> go def
      EHasAttr base steps -> go base <> foldMap goStep steps
      EAssert c b -> go c <> go b
      EWith sc b -> go sc <> go b
      EIf c a b -> go c <> go a <> go b
      EList xs -> foldMap go xs
      ECast e _ -> go e
      EAscribe e _ -> go e
      EInterp _ parts -> foldMap goPart parts
      EPathInterp parts -> foldMap goPart parts
      _ -> Set.empty
    goPart = \case
      StrExpr e -> go e
      _ -> Set.empty
    goStep = \case
      SelectDynamic e -> go e
      SelectName _ -> Set.empty
    goPat = \case
      PAttrSet fields _ _ -> foldMap (foldMap go . patternFieldDefault) fields
      PVar _ _ -> Set.empty
    goLet = \case
      LetBinding _ e -> go e
      LetInherit (Just src) _ -> go src
      LetInherit Nothing names -> Set.fromList names
      LetPath steps e -> foldMap goStep steps <> go e
      LetSignature _ _ -> Set.empty
    goAttr = \case
      AttrField _ e -> go e
      AttrInherit names -> Set.fromList names
      AttrInheritFrom src _ -> go src
      AttrPath steps e -> foldMap goStep steps <> go e
    attrNames = \case
      AttrField name _ -> [name]
      AttrPath (SelectName name : _) _ -> [name]
      AttrInheritFrom _ names -> names
      _ -> []

-- | Instantiate a polymorphic scheme by replacing quantified variables with
-- fresh inference metas.
instantiate :: Scheme -> InferM Type
instantiate (Scheme vars ty) = do
  reps <- traverse (const freshMeta) vars
  pure (substituteTypeVars (Map.fromList (zip vars reps)) ty)

-- | Allocate a fresh soft meta (see 'InferState').
freshSoftMeta :: InferM Type
freshSoftMeta = do
  meta <- freshMeta
  case meta of
    TMeta n -> modify' (\st -> st{softMetas = Set.insert n (softMetas st)})
    _ -> pure ()
  pure meta

isSoftMeta :: Int -> InferM Bool
isSoftMeta n = gets (Set.member n . softMetas)

-- | Allocate a fresh inference meta variable.
freshMeta :: InferM Type
freshMeta = do
  st <- get
  put st{nextMeta = nextMeta st + 1}
  pure (TMeta (nextMeta st))

-- | Apply the current substitution set to a type.
--
-- This is the main "read your work back" operation of the inference engine:
-- callers use it after unification or binding a meta to recover the most
-- up-to-date structural view.
zonk :: Type -> InferM Type
zonk ty = substituteMetas <$> gets substitutions <*> pure ty

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
    (Nothing, Left err) -> lift (Left err)
    (Just TynixIgnore, Right result) -> pure result
    (Just TynixIgnore, Left _) -> pure (CheckResult (Just (Scheme [] tDynamic)) Map.empty)
    (Just TynixExpected, Left _) -> pure (CheckResult (Just (Scheme [] tDynamic)) Map.empty)
    (Just TynixExpected, Right _) -> throwCheck (withCode TC0006UnusedExpectedDirective "unused @tynix-expected directive on root expression")

-- | Zonk a binding's scheme after the whole program has been solved and close
-- any metas that are still open, for stable user-facing output.
displayScheme :: Scheme -> InferM Scheme
displayScheme (Scheme vars ty) = do
  zonked0 <- zonk ty
  let zonked = substituteMetas (Map.fromSet (const pureEffects) (loneEffectMetas zonked0)) zonked0
      metas = sort (Set.toList (freeMetas zonked))
      taken = Set.fromList vars <> freeTypeVars zonked
      names = take (length metas) [name | i <- [0 :: Int ..], let name = T.pack ("t" <> show i), not (Set.member name taken)]
  pure (hideSingletonRows (Scheme (vars <> names) (substituteMetas (Map.fromList (zip metas (TVar <$> names))) zonked)))

-- | A row variable that occurs only once in a scheme carries no information
-- ("some further fields"), so it is displayed as a plain `...`.
hideSingletonRows :: Scheme -> Scheme
hideSingletonRows (Scheme vars ty) =
  let counts = occurrences ty
      single name = Map.findWithDefault 0 name counts == (1 :: Int)
      hide = \case
        TOpenRecord fields (TVar name) | single name -> TOpenRecord (fmap hide fields) TDynamic
        TOpenRecord fields tail' -> TOpenRecord (fmap hide fields) (hide tail')
        TRecord fields -> TRecord (fmap hide fields)
        TArrow arrow a b -> TArrow arrow (hide a) (hide b)
        TApp f x -> TApp (hide f) (hide x)
        TUnion members -> TUnion (map hide members)
        TOptional inner -> TOptional (hide inner)
        TTypeList items -> TTypeList (map hide items)
        other -> other
      hidden = hide ty
   in Scheme (filter (`Set.member` freeTypeVars hidden) vars) hidden
  where
    occurrences = \case
      TVar name -> Map.singleton name 1
      TOpenRecord fields tail' -> Map.unionsWith (+) (occurrences tail' : map occurrences (Map.elems fields))
      TRecord fields -> Map.unionsWith (+) (map occurrences (Map.elems fields))
      TFun _ a b -> Map.unionWith (+) (occurrences a) (occurrences b)
      TApp f x -> Map.unionWith (+) (occurrences f) (occurrences x)
      TUnion members -> Map.unionsWith (+) (map occurrences members)
      TOptional inner -> occurrences inner
      TTypeList items -> Map.unionsWith (+) (map occurrences items)
      _ -> Map.empty

catchInfer :: InferM a -> InferM (Either CheckError a)
catchInfer action = do
  snapshot <- get
  case runStateT action snapshot of
    Left err -> pure (Left err)
    Right (value, state') -> put state' >> pure (Right value)

recoverSuppressedType :: CheckContext -> Type -> InferM Type
recoverSuppressedType ctx expected = constrain ctx tDynamic expected *> zonk expected

-- | Check that an inferred type satisfies an expected type.
--
-- Compared with `unify`, `constrain` is intentionally directional. It is used
-- for user annotations and function arguments where one side represents an
-- obligation rather than an unknown peer.
--
-- A few policy choices are important here:
--
-- * exact subtyping succeeds immediately,
-- * gradual consistency is only accepted when `dynamic` participates,
-- * plain concrete mismatches do /not/ fall through to permissive unification,
-- * sequence types may compare through their structural `List` view when one
--   side explicitly asks for `List`.
--
-- Representative examples:
--
-- @
-- constrain (Vec 2 Int) (List Int)
--   => succeeds through the structural list view
--
-- constrain (Vec 3 Int) (Vec (2 | Range 4 8 Nat) Int)
--   => fails
--
-- constrain dynamic String
--   => succeeds, because the mismatch is genuinely gradual
-- @
constrain :: CheckContext -> Type -> Type -> InferM Type
constrain ctx actualRaw expected = do
  -- A polymorphic value may be used at any instance (a `rec` field such as
  -- `id :: forall t0. t0 -> t0` meeting `Int -> Int`).
  actual <- zonkHead actualRaw >>= instantiateRank
  decomposed <- constrainStructurally ctx actual expected
  case decomposed of
    Just result -> pure result
    Nothing -> constrainResolved ctx actual expected

-- | Decompose a constraint along the *unzonked* structure of the expected
-- type, so a meta inside it (say the `a` of `List a`) is still identifiable
-- when it is reached:
--
-- * `List`/`AttrsOf` are covariant: `List x <: List y` is `x <: y`, and an
--   exact sequence (`Vec`/`Tuple`) meets `List y` through its element join;
-- * a union on the left must fit member-wise, one on the right is tried
--   member by member while metas remain;
-- * a meta already solved to a literal (from an earlier argument) widens to
--   the join when another literal of the same family arrives, so
--   `max 1 2` and `elem 1 [ 1 2 ]` check.
constrainStructurally :: CheckContext -> Type -> Type -> InferM (Maybe Type)
constrainStructurally ctx actual expected = do
  let aliases = checkAliases ctx
  actualHead <- normalizeIndexedType . resolveHead aliases <$> zonkHead actual
  expectedHead <- resolveHead aliases <$> zonkHead expected
  bound <- boundMeta expected
  actualFull <- zonk actual
  case (actualHead, expectedHead) of
    _
      | Just (n, solved) <- bound,
        literalish solved,
        literalish actualFull,
        sameLiteralFamily solved actualFull,
        not (isSubtype aliases actualFull solved) -> do
          let joined = joinTypes aliases solved actualFull
          modify' (\st -> st{substitutions = Map.insert n joined (substitutions st)})
          pure (Just joined)
    (TApp (TCon c) x, TApp (TCon c') y)
      | c == c',
        c `elem` ["List", "AttrsOf"] ->
          Just . TApp (TCon c) <$> constrain ctx x y
    (_, TApp (TCon "List") y)
      | Just listTy <- sequenceListView actualHead,
        TApp (TCon "List") x <- listTy ->
          Just . tList <$> constrain ctx x y
    -- (An unsolved meta on the right simply takes the whole union.)
    (TUnion members, _)
      | not (isMeta expectedHead),
        hasUnresolvedMetas actualFull expectedHead || hasMetaInside expected -> do
          forM_ members (\member -> constrain ctx member expected)
          Just <$> zonk expected
    (_, TUnion members)
      | hasUnresolvedMetas actualFull actualFull -> firstSuccess members
    _ -> pure Nothing
  where
    firstSuccess [] = pure Nothing
    firstSuccess (member : rest) = do
      attempt <- catchInfer (constrain ctx actual member)
      case attempt of
        Right _ -> Just <$> zonk expected
        Left _ -> firstSuccess rest
    hasMetaInside ty = not (Set.null (freeTypeMetas ty))

-- | Chase a chain of solved metas at the top of a type only, leaving the
-- children (and any metas inside them) untouched.
zonkHead :: Type -> InferM Type
zonkHead = \case
  TMeta n -> gets (Map.lookup n . substitutions) >>= maybe (pure (TMeta n)) zonkHead
  other -> pure other

-- | If a type is a meta (chain) that is already solved, the meta holding the
-- solution and the solution itself.
boundMeta :: Type -> InferM (Maybe (Int, Type))
boundMeta = \case
  TMeta n -> do
    solved <- gets (Map.lookup n . substitutions)
    case solved of
      Just next@(TMeta _) -> boundMeta next
      Just ty -> Just . (,) n <$> zonk ty
      Nothing -> pure Nothing
  _ -> pure Nothing

-- | Literal singletons and unions of them.
literalish :: Type -> Bool
literalish = \case
  TLit _ -> True
  TUnion members -> all literalish members
  _ -> False

sameLiteralFamily :: Type -> Type -> Bool
sameLiteralFamily a b = widenLiterals a == widenLiterals b

constrainResolved :: CheckContext -> Type -> Type -> InferM Type
constrainResolved ctx actual expected = do
  -- Aliases are expanded lazily, so resolve each side's head before
  -- comparing structure (`Mapper a b` must meet `Int -> Int` as a function).
  actualZ <- zonk actual
  expectedZ <- zonk expected
  let actual' = normalizeIndexedType (resolveHead (checkAliases ctx) actualZ)
      expected' = normalizeIndexedType (resolveHead (checkAliases ctx) expectedZ)
  case (actual', expected') of
    _
      | Just actualList <- sequenceListView actual',
        isPlainListType expected' ->
          constrain ctx actualList expected'
      | isPlainListType actual',
        Just expectedList <- sequenceListView expected' ->
          constrain ctx actual' expectedList
    (TMeta n, TMeta m) | n == m -> pure actual'
    -- Passing a still-unknown value where *anything* (or any attribute set)
    -- is accepted must not pin it to that top type: `builtins.hasAttr k x`
    -- says nothing about the rest of `x`.
    (TMeta _, TUnknown) -> pure expected'
    (TMeta n, TApp (TCon "AttrsOf") TUnknown) -> do
      rowTail <- freshMeta
      _ <- bindMeta n (TOpenRecord Map.empty rowTail)
      pure expected'
    -- Metas are solved to the unresolved type so alias names survive.
    (TMeta n, _) -> bindMeta n expectedZ
    (_, TMeta n) -> bindMeta n actualZ
    (TTypeList xs, TTypeList ys)
      | length xs == length ys ->
          TTypeList <$> zipWithM (constrain ctx) xs ys
    (TArrow actualArrow actualArg actualResult, TArrow expectedArrow expectedArg expectedResult)
      | multiplicitySubtype (arrowMult actualArrow) (arrowMult expectedArrow) -> do
          constrainEffects (arrowEffects actualArrow) (arrowEffects expectedArrow)
          unless (capturesWithin (arrowCaptures actualArrow) (arrowCaptures expectedArrow)) $
            throwCheck (withCode TC0025CaptureNotAllowed ("closure captures more than its expected type allows: " <> showType actual' <> " vs " <> showType expected'))
          _ <- constrain ctx expectedArg actualArg
          -- Dependent binders on either side denote the same argument.
          let shared = TSingleton "it" expectedArg
          _ <- constrain ctx (eraseBinder actualArrow shared actualResult) (eraseBinder expectedArrow shared expectedResult)
          zonk expected'
      | otherwise ->
          throwCheck (withCode TC0026LinearityViolation ("expected a linear function (`%1 ->`), but got an unrestricted one: " <> showType actual'))
    (TOptional a, TOptional b) -> TOptional <$> constrain ctx a b
    _
      | hasUnresolvedMetas actual' expected',
        Just (actualFields, actualTail) <- recordView (resolveHead (checkAliases ctx) actual'),
        Just (expectedFields, expectedTail) <- recordView (resolveHead (checkAliases ctx) expected') -> do
          constrainRecord ctx actualFields actualTail expectedFields
          -- An open expected row (`{ ...r }` from a signature) captures the
          -- fields the expectation did not mention, so `r` is solved.
          -- When both sides already share that row (a record meeting
          -- itself through an instantiated signature), there is nothing to
          -- capture, and binding it would only build an infinite row.
          case expectedTail of
            Just (TMeta n)
              | actualTail /= Just (TMeta n) -> do
                  let extra = Map.difference actualFields expectedFields
                  _ <- bindMeta n (maybe (TRecord extra) (mkOpenRecord extra) actualTail)
                  pure ()
            _ -> pure ()
          zonk expected'
      | hasUnresolvedMetas actual' expected',
        Just (actualFields, _) <- recordView (resolveHead (checkAliases ctx) actual'),
        Just valueTy <- attrsOfView (resolveHead (checkAliases ctx) expected') ->
          -- A record used as a dictionary: its values must fit together, so
          -- their join (not the first field) determines the value type.
          case map unOptional (Map.elems actualFields) of
            [] -> pure expected'
            x : xs -> do
              first <- zonk x
              rest <- traverse zonk xs
              let joined = foldRight1 (joinTypes (checkAliases ctx)) first rest
              constrain ctx (widenLiterals joined) valueTy $> expected'
    _ | actual' == expected' -> pure expected'
    _ | isSubtype (checkAliases ctx) actual' expected' -> pure expected'
    _ | allowsGradualConsistency actual' expected' && isConsistent (checkAliases ctx) actual' expected' -> pure expected'
    _ | hasUnresolvedMetas actual' expected' -> unify ctx actual' expected'
    _
      | Just detail <- recordMismatchDetail (checkAliases ctx) actual' expected' -> throwCheck detail
    _ -> throwCheck (withCode TC0013TypeMismatch ("type mismatch: " <> showType actual' <> " vs " <> showType expected'))

-- | Explain why one record does not satisfy another: the first missing
-- required field, or the first field whose type does not fit.
recordMismatchDetail :: AliasEnv -> Type -> Type -> Maybe String
recordMismatchDetail aliases actual expected = do
  (actualFields, actualTail) <- recordView (resolveHead aliases actual)
  (expectedFields, _) <- recordView (resolveHead aliases expected)
  let open = actualTail == Just tDynamic || actualTail == Just tAny
      missing =
        [ name
        | (name, ty) <- Map.toList expectedFields,
          not (isOptionalField ty),
          not (Map.member name actualFields),
          not open
        ]
      wrong =
        [ (name, unOptional a, unOptional e)
        | (name, e) <- Map.toList expectedFields,
          Just a <- [Map.lookup name actualFields],
          not (isSubtype aliases (unOptional a) (unOptional e))
        ]
  case (missing, wrong) of
    (name : _, _) ->
      Just (withCode TC0009MissingField ("missing field " <> quoteName name <> ": expected " <> showType expected <> " but got " <> showType actual))
    ([], (name, a, e) : _) ->
      Just (withCode TC0013TypeMismatch ("type mismatch in field " <> quoteName name <> ": " <> showType a <> " vs " <> showType e))
    _ -> Nothing
  where
    isOptionalField = \case
      TOptional _ -> True
      _ -> False

-- | Width-subtyping obligation between record shapes that still contain
-- metas: every expected field must be provided (unless optional). A missing
-- field extends the actual row when its tail is still open.
constrainRecord :: CheckContext -> Map Name Type -> Maybe Type -> Map Name Type -> InferM ()
constrainRecord ctx actualFields actualTail expectedFields =
  forM_ (Map.toList expectedFields) $ \(name, expectedTy) ->
    case Map.lookup name actualFields of
      Just actualTy -> void (constrain ctx (unOptional actualTy) (unOptional expectedTy))
      Nothing ->
        case expectedTy of
          TOptional _ -> pure ()
          _ -> do
            tail' <- traverse zonk actualTail
            case tail' of
              Just (TMeta n) -> do
                rowTail <- freshMeta
                void (bindMeta n (TOpenRecord (Map.singleton name expectedTy) rowTail))
              Just ty | ty == tDynamic || ty == tAny -> pure ()
              _ -> throwCheck (withCode TC0009MissingField ("missing field " <> quoteName name <> " required by " <> showType (TRecord expectedFields)))

-- | Symmetric structural unification used while solving metas.
--
-- Unlike `constrain`, both sides are treated as peers here. When metas are
-- present the function may bind them, recursively unify structured types, or
-- join gradually consistent shapes when `dynamic` is involved.
--
-- Representative examples:
--
-- @
-- unify ?0 Int
--   => binds ?0 := Int
--
-- unify (List ?0) (List String)
--   => binds ?0 := String
--
-- unify Int String
--   => fails
-- @
unify :: CheckContext -> Type -> Type -> InferM Type
unify ctx left right = do
  left' <- normalizeIndexedType . resolveHead (checkAliases ctx) <$> zonk left
  right' <- normalizeIndexedType . resolveHead (checkAliases ctx) <$> zonk right
  case (left', right') of
    _
      | Just leftList <- sequenceListView left',
        isPlainListType right' ->
          unify ctx leftList right'
      | isPlainListType left',
        Just rightList <- sequenceListView right' ->
          unify ctx left' rightList
    (TMeta n, TMeta m) | n == m -> pure left'
    (TMeta n, ty) -> bindMeta n ty
    (ty, TMeta n) -> bindMeta n ty
    (TTypeList xs, TTypeList ys)
      | length xs == length ys ->
          TTypeList <$> zipWithM (unify ctx) xs ys
    -- Two exact sequence shapes that disagree (a 1-element and a 3-element
    -- list literal, say) meet at their common `List` view.
    _
      | Just leftList <- sequenceListView left',
        Just rightList <- sequenceListView right',
        hasUnresolvedMetas left' right' || not (isSubtype (checkAliases ctx) left' right' || isSubtype (checkAliases ctx) right' left') ->
          unify ctx leftList rightList
    (TArrow leftArrow a b, TArrow rightArrow c d)
      | arrowMult leftArrow == arrowMult rightArrow -> do
          effects <- unifyEffects (arrowEffects leftArrow) (arrowEffects rightArrow)
          dom <- unify ctx a c
          cod <- unify ctx (eraseBinder leftArrow dom b) (eraseBinder rightArrow dom d)
          pure (TArrow leftArrow{arrowEffects = effects, arrowBinder = Nothing} dom cod)
    (TRecord a, TRecord b) -> unifyRecord a b
    _
      | Just (a, ta) <- recordView left',
        Just (b, tb) <- recordView right',
        isJust ta || isJust tb ->
          unifyRows left' right' a ta b tb
    (TOptional a, TOptional b) -> TOptional <$> unify ctx a b
    (TApp f x, TApp g y) -> TApp <$> unify ctx f g <*> unify ctx x y
    _ | left' == right' -> pure left'
    _ | isSubtype (checkAliases ctx) left' right' -> pure right'
    _ | isSubtype (checkAliases ctx) right' left' -> pure left'
    _ | allowsGradualConsistency left' right' && isConsistent (checkAliases ctx) left' right' -> pure (joinTypes (checkAliases ctx) left' right')
    _ -> throwCheck (withCode TC0013TypeMismatch ("type mismatch: " <> showType left' <> " vs " <> showType right'))
  where
    -- Rows unify field-wise on their common labels; each side's tail absorbs
    -- the labels only the other side has, sharing one fresh rest-row.
    unifyRows leftTy rightTy a ta b tb = do
      sequence_ (Map.intersectionWith (unify ctx) a b)
      let onlyA = Map.difference a b
          onlyB = Map.difference b a
      case (ta, tb) of
        (Nothing, Just tailB)
          | Map.null onlyB -> unify ctx tailB (TRecord onlyA) *> zonk leftTy
        (Just tailA, Nothing)
          | Map.null onlyA -> unify ctx tailA (TRecord onlyB) *> zonk rightTy
        (Just tailA, Just tailB) -> do
          rest <- freshMeta
          _ <- unify ctx tailA (mkOpenRecord onlyB rest)
          _ <- unify ctx tailB (mkOpenRecord onlyA rest)
          zonk leftTy
        _ -> throwCheck (withCode TC0014RecordMismatch ("record mismatch: " <> showType leftTy <> " vs " <> showType rightTy))
    unifyRecord a b
      | Map.keysSet b `Set.isSubsetOf` Map.keysSet a =
          Map.traverseWithKey (\name bTy -> maybe (pure bTy) (\aTy -> unify ctx aTy bTy) (Map.lookup name a)) b
            <&> TRecord
      | Map.keysSet a `Set.isSubsetOf` Map.keysSet b =
          Map.traverseWithKey (\name aTy -> maybe (pure aTy) (unify ctx aTy) (Map.lookup name b)) a
            <&> TRecord
      | otherwise = throwCheck (withCode TC0014RecordMismatch ("record mismatch: " <> showRecord a <> " vs " <> showRecord b))

-- | Unify two latent effect rows; an untracked row defers to the other.
unifyEffects :: Type -> Type -> InferM Type
unifyEffects left right = do
  left' <- zonk left
  right' <- zonk right
  case (left', right') of
    (TDynamic, _) -> pure right'
    (_, TDynamic) -> pure left'
    _ -> do
      subsumeEffects left' right'
      subsumeEffects right' left'
      zonk left'

-- | Bind one inference meta to a solved type, performing the occurs check.
bindMeta :: Int -> Type -> InferM Type
bindMeta n ty = do
  resolved <- zonk ty
  when (resolved == TMeta n || n `Set.member` freeMetas resolved) (throwCheck (withCode TC0016OccursCheckFailed "occurs check failed"))
  modify' (\st -> st{substitutions = Map.insert n resolved (substitutions st)})
  pure resolved

-- | Validate an explicit `expr as Type` assertion.
--
-- Casts deliberately live between plain assignment and fully-unsound escape
-- hatches. They are accepted when the two sides already overlap structurally,
-- when a gradual boundary such as `any`, `unknown`, or `dynamic` connects
-- them, or when the cast still contains unresolved inference metas that can be
-- solved by unification.
--
-- Representative examples:
--
-- @
-- { value = 1; } as { value :: Int; }
--   => accepted
--
-- import ./opaque.nix as { value :: String; }
--   => accepted when the import is `dynamic`
--
-- 1 as String
--   => rejected
-- @
checkCast :: CheckContext -> Type -> Type -> InferM Type
checkCast ctx actual expected = do
  -- Aliases are expanded lazily, so resolve each side's head before
  -- comparing structure (`Mapper a b` must meet `Int -> Int` as a function).
  actual' <- normalizeIndexedType . resolveHead (checkAliases ctx) <$> zonk actual
  expected' <- normalizeIndexedType . resolveHead (checkAliases ctx) <$> zonk expected
  let aliases = checkAliases ctx
  -- An opaque type is converted to and from its representation only here.
  let reveal ty = fromMaybe ty (revealOpaque aliases ty)
      related a b = isSubtype aliases a b || isSubtype aliases b a || isConsistent aliases a b
  if hasUnresolvedMetas actual' expected'
    then unify ctx (reveal actual') (reveal expected') $> expected
    else
      if related actual' expected'
        || related (reveal actual') expected'
        || related actual' (reveal expected')
        then pure expected
        else throwCheck (withCode TC0015InvalidCast ("invalid cast: " <> showType actual' <> " as " <> showType expected'))

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

duplicateNames :: (Ord a) => [a] -> [a]
duplicateNames = foldr step [] . group . sort
  where
    step xs acc =
      case xs of
        first : _ | length xs > 1 -> first : acc
        _ -> acc

-- | Render a name inside backticks so diagnostics stay readable.
quoteName :: Name -> String
quoteName name = "`" <> T.unpack name <> "`"

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

-- | Render a list of names as a comma-separated, backtick-quoted sequence.
quoteNames :: [Name] -> String
quoteNames = intercalate ", " . map quoteName

-- | Render a type using the surface syntax produced by 'Pretty'.
showType :: Type -> String
showType = T.unpack . T.unwords . map T.strip . T.lines . renderType

-- | Render a record's field map as the equivalent record type.
showRecord :: Map Name Type -> String
showRecord = showType . TRecord

-- | Count syntactic occurrences of a binder in an expression.
--
-- Shadowing stops the walk for the shadowed name, and recursive `let`
-- definitions are treated conservatively by not counting occurrences through a
-- re-bound name.
usageCount :: Name -> Expr -> Int
usageCount target = go
  where
    go = \case
      EVar name
        | name == target -> 1
        | otherwise -> 0
      EString _ -> 0
      EInterp _ parts -> sum [go expr | StrExpr expr <- parts]
      EFloat _ -> 0
      EInt _ -> 0
      EBool _ -> 0
      ENull -> 0
      EPath _ -> 0
      ESearchPath _ -> 0
      EPathInterp parts -> sum [go expr | StrExpr expr <- parts]
      ELoc _ inner -> go inner
      ESelectOr base steps fallback -> go base + sum (map selectStepCount steps) + go fallback
      ELambda pattern' body
        | target `elem` patternBoundNames pattern' -> 0
        | otherwise -> go body
      EApp fun arg -> go fun + go arg
      EBinaryOp _ left right -> go left + go right
      EUnaryOp _ operand -> go operand
      ELet items body ->
        let names = concatMap (letBoundNames' . markedValue) items
         in if target `elem` names
              then 0
              else sum (map (letItemCount . markedValue) items) + go body
      EAttrSet items -> sum (map attrItemCount items)
      ERec items ->
        if target `elem` concatMap attrBoundNames items
          then 0
          else sum (map attrItemCount items)
      ESelect base steps -> go base + sum (map selectStepCount steps)
      EHasAttr base steps -> go base + sum (map selectStepCount steps)
      EIf cond yesExpr noExpr -> go cond + max (go yesExpr) (go noExpr)
      EAssert cond body -> go cond + go body
      EWith scope body -> go scope + go body
      EList items -> sum (map go items)
      ECast expr _ -> go expr
      EAscribe expr _ -> go expr
    letItemCount = \case
      LetSignature _ _ -> 0
      LetBinding _ expr -> go expr
      LetInherit source names -> maybe (length (filter (== target) names)) go source
      LetPath steps expr -> sum (map selectStepCount steps) + go expr
    attrItemCount = \case
      AttrField _ expr -> go expr
      AttrInherit names -> length (filter (== target) names)
      AttrInheritFrom source _ -> go source
      AttrPath steps expr -> sum (map selectStepCount steps) + go expr
    letBoundNames' = \case
      LetBinding name _ -> [name]
      LetPath (SelectName name : _) _ -> [name]
      LetInherit _ names -> names
      _ -> []
    attrBoundNames = \case
      AttrField name _ -> [name]
      AttrPath (SelectName name : _) _ -> [name]
      AttrInherit names -> names
      AttrInheritFrom _ names -> names
      _ -> []
    selectStepCount = \case
      SelectName _ -> 0
      SelectDynamic expr -> go expr

patternBoundNames :: Pattern -> [Name]
patternBoundNames = \case
  PVar name _ -> [name]
  PAttrSet fields _ binder -> patternFieldNames fields <> maybe [] (pure . binderName) binder
  where
    binderName = \case
      BinderBefore name -> name
      BinderAfter name -> name

multiplicitySubtype :: Multiplicity -> Multiplicity -> Bool
multiplicitySubtype actual expected =
  actual == expected
    || case (actual, expected) of
      (One, Many) -> True
      _ -> False

-- | Widen a precise tuple/tensor into its structural list view when possible.
--
-- This is the bridge that lets exact sequence types interact with explicit
-- `List` annotations and ambient declarations.
sequenceListView :: Type -> Maybe Type
sequenceListView ty =
  case tensorListView ty of
    Just listTy -> Just listTy
    Nothing -> tupleListView ty

-- | Recognize the plain built-in `List a` shape.
isPlainListType :: Type -> Bool
isPlainListType ty =
  case collectApps ty of
    (TCon "List", [_]) -> True
    _ -> False

-- | Decide whether a consistency-based escape hatch is allowed.
--
-- Gradual consistency exists to smooth interop with genuinely dynamic values,
-- not to silently blur two unrelated concrete types.
allowsGradualConsistency :: Type -> Type -> Bool
allowsGradualConsistency left right = hasDynamic left || hasDynamic right

-- | Check whether either side still contains unsolved metas.
hasUnresolvedMetas :: Type -> Type -> Bool
hasUnresolvedMetas left right = not (Set.null (freeTypeMetas left <> freeTypeMetas right))

-- | Detect whether a type tree mentions `dynamic` anywhere inside it.
hasDynamic :: Type -> Bool
hasDynamic = \case
  TDynamic -> True
  TTypeList items -> any hasDynamic items
  TFun _ left right -> hasDynamic left || hasDynamic right
  TRecord fields -> any hasDynamic fields
  TOpenRecord fields tail' -> any hasDynamic fields || hasDynamic tail'
  TOptional inner -> hasDynamic inner
  TUnion members -> any hasDynamic members
  TApp fun arg -> hasDynamic fun || hasDynamic arg
  TForall _ body -> hasDynamic body
  TConditional actual patternTy yesTy noTy ->
    any hasDynamic [actual, patternTy, yesTy, noTy]
  _ -> False
