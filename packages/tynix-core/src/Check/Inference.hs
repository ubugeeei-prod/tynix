{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Inference state, scheme generalization, and source-attributed failures.
module Check.Inference
  ( CheckContext (..),
    CheckError (..),
    CheckResult (..),
    Usage (..),
    InferM,
    TypeEnv,
    runInference,
    throwCheck,
    throwCheckError,
    withSpan,
    atSpanOf,
    catchInfer,
    freshMeta,
    freshSoftMeta,
    isSoftMeta,
    instantiate,
    instantiateRank,
    skolemize,
    generalize,
    zonk,
    zonkHead,
    boundMeta,
    bindMeta,
    replaceMetaSolution,
    displayScheme,
    hideSingletonRows,
    ambientEffects,
    withEffects,
    noteUse,
    withScale,
    inLambdaBody,
    trackPattern,
    hideNames,
    branchUsages,
  )
where

import Alias (AliasEnv)
import Check.Expression (patternBoundNames)
import Control.Monad (when)
import Control.Monad.State.Strict
  ( StateT (..),
    evalStateT,
    get,
    gets,
    lift,
    modify',
    put,
  )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Diagnostics (DiagnosticCode (..), withCode)
import Syntax
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

-- | Start an independent inference run.
runInference :: InferM a -> Either CheckError a
runInference action = evalStateT action initialInferState

-- | Propagate a failure without discarding its source span.
throwCheckError :: CheckError -> InferM a
throwCheckError = lift . Left

-- | Abort inference with a (coded) message. The span is attached by the
-- nearest enclosing 'ELoc' as the error propagates outward.
throwCheck :: String -> InferM a
throwCheck message = throwCheckError (CheckError message Nothing)

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

-- | Replace the quantified variables of a polymorphic expectation by fresh
-- rigid variables, so a lambda checked against `forall a. a -> a` must work
-- for an arbitrary `a` (and cannot confuse it with an `a` in scope).
skolemize :: Type -> InferM Type
skolemize = \case
  TForall vars body -> do
    n <- freshMetaId
    let rigid var = TVar (var <> "'" <> T.pack (show n))
    skolemize
      (substituteTypeVars (Map.fromList [(var, rigid var) | var <- vars]) body)
  other -> pure other

-- | Instantiate a result whose type is itself polymorphic (`Int -> forall a.
-- a -> a`), as a higher-rank signature may produce.
instantiateRank :: Type -> InferM Type
instantiateRank = \case
  TForall vars body -> instantiate (Scheme vars body)
  other -> pure other

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
      quantifiable = Set.toAscList (freeMetas zonked `Set.difference` envMetas)
      effectsOnly = effectOnlyMetas zonked
      taken = freeTypeVars zonked
      fresh prefix = freshTypeNames prefix taken
      typeMetas = filter (not . (`Set.member` effectsOnly)) quantifiable
      effectMetas = filter (`Set.member` effectsOnly) quantifiable
      renaming =
        Map.fromList
          (zip typeMetas (TVar <$> fresh "t") <> zip effectMetas (TVar <$> fresh "e"))
      names = take (length typeMetas) (fresh "t") <> take (length effectMetas) (fresh "e")
  pure (Scheme names (substituteMetas renaming zonked))

-- | Instantiate a polymorphic scheme by replacing quantified variables with
-- fresh inference metas.
instantiate :: Scheme -> InferM Type
instantiate (Scheme vars ty) = do
  reps <- traverse (const freshMeta) vars
  pure (substituteTypeVars (Map.fromList (zip vars reps)) ty)

-- | Allocate a fresh soft meta (see 'InferState').
freshSoftMeta :: InferM Type
freshSoftMeta = do
  meta <- freshMetaId
  modify' (\state -> state{softMetas = Set.insert meta (softMetas state)})
  pure (TMeta meta)

isSoftMeta :: Int -> InferM Bool
isSoftMeta n = gets (Set.member n . softMetas)

-- | Allocate a fresh inference meta variable.
freshMeta :: InferM Type
freshMeta = TMeta <$> freshMetaId

freshMetaId :: InferM Int
freshMetaId = do
  state <- get
  put state{nextMeta = nextMeta state + 1}
  pure (nextMeta state)

-- | Apply the current substitution set to a type.
--
-- This is the main "read your work back" operation of the inference engine:
-- callers use it after unification or binding a meta to recover the most
-- up-to-date structural view.
zonk :: Type -> InferM Type
zonk ty = substituteMetas <$> gets substitutions <*> pure ty

-- | Zonk a binding's scheme after the whole program has been solved and close
-- any metas that are still open, for stable user-facing output.
displayScheme :: Scheme -> InferM Scheme
displayScheme (Scheme vars ty) = do
  zonked0 <- zonk ty
  let zonked =
        substituteMetas
          (Map.fromSet (const pureEffects) (loneEffectMetas zonked0))
          zonked0
      metas = Set.toAscList (freeMetas zonked)
      taken = Set.fromList vars <> freeTypeVars zonked
      names = take (length metas) (freshTypeNames "t" taken)
      renaming = Map.fromList (zip metas (TVar <$> names))
      closed = Scheme (vars <> names) (substituteMetas renaming zonked)
  pure (hideSingletonRows closed)

-- | Stable names for type and effect metas, avoiding names already in scope.
freshTypeNames :: String -> Set.Set Name -> [Name]
freshTypeNames prefix taken = filter (`Set.notMember` taken) candidates
  where
    candidates = [T.pack (prefix <> show index) | index <- [0 :: Int ..]]

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

-- | Commit a successful attempt, or restore the complete state on failure.
catchInfer :: InferM a -> InferM (Either CheckError a)
catchInfer action = do
  snapshot <- get
  case runStateT action snapshot of
    Left err -> pure (Left err)
    Right (value, state') -> put state' >> pure (Right value)

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

-- | Bind one inference meta to a solved type, performing the occurs check.
bindMeta :: Int -> Type -> InferM Type
bindMeta n ty = do
  resolved <- zonk ty
  when
    (resolved == TMeta n || n `Set.member` freeMetas resolved)
    (throwCheck (withCode TC0016OccursCheckFailed "occurs check failed"))
  replaceMetaSolution n resolved
  pure resolved

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
      modify'
        ( \st' -> st'{usages = maybe (Map.delete name) (Map.insert name) saved (usages st')}
        )
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

-- | Store a non-recursive solution after an occurs check or a literal join.
replaceMetaSolution :: Int -> Type -> InferM ()
replaceMetaSolution meta ty =
  modify'
    (\state -> state{substitutions = Map.insert meta ty (substitutions state)})

-- | Effects admitted by the current computation.
ambientEffects :: InferM Type
ambientEffects = gets currentEffects

-- | Infer a nested computation with its own effect row.
withEffects :: Type -> InferM a -> InferM a
withEffects effects action = do
  outer <- gets currentEffects
  modify' (\state -> state{currentEffects = effects})
  result <- action
  modify' (\state -> state{currentEffects = outer})
  pure result

-- | Infer alternative branches from the same usage counts, then join them.
-- Type solutions and effects remain shared: only one branch consumes a binder,
-- but either branch may constrain its type or perform an effect.
branchUsages :: InferM a -> InferM b -> InferM (a, b)
branchUsages yes no = do
  before <- gets usages
  yesResult <- yes
  afterYes <- gets usages
  modify' (\state -> state{usages = before})
  noResult <- no
  afterNo <- gets usages
  let joined = Map.unionWith (\(depth, a) (_, b) -> (depth, joinUsage a b)) afterYes afterNo
  modify' (\state -> state{usages = joined})
  pure (yesResult, noResult)
