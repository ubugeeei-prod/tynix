{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Effect-row constraints, primitive effects, and handler scopes.
module Check.Effects
  ( effectfulArrow,
    impureValues,
    perform,
    constrainEffects,
    unifyEffects,
    handledEffects,
    handleEffects,
  )
where

import Check.Diagnostics
import Check.Expression (unloc)
import Check.Inference
import Control.Monad (unless, void, when)
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Diagnostics (DiagnosticCode (..), withCode)
import Syntax
import Type

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
    throwCheck
      ( withCode
          TC0024ImpureInPureEval
          "impure operation in pure evaluation: flakes cannot read the \
          \environment, the clock, or the host platform"
      )
  ambient <- ambientEffects
  subsumeEffects row' ambient

-- | Check a latent effect row against an expected one. Untracked rows on
-- either side are the gradual case.
constrainEffects :: Type -> Type -> InferM ()
constrainEffects actual expected = do
  actual' <- zonk actual
  expected' <- zonk expected
  unless
    (actual' == TDynamic || expected' == TDynamic)
    (subsumeEffects actual' expected')

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
            ( "effect `"
                <> T.unpack label
                <> "` is not allowed here: the expected effects are "
                <> showEffects other
            )
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
            ( "effects `"
                <> T.unpack var
                <> "` of a polymorphic callee are not allowed here: the expected effects are "
                <> showEffects other
            )
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
  inner <- freshMeta
  result <- withEffects inner action
  performed <- zonk inner
  let remaining = case performed of
        TRecord fields -> TRecord (foldr Map.delete fields handled)
        TOpenRecord fields tail' -> mkOpenRecord (foldr Map.delete fields handled) tail'
        other -> other
  perform ctx remaining
  pure result

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
