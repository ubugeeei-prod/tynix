{-# LANGUAGE LambdaCase #-}

-- | Syntactic dependency, free-variable, and binder-usage analysis.
module Check.Expression
  ( unloc,
    exprFreeNames,
    freeVariables,
    letBoundNames,
    usageCount,
    patternBoundNames,
  )
where

import Data.Set qualified as Set
import Syntax
import Type

-- | Strip the outermost location wrappers to inspect an expression's shape.
unloc :: Expr -> Expr
unloc = \case
  ELoc _ inner -> unloc inner
  other -> other

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
        (foldMap (goLet . markedValue) items <> go body)
          `Set.difference` Set.fromList (letBoundNames items)
      EAttrSet items -> foldMap goAttr items
      ERec items ->
        foldMap goAttr items `Set.difference` Set.fromList (concatMap attrNames items)
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
