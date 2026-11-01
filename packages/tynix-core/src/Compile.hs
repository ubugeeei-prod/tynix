-- | Erasure-based compiler from `.tynix` to `.nix`.
--
-- The compiler does not change runtime semantics. It simply removes type-only
-- constructs, leaving a Nix expression that stays close to the original source
-- layout.
module Compile (compileProgram) where

import Data.Text (Text)
import Pretty (renderProgramAsNix)
import Syntax

-- | Compile a checked or unchecked program by erasing type syntax first.
compileProgram :: Program -> Either Text Text
compileProgram = renderProgramAsNix . eraseProgram

eraseProgram :: Program -> Program
eraseProgram program =
  program
    { programExpr = fmap (\marked -> marked{markedValue = eraseExpr (markedValue marked)}) (programExpr program)
    }

eraseExpr :: Expr -> Expr
eraseExpr expr =
  case expr of
    ELambda pattern' body -> ELambda (erasePattern pattern') (eraseExpr body)
    EApp fun arg -> EApp (eraseExpr fun) (eraseExpr arg)
    EBinaryOp op left right -> EBinaryOp op (eraseExpr left) (eraseExpr right)
    EUnaryOp op operand -> EUnaryOp op (eraseExpr operand)
    ELet items body -> ELet [eraseMarkedLetItem item | item <- items, isLetBinding (markedValue item)] (eraseExpr body)
    EAttrSet items -> EAttrSet (map eraseAttrItem items)
    ERec items -> ERec (map eraseAttrItem items)
    ESelect base fields -> ESelect (eraseExpr base) (map eraseSelectStep fields)
    EHasAttr base path -> EHasAttr (eraseExpr base) (map eraseSelectStep path)
    EAssert cond body -> EAssert (eraseExpr cond) (eraseExpr body)
    EWith scope body -> EWith (eraseExpr scope) (eraseExpr body)
    EIf cond yesExpr noExpr -> EIf (eraseExpr cond) (eraseExpr yesExpr) (eraseExpr noExpr)
    EList members -> EList (map eraseExpr members)
    EInterp form parts -> EInterp form (map eraseStringPart parts)
    ECast inner _ -> eraseExpr inner
    EAscribe inner _ -> eraseExpr inner
    ESelectOr base fields fallback -> ESelectOr (eraseExpr base) (map eraseSelectStep fields) (eraseExpr fallback)
    EPathInterp parts -> EPathInterp (map eraseStringPart parts)
    ELoc span' inner -> ELoc span' (eraseExpr inner)
    other -> other

eraseStringPart :: StringPart -> StringPart
eraseStringPart (StrExpr expr) = StrExpr (eraseExpr expr)
eraseStringPart part = part

erasePattern :: Pattern -> Pattern
erasePattern (PVar name _) = PVar name Nothing
erasePattern (PAttrSet fields open binder) =
  PAttrSet
    [field{patternFieldType = Nothing, patternFieldDefault = eraseExpr <$> patternFieldDefault field} | field <- fields]
    open
    binder

eraseLetItem :: LetItem -> LetItem
eraseLetItem (LetBinding name expr) = LetBinding name (eraseExpr expr)
eraseLetItem (LetInherit source names) = LetInherit (eraseExpr <$> source) names
eraseLetItem (LetPath steps expr) = LetPath (map eraseSelectStep steps) (eraseExpr expr)
eraseLetItem item = item

eraseMarkedLetItem :: Marked LetItem -> Marked LetItem
eraseMarkedLetItem marked = marked{markedValue = eraseLetItem (markedValue marked)}

isLetBinding :: LetItem -> Bool
isLetBinding LetSignature{} = False
isLetBinding _ = True

eraseAttrItem :: AttrItem -> AttrItem
eraseAttrItem item =
  case item of
    AttrField name expr -> AttrField name (eraseExpr expr)
    AttrInherit names -> AttrInherit names
    AttrInheritFrom source names -> AttrInheritFrom (eraseExpr source) names
    AttrPath steps expr -> AttrPath (map eraseSelectStep steps) (eraseExpr expr)

eraseSelectStep :: SelectStep -> SelectStep
eraseSelectStep step =
  case step of
    SelectName name -> SelectName name
    SelectDynamic expr -> SelectDynamic (eraseExpr expr)
