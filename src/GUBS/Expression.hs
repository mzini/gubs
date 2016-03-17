module GUBS.Expression
       (
         Expression (..)
       , literal
       , constant
       , evalWithM
       , evalWith
       ) where

import Data.Functor.Identity (runIdentity)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data Expression l =
  Var l
  | Const Integer
  | Mult (Expression l) (Expression l)
  | Plus (Expression l) (Expression l)
  | Minus (Expression l) (Expression l)
  | Neg (Expression l)

literal :: l -> Expression l
literal = Var

constant :: Integer -> Expression l
constant = Const

ppBin :: PP.Pretty e => String -> e -> e -> PP.Doc
ppBin op e1 e2 = PP.parens (PP.pretty e1) PP.<+> PP.text op PP.<+> PP.parens (PP.pretty e2)

instance Show l => PP.Pretty (Expression l) where
  pretty (Var v) = PP.text (show v)
  pretty (Const i) = PP.text (show i)
  pretty (Mult a b) = ppBin "*" a b
  pretty (Plus a b) = ppBin "+" a b
  pretty (Minus a b) = ppBin "-" a b
  pretty (Neg a) = PP.text "-" PP.<+> PP.parens (PP.pretty a)
  
liftIntOp f _ (Const i) (Const j) = Const (f i j)
liftIntOp _ c e1 e2 = c e1 e2

plus :: Expression l -> Expression l -> Expression l
plus (Const 0) y = y
plus x (Const 0) = x
plus x y = liftIntOp (+) Plus x y

minus :: Expression l -> Expression l -> Expression l
minus x (Const 0) = x
minus x y = liftIntOp (-) Minus x y

mult :: Expression l -> Expression l -> Expression l
mult (Const 0) _ = Const 0
mult x (Const 0) = Const 0
mult (Const 1) y = y
mult x (Const 1) = x
mult x y = liftIntOp (*) Mult x y

neg :: Expression l -> Expression l
neg (Neg i) = i
neg (Const j) = Const (-j)
neg e = Neg e

instance Num (Expression l) where
  fromInteger = Const
  (+) = plus
  (-) = minus
  (*) = mult
  negate = neg
  abs = error "Expression: abs undefined"
  signum = error "Expression: signum undefined"

evalWithM :: Monad m => (l -> m Integer) -> Expression l -> m Integer
evalWithM getValue = eval where
 eval (Var v) = getValue v
 eval (Const i) = return i
 eval (Mult e1 e2) = (*) <$> eval e1 <*> eval e2
 eval (Plus e1 e2) = (+) <$> eval e1 <*> eval e2
 eval (Minus e1 e2) = (-) <$> eval e1 <*> eval e2
 eval (Neg e1) = negate <$> eval e1

evalWith :: (l -> Integer) -> Expression l -> Integer
evalWith getValue = runIdentity . evalWithM (return . getValue)


-- eval :: S.Solver s m => Expression l -> S.SolverM s m Integer
-- eval (Var v) = S.getValue v
-- eval (Const i) = return i
-- eval (Mult e1 e2) = (*) <$> eval e1 <*> eval e2
-- eval (Plus e1 e2) = (+) <$> eval e1 <*> eval e2
-- eval (Minus e1 e2) = (-) <$> eval e1 <*> eval e2
-- eval (Neg e1) = negate <$> eval e1

-- freshLiteral :: S.Solver s m => S.SolverM s m (Expression l)
-- freshLiteral = Var <$> S.fresh

-- assert :: S.Solver s m => ConstraintExp l -> S.SolverM s m ()
-- assert (GEQ e1 e2) = S.assert (toSolverExp e1 `S.geq` toSolverExp e2)
