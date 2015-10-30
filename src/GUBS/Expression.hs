module GUBS.Expression
       (
         Expression
       , ConstraintExp
       , S.SolverM
       , S.Literal
       , S.Solver
       , freshLiteral
       , geq
       , eval
       , assert
       , S.checkSat
       ) where

import qualified Text.PrettyPrint.ANSI.Leijen as PP -- TODO
import qualified GUBS.Solver.Class as S

data Expression s =
  Var (S.Literal s)
  | Const Integer
  | Mult (Expression s) (Expression s)
  | Plus (Expression s) (Expression s)
  | Minus (Expression s) (Expression s)
  | Neg (Expression s)

data ConstraintExp s = GEQ (Expression s) (Expression s)

geq :: Expression s -> Expression s -> ConstraintExp s
geq = GEQ

-- TOOD
ppBin op e1 e2 = PP.parens (PP.pretty e1) PP.<+> PP.text op PP.<+> PP.parens (PP.pretty e2)

instance S.SMTSolver s => PP.Pretty (Expression s) where
  pretty (Var v) = PP.text (show v)
  pretty (Const i) = PP.text (show i)
  pretty (Mult a b) = ppBin "*" a b
  pretty (Plus a b) = ppBin "+" a b
  pretty (Minus a b) = ppBin "-" a b
  pretty (Neg a) = PP.text "-" PP.<+> PP.parens (PP.pretty a)
  
liftIntOp f _ (Const i) (Const j) = Const (f i j)
liftIntOp _ c e1 e2 = c e1 e2

plus :: Expression s -> Expression s -> Expression s
plus (Const 0) y = y
plus x (Const 0) = x
plus x y = liftIntOp (+) Plus x y

minus :: Expression s -> Expression s -> Expression s
minus x (Const 0) = x
minus x y = liftIntOp (-) Minus x y

mult :: Expression s -> Expression s -> Expression s
mult (Const 0) _ = Const 0
mult x (Const 0) = Const 0
mult (Const 1) y = y
mult x (Const 1) = x
mult x y = liftIntOp (*) Mult x y

neg :: Expression s -> Expression s
neg (Neg i) = i
neg (Const j) = Const (-j)
neg e = Neg e

instance Num (Expression s) where
  fromInteger x = Const x
  (+) = plus
  (-) = minus
  (*) = mult
  negate = neg
  abs = error "Expression: abs undefined"
  signum = error "Expression: signum undefined"

eval :: S.Solver s m => Expression s -> S.SolverM s m Integer
eval (Var v) = S.getValue v
eval (Const i) = return i
eval (Mult e1 e2) = (*) <$> eval e1 <*> eval e2
eval (Plus e1 e2) = (+) <$> eval e1 <*> eval e2
eval (Minus e1 e2) = (-) <$> eval e1 <*> eval e2
eval (Neg e1) = negate <$> eval e1

freshLiteral :: S.Solver s m => S.SolverM s m (Expression s)
freshLiteral = Var <$> S.fresh

assert :: S.Solver s m => ConstraintExp s -> S.SolverM s m ()
assert (GEQ e1 e2) = S.assert (toSolverExp e1 `S.geq` toSolverExp e2)

toSolverExp :: S.SMTSolver s => Expression s -> S.Expression s
toSolverExp (Var v) = S.literal v
toSolverExp (Const i) = S.constant i
toSolverExp (Mult e1 e2) = toSolverExp e1 * toSolverExp e2
toSolverExp (Plus e1 e2) = toSolverExp e1 + toSolverExp e2
toSolverExp (Minus e1 e2) = toSolverExp e1 - toSolverExp e2
toSolverExp (Neg e) = negate (toSolverExp e)                                              


-- class Simplifiable a where
--   simplify :: a -> S.SolverM s m (Maybe a)
-- instance Simplifiable 
