module GUBS.CS (
  Term (..)
  , funs
  , Constraint (..)
  , lhs
  , rhs
  , definedSymbol
  , ConstraintSystem (..)
  , lhss
  , rhss
  , sccs
  ) where
import Data.List (nub)
import Data.Graph

data Term f v =
  Var v
  | Const Integer
  | Fun f [Term f v]
  | Mult (Term f v) (Term f v)
  | Plus (Term f v) (Term f v)
  | Minus (Term f v) (Term f v)
  | Neg (Term f v)
  deriving (Show)

funs :: Eq f => Term f v -> [f]
funs = nub . funs' where
  funs' (Fun f ts) = f : concatMap funs ts
  funs' (Mult t1 t2) = funs t1 ++ funs t2
  funs' (Plus t1 t2) = funs t1 ++ funs t2

definedSymbol :: Term f v -> Maybe f
definedSymbol (Fun f _) = Just f
definedSymbol _ = Nothing              


liftIntOp f _ (Const i) (Const j) = Const (f i j)
liftIntOp _ c e1 e2 = c e1 e2

plus :: Term f v -> Term f v -> Term f v
plus (Const 0) y = y
plus x (Const 0) = x
plus x y = liftIntOp (+) Plus x y

mult :: Term f v -> Term f v -> Term f v
mult (Const 0) _ = Const 0
mult x (Const 0) = Const 0
mult (Const 1) y = y
mult x (Const 1) = x
mult x y = liftIntOp (*) Mult x y

minus :: Term f v -> Term f v -> Term f v
minus x (Const 0) = x
minus x y = liftIntOp (-) Minus x y

neg :: Term f v -> Term f v
neg (Neg i) = i
neg (Const j) = Const (-j)
neg e = Neg e

instance Num (Term f v) where
  fromInteger = Const
  (+) = plus
  (-) = minus
  (*) = mult
  negate = neg
  abs = error "Term: abs undefined"
  signum = error "Term: signum undefined"


data Constraint f v = Term f v :>=: Term f v

lhs, rhs :: Constraint f v -> Term f v 
lhs (l :>=: r) = l
rhs (l :>=: r) = r

type ConstraintSystem f v = [Constraint f v]

sccs :: Eq f => ConstraintSystem f v -> [[Constraint f v]]
sccs cs = map flattenSCC sccs' where
  sccs' = stronglyConnComp [ (c, i , succs c ) | (i,c) <- ecs ]
  ecs = zip [0..] cs
  succs c = [ j | (j,c') <- ecs
                , any (`elem` (funs (rhs c))) (funs (lhs c'))
                  || any (`elem` (funs (lhs c))) (funs (lhs c')) ]

lhss = map lhs

rhss = map rhs

