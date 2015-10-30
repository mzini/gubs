module GUBS.CS where
import Data.List (nub)
import Data.Graph

data Term f v =
  Var v
  | Const Int
  | Fun f [Term f v]
  deriving (Show)

funs :: Eq f => Term f v -> [f]
funs = nub . funs' where
  funs' (Fun f ts) = f : concatMap funs ts
  funs' _          = []

data Constraint f v = Term f v :>=: Term f v

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

