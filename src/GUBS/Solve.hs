module GUBS.Solve (
  Method (..)
  , solve
  , solveWith
  ) where

import GUBS.CS
import GUBS.Solver.Class
import GUBS.Solve.Incremental (solveIncremental)
import GUBS.Solve.Simple (solveSimple, ConcreteInterpretation)

data Method = Simple | Incremental deriving Show

solveWith :: (Solver s m, Ord f, Ord v) => Method -> ConcreteInterpretation f -> ConstraintSystem f v -> SolverM s m (Maybe (ConcreteInterpretation f))
solveWith Simple = solveSimple
solveWith Incremental = solveIncremental

solve :: (Solver s m, Ord f, Ord v) => ConcreteInterpretation f -> ConstraintSystem f v -> SolverM s m (Maybe (ConcreteInterpretation f))
solve = solveWith Simple



