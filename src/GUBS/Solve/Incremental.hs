{-# LANGUAGE FlexibleContexts #-}
module GUBS.Solve.Incremental (solveIncremental) where

import GUBS.Solve.Simple
import GUBS.Solver.Class
import GUBS.CS
import qualified GUBS.Interpretation as I

solveIncremental :: (Solver s m, Ord f, Ord v) => ConcreteInterpretation f -> ConstraintSystem f v -> SolverM s m (Maybe (ConcreteInterpretation f))
solveIncremental i = solve i . sccs where
  solve inter [] = return (Just inter)
  solve inter (scc:cs) = do
     minter <- solveSimple inter scc
     case minter of
       Nothing -> return Nothing
       Just inter' -> solve (inter `I.union` inter') cs


