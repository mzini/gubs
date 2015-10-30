{-# LANGUAGE FlexibleContexts #-}
module GUBS.Solve.Incremental where

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

-- solveIncremental :: (Solver s m, Ord f, Ord v) => ConcreteInterpretation f -> ConstraintSystem f v -> SolverM s m (Either (ConstraintSystem f v, ConcreteInterpretation f) (ConcreteInterpretation f))
-- solveIncremental i = solve i . sccs where
--   solve inter [] = return (Right inter)
--   solve inter (scc:cs) = do
--      minter <- solveSimple inter scc
--      case minter of
--        Nothing -> return (Left (scc, inter))
--        Just inter' -> solve (inter `I.union` inter') cs


-- solve :: (MonadIO m, Ord f, PP.Pretty (ConcreteInterpretation f), PP.Pretty (AbstractPolynomial s v), Ord v) => ConcreteInterpretation f -> ConstraintSystem f v -> m (Either (ConstraintSystem f v, ConcreteInterpretation f) (ConcreteInterpretation f))
-- solve inter cs = Solver.withZ3 (solveM inter cs)


