{-# LANGUAGE FlexibleContexts #-}
module Main where

import GUBS
import GUBS.Utils
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import Data.Tree (drawTree, Tree (..))


smtOpts :: SMTOpts
smtOpts = defaultSMTOpts { minimize = tryM (iterM 3 zeroOut) `andThenM` tryM (iterM 3 shiftMax) `andThenM` iterM 3 decreaseCoeffs }

smtSolver :: Solver
smtSolver = Z3 -- MiniSmt

processor :: Processor Symbol Integer Variable IO
processor =
  withLog (try simplify) ==> try (exhaustive (logAs "SCC" (sccDecompose simple)))
  where
    withLog p cs = 
      logOpenConstraints cs *> p cs <* logInterpretation cs <* logConstraints cs
      
    logAs str p cs = logBlk (str++"...") (p cs)
    simple =
      logAs "SOLVE" $ timed $ withLog $
        try simplify
        ==> try (smt' "SMT-MSLI"   smtOpts { degree = 1, maxCoeff = Just 1, maxPoly = True })
        ==> try (smt' "SMT-SLI"    smtOpts { degree = 1, maxCoeff = Just 1 })
        ==> try (smt' "SMT-LI"     smtOpts { degree = 1 })
        ==> try (smt' "SMT-MMI(2)" smtOpts { degree = 2})
        ==> try (smt' "SMT-MI(2)"  smtOpts { degree = 2, shape = Mixed})
        ==> try (smt' "SMT-MMI(3)" smtOpts { degree = 3})
        ==> try (smt' "SMT-MI(3)"  smtOpts { degree = 4, shape = Mixed})
    smt' n o = logAs n $ timed $ smt smtSolver o
    simplify = 
      logAs "Simplification" $
        try instantiate
        ==> try eliminate
        ==> try (exhaustive (propagateUp <=> propagateDown))
        
main :: IO ()
main = do
  parsed <- csFromFile =<< (head <$> getArgs)
  case parsed of
    Left err -> do
      putDocLn (text "ERROR" <$$> text (show err))
      exitFailure
    Right cs -> do
      (r,l) <- cs `solveWith` processor
      putResult r
      putLog l
      exitSuccess
  where
    putResult = putDocLn
    putLog l = putDocErrLn (text "" <$$> text (drawTree (Node "ExecutionLog" l)))

      
      

