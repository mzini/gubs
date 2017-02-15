{-# LANGUAGE FlexibleContexts #-}
module Main where

import GUBS
import GUBS.Utils

import Control.Monad (when)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import System.Console.CmdArgs
import System.Exit (exitSuccess, exitFailure)
import Data.Tree (drawTree, Tree (..))


smtOpts :: SMTOpts
smtOpts = defaultSMTOpts { minimize = tryM (iterM 3 zeroOut) `andThenM` tryM (iterM 3 shiftMax) `andThenM` iterM 3 decreaseCoeffs }

processor :: Solver -> Processor Symbol Integer Variable IO
processor smtSolver =
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

deriving instance Data Solver

data GUBS = GUBS { solver  :: Solver
                 , input   :: FilePath
                 , verbose :: Bool }
          deriving Data

defaultConfig :: GUBS
defaultConfig =
  GUBS { input = def &= typFile &= argPos 0
       , solver = Z3 &= help "SMT solver (minismt, z3). Defaults to z3."
       , verbose = False }
  &= summary "GUBS Upper Bound Solver 0.3"
  
main :: IO ()
main = do
  GUBS{..} <- cmdArgs defaultConfig
  parsed <- csFromFile input
  case parsed of
    Left err -> do
      putDocLn (text "ERROR" <$$> text (show err))
      exitFailure
    Right cs -> do
      (r,l) <- cs `solveWith` processor solver
      putDocLn r
      when verbose (putLog l)
      exitSuccess
  where
    putLog l = putDocErrLn (text "" <$$> text (drawTree (Node "ExecutionLog" l)))

      
      

