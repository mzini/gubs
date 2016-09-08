module Main where

import GUBS
import GUBS.Utils
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import Data.Tree (drawTree, Tree (..))

processor :: Processor Symbol Integer Variable IO
processor = logCS ==> try simplify ==> logCS ==> simple
  where
    logCS cs = logOpenConstraints cs >> return (Progress cs)
    smt' = smt MiniSmt
    simple =
      try (smt' defaultSMTOpts { degree = 1, maxCoeff = Just 1} )
      ==> try (smt' defaultSMTOpts { degree = 1 })
      ==> try (smt' defaultSMTOpts { degree = 2, maxCoeff = Just 1, maxConst = Just 1})
    simplify =
      try instantiate
      ==> try propagateEq
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

      
      

