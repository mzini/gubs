module Main where

import GUBS
import GUBS.Utils
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import Data.Tree (drawTree, Tree (..))

processor :: Processor Symbol Integer Variable IO
processor = try simplify ==> try (minismt 0) ==> try (minismt 1) ==> try (minismt 2)
  where
    simplify = exhaustive (propagateUp <=> propagateDown)
    minismt = smt MiniSmt

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
    putResult (Sat i) = putDocLn (text "SUCCESS" <$$> pretty i)
    putResult (Open cs i) = putDocLn (text "OPEN" <$$> pretty cs <$$> pretty i)
    putLog l = putDocErrLn (text "" <$$> text (drawTree (Node "ExecutionLog" l)))

      
      

