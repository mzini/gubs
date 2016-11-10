{-# LANGUAGE FlexibleContexts #-}
module Main where

import GUBS
import GUBS.Utils
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import Data.Tree (drawTree, Tree (..))


processor =
  logCS ==> try simplify ==> logCS ==> try (exhaustive (sccDecompose (logCS ==> try simplify ==> simple)))
  where
    logCS cs = logOpenConstraints cs >> return (Progress cs)
    logStr str cs = logMsg str >> return (Progress cs)
    simple =
      logStr "SMT: trying strongly linear interpretation"
      ==> try (smt' defaultSMTOpts { degree = 1, maxCoeff = Just 1} )
      ==> logStr "SMT: trying linear interpretation"      
      ==> try (smt' defaultSMTOpts { degree = 1 })
      ==> logStr "SMT: trying strongly multmixed interpretation"            
      ==> try (smt' defaultSMTOpts { degree = 2, maxCoeff = Just 1})
      ==> logStr "SMT: trying multmixed interpretation"            
      ==> try (smt' defaultSMTOpts { degree = 2, maxCoeff = Nothing})
      ==> logStr "SMT: trying mixed interpretation"                  
      ==> try (smt' defaultSMTOpts { degree = 2, shape = Mixed, maxCoeff = Nothing})
      ==> logStr "SMT: trying multmixed interpretation of degree 3"            
      ==> try (smt' defaultSMTOpts { degree = 3, maxCoeff = Nothing})
      ==> logStr "SMT: trying multmixed interpretation of degree 3"            
      ==> try (smt' defaultSMTOpts { degree = 3, shape = Mixed, maxCoeff = Nothing})
    smt' = smt Z3
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

      
      

