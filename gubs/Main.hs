{-# LANGUAGE FlexibleContexts #-}
module Main where

import GUBS
import GUBS.Utils

import Control.Monad (when)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import System.Console.CmdArgs
import System.Exit (exitSuccess, exitFailure)
import Data.Tree (drawTree, Tree (..))


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
      (r,l) <- cs `solveWithLog` defaultProcessor solver
      putDocLn r
      when verbose (putLog l)
      exitSuccess
  where
    putLog l = putDocErrLn (text "" <$$> text (drawTree (Node "ExecutionLog" l)))

      
      

