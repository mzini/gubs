{-# LANGUAGE FlexibleContexts #-}
module GUBS.Solve.SCC (sccDecompose) where

import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           GUBS.Solve.Strategy
import           GUBS.Algebra
import qualified GUBS.ConstraintSystem as CS



sccDecompose :: (Monad m, Ord v, PP.Pretty v, Eq c, Integral c, SemiRing c, Max c, IsNat c, PP.Pretty c, Eq f, Ord f, PP.Pretty f) => Processor f c v m -> Processor f c v m
sccDecompose p cs =
  case CS.sccs cs of
    [] -> return NoProgress
    (scc:sccs) -> do
      logMsg ("SCC:" ++ show (length sccs + 1) ++ " SCCs")
      toResult sccs <$> p scc
  where
    toResult sccs (Progress []) = Progress (concat sccs)
    toResult _    _             = NoProgress
