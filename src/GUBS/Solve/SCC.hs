{-# LANGUAGE FlexibleContexts #-}
module GUBS.Solve.SCC (sccDecompose) where

import GUBS.Solve.Strategy
import GUBS.CS
import qualified Text.PrettyPrint.ANSI.Leijen as PP

sccDecompose :: (Eq f, Monad m, Ord v, PP.Pretty v, Eq c, Num c, Integral c, Ord f, PP.Pretty f, PP.Pretty c) => Processor f c v m -> Processor f c v m
sccDecompose p cs =
  case sccs cs of
    [] -> return NoProgress
    (scc:sccs) -> do
      logMsg ("SCC:" ++ show (length sccs + 1) ++ " SCCs")
      toResult sccs <$> (p scc <* logInterpretation <* logConstraints scc)
  where
    toResult sccs (Progress []) = Progress (concat sccs)
    toResult _    _             = NoProgress
