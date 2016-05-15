{-# LANGUAGE FlexibleContexts #-}
module GUBS.Solve.SCC (sccDecompose) where

import GUBS.Solve.Strategy
import GUBS.CS
import qualified Text.PrettyPrint.ANSI.Leijen as PP

sccDecompose :: (Eq f, Monad m, Ord v, PP.Pretty v, Eq c, Num c, Ord f, PP.Pretty f, PP.Pretty c) => Processor f c v m -> Processor f c v m
sccDecompose p cs =
  case sccs cs of
    [] -> return NoProgress
    (scc:sccs) -> do 
      r <- logBlk "SCC decompose" (p scc <* logInterpretation <* logConstraints scc)
      case r of
       Progress [] -> return (Progress (concat sccs))
       _ -> return NoProgress
