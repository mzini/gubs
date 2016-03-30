{-# LANGUAGE FlexibleContexts #-}
module GUBS.Solve.SCC (sccDecompose) where

import GUBS.Solve.Strategy
import GUBS.CS

sccDecompose :: (Eq f, Monad m) => Processor f c v m -> Processor f c v m
sccDecompose p cs =
  case sccs cs of
    [] -> return NoProgress
    (scc:sccs) -> do 
      r <- p scc 
      case r of
        NoProgress -> return NoProgress
        Progress cs' -> return (Progress (concat (cs':sccs)))

