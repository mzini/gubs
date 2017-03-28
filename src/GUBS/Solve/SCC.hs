module GUBS.Solve.SCC (sccDecompose, sccDecomposeWith) where

import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           GUBS.Solve.Strategy
import           GUBS.Algebra
import qualified GUBS.ConstraintSystem as CS


sccDecompose :: (Eq f, Monad m) => Processor f c v m -> Processor f c v m
sccDecompose = sccDecomposeWith CS.sccs

sccDecomposeWith :: Monad m => (CS.ConstraintSystem f v -> [CS.ConstraintSystem f v]) -> Processor f c v m -> Processor f c v m
sccDecomposeWith f p cs =
  case f cs of
    [] -> return NoProgress
    (scc:sccs) -> do
      logMsg ("SCC: " ++ show (length sccs + 1) ++ " SCCs")
      toResult sccs <$> p scc
  where
    toResult sccs (Progress []) = Progress (concat sccs)
    toResult _    _             = NoProgress

