module GUBS.Solve (
  solveWith
  , Answer (..)
  , module S
  , module P
  ) where

import           Control.Arrow
import           Data.Tree
import           GUBS.CS
import qualified GUBS.Interpretation as I
import qualified GUBS.Solve.Strategy as S
import           GUBS.Solve.Strategy hiding (Abort(..))

import           GUBS.Solve.SMT as P
import           GUBS.Solve.SCC as P
import           GUBS.Solve.Simplify as P

data Answer f v c = DontKnow | Open (ConstraintSystem f v) (Interpretation f c) | Sat (Interpretation f c)
  
solveWith :: Monad m => ConstraintSystem f v -> Processor f c v m -> m (Answer f v c, ExecutionLog)
solveWith cs p = toAnswer <$> run I.empty (p cs) where
  toAnswer (Progress [],i,l) = (Sat i, l)
  toAnswer (Progress cs',i,l) = (Open cs' i, l) 
  toAnswer (NoProgress,i,l) = (Open cs i, l)   

