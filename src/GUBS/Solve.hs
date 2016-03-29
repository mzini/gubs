module GUBS.Solve (
  solveWith
  , Answer (..)
  , module S
  , module P
  ) where

import GUBS.CS
import GUBS.Solve.Strategy hiding (Abort(..))
import qualified GUBS.Solve.Strategy as S
import qualified GUBS.Interpretation as I

import GUBS.Solve.SMT as P
import GUBS.Solve.SCC as P

data Answer f v c = DontKnow | Open (ConstraintSystem f v) (Interpretation f c) | Sat (Interpretation f c)
  
solveWith :: Monad m => ConstraintSystem f v -> Processor f c v m -> m (Answer f v c)
solveWith cs p = toAnswer <$> run I.empty (p cs) where
  toAnswer (Left S.Abort) = DontKnow
  toAnswer (Right ([],i)) = Sat i
  toAnswer (Right (cs,i)) = Open cs i

