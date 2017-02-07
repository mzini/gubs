module GUBS.Solve (
  solveWith
  , solveWith'
  , Answer (..)
  , interpretation
  , module S
  , module P
  ) where


import           Data.Maybe (fromMaybe)

import qualified GUBS.Interpretation as I
import qualified GUBS.Polynomial as Poly
import           GUBS.Solve.SCC as P
import           GUBS.Solve.SMT as P
import           GUBS.Solve.Simplify as P
import qualified GUBS.Solve.Strategy as S
import           GUBS.ConstraintSystem
import           GUBS.Algebra
import           GUBS.Solve.Strategy hiding (Abort(..))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data Answer f v c = Open (ConstraintSystem f v) (Interpretation f c) | Sat (Interpretation f c) deriving (Show)

solveWith :: (Eq c, Integral c, IsNat c, SemiRing c, Max c, PP.Pretty c, PP.Pretty f, Ord f, Ord v, PP.Pretty v, Monad m) =>
  ConstraintSystem f v -> Processor f c v m -> m (Answer f v c, ExecutionLog)
solveWith cs p = toAnswer <$> run I.empty (p cs) where
  toAnswer (Progress [],i,l) = (Sat i, l)
  toAnswer (Progress cs',i,l) = (Open cs' i, l) 
  toAnswer (NoProgress,i,l) = (Open cs i, l)   


solveWith' :: (Eq c, Integral c, IsNat c, SemiRing c, Max c, PP.Pretty c, PP.Pretty f, Ord f, Ord v, PP.Pretty v, Monad m) =>
  ConstraintSystem f v -> Processor f c v m -> m (Answer f v c)
solveWith' cs p = fst <$> solveWith cs p

interpretation :: Answer f v c -> Maybe (Interpretation f c)
interpretation (Sat i) = Just i
interpretation _ = Nothing

instance (PP.Pretty f, PP.Pretty c, Eq c, IsNat c, SemiRing c, Max c, PP.Pretty v) => PP.Pretty (Answer f v c) where
    pretty (Sat i) = PP.text "SUCCESS" PP.<$$> PP.pretty i
    pretty (Open cs i) = PP.text "OPEN" PP.<$$> PP.pretty cs PP.<$$> PP.pretty i
