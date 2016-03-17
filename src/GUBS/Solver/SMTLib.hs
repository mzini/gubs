module GUBS.Solver.SMTLib (
  z3
  ) where

import GUBS.Solver.Class
import qualified Language.SMTLib2 as SMT
import qualified Language.SMTLib2.Solver as SMT
import Control.Monad.Trans (MonadIO)

data SMTLibSolver = SMTLibSolver

liftUn f (Exp e) = Exp (f e)
liftBin f (Exp e1) (Exp e2) = Exp (f e1 e2)

instance Num (Exp SMTLibSolver) where
  fromInteger = Exp . fromInteger
  (+) = liftBin (+)
  (-) = liftBin (-)
  (*) = liftBin (*)
  negate = liftUn negate
  abs = liftUn abs
  signum = liftUn signum

lift :: SMT.SMT' m a -> SolverM SMTLibSolver m a
lift = SMT

instance Monad m => Applicative (SolverM SMTLibSolver m) where
  pure a = SMT (pure a)
  SMT a1 <*> SMT a2 = SMT (a1 <*> a2)

instance Monad m => Monad (SolverM SMTLibSolver m) where
  return a = SMT (return a)
  SMT m >>= f = SMT (m >>= \ a -> case f a of SMT m2 -> m2)
  
instance SMTSolver SMTLibSolver where
  data SolverM SMTLibSolver m a = SMT (SMT.SMT' m a) deriving (Functor)
  data Literal SMTLibSolver = Lit (SMT.SMTExpr Integer) deriving Show
  data Exp SMTLibSolver = Exp (SMT.SMTExpr Integer)

  lit (Lit e) = Exp e
  constnt = Exp <$> SMT.constant
  fresh = Lit <$> lift SMT.var
  getValue (Lit e) = lift (SMT.getValue e)
  push = lift SMT.push
  pop = lift SMT.pop
  assert (GEQ (Exp l) (Exp r)) = lift (SMT.assert (l SMT..>=. r))
  checkSat = lift SMT.checkSat

z3 :: MonadIO m => SolverM SMTLibSolver m a -> m a
z3 (SMT m) = SMT.withZ3 m
