module GUBS.Solver.SMTLib (
  z3
  ) where

import GUBS.Algebra
import GUBS.Solver.Class
import qualified Language.SMTLib2 as SMT
import qualified Language.SMTLib2.Internals as SMTI
import qualified Language.SMTLib2.Solver as SMT
import Control.Monad.Trans (MonadIO, liftIO, MonadTrans (..)) --TODO

data SMTLibSolver = SMTLibSolver

instance IsNat (Exp SMTLibSolver) where
  fromNatural_ = Exp . fromInteger
  
instance Additive (Exp SMTLibSolver) where
  zero = fromNatural 0
  Exp e1 .+ Exp e2 = Exp (e1 + e2)

instance Multiplicative (Exp SMTLibSolver) where
  one = fromNatural 1
  Exp e1 .* Exp e2 = Exp (e1 * e2)  
  

liftSMT :: SMT.SMT' m a -> SolverM SMTLibSolver m a
liftSMT = SMT

toSMTFormula :: Formula SMTLibSolver -> SMTI.SMTExpr Bool
toSMTFormula f =
  case f of
    Top -> SMT.constant True
    Bot -> SMT.constant False
    Geq (Exp l) (Exp r) -> l SMT..>=. r
    Eq (Exp l) (Exp r) ->  l SMT..==. r
    Not f -> SMT.not' (toSMTFormula f)
    Iff f1 f2 -> toSMTFormula (And (Imp f1 f2) (Imp f2 f1))
    Imp f1 f2 -> toSMTFormula f1 SMT..=>. toSMTFormula f2
    Ite f1 f2 f3 -> SMT.ite (toSMTFormula f1) (toSMTFormula f2) (toSMTFormula f3)
    And f1 f2 -> app SMT.and' [f1,f2]
    Or f1 f2 -> app SMT.or' [f1,f2]
  where app op fs = SMT.app op (map toSMTFormula fs)

instance Monad m => Applicative (SolverM SMTLibSolver m) where
  pure a = SMT (pure a)
  SMT a1 <*> SMT a2 = SMT (a1 <*> a2)

instance Monad m => Monad (SolverM SMTLibSolver m) where
  return a = SMT (return a)
  SMT m >>= f = SMT (m >>= \ a -> case f a of SMT m2 -> m2)

instance MonadTrans (SolverM SMTLibSolver) where
  lift m = SMT (lift m)

instance Monad m => Supply (SolverM SMTLibSolver m) (Literal SMTLibSolver) where
  fresh = do
    SMTI.Var res ann <- liftSMT var
    return (Lit (res,ann))
    where
      var :: Monad m => SMT.SMT' m (SMT.SMTExpr Integer)
      var = SMT.var

instance SMTSolver SMTLibSolver where
  data SolverM SMTLibSolver m a = SMT (SMT.SMT' m a) deriving (Functor)
  data Literal SMTLibSolver = Lit (Integer, SMTI.SMTAnnotation Integer) deriving Show
  data Exp SMTLibSolver = Exp (SMT.SMTExpr Integer)

  lit (Lit (i,ann)) = Exp (SMTI.Var i ann)
  constnt = Exp <$> SMT.constant
  assertFormula f = liftSMT (SMT.assert (toSMTFormula f))
  getValue (Lit (i,ann)) = liftSMT (SMT.getValue (SMTI.Var i ann))
  push = liftSMT SMT.push
  pop = liftSMT SMT.pop
  checkSat = liftSMT SMT.checkSat

z3 :: MonadIO m => SolverM SMTLibSolver m a -> m a
z3 (SMT m) = SMT.withZ3 m

deriving instance Eq (Literal SMTLibSolver)
deriving instance Ord (Literal SMTLibSolver)
