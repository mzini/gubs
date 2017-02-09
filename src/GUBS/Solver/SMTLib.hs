module GUBS.Solver.SMTLib (
  z3
  ) where

import GUBS.Algebra
import GUBS.Solver.Class
import qualified GUBS.Expression as E
import qualified GUBS.Solver.Formula as F
import qualified Language.SMTLib2 as SMT
import qualified Language.SMTLib2.Internals as SMTI
import qualified Language.SMTLib2.Solver as SMT
import Control.Monad.Trans (MonadIO, liftIO, MonadTrans (..)) --TODO
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data SMTLibSolver = SMTLibSolver

liftSMT :: SMT.SMT' m a -> SolverM SMTLibSolver m a
liftSMT = SMT

instance IsNat (SMTI.SMTExpr Integer) where
  fromNatural_ = fromInteger
  
instance Additive (SMTI.SMTExpr Integer) where
  zero = fromNatural 0
  e1 .+ e2 = e1 + e2

instance Multiplicative (SMTI.SMTExpr Integer) where
  one = fromNatural 1
  e1 .* e2 = e1 * e2

toSMTExp :: SMTExpression SMTLibSolver -> SMTI.SMTExpr Integer
toSMTExp = E.toNatural fromNatural var where var (NLit (res,ann)) = SMTI.Var res ann

toSMTFormula :: SMTFormula SMTLibSolver -> SMTI.SMTExpr Bool
toSMTFormula f =
  case f of
    Top -> SMT.constant True
    Bot -> SMT.constant False
    Atom (Geq l r) -> toSMTExp l SMT..>=. toSMTExp r
    Atom (Eq l r) ->  toSMTExp l SMT..==. toSMTExp r
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

instance SMTSolver SMTLibSolver where
  data SolverM SMTLibSolver m a = SMT (SMT.SMT' m a) deriving (Functor)
  data NLiteral SMTLibSolver = NLit (Integer, SMTI.SMTAnnotation Integer) deriving (Eq, Ord, Show)
  data BLiteral SMTLibSolver = BLit (Integer, SMTI.SMTAnnotation Bool) deriving (Eq, Ord, Show)

  freshBool = fromVar <$> liftSMT SMT.var
    where
      fromVar :: SMT.SMTExpr Bool -> BLiteral SMTLibSolver
      fromVar (SMTI.Var res ann) = BLit (res,ann)
  freshNat = do
    n <- fromVar <$> liftSMT SMT.var
    assertFormula (E.variable n `F.geqA` fromNatural 0)
    return n
    where
      fromVar :: SMT.SMTExpr Integer -> NLiteral SMTLibSolver
      fromVar (SMTI.Var res ann) = NLit (res,ann)
      
  push = liftSMT SMT.push
  pop = liftSMT SMT.pop
  
  assertFormula f = liftSMT (SMT.assert (toSMTFormula f))
  getValue (NLit (i,ann)) = liftSMT (SMT.getValue (SMTI.Var i ann))
  checkSat = liftSMT SMT.checkSat

z3 :: MonadIO m => SolverM SMTLibSolver m a -> m a
z3 (SMT m) = SMT.withZ3 m

-- deriving instance Eq (NLiteral SMTLibSolver)
-- deriving instance Ord (NLiteral SMTLibSolver)
-- deriving instance Eq (BLiteral SMTLibSolver)
-- deriving instance Ord (BLiteral SMTLibSolver)


instance PP.Pretty (NLiteral SMTLibSolver) where
  pretty (NLit (i,_)) = PP.text "v" PP.<> PP.integer i
instance PP.Pretty (BLiteral SMTLibSolver) where
  pretty (BLit (i,_)) = PP.text "v" PP.<> PP.integer i
