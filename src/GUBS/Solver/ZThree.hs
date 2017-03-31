module GUBS.Solver.ZThree (zthree) where


import           GUBS.Algebra
import qualified GUBS.Expression              as E
import qualified GUBS.MaxPolynomial           as MP
import           GUBS.Solver.Class
import qualified GUBS.Solver.Formula          as F

import Data.Maybe (fromJust)
import           Control.Monad.State.Strict
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Z3.Monad                     as Z


data ZThree = ZThree deriving (Eq, Ord, Show)

instance SMTSolver ZThree where
  newtype SolverM ZThree a = Z3 { unZ3 :: Z.Z3 a } deriving (Functor, Applicative, Monad, MonadIO, Z.MonadZ3)
  data NLiteral ZThree     = NLit Z.AST deriving (Eq, Ord, Show)
  data BLiteral ZThree     = BLit Z.AST deriving (Eq, Ord, Show)

  freshBool = BLit <$> Z.mkFreshBoolVar "b"
  freshNat  = do
    n <- Z.mkFreshIntVar "n"
    z <- Z.mkIntNum 1
    Z.assert =<< n `Z.mkGe` z
    return $ NLit n

  push = Z.push
  pop  = Z.pop 1

  assertFormula     = aFormula

  checkSat          = (==) Z.Sat <$> Z.check
  -- getValue (NLit n) = Z.getInt n -- MS: uses current context; not whole model
  getValue (NLit n) = do
    (_,mM) <- Z.getModel
    fromJust <$> Z.evalInt (fromJust mM) n



aFormula :: SMTFormula ZThree -> SolverM ZThree ()
aFormula f = Z.assert =<< toFormula f

toFormula :: SMTFormula ZThree -> SolverM ZThree Z.AST
toFormula Top                      = Z.mkBool True
toFormula Bot                      = Z.mkBool False
toFormula (Lit (BoolLit (BLit l))) = return l
toFormula (Atom (Geq l r))         = join $ Z.mkGe <$> toExpression l <*> toExpression r
toFormula (Atom (Eq l r))          = join $ Z.mkEq <$> toExpression l <*> toExpression r
toFormula (Or a b)                 = Z.mkOr  =<< sequence [toFormula a, toFormula b]
toFormula (And a b)                = Z.mkAnd =<< sequence [toFormula a, toFormula b]
toFormula (Iff a b)                = join $ Z.mkIff <$> toFormula a <*> toFormula b


toExpression :: SMTExpression ZThree -> SolverM ZThree Z.AST
toExpression = fromMP . E.fromPolynomial MP.variable MP.constant where -- to exploit simplification
  fromMP (MP.Var (NLit v)) = return v
  fromMP (MP.Const i)      = Z.mkIntNum i
  fromMP (MP.Plus a b)     = Z.mkAdd =<< sequence [fromMP a, fromMP b]
  fromMP (MP.Mult a b)     = Z.mkMul =<< sequence [fromMP a, fromMP b]
  fromMP MP.Max{}          = error "max encountered"

zthree :: SolverM ZThree a -> IO a
-- zthree (Z3 z3) = Z.evalZ3With (Just Z.QF_NIA) Z.stdOpts z3
zthree (Z3 z3) = Z.evalZ3 z3

