module GUBS.Solver.Class (
  Supply (..)
  , SMTSolver (..)
  , Solver
  , stack
  , evalM
  , Formula (..)
  , Atom (..)
  , assert
  , smtTop
  , smtBot
  , smtBool
  , smtNot
  , smtAnd
  , smtBigOr
  , smtBigAnd
  , smtEq
  , smtGeq
  ) where

import Control.Monad.Trans (MonadIO, MonadTrans)
import Control.Monad.Trace
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import GUBS.Algebra hiding (neg)
import GUBS.Expression
import GUBS.Solver.Formula
import GUBS.Constraint (Constraint (..))
import qualified GUBS.Polynomial as Poly

-- TODO IEQs not allowed under negative positions
  
-- data Constrt s =
--   GEQC { clhs :: Exp s, crhs :: Exp s }
--   | EQC { clhs :: Exp s, crhs :: Exp s }

class Supply m v where
  fresh :: m v
    
class (Show (Literal s), PP.Pretty (Literal s), Ord (Literal s), SemiRing (Exp s), IsNat (Exp s), MonadTrans (SolverM s)) => SMTSolver s where
  data SolverM s :: (* -> *) -> * -> *
  data Literal s :: *
  data Exp s :: *

  constnt :: Integer -> Exp s
  lit :: Literal s -> Exp s

  toSolverExp :: Expression (Literal s) -> Exp s
  toSolverExp = toNatural constnt lit
  
  push :: Monad m => SolverM s m ()
  pop  :: Monad m => SolverM s m ()

  assertFormula :: Monad m => Formula (Exp s) -> SolverM s m () -- TODO monadio
  checkSat :: (MonadIO m, MonadTrace String m) => SolverM s m Bool
  getValue :: Monad m => Literal s -> SolverM s m Integer

type Solver s m = (SMTSolver s, MonadIO m, Monad (SolverM s m), Supply (SolverM s m) (Literal s))

assert :: (Solver s m) => Formula (Exp s) -> SolverM s m ()
assert = assertFormula


isZero :: SMTSolver s => Expression (Literal s) -> Formula (Exp s)
isZero e = smtBigAnd [ smtBool (c == 0)
                       `smtOr` smtBigOr [ lit v `eqA` fromNatural 0 | (v,_) <- Poly.toPowers m]
                     | (c,m) <- Poly.toMonos e]
  
simpEq,simpGeq :: SMTSolver s => Expression (Literal s) -> Expression (Literal s) -> Formula (Exp s)
simpEq e1 e2 = toSolverExp e1 `eqA` toSolverExp e2
simpGeq e1 e2 = toSolverExp e1 `geqA` toSolverExp e2
-- simpEq e1 e2 | Poly.isZero e1 = isZero e2
--              | Poly.isZero e2 = isZero e1
--              | otherwise      = toSolverExp e1 `Eq` toSolverExp e2
-- simpGeq e1 e2 | Poly.isZero e1 = isZero e2
--               | otherwise      = toSolverExp e1 `Geq` toSolverExp e2


smtEq,smtGeq :: SMTSolver s => Expression (Literal s) -> Expression (Literal s) -> Formula (Exp s)
-- smtEq e1 e2 = Eq (toSolverExp e1) (toSolverExp e2)
-- smtGeq e1 e2 = Geq (toSolverExp e1) (toSolverExp e2)
smtEq = smtFactorIEQ simpEq
smtGeq = smtFactorIEQ simpGeq

smtFactorIEQ :: SMTSolver s => (Expression (Literal s) -> Expression (Literal s) -> Formula (Exp s)) -> Expression (Literal s) -> Expression (Literal s) -> Formula (Exp s)
smtFactorIEQ eq e1 e2 = 
  case Poly.factorise [e1,e2] of
    Just ((_,m), [e1',e2']) -> smtBigOr ([ lit v `eqA` fromNatural 0 | v <- Poly.monoVariables m] ++ [e1' `eq` e2'])
    _ -> e1 `eq` e2
    
stack :: Solver s m => SolverM s m a -> SolverM s m a
stack m = push *> m <* pop

evalM :: Solver s m => Expression (Literal s) -> SolverM s m Integer
evalM = evalWithM getValue

