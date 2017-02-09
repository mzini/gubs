module GUBS.Solver.Class (
  SMTSolver (..)
  , Solver
  , stack
  , evalM
  , F.BoolLit (..)
  , F.Formula (..)
  , F.Atom (..)
  , SMTFormula
  , SMTExpression
  , assert
  , F.smtTop
  , F.smtBot
  , F.smtBool
  -- , F.smtNot
  , F.smtOr
  , F.smtAnd  
  , F.smtBigOr
  , F.smtBigAnd
  , smtEq
  , smtGeq
  ) where

import Control.Monad ((>=>))
import Control.Monad.Trans (MonadIO, MonadTrans)
import Control.Monad.Trace
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import GUBS.Algebra hiding (neg)
import qualified GUBS.Expression as E
import qualified GUBS.Solver.Formula as F
import GUBS.Constraint (Constraint (..))
import qualified GUBS.Polynomial as Poly

type SMTExpression s = E.Expression (NLiteral s)
type SMTFormula s = F.Formula (BLiteral s) (SMTExpression s)

class (Ord (NLiteral s), MonadTrans (SolverM s)) => SMTSolver s where
  data SolverM s :: (* -> *) -> * -> *
  data NLiteral s :: *
  data BLiteral s :: *

  freshBool :: Monad m => SolverM s m (BLiteral s)
  freshNat  :: Monad m => SolverM s m (NLiteral s)
  
  push :: Monad m => SolverM s m ()
  pop  :: Monad m => SolverM s m ()

  assertFormula :: Monad m => SMTFormula s -> SolverM s m ()
  checkSat :: (MonadIO m, MonadTrace String m) => SolverM s m Bool
  getValue :: Monad m => NLiteral s -> SolverM s m Integer

type Solver s m = (SMTSolver s, MonadIO m, Monad (SolverM s m))

assert :: (Solver s m) => SMTFormula s -> SolverM s m ()
assert = letElim >=> assertFormula where
  letElim F.Top = return F.Top
  letElim F.Bot = return F.Bot
  letElim l@F.Lit{} = return l
  letElim a@F.Atom{} = return a
  letElim (F.Or f1 f2) = F.Or <$> letElim f1 <*> letElim f2
  letElim (F.And f1 f2) = F.And <$> letElim f1 <*> letElim f2
  letElim (F.LetB e f) = do
    e' <- letElim e
    b <- freshBool
    assertFormula (F.Iff (F.literal b) e')
    letElim (f b)

-- isZero :: SMTSolver s => Expression (Literal s) -> Formula (Exp s)
-- isZero e = smtBigAnd [ smtBool (c == 0)
--                        `smtOr` smtBigOr [ elit v `eqA` fromNatural 0 | (v,_) <- Poly.toPowers m]
--                      | (c,m) <- Poly.toMonos e]
  
simpEq,simpGeq :: SMTSolver s => SMTExpression s -> SMTExpression s -> SMTFormula s
simpEq e1 e2  = e1 `F.eqA` e2
simpGeq e1 e2 = e1 `F.geqA` e2
-- simpEq e1 e2 | Poly.isZero e1 = isZero e2
--              | Poly.isZero e2 = isZero e1
--              | otherwise      = toSolverExp e1 `Eq` toSolverExp e2
-- simpGeq e1 e2 | Poly.isZero e1 = isZero e2
--               | otherwise      = toSolverExp e1 `Geq` toSolverExp e2


smtEq,smtGeq :: SMTSolver s => SMTExpression s -> SMTExpression s -> SMTFormula s
-- smtEq e1 e2 = Eq (toSolverExp e1) (toSolverExp e2)
-- smtGeq e1 e2 = Geq (toSolverExp e1) (toSolverExp e2)
smtEq = smtFactorIEQ simpEq
smtGeq = smtFactorIEQ simpGeq

smtFactorIEQ :: SMTSolver s => (SMTExpression s -> SMTExpression s -> SMTFormula s)
             -> SMTExpression s -> SMTExpression s -> SMTFormula s
smtFactorIEQ eq e1 e2 = 
  case Poly.factorise [e1,e2] of
    Just ((_,m), [e1',e2']) -> F.smtBigOr ([ Poly.variable v `F.eqA` fromNatural 0 | v <- Poly.monoVariables m] ++ [e1' `eq` e2'])
    _ -> e1 `eq` e2
    
stack :: Solver s m => SolverM s m a -> SolverM s m a
stack m = push *> m <* pop

evalM :: Solver s m => SMTExpression s -> SolverM s m Integer
evalM = E.evalWithM getValue

