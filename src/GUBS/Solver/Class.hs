module GUBS.Solver.Class (
  SMTSolver (..)
  , Solver
  , stack
  , evalM
  , Constrt (..)
  , geq
  , leq
  , eq
  ) where

import Control.Monad.Trans (MonadIO)
import Control.Monad.Trace
import GUBS.Expression
import qualified GUBS.Polynomial as Poly

data Constrt s =
  GEQC { clhs :: Exp s, crhs :: Exp s }
  | EQC { clhs :: Exp s, crhs :: Exp s }

class (Show (Literal s), Ord (Literal s), Num (Exp s)) => SMTSolver s where
  data SolverM s :: (* -> *) -> * -> *
  data Literal s :: *
  data Exp s :: *

  constnt :: Integer -> Exp s
  lit :: Literal s -> Exp s

  toSolverExp :: Expression (Literal s) -> Exp s
  toSolverExp = toNum constnt lit
  
  fresh :: Monad m => SolverM s m (Literal s)

  push :: Monad m => SolverM s m ()
  pop  :: Monad m => SolverM s m ()

  assert :: Monad m => Constrt s -> SolverM s m ()
  checkSat :: (MonadIO m, MonadTrace String m) => SolverM s m Bool
  getValue :: Monad m => Literal s -> SolverM s m Integer

type Solver s m = (SMTSolver s, MonadIO m, Monad (SolverM s m))

-- TODO

compzero :: (Ord v) => Expression v -> Expression v -> Expression v
compzero e1 e2 = e1 - e2

geq,leq,eq :: (SMTSolver s) => Expression (Literal s) -> Expression (Literal s) -> Constrt s
geq e1 e2 = GEQC (toSolverExp (compzero e1 e2)) 0
-- geq (Plus e1 c1@Const{}) c2@Const{} = GEQC (toSolverExp e1) (toSolverExp (c2 - c1))
-- geq (Plus e1 (Neg e2)) e3 = GEQC (toSolverExp e1) (toSolverExp (e3 + e2))
-- geq e1 e2 = GEQC (toSolverExp e1) (toSolverExp e2)
leq = flip geq
eq e1 e2 = EQC (toSolverExp (compzero e1 e2)) 0
-- eq (Plus e1 c1@Const{}) c2@Const{} = EQC (toSolverExp e1) (toSolverExp (c2 - c1))
-- eq (Plus e1 (Neg e2)) e3 = EQC (toSolverExp e1) (toSolverExp (e3 + e2))
-- eq e1 e2 = EQC (toSolverExp e1) (toSolverExp e2)
       
stack :: Solver s m => SolverM s m a -> SolverM s m a
stack m = push *> m <* pop

evalM :: Solver s m => Expression (Literal s) -> SolverM s m Integer
evalM = evalWithM getValue
