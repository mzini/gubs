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

data Constrt s =
  GEQC { clhs :: Exp s, crhs :: Exp s}
  | EQC { clhs :: Exp s, crhs :: Exp s}

class (Show (Literal s), Num (Exp s)) => SMTSolver s where
  data SolverM s :: (* -> *) -> * -> *
  data Literal s :: *
  data Exp s :: *

  constnt :: Integer -> Exp s
  lit :: Literal s -> Exp s
  
  toSolverExp :: Expression (Literal s) -> Exp s
  toSolverExp (Var v) = lit v
  toSolverExp (Const i) = constnt i
  toSolverExp (Mult e1 e2) = toSolverExp e1 * toSolverExp e2
  toSolverExp (Plus e1 e2) = toSolverExp e1 + toSolverExp e2
  toSolverExp (Neg e) = negate (toSolverExp e)                                              
  
  fresh :: Monad m => SolverM s m (Literal s)

  push :: Monad m => SolverM s m ()
  pop  :: Monad m => SolverM s m ()

  assert :: Monad m => Constrt s -> SolverM s m ()
  checkSat :: (MonadIO m, MonadTrace String m) => SolverM s m Bool
  getValue :: Monad m => Literal s -> SolverM s m Integer

type Solver s m = (SMTSolver s, MonadIO m, Monad (SolverM s m))

geq,leq,eq :: SMTSolver s => Expression (Literal s) -> Expression (Literal s) -> Constrt s
geq (Plus e1 c1@Const{}) c2@Const{} = GEQC (toSolverExp e1) (toSolverExp (c2 - c1))
geq (Plus e1 (Neg e2)) e3 = GEQC (toSolverExp e1) (toSolverExp (e3 + e2))
geq e1 e2 = GEQC (toSolverExp e1) (toSolverExp e2)
leq = flip geq
eq (Plus e1 c1@Const{}) c2@Const{} = EQC (toSolverExp e1) (toSolverExp (c2 - c1))
eq (Plus e1 (Neg e2)) e3 = EQC (toSolverExp e1) (toSolverExp (e3 + e2))
eq e1 e2 = EQC (toSolverExp e1) (toSolverExp e2)
       
stack :: Solver s m => SolverM s m a -> SolverM s m a
stack m = do { push; a <- m; pop; return a }

evalM :: Solver s m => Expression (Literal s) -> SolverM s m Integer
evalM = evalWithM getValue 
