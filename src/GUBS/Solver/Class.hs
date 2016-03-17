module GUBS.Solver.Class (
  SMTSolver (..)
  , Solver
  , stack
  , evalM
  , Constrt (..)
  , geq
  , leq
  ) where

import Control.Monad.Trans (MonadIO)
import GUBS.Expression

data Constrt s = GEQ (Exp s) (Exp s)

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
  toSolverExp (Minus e1 e2) = toSolverExp e1 - toSolverExp e2
  toSolverExp (Neg e) = negate (toSolverExp e)                                              
  
  fresh :: Monad m => SolverM s m (Literal s)

  push :: Monad m => SolverM s m ()
  pop  :: Monad m => SolverM s m ()

  assert :: Monad m => Constrt s -> SolverM s m ()
  checkSat :: MonadIO m => SolverM s m Bool
  getValue :: Monad m => Literal s -> SolverM s m Integer

type Solver s m = (SMTSolver s, MonadIO m, Monad (SolverM s m))

geq,leq :: SMTSolver s => Expression (Literal s) -> Expression (Literal s) -> Constrt s
geq e1 e2 = GEQ (toSolverExp e1) (toSolverExp e2)
leq = flip geq

stack :: Solver s m => SolverM s m a -> SolverM s m a
stack m = do { push; a <- m; pop; return a }

evalM :: Solver s m => Expression (Literal s) -> SolverM s m Integer
evalM = evalWithM getValue 
