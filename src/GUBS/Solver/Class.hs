module GUBS.Solver.Class (
  SMTSolver (..)
  , Solver
  , stack
  , evalM
  , Constrt (..)
  , Disj
  , assertGeq
  , assertLeq
  , assertEq
  ) where

import Control.Monad.Trans (MonadIO, MonadTrans)
import Control.Monad.Trace
import GUBS.Expression
import qualified GUBS.Polynomial as Poly

type Disj a = [a]

data Constrt s =
  GEQC { clhs :: Exp s, crhs :: Exp s }
  | EQC { clhs :: Exp s, crhs :: Exp s }

class (Show (Literal s), Ord (Literal s), Num (Exp s), MonadTrans (SolverM s)) => SMTSolver s where
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

  assert :: Monad m => Disj (Constrt s) -> SolverM s m () --TODO monadio
  checkSat :: (MonadIO m, MonadTrace String m) => SolverM s m Bool
  getValue :: Monad m => Literal s -> SolverM s m Integer

type Solver s m = (SMTSolver s, MonadIO m, Monad (SolverM s m))

-- TODO

assertGeq,assertLeq,assertEq :: (Solver s m) => Expression (Literal s) -> Expression (Literal s) -> SolverM s m ()
assertGeq e1 e2 = 
  case Poly.factorise (e1 - e2) of
    Nothing -> assert [GEQC (toSolverExp e1) (toSolverExp e2)]
    (Just ((m,_),p)) -> do
      mapM (\ v -> assert [GEQC (lit v) (fromInteger 0)]) (Poly.monoVariables m)
      assert (GEQC (toSolverExp p) (fromInteger 0)
                                : [ EQC (lit v) (fromInteger 0) | v <- Poly.monoVariables m])
assertLeq = flip assertGeq
assertEq e1 e2 = assert [EQC (toSolverExp e1) (toSolverExp e2)]
-- assertEq e1 e2 = assert (EQC (toSolverExp p) (fromInteger 0)
--                           : [ EQC (lit v) (fromInteger 0) | v <- Poly.monoVariables m])
--   where ((m,_),p) = Poly.factorise (e1 - e2) 
       
stack :: Solver s m => SolverM s m a -> SolverM s m a
stack m = push *> m <* pop

evalM :: Solver s m => Expression (Literal s) -> SolverM s m Integer
evalM = evalWithM getValue
