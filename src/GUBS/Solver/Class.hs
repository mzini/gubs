{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module GUBS.Solver.Class where

class (Show (Literal s), Num (Expression s)) => SMTSolver s where
  data SolverM s :: (* -> *) -> * -> *
  data Literal s :: *
  data Expression s :: *
  data Constraint s :: *

  constant :: Integer -> Expression s
  literal :: Literal s -> Expression s
  fresh :: Monad m => SolverM s m (Literal s)
  getValue :: Monad m => Literal s -> SolverM s m Integer

  -- simplifyExp :: (Monad m, Monad (SolverM s m)) => Expression s -> SolverM s m (Maybe (Expression s))
  -- simplifyExp _ = return Nothing
  -- simplifyConstr :: (Monad m, Monad (SolverM s m)) => Constraint s -> SolverM s m (Maybe (Constraint s))
  -- simplifyConstr _ = return Nothing
  geq :: Expression s -> Expression s -> Constraint s
  assert :: Monad m => Constraint s -> SolverM s m ()
  checkSat :: Monad m => SolverM s m Bool

type Solver s m = (SMTSolver s, Monad m, Monad (SolverM s m))
