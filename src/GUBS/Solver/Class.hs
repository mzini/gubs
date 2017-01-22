module GUBS.Solver.Class (
  Supply (..)
  , SMTSolver (..)
  , Solver
  , stack
  , evalM
  , Formula (..)
  , assert
  , smtTop
  , smtBot
  , smtBool
  , smtNot
  , smtAnd
  , smtOr
  , smtIff
  , smtImp
  , smtIte
  , smtBigOr
  , smtBigAnd
  , smtEq
  , smtGeq
  ) where

import Control.Monad.Trans (MonadIO, MonadTrans)
import Control.Monad.Trace

import GUBS.Algebra hiding (neg)
import GUBS.Expression
import GUBS.Constraint (Constraint (..))
import qualified GUBS.Polynomial as Poly

data Formula s =
  Top
  | Bot
  | Eq (Exp s) (Exp s)
  | Geq (Exp s) (Exp s)
  | Not (Formula s)
  | Iff (Formula s) (Formula s)
  | Imp (Formula s) (Formula s)
  | Ite (Formula s) (Formula s) (Formula s)
  | And (Formula s) (Formula s)
  | Or (Formula s) (Formula s)
  
-- data Constrt s =
--   GEQC { clhs :: Exp s, crhs :: Exp s }
--   | EQC { clhs :: Exp s, crhs :: Exp s }

class Supply m v where
  fresh :: m v
    
class (Show (Literal s), Ord (Literal s), SemiRing (Exp s), IsNat (Exp s), MonadTrans (SolverM s)) => SMTSolver s where
  data SolverM s :: (* -> *) -> * -> *
  data Literal s :: *
  data Exp s :: *

  constnt :: Integer -> Exp s
  lit :: Literal s -> Exp s

  toSolverExp :: Expression (Literal s) -> Exp s
  toSolverExp = toNatural constnt lit
  
  push :: Monad m => SolverM s m ()
  pop  :: Monad m => SolverM s m ()

  assertFormula :: Monad m => Formula s -> SolverM s m () -- TODO monadio
  checkSat :: (MonadIO m, MonadTrace String m) => SolverM s m Bool
  getValue :: Monad m => Literal s -> SolverM s m Integer

type Solver s m = (SMTSolver s, MonadIO m, Monad (SolverM s m), Supply (SolverM s m) (Literal s))

assert :: (Solver s m) => Formula s -> SolverM s m ()
assert = assertFormula

smtTop, smtBot :: Formula s
smtTop = Top
smtBot = Bot

smtBool :: Bool -> Formula s
smtBool True  = Top
smtBool False = Bot

smtNot :: Formula s -> Formula s
smtNot (Not f) = f
smtNot f       = Not f

smtAnd, smtOr, smtIff, smtImp :: Formula s -> Formula s -> Formula s
Top `smtAnd` f2  = f2
f1  `smtAnd` Top = f1
Bot `smtAnd` _   = Bot
_   `smtAnd` Bot = Bot
f1  `smtAnd` f2  = And f1 f2

Bot `smtOr` f2  = f2
f1  `smtOr` Bot = f1
Top `smtOr` _   = Top
_   `smtOr` Top = Top
f1  `smtOr` f2  = Or f1 f2

Bot `smtIff` f2  = smtNot f2
f1  `smtIff` Bot = smtNot f1
Top `smtIff` f2  = f2
f1  `smtIff` Top = f1
f1  `smtIff` f2  = Iff f1 f2

Bot `smtImp` _   = Top
Top `smtImp` f2  = f2
f1  `smtImp` Bot = smtNot f1
_   `smtImp` Top = Top
f1  `smtImp` f2  = Imp f1 f2

smtIte :: Formula s -> Formula s -> Formula s -> Formula s
smtIte Top t   _   = t
smtIte Bot _   e   = e
smtIte g   Bot e   = smtNot g `smtAnd` e
smtIte g   t   Bot = g `smtAnd` t

smtBigOr, smtBigAnd :: [Formula s] -> Formula s
smtBigOr = foldr smtOr smtBot
smtBigAnd = foldr smtAnd smtTop

isZero :: SMTSolver s => Expression (Literal s) -> Formula s
isZero e = smtBigAnd [ smtBool (c == 0)
                       `smtOr` smtBigOr [ lit v `Eq` fromNatural 0 | (v,_) <- Poly.toPowers m]
                     | (c,m) <- Poly.toMonos e]
  
simpEq,simpGeq :: SMTSolver s => Expression (Literal s) -> Expression (Literal s) -> Formula s
-- simpEq e1 e2 = toSolverExp e1 `Eq` toSolverExp e2
-- simpGeq e1 e2 = toSolverExp e1 `Geq` toSolverExp e2
simpEq e1 e2 | Poly.isZero e1 = isZero e2
             | Poly.isZero e2 = isZero e1
             | otherwise      = toSolverExp e1 `Eq` toSolverExp e2
simpGeq e1 e2 | Poly.isZero e1 = isZero e2
              | otherwise      = toSolverExp e1 `Geq` toSolverExp e2


smtEq,smtGeq :: SMTSolver s => Expression (Literal s) -> Expression (Literal s) -> Formula s
smtEq = smtFactorIEQ simpEq
smtGeq = smtFactorIEQ simpGeq

smtFactorIEQ :: SMTSolver s => (Expression (Literal s) -> Expression (Literal s) -> Formula s) -> Expression (Literal s) -> Expression (Literal s) -> Formula s
smtFactorIEQ eq e1 e2 =
  case Poly.factorise [e1,e2] of
    Just ((_,m), [e1',e2']) -> smtBigOr (e1' `eq` e2' : [ lit v `Eq` fromNatural 0 | v <- Poly.monoVariables m])
    _ -> e1 `eq` e2
    
-- TODO       
stack :: Solver s m => SolverM s m a -> SolverM s m a
stack m = push *> m <* pop

evalM :: Solver s m => Expression (Literal s) -> SolverM s m Integer
evalM = evalWithM getValue
