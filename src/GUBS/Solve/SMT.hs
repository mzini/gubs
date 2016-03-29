module GUBS.Solve.SMT ( smt, SMTSolver (..) ) where

import Control.Monad (forM_, liftM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad.Trans (lift)

import qualified Data.Map.Strict as M
import GUBS.CS
import GUBS.Expression (Expression, literal)
import qualified GUBS.Interpretation as I
import qualified GUBS.Polynomial as P
import GUBS.Solve.Strategy
import GUBS.Solver hiding (SMTSolver)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data SMTSolver = MiniSmt | Z3 deriving (Show)

type AbstractPolynomial s v = P.Polynomial v (Expression (Literal s))
type AbstractInterpretation s f = I.Interpretation f (Expression (Literal s))

freshPoly :: (Solver s m) => SolverM s m (Expression (Literal s)) -> Int -> Int -> SolverM s m (AbstractPolynomial s I.Var)
freshPoly freshVar dim ar =
  P.fromMonos <$> sequence [ (,) <$> freshVar <*> return (P.fromPowers mono)
                           | mono <- concat (template dim) ]
  where 
    template 0 = [[[]]]
    template d = [ v `mult` mono | mono <- lead, v <- take ar I.variables ] : ps
      where
        ps@(lead:_) = template (d - 1)
        v `mult` [] = [(v,1)]
        v `mult` ((v',i) : mono)
          | v == v' = (v',i+1) : mono
          | otherwise = (v',i) : v `mult` mono

interpret :: (Solver s m, Ord f, Ord v) => Int -> Term f v -> StateT (AbstractInterpretation s f) (SolverM s m) (AbstractPolynomial s v)
interpret _ (Var v) = return (P.variable v)
interpret _ (Const i) = return (fromIntegral i)
interpret d (Plus t1 t2) = (+) <$> interpret d t1 <*> interpret d t2
interpret d (Mult t1 t2) = (*) <$> interpret d t1 <*> interpret d t2
interpret d (Fun f ts) = do I.apply <$> getPoly <*> mapM (interpret d) ts where
    getPoly = do
      ainter <- get
      maybe (addPoly ainter) return (I.get ainter f)
    addPoly ainter = do
      p <- lift (freshPoly freshVar (max 1 d) (length ts))
      put (I.insert ainter f p)
      return p
    freshVar = do
      v <- literal <$> fresh
      assert (v `geq` 0)
      when (d <= 0) (assert (1 `geq` v))
      return v


fromAssignment :: (Solver s m) => AbstractInterpretation s f -> SolverM s m (Interpretation f Integer)
fromAssignment = traverse evalM

solveM :: (Ord f, Ord v, Solver s m) => Interpretation f Integer -> Int -> ConstraintSystem f v -> SolverM s m (Maybe (Interpretation f Integer))
solveM inter degree cs = do
  ainter <- flip execStateT (I.map (fmap fromIntegral) inter) $ 
    forM_ cs $ \ c -> do
      l <- interpret degree (lhs c)
      r <- interpret degree (rhs c)
      forM_ (P.coefficients (l - r)) $ \ d -> 
        lift (assert (d `geq` 0) )
  sat <- checkSat
  if sat then Just <$> fromAssignment ainter else return Nothing

smt :: (Ord f, Ord v, MonadIO m) => SMTSolver -> Int -> Processor f Integer v m
smt solver degree cs = do
  getInterpretation >>= run solver >>= maybe abort success
  where
    run Z3 inter = z3 (solveM inter degree cs)
    run MiniSmt inter = miniSMT (solveM inter degree cs)
    
    success inter = modifyInterpretation (const inter) >> return []
  
