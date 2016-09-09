module GUBS.Solve.SMT ( smt, SMTSolver (..), PolyShape (..), SMTOpts (..), defaultSMTOpts ) where

import Data.List (subsequences)
import Control.Monad (forM_, liftM, when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad.Trans (lift)
import Control.Monad.Trace

import GUBS.CS
import GUBS.Expression (Expression, variable)
import qualified GUBS.Interpretation as I
import qualified GUBS.Polynomial as P
import GUBS.Solve.Strategy
import GUBS.Solver hiding (SMTSolver)

-- TODO remove
import GUBS.Utils (tracePretty)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data SMTSolver = MiniSmt | Z3 deriving (Show)

type AbstractPolynomial s v = P.Polynomial v (Expression (Literal s))
type AbstractInterpretation s f = I.Interpretation f (Expression (Literal s))

data PolyShape = Mixed | MultMixed
  deriving (Eq, Show)

data SMTOpts =
  SMTOpts { shape    :: PolyShape
          , degree   :: Int 
          , maxCoeff :: Maybe Int
          , maxConst :: Maybe Int}

defaultSMTOpts :: SMTOpts
defaultSMTOpts = SMTOpts { shape = MultMixed
                         , degree = 2
                         , maxCoeff = Nothing
                         , maxConst = Nothing }

freshPoly :: (Solver s m) => SMTOpts -> Int -> SolverM s m (AbstractPolynomial s I.Var)
freshPoly opts ar =
  P.fromMonos <$> sequence [ (,) <$> freshCoeff mono <*> return (P.fromPowers mono)
                           | mono <- fromShape (shape opts)]
  where
    vars = take ar I.variables
    -- fromShape StronglyLinear = [] : [ [(v,1)] | v <- vars ]
    -- fromShape Linear = [] : [ [(v,1)] | v <- vars ]
    fromShape MultMixed = [ [ (v,1) | v <- ms] | ms <- subsequences vars, length ms <= degree opts ]
    fromShape Mixed     = concat (template (degree opts)) where 
      template 0 = [[[]]]
      template d = [ v `mult` mono | mono <- lead, v <- vars ] : ps
        where
          ps@(lead:_) = template (d - 1)
          v `mult` [] = [(v,1)]
          v `mult` ((v',i) : mono)
            | v == v' = (v',i+1) : mono
            | otherwise = (v',i) : v `mult` mono
            
    freshCoeff mono = do
      v <- variable <$> fresh
      assert (v `geq` 0)
      let maxC = if null mono then maxConst opts else maxCoeff opts
      maybe (return ()) (\ ub -> assert (fromIntegral ub `geq` v)) maxC
        -- unless (null mono) (assert (v `geq` 0)) -- TODO: problems with Z3
        -- when (shape opts == StronglyLinear && null mono) (assert (1 `geq` v))
      return v

interpret :: (Solver s m, Ord f, Ord v) => SMTOpts -> Term f v -> StateT (AbstractInterpretation s f) (SolverM s m) (AbstractPolynomial s v)
interpret _ (Var v) = return (P.variable v)
interpret _ (Const i) = return (fromIntegral i)
interpret opts (Plus t1 t2) = (+) <$> interpret opts t1 <*> interpret opts t2
interpret opts (Mult t1 t2) = (*) <$> interpret opts t1 <*> interpret opts t2
interpret opts (Minus t1 t2) = (-) <$> interpret opts t1 <*> interpret opts t2
interpret opts (Neg t) = negate <$> interpret opts t
interpret opts (Fun f ts) = do I.apply <$> getPoly <*> mapM (interpret opts) ts where
    getPoly = do
      ainter <- get
      maybe (addPoly ainter) return (I.get ainter f)
    addPoly ainter = do
      p <- lift (freshPoly opts (length ts))
      put (I.insert ainter f p)
      return p


fromAssignment :: (Solver s m) => AbstractInterpretation s f -> SolverM s m (Interpretation f Integer)
fromAssignment = traverse evalM

-- TODO
solveM :: (Ord f, Ord v, Solver s m, MonadTrace String m) => Interpretation f Integer -> SMTOpts -> ConstraintSystem f v -> SolverM s m (Maybe (Interpretation f Integer))
solveM inter opts cs = do
  ainter <- flip execStateT (I.mapInter (fmap fromIntegral) inter) $ 
    forM_ cs $ \ c -> do
      l <- interpret opts (lhs c)
      r <- interpret opts (rhs c)
      -- TODO
      (lift . assert . constraint c) `mapM` P.coefficients (l - r) 
  sat <- checkSat
  if sat then Just <$> fromAssignment ainter else return Nothing
  where 
    constraint (_ :>=: _) d = factor d `geq` 0
    constraint (_ :=: _) d = factor d `eq` 0
    factor = snd . P.factorise


-- TODO
smt :: (Ord f, Ord v, Show v, PP.Pretty v, MonadIO m) => SMTSolver -> SMTOpts -> Processor f Integer v m
smt _ _ [] = return NoProgress
smt solver opts cs = do
  getInterpretation >>= run solver >>= maybe fail success
  where
    -- TODO
    -- run Z3 inter = z3 (solveM inter opts cs)
    run MiniSmt inter = miniSMT (solveM inter opts cs)
    
    fail = return NoProgress
    success inter = modifyInterpretation (const inter) >> return (Progress [])

