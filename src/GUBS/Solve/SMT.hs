module GUBS.Solve.SMT ( smt, SMTSolver (..), PolyShape (..), SMTOpts (..), defaultSMTOpts ) where

import Data.List (subsequences,nub)
import Control.Arrow (second)
import Control.Monad (forM, forM_, liftM, when, unless, filterM, (>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, runStateT, get, put)
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
          , maxConst :: Maybe Int
          , minimize :: Bool }

defaultSMTOpts :: SMTOpts
defaultSMTOpts = SMTOpts { shape = MultMixed
                         , degree = 2
                         , maxCoeff = Nothing
                         , maxConst = Nothing
                         , minimize = True}

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
      assertGeq v 0
      let maxC = if null mono then maxConst opts else maxCoeff opts
      maybe (return ()) (\ ub -> assertGeq (fromIntegral ub) v) maxC
        -- unless (null mono) (assert (v `geq` 0)) -- TODO: problems with Z3
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

solveM :: (Ord f, Ord v, Solver s m, MonadTrace String m) => Interpretation f Integer -> SMTOpts -> ConstraintSystem f v -> SolverM s m (Maybe (Interpretation f Integer))
solveM inter opts cs = do
  (coeffs,ainter) <- flip runStateT (I.mapInter (fmap fromIntegral) inter) $ 
    forM cs $ \ c -> do
    l <- interpret opts (lhs c)
    r <- interpret opts (rhs c)
    let coeffs = P.coefficients (l - r) 
    (lift . constraint c) `mapM` coeffs
    return coeffs
  sat <- checkSat
  if sat
    then Just <$> refine (concat coeffs) ainter
    else return Nothing
  where
    constraint (_ :>=: _) d = d `assertGeq` 0
    constraint (_ :=: _) d = d `assertEq` 0
    refine coeffs ainter
      | not (minimize opts) = fromAssignment ainter 
      | otherwise           = fromAssignment ainter
                              -- >>= setZero
                              >>= minimizeCoeffs 5
          -- 
          -- forM_ bs $ \ (coeff,b) -> coeff `assertLeq` (fromIntegral b)
          -- loop 5 bs
      where
        -- vs = nub (concatMap P.variables coeffs)
        -- setZero inter = do
        --   vs' <- filterM (getValue >=> \ w -> return (w > 0)) vs
        --   push
        --   assert [ EQC (lit v) 0 | v <- vs']
        --   sat <- checkSat
        --   if sat
        --    then fromAssignment ainter >>= setZero
        --    else pop >> return inter
             
        
        minimizeCoeffs 0 inter = return inter
        minimizeCoeffs n inter = do
          bs <- filter (\ (_,b) -> b > 0) <$> sequence [ (,) coeff <$> evalM coeff | coeff <- coeffs ]
          if null bs
            then return inter
            else do 
            forM_ bs $ \ (coeff,b) -> coeff `assertLeq` (fromIntegral b)
            assert [ GEQC (fromIntegral b) (toSolverExp (coeff + 1)) | (coeff,b) <- bs ] -- TODO; interface broken
            sat <- checkSat
            if sat
              then do
               fromAssignment ainter >>= minimizeCoeffs (n-1)
              else return inter
          
      
         
smt :: (Ord f, Ord v, MonadIO m) => SMTSolver -> SMTOpts -> Processor f Integer v m
smt _ _ [] = return NoProgress
smt solver opts cs = do
  getInterpretation >>= run solver >>= maybe fail success
  where
    run Z3 inter = z3 (solveM inter opts cs)
    run MiniSmt inter = miniSMT (solveM inter opts cs)
    
    fail = return NoProgress
    success inter = modifyInterpretation (const inter) >> return (Progress [])

