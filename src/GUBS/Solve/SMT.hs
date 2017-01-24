module GUBS.Solve.SMT ( smt, SMTSolver (..), PolyShape (..), SMTOpts (..), defaultSMTOpts ) where

import Data.List (subsequences,nub)
import Control.Arrow (second)
import Control.Monad (forM, foldM, forM_, liftM, when, unless, filterM, (>=>), replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, runStateT, get, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Trace

import           GUBS.Algebra
import           GUBS.Utils
import           GUBS.Term (Term (..))
import qualified GUBS.Term as T
import qualified GUBS.Expression as E
import qualified GUBS.Interpretation as I
import qualified GUBS.MaxPolynomial as MP
import qualified GUBS.Polynomial as P
import           GUBS.Constraint (Constraint (..), ConditionalConstraint (..), lhs, rhs)
import           GUBS.ConstraintSystem (ConstraintSystem)
import           GUBS.Solve.Strategy
import           GUBS.Solver hiding (SMTSolver)
import qualified GUBS.Solver.Class as SMT

-- TODO remove
import GUBS.Utils (tracePretty)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data SMTSolver = MiniSmt | Z3 deriving (Show)

type AbstractCoefficient s = E.Expression (Literal s) -- TODO: better literals (One, Boolean, ArbitraryNat,...)
type AbstractMaxPoly s v = MP.MaxPoly v (AbstractCoefficient s)
type AbstractPoly s v = P.Polynomial v (AbstractCoefficient s)
type AbstractInterpretation s f = I.Interpretation f (AbstractCoefficient s)

data PolyShape = Mixed | MultMixed
  deriving (Eq, Show)

data SMTOpts =
  SMTOpts { shape    :: PolyShape
          , degree   :: Int 
          , maxCoeff :: Maybe Int
          , maxConst :: Maybe Int          
          , maxPoly  :: Bool
          , minimize :: Bool }

defaultSMTOpts :: SMTOpts
defaultSMTOpts =
  SMTOpts { shape = MultMixed
          , degree = 2
          , maxCoeff = Nothing
          , maxConst = Nothing          
          , maxPoly  = True
          , minimize = True}


freshNat :: (Solver s m) => Maybe Int -> SolverM s m (AbstractCoefficient s)
freshNat mub = do
  v <- E.variable <$> fresh
  assert (v `smtGeq` fromNatural 0)
  whenJust mub $ \ ub -> assert (fromNatural ub `smtGeq` v)
  return v


freshPoly :: (Solver s m) => SMTOpts -> Int -> SolverM s m (AbstractMaxPoly s I.Var)
freshPoly SMTOpts { .. } ar
  | maxPoly   = maxA <$> polyFromTemplate template <*> polyFromTemplate template
  | otherwise = polyFromTemplate template 
  where
    template | shape == MultMixed = [ [ (v,1) | v <- ms] | ms <- subsequences (take ar I.variables)
                                                         , length ms <= degree ]
             | shape == Mixed     = concat (template' degree) where 
      template' 0 = [[[]]]
      template' d = [ v `mult` mono | mono <- lead, v <- take ar I.variables ] : ps
        where
          ps@(lead:_) = template' (d - 1)
          v `mult` [] = [(v,1)]
          v `mult` ((v',i) : mono)
            | v == v' = (v',i+1) : mono
            | otherwise = (v',i) : v `mult` mono

    polyFromTemplate tp =
      sumA <$> sequence [ toMP mono <$> freshNat (if null mono then maxConst else maxCoeff) | mono <- tp]
      where toMP mono c = MP.constant c .* prod [MP.variable v .^ i | (v,i) <- mono]
            

interpret :: (Solver s m, Ord f, Ord v) => SMTOpts -> Term f v -> StateT (AbstractInterpretation s f) (SolverM s m) (AbstractMaxPoly s v)
interpret opts = T.interpretM (return . MP.variable) i where
  i f as = I.apply <$> getPoly <*> return as
    where
      ar = length as                             
      getPoly = do
        ainter <- get
        maybe addFreshPoly return (I.get ainter f ar)
      addFreshPoly = do
        p <- lift (freshPoly opts ar)
        modify (\ ainter -> I.insert ainter f ar p)
        return p


condElim :: (Ord v, Solver s m) => ConditionalConstraint (AbstractPoly s v) -> SolverM s m (Constraint (AbstractPoly s v))
condElim CC { .. }
  | null premises = return constraint
  | otherwise = do
      eliminateCond constraint <$> replicateM (length premises) (freshNat (Just 1))
        where
          eliminateCond (p :>=: q) cs = l :>=: r where
            -- heuristic from Maximal Termination Paper, footnote 15
            l = p .+ sumA [ P.coefficient ci .* qi | (ci,_ :>=: qi) <- zip cs premises]
            r = q .+ sumA [ P.coefficient ci .* pi | (ci,pi :>=: _) <- zip cs premises]
                                           

data AbstractCoefficientDiff s =
  ACDiff { posAC :: E.Expression (Literal s)
         , negAC :: E.Expression (Literal s) }

instance IsNat (AbstractCoefficientDiff s) where
  fromNatural_ n = ACDiff { posAC = fromNatural_ n, negAC = E.zero }

type AbstractDiffPoly s v = P.Polynomial v (AbstractCoefficientDiff s)

toDiff :: AbstractCoefficient s -> AbstractCoefficientDiff s
toDiff c = ACDiff { posAC = c, negAC = E.zero }

toDiffPoly :: AbstractPoly s v -> AbstractDiffPoly s v
toDiffPoly = fmap toDiff 

negateDiff :: AbstractCoefficientDiff s -> AbstractCoefficientDiff s
negateDiff p = ACDiff { posAC = negAC p, negAC = posAC p}

instance SMT.SMTSolver s => Additive (AbstractCoefficientDiff s) where
  zero = toDiff E.zero
  d1 .+ d2 = ACDiff { posAC = posAC d1 .+ posAC d2
                    , negAC = negAC d1 .+ negAC d2 }

instance SMT.SMTSolver s => AdditiveGroup (AbstractCoefficientDiff s) where
  neg = negateDiff
  

fromAssignment :: (Solver s m) => AbstractInterpretation s f -> SolverM s m (Interpretation f Integer)
fromAssignment = traverse evalM

solveM :: (PP.Pretty v, PP.Pretty (Literal s), Ord f, Ord v, Solver s m, MonadTrace String m) => Interpretation f Integer -> SMTOpts -> ConstraintSystem f v -> SolverM s m (Maybe (Interpretation f Integer))
solveM inter opts cs = do
  (cconstrs,ainter) <- flip runStateT (I.mapInter (fmap fromNatural) inter) $
    foldM collectCoefficientConstraints [] cs
  assert (smtBigAnd [ p `smtGeq` n | p :>=: n <- cconstrs])
  sat <- checkSat  
  if sat
    then Just <$> refine cconstrs ainter
    else return Nothing

    where
      toCoefficientConstraints (il' :>=: ir') =
        [ (posAC p :>=: negAC p) | p <- P.coefficients (toDiffPoly il' .- toDiffPoly ir') ]      
      
      collectCoefficientConstraints ccs (l :>=: r) = do
        il <- interpret opts l
        ir <- interpret opts r
        let mcs = MP.maxElim (il :>=: ir)
        cs <- forM mcs $ \cc -> lift $ lift (logMsg cc) >> condElim cc
        return (foldr ((++) . toCoefficientConstraints) ccs cs)

      refine cconstrs ainter
        | not (minimize opts) = fromAssignment ainter 
        | otherwise           = fromAssignment ainter >>= minimizeCoeffs 5
        where
          minimizeCoeffs 0 inter = return inter
          minimizeCoeffs n inter = do
            bs <- filter (\ (_,b) -> b > 0) <$> sequence [ (,) coeff <$> evalM coeff | (coeff :>=: _) <- cconstrs ]
            if null bs
              then return inter
              else do
              assert (smtBigAnd [ fromNatural b     `smtGeq` coeff | (coeff,b) <- bs ])
              assert (smtBigOr  [ fromNatural (b-1) `smtGeq` coeff | (coeff,b) <- bs ])
              sat <- checkSat
              if sat
                then fromAssignment ainter >>= minimizeCoeffs (n-1)
                else return inter
          
        -- vs = nub (concatMap P.variables coeffs)
        -- setZero inter = do
        --   vs' <- filterM (getValue >=> \ w -> return (w > 0)) vs
        --   push
        --   assert [ EQC (lit v) 0 | v <- vs']
        --   sat <- checkSat
        --   if sat
        --    then fromAssignment ainter >>= setZero
        --    else pop >> return inter
             
      
         
smt :: (PP.Pretty v, Ord f, Ord v, MonadIO m) => SMTSolver -> SMTOpts -> Processor f Integer v m
smt _ _ [] = return NoProgress
smt solver opts cs = do
  getInterpretation >>= run solver >>= maybe fail success
  where
    run Z3 inter = z3 (solveM inter opts cs)
    run MiniSmt inter = miniSMT (solveM inter opts cs)
    
    fail = return NoProgress
    success inter = modifyInterpretation (const inter) >> return (Progress [])

