module GUBS.Solve.SMT
  ( smt
  -- * options
  , SMTSolver (..)
  , PolyShape (..)
  , SMTOpts (..)
  , defaultSMTOpts
  -- * minimisation strategies
  , zeroOut
  , decreaseCoeffs
  , tryM
  , iterM
  , andThenM
  , exhaustiveM
  , noneM
  ) where

import Data.List (subsequences,nub, (\\))
import Control.Arrow (second)
import Control.Applicative ((<|>))
import Control.Monad (forM, foldM, forM_, liftM, when, unless, filterM, (>=>), replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, runStateT, execStateT, get, put, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Trace
import qualified Data.Map as M

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
import           GUBS.Solve.Strategy (Processor, Interpretation, Result (..), modifyInterpretation, getInterpretation)
import           GUBS.Solver hiding (SMTSolver)
import qualified GUBS.Solver.Class as SMT
import qualified GUBS.Solver.Formula as F

-- TODO remove
import GUBS.Utils (tracePretty)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

-- options
----------------------------------------------------------------------

data SMTSolver = MiniSmt | Z3 deriving (Show)

data PolyShape = Mixed | MultMixed deriving (Eq, Show)

data MM = ZeroOut | DecreaseCoeff deriving (Show)

data SMTMinimization = SMTMinimize MM | SMTSequence SMTMinimization SMTMinimization | Try SMTMinimization | Success

zeroOut :: SMTMinimization
zeroOut = SMTMinimize ZeroOut

decreaseCoeffs :: SMTMinimization
decreaseCoeffs = SMTMinimize DecreaseCoeff

tryM :: SMTMinimization -> SMTMinimization
tryM = Try

andThenM :: SMTMinimization -> SMTMinimization -> SMTMinimization
andThenM = SMTSequence

iterM :: Int -> SMTMinimization -> SMTMinimization
iterM i s | i <= 0 = Success
         | otherwise = s `andThenM` tryM (iterM (i-1) s)

exhaustiveM :: SMTMinimization -> SMTMinimization
exhaustiveM s = s `andThenM` tryM (exhaustiveM s)

noneM :: SMTMinimization
noneM = Success

data SMTOpts =
  SMTOpts { shape    :: PolyShape
          , degree   :: Int 
          , maxCoeff :: Maybe Int
          , maxConst :: Maybe Int          
          , maxPoly  :: Bool
          , minimize :: SMTMinimization }

defaultSMTOpts :: SMTOpts
defaultSMTOpts =
  SMTOpts { shape = MultMixed
          , degree = 2
          , maxCoeff = Nothing
          , maxConst = Nothing          
          , maxPoly  = False
          , minimize = tryM zeroOut `andThenM` iterM 3 decreaseCoeffs }

-- encoding 
----------------------------------------------------------------------

type AbstractCoefficient s = SMTExpression s
type AbstractMaxPoly s v = MP.MaxPoly v (AbstractCoefficient s)
type AbstractPoly s v = P.Polynomial v (AbstractCoefficient s)
type AbstractInterpretation s f = I.Interpretation f (AbstractCoefficient s)


freshCoeff :: (Solver s m) => Maybe Int -> SolverM s m (AbstractCoefficient s)
freshCoeff mub = do
  v <- E.variable <$> SMT.freshNat
  whenJust mub $ \ ub -> assert (fromNatural ub `smtGeq` v)
  return v


freshPoly :: (Solver s m) => SMTOpts -> Int -> SolverM s m (AbstractMaxPoly s I.Var)
freshPoly SMTOpts { .. } ar
  | maxPoly   = do
      p1 <- polyFromTemplate template
      p2 <- polyFromTemplate template
      exclusive p1 p2
      return (toMaxPoly p1 `maxA` toMaxPoly p2)
  | otherwise = toMaxPoly <$> polyFromTemplate template 
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

    exclusive p1 p2 = assert $ smtBigAnd
      [ (c1 `smtEq` fromNatural 0) `smtOr` (c2 `smtEq` fromNatural 0)
      | (m,c1) <- M.toList (P.toMonoMap p1), let Just c2 = M.lookup m (P.toMonoMap p2) ]
    toMaxPoly = P.fromPolynomial MP.variable MP.constant
    polyFromTemplate tp =
      sumA <$> sequence [ toP mono <$> freshCoeff (if null mono then maxConst else maxCoeff) | mono <- tp]
      where toP mono c = P.coefficient c .* prod [P.variable v .^ i | (v,i) <- mono]
            

interpret :: (Solver s m, Ord f, Ord v) => SMTOpts -> Term f v -> StateT (Interpretation f Integer,AbstractInterpretation s f) (SolverM s m) (AbstractMaxPoly s v)
interpret opts = T.interpretM (return . MP.variable) i where
  i f as = I.apply <$> getPoly <*> return as
    where
      ar = length as                             
      getPoly = do
        (inter,ainter) <- get
        case (fmap fromNatural `fmap` I.get inter f ar) <|> I.get ainter f ar of
          Nothing -> addFreshPoly
          Just p -> return p
      addFreshPoly = do
        p <- lift (freshPoly opts ar)
        modify (\ (inter,ainter) -> (inter,I.insert ainter f ar p))
        return p

maxElim :: (Ord v, IsNat c, SemiRing c) => Constraint (MP.MaxPoly v c) -> Formula l (P.Polynomial v c)
maxElim = smtBigAnd . map elimLhs . elimRhs
  where
    elimRhs (l :>=: r) = [ (l,r') | r' <- MP.splitMax r ]
    elimLhs (l,r) = smtBigOr [ Atom (l' `Geq` r) | l' <- MP.splitMax l ]

    
-- condElim :: (Ord v, Solver s m) => ConditionalConstraint (AbstractPoly s v) -> SolverM s m (Constraint (AbstractPoly s v))
-- condElim CC { .. }
--   | null premises = return constraint
--   | otherwise = do
--       eliminateCond constraint <$> replicateM (length premises) (freshNat (Just 1))
--         where
--           eliminateCond (p :>=: q) cs = l :>=: r where
--             -- heuristic from Maximal Termination Paper, footnote 15
--             l = p .+ sumA [ P.coefficient ci .* qi | (ci,_ :>=: qi) <- zip cs premises]
--             r = q .+ sumA [ P.coefficient ci .* pi | (ci,pi :>=: _) <- zip cs premises]
                                             


fromAssignment :: (Solver s m) => AbstractInterpretation s f -> SolverM s m (Interpretation f Integer)
fromAssignment i = I.mapInter MP.simp <$> traverse evalM i

minimizeM :: (PP.Pretty f, Ord f, Ord v, Solver s m, MonadTrace String m) => ConstraintSystem f v -> AbstractInterpretation s f -> SMTMinimization -> SolverM s m (Interpretation f Integer)
minimizeM cs ainter ms = fst <$> (stateFromModel >>= execStateT (walkS ms)) where

  walkS Success          = return True
  walkS (Try m)          = walkS m >> return True
  walkS (SMTMinimize mm) = minimizeWith mm
  walkS (SMTSequence m1 m2) = do
    cont <- walkS m1
    if cont then walkS m2 else return False
    
  minimizeWith mm = do
    cinter <- getCurrentInterpretation
    lift push
    lift (lift (logMsg (PP.text "current interpretation:" PP.<$$> PP.indent 3 (PP.align (PP.pretty cinter)))
                >> logMsg ("minimizing with " ++ show mm ++ " ...")))
    constraintWith mm >>= lift . assert
    ifM check (return True) (lift pop >> return False)

  constraintWith DecreaseCoeff =  do
    cinter <- getCurrentInterpretation
    return (fromNatural (coeffSum cinter - 1) `smtGeq` coeffSum ainter)
    where 
      coeffSum i = sumA [ sumA (MP.coefficients p) | (fi,p) <- I.toList i]
  constraintWith ZeroOut = do
    bs <- getCurrentCoefficients
    return $ smtBigAnd [ fromNatural v `smtGeq` c | (c,v) <- bs ]
             `smtAnd` smtBigOr [ zero `smtEq` c | (c,v) <- bs, v > 0 ]

  getCurrentInterpretation = fst <$> get
  getCurrentCoefficients   = snd <$> get

  stateFromModel = (,) <$> fromAssignment ainter <*> sequence [ (,) c <$> evalM c | c <- cs ]
  
  cs = concatMap MP.coefficients (I.image ainter)

  check = ifM (lift checkSat) (lift stateFromModel >>= put >> return True) (return False)
    

solveM :: (PP.Pretty f, Ord f, Ord v, Solver s m, MonadTrace String m) => Interpretation f Integer -> SMTOpts -> ConstraintSystem f v -> SolverM s m (Maybe (Interpretation f Integer))
solveM inter opts cs = do
  (ieqs,(_,ainter)) <- flip runStateT (inter,I.empty) $ sequence $
    [(:>=:) <$> interpret opts l <*> interpret opts r | (l :>=: r) <- cs ]
  mapM_ (assert . F.subst dio . maxElim) ieqs
  ifM checkSat (Just <$> I.union inter <$> minimizeM cs ainter (minimize opts)) (return Nothing)
  where
    dio (Geq l r) = smtBigAnd [ (p `smtGeq` q) | (p :>=: q) <- P.strictlyPositive (l `P.minus` r) ]
         
smt :: (PP.Pretty f, PP.Pretty v, Ord f, Ord v, MonadIO m) => SMTSolver -> SMTOpts -> Processor f Integer v m
smt _ _ [] = return NoProgress
smt solver opts cs = do
  getInterpretation >>= run solver >>= maybe fail success
  where
    run Z3 inter = z3 (solveM inter opts cs)
    run MiniSmt inter = miniSMT (solveM inter opts cs)
    
    fail = return NoProgress
    success inter = modifyInterpretation (const inter) >> return (Progress [])

