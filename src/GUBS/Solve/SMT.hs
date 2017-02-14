module GUBS.Solve.SMT
  ( smt
  -- * options
  , SMTSolver (..)
  , PolyShape (..)
  , SMTOpts (..)
  , Solver (..)
  , defaultSMTOpts
  -- * minimisation strategies
  , zeroOut
  , decreaseCoeffs
  , shiftMax
  , tryM
  , iterM
  , andThenM
  , exhaustiveM
  , noneM
  ) where

import Data.Maybe (fromJust)
import Data.List (subsequences,nub, (\\))
import Control.Arrow (second)
import Control.Applicative ((<|>))
import Control.Monad (forM, foldM, forM_, liftM, when, unless, filterM, (>=>), replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, runStateT, execStateT, evalStateT, get, put, modify)
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
import           GUBS.Solve.Strategy (Processor, Interpretation, Result (..), modifyInterpretation, getInterpretation, liftTrace)
import           GUBS.Solver
import qualified GUBS.Solver.Formula as F

import qualified Text.PrettyPrint.ANSI.Leijen as PP

-- options
----------------------------------------------------------------------

data Solver = MiniSmt | Z3 deriving (Show)

data PolyShape = Mixed | MultMixed deriving (Eq, Show)

data MM = ZeroOut | DecreaseCoeff | ShiftMax deriving (Show)

data SMTMinimization = SMTMinimize MM | SMTSequence SMTMinimization SMTMinimization | Try SMTMinimization | Success

zeroOut :: SMTMinimization
zeroOut = SMTMinimize ZeroOut

shiftMax :: SMTMinimization
shiftMax = SMTMinimize ShiftMax

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
          , minimize = tryM (iterM 3 zeroOut) `andThenM` iterM 3 decreaseCoeffs }

-- encoding 
----------------------------------------------------------------------

type AbstractCoefficient s = SMTExpression s
type AbstractMaxPoly s v = MP.MaxPoly v (AbstractCoefficient s)
type AbstractPoly s v = P.Polynomial v (AbstractCoefficient s)
type AbstractInterpretation s f = I.Interpretation f (AbstractCoefficient s)

type SMT s f a = StateT (SMTOpts, Interpretation f Integer,AbstractInterpretation s f) (TraceT String (SolverM s)) a

liftSMT :: SMTSolver s => SolverM s a -> SMT s f a
liftSMT = lift . lift

freshCoeff :: SMTSolver s => Maybe Int -> SMT s f (AbstractCoefficient s)
freshCoeff mub = liftSMT $ do
  v <- E.variable <$> freshNat
  whenJust mub $ \ ub -> assert (fromNatural ub `smtGeq` v)
  return v

getOpts :: SMTSolver s => SMT s f SMTOpts
getOpts = do (opts,_,_) <- get; return opts

getAInter :: SMTSolver s => SMT s f (AbstractInterpretation s f)
getAInter = do (_,_,ai) <- get; return ai

getCInter :: SMTSolver s => SMT s f (Interpretation f Integer)
getCInter = do (_,ci,_) <- get; return ci


-- SMTOpts { .. }
freshPoly :: SMTSolver s => Int -> SMT s f (AbstractMaxPoly s I.Var)
freshPoly ar = do
  SMTOpts {..} <- getOpts
  let t = template shape degree
  if maxPoly
    then do
    ps <- replicateM (min ar 3) (polyFromTemplate t)
    liftSMT (exclusive ps)
    return (maximumA [toMaxPoly p | p <- ps])
    else toMaxPoly <$> polyFromTemplate t
  where
    template MultMixed degree = [ [ (v,1) | v <- ms] | ms <- subsequences (take ar I.variables)
                                                     , length ms <= degree ]
    template Mixed degree = concat (template' degree) where 
      template' 0 = [[[]]]
      template' d = [ v `mult` mono | mono <- lead, v <- take ar I.variables ] : ps
        where
          ps@(lead:_) = template' (d - 1)
          v `mult` [] = [(v,1)]
          v `mult` ((v',i) : mono)
            | v == v' = (v',i+1) : mono
            | otherwise = (v',i) : v `mult` mono

    exclusive ps = assert $ 
      smtBigAnd [ smtAll (smtEq zero . fst) ms `smtOr` smtAny dominating ms
                | p <- ps, let ms = [(c,m) | (c,m) <- P.toMonos p, m /= P.unitMono]]
      where
        dominating (c,m) = smtBigAnd [ c `smtGeq` (c' .+ fromNatural 1) | c' <- coeffs M.! m, c /= c' ]
        coeffs = M.unionsWith (++) [ (: []) `fmap` P.toMonoMap p | p <- ps]
      
      -- [ (c1 `smtEq` fromNatural 0) `smtOr` (c2 `smtEq` fromNatural 0)
      -- -| (c1,m) <- P.toMonos p1, not (null (P.toPowers m)), let c2 = P.coefficientOf m p2]
    toMaxPoly = P.fromPolynomial MP.variable MP.constant
    polyFromTemplate tp = do
      SMTOpts {..} <- getOpts
      sumA <$> sequence [ toP mono <$> freshCoeff (if null mono then maxConst else maxCoeff) | mono <- tp]
      where toP mono c = P.coefficient c .* prod [P.variable v .^ i | (v,i) <- mono]
            

interpret :: (Ord f, Ord v, SMTSolver s) => Term f v -> SMT s f (AbstractMaxPoly s v)
interpret = T.interpretM (return . MP.variable) i where
  i f as = I.apply <$> getPoly <*> return as
    where
      ar = length as                             
      getPoly = do
        inter <- getCInter
        ainter <- getAInter
        case (fmap fromNatural `fmap` I.get inter f ar) <|> I.get ainter f ar of
          Nothing -> addFreshPoly
          Just p -> return p
      addFreshPoly = do
        p <- freshPoly ar
        modify (\ (opts,inter,ainter) -> (opts,inter,I.insert ainter f ar p))
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
                                             


fromAssignment :: SMTSolver s => AbstractInterpretation s f -> SMT s f (Interpretation f Integer)
fromAssignment i = I.mapInter MP.simp <$> liftSMT (traverse evalM i)

minimizeM :: (PP.Pretty f, Ord f, Ord v, SMTSolver s) => ConstraintSystem f v -> SMTMinimization -> SMT s f (Interpretation f Integer)
minimizeM cs ms = fst <$> (stateFromModel >>= execStateT (logInter >> walkS ms)) where

  stateFromModel = do
    ainter <- getAInter
    let cs = concatMap MP.coefficients (I.image ainter)
    (,) <$> fromAssignment ainter <*> sequence [ (,) c <$> liftSMT (evalM c) | c <- cs ]
  
  getCurrentInterpretation = fst <$> get
  getCurrentCoefficients   = snd <$> get
  
  walkS Success          = return True
  walkS (Try m)          = walkS m >> return True
  walkS (SMTMinimize mm) = minimizeWith mm
  walkS (SMTSequence m1 m2) = do
    cont <- walkS m1
    if cont then walkS m2 else return False
    
  minimizeWith mm = do
    cstrt <- constraintWith mm
    success <- lift $ liftSMT $ push >> assert cstrt >> checkSat
    if success
      then do
        lift stateFromModel >>= put
        lift $ logMsg (PP.text "Minimizing with " PP.<+> PP.text (show mm) PP.<+> PP.text  "...")
        logInter
      else lift (logMsg ("Minimizing with " ++ show mm ++ "... Failed" ))
    lift $ liftSMT pop
    return success

  constraintWith ZeroOut = do
    bs <- getCurrentCoefficients
    return $ smtBigAnd [ smtBigAnd [ fromNatural v `smtGeq` c | (c,v) <- bs ]
                       , smtBigOr [ zero `smtEq` c | (c,v) <- bs, v > 0 ] ]
  constraintWith DecreaseCoeff =  do
    bs <- getCurrentCoefficients
    return $ smtBigAnd [ smtBigAnd [ fromNatural v `smtGeq` c | (c,v) <- bs ]
                       , smtBigOr [ fromNatural (v - 1) `smtGeq` c | (c,v) <- bs, v > 0 ] ]
  constraintWith ShiftMax = do
    ainter <- lift getAInter
    cs <- getCurrentCoefficients
    let coeffVal c = fromJust (lookup c cs)
        coeffs p = [ (c,m) | (c,m) <- P.toMonos p, coeffVal c > 0]
        candidateShift p ps =
              [ (c,c') | let csp = coeffs p
                       , (c,m) <- csp
                       , q <- ps
                       , let c' = P.coefficientOf m q
                       , let csq = coeffs q
                       , length csp - 1 >= length csq + if coeffVal c' == 0 then 1 else 0 ]
        candidates = [ (c,c',ps) | p <- I.image ainter, let ps = MP.splitMax p, p <- ps, (c,c') <- candidateShift p ps ]
        maybeShift (c,c',ps) =
          smtBigAnd [ smtBigOr [ smtBigAnd [c `smtEq` zero, fromNatural (coeffVal c' `max` coeffVal c) `smtGeq` c']
                               , fromNatural (coeffVal c') `smtGeq` c' ]
                    , smtBigAnd [ fromNatural (coeffVal d) `smtGeq` d
                                | (d,m') <- concatMap P.toMonos ps, d `notElem` [c'], m' /= P.unitMono]
                    ]
    return $ smtBigAnd [ smtAny (\ (c,_,_) -> c `smtEq` zero) candidates
                       , smtAll maybeShift candidates ]

  logInter = I.toList <$> getCurrentInterpretation >>= logBlk "Interpretation" . mapM_ logBinding where
    logBinding ((f,i),p) = logMsg (PP.pretty f PP.<> ppArgs i PP.<+> PP.text "=" PP.<+> PP.pretty p)
    ppArgs i = PP.parens (PP.hcat (PP.punctuate (PP.text ",") [PP.pretty v | v <- take i I.variables]))        

solveM :: (PP.Pretty f, PP.Pretty v, PP.Pretty (NLiteral s), Ord f, Ord v, SMTSolver s) => ConstraintSystem f v -> SMT s f (Maybe (Interpretation f Integer))
solveM cs = do
  let assertIeq = liftSMT . assert . F.subst dio . maxElim
  sequence_ [assertIeq =<< (:>=:) <$> interpret l <*> interpret r | l :>=: r <- cs ]
  mo <- minimize <$> getOpts
  ifM (liftSMT checkSat) (Just <$> minimizeM cs mo) (return Nothing)
  where
    dio (Geq l r) = smtBigAnd [ p `smtGeq` q | (p :>=: q) <- P.absolutePositive (l `P.minus` r) ]


smt :: (PP.Pretty f, PP.Pretty v, Ord f, Ord v, MonadIO m) => Solver -> SMTOpts -> Processor f Integer v m
smt _ _ [] = return NoProgress
smt solver opts cs = do
  inter <- getInterpretation
  let run Z3 = mapTraceT (liftIO . z3)           $ evalStateT (solveM cs) (opts,inter,I.empty)
      run MiniSmt = mapTraceT (liftIO . miniSMT) $ evalStateT (solveM cs) (opts,inter,I.empty)
  mi <- liftTrace (run solver)
  case mi of
    Nothing -> return NoProgress
    Just inter' -> modifyInterpretation (I.union inter') >> return (Progress [])

