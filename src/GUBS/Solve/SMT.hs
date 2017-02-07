module GUBS.Solve.SMT ( smt, SMTSolver (..), PolyShape (..), Minimization (..), SMTOpts (..), defaultSMTOpts ) where

import Data.List (subsequences,nub, (\\))
import Control.Arrow (second)
import Control.Applicative ((<|>))
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
import qualified GUBS.Solver.Formula as F

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

data Minimization = MinimizeFull | MinimizeIterate Int

data SMTOpts =
  SMTOpts { shape    :: PolyShape
          , degree   :: Int 
          , maxCoeff :: Maybe Int
          , maxConst :: Maybe Int          
          , maxPoly  :: Bool
          , minimize :: Minimization }

defaultSMTOpts :: SMTOpts
defaultSMTOpts =
  SMTOpts { shape = MultMixed
          , degree = 2
          , maxCoeff = Nothing
          , maxConst = Nothing          
          , maxPoly  = True
          , minimize = MinimizeFull }


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

maxElim :: (Ord v, IsNat c, SemiRing c) => Constraint (MP.MaxPoly v c) -> Formula (P.Polynomial v c)
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

solveM :: (PP.Pretty f, Ord (Literal s), Ord f, Ord v, Solver s m, MonadTrace String m) => Interpretation f Integer -> SMTOpts -> ConstraintSystem f v -> SolverM s m (Maybe (Interpretation f Integer))
solveM inter opts cs = do
  (ieqs,(_,ainter)) <- flip runStateT (inter,I.empty) $ sequence $
    [(:>=:) <$> interpret opts l <*> interpret opts r | (l :>=: r) <- cs ]
  mapM_ (assert . F.subst dio . maxElim) ieqs
  ifM checkSat (Just <$> I.union inter <$> minimizeInter (minimize opts) ainter) (return Nothing)
  where
    dio (Geq l r) = smtBigAnd [ (p `smtGeq` q) | (p :>=: q) <- P.strictlyPositive (l `P.minus` r) ]

    minimizeInter (MinimizeIterate n) ainter | n <= 0 = fromAssignment ainter
    minimizeInter mo ainter = do
      cinter <- fromAssignment ainter
      let w = metric cinter - 1
      lift (logMsg (PP.text "interpretation:" PP.<+> PP.align (PP.pretty cinter)))
      lift (logMsg ("minimizing weight " ++ show w ++ " ..."))
      assert (fromNatural (w) `smtGeq` metric ainter)
      ifM checkSat (minimizeInter mo' ainter) (return cinter)
      where
        mo' = case mo of { MinimizeFull -> MinimizeFull; MinimizeIterate i -> MinimizeIterate (i - 1) }

        metric i = sumA [ m p | (fi,p) <- I.toList i] where
          m p = sumA (MP.coefficients p)
        --   m (MP.Var v)    = fromNatural 0
        --   m (MP.Const c)  = c
        --   m (MP.Plus p q) = m p .+ m q
        --   m (MP.Mult p q) = m p .+ m q
        --   m (MP.Max p q)  = fromNatural 1 .+ (m p .+ m q)
      
  -- do
  -- (cconstrs,ainter) <- flip runStateT (I.mapInter (fmap fromNatural) inter) $
  --   foldM collectCoefficientConstraints [] cs
  -- assert (smtBigAnd [ p `smtGeq` n | p :>=: n <- cconstrs])
  -- sat <- checkSat  
  -- if sat
  --   then Just <$> refine cconstrs ainter
  --   else return Nothing

  --   where
  --     toCoefficientConstraints (il' :>=: ir') =
  --       [ (posAC p :>=: negAC p) | p <- P.coefficients (toDiffPoly il' .- toDiffPoly ir') ]      
      
  --     collectCoefficientConstraints ccs (l :>=: r) = do
  --       il <- interpret opts l
  --       ir <- interpret opts r
  --       let mcs = MP.maxElim (il :>=: ir)
  --       -- cs <- forM mcs $ \cc -> lift (condElim cc)
  --       lift (lift (logMsg (il :>=: ir)))
  --       cs <- forM mcs $ \cc -> lift $ lift (logMsg cc) >> condElim cc
  --       return (foldr ((++) . toCoefficientConstraints) ccs cs)

  --     refine cconstrs ainter = fromAssignment ainter >>= minimizeCoeffs (minimize opts)
  --       where
  --         minimizeCoeffs (MinimizeIterate n) inter | n <= 0 = return inter
  --         minimizeCoeffs mo inter = do
  --           bs <- filter (\ (_,b) -> b > 0) <$> sequence [ (,) coeff <$> evalM coeff | (coeff :>=: _) <- cconstrs ]
  --           if null bs
  --             then return inter
  --             else do
  --             lift (logMsg ("minimizing " ++ show (length bs) ++ " candidates.."))
  --             assert (smtBigAnd [ fromNatural b     `smtGeq` coeff | (coeff,b) <- bs ])
  --             assert (smtBigOr  [ fromNatural (b-1) `smtGeq` coeff | (coeff,b) <- bs ])
  --             sat <- checkSat
  --             if sat
  --               then fromAssignment ainter >>= minimizeCoeffs (pred mo)
  --               else return inter
  --               where pred MinimizeFull = MinimizeFull
  --                     pred (MinimizeIterate i) = MinimizeIterate (i - 1)
  --       -- vs = nub (concatMap P.variables coeffs)
  --       -- setZero inter = do
  --       --   vs' <- filterM (getValue >=> \ w -> return (w > 0)) vs
  --       --   push
  --       --   assert [ EQC (lit v) 0 | v <- vs']
  --       --   sat <- checkSat
  --       --   if sat
  --       --    then fromAssignment ainter >>= setZero
  --       --    else pop >> return inter
             
      
         
smt :: (PP.Pretty f, PP.Pretty v, Ord f, Ord v, MonadIO m) => SMTSolver -> SMTOpts -> Processor f Integer v m
smt _ _ [] = return NoProgress
smt solver opts cs = do
  getInterpretation >>= run solver >>= maybe fail success
  where
    run Z3 inter = z3 (solveM inter opts cs)
    run MiniSmt inter = miniSMT (solveM inter opts cs)
    
    fail = return NoProgress
    success inter = modifyInterpretation (const inter) >> return (Progress [])

