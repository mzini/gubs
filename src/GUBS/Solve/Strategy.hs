module GUBS.Solve.Strategy (
  Interpretation
  , ProcT
  , Processor
  , Result (..)
  , ExecutionLog
  , run
  , logMsg
  , logBlk                       
  , (==>)
  , (<=>)
  , abort
  , try
  , exhaustive
  , getInterpretation
  , modifyInterpretation
  , logConstraints
  , logOpenConstraints
  , logInterpretation
  ) where


import           Data.Maybe (isNothing,fromMaybe)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           Control.Monad.State
import           Control.Monad.Trace
import           Data.Tree (Forest)
import qualified GUBS.Polynomial as Poly
import           GUBS.Interpretation hiding (get)
import           GUBS.CS

type ExecutionLog = Forest String
                           
newtype ProcT f c m a = ProcT { runProcT_ :: StateT (Interpretation f c) (TraceT String m) a }
  deriving (Applicative, Functor, Monad, MonadState (Interpretation f c), MonadTrace String, MonadIO)


instance MonadTrans (ProcT f c) where lift = ProcT . lift . lift

run :: Monad m => Interpretation f c -> ProcT f c m a -> m (a, Interpretation f c, ExecutionLog)
run i = liftM (\((a,i),l) -> (a,i,l)) . runTraceT .  flip runStateT i . runProcT_

getInterpretation :: Monad m => ProcT f c m (Interpretation f c)
getInterpretation = get

modifyInterpretation :: Monad m => (Interpretation f c -> Interpretation f c) -> ProcT f c m ()
modifyInterpretation = modify

renderPretty :: PP.Pretty e => e -> String
renderPretty d = PP.displayS (PP.renderSmart 1.0 200 (PP.pretty d)) ""
                             
logMsg :: MonadTrace String m => PP.Pretty e => e -> m ()
logMsg = trace . renderPretty

logBlk :: MonadTrace String m => PP.Pretty e => e -> m a -> m a
logBlk = scopeTrace . renderPretty

logInterpretation :: (Eq c, Num c, PP.Pretty c, PP.Pretty f, Ord f, Monad m) =>  ProcT f c m () --TODO use pretty printer?
logInterpretation = void $ logBlk "Interpretation" $ 
    fmap toList getInterpretation >>= mapM logBinding where
  logBinding (f,p) = 
    logMsg (PP.pretty f PP.<> PP.parens (PP.hcat (PP.punctuate (PP.text ",") [PP.pretty v | v <- Poly.variables p]))
            PP.<+> PP.text "=" PP.<+> PP.pretty p)          
 
logConstraints :: (Eq c, Num c, PP.Pretty c, PP.Pretty f, Ord f, Ord v, PP.Pretty v, Monad m) =>  ConstraintSystem f v -> ProcT f c m ()
logConstraints  cs = 
  void $ logBlk "Constraints" $ do 
    i <- getInterpretation
    mapM (logConstraint i) cs where
  logConstraint i (l :>=: r) = 
    logMsg (ppIEQ l r PP.<> PP.text":" PP.<+> ppC i l r)
  ppC i l r = fromMaybe (PP.text "interpretation open") $ do 
    il <- interpret i l
    ir <- interpret i r
    return (ppIEQ il ir)
  ppIEQ l r = PP.pretty l PP.<+> PP.text "≥" PP.<+> PP.pretty r

logOpenConstraints :: (Eq c, Num c, PP.Pretty c, PP.Pretty f, Ord f, Ord v, PP.Pretty v, Monad m) =>  ConstraintSystem f v -> ProcT f c m ()
logOpenConstraints  cs = 
  void $ logBlk "Open Constraints" $ do 
    i <- getInterpretation
    mapM logMsg (filter (nonInterpreted i) cs) where
  nonInterpreted i (l :>=: r) = isNothing (interpret i l) || isNothing (interpret i r)

data Result f v = Progress (ConstraintSystem f v)
                | NoProgress

type Processor f c v m = ConstraintSystem f v -> ProcT f c m (Result f v)

abort :: Monad m => Processor f c v m
abort _ = return NoProgress

try :: Monad m => Processor f c v m -> Processor f c v m
try p cs = do
  inter <- get
  r <- p cs          
  case r of 
    Progress cs' -> return (Progress cs')
    NoProgress -> put inter >> return (Progress cs)                       

(<=>) :: Monad m => Processor f c v m -> Processor f c v m -> Processor f c v m
p1 <=> p2 = \cs -> do 
  inter <- get
  r <- p1 cs
  case r of 
    Progress cs' -> return (Progress cs')
    NoProgress -> put inter >> p2 cs

(==>) :: Monad m => Processor f c v m -> Processor f c v m -> Processor f c v m
p1 ==> p2 = \cs -> do 
  r <- p1 cs
  case r of 
    Progress cs' -> p2 cs'
    NoProgress -> return NoProgress
  
exhaustive :: Monad m => Processor f c v m -> Processor f c v m
exhaustive p = p ==> try (exhaustive p)


