module GUBS.Solve.Strategy (
  Interpretation
  , Abort (..)
  , ProcT
  , Processor
  , run
  , (>=>)
  , (<|>)
  , choice
  , abort
  , try
  , getInterpretation
  , modifyInterpretation
  ) where

import Control.Applicative
import Control.Monad (guard, MonadPlus(..), ap)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans (lift)

import GUBS.Interpretation hiding (get)
import GUBS.CS

data Abort = Abort deriving (Eq, Show)

instance Monoid Abort where
  mempty = Abort
  a `mappend` _ = a
  
newtype ProcT f c m a = ProcT { runProcT_ :: StateT (Interpretation f c) (ExceptT Abort m) a }
  deriving (Applicative, Functor, Monad
            , MonadError Abort
            , MonadState (Interpretation f c)
            , Alternative
            , MonadPlus
            , MonadIO)

instance MonadTrans (ProcT f c) where lift = ProcT . lift . lift

           
run :: Interpretation f c -> ProcT f c m a -> m (Either Abort (a, Interpretation f c))
run i = runExceptT . flip runStateT i . runProcT_

getInterpretation :: Monad m => ProcT f c m (Interpretation f c)
getInterpretation = get

modifyInterpretation :: Monad m => (Interpretation f c -> Interpretation f c) -> ProcT f c m ()
modifyInterpretation = modify

abort :: Monad m => ProcT f c m a
abort = throwError Abort

choice :: Monad m => [ProcT f c m a] -> ProcT f c m a
choice = msum

type Processor f c v m = ConstraintSystem f v -> ProcT f c m (ConstraintSystem f v)

try :: Monad m => Processor f c v m -> Processor f c v m
try p cs = do
  inter <- get
  p cs `catchError` (const (put inter >> return cs))

