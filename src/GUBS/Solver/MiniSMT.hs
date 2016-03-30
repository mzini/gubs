module GUBS.Solver.MiniSMT (
  miniSMT
  ) where

import Prelude hiding (lookup)
import Text.Read hiding (Symbol)
import GUBS.Solver.Class
import GUBS.Expression
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Set (Set)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Control.Monad.State as St

import qualified Data.ByteString.Builder    as BS

import           Data.Monoid
import           System.IO                  (Handle, hClose, hFlush, hSetBinaryMode, stderr, hPutStrLn)
import           System.IO.Temp             (withSystemTempFile)
import           System.Exit
import           System.Process

data MiniSMT = MiniSMT

newtype Symbol = Symbol Int deriving (Eq, Ord, Show)

type Assign = Map Symbol Integer

lookup :: Symbol -> Assign -> Integer
lookup s a = fromMaybe 0 (Map.lookup s a)

stringBS :: String -> BS.Builder
stringBS = BS.string8

integerBS :: Integer -> BS.Builder
integerBS n | n >= 0 = BS.integerDec n
            | otherwise = app "-" [integerBS (-n)]

charBS :: Char -> BS.Builder
charBS = BS.char8

(<+>) :: BS.Builder -> BS.Builder -> BS.Builder
e1 <+> e2 = e1 <> charBS ' ' <> e2

(</>) :: BS.Builder -> BS.Builder -> BS.Builder
e1 </> e2 = e1 <> charBS '\n' <> e2

sepWith :: (BS.Builder -> BS.Builder -> BS.Builder) -> [BS.Builder] -> BS.Builder
sepWith sep [] = mempty
sepWith _ [e]  = e
sepWith sep (e:es) = e `sep` sepWith sep es

vsep,hsep :: [BS.Builder] -> BS.Builder
vsep = sepWith (</>)
hsep = sepWith (<+>)

sexpr :: [BS.Builder] -> BS.Builder
sexpr es = charBS '(' <> hsep es <> charBS ')'

app :: String -> [BS.Builder] -> BS.Builder
app f es = sexpr (stringBS f : es)
  
toScript :: [Symbol] -> [Constrt MiniSMT] -> BS.Builder
toScript vs cs =
  app "set-logic" [stringBS "QF_NIA"]
  </> vsep [ app "declare-fun" [stringBS (show (Lit v)), sexpr [], stringBS "Nat"]
           | v <- vs]
  </> vsep [ app "assert" [ app ">=" [expToBS l, expToBS r] ]
           | GEQ l r <- cs]
  </> app "check-sat" []
  where
    expToBS (Exp e) = expressionToBS e
    expressionToBS (Var l) = stringBS (show l)
    expressionToBS (Const i) = integerBS i
    expressionToBS (Mult e1 e2) = app "*" [expressionToBS e1, expressionToBS e2]
    expressionToBS (Plus e1 e2) = app "+" [expressionToBS e1, expressionToBS e2]
    expressionToBS (Minus e1 e2) = app "-" [expressionToBS e1, expressionToBS e2]
    expressionToBS (Neg e) = app "-" [expressionToBS e]        
    

parseOut :: String -> Maybe Assign
parseOut out =
  case lines out of
    "sat" : _ : ls -> Just $ 
      Map.fromList [ (case read var of Lit s -> s, read val)
                   | l <- ls
                   , let (var,_:val) = break (== '=') (filter (/= ' ') l)]    
    _ -> Nothing

runMiniSMT :: MonadIO m => [Symbol] -> [Constrt MiniSMT] -> m (Maybe Assign)
runMiniSMT vs cs = 
  liftIO $ withSystemTempFile "smt" $ \file hfile -> do
    hSetBinaryMode hfile True
    let script = toScript vs cs
    -- BS.hPutBuilder stderr script
    BS.hPutBuilder hfile script
    hFlush hfile
    hClose hfile
    (code, out, err) <- readProcessWithExitCode "minismt" ["-m","-v2",file] ""
    case code of
      ExitFailure _ -> hPutStrLn stderr err >> return Nothing
      ExitSuccess   -> return (parseOut out)

          
data Frame = Frame { fFreeVars :: Set Symbol
                   , fConstraints :: [Constrt MiniSMT]}

data SolverState = SolverState { freshId    :: Int
                               , assign     :: Maybe Assign
                               , curFrame   :: Frame
                               , frameStack :: [Frame] }

freeVars :: SolverState -> [Symbol]
freeVars = Set.toList . fFreeVars . curFrame

constraints :: SolverState -> [Constrt MiniSMT]
constraints st = concatMap fConstraints (curFrame st : frameStack st)

pushFrame :: SolverState -> SolverState
pushFrame st@SolverState{..} =
  st { curFrame = Frame (fFreeVars curFrame) []
     , frameStack = curFrame : frameStack }

popFrame :: SolverState -> SolverState
popFrame st =
  case frameStack st of
    [] -> error "MiniSMT: pop on empty stack"
    f:fs -> st { curFrame = f, frameStack = fs }

addConstraint :: Constrt MiniSMT -> SolverState -> SolverState
addConstraint c st@SolverState{..} =
  st { curFrame = curFrame { fConstraints = c : fConstraints curFrame }
     , assign = Nothing } -- TODO: maybe check satisfiability of c and keep


instance SMTSolver MiniSMT where
  data SolverM MiniSMT m a = S (St.StateT SolverState m a) deriving (Functor)
  data Literal MiniSMT = Lit Symbol
  data Exp MiniSMT = Exp (Expression (Literal MiniSMT))

  lit = Exp . literal
  constnt = Exp . constant
  
  fresh = do
    st@SolverState{..} <- St.get
    let sym = Symbol freshId
    St.put st { freshId = freshId + 1
              , curFrame = curFrame { fFreeVars = Set.insert sym (fFreeVars curFrame) } }
    return (Lit sym)

  toSolverExp = Exp
  
  push = St.modify pushFrame
  pop = St.modify popFrame
  assert c = St.modify (addConstraint c) 

  getValue (Lit s) = 
    maybe (error "model not available") (lookup s) <$> assign <$> St.get

  checkSat = do
    st <- St.get
    ma <- S (runMiniSMT (freeVars st) (constraints st))
    St.modify (\ st -> st { assign = ma })
    return (isJust ma)
    
  
instance Monad m => Applicative (SolverM MiniSMT m) where
  pure a = S (pure a)
  S a1 <*> S a2 = S (a1 <*> a2)

instance Monad m => Monad (SolverM MiniSMT m) where
  return a = S (return a)
  S m >>= f = S (m >>= \ a -> case f a of S m2 -> m2)

instance Monad m => St.MonadState SolverState (SolverM MiniSMT m) where
  put s = S (St.put s)
  get = S St.get


instance Show (Literal MiniSMT) where
  show (Lit (Symbol i)) = "v" ++ show i

instance Read (Literal MiniSMT) where
  readPrec = get >>= readLit where
    readLit 'v' = Lit <$> Symbol <$> readPrec
    readLit  _  = pfail

deriving instance Show (Expression (Literal MiniSMT))

liftUn f (Exp e) = Exp (f e)
liftBin f (Exp e1) (Exp e2) = Exp (f e1 e2)

instance Num (Exp MiniSMT) where
  fromInteger = Exp . fromInteger
  (+) = liftBin (+)
  (-) = liftBin (-)
  (*) = liftBin (*)
  negate = liftUn negate
  abs = liftUn abs
  signum = liftUn signum

miniSMT :: MonadIO m => SolverM MiniSMT m a -> m a
miniSMT (S m) = St.evalStateT m initialState where
  initialState = SolverState { freshId = 0
                             , assign = Nothing
                             , curFrame = initialFrame
                             , frameStack = [] }
  initialFrame = Frame { fFreeVars = Set.empty, fConstraints = [] }
