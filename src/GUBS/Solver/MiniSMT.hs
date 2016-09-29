module GUBS.Solver.MiniSMT (
  miniSMT
  ) where

import           Prelude hiding (lookup)
import           Text.Read hiding (Symbol)

import qualified Control.Monad.State as St
import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Monad.Trace
import qualified Data.ByteString.Builder as BS
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust)
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           System.IO (Handle, hClose, hFlush, hSetBinaryMode, stderr, hPutStrLn)
import           System.IO.Temp (withSystemTempFile)
import           System.Exit
import           System.Process
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           GUBS.Utils
import           GUBS.Expression
import qualified GUBS.Polynomial as Poly
import           GUBS.Solver.Class


data MiniSMT = MiniSMT

newtype Symbol = Symbol Int deriving (Eq, Ord, Show)

type Assign = Map Symbol Integer

lookup :: Symbol -> Assign -> Integer
lookup s a = fromMaybe 0 (Map.lookup s a)

          
data Frame = Frame { fFreeVars :: Set Symbol
                   , fConstraints :: [Disj (Constrt MiniSMT)]}

data SolverState = SolverState { freshId    :: Int
                               , assign     :: Maybe Assign
                               , curFrame   :: Frame
                               , frameStack :: [Frame] }

freeVars :: SolverState -> [Symbol]
freeVars = Set.toList . fFreeVars . curFrame

constraints :: SolverState -> [Disj (Constrt MiniSMT)]
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

addConstraint :: Disj (Constrt MiniSMT) -> SolverState -> SolverState
addConstraint c st@SolverState{..} =
  st { curFrame = curFrame { fConstraints = c : fConstraints curFrame }
     , assign = Nothing } -- TODO: maybe check satisfiability of c and keep



-- smt script formatter
----------------------------------------------------------------------

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
  
toScript :: [Symbol] -> [Disj (Constrt MiniSMT)] -> BS.Builder
toScript vs cs =
  app "set-logic" [stringBS "QF_NIA"]
  </> vsep [ app "declare-fun" [stringBS (show (Lit v)), sexpr [], stringBS "Nat"]
           | v <- vs]
  </> vsep [ app "assert" [disjToBS d] | d <- cs]
  </> app "check-sat" []
  where
    disjToBS [] = stringBS "false"
    disjToBS [e] = constrToBS e
    disjToBS es = app "or" [ constrToBS e | e <- es ]
    constrToBS c =
      app (eqSym c) [expressionToBS (clhs c), expressionToBS (crhs c) ] where
        eqSym GEQC {} = ">="
        eqSym EQC {} = "="

    expressionToBS (Exp e) = polyToBS e where
      add [] = integerBS 0
      add [a] = a
      add as = app "+" as
      mul 0 _ = integerBS 0
      mul c [] = integerBS c
      mul 1 [a] = a
      mul (-1) [a] = app "-" [a]
      mul 1 as = app "*" as
      mul c as = app "*" (integerBS c : as)
      
      polyToBS (Poly.toMonos -> ms) = add [ monoToBS c m | (c,m) <- ms ]
      monoToBS c (Poly.toPowers -> ps) = mul c (concat [ replicate e (stringBS (show v)) | (v,e) <- ps ])
    

-- result parser
----------------------------------------------------------------------

parseOut :: String -> Maybe Assign
parseOut out =
  case lines out of
    "sat" : _ : ls -> Just $ 
      Map.fromList [ (case read var of Lit s -> s, read val)
                   | l <- ls
                   , let (var,_:val) = break (== '=') (filter (/= ' ') l)]    
    _ -> Nothing

-- minismt wrapper
----------------------------------------------------------------------
runMiniSMT :: (MonadTrace String m, MonadIO m) => [Symbol] -> [Disj (Constrt MiniSMT)] -> m (Maybe Assign)
runMiniSMT vs cs = do
  let script = toScript vs cs
  logBlk "MiniSMT" (logMsg (ppScript script))
  liftIO $ withSystemTempFile "smt" $ \file hfile -> do
    hSetBinaryMode hfile True
    BS.hPutBuilder hfile script
    hFlush hfile
    hClose hfile
    (code, out, err) <- readProcessWithExitCode "minismt" ["-m","-v2",file] ""
    case code of
      ExitFailure _ -> hPutStrLn stderr err >> return Nothing
      ExitSuccess   -> return (parseOut out)
      where
        ppScript = PP.vcat . map PP.text . map ("   " ++) . lines . unpack . BS.toLazyByteString
-- SMTSolver instance
----------------------------------------------------------------------

instance SMTSolver MiniSMT where
  data SolverM MiniSMT m a = S (St.StateT SolverState m a) deriving (Functor)
  data Literal MiniSMT = Lit Symbol
  data Exp MiniSMT = Exp (Expression (Literal MiniSMT))

  lit = Exp . variable
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

instance PP.Pretty (Literal MiniSMT) where
  pretty (Lit (Symbol i)) = PP.text "v" PP.<> PP.int i

instance Read (Literal MiniSMT) where
  readPrec = get >>= readLit where
    readLit 'v' = Lit <$> Symbol <$> readPrec
    readLit  _  = pfail

deriving instance Eq (Literal MiniSMT)
deriving instance Ord (Literal MiniSMT)

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

miniSMT :: (MonadTrace String m, MonadIO m) => SolverM MiniSMT m a -> m a
miniSMT (S m) = St.evalStateT m initialState where
  initialState = SolverState { freshId = 0
                             , assign = Nothing
                             , curFrame = initialFrame
                             , frameStack = [] }
  initialFrame = Frame { fFreeVars = Set.empty, fConstraints = [] }
