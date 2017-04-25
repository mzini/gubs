-- defines in/out pipe for solvers compliant with smtlib2 standard
-- MS: seems to work but
--  (i)  no proper error handling
--  (ii) there should be a more idiomatic way handling input/output using eg a stream library
module GUBS.Solver.SMTLib2.Pipe (
  runSMTLib2
  ) where


import           Text.Read                    hiding (Symbol, lift)
import           Control.Exception            (evaluate)
import           Control.Monad                (unless)
import qualified Control.Monad.State          as St
import           Control.Monad.Trans          (MonadIO, liftIO)
import qualified Data.ByteString.Builder      as BS
import           Data.Monoid
import           System.IO                    (BufferMode (..), Handle, hFlush, hGetContents, hGetLine,
                                               hPutStrLn, hSetBinaryMode, hSetBuffering, hWaitForInput, stderr)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified GUBS.Polynomial              as Poly
import           GUBS.Solver.Class

import           System.Process.Typed

newtype Symbol  = Symbol Int deriving (Eq, Ord)

data SolverState = SolverState
  { freshId   :: Int
  , inHandle  :: Handle
  , outHandle :: Handle
  , errHandle :: Handle }

runSMTLib2 :: String -> [String] -> SolverM SMTLib2 a -> IO a
runSMTLib2 cmd args m' = go (send qfniaBS *> m' <* send exitBS) where
  go (SMTLib2 m) =
    let cfg = setStdin createPipe $ setStdout createPipe $ setStderr createPipe $ proc cmd args in
    withProcess_ cfg  $ \p -> do
      let inh = getStdin  p
      hSetBinaryMode inh True
      hSetBuffering inh (BlockBuffering Nothing)
      a <- St.evalStateT
        m
        SolverState
          { freshId   = 0
          , inHandle  = inh
          , outHandle = getStdout p
          , errHandle = getStderr p }
      err <- hGetContents $ getStderr p
      _ <- evaluate err
      unless (null err) $ System.IO.hPutStrLn stderr err
      return a

send :: BS.Builder -> SolverM SMTLib2 ()
send msg = St.gets inHandle >>= \inh -> liftIO $ do
  BS.hPutBuilder inh (msg <> charBS '\n')
  hFlush inh

receive :: (String -> a) ->  SolverM SMTLib2 a
receive f = St.gets outHandle >>= \outh -> liftIO $ do
  _ <- hWaitForInput outh (-1)
  out <- hGetLine outh
  _ <- evaluate out
  return $ f out


-- smt script formatter
----------------------------------------------------------------------

stringBS :: String -> BS.Builder
stringBS = BS.string8

integerBS :: Integer -> BS.Builder
integerBS n | n >= 0 = BS.integerDec n
            | otherwise = app "-" [integerBS (-n)]
natBS :: Int -> BS.Builder
natBS = BS.intDec

charBS :: Char -> BS.Builder
charBS = BS.char8

-- eol :: BS.Builder -> BS.Builder
-- eol bs = bs <> charBS '\n'

(<+>) :: BS.Builder -> BS.Builder -> BS.Builder
e1 <+> e2 = e1 <> charBS ' ' <> e2

-- (</>) :: BS.Builder -> BS.Builder -> BS.Builder
-- e1 </> e2 = e1 <> charBS '\n' <> e2

sepWith :: (BS.Builder -> BS.Builder -> BS.Builder) -> [BS.Builder] -> BS.Builder
sepWith _ []       = mempty
sepWith _ [e]      = e
sepWith sep (e:es) = e `sep` sepWith sep es

-- vsep,hsep :: [BS.Builder] -> BS.Builder
-- vsep = sepWith (</>)
hsep :: [BS.Builder] -> BS.Builder
hsep = sepWith (<+>)

sexpr :: [BS.Builder] -> BS.Builder
sexpr es = charBS '(' <> hsep es <> charBS ')'

app :: String -> [BS.Builder] -> BS.Builder
app f es = sexpr (stringBS f : es)


-- smtlib2-command

qfniaBS :: BS.Builder
qfniaBS =app "set-logic" [stringBS "QF_NIA"]

exitBS :: BS.Builder
exitBS = app "exit" []

declareFunBS :: Show a => a -> String -> BS.Builder
declareFunBS v t = app "declare-fun" [stringBS (show v), sexpr [], stringBS t]

declareIntBS :: NLiteral SMTLib2 -> BS.Builder
declareIntBS v = declareFunBS v "Int"

declareBoolBS :: BLiteral SMTLib2 -> BS.Builder
declareBoolBS v = declareFunBS v "Bool"

assertBS :: Formula (BLiteral SMTLib2) (Poly.Polynomial (NLiteral SMTLib2) Integer) -> BS.Builder
assertBS f                      = app "assert" [formToBS f] where
  formToBS Top                  = stringBS "true"
  formToBS Bot                  = stringBS "false"
  formToBS (Lit (BoolLit l))    = stringBS (show l)
  formToBS (Lit (NegBoolLit l)) = app "not" [stringBS (show l)]
  formToBS (Atom (Eq e1 e2))    = app "=" [ expressionToBS e1, expressionToBS e2 ]
  formToBS (Atom (Geq e1 e2))   = app ">=" [ expressionToBS e1, expressionToBS e2 ]
  formToBS (And f1 f2)          = app "and" [ formToBS f1, formToBS f2 ]
  formToBS (Or f1 f2)           = app "or" [ formToBS f1, formToBS f2 ]
  formToBS Iff{}                = error "SmtLib2.assert: Iff not defined"
  formToBS LetB{}               = error "SmtLib2.assert: Iff not defined"

  expressionToBS e = polyToBS e where
    add []       = integerBS 0
    add [a]      = a
    add as       = app "+" as
    mul 0 _      = integerBS 0
    mul c []     = integerBS c
    mul 1 [a]    = a
    mul (-1) [a] = app "-" [a]
    mul 1 as     = app "*" as
    mul c as     = app "*" (integerBS c : as)

    polyToBS (Poly.toMonos -> ms)    = add [ monoToBS c m | (c,m) <- ms ]
    monoToBS c (Poly.toPowers -> ps) = mul c (concat [ replicate ex (stringBS (show v)) | (v,ex) <- ps ])

checkSatBS :: BS.Builder
checkSatBS = app "check-sat" []

getValueBS :: NLiteral SMTLib2 -> BS.Builder
getValueBS l = app "get-value" [sexpr [stringBS (show l)]]

freshSymbol :: SolverM SMTLib2 Symbol
freshSymbol = do
  st@SolverState{..} <- St.get
  let sym = freshId `seq` Symbol freshId
  St.put st { freshId = freshId + 1 }
  return sym


data SMTLib2

instance SMTSolver SMTLib2 where
  newtype SolverM  SMTLib2 a = SMTLib2 (St.StateT SolverState IO a)
    deriving (Functor, Applicative, Monad, St.MonadState SolverState, MonadIO)
  newtype NLiteral SMTLib2   = NLit Symbol deriving (Eq, Ord)
  newtype BLiteral SMTLib2   = BLit Symbol deriving (Eq, Ord)

  freshBool = do
    s <- freshSymbol
    let v = BLit s
    send $ declareBoolBS v
    return v

  freshNat = do
    s <- freshSymbol
    let v = NLit s
    send $ declareIntBS v
    send $ assertBS $ Atom $ Poly.variable v `Geq` Poly.coefficient 0
    return v

  push            = send $ app "push" []
  pop             = send $ app "pop" [natBS 1]
  assertFormula c = send $ assertBS c

  -- MS: assumes that the result is written in one line
  getValue l = send (getValueBS l) *> receive parseValue
    where parseValue = read . takeWhile (/= ')') . tail . dropWhile (/= ' ')

  checkSat = send checkSatBS *> receive (== "sat")

instance Show (NLiteral SMTLib2) where
  show (NLit (Symbol i)) = 'n': show i

instance Show (BLiteral SMTLib2) where
  show (BLit (Symbol i)) = 'b': show i

instance Read (NLiteral SMTLib2) where
  readPrec = get >>= readLit where
    readLit 'n' = NLit . Symbol <$> readPrec
    readLit  _  = pfail

instance Read (BLiteral SMTLib2) where
  readPrec = get >>= readLit where
    readLit 'b' = BLit . Symbol <$> readPrec
    readLit  _  = pfail

instance PP.Pretty (NLiteral SMTLib2) where
  pretty l = PP.text (show l)

instance PP.Pretty (BLiteral SMTLib2) where
  pretty l = PP.text (show l)

