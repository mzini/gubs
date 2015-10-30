module GUBS.Solve.Simple where

import qualified GUBS.Interpretation as I
import GUBS.CS
import GUBS.Expression hiding (Constraint)
import qualified GUBS.Polynomial as P

import qualified Data.Map.Strict as M
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad (forM_, liftM)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- TODO
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Control.Monad (when)


type AbstractPolynomial s v = P.Polynomial v (Expression s)
type AbstractInterpretation s f = I.Interpretation f (Expression s)
type ConcreteInterpretation f = I.Interpretation f Integer

-- -- TODO
-- putDocLn :: PP.Pretty e => e -> IO ()
-- putDocLn = putStrLn . renderPretty

-- renderPretty :: PP.Pretty e => e -> String
-- renderPretty d = PP.displayS (PP.renderSmart 1.0 80 (PP.pretty d)) ""

-- writeDocFile :: PP.Pretty e => FilePath -> e -> IO ()
-- writeDocFile fn = writeFile fn . renderPretty
-- -- TODO end

freshPoly :: (Solver s m) => SolverM s m (Expression s) -> Int -> Int -> SolverM s m (AbstractPolynomial s I.Var)
freshPoly freshVar dim ar =
  P.fromMonos <$> sequence [ (,) <$> freshLiteral <*> return (P.fromPowers mono)
                           | mono <- concat (template dim) ]
  where 
    template 0 = [[[]]]
    template d = [ v `mult` mono | mono <- lead, v <- take ar I.variables ] : ps
      where
        ps@(lead:_) = template (d - 1)
        v `mult` [] = [(v,1)]
        v `mult` ((v',i) : mono)
          | v == v' = (v',i+1) : mono
          | otherwise = (v',i) : v `mult` mono

getInterpretation :: (Solver s m, Ord f) => (Int -> SolverM s m (AbstractPolynomial s I.Var)) -> ConcreteInterpretation f -> (f,Int)
                     -> StateT (AbstractInterpretation s f) (SolverM s m) (AbstractPolynomial s I.Var)
getInterpretation mkPoly inter (f,ar) =
  case I.get inter f of
    Just p ->
      return (fmap fromIntegral p)
    Nothing -> do
      ainter <- get
      case I.get ainter f of
        Just p -> return p
        Nothing -> do
          p <- lift (mkPoly ar)
          put (I.insert ainter f p)
          return p

interpret :: (Solver s m, Ord f, Ord v) => Int -> ConcreteInterpretation f -> Term f v -> StateT (AbstractInterpretation s f) (SolverM s m) (AbstractPolynomial s v)
interpret _ _ (Var v) = return (P.variable v)
interpret _ _ (Const i) = return (fromIntegral i)
interpret d inter (Fun f ts) = do
  p <- getInterpretation (freshPoly freshVar (max 1 d)) inter (f,length ts) 
  I.apply p <$> mapM (interpret d inter) ts
  where
    freshVar = do
      v <- freshLiteral
      assert (v `geq` 0)
      when (d <= 0) (assert (1 `geq` v))
      return v


fromAssignment :: (Solver s m) => AbstractInterpretation s f -> SolverM s m (ConcreteInterpretation f)
fromAssignment = traverse eval

solveSimple :: (Solver s m, Ord f, Ord v) => ConcreteInterpretation f -> [Constraint f v] -> SolverM s m (Maybe (ConcreteInterpretation f))
solveSimple inter cs = try 0 where
    try j | j >= 3 = return Nothing
    try j = do
      ainter <- flip execStateT I.empty $ do
        forM_ cs $ \ c -> do
          l <- interpret 1 inter (lhs c)
          r <- interpret 1 inter (rhs c)
          forM_ (P.coefficients (l - r)) $ \ d -> do
            lift (assert ( d `geq` 0) )
      sat <- checkSat
      if sat then Just <$> fromAssignment ainter else try (j + 1 )
    
  
