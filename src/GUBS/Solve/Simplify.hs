module GUBS.Solve.Simplify where

import           Control.Monad
import           Data.Function (on)
import           Data.List (groupBy,sortBy)
import           GUBS.CS
import           GUBS.Interpretation
import qualified GUBS.Polynomial as P
import           GUBS.Solve.Strategy
import qualified Text.PrettyPrint.ANSI.Leijen as PP

renaming :: (Eq v, Num c) => [Term f v] -> [Var] -> Maybe (P.Substitution c v Var)
renaming [] _ = Just []
renaming (Var v:ts) (pv:pvs) = do 
  s <- renaming ts pvs
  guard (v `notElem` map fst s)
  return ((v,P.variable pv):s)
renaming _ _ = Nothing

funsOfArgs :: [Term f v] -> [f]
funsOfArgs = foldr funsDL [] . foldr argsDL []

logBinding f s p = 
    logMsg (PP.text "Propagated:" 
            PP.<+> PP.pretty f PP.<> PP.parens (PP.cat (PP.punctuate PP.comma [ PP.pretty v | (_,v) <- s]))
            PP.<+> PP.text "↦" PP.<+> PP.pretty p)

groupWith f = groupBy (\eq1 eq2 -> f eq1 == f eq2) . sortBy (compare `on` f)

-- TODO: equalities; not working ATM
propagateUp :: (Eq c, Num c, PP.Pretty c, Eq f, Ord f, PP.Pretty f, PP.Pretty v, Ord v, Monad m) => Processor f c v m
propagateUp cs = do 
  i <- getInterpretation
  toProgress cs <$> concat <$> mapM (propagate i) (groupWith dsym cs) where
    dsym = definedSymbol . lhs
    toProgress cs cs' = if length cs > length cs' then Progress cs' else NoProgress
    propagate i [t@ (Fun f ts) :>=: b] 
      | Just s <- renaming ts variables
      , Just p <- interpret i b 
      , Nothing <- get i f
      , all (`elem` map fst s) (P.variables p)
      , f `notElem`  funsOfArgs (lhss cs) = do
           let p' = P.substitute p s
           logBinding f s p'                      
           modifyInterpretation (\i' -> insert i' f p')
           return []                     
    propagate _ g = return g

-- TODO: equalities
propagateDown :: (Eq c, Num c, PP.Pretty c, Eq f, Ord f, PP.Pretty f, Ord v, Monad m, Show v, PP.Pretty v) => Processor f c v m
propagateDown cs = do 
  i <- getInterpretation
  toProgress cs <$> concat <$> mapM (propagate i) (groupWith dsym cs) where
   toProgress cs cs' = if length cs > length cs' then Progress cs' else NoProgress
   dsym = definedSymbol . rhs
   propagate i [ h :>=: Fun f ts] 
     | Just s <- renaming ts variables
     , Just p <- interpret i h
     , Nothing <- get i f       
     , all (`elem` map fst s) (P.variables p)
     , f `notElem`  funsOfArgs (rhss cs) = do 
           let p' = P.substitute p s
           logBinding f s p'                      
           modifyInterpretation (\i' -> insert i' f p')
           return []                            
   propagate _ g = return g


