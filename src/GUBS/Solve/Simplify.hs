module GUBS.Solve.Simplify where

import           Control.Monad
import           Data.List (groupBy)
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

propagateUp :: (Eq c, Num c, Eq f, Ord f, Ord v, Monad m, PP.Pretty f, PP.Pretty c, PP.Pretty v, Show c, Show v) => Processor f c v m
propagateUp cs = do 
  i <- getInterpretation
  toProgress cs <$> concat <$> mapM (propagate i) (groupBy (\eq1 eq2 -> ds eq1 == ds eq2) cs) where
    ds = definedSymbol . lhs
    toProgress cs cs' = if length cs > length cs' then Progress cs' else NoProgress
    propagate i [Fun f ts :>=: b] 
      | Just s <- renaming ts variables
      , Just p <- interpret i b 
      , Nothing <- get i f = do 
          let p' = P.substitute p s
          logMsg (PP.text "Propagated:" 
                  PP.<+> PP.pretty f PP.<> PP.parens (PP.cat (PP.punctuate PP.comma [ PP.pretty v | (_,v) <- s]))
                  PP.<+> PP.text "↦" PP.<+> PP.pretty p')
          modifyInterpretation (\i' -> insert i' f p')
          return []                     
    propagate _ g = return g


propagateDown :: (Num c, Eq f, Ord f, Ord v, Monad m) => Processor f c v m
propagateDown cs = do 
  i <- getInterpretation
  toProgress cs <$> concat <$> mapM (propagate i) (groupBy (\eq1 eq2 -> ds eq1 == ds eq2) cs) where
   toProgress cs cs' = if length cs > length cs' then Progress cs' else NoProgress
   ds = definedSymbol . rhs
   propagate i [ h :>=: Fun f ts] 
     | Just s <- renaming ts variables
     , Just p <- interpret i h
     , Nothing <- get i f = do 
         modifyInterpretation (\i' -> insert i' f (P.substitute p s))
         return []                     
   propagate _ g = return g


