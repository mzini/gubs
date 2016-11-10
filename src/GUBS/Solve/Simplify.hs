module GUBS.Solve.Simplify where

import           Control.Monad
import           Data.Function (on)
import           Data.Either (partitionEithers)
import           GUBS.CS
import           GUBS.Interpretation
import qualified GUBS.Polynomial as P
import           GUBS.Solve.Strategy
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Data.Function (on)
import qualified Data.Set as Set
import           Data.List (groupBy,sortBy,nub)

renaming :: (Eq v, Num c) => [Term f v] -> [Var] -> Maybe (P.Substitution c v Var)
renaming [] _ = Just []
renaming (Var v:ts) (pv:pvs) = do 
  s <- renaming ts pvs
  guard (v `notElem` P.sdomain s)
  return ((v,P.variable pv):s)
renaming _ _ = Nothing

funsOfArgs :: [Term f v] -> [(f,Int)]
funsOfArgs = foldr funsDL [] . foldr argsDL []

logBinding f s p = 
    logMsg (PP.text "Propagated:" 
            PP.<+> PP.pretty f PP.<> PP.parens (PP.cat (PP.punctuate PP.comma [ PP.pretty v | (_,v) <- s]))
            PP.<+> PP.text "↦" PP.<+> PP.pretty p)

groupWith :: (Eq b, Ord b) => (a -> b) -> [a] -> [[a]]
groupWith f = groupBy (\eq1 eq2 -> f eq1 == f eq2) . sortBy (compare `on` f)

propagateEq :: (Eq c, Num c, PP.Pretty c, Eq f, Ord f, PP.Pretty f, PP.Pretty v, Ord v, Monad m) => Processor f c v m
propagateEq cs = do
  i <- getInterpretation
  toProgress <$> partitionEithers <$> mapM (propagate i) cs where
  toProgress (_,[]) = NoProgress
  toProgress (ls,_) = Progress ls
  propagate i (Fun f ts :=: b) 
      | Just s <- renaming ts variables
      , Just p <- interpret i b 
      , Nothing <- get i f ar
      , all (`elem` P.sdomain s) (P.variables p) = do
           let p' = P.substitute p s
           logBinding f s p'                      
           modifyInterpretation (\i' -> insert i' f ar p')
           return (Right ())
             where ar = length ts                 
  propagate _ c = return (Left c)

partiallyInterpret :: (Ord f, Num c, Ord v, Integral c, Monad m) => Processor f c v m
partiallyInterpret cs = do
  i <- getInterpretation
  return (Progress (pInterpretCS i cs))

propagateUp :: (Eq c, Integral c, Num c, PP.Pretty c, Eq f, Ord f, PP.Pretty f, PP.Pretty v, Ord v, Monad m) => Processor f c v m
propagateUp = propagateUp' ==> partiallyInterpret where
  propagateUp' cs = do 
    i <- getInterpretation
    toProgress cs <$> concat <$> mapM (propagate i) (groupWith dsym cs)
      where
        dsym = definedSymbol . lhs
        toProgress cs cs' = if length cs > length cs' then Progress cs' else NoProgress
        propagate i [(Fun f ts) :>=: b] 
          | Just s <- renaming ts variables
          , Just p <- interpret i b 
          , Nothing <- get i f ar
          , all (`elem` P.sdomain s) (P.variables p)
          , (f,ar) `notElem`  funsOfArgs (lhss cs) = do
              let p' = P.substitute p s
              logBinding f s p'                      
              modifyInterpretation (\i' -> insert i' f ar p')
              return []
                where ar = length ts      
        propagate _ g = return g

propagateDown :: (Eq c, Integral c, Num c, PP.Pretty c, Eq f, Ord f, PP.Pretty f, Ord v, Monad m, Show v, PP.Pretty v) => Processor f c v m
propagateDown = propagateDown' ==> partiallyInterpret where
  propagateDown' cs = do 
    i <- getInterpretation
    toProgress cs <$> concat <$> mapM (propagate i) (groupWith dsym cs)
      where
        toProgress cs cs' = if length cs > length cs' then Progress cs' else NoProgress
        dsym = definedSymbol . rhs
        propagate i [ h :>=: Fun f ts ] 
          | Just s <- renaming ts variables
          , Just p <- interpret i h
          , Nothing <- get i f ar
          , all (`elem` map fst s) (P.variables p)
          , (f,ar) `notElem`  funsOfArgs (rhss cs) = do 
              let p' = P.substitute p s
              logBinding f s p'                      
              modifyInterpretation (\i' -> insert i' f ar p')
              return []
                where ar = length ts                 
        propagate _ g = return g

eliminate :: (Eq c, Integral c, Num c, PP.Pretty f, PP.Pretty v, Eq v, Ord v, Monad m, Ord f) => Processor f c v m
eliminate = partiallyInterpret <== \ cs ->  do 
  i <- getInterpretation
  let fs = rfuns cs Set.\\ (Set.fromList (domain i) `Set.union` lfuns cs)
  eliminate' (Set.toList fs) cs
  where 
    rfuns = Set.fromList . foldl (flip funsDL) [] . rhss
    lfuns = Set.fromList . foldl (flip funsDL) [] . lhss
    eliminate' [] _ = return NoProgress
    eliminate' fs cs = do
      forM fs $ \ (f,ar) -> do
        logMsg (PP.text "Eliminated:" PP.<+> PP.pretty f PP.<> PP.text "/" PP.<> PP.int ar)
        modifyInterpretation (\ i -> insert i f ar (fromIntegral 0))
      return (Progress cs)

instantiate :: (PP.Pretty f, PP.Pretty v, Eq v, Monad m) => Processor f c v m
instantiate cs = toProgress <$> partitionEithers <$> mapM inst cs where
  toProgress (_,[]) = NoProgress
  toProgress (ls,rs) = Progress (ls ++ rs)
  inst c@(s :=: t) = return (Left c)
  inst c@(s :>=: t) =
    case nub [ v | v <- vars s, v `notElem` vars t ] of
      [] -> return (Left c) 
      vs -> do
        let subst v | v `elem` vs = 0
                    | otherwise = Var v
        let c' = substitute subst s :>=: t
        logMsg (PP.text "Substituted:" PP.<+> PP.pretty c PP.<+> PP.text "↦" PP.<+> PP.pretty c')
        return (Right c')
