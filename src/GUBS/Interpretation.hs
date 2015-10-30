module GUBS.Interpretation where

import Data.Maybe (fromMaybe)
import GUBS.Polynomial
import qualified Data.Map.Strict as M

type Var = Int

variables :: [Var]
variables = [0..]

newtype Interpretation f c = Inter (M.Map f (Polynomial Var c))
  deriving (Eq, Ord, Show)

deriving instance Functor (Interpretation f)
deriving instance Foldable (Interpretation f)
deriving instance Traversable (Interpretation f)

get :: Ord f => Interpretation f c -> f -> Maybe (Polynomial Var c)
get (Inter m) f = M.lookup f m

get' :: Ord f => Interpretation f c -> f -> Polynomial Var c
get' inter = fromMaybe (error "GUBS.interpretation: function symbol not found") . get inter

insert :: Ord f => Interpretation f c -> f -> Polynomial Var c -> Interpretation f c 
insert (Inter m) f p = Inter (M.insert f p m)

union :: Ord f => Interpretation f c -> Interpretation f c -> Interpretation f c
union (Inter m1) (Inter m2) = Inter (m1 `M.union` m2)

empty :: Interpretation f c
empty = Inter M.empty

apply :: (Num c, Ord v) => Polynomial Var c -> [Polynomial v c] -> Polynomial v c
apply p args = substitute p (zip variables args) 

fromList :: Ord f => [(f, Polynomial Var c)] -> Interpretation f c
fromList = Inter . M.fromList

toList :: Interpretation f c -> [(f, Polynomial Var c)]
toList (Inter m) = M.toList m
