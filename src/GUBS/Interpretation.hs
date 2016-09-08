module GUBS.Interpretation where

import Data.Maybe (fromMaybe)
import GUBS.Polynomial hiding (variables)
import qualified GUBS.Polynomial as P
import GUBS.CS (Term (..))
import qualified Data.Map.Strict as M
import qualified Text.PrettyPrint.ANSI.Leijen as PP

newtype Var = V Int deriving (Eq, Ord, Show)

variables :: [Var]
variables = [V i | i <- [0..]]

newtype Interpretation f c = Inter (M.Map f (Polynomial Var c))
  deriving (Eq, Ord, Show)

deriving instance Functor (Interpretation f)
deriving instance Foldable (Interpretation f)
deriving instance Traversable (Interpretation f)

get :: Ord f => Interpretation f c -> f -> Maybe (Polynomial Var c)
get (Inter m) f = M.lookup f m

get' :: Ord f => Interpretation f c -> f -> Polynomial Var c
get' inter = fromMaybe err . get inter where
   err = error "GUBS.interpretation: function symbol not found"  

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

mapInter :: (Polynomial Var c -> Polynomial Var c') -> Interpretation f c -> Interpretation f c'
mapInter f (Inter m) = Inter (M.map f m)

interpret :: (Ord f, Num c, Ord v) => Interpretation f c -> Term f v -> Maybe (Polynomial v c) 
interpret _ (Var v) = return (variable v)
interpret _ (Const i) = return (fromIntegral i)
interpret i (Plus t1 t2) = (+) <$> interpret i t1 <*> interpret i t2
interpret i (Mult t1 t2) = (*) <$> interpret i t1 <*> interpret i t2
interpret i (Minus t1 t2) = (-) <$> interpret i t1 <*> interpret i t2
interpret i (Neg t) = negate <$> interpret i t
interpret i t@(Fun f ts) = apply <$> get i f <*> mapM (interpret i) ts

pInterpret :: (Ord f, Num c, Ord v, Integral c) => Interpretation f c -> Term f v -> Term f v
pInterpret _ (Var v) = Var v
pInterpret _ (Const i) = Const i
pInterpret i (Plus t1 t2) = pInterpret i t1 + pInterpret i t2
pInterpret i (Mult t1 t2) = pInterpret i t1 * pInterpret i t2
pInterpret i (Minus t1 t2) = pInterpret i t1 - pInterpret i t2
pInterpret i (Neg t) = negate (pInterpret i t)
pInterpret i t@(Fun f ts) =
  case apply <$> get i f <*> mapM (interpret i) ts of
    Nothing -> Fun f (pInterpret i `map` ts)
    Just p -> polyToTerm p


-- pretty printers
instance PP.Pretty Var where
  pretty (V i) = PP.text "x" PP.<> PP.int i

instance (PP.Pretty f, PP.Pretty c, Eq c, Num c) => PP.Pretty (Interpretation f c) where
  pretty inter = PP.vcat [ pp b | b <- toList inter ] where
    pp (f,p) = PP.pretty f PP.<> PP.parens (PP.hcat (PP.punctuate (PP.text ",") [PP.pretty v | v <- P.variables p]))
                PP.<+> PP.text "=" PP.<+> PP.pretty p

