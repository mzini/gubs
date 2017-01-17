module GUBS.Interpretation where

import Data.Maybe (fromMaybe)
import Data.List (foldl')
import GUBS.Polynomial hiding (variables, substitute)
import qualified GUBS.Polynomial as P
import GUBS.CS (Term (..), Constraint (..), ConstraintSystem, substitute)
import qualified Data.Map.Strict as M
import qualified Text.PrettyPrint.ANSI.Leijen as PP

newtype Var = V Int deriving (Eq, Ord, Show)

variables :: [Var]
variables = [V i | i <- [0..]]

newtype Interpretation f c = Inter (M.Map (f,Int) (Polynomial Var c))
  deriving (Eq, Ord, Show)

deriving instance Functor (Interpretation f)
deriving instance Foldable (Interpretation f)
deriving instance Traversable (Interpretation f)

domain :: Interpretation f c -> [(f,Int)]
domain (Inter m) = M.keys m

get :: Ord f => Interpretation f c -> f -> Int -> Maybe (Polynomial Var c)
get (Inter m) f i = M.lookup (f,i) m

get' :: Ord f => Interpretation f c -> f -> Int -> Polynomial Var c
get' inter f i = fromMaybe err (get inter f i) where
   err = error "GUBS.interpretation: function symbol not found"  

insert :: Ord f => Interpretation f c -> f -> Int -> Polynomial Var c -> Interpretation f c 
insert (Inter m) f i p = Inter (M.insert (f,i) p m)

empty :: Interpretation f c
empty = Inter M.empty

union :: Ord f => Interpretation f c -> Interpretation f c -> Interpretation f c
union (Inter m1) (Inter m2) = Inter (m1 `M.union` m2)

unions :: Ord f => [Interpretation f c] -> Interpretation f c
unions = foldl' union empty 

apply :: (Eq c, Num c, Ord v) => Polynomial Var c -> [Polynomial v c] -> Polynomial v c
apply p args = P.substitute p (zip variables args) 

fromList :: Ord f => [((f,Int), Polynomial Var c)] -> Interpretation f c
fromList = Inter . M.fromList

toList :: Interpretation f c -> [((f,Int), Polynomial Var c)]
toList (Inter m) = M.toList m

mapInter :: (Polynomial Var c -> Polynomial Var c') -> Interpretation f c -> Interpretation f c'
mapInter f (Inter m) = Inter (M.map f m)

interpret :: (Ord f, Eq c, Num c, Ord v) => Interpretation f c -> Term f v -> Maybe (Polynomial v c) 
interpret _ (Var v) = return (variable v)
interpret _ (Const i) = return (fromIntegral i)
interpret i (Plus t1 t2) = (+) <$> interpret i t1 <*> interpret i t2
interpret i (Mult t1 t2) = (*) <$> interpret i t1 <*> interpret i t2
interpret i (Minus t1 t2) = (-) <$> interpret i t1 <*> interpret i t2
interpret i (Neg t) = negate <$> interpret i t
interpret i t@(Fun f ts) = apply <$> get i f (length ts) <*> mapM (interpret i) ts

pInterpret :: (Ord f, Num c, Ord v, Integral c) => Interpretation f c -> Term f v -> Term f v
pInterpret _ (Var v) = Var v
pInterpret _ (Const i) = Const i
pInterpret i (Plus t1 t2) = pInterpret i t1 + pInterpret i t2
pInterpret i (Mult t1 t2) = pInterpret i t1 * pInterpret i t2
pInterpret i (Minus t1 t2) = pInterpret i t1 - pInterpret i t2
pInterpret i (Neg t) = negate (pInterpret i t)
pInterpret i t@(Fun f ts) =
  case get i f (length ts) of
    Nothing -> Fun f ts'
    Just p -> substitute s (polyToTerm p) 
  where
    ts' = pInterpret i `map` ts
    s (V i) = ts'!!i

pInterpretC :: (Ord f, Num c, Ord v, Integral c) => Interpretation f c -> Constraint f v -> Constraint f v
pInterpretC i (l :=: r) = pInterpret i l :=: pInterpret i r
pInterpretC i (l :>=: r) = pInterpret i l :>=: pInterpret i r

pInterpretCS :: (Ord f, Num c, Ord v, Integral c) => Interpretation f c -> ConstraintSystem f v -> ConstraintSystem f v
pInterpretCS i = map (pInterpretC i) 


-- pretty printers
instance PP.Pretty Var where
  pretty (V i) = PP.text "x" PP.<> PP.int i

instance (PP.Pretty f, PP.Pretty c, Eq c, Num c) => PP.Pretty (Interpretation f c) where
  pretty inter = PP.vcat [ pp b | b <- toList inter ] where
    pp ((f,i),p) = PP.pretty f PP.<> PP.parens (PP.hcat (PP.punctuate (PP.text ",") [PP.pretty v | v <- take i variables]))
                PP.<+> PP.text "=" PP.<+> PP.pretty p

