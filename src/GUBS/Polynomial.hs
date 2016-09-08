module GUBS.Polynomial where

import           Data.List (foldl',sort, nub)
import qualified Data.Map.Strict as M
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import GUBS.CS

newtype Monomial v = Mono (M.Map v Int)
  deriving (Eq, Ord, Show)

newtype Polynomial v c = Poly (M.Map (Monomial v) c)
  deriving (Eq, Ord, Show)

deriving instance Functor (Polynomial v)
deriving instance Foldable (Polynomial v)
deriving instance Traversable (Polynomial v)

fromPowers :: Ord v => [(v,Int)] -> Monomial v
fromPowers = Mono . M.fromList

toPowers :: Monomial v -> [(v,Int)]
toPowers (Mono m) = [ p | p <- M.toList m , snd p /= 0]

fromMono :: Num c => Monomial v -> Polynomial v c 
fromMono m = Poly (M.singleton m 1)

fromMonos :: (Num c, Ord v) => [(c,Monomial v)] -> Polynomial v c
fromMonos ms = sum [ scale c (fromMono m) | (c,m) <- ms]

toMonos :: (Num c) => Polynomial v c -> [(c,Monomial v)]
toMonos (Poly m) = [ (c,m) | (m,c) <- M.toList m ]
-- toMonos (Poly m) = [ (c,m) | (m,c) <- M.toList m , c /= fromIntegral 0]

coefficients :: Polynomial v c -> [c]
coefficients (Poly ms) = M.elems ms
                                 
variables :: (Num c, Eq v, Ord v) => Polynomial v c -> [v]
variables = sort . nub . concatMap monoVars . toMonos where
  monoVars = map fst . toPowers . snd                                                                                                       

rename :: Ord v' => (v -> v') -> Polynomial v c -> Polynomial v' c
rename f (Poly ms) = Poly (M.mapKeys (\ (Mono m) -> Mono (M.mapKeys f m)) ms)

constant :: Num c => Integer -> Polynomial v c
constant 0 = Poly M.empty
constant c = Poly (M.singleton (Mono M.empty) (fromIntegral c))

variable :: (Num c, Ord v) => v -> Polynomial v c
variable v = Poly (M.singleton (Mono (M.singleton v 1)) 1)

plus :: (Num c, Ord v) => Polynomial v c -> Polynomial v c -> Polynomial v c
plus (Poly ms1) (Poly ms2) = Poly (M.unionWith (+) ms1 ms2)

neg :: (Num c) => Polynomial v c -> Polynomial v c
neg (Poly ms) = Poly (M.map negate ms)

minus :: (Num c, Ord v) => Polynomial v c -> Polynomial v c -> Polynomial v c
minus p1 p2 = p1 `plus` neg p2

scale :: (Num c) => c -> Polynomial v c -> Polynomial v c
scale c (Poly ms) = Poly (M.map (* c) ms)
  -- -| c == fromIntegral 0 = (constant 0)
  -- -| otherwise = Poly (M.map (* c) ms)

mult :: (Num c, Ord v) => Polynomial v c -> Polynomial v c -> Polynomial v c
mult (Poly ms1) (Poly ms2) = Poly (M.fromListWith (+) ms) where
  ms = [ (m1 `mmult` m2, c1 * c2) | (m1,c1) <- M.toList ms1, (m2,c2) <- M.toList ms2 ]
  Mono ps1 `mmult` Mono ps2 = Mono (M.unionWith (+) ps1 ps2)

instance (Num c, Ord v) => Num (Polynomial v c) where
  fromInteger = constant
  (+) = plus
  (-) = minus
  (*) = mult
  negate = neg
  abs = error "Polynomial: abs undefined"
  signum = error "Polynomial: signum undefined"

-- substitution
   
type Substitution c v v' = [(v,Polynomial v' c)]

sdomain :: Substitution c v v' -> [v]
sdomain = map fst

substitute1 :: (Num c, Ord v) => Polynomial v c -> (v,Polynomial v c) -> Polynomial v c
substitute1 (Poly ms) (v,p) = sum [ scale c (substituteMono m) | (m,c) <- M.toList ms] where
  substituteMono (Mono m) =
    case M.lookup v m of
      Nothing -> fromMono (Mono m)
      Just i -> product (fromMono (Mono (M.delete v m)) : replicate i p)

substitute :: (Num c, Ord v, Ord v') => Polynomial v c -> Substitution c v v' -> Polynomial v' c
substitute p s = rename fromRight (foldl' substitute1 p' s') where
  p' = rename Left p
  s' = [(Left v,rename Right q) | (v,q) <- s]
  fromRight (Right b) = b
  fromRight (Left v) = error $ "Polynomial.substitute: not all variables substituted" 

polyToTerm :: Integral c => Polynomial v c -> Term f v
polyToTerm p = sum [Const (toInteger c) * monoToTerm m | (c,m) <- toMonos p] where
  monoToTerm m = product [ product (replicate p (Var v)) | (v,p) <- toPowers m]
  
-- pretty printers
   
ppPower :: PP.Pretty a => (a, Int) -> PP.Doc
ppPower (v,i) = PP.pretty v PP.<> if i == 1 then PP.empty else PP.char '^' PP.<> PP.int i

instance PP.Pretty v => PP.Pretty (Monomial v) where
  pretty mono = pretty' (toPowers mono) where
    pretty' [] = PP.char '1'
    pretty' ps = PP.hcat (PP.punctuate (PP.char '*') [ppPower p | p <- ps])

instance (PP.Pretty c, Num c, Eq c, PP.Pretty v) => PP.Pretty (Polynomial v c) where
  pretty poly = pretty' [p | p <- toMonos poly, fst p /= 0] where
    pretty' [] = PP.char '0'
    pretty' ps = PP.hcat (PP.punctuate (PP.char '+') (ppMono `map` ps))
    ppMono (1,mono) = PP.pretty mono --TODO
    ppMono (c,toPowers -> []) = PP.pretty c
    ppMono (c,mono) = PP.pretty c PP.<> PP.char '*' PP.<> PP.pretty mono

   
