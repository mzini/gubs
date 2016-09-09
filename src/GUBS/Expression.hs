module GUBS.Expression
       (
         Expression
       , variable
       , constant
       , evalWithM
       , evalWith
       , toNum
       ) where

import           GUBS.Utils
import           GUBS.Polynomial
-- import qualified Data.MultiSet as MS
import qualified Text.PrettyPrint.ANSI.Leijen as PP

-- newtype Mono v = Mono (MultiSet v, Integer)
--   deriving (Eq, Ord, Show)

-- newtype Poly v = Poly [Mono v] deriving (Show)

type Expression v = Polynomial v Integer
-- pattern ZERO = Mono (Const 0)
-- pattern ONE  = Mono (Const 1)
-- pattern CONST c = Mono (Const c)

-- mcoeff :: Mono v -> Integer
-- mcoeff (Mono (_,i)) = i

-- mvars :: Mono v -> MultiSet v
-- mvars (Mono (vs,_)) = vs

-- variable :: v -> Expression v
-- variable v = Poly [(MS.singleton v, 1)]

-- constant :: Integer -> Expression v
-- constant i = Poly [(MS.empty , i)]

-- mapMono :: (Integer -> Integer) -> (v -> v') -> Mono v -> Mono v'
-- mapMono f g (Mono (ms,i)) = Mono (MS.map g ms, f i)

-- mapPoly :: (v -> v') -> (Expression v -> Expression v')
-- mapPoly f = Mono (f m)
-- mapPoly f (Add m e) = Add (f m) (mapExp f e)

-- monos :: Expression v -> [Monomial v]
-- monos (Mono m) = [m]
-- monos (Add m e) = m : monos e

-- negMono :: Monomial v -> Monomial v
-- negMono (Const c) = Const (-c)
-- negMono (Mult v m) = Mult v (negMono m)

-- neg :: Expression v -> Expression v
-- neg (Poly ms) = Poly [ mapMono negate id  | m <- ms]

-- plus :: Expression v -> Expression v -> Expression v
-- plus (Poly ms1) (Poly ms2) = Poly (map ins1 ms2) where
--   ins1 m1 [] = m1
--   ins1 m1 (m2:ms')
--     | mvars m1 == mvars m2 = Mono (m1,mcoeff m1 + mcoeff m2) : ms'
--     | otherwise = m2 : ins1 m1 ms'

-- minus :: Expression v -> Expression v -> Expression v
-- minus e1 e2 = plus e1 (neg e2)

-- mult :: Expression v -> Expression v -> Expression v
-- mult (Poly ms1) (Poly ms2) 
-- mult e1 (Mono m) = mapExp (multM m) e1 where
--   multM (Const c) m2 = mapMono (* c) id m2
--   multM (Mult v m1) m2 = Mult v (multM m1 m2)


-- instance Num (Expression v) where
--   fromInteger c = CONST c
--   (+) = plus
--   (-) = minus
--   (*) = mult
--   negate = neg
--   abs = error "Expression: abs undefined"
--   signum = error "Expression: signum undefined"


-- pretty printers

-- instance Show v => PP.Pretty (Monomial v) where
--   pretty (Const i) = PP.text (show i)
--   pretty (Mult v m) = ppCall "*" [ppCall "var" [PP.text (show v)], PP.pretty m]
  
-- instance Show v => PP.Pretty (Expression v) where
--   pretty (Mono m) = PP.pretty m
--   pretty (Add m e) = ppCall "+" [PP.pretty m, PP.pretty e]
