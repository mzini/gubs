module GUBS.MaxPolynomial where

import Data.List (nub)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Data.Foldable (toList)

import           GUBS.Utils
import           GUBS.Algebra
import qualified GUBS.Term as T
import           GUBS.Constraint
import qualified GUBS.Polynomial as P
import qualified GUBS.Solver.Class as S

data MaxPoly v c =
  Var v
  | Const c
  | Plus (MaxPoly v c) (MaxPoly v c)
  | Mult (MaxPoly v c) (MaxPoly v c)
  | Max (MaxPoly v c) (MaxPoly v c)
  deriving (Show, Eq, Functor, Foldable, Traversable)

constant :: c -> MaxPoly v c
constant = Const

variable :: v -> MaxPoly v c
variable = Var

variables :: MaxPoly v c -> [v]
variables (Var v)    = [v]
variables (Const _)  = []
variables (Plus p q) = variables p ++ variables q
variables (Mult p q) = variables p ++ variables q
variables (Max p q)  = variables p ++ variables q

coefficients :: MaxPoly v c -> [c]
coefficients = toList


instance IsNat c => IsNat (MaxPoly v c) where
  fromNatural_ = Const . fromNatural
  
instance (Eq c, Additive c) => Additive (MaxPoly v c) where
  zero = Const zero
  Const i .+ t2      | zero == i = t2
  t1      .+ Const j | zero == j = t1
  Const i .+ Const j             = Const (i .+ j)
  t1 .+ t2                       = Plus t1 t2

instance (Eq c, Additive c) => Max (MaxPoly v c) where
  Const i `maxA` t2      | zero == i = t2
  t1      `maxA` Const j | zero == j = t1
  t1      `maxA` t2                  = Max t1 t2
  
instance (Eq c, Additive c, Multiplicative c) => Multiplicative (MaxPoly v c) where
  one = Const one
  Const i .* t2      | zero == i = zero
                     | one == i  = t2
  t1      .* Const j | zero == j = zero
                     | one == j  = t1
  Const i .* Const j             = Const (i .* j)
  t1 .* t2                       = Mult t1 t2

-- operations

-- rename :: (v -> v') -> MaxPoly v c -> MaxPoly v' c
-- rename f (Var v) = Var (f v)
-- rename _ (Const c) = Const c
-- rename f (Plus p q) = rename f p `Plus` rename f q
-- rename f (Mult p q) = rename f p `Mult` rename f q
-- rename f (Max p q)  = rename f p `Max` rename f q

fromMaxPoly :: (Max a, SemiRing a) => (v -> a) -> (c -> a) -> MaxPoly v c -> a
fromMaxPoly var _   (Var v)    = var v
fromMaxPoly _   con (Const c)  = con c
fromMaxPoly var con (Plus p q) = fromMaxPoly var con p .+ fromMaxPoly var con q
fromMaxPoly var con (Mult p q) = fromMaxPoly var con p .* fromMaxPoly var con q
fromMaxPoly var con (Max p q)  = fromMaxPoly var con p `maxA` fromMaxPoly var con q

substitute :: (Eq c, SemiRing c) => (v -> MaxPoly v' c) -> MaxPoly v c -> MaxPoly v' c
substitute s (Var v)    = s v
substitute s (Const c)  = Const c
substitute s (Plus p q) = substitute s p .+ substitute s q
substitute s (Mult p q) = substitute s p .* substitute s q
substitute s (Max p q)  = substitute s p `maxA` substitute s q
  
-- * max elimination

splitMax :: (Ord v, IsNat c, SemiRing c) =>  MaxPoly v c -> [P.Polynomial v c]
splitMax (Var v)    = [P.variable v]
splitMax (Const c)  = [P.coefficient c]
splitMax (Plus p q) = (.+) <$> splitMax p <*> splitMax q
splitMax (Mult p q) = (.*) <$> splitMax p <*> splitMax q
splitMax (Max p q)  = splitMax p ++ splitMax q

-- maxElim :: (Ord v, Eq c, IsNat c, SemiRing c) => Constraint (MaxPoly v c) -> [ConditionalConstraint (P.Polynomial v c)]
-- maxElim ieq = walk [([],ieq)] where
--   walk [] = []
--   walk ((ps,ieq):ceqs) =
--     case splitIEQ ieq of
--       Nothing -> CC { premises = ps, constraint = fmap toPoly ieq } : walk ceqs
--       Just (p,q,ctx) -> walk (cp:cq:ceqs)
--         where
--           p' = toPoly p
--           q' = toPoly q
--           cp = ((p' :>=: q')          : ps,ctx p)
--           cq = ((q' :>=: (p' .+ one)) : ps,ctx q)
                      
--   toPoly (Var v)    = P.variable v
--   toPoly (Const c)  = P.coefficient c
--   toPoly (Plus p q) = toPoly p .+ toPoly q
--   toPoly (Mult p q) = toPoly p .* toPoly q
--   toPoly (Max {})     = error "maxElim: polynomial still contains max"
  
--   splitIEQ (p1 :>=: p2) =
--     case (splitMaxPoly p1, splitMaxPoly p2) of
--       (Right (p,q,ctx), _              ) -> Just (p,q, \pi -> ctx pi :>=: p2)
--       (_              , Right (p,q,ctx)) -> Just (p,q, \pi -> p1 :>=: ctx pi)    
--       (_              , _              ) -> Nothing
--   splitBinOp c p1 p2 = 
--     case (splitMaxPoly p1, splitMaxPoly p2) of
--       (Right (p1',p2',ctx), _                  ) -> Right (p1',p2', \ pi' -> c (ctx pi') p2)
--       (_                  , Right (p1',p2',ctx)) -> Right (p1',p2', \ pi' -> c p1 (ctx pi'))
--       (_                  , _                  ) -> Left (c p1 p2)
--   splitMaxPoly p@(Var _) = Left p
--   splitMaxPoly p@(Const _) = Left p
--   splitMaxPoly (Max p1 p2)  =
--     case splitBinOp Max p1 p2 of {Left _ -> Right (p1, p2, id); s -> s}
--   splitMaxPoly (Plus p1 p2) = splitBinOp Plus p1 p2
--   splitMaxPoly (Mult p1 p2) = splitBinOp Mult p1 p2  
      
-- pretty printing

simp :: (Ord c, Ord v, IsNat c, SemiRing c) => MaxPoly v c -> MaxPoly v c
simp = fromPolyList . filterSubsumed . nub . splitMax where -- 
  fromPolyList [] = zero
  fromPolyList ps = maximumA (map fromPoly ps)

  fromPoly = P.fromPolynomial variable constant
  -- TODO  max(x0 + x1 + x2,1 + x0)
  filterSubsumed ps = foldr (\ p -> filter (not . subsumes p)) ps ps
  p1 `subsumes` p2 =
    and [ c1 >= c2 | (c1 :>=: c2) <- P.strictlyPositive (p1 `P.minus` p2)]
    && p1 /= p2

instance (Eq c, Eq v, IsNat c, Max c, SemiRing c, PP.Pretty v, PP.Pretty c) => PP.Pretty (MaxPoly v c) where
  pretty = pp id where
    pp _   (Var v)      = PP.pretty v
    pp _   (Const i)    = PP.pretty i
    pp par (Mult t1 t2) = ppBin par "*" (pp PP.parens t1) (pp PP.parens t2)
    pp par (Plus t1 t2) = ppBin par "+" (PP.pretty t1) (PP.pretty t2)
    pp par (Max t1 t2)  = par (PP.text "max" PP.<> PP.tupled [PP.pretty t1, PP.pretty t2])

