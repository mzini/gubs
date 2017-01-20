module GUBS.MaxPolynomial where

import qualified Text.PrettyPrint.ANSI.Leijen as PP

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

instance IsNat c => IsNat (MaxPoly v c) where
  fromNatural_ = Const . fromNatural
  
instance Additive c => Additive (MaxPoly v c) where
  zero = Const zero
  t1 .+ t2 = Plus t1 t2

instance Additive c => Max (MaxPoly v c) where
  t1 `maxA` t2 = Max t1 t2
  
instance Multiplicative c => Multiplicative (MaxPoly v c) where
  one = Const one
  t1 .* t2 = Mult t1 t2

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

substitute :: SemiRing c => (v -> MaxPoly v' c) -> MaxPoly v c -> MaxPoly v' c
substitute s (Var v)    = s v
substitute s (Const c)  = Const c
substitute s (Plus p q) = substitute s p .+ substitute s q
substitute s (Mult p q) = substitute s p .* substitute s q
substitute s (Max p q)  = substitute s p `maxA` substitute s q
  
-- * max elimination

maxElim :: (Ord v, Eq c, IsNat c, SemiRing c) => Constraint (MaxPoly v c) -> [ConditionalConstraint (P.Polynomial v c)]
maxElim ieq = walk [([],ieq)] where
  walk [] = []
  walk ((ps,ieq):ceqs) =
    case splitIEQ ieq of
      Nothing -> CC { premises = ps, constraint = fmap toPoly ieq } : walk ceqs
      Just (p,q,ctx) -> walk (cp:cq:ceqs)
        where
          p' = toPoly p
          q' = toPoly q
          cp = ((p' :>=: q')          : ps,ctx p)
          cq = ((q' :>=: (p' .+ one)) : ps,ctx q)
                      
  toPoly (Var v)    = P.variable v
  toPoly (Const c)  = P.coefficient c
  toPoly (Plus p q) = toPoly p .+ toPoly q
  toPoly (Mult p q) = toPoly p .* toPoly q
  toPoly (Max {})     = error "maxElim: polynomial still contains max"
  
  splitIEQ (p1 :>=: p2) =
    case (splitMaxPoly p1, splitMaxPoly p2) of
      (Right (p,q,ctx), _              ) -> Just (p,q, \pi -> ctx pi :>=: p2)
      (_              , Right (p,q,ctx)) -> Just (p,q, \pi -> p1 :>=: ctx pi)    
      (_              , _              ) -> Nothing
  splitBinOp c p1 p2 = 
    case (splitMaxPoly p1, splitMaxPoly p2) of
      (Right (p1',p2',ctx), _                  ) -> Right (p1',p2', \ pi' -> c (ctx pi') p2)
      (_                  , Right (p1',p2',ctx)) -> Right (p1',p2', \ pi' -> c p1 (ctx pi'))
      (_                  , _                  ) -> Left (c p1 p2)
  splitMaxPoly p@(Var _) = Left p
  splitMaxPoly p@(Const _) = Left p
  splitMaxPoly (Max p1 p2)  =
    case splitBinOp Max p1 p2 of {Left _ -> Right (p1, p2, id); s -> s}
  splitMaxPoly (Plus p1 p2) = splitBinOp Plus p1 p2
  splitMaxPoly (Mult p1 p2) = splitBinOp Mult p1 p2  
      
-- pretty printing

simp :: (Eq c, Eq c, Eq v, IsNat c, SemiRing c, Max c) => MaxPoly v c -> MaxPoly v c
simp = simp' where
  simp' (Var v)      = Var v  
  simp' (Const i)    = Const i
  simp' (Mult t1 t2) = simp' t1 `mult` simp' t2
  simp' (Plus t1 t2) = simp' t1 `plus` simp' t2
  simp' (Max t1 t2)  = simp' t1 `max` simp' t2

  Const i `mult` t2
    | fromNatural 0 == i = Const (fromNatural 0)
    | fromNatural 1 == i = t2
  t1      `mult` Const i
    | fromNatural 0 == i = Const (fromNatural 0)
    | fromNatural 1 == i = t1
  Const i `mult` Const j = Const (i .* j)  
  t1      `mult` t2      = t1 `Mult` t2

  Const i `plus` t2      | fromNatural 0 == i = t2
  t1      `plus` Const i | fromNatural 0 == i = t1
  Const i `plus` Const j = Const (i .+ j)  
  t1      `plus` t2      = t1 `Plus` t2

  Const i `max` t2       | fromNatural 0 == i = t2
  t1      `max` Const i  | fromNatural 0 == i = t1  
  t1      `max` t2       | t1 == t2           = t1
                         | otherwise          = t1 `Max` t2

  
  

instance (Eq c, Eq v, IsNat c, SemiRing c, Max c, PP.Pretty v, PP.Pretty c) => PP.Pretty (MaxPoly v c) where
  pretty = pp id . simp where
    pp _   (Var v)      = PP.pretty v
    pp _   (Const i)    = PP.pretty i
    pp par (Mult t1 t2) = ppBin par "*" t1 t2
    pp par (Plus t1 t2) = ppBin par "+" t1 t2
    pp par (Max t1 t2)  = par (PP.text "max" PP.<> PP.tupled [PP.pretty t1, PP.pretty t2])
