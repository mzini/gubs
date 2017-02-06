module GUBS.Solver.Formula where

import GUBS.Algebra

data Atom e = Geq e e | Eq e e
  deriving (Functor, Foldable, Traversable)
  
data Formula e =
  Top
  | Bot
  | Atom (Atom e)
  | Or (Formula e) (Formula e)
  | And (Formula e) (Formula e)  
  deriving (Functor, Foldable, Traversable)

subst :: (Atom e -> Formula e') -> Formula e -> Formula e'
subst _ Top = Top
subst _ Bot = Bot
subst f (Atom a) = f a
subst f (Or e1 e2) = Or (subst f e1) (subst f e2)
subst f (And e1 e2) = And (subst f e1) (subst f e2)
  
gtA,eqA,geqA :: (IsNat e, Additive e) => e -> e -> Formula e
gtA e1 e2 = Atom (Geq e1 (e2 .+ fromNatural 1))
eqA e1 e2 = Atom (Eq e1 e2)
geqA e1 e2 = Atom (Geq e1 e2)

smtTop, smtBot :: Formula s
smtTop = Top
smtBot = Bot

smtBool :: Bool -> Formula s
smtBool True  = Top
smtBool False = Bot

smtNot :: (IsNat e, Additive e) => Formula e -> Formula e
smtNot Top = Bot
smtNot Bot = Top
smtNot (Atom (Geq e1 e2)) = e2 `gtA` e1
smtNot (Atom (Eq e1 e2)) = Or (e1 `gtA` e2) (e2 `gtA` e1)
smtNot (Or f1 f2) = And (smtNot f1) (smtNot f2)
smtNot (And f1 f2) = Or (smtNot f1) (smtNot f2)


smtAnd, smtOr :: Formula s -> Formula s -> Formula s
Top `smtAnd` f2  = f2
f1  `smtAnd` Top = f1
Bot `smtAnd` _   = Bot
_   `smtAnd` Bot = Bot
f1  `smtAnd` f2  = And f1 f2

Bot `smtOr` f2  = f2
f1  `smtOr` Bot = f1
Top `smtOr` _   = Top
_   `smtOr` Top = Top
f1  `smtOr` f2  = Or f1 f2


-- smtIte :: Formula e -> Formula e -> Formula e -> Formula e
-- smtIte Top t   _   = t
-- smtIte Bot _   e   = e
-- smtIte g   Bot e   = smtNot g `smtAnd` e
-- smtIte g   t   Bot = g `smtAnd` t
-- smtIte 

smtBigOr, smtBigAnd :: [Formula s] -> Formula s
smtBigOr = foldr smtOr smtBot
smtBigAnd = foldr smtAnd smtTop
