module GUBS.Algebra where

infixr 8 .^
infixl 7 .*
infixl 6 .+, .-

class Additive a where
  (.+)   :: a -> a -> a
  zero  :: a
  
  sumA :: Foldable f => f a -> a
  sumA = foldr (.+) zero

class Additive a => Max a where
  maxA :: a -> a -> a
  maximumA :: Foldable f => f a -> a
  maximumA = foldr maxA zero


(.^) :: (Additive a, Integral b) => a -> b -> a
a .^ b = sumA (replicate (fromIntegral b) a)
  
class Additive a => AdditiveGroup a where
  neg    :: a -> a
  neg a = zero .- a
  (.-)   :: a -> a -> a
  a .- b = a .+ neg b

class Multiplicative a where
  (.*)   :: a -> a -> a
  one    :: a
  
  prod :: Foldable f => f a -> a
  prod = foldr (.*) one

type SemiRing a = (Additive a,      Multiplicative a)
type Ring  a    = (AdditiveGroup a, Multiplicative a)

class IsNat a where
  fromNatural_ :: Integer -> a

fromNatural :: (Integral a, IsNat b) => a -> b
fromNatural n | i < 0 = error "fromNatural: negative Integer"
              | otherwise = fromNatural_ i
  where i = fromIntegral n

-- instances

instance IsNat Integer where
  fromNatural_ i = i
instance Additive Integer where
  (.+) = (+)
  zero = 0
instance Max Integer where
  maxA = max
instance AdditiveGroup Integer where
  neg = negate
  (.-) = (-)
  
instance Multiplicative Integer where
  (.*) = (*)
  one = 1
  
  
  