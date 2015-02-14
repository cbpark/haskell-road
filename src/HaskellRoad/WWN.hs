--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.WWN
--
-- Chapter 8. Working with Numbers
--
--------------------------------------------------------------------------------
module HaskellRoad.WWN where

import           HaskellRoad.Nats

import           Data.Ratio       ((%))

{-
gcd :: Integral a => a -> a -> a
gcd 0 0 = error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y = gcd' (abs x) (abs y)
  where gcd' x' 0 = x'
        gcd' x' y' = gcd' y' (x' `rem` y')
-}

coprime :: Integer -> Integer -> Bool
coprime m n = gcd m n == 1

data Sgn = P | N deriving (Eq, Show)
type MyInt = (Sgn, Natural)

myplus :: MyInt -> MyInt -> MyInt
myplus (s1, m) (s2, n) | s1 == s2          = (s1, m + n)
                       | s1 == P && n <= m = (P, m - n)
                       | s1 == P && n >  m = (N, n - m)
                       | otherwise         = myplus (s2, n) (s1, m)

type NatPair = (Natural, Natural)

plus1 :: NatPair -> NatPair -> NatPair
plus1 (m1, m2) (n1, n2) = (m1 + n1, m2 + n2)

subtr1 :: NatPair -> NatPair -> NatPair
subtr1 (m1, m2) (n1, n2) = plus1 (m1, m2) (n2, n1)

mult1 :: NatPair -> NatPair -> NatPair
mult1 (m1, m2) (n1, n2) = (m1*n1 + m2*n2, m1*n2 + m2*n1)

eq1 :: NatPair -> NatPair -> Bool
eq1 (m1, m2) (n1, n2) = m1 + n2 == m2 + n1

reduce1 :: NatPair -> NatPair
reduce1 (m1, Z)      = (m1, Z)
reduce1 (Z, m2)      = (Z, m2)
reduce1 (S m1, S m2) = reduce1 (m1, m2)

mechanicsRule :: Rational -> Rational -> Rational
mechanicsRule p x = (1 % 2) * (x + (p * recip x))

mechanics :: Rational -> Rational -> [Rational]
mechanics = iterate . mechanicsRule

sqrtM :: Rational -> [Rational]
sqrtM p | p < 0     = error "negative argument"
        | otherwise = mechanics p s
  where s = if null xs then 1 else last xs
        xs = takeWhile (\m -> m^2 <= p) [1..]

approximate :: Rational -> [Rational] -> Rational
approximate eps (x:y:zs) | abs (y - x) < eps = y
                         | otherwise         = approximate eps (y:zs)

apprx :: [Rational] -> Rational
apprx = approximate (1/10^6)

mySqrt :: Rational -> Rational
mySqrt = apprx . sqrtM
