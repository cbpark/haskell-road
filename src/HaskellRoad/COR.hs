--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.COR
--
-- Chapter 10. Corecursion
--
--------------------------------------------------------------------------------
module HaskellRoad.COR where

default (Integer, Rational, Double)

ones :: [Integer]
ones = 1 : ones

nats :: [Integer]
nats = 0 : map (+1) nats

odds :: [Integer]
odds = 1 : map (+2) odds

{-
iterate :: (a -> a) -> a -> [a]
iterate f x = x : interate f (f x)
-}

theOnes :: [Integer]
-- theOnes = iterate id 1
theOnes = repeat 1

theNats :: [Integer]
theNats = iterate (+1) 0

theOdds :: [Integer]
theOdds = iterate (+2) 1

theNats1 :: [Integer]
theNats1 = 0 : zipWith (+) ones theNats1

theFibs :: [Integer]
theFibs = 0 : 1 : zipWith (+) theFibs (tail theFibs)

sieve' :: [Integer] -> [Integer]
sieve' (n:xs) = n : sieve' (filter (\m -> m `rem` n /= 0) xs)

primes' :: [Integer]
primes' = sieve' [2..]
