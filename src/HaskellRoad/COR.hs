--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.COR
--
-- Chapter 10. Corecursion
--
--------------------------------------------------------------------------------
module HaskellRoad.COR where

import           HaskellRoad.Polynomials
import           HaskellRoad.PowerSeries

import           System.Random           (Random (..), mkStdGen)

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

randomInts :: Int -> Int -> [Int]
randomInts bound seed = tail $ randomRs (0, bound) (mkStdGen seed)

type Process = [Int] -> [String]

start :: Process -> Int -> Int -> [String]
start process bound seed = process (randomInts bound seed)

clock :: Process
clock (0:xs) = "tick" : clock xs
clock (1:_)  = ["crack"]

vending, vending1, vending2, vending3 :: Process
vending  (0:xs) = "coin"      : vending1 xs
vending  (1:xs) =               vending  xs
vending1 (0:xs) = "coin"      : vending2 xs
vending1 (1:xs) = "water"     : vending  xs
vending2 (0:xs) = "coin"      : vending3 xs
vending2 (1:xs) = "beer"      : vending  xs
vending3 (0:xs) = "moneyback" : vending  xs
vending3 (1:xs) =               vending3 xs

ptd :: Process
ptd = ptd0 0

ptd0 :: Int -> Process
ptd0 0 (0:xs) = ptd0 0 xs
ptd0 i (0:xs) = ("return " ++ show i ++ " euro") : ptd0 0 xs
ptd0 i (1:xs) = "1 euro" : ptd0 (i+1) xs
ptd0 i (2:xs) = "2 euro" : ptd0 (i+2) xs
ptd0 0 (3:xs) = ptd0 0 xs
ptd0 i (3:xs) = ("ticket " ++ show (i * 20) ++ " min") : ptd0 0 xs

actions :: [Int]
actions = user [0,0,1] responses

responses :: [String]
responses = vending actions

user :: [Int] -> [String] -> [Int]
user acts ~(r:s:p:resps) = acts ++ user (proc [r,s,p]) resps

proc :: [String] -> [Int]
proc ["coin", "coin", "beer"] = [0,0,1]

{-
undefined :: a
undefined | False = undefined
-}

approx :: Integer -> [a] -> [a]
approx _ []     = []
approx n (x:xs) = x : approx (n-1) xs

o2e :: Num a => [a]  -> [a]
o2e []     = []
o2e (f:fs) = f : o2e (deriv (f:fs))

e2o :: (Ord a, Fractional a) => [a] -> [a]
e2o []     = []
e2o (f:fs) = [f] + (int . e2o) fs
