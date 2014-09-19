--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.GS
--
-- Chapter 1. Getting Started
--
--------------------------------------------------------------------------------
module HaskellRoad.GS where

import           Exercise.Chap1 (removeFst)

divides :: Integer -> Integer -> Bool
divides d n = n `rem` d == 0

ld :: Integer -> Integer
ld = ldf 2

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k ^ 2 > n   = n
        | otherwise   = ldf (k + 1) n

prime0 :: Integer -> Bool
prime0 n | n < 1     = error "not a positive integer"
         | n == 1    = False
         | otherwise = ld n == n

mnmInt :: [Int] -> Int
mnmInt []     = error "empty list"
mnmInt [x]    = x
mnmInt (x:xs) = min x (mnmInt xs)

min' :: Int -> Int -> Int
min' x y | x <= y    = x
         | otherwise = y

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : srtInts (removeFst m xs) where m = mnmInt xs

srtInts' :: [Int] -> [Int]
srtInts' [] = []
srtInts' xs = let m = mnmInt xs
              in m : srtInts' (removeFst m xs)

average :: [Int] -> Double
average [] = error "empty list"
average xs = (fromIntegral . sum) xs / (fromIntegral . length) xs

sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

prefix :: String -> String -> Bool
prefix []     _      = True
prefix _      []     = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

factors :: Integer -> [Integer]
factors n | n < 1     = error "argument not positive"
          | n == 1    = []
          | otherwise = p : factors (n `div` p) where p = ld n

primes0 :: [Integer]
primes0 = filter prime0 [2..]

ldp :: Integer -> Integer
ldp = ldpf primes1

ldpf :: [Integer] -> Integer -> Integer
ldpf []     _                  = 0
ldpf (p:ps) n | n `rem` p == 0 = p
              | p ^ 2 > n      = n
              | otherwise      = ldpf ps n

primes1 :: [Integer]
primes1 = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n | n < 1     = error "not a positive integer"
        | n == 1    = False
        | otherwise = ldp n == n
