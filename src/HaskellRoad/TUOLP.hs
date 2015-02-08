--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.TUOLP
--
-- Chapter 3. The Use of Logic Proof
--
--------------------------------------------------------------------------------
module HaskellRoad.TUOLP where

import           HaskellRoad.GS (prime)

sieve :: [Integer] -> [Integer]
sieve []     = []
sieve (0:xs) = sieve xs
sieve (n:xs) = n : sieve (mark xs 1 n)
  where mark :: [Integer] -> Integer -> Integer -> [Integer]
        mark (y:ys) k m | k == m    = 0 : mark ys 1     m
                        | otherwise = y : mark ys (k+1) m
        mark _     _ _             = []

primes :: [Integer]
primes = sieve [2..]

mersenne :: [(Integer, Integer)]
mersenne = [(p, 2^p - 1) | p <- primes, prime (2^p - 1)]

notmersenne :: [(Integer, Integer)]
notmersenne = [(p, 2^p - 1) | p <- primes, (not . prime) (2^p - 1)]

pdivisors :: Integer -> [Integer]
pdivisors n = [d | d <- [1..(n-1)], n `rem` d == 0]

primePairs :: [(Integer, Integer)]
primePairs = pairs primes
  where pairs (x:y:xys) | x + 2 == y = (x, y) : pairs (y:xys)
                        | otherwise  = pairs (y:xys)
        pairs _                      = []

primeTriples :: [(Integer, Integer, Integer)]
primeTriples = triples primes
  where triples (x:y:z:xyzs)
          | x + 2 == y && y + 2 == z = (x, y, z) : triples (y:z:xyzs)
          | otherwise                = triples (y:z:xyzs)
        triples _                    = []
