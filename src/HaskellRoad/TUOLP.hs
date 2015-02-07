module HaskellRoad.TUOLP where

sieve :: [Integer] -> [Integer]
sieve []     = [0]
sieve (0:xs) = sieve xs
sieve (n:xs) = n : sieve (mark xs 1 n)
  where mark :: [Integer] -> Integer -> Integer -> [Integer]
        mark (y:ys) k m | k == m    = 0 : mark ys 1     m
                        | otherwise = y : mark ys (k+1) m
        mark [] _ _                 = [0]

primes :: [Integer]
primes = sieve [2..]
