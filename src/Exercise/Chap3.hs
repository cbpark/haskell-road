module Exercise.Chap3 where

import           HaskellRoad.GS    (prime)
import           HaskellRoad.TUOLP

-- | Exercise 3.38
fasterprimes :: [Integer]
fasterprimes = 2 : sieve oddsFrom3
  where oddsFrom3 :: [Integer]
        oddsFrom3 = 3 : map (+2) oddsFrom3

-- | Exercise 3.39
refute :: [[Integer]]
refute = [ take n fasterprimes | n <- [0..],
           (not . prime) (product (take n fasterprimes) + 1) ]
