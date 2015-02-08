--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.STAL
--
-- Chapter 4. Sets, Types and Lists
--
--------------------------------------------------------------------------------
module HaskellRoad.STAL where

naturals :: [Integer]
naturals = [0..]

evens1 :: [Integer]
evens1 = [n | n <- naturals, even n]

odds1 :: [Integer]
odds1 = [n | n <- naturals, odd n]

evens2 :: [Integer]
evens2 = [2*n | n <- naturals]

smallSquares1 :: [Integer]
smallSquares1 = [n^2 | n <- [0..999]]

-- | Unlike 'smallsquares1', this never terminates.
smallSquares2 :: [Integer]
smallSquares2 = [n^2 | n <- naturals, n < 1000]

run :: Integer -> [Integer]
run n | n < 1  = error "argument not positive"
      | n == 1 = [1]
      | even n = n : run (n `div` 2)
      | odd n  = n : run (3*n + 1)

{-
funny x | halts x x = undefined
        | otherwise = True

halts f x = f /= g
  where g y | y == x   = undefined
            | otherwise = f y
-}

ones :: [Integer]
ones = 1 : ones
