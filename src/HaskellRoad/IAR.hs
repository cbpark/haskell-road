--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.IAR
--
-- Chapter 7. Induction and Recursion
--
--------------------------------------------------------------------------------
module HaskellRoad.IAR where

sumOdds' :: Integer -> Integer
sumOdds' n = sum [2*k - 1 | k <- [1..n]]

sumOdds :: Integer -> Integer
sumOdds n = n^2

sumEvens' :: Integer -> Integer
sumEvens' n = sum [2*k | k <- [1..n]]

sumEvens :: Integer -> Integer
sumEvens n = n * (n+1)

sumInts :: Integer -> Integer
sumInts n = n * (n+1) `div` 2

sumSquares' :: Integer -> Integer
sumSquares' n = sum [k^2 | k <- [1..n]]

sumSquares :: Integer -> Integer
sumSquares n = n * (n+1) * (2*n+1) `div` 6

sumCubes' :: Integer -> Integer
sumCubes' n = sum [k^3 | k <- [1..n]]

sumCubes :: Integer -> Integer
sumCubes n = (n * (n+1) `div` 2) ^ 2

data Natural = Z | S Natural deriving (Eq, Show)

plus :: Natural -> Natural -> Natural
-- m `plus` Z     = m
-- m `plus` (S n) = S (m `plus` n)
plus = foldn S

-- |
-- >>> (S (S Z)) `mult` (S (S (S Z)))
-- S (S (S (S (S (S Z)))))
mult :: Natural -> Natural -> Natural
-- _ `mult` Z     = Z
-- m `mult` (S n) = (m `mult` n) `plus` m
mult m = foldn (plus m) Z

-- |
-- >>> (S (S Z)) `expn` (S (S (S Z)))
-- S (S (S (S (S (S (S (S Z)))))))
expn :: Natural -> Natural -> Natural
-- _ `expn` Z     = S Z
-- m `expn` (S n) = (m `expn` n) `mult` m
expn m = foldn (mult m) (S Z)

leq :: Natural -> Natural -> Bool
Z     `leq` _     = True
(S _) `leq` Z     = False
(S m) `leq` (S n) = m `leq` n

geq :: Natural -> Natural -> Bool
m `geq` n = n `leq` m

gt :: Natural -> Natural -> Bool
m `gt` n = not (m `leq` n)

lt :: Natural -> Natural -> Bool
m `lt` n = not (m `geq` n)

foldn :: (a -> a) -> a -> Natural -> a
foldn _ c Z     = c
foldn h c (S n) = h (foldn h c n)

exclaim :: Natural -> String
exclaim = foldn ('!':) []
