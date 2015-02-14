--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.POL
--
-- Chapter 9. Polynomials
--
--------------------------------------------------------------------------------
module HaskellRoad.POL where

import           Data.Ratio (denominator, numerator, (%))

-- |
-- >>> difs [1,4,11,22,37,56,79,106,137,172,211,254,301]
-- [3,7,11,15,19,23,27,31,35,39,43,47]
--
-- >>> (difs . difs. difs) [-12,-11,6,45,112,213,354,541,780,1077]
-- [6,6,6,6,6,6,6]
difs :: [Integer] -> [Integer]
difs []       = []
difs [_]      = []
difs (n:m:ks) = m - n : difs (m:ks)

-- |
-- >>> difLists [[-12,-11,6,45,112,213,354,541,780,1077]]
-- [[6,6,6,6,6,6,6],[16,22,28,34,40,46,52,58],[1,17,39,67,101,141,187,239,297],[-12,-11,6,45,112,213,354,541,780,1077]]
difLists :: [[Integer]] -> [[Integer]]
difLists [] = []
difLists lists@(xs:_) = if constant xs then lists else difLists (difs xs : lists)
  where constant (n:m:ms) = all (==n) (m:ms)
        constant _        = error "lack of data or not a polynomial fct"

genDifs :: [Integer] -> [Integer]
genDifs xs = map last (difLists [xs])

nextD :: [Integer] -> [Integer]
nextD []       = error "no data"
nextD [n]      = [n]
nextD (n:m:ks) = n : nextD (n + m : ks)

-- |
-- >>> next [-12,-11,6,45,112,213,354,541,780,1077]
-- 1438
next :: [Integer] -> Integer
next = last . nextD . genDifs

-- |
-- >>> take 20 (continue [-12,-11,6,45,112,213,354,541,780,1077])
-- [1438,1869,2376,2965,3642,4413,5284,6261,7350,8557,9888,11349,12946,14685,16572,18613,20814,23181,25720,28437]
continue :: [Integer] -> [Integer]
continue xs = map last (iterate nextD differences)
  where differences = nextD (genDifs xs)

degree :: [Integer] -> Int
degree xs = length (difLists [xs]) - 1

type Matrix = [Row]
type Row = [Integer]

rows, cols :: Matrix -> Int
rows = length
cols m | null m    = 0
       | otherwise = (length . head) m

-- |
-- >>> genMatrix [-7,-2,15,50,109,198,323]
-- [[1,0,0,0,-7],[1,1,1,1,-2],[1,2,4,8,15],[1,3,9,27,50]]
genMatrix :: [Integer] -> Matrix
genMatrix xs = zipWith (++) (genM d) [[x] | x <- xs]
  where d = degree xs
        genM n = [[toInteger x^m | m <- [0..n]] |  x <- [0..n]]

adjustWith :: Row -> Row -> Row
adjustWith (m:ms) (n:ns) = zipWith (-) (map (n*) ms) (map (m*) ns)

eliminate :: Rational -> Matrix -> Matrix
eliminate p = map (simplify c a)
  where c = numerator p
        a = denominator p
        simplify c' a' row = (init . init) row' ++ [a'*d - b*c']
          where d = last row
                b = (last . init) row
                row' = map (*a) row

-- |
-- >>> backsubst [[1,0,0,0,-7],[0,-1,-1,-1,-5],[0,0,-2,-6,-12],[0,0,0,-12,-12]]
-- [(-7) % 1,1 % 1,3 % 1,1 % 1]
backsubst :: Matrix -> [Rational]
backsubst rs = backsubst' rs []
  where backsubst' []   ps = ps
        backsubst' rs0  ps = backsubst' rs' (p:ps)
          where a = last rs0 !! (cols rs0 - 2)
                c = last rs0 !! (cols rs0 - 1)
                p = c % a
                rs' = eliminate p (init rs0)

choose :: Integer -> Integer -> Integer
choose n k = product [(n-k+1)..n] `div` product [1..k]

choose' :: Integer -> Integer -> Integer
choose' _ 0             = 1
choose' n k | n <  k    = 0
            | n == k    = 1
            | otherwise = choose' (n-1) (k-1) + choose' (n-1) k

binom :: Integer -> Integer -> Integer
binom _ 0             = 1
binom n k | n < k     = 0
          | otherwise = n * binom (n-1) (k-1) `div` k
