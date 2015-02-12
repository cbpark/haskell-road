--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.FCT
--
-- Chapter 6. Functions
--
--------------------------------------------------------------------------------
module HaskellRoad.FCT where

import           HaskellRoad.STAL (nub)

list2fct :: Eq a => [(a, b)] -> a -> b
list2fct []           _             = error "function not total"
list2fct ((u, v):uvs) x | x == u    = v
                        | otherwise = list2fct uvs x

fcn2list :: (a -> b) -> [a] -> [(a, b)]
fcn2list f xs = [(x, f x) | x <- xs]

-- | range of a function, implemented as a list of pairs.
ranPairs :: Eq b => [(a, b)] -> [b]
ranPairs f = nub [y | (_, y) <- f]

listValues :: Enum a => (a -> b) -> a -> [b]
listValues f i = f i : listValues f (succ i)

listRanges :: (Bounded a, Enum a) => (a -> b) -> [b]
listRanges f = [f i | i <- [minBound .. maxBound]]

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

fac :: Integer -> Integer
fac 0 = 1
fac n = fac (n - 1) * n

fac' :: Integer -> Integer
fac' n = product [1..n]

restrict :: Eq a => (a -> b) -> [a] -> a -> b
restrict f xs x | x `elem` xs = f x
                | otherwise   = error "argument not in domain"

restrictPairs :: Eq a => [(a, b)] -> [a] -> [(a, b)]
restrictPairs xys xs = [(x, y) | (x, y) <- xys, x `elem` xs]

-- |
-- >>> image (*2) [1,2,3]
-- [2,4,6]
image :: Eq b => (a -> b) -> [a] -> [b]
image f xs = nub [f x | x <- xs]

-- |
-- >>> coImage (*2) [1,2,3] [2,3,4]
-- [1,2]
coImage :: Eq b => (a -> b) -> [a] -> [b] -> [a]
coImage f xs ys = [x | x <- xs, f x `elem` ys]

imagePairs :: (Eq a, Eq b) => [(a, b)] -> [a] -> [b]
imagePairs f = image (list2fct f)

coImagePairs :: (Eq a, Eq b) => [(a, b)] -> [a] -> [b] -> [a]
coImagePairs f = coImage (list2fct f)

injective :: Eq b => (a -> b) -> [a] -> Bool
injective _ []     = True
injective f (x:xs) = f x `notElem` image f xs && injective f xs

surjective :: Eq b => (a -> b) -> [a] -> [b] -> Bool
surjective _ _  []     = True
surjective f xs (y:ys) = y `elem` image f xs && surjective f xs ys
