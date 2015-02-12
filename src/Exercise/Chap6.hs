module Exercise.Chap6 where

import           Exercise.Chap4  ((\\))
import           HaskellRoad.FCT

import           Data.Char       (isAlpha)

-- | Exercise 6.23
bijective :: Eq b => (a -> b) -> [a] -> [b] -> Bool
bijective f dom codom = injective f dom && surjective f dom codom

-- | Exercise 6.24
injectivePairs :: (Eq a, Eq b) => [(a, b)] -> [a] -> Bool
injectivePairs f = injective (list2fct f)

-- | Exercise 6.24
surjectivePairs :: (Eq a, Eq b) => [(a, b)] -> [a] -> [b] -> Bool
surjectivePairs f = surjective (list2fct f)

-- | Exercise 6.24
bijectivePairs :: (Eq a, Eq b) => [(a, b)] -> [a] -> [b] -> Bool
bijectivePairs f = bijective (list2fct f)

-- | Exercise 6.27
injs :: [Int] -> [Int] -> [[(Int, Int)]]
injs []     _  = [[]]
injs _      [] = []
injs (x:xs) ys = concat [map ((x, y):) (injs xs (ys \\ [y])) | y <- ys]

-- | Exercise 6.28
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concatMap (interleave x) (perms xs)
  where interleave :: a -> [a] -> [[a]]
        interleave x' []     = [[x']]
        interleave x' (y:ys) = (x':y:ys) : map (y:) (interleave x' ys)

-- | Exercise 6.32
comp :: (Eq a, Eq b) => [(b, c)] -> [(a, b)] -> [(a, c)]
f `comp` g = [(x, list2fct f y) | (x, y) <- g]

-- | Exercise 6.57
stringCompare :: String -> String -> Maybe Ordering
stringCompare str1 str2
  | any (not . isAlpha) (str1 ++ str2) = Nothing
  | otherwise                          = Just $ compare str1 str2

-- | Exercise 6.54
--
-- >>> fct2listpart even [1..20]
-- [[1,3,5,7,9,11,13,15,17,19],[2,4,6,8,10,12,14,16,18,20]]
-- >>> fct2listpart (`rem` 3) [1..20]
-- [[1,4,7,10,13,16,19],[2,5,8,11,14,17,20],[3,6,9,12,15,18]]
fct2listpart :: (Eq a, Eq b) => (a -> b) -> [a] -> [[a]]
fct2listpart _ [] = []
fct2listpart f (x:xs) = xclass : fct2listpart f (xs \\ xclass)
  where xclass = x : [y | y <- xs, f x == f y]
