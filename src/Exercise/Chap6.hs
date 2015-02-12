module Exercise.Chap6 where

import           Exercise.Chap4  ((\\))
import           HaskellRoad.FCT

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
