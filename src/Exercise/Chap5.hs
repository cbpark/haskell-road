module Exercise.Chap5 where

import           Exercise.Chap4     ((\\))
import           HaskellRoad.REL
import           HaskellRoad.SetOrd
import           HaskellRoad.STAL   (intersect, union)

-- | Exercise 5.52
--
-- restriction of a relation to a set.
restrictR :: Ord a => Set a -> Rel a -> Rel a
restrictR set = intersectSet (totalR set)

intersectSet :: Ord a => Set a -> Set a -> Set a
intersectSet (Set [])     _   = Set []
intersectSet (Set (x:xs)) set
  | x `inSet` set = insertSet x (intersectSet (Set xs) set)
  | otherwise     = intersectSet (Set xs) set

-- | Exercise 5.53
rclosR :: Ord a => Rel a -> Rel a
rclosR r = unionSet r (idR background)
  where background = unionSet (domR r) (ranR r)

-- | Exercise 5.53
sclosR :: Ord a => Rel a -> Rel a
sclosR r = unionSet r (invR r)

-- | Exercise 5.54
tclosR :: Ord a => Rel a -> Rel a
tclosR r | transR r  = r
         | otherwise = tclosR (unionSet r (compR r r))

-- | Exercise 5.56
transClosure' :: [a] -> Rel' a -> Rel' a
transClosure' xs r | transR' xs r = r
                   | otherwise    = transClosure' xs (unionR' r (compR' xs r r))

-- | Exercise 5.82
raccess :: Rel' a -> a -> [a] -> [a]
raccess r x list = [y | y <- list, r x y]

-- | Exercise 5.104
stirling :: Integer -> Integer -> Integer
stirling _ 1             = 1
stirling n k | n == k    = 1
             | otherwise = k * stirling (n-1) k + stirling (n-1) (k-1)

-- | Exercise 5.104
bell :: Integer -> Integer
bell 0 = 1
bell n = sum [stirling n k | k <- [1..n]]

-- | Exercise 5.109
listPartition :: Eq a => [a] -> [[a]] -> Bool
listPartition xs xss = all (`elem` xs) (concat xss) && all (`elem` concat xss) xs
                       && listPartition' xss []
  where listPartition' []         _       = True
        listPartition' ([]:_)     _       = False
        listPartition' (xs':xss') domain
          | null (xs' `intersect` domain) = listPartition' xss' (xs' `union` domain)
          | otherwise                     = False

-- | Exercise 5.110
listpart2equiv :: Ord a => [a] -> [[a]] -> Rel a
listpart2equiv domain xss
  | not (listPartition domain xss) = error "not a list partition"
  | otherwise                      = list2set $ concatMap f xss
  where f xs = [(x, y) | x <- xs, y <- xs]

-- | Exercise 5.112
equiv2listpart :: Ord a => Set a -> Rel a -> [[a]]
equiv2listpart s@(Set xs) r
  | not (equivalenceR s r) = error "not an equivalence relation"
  | otherwise              = listpart r xs
  where listpart _  []      = []
        listpart r' (x:xs') = xclass : listpart r' (xs' \\ xclass)
          where xclass = x : [y | y <- xs', (x, y) `inSet` r']

-- | Exercise 5.113
equiv2part :: Ord a => Set a -> Rel a -> Set (Set a)
equiv2part set r = list2set $ map list2set (equiv2listpart set r)
