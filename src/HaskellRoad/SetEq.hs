--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.SetEq
--
--------------------------------------------------------------------------------
module HaskellRoad.SetEq where

import           Data.List        (delete)

import           HaskellRoad.STAL (powerList)

infixl 9 !!!

newtype Set a = Set [a]

instance Eq a => Eq (Set a) where
  set1 == set2 = set1 `subSet` set2 && set2 `subSet` set1

instance Show a => Show (Set a) where
  showsPrec _ (Set s) = showSet s

showSet :: Show a => [a] -> String -> String
showSet []     str = showString "{}" str
showSet (x:xs) str = showChar '{' (shows x (showl xs str))
  where showl []     str' = showChar '}' str'
        showl (y:ys) str' = showChar ',' (shows y (showl ys str'))

emptySet :: Set a
emptySet = Set []

isEmpty :: Set a -> Bool
isEmpty (Set []) = True
isEmpty _        = False

inSet :: Eq a => a -> Set a -> Bool
inSet x (Set s) = x `elem` s

subSet :: Eq a => Set a -> Set a -> Bool
subSet (Set [])     _   = True
subSet (Set (x:xs)) set = x `inSet` set && subSet (Set xs) set

insertSet :: Eq a => a -> Set a -> Set a
insertSet x (Set ys) | x `inSet` Set ys = Set ys
                     | otherwise        = Set (x:ys)

deleteSet :: Eq a => a -> Set a -> Set a
deleteSet x (Set xs) = Set (delete x xs)

list2set :: Eq a => [a] -> Set a
list2set = foldr insertSet (Set [])

powerSet :: Eq a => Set a -> Set (Set a)
powerSet (Set xs) = Set $ map Set (powerList xs)

takeSet :: Eq a => Int -> Set a -> Set a
takeSet n (Set xs) = Set (take n xs)

(!!!) :: Eq a => Set a -> Int -> a
(Set xs) !!! n = xs !! n
