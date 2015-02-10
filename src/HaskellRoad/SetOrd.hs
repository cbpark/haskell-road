--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.SetOrd
--
-- Sets implemented as ordered lists without duplicates.
--
--------------------------------------------------------------------------------
module HaskellRoad.SetOrd where

import           Data.List (sort)

newtype Set a = Set [a] deriving (Eq, Ord)

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

inSet :: Ord a => a -> Set a -> Bool
inSet x (Set s) = x `elem` takeWhile (<= x) s

subSet :: Ord a => Set a -> Set a -> Bool
subSet (Set [])     _   = True
subSet (Set (x:xs)) set = x `inSet` set && subSet (Set xs) set

insertSet :: Ord a => a -> Set a -> Set a
insertSet x (Set s) = Set $ insertList x s

insertList :: Ord a => a -> [a] -> [a]
insertList x []         = [x]
insertList x ys@(y:ys') = case compare x y of GT -> y : insertList x ys'
                                              EQ -> ys
                                              _  -> x : ys

deleteSet :: Ord a => a -> Set a -> Set a
deleteSet x (Set s) = Set $ deleteList x s

deleteList :: Ord a => a -> [a] -> [a]
deleteList _ []         = []
deleteList x ys@(y:ys') = case compare x y of GT -> y : deleteList x ys'
                                              EQ -> ys'
                                              _  -> ys

list2set :: Ord a => [a] -> Set a
-- list2set []     = Set []
-- list2set (x:xs) = insertSet x (list2set xs)
-- list2set xs = Set $ foldr insertList [] xs
list2set = foldr insertSet (Set [])

powerSet :: Ord a => Set a -> Set (Set a)
powerSet (Set xs) = Set $ (sort . map list2set) (powerList xs)

powerList :: [a] -> [[a]]
powerList []     = [[]]
powerList (x:xs) = powerList xs ++ map (x:) (powerList xs)

takeSet :: Eq a => Int -> Set a -> Set a
takeSet n (Set xs) = Set $ take n xs

infixl 9 !!!

(!!!) :: Eq a => Set a -> Int -> a
Set xs !!! n = xs !! n

unionSet :: Ord a => Set a -> Set a -> Set a
unionSet (Set []) set2     = set2
unionSet (Set (x:xs)) set2 = insertSet x $ unionSet (Set xs) (deleteSet x set2)
