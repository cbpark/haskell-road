module Exercise.Chap5 where

import           HaskellRoad.REL
import           HaskellRoad.SetOrd

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
