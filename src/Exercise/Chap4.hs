module Exercise.Chap4 where

import           HaskellRoad.SetEq
import           HaskellRoad.STAL

-- | Exercise 4.47
splitList :: [a] -> [([a], [a])]
splitList [x,y]    = [([x], [y])]
splitList (x:y:ys) = ([x], y:ys) : map (\(us, vs) -> (x:us, vs)) (splitList (y:ys))
splitList _        = []

-- | Exercise 4.48
ex448 :: [String]
ex448 = [y | (x, y) <- act, x == "Robert De Niro" || x == "Kevin Spacey"]

-- | Exercise 4.49
ex449 :: [String]
ex449 = nub $ [x | ("Quentin Tarantino", x) <- act, releaseP (x, "1994")]
        ++ [x | ("Quentin Tarantino", x) <- direct, releaseP (x, "1994")]

-- | Exercise 4.50
ex450 :: [String]
ex450 = [x | (x, y) <- release, y > "1997", (not . actP) ("William Hurt", x)]

-- | Exercise 4.51
(\\) :: Eq a => [a] -> [a] -> [a]
[]     \\ _                = []
xs     \\ []               = xs
(x:xs) \\ ys | x `elem` ys = xs \\ ys
             | otherwise   = x : xs \\ ys

-- | Exercise 4.53
genUnion :: Eq a => [[a]] -> [a]
genUnion []       = []
genUnion [xs]     = xs
genUnion (xs:xss) = xs `union` genUnion xss

-- | Exercise 4.53
genIntersect :: Eq a => [[a]] -> [a]
genIntersect []       = error "undefined on empty list of lists"
genIntersect [xs]     = xs
genIntersect (xs:xss) = xs `intersect` genIntersect xss

-- | Exercise 4.54
unionSet :: Eq a => Set a -> Set a -> Set a
unionSet (Set [])     set = set
unionSet (Set (x:xs)) set = insertSet x (unionSet (Set xs) (deleteSet x set))

-- | Exercise 4.54
intersectSet :: Eq a => Set a -> Set a -> Set a
intersectSet (Set [])     _   = Set []
intersectSet (Set (x:xs)) set
  | x `inSet` set = insertSet x (intersectSet (Set xs) set)
  | otherwise     = intersectSet (Set xs) set

-- | Exercise 4.54
differenceSet :: Eq a => Set a -> Set a -> Set a
differenceSet set (Set [])     = set
differenceSet set (Set (x:xs)) = differenceSet (deleteSet x set) (Set xs)
