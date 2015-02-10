--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.REL
--
-- Chapter 5. Relations
--
--------------------------------------------------------------------------------
module HaskellRoad.REL where

import           HaskellRoad.SetOrd

divisors :: Integer -> [(Integer, Integer)]
divisors n = [(d, n `quot` d) | d <- [1..k], n `rem` d == 0]
  where k = (floor . sqrt . fromInteger) n

prime'' :: Integer -> Bool
prime'' n = divisors n == [(1, n)]

divs :: Integer -> [Integer]
divs n = fst list ++ (reverse . snd) list
  where list = (unzip . divisors) n

properDivs :: Integer -> [Integer]
properDivs = init . divs

perfect :: Integer -> Bool
perfect n = (sum . properDivs) n == n

type Rel a = Set (a, a)

-- | domain of a relation.
domR :: Ord a => Rel a -> Set a
domR (Set r) = list2set [x | (x, _) <- r]

-- | range of a relation.
ranR :: Ord a => Rel a -> Set a
ranR (Set r) = list2set [y | (_, y) <- r]

-- | identity relation over a set.
idR :: Ord a => Set a -> Rel a
idR (Set xs) = Set [(x, x) | x <- xs]

-- | total relation over a set.
totalR :: Set a -> Rel a
totalR (Set xs) = Set [(x, y) | x <- xs, y <- xs]

-- | inverts a relation.
invR :: Ord a => Rel a -> Rel a
invR (Set [])           = Set []
invR (Set ((x, y) : r)) = insertSet (y, x) (invR (Set r))

-- | checks whether a pair is in a relation.
inR :: Ord a => Rel a -> (a, a) -> Bool
inR r (x, y) = (x, y) `inSet` r

-- | complement of a relation
complR :: Ord a => Set a -> Rel a -> Rel a
complR (Set xs) r = Set [(x, y) | x <- xs, y <- xs, not (inR r (x, y))]

-- | checks reflexivity.
reflR :: Ord a => Set a -> Rel a -> Bool
reflR set r = idR set `subSet` r

-- | checks irreflexivity.
irreflR :: Ord a => Set a -> Rel a -> Bool
irreflR (Set xs) r = all (not . inR r) [(x, x) | x <- xs]

-- | checks symmetry
symR :: Ord a => Rel a -> Bool
symR (Set []) = True
symR (Set ((x, y):pairs))
  | x == y = symR (Set pairs)
  | otherwise = (y, x) `inSet` Set pairs && symR (deleteSet (y, x) (Set pairs))

-- | checks transitivity.
transR :: Ord a => Rel a -> Bool
transR (Set []) = True
transR (Set s)  = and [trans pair (Set s) | pair <- s]
  where trans (x, y) (Set r) = and [(x, v) `inSet` Set r | (u, v) <- r, u == y]

composePair :: Ord a => (a, a) -> Rel a -> Rel a
composePair _      (Set []) = Set []
composePair (x, y) (Set ((u, v):s))
  | y == u    = insertSet (x, v) (composePair (x, y) (Set s))
  | otherwise = composePair (x, y) (Set s)

-- | relation composition.
compR :: Ord a => Rel a -> Rel a -> Rel a
compR (Set [])         _ = Set []
compR (Set ((x, y):s)) r = unionSet (composePair (x, y) r) (compR (Set s) r)

-- | composition of a relation with itself.
repeatR :: Ord a => Rel a -> Int -> Rel a
repeatR r n | n <  1    = error "argument < 1"
            | n == 1    = r
            | otherwise = compR r (repeatR r (n-1))
