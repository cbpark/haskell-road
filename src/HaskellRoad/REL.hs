--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.REL
--
-- Chapter 5. Relations
--
--------------------------------------------------------------------------------
module HaskellRoad.REL where

import           Prelude            hiding (curry, flip, uncurry)

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
symR (Set [])                         = True
symR (Set ((x, y):pairs)) | x == y    = symR (Set pairs)
                          | otherwise = (y, x) `inSet` Set pairs
                                        && symR (deleteSet (y, x) (Set pairs))

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

divides :: Integer -> Integer -> Bool
d `divides` n | d == 0    = error "divides: zero divisor"
              | otherwise = n `rem` d == 0

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f p = f (fst p) (snd p)

eq :: Eq a => (a, a) -> Bool
eq = uncurry (==)

lessEq :: Ord a => (a, a) -> Bool
lessEq = uncurry (<=)

inverse :: ((a, b) -> c) -> (b, a) -> c
inverse f (x, y) = f (y, x)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

type Rel' a = a -> a -> Bool

emptyR' :: Rel' a
emptyR' _ _ = False

list2rel' :: Eq a => [(a, a)] -> Rel' a
list2rel' xys x y = (x, y) `elem` xys

idR' :: Eq a => [a] -> Rel' a
idR' xs x y = x == y && x `elem` xs

invR' :: Rel' a -> Rel' a
invR' = flip

inR' :: Rel' a -> (a, a) -> Bool
inR' = uncurry

reflR' :: [a] -> Rel' a -> Bool
reflR' xs r = and [r x x | x <- xs]

irreflR' :: [a] -> Rel' a -> Bool
irreflR' xs r = and [not (r x x) | x <- xs]

symR' :: [a] -> Rel' a -> Bool
symR' xs r = and [not (r x y && not (r y x)) | x <- xs, y <- xs]

transR' :: [a] -> Rel' a -> Bool
transR' xs r = and [not (r x y && r y z && not (r x z)) | x <- xs, y <- xs, z <- xs]

unionR' :: Rel' a -> Rel' a -> Rel' a
unionR' r s x y = r x y || s x y

intersR' :: Rel' a -> Rel' a -> Rel' a
intersR' r s x y = r x y && s x y

reflClosure' :: Eq a => Rel' a -> Rel' a
reflClosure' r = unionR' r (==)

symClosure' :: Rel' a -> Rel' a
symClosure' r = unionR' r (invR' r)

compR' :: [a] -> Rel' a -> Rel' a -> Rel' a
compR' xs r s x y = or [r x z && s z y | z <- xs]

repeatR' :: [a] -> Rel' a -> Int -> Rel' a
repeatR' xs r n | n <  1    = error "argument < 1"
                | n == 1    = r
                | otherwise = compR' xs r (repeatR' xs r (n - 1))

equivalenceR :: Ord a => Set a -> Rel a -> Bool
equivalenceR set r = reflR set r && symR r && transR r

equivalenceR' :: [a] -> Rel' a -> Bool
equivalenceR' xs r = reflR' xs r && symR' xs r && transR' xs r

-- | modulo relation.
modulo :: Integer -> Integer -> Integer -> Bool
modulo n x y = n `divides` (x - y)

equalSize :: [a] -> [b] -> Bool
equalSize list1 list2 = length list1 == length list2

type Part = [Int]
type CmprPart = (Int, Part)

expand :: CmprPart -> Part
expand (0, p) = p
expand (n, p) = 1 : expand (n - 1, p)

nextpartition :: CmprPart -> CmprPart
nextpartition (k, [])   = (k, [])
nextpartition (k, x:xs) = pack (x - 1) (k + x, xs)

pack :: Int -> CmprPart -> CmprPart
pack 1 (m, xs) = (m, xs)
pack k (m, xs) = if k > m then pack (k - 1) (m, xs) else pack k (m - k, k:xs)

generatePs :: CmprPart -> [Part]
generatePs p@(_, []) = [expand p]
generatePs p         = expand p : generatePs (nextpartition p)

part :: Int -> [Part]
part n | n <  1    = error "part: argument <= 0"
       | n == 1    = [[1]]
       | otherwise = generatePs (0, [n])
