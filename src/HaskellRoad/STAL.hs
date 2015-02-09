--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.STAL
--
-- Chapter 4. Sets, Types and Lists
--
--------------------------------------------------------------------------------
module HaskellRoad.STAL where

import           HaskellRoad.DB

naturals :: [Integer]
naturals = [0..]

evens1 :: [Integer]
evens1 = [n | n <- naturals, even n]

odds1 :: [Integer]
odds1 = [n | n <- naturals, odd n]

evens2 :: [Integer]
evens2 = [2*n | n <- naturals]

smallSquares1 :: [Integer]
smallSquares1 = [n^(2 :: Integer) | n <- [0..999]]

-- | Unlike 'smallsquares1', this never terminates.
smallSquares2 :: [Integer]
smallSquares2 = [n^(2 :: Integer) | n <- naturals, n < 1000]

run :: Integer -> [Integer]
run n | n < 1  = error "argument not positive"
      | n == 1 = [1]
      | even n = n : run (n `div` 2)
      | odd n  = n : run (3*n + 1)

{-
funny x | halts x x = undefined
        | otherwise = True

halts f x = f /= g
  where g y | y == x   = undefined
            | otherwise = f y
-}

ones :: [Integer]
ones = 1 : ones

{-
data [a] = [] | a : [a] deriving (Eq, Ord)

instance Eq a => Eq [a] where
  []     == []     = True
  (x:xs) == (y:ys) = x == y && xs == ys
  _      == _      = False

instance Ord a => Ord [a] where
  compare []     (_:_)  = LT
  compare []     []     = EQ
  compare (_:_)  []     = GT
  compare (x:xs) (y:ys) = primCompAux x y (compare xs ys)

primCompAux :: Ord a => a -> a -> Ordering -> Ordering
primCompAux x y o = case compare x y of EQ -> o
                                        LT -> LT
                                        GT -> GT

head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

last :: [a] -> a
last [x]    = x
last (_:xs) = last xs

null :: [a] -> Bool
null []    = True
null (_:_) = False
-}

nub :: Eq a => [a] -> [a]
nub []     = []
nub (x:xs) = x : nub (remove x xs)
  where remove _ []                 = []
        remove y (z:zs) | y == z    = remove y zs
                        | otherwise = z : remove y zs

characters :: [String]
characters = nub [x | ["play", _, _, x] <- db]

movies :: [String]
movies = [x | ["release", x, _] <- db]

actors :: [String]
actors = nub [x | ["play", x, _, _] <- db]

directors :: [String]
directors = nub [x | ["direct", x, _] <- db]

dates :: [String]
dates = nub [x | ["release", _, x] <- db]

direct :: [(String, String)]
direct = [(x, y) | ["direct", x, y] <- db]

act :: [(String, String)]
act = [(x, y) | ["play", x, y, _] <- db]

play :: [(String, String, String)]
play = [(x, y, z) | ["play", x, y, z] <- db]

release :: [(String, String)]
release = [(x, y) | ["release", x, y] <- db]

charP :: String -> Bool
charP = (`elem` characters)

actorP :: String -> Bool
actorP = (`elem` actors)

movieP :: String -> Bool
movieP = (`elem` movies)

directorP :: String -> Bool
directorP = (`elem` directors)

dateP :: String -> Bool
dateP = (`elem` dates)

actP :: (String, String) -> Bool
actP = (`elem` act)

releaseP :: (String, String) -> Bool
releaseP = (`elem` release)

directP :: (String, String) -> Bool
directP = (`elem` direct)

playP :: (String, String, String) -> Bool
playP = (`elem` play)

q1 :: [String]
q1 = [x | x <- actors, directorP x]

q2 :: [(String, String)]
q2 = [(x, y) | (x, y) <- act, directorP x]

-- q3 = [(x, y, z) | (x, y) <- direct, (y, z) <- release]
q4 :: [(String, String, String)]
q4 = [(x, y, z) | (x, y) <- direct, (u, z) <- release, y == u]

q5 :: [(String, String)]
q5 = [(x, y) | (x, y) <- direct, (u, "1995") <- release, y == u]

q6 :: [(String, String, String)]
q6 = [(x, y, z) | (x, y) <- direct, (u, z) <- release, y == u, z > "1995"]

q7 :: [String]
q7 = [x | ("Kevin Spacey", x) <- act]

q8 :: [String]
q8 = [x | (x, y) <- release, y > "1997", actP ("William Hurt", x)]

q9 :: Bool
q9 = q1 /= []

q10 :: Bool
q10 = (not . null) [x | ("Woody Allen", x) <- direct]

q10' :: Bool
q10' = directorP "Woody Allen"

delete :: Eq a => a -> [a] -> [a]
delete _ []                 = []
delete x (y:ys) | x == y    = ys
                | otherwise = y : delete x ys

elem' :: Eq a => a -> [a] -> Bool
elem' _ []                 = False
elem' x (y:ys) | x == y    = True
               | otherwise = elem' x ys

union :: Eq a => [a] -> [a] -> [a]
union []     ys = ys
union (x:xs) ys = x : union xs (delete x ys)

intersect :: Eq a => [a] -> [a] -> [a]
intersect []     _              = []
intersect (x:xs) s | x `elem` s = x : xs `intersect` s
                   | otherwise  = xs `intersect` s

addElem :: a -> [[a]] -> [[a]]
addElem x = map (x:)

powerList :: [a] -> [[a]]
powerList []     = [[]]
powerList (x:xs) = powerList xs ++ map (x:) (powerList xs)

data S = Void deriving (Eq, Show)

empty :: [S]
empty = []
