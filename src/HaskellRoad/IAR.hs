--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.IAR
--
-- Chapter 7. Induction and Recursion
--
--------------------------------------------------------------------------------
module HaskellRoad.IAR where

sumOdds' :: Integer -> Integer
sumOdds' n = sum [2*k - 1 | k <- [1..n]]

sumOdds :: Integer -> Integer
sumOdds n = n^2

sumEvens' :: Integer -> Integer
sumEvens' n = sum [2*k | k <- [1..n]]

sumEvens :: Integer -> Integer
sumEvens n = n * (n+1)

sumInts :: Integer -> Integer
sumInts n = n * (n+1) `div` 2

sumSquares' :: Integer -> Integer
sumSquares' n = sum [k^2 | k <- [1..n]]

sumSquares :: Integer -> Integer
sumSquares n = n * (n+1) * (2*n+1) `div` 6

sumCubes' :: Integer -> Integer
sumCubes' n = sum [k^3 | k <- [1..n]]

sumCubes :: Integer -> Integer
sumCubes n = (n * (n+1) `div` 2) ^ 2

data Natural = Z | S Natural deriving (Eq, Show)

plus :: Natural -> Natural -> Natural
-- m `plus` Z     = m
-- m `plus` (S n) = S (m `plus` n)
plus = foldn S

-- |
-- >>> (S (S Z)) `mult` (S (S (S Z)))
-- S (S (S (S (S (S Z)))))
mult :: Natural -> Natural -> Natural
-- _ `mult` Z     = Z
-- m `mult` (S n) = (m `mult` n) `plus` m
mult m = foldn (plus m) Z

-- |
-- >>> (S (S Z)) `expn` (S (S (S Z)))
-- S (S (S (S (S (S (S (S Z)))))))
expn :: Natural -> Natural -> Natural
-- _ `expn` Z     = S Z
-- m `expn` (S n) = (m `expn` n) `mult` m
expn m = foldn (mult m) (S Z)

leq :: Natural -> Natural -> Bool
Z     `leq` _     = True
(S _) `leq` Z     = False
(S m) `leq` (S n) = m `leq` n

geq :: Natural -> Natural -> Bool
m `geq` n = n `leq` m

gt :: Natural -> Natural -> Bool
m `gt` n = not (m `leq` n)

lt :: Natural -> Natural -> Bool
m `lt` n = not (m `geq` n)

foldn :: (a -> a) -> a -> Natural -> a
foldn _ c Z     = c
foldn h c (S n) = h (foldn h c n)

exclaim :: Natural -> String
exclaim = foldn ('!':) []

data BinTree = L | N BinTree BinTree deriving Show

makeBinTree :: Integer -> BinTree
makeBinTree 0 = L
makeBinTree n = N (makeBinTree (n-1)) (makeBinTree (n-1))

-- |
-- >>> count (makeBinTree 6) == 2^7 - 1
-- True
count :: BinTree -> Integer
count L         = 1
count (N t1 t2) = 1 + count t1 + count t2

depth :: BinTree -> Integer
depth L         = 0
depth (N t1 t2) = max (depth t1) (depth t2) + 1

balanced :: BinTree -> Bool
balanced L         = True
balanced (N t1 t2) = balanced t1 && balanced t2 && depth t1 == depth t2

data Tree = Lf | Nd Int Tree Tree deriving Show

data Tr a = Nil | T a (Tr a) (Tr a) deriving (Eq, Show)

data LeafTree a = Leaf a | Node (LeafTree a) (LeafTree a) deriving Show

ltree :: LeafTree String
ltree = Node (Leaf "I") (Node
                         (Leaf "love")
                         (Leaf "you"))

data Rose a = Bud a | Br [Rose a] deriving (Eq, Show)

rose :: Rose Int
rose = Br [Bud 1, Br [Bud 2, Bud 3, Br [Bud 4, Bud 5, Bud 6]]]

len :: [a] -> Int
len []     = 0
len (_:xs) = 1 + len xs

cat :: [a] -> [a] -> [a]
cat [] ys     = ys
cat (x:xs) ys = x : cat xs ys

{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)
-}

add :: [Natural] -> Natural
add = foldr plus Z

mlt :: [Natural] -> Natural
mlt = foldr mult (S Z)

ln :: [a] -> Natural
ln = foldr (\_ n -> S n) Z

{-
or :: [Bool] -> Bool
or []     = False
or (x:xs) = x || or xs

and :: [Bool] -> Bool
and []     = True
and (x:xs) = x && and xs

and, or :: [Bool] -> Bool
and = foldr (&&) True
or  = foldr (||) False
-}

{-
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ z []     = z
foldl f z (x:xs) = foldl f (f z x) xs
-}

rev :: [a] -> [a]
rev = foldl (\xs x -> x:xs) []

rev' :: [a] -> [a]
rev' = foldr (\x xs -> xs ++ [x]) []

data Form = P Int | Conj Form Form | Disj Form Form | Neg Form

instance Show Form where
  show (P i)        = 'P' : show i
  show (Conj f1 f2) = "(" ++ show f1 ++ " & " ++ show f2 ++ ")"
  show (Disj f1 f2) = "(" ++ show f1 ++ " v " ++ show f2 ++ ")"
  show (Neg f)      = "~" ++ show f

-- |
-- >>> subforms (Neg (Disj (P 1) (Neg (P 2))))
-- [~(P1 v ~P2),(P1 v ~P2),P1,~P2,P2]
subforms :: Form -> [Form]
subforms (P n)        = [P n]
subforms (Conj f1 f2) = Conj f1 f2 : (subforms f1 ++ subforms f2)
subforms (Disj f1 f2) = Disj f1 f2 : (subforms f1 ++ subforms f2)
subforms (Neg f)      = Neg f : subforms f

ccount :: Form -> Int
ccount (P _)        = 0
ccount (Conj f1 f2) = 1 + ccount f1 + ccount f2
ccount (Disj f1 f2) = 1 + ccount f1 + ccount f2
ccount (Neg f)      = 1 + ccount f

acount :: Form -> Int
acount (P _)        = 1
acount (Conj f1 f2) = acount f1 + acount f2
acount (Disj f1 f2) = acount f1 + acount f2
acount (Neg f)      = acount f
