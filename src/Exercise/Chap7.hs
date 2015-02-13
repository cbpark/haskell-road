module Exercise.Chap7 where

import           HaskellRoad.IAR
import           HaskellRoad.STAL (intersect, nub, union)

import           Data.List        (sort)

-- | Exercise 7.14
--
-- >>> (S (S (S Z))) `subtr'` (S (S (S (S Z))))
-- Z
-- >>> (S (S (S (S Z)))) `subtr'` (S (S (S (S Z))))
-- Z
-- >>> (S (S (S (S (S Z))))) `subtr'` (S (S (S (S Z))))
-- S Z
subtr' :: Natural -> Natural -> Natural
Z     `subtr'` _     = Z
m     `subtr'` Z     = m
(S m) `subtr'` (S n) = m `subtr'` n

-- | Exercise 7.15
quotient :: Natural -> Natural -> Natural
quotient _ Z             = error "division by zero"
quotient m n | m `lt` n  = Z
             | otherwise = S Z `plus` quotient (m `subtr'` n) n

-- | Exercise 7.15
remainder :: Natural -> Natural -> Natural
remainder m n = m `subtr'` (quotient m n `mult` n)

-- | Exercise 7.16
subtr :: Natural -> Natural -> Natural
subtr = foldn pre
  where pre Z     = Z
        pre (S n) = n

-- | Exercise 7.18
bittest :: [Int] -> Bool
bittest []       = True
bittest [0]      = True
bittest (1:xs)   = bittest xs
bittest (0:1:xs) = bittest xs
bittest _        = False

-- | Exercise 7.19
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib' :: Integer -> Integer
fib' = fib2 0 1
  where fib2 a _ 0 = a
        fib2 a b n = fib2 b (a+b) (n-1)

-- | Exercise 7.20
catalan :: Integer -> Integer
catalan 0 = 1
catalan n = sum [catalan i * catalan (n-1-i) | i <- [0..(n-1)]]

-- | Exercise 7.25
data TerTree = TL | TN TerTree TerTree TerTree deriving Show

-- | Exercise 7.25
makeTerTree :: Integer -> TerTree
makeTerTree 0 = TL
makeTerTree n = TN (makeTerTree (n-1)) (makeTerTree (n-1)) (makeTerTree (n-1))

-- | Exercise 7.25
countTerTree :: TerTree -> Integer
countTerTree TL            = 1
countTerTree (TN t1 t2 t3) = 1 + countTerTree t1 + countTerTree t2 + countTerTree t3

-- | Exercise 7.28
insertTree :: Int -> Tree -> Tree
insertTree n Lf                       = Nd n Lf Lf
insertTree n t@(Nd m l r) | n < m     = Nd m (insertTree n l) r
                          | n > m     = Nd m l (insertTree n r)
                          | otherwise = t

-- | Exercise 7.29
list2tree :: [Int] -> Tree
list2tree = foldr insertTree Lf

-- | Exercise 7.29
tree2list :: Tree -> [Int]
tree2list Lf         = []
tree2list (Nd m l r) = tree2list l ++ [m] ++ tree2list r

-- | Exercise 7.30
inTree :: Int -> Tree -> Bool
inTree _ Lf                     = False
inTree n (Nd m l r) | n < m     = inTree n l
                    | n > m     = inTree n r
                    | otherwise = True

-- | Exercise 7.31
mergeTrees :: Tree -> Tree -> Tree
mergeTrees t1 t2 = foldr insertTree t2 (tree2list t1)

-- | Exercise 7.32
countStep :: Int -> Tree -> Int
countStep _ Lf                     = -1
countStep n (Nd m l r) | n < m     = if s1 == -1 then -1 else s1 + 1
                       | n > m     = if s2 == -1 then -1 else s2 + 1
                       | otherwise = 0
  where s1 = countStep n l
        s2 = countStep n r

-- | Exercise 7.33
mapT :: (a -> b) -> Tr a -> Tr b
mapT _ Nil       = Nil
mapT f (T x l r) = T (f x) (mapT f l) (mapT f r)

-- | Exercise 7.34
foldT :: (a -> b -> b -> b) -> b -> Tr a -> b
foldT _ x Nil       = x
foldT f x (T y l r) = f y (foldT f x l) (foldT f x r)

-- | Exercise 7.35
preorderTraversal :: Tr a -> [a]
preorderTraversal = foldT preLists []
  where preLists x ys zs = (x:ys) ++ zs

-- | Exercise 7.35
inorderTraversal :: Tr a -> [a]
inorderTraversal = foldT inLists []
  where inLists x ys zs = ys ++ [x] ++ zs

-- | Exercise 7.35
postorderTraversal :: Tr a -> [a]
postorderTraversal = foldT postLists []
  where postLists x ys zs = ys ++ zs ++ [x]

-- | Exercise 7.36
orderedTree :: Ord a => Tr a -> Bool
orderedTree = ordered . inorderTraversal
  where ordered xs = (sort . nub) xs == xs

type Dict = Tr (String, String)

-- | Exercise 7.37
lookupD :: String -> Dict -> [String]
lookupD _   Nil            = []
lookupD str (T (v, w) l r) = case str `compare` v of LT -> lookupD str l
                                                     GT -> lookupD str r
                                                     _  -> [w]

split :: [a] -> ([a], a, [a])
split xs = (ys1, y, ys2)
  where ys1     = take n xs
        (y:ys2) = drop n xs
        n       = length xs `div` 2

-- | Exercise 7.38
buildTree :: [a] -> Tr a
buildTree [] = Nil
buildTree xs = T m (buildTree l) (buildTree r)
  where (l, m, r) = split xs

-- | Exercise 7.39
mapLT :: (a -> b) -> LeafTree a -> LeafTree b
mapLT f (Leaf x)   = Leaf (f x)
mapLT f (Node l r) = Node (mapLT f l) (mapLT f r)

-- | Exercise 7.40
reflect :: LeafTree a -> LeafTree a
reflect (Leaf x)   = Leaf x
reflect (Node l r) = Node (reflect r) (reflect l)

-- | Exercise 7.42
--
-- >>> rose
-- Br [Bud 1,Br [Bud 2,Bud 3,Br [Bud 4,Bud 5,Bud 6]]]
-- >>> mapR succ rose
-- Br [Bud 2,Br [Bud 3,Bud 4,Br [Bud 5,Bud 6,Bud 7]]]
mapR :: (a -> b) -> Rose a -> Rose b
mapR f (Bud x) = Bud (f x)
mapR f (Br xs) = Br (map (mapR f) xs)

-- | Exercise 7.46
genUnion :: Eq a => [[a]] -> [a]
genUnion = foldr union []

-- | Exercise 7.46
genIntersect :: Eq a => [[a]] -> [a]
genIntersect = foldr1 intersect

-- | Exercise 7.47
insrt :: Ord a => a -> [a] -> [a]
insrt x []     = [x]
insrt x (y:ys) = if x <= y then x:y:ys else y : insrt x ys

-- | Exercise 7.47
srt :: Ord a => [a] -> [a]
srt = foldr insrt []

-- | Exercise 7.50
rev1 :: [a] -> [a]
rev1 xs = rev2 xs []
  where rev2 []       ys = ys
        rev2 (x':xs') ys = rev2 xs' (x':ys)

-- | Exercise 7.51
ln' :: [a] -> Natural
ln' = foldl (\n _ -> S n) Z
