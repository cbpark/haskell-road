module Exercise.Chap2 where

import           HaskellRoad.TAMO

-- | Exercise 2.9
--
-- >>> valid2 ex29
-- True
ex29 :: Bool -> Bool -> Bool
ex29 p q = (p <+> q) <+> q <=> p

-- | Exercise 2.13
theorem2121a :: Bool
theorem2121a = not True <=> False

theorem2121b :: Bool
theorem2121b = not False <=> True

theorem2122 :: Bool
theorem2122 = logEquiv1 (\p -> p ==> False) (\p -> not p)

-- | dominance laws.
theorem2123a :: Bool
theorem2123a = logEquiv1 (\p -> p || True) (const True)

theorem2123b :: Bool
theorem2123b = logEquiv1 (\p -> p && False) (const False)

-- | identity laws.
theorem2124a :: Bool
theorem2124a = logEquiv1 (\p -> p || False) id

theorem2124b :: Bool
theorem2124b = logEquiv1 (\p -> p && True) id

-- | laws of excluded middle.
theorem2125 :: Bool
theorem2125 = logEquiv1 excludedMiddle (const True)

-- | contradiction.
theorem2126 :: Bool
theorem2126 = logEquiv1 (\p -> p && not p) (const False)

-- | Exercise 2.15
contrad1 :: (Bool -> Bool) -> Bool
contrad1 bf = not (bf True) && not (bf False)

contrad2 :: (Bool -> Bool -> Bool) -> Bool
contrad2 bf = and [not (bf p q) | p <- [True,False], q <- [True,False]]

contrad3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
contrad3 bf = and [not (bf p q r) | p <- [True,False]
                                  , q <- [True,False]
                                  , r <- [True,False]]

-- | Exercise 2.51
unique :: (a -> Bool) -> [a] -> Bool
unique _ []     = False
unique p (x:xs) = if p x then not (any p xs) else unique p xs

-- | Exercise 2.52
parity :: [Bool] -> Bool
parity = foldr (/=) True

-- | Exercise 2.53
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p = parity . map p
