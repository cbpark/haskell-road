module Exercise.Chap7 where

import           HaskellRoad.IAR

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
quotient _ Z = error "division by zero"
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
