module Exercise.Chap10 where

import           HaskellRoad.COR

-- | Exercise 10.1
evens :: [Integer]
evens = 0 : map (+2) evens

-- | Exercise 10.2
theEvens :: [Integer]
theEvens = iterate (+2) 0

swap01 :: String -> String
swap01 "" = ""
swap01 ('1':xs) = '0' : swap01 xs
swap01 ('0':xs) = '1' : swap01 xs

-- | Exercise 10.3
thue :: String
thue = '0' : morse "1"
  where morse xs = xs ++ morse (xs ++ swap01 xs)

-- | Exercise 10.10
vend, vend1, vend2, vend3, vend4 :: Process
vend  (0:xs) = "coin"      : vend1 xs
vend  (1:xs) = "coin"      : vend4 xs
vend1 (0:xs) = "coin"      : vend2 xs
vend1 (1:xs) =               vend1 xs
vend2 (0:xs) = "beer"      : vend  xs
vend2 (1:xs) = "coin"      : vend3 xs
vend3 (0:xs) = "moneyback" : vend  xs
vend3 (1:xs) =               vend3 xs
vend4 (0:xs) = "water"     : vend  xs
vend4 (1:xs) =               vend4 xs
