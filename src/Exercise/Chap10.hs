module Exercise.Chap10 where

-- | Exercise 10.1
evens :: [Integer]
evens = 0 : map (+2) evens

-- | Exercise 10.2
theEvens :: [Integer]
theEvens = iterate (+2) 0

-- | Exercise 10.3
swap01 :: String -> String
swap01 "" = ""
swap01 ('1':xs) = '0' : swap01 xs
swap01 ('0':xs) = '1' : swap01 xs

thue :: String
thue = '0' : morse "1"
  where morse xs = xs ++ morse (xs ++ swap01 xs)
