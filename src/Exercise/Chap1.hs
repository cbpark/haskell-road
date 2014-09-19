module Exercise.Chap1 where

import           Data.List (isPrefixOf)

-- | Exercise 1.9
mxmInt :: [Int] -> Int
mxmInt []     = error "empty list"
mxmInt [x]    = x
mxmInt (x:xs) = max x (mxmInt xs)

-- | Exercise 1.10
removeFst :: Eq a => a -> [a] -> [a]
removeFst _ []                 = []
removeFst m (n:ns) | m == n    = ns
                   | otherwise = n : removeFst m ns

-- | Exercise 1.13
count :: Char -> String -> Int
count _ ""                 = 0
count a (c:cs) | a == c    = 1 + count a cs
               | otherwise = count a cs

-- | Exercise 1.14
blowup :: String -> String
blowup str = blowup' str 1
  where blowup' :: String -> Int -> String
        blowup' [] _     = []
        blowup' (x:xs) n = replicate n x ++ blowup' xs (n + 1)

mnmString :: [String] -> String
mnmString []     = error "empty string"
mnmString [x]    = x
mnmString (x:xs) = min x (mnmString xs)

-- | Exercise 1.15
srtString :: [String] -> [String]
srtString [] = []
srtString xs = m : srtString (removeFst m xs) where m = mnmString xs

-- | Exercise 1.17
substring :: String -> String -> Bool
substring [] _          = True
substring _ []          = False
substring xs ys@(_:ys') = xs `isPrefixOf` ys || substring xs ys'

-- | Exercise 1.20
lengths :: [[a]] -> [Int]
lengths = map length

-- | Exercise 1.21
sumLengths :: [[a]] -> Int
sumLengths = sum . map length
