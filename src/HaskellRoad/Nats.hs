--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.Nats
--
--------------------------------------------------------------------------------
module HaskellRoad.Nats where

import           Data.Char (intToDigit)

data Natural = Z | S Natural deriving (Eq, Show)

instance Ord Natural where
  Z     `compare` Z     = EQ
  Z     `compare` _     = LT
  _     `compare` Z     = GT
  (S m) `compare` (S n) = m `compare` n

instance Enum Natural where
  succ = S
  pred Z     = Z
  pred (S n) = n
  toEnum 0 = Z
  toEnum n = (S . toEnum) (n-1)
  fromEnum Z     = 0
  fromEnum (S n) = 1 + fromEnum n
  enumFrom n = map toEnum [(fromEnum n) ..]

instance Num Natural where
  (+) = foldn succ
  (*) m = foldn (+m) Z
  (-) = foldn pred
  abs = id
  signum Z = Z
  signum _ = S Z
  fromInteger n | n <  0    = error "no negative naturals"
                | n == 0    = Z
                | otherwise = (S . fromInteger) (n-1)

foldn :: (a -> a) -> a -> Natural -> a
foldn _ c Z     = c
foldn h c (S n) = h (foldn h c n)

instance Real Natural where
  toRational = toRational . toInteger

instance Integral Natural where
  n `quotRem` d | d > n = (Z, n)
                | otherwise = (S q, r)
    where (q, r) = (n-d) `quotRem` d
  toInteger = foldn succ 0

toInt :: Integral a => a -> Int
toInt = fromIntegral

binary :: Integral a => a -> [Int]
binary = reverse . bits
  where bits 0 = [0]
        bits 1 = [1]
        bits n = toInt (n `rem` 2) : bits (n `quot` 2)

showDigits :: [Int] -> String
showDigits = map intToDigit

bin :: Integral a => a -> String
bin = showDigits . binary
