module Exercise.Chap8 where

import           HaskellRoad.Nats
import           HaskellRoad.WWN

toBase :: Integral a => a -> a -> [Int]
toBase b n | b < 2 || b > 16 = error "base not in [2..16]"
           | n < 0           = error "negative argument"
           | otherwise       = reverse $ toB b n
  where toB b' n' | n' < b'   = [toInt n']
                  | otherwise = toInt (n' `rem` b') : toB b' (n' `quot` b')

-- | Exercise 8.1
hex :: Integral a => a -> String
hex = showDigits . toBase 16

-- | Exercise 8.2
leq1 :: NatPair -> NatPair -> Bool
leq1 (m1, m2) (n1, n2) = m1 + n2 <= m2 + n1

-- | Exercise 8.2
gt1 :: NatPair -> NatPair -> Bool
gt1 p1 p2 = not (p1 `leq1` p2)
