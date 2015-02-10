--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.Hierarchy
--
--------------------------------------------------------------------------------
module HaskellRoad.Hierarchy where

import HaskellRoad.SetEq (Set (..), powerSet)

data S = Void deriving (Eq, Show)

empty, v0 :: Set S
empty = Set []
v0    = empty

v1 :: Set (Set S)
v1 = powerSet v0

v2 :: Set (Set (Set S))
v2 = powerSet v1

v3 :: Set (Set (Set (Set S)))
v3 = powerSet v2

v4 :: Set (Set (Set (Set (Set S))))
v4 = powerSet v3

v5 :: Set (Set (Set (Set (Set (Set S)))))
v5 = powerSet v4

display :: Int -> String -> IO ()
display n str = putStrLn (display' n 0 str)
  where display' _  _ []                 = []
        display' n' m (x:xs) | n' == m   = '\n' : display' n' 0     (x:xs)
                             | otherwise =    x : display' n' (m+1) xs
