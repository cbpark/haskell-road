module Exercise.Chap4 where

-- | Exercise 4.47
splitList :: [a] -> [([a], [a])]
splitList [x,y]    = [([x], [y])]
splitList (x:y:ys) = ([x], y:ys) : map (\(us, vs) -> (x:us, vs)) (splitList (y:ys))
splitList _        = []
