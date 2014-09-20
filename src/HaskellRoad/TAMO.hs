--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.TAMO
--
-- Chapter 2. Talking About Mathematical Objects
--
--------------------------------------------------------------------------------
module HaskellRoad.TAMO where

infix 1 ==>

-- | implication.
(==>) :: Bool -> Bool -> Bool
-- x ==> y = not x || y
True  ==> x = x
False ==> _ = True

infix 1 <=>

-- | equivalence.
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

infixr 2 <+>

-- | exclusive /or/.
(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y

formula1 :: Bool
formula1 = let p = True
               q = False
           in not p && (p ==> q) <=> not (q && not p)

-- |
-- >>> valid2 formula2
-- False
formula2 :: Bool -> Bool -> Bool
formula2 p q = not p && (p ==> q) <=> not (q && not p)

valid1 :: (Bool -> Bool) -> Bool
valid1 bf = bf True && bf False

-- |
-- >>> valid1 excludedMiddle
-- True
excludedMiddle :: Bool -> Bool
excludedMiddle p = p || not p

valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 bf = bf True True && bf True False
            && bf False True && bf False False

-- |
-- >>> valid2 form1
-- True
form1 :: Bool -> Bool -> Bool
form1 p q = p ==> (q ==> p)

-- |
-- >>> valid2 form2
-- False
form2 :: Bool -> Bool -> Bool
form2 p q = (p ==> q) ==> p

valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid3 bf = and [bf p q r | p <- [True,False]
                          , q <- [True,False]
                          , r <- [True,False]]

valid4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
valid4 bf = and [bf p q r s | p <- [True,False]
                            , q <- [True,False]
                            , r <- [True,False]
                            , s <- [True,False]]

logEquiv1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bf1 bf2 = (bf1 True <=> bf2 True) && (bf1 False <=> bf2 False)

logEquiv2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
logEquiv2 bf1 bf2 = and [bf1 p q <=> bf2 p q | p <- [True,False]
                                             , q <- [True,False]]

logEquiv3 :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool)
             -> Bool
logEquiv3 bf1 bf2 = and [bf1 p q r <=> bf2 p q r | p <- [True,False]
                                                 , q <- [True,False]
                                                 , r <- [True,False]]

formula3 :: Bool -> Bool -> Bool
formula3 p _ = p

-- |
-- >>> logEquiv2 formula3 formula4
-- True
formula4 :: Bool -> Bool -> Bool
formula4 p q = (p <+> q) <+> q

-- |
-- >>> valid2 formula5
-- True
formula5 :: Bool -> Bool -> Bool
formula5 p q = p <=> ((p <+> q) <+> q)

-- | law of double negation.
test1 :: Bool
test1 = logEquiv1 id (\p -> (not . not) p)

-- | laws of idempotence.
test2a :: Bool
test2a = logEquiv1 id (\p -> p && p)

test2b :: Bool
test2b = logEquiv1 id (\p -> p || p)

test3a :: Bool
test3a = logEquiv2 (\p q -> p ==> q) (\p q -> not p || q)

test3b :: Bool
test3b = logEquiv2 (\p q -> not (p ==> q)) (\p q -> p && not q)

-- | laws of contraposition.
test4a :: Bool
test4a = logEquiv2 (\p q -> not p ==> not q) (\p q -> q ==> p)

test4b :: Bool
test4b = logEquiv2 (\p q -> p ==> not q) (\p q -> q ==> not p)

test4c :: Bool
test4c = logEquiv2 (\p q -> not p ==> q) (\p q -> not q ==> p)

test5a :: Bool
test5a = logEquiv2 (\p q -> p <=> q) (\p q -> (p ==> q) && (q ==> p))

test5b :: Bool
test5b = logEquiv2 (\p q -> p <=> q) (\p q -> (p && q) || (not p && not q))

-- | laws of commutativity.
test6a :: Bool
test6a = logEquiv2 (\p q -> p && q) (\p q -> q && p)

test6b :: Bool
test6b = logEquiv2 (\p q -> p || q) (\p q -> q || p)

-- | De Morgan laws.
test7a :: Bool
test7a = logEquiv2 (\p q -> not (p && q)) (\p q -> not p || not q)

test7b :: Bool
test7b = logEquiv2 (\p q -> not (p || q)) (\p q -> not p && not q)

-- | laws of associativity.
test8a :: Bool
test8a = logEquiv3 (\p q r -> p && (q && r)) (\p q r -> (p && q) && r)

test8b :: Bool
test8b = logEquiv3 (\p q r -> p || (q || r)) (\p q r -> (p || q) || r)

-- | distribution laws.
test9a :: Bool
test9a = logEquiv3 (\p q r -> p && (q || r)) (\p q r -> (p && q) || (p && r))

test9b :: Bool
test9b = logEquiv3 (\p q r -> p || (q && r)) (\p q r -> (p || q) && (p || r))
