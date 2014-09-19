--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.TAMO
--
-- Chapter 2. Talking About Mathematical Objects
--
--------------------------------------------------------------------------------
module HaskellRoad.TAMO where

infix 1 ==>

(==>) :: Bool -> Bool -> Bool
-- x ==> y = not x || y
True  ==> x = x
False ==> _ = True
