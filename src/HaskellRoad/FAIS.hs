--------------------------------------------------------------------------------
-- |
-- Module : HaskellRoad.FAIS
--
-- Chapter 11. Finite and Infinite Sets
--
--------------------------------------------------------------------------------
module HaskellRoad.FAIS where

succs :: [(Integer, Integer)]
succs = [(n, succ n) | n <- [0..]]
