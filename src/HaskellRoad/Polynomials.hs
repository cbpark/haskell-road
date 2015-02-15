module HaskellRoad.Polynomials where

default (Integer, Rational, Double)

infixl 7 .*
(.*) :: Num a => a -> [a] -> [a]
_ .* []     = []
c .* (f:fs) = c*f : c .* fs

z :: Num a => [a]
z = [0,1]

instance (Num a, Ord a) => Num [a] where
  fromInteger c = [fromInteger c]
  negate []     = []
  negate (f:fs) = negate f : negate fs
  signum [] = []
  signum gs | signum (last gs) < 0 = negate z
            | otherwise            = z
  abs [] = []
  abs gs | signum gs == z = gs
         | otherwise      = negate gs
  fs     + []     = fs
  []     + gs     = gs
  (f:fs) + (g:gs) = f + g : fs + gs
  _      * []     = []
  []     * _      = []
  (f:fs) * (g:gs) = f * g : (f .* gs + fs * (g:gs))

delta :: (Num a, Ord a) => [a] -> [a]
delta = ([1,-1] *)

shift :: [a] -> [a]
shift = tail

-- |
-- >>> [n^3 + 5 * n^2 - 5 * n - 12 | n <- [0..9]]
-- [-12,-11,6,45,112,213,354,541,780,1077]
-- >>> map (p2fct [-12,-5,5,1]) [0..9]
-- [-12,-11,6,45,112,213,354,541,780,1077]
p2fct :: Num a => [a] -> a -> a
p2fct []     _ = 0
p2fct (a:as) x = a + (x * p2fct as x)

-- | composition of two polynomials
--
-- >>> comp (z^2) (z+1)
-- [1,2,1]
-- >>> comp (z^3) (z+1)
-- [1,3,3,1]
-- >>> comp (z^4) (z+1)
-- [1,4,6,4,1]
comp :: (Eq a, Num a, Ord a) => [a] -> [a] -> [a]
comp _      []     = error ".."
comp []     _      = []
comp (f:fs) (0:gs) = f : gs * comp fs (0:gs)
comp (f:fs) (g:gs) = ([f] + [g] * comp fs (g:gs)) + (0 : gs * comp fs (g:gs))

deriv :: (Num a) => [a] -> [a]
deriv []     = []
deriv (_:fs) = deriv1 fs 1
  where deriv1 [] _     = []
        deriv1 (g:gs) n = n*g : deriv1 gs (n+1)
