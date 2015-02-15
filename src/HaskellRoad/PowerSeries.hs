module HaskellRoad.PowerSeries where

import           HaskellRoad.Polynomials ((.*))

default (Integer, Rational, Double)

instance (Ord a, Fractional a) => Fractional [a] where
  fromRational c = [fromRational c]
  _      / []     = error "division by 0 attempted"
  []     / _      = []
  (0:fs) / (0:gs) = fs / gs
  _      / (0:_)  = error "division by 0 attempted"
  (f:fs) / (g:gs) = let q = f / g in q : (fs - q.*gs) / (g:gs)

int :: Fractional a => [a] -> [a]
int fs = 0 : intl fs 1
  where intl []     _ = []
        intl (g:gs) n = g/n : intl gs (n+1)

expz :: (Num a, Fractional a, Ord a) => [a]
expz = 1 + int expz
