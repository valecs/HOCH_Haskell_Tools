module PhaseSpaceSums
    ( integrCount
      ,θ
    ) where

import Data.List (foldl')

--Evaluates func at all elements and returns the total sum and count
integrCount :: (Num b) => (a -> Double) -> [a] -> (Double, b)
integrCount func = foldl' apply (0 , 0) where
  apply (f, n) e = (f + func e, n + 1)

-- Heaviside Function
θ ::(Ord a, Num a, Num t) => a -> t
θ x
  | x < 0 = 0
  | otherwise = 1
