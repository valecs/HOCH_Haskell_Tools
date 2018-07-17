module Formaldehyde.BirdsNest(
  inBirdsNest
  ,filter'
  ,toHHCOHH
  ,toHHCOHHa
  ,toHHCOHHb
  ,dihedral
  ) where

--import Data.List (foldl')
import Numeric.LinearAlgebra (Vector, norm_2, fromList, cross, unitary, dot)
import Formaldehyde.Helpers
import Formaldehyde.Data


inBirdsNest :: Vector Double -> Bool
inBirdsNest v = norm_2 ( v ##@ [C,O] - v ##@ [H1,H2]) < cohhMax &&
                v ##$ (H1,H2) < hhMax where
                  hhMax   = 5.86728
                  cohhMax = 3.420


-- | Stops filtering after the first time p x == False
-- TODO: re-write with foldl'
-- >>> filter' (<5) [1,3,5,7,5,3,1]
-- [1,3]
-- use like, e.g.: filter' inBirdsNest
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = []


-- some useful coordinate transformations

-- | to the two jacobi coordinates rH2-CO, rH-H
toHHCOHH :: Vector Double -> Vector Double
toHHCOHH v = fromList [rCOHH, rHH] where
  rCOHH = norm_2 $ (v ##@ [C,O]) - (v ##@ [H1,H2])
  rHH   = v ##$ (H1,H2)


-- | to the two jacobi coordinates rH2-CO, rH-H, with the dihedral
toHHCOHHa :: Vector Double -> [Double]
toHHCOHHa v = [rCOHH, rHH, a ] where
  rCOHH = norm_2 $ (v ##@ [C,O]) - (v ##@ [H1,H2])
  rHH   = v ##$ (H1,H2)
  a = dihedral v

-- | to the two jacobi coordinates rH2-CO, rH-H, with angle between the H2 and CO
toHHCOHHb :: Vector Double -> [Double]
toHHCOHHb v = [rCOHH, rHH, b ] where
  rCOHH = norm_2 $ (v ##@ [C,O]) - (v ##@ [H1,H2])
  rHH   = v ##$ (H1,H2)
  -- abs reflects the indistinguishability of the the two H atoms
  b = abs $ (unitary v ## (H1, H2)) `dot` (unitary v ## (C, O))


(∧) :: Vector Double -> Vector Double -> Vector Double 
(∧) = cross

-- | computes the cosine of the dihedral angle formed by the out of plane H
dihedral :: Vector Double -> Double
dihedral v = cosa where
  cosa = (unitary (a ∧ b)) `dot` (unitary (a ∧ c)) where
    a = v ## (O, C)
    b = v ## (C, H1)
    c = v ## (C, H2)
    
    
    
