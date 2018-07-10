module Formaldehyde.BirdsNest(
  inBirdsNest
  ,filter'
  ,toHHCOHH
  ) where

--import Data.List (foldl')
import Numeric.LinearAlgebra (Vector, norm_2, fromList)
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
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = []


toHHCOHH :: Vector Double -> Vector Double
toHHCOHH v = fromList [rCOHH, rHH] where
  rCOHH = norm_2 $ (v ##@ [C,O]) - (v ##@ [H1,H2])
  rHH   = v ##$ (H1,H2)
