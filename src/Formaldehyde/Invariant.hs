{-
Implements 2014.06.06:149
-}

module Formaldehyde.Invariant (
  toHOCPlane
  ,centerAndRotate
  ,rotationFormula
  ) where

import Numeric.LinearAlgebra
import Formaldehyde.Data (Center(..))
import Formaldehyde.Helpers (cm, (#))

data Direction = Clockwise | AntiClockwise
                             deriving (Show, Eq)

toCOM :: (Vector Double, Vector Double, Vector Double) -> Vector Double -> Vector Double
toCOM (rH, rO, rC) r =
    let  com = ((rH * mH) + (rO * mO) + (rC * mC)) / (mH + mO + mC)
           where mH = 1837.15
                 mO = 29156.95
                 mC = 21874.66
    in r - com

-- gives the matrix for the rotation of a vector about the Z (3rd) axis
rotZ :: Direction -> Double -> Matrix Double
rotZ dir cosTheta = fromLists [[cosTheta, -sinTheta, 0]
                       , [sinTheta, cosTheta, 0 ]
                       , [0, 0, 1]] :: Matrix Double
                                       where  sinTheta = rotSign dir * sqrt (1 - cosTheta^(2 :: Int) )

rotSign :: Fractional a => Direction -> a
rotSign Clockwise = 1.0
rotSign AntiClockwise = -1.0

sign :: (Num a, Ord a) => a -> Direction
sign n
  | n <  0 = AntiClockwise
  | n >= 0 = Clockwise
  | otherwise = error "foo"

-- "rotation formula" from Goldstein (1980), p. 165
rotationFormula :: Vector Double -> Double -> Matrix Double
rotationFormula a cosPhi = let
  i3 = ident 3 :: Matrix Double
  sinPhi = sqrt $ 1 - cosPhi^(2 :: Int)
  in  scale cosPhi i3
      + scale (1 - cosPhi) (outer a a) -- (outer a b) forms the dyad ab
      - scale sinPhi (crossMatrix a)

-- (crossMatrix a) <> b = a x b
crossMatrix :: Vector Double -> Matrix Double
crossMatrix v = fromLists [[0, - v ! z, v ! y]
                          ,[v ! z ,0,- v ! x]
                          ,[-v ! y, v ! x,0]]
  where x = 0
        y = 1
        z = 2

transform1 :: (Vector Double, Vector Double, Vector Double) -> Vector Double -> Vector Double
transform1 = toCOM

transform2 :: (Vector Double, Vector Double, Vector Double) -> Vector Double -> Vector Double
transform2 (rH', rO', rC') r' = let
  z = 3 |> [0,0,1] :: Vector Double
  rHC' = rH' - rC'
  rOC' = rO' - rC'
  n = normdX rHC' rOC'
  a = normdX z n
  cosPhi = n <.> z
  in rotationFormula a cosPhi #> r'

transform3 :: (Vector Double, Vector Double, Vector Double) -> Vector Double -> Vector Double
transform3 (_, rO'', rC'') r'' = let
  y = 3 |> [0,1,0] :: Vector Double
  x = 3 |> [1,0,0] :: Vector Double
  rOC'' = rO'' - rC''
  cosTheta = (rOC'' <.> y) / norm_2 rOC''
  in rotZ (sign $  rOC'' <.> x) cosTheta #> r''

-- returns (a x b) / |a x b|
normdX :: Vector Double -> Vector Double -> Vector Double
normdX a b = unitary $ cross a b

toHOCPlane :: (Vector Double, Vector Double, Vector Double) -> Vector Double -> Vector Double
toHOCPlane c r =
  let
    f' = transform1 c
    g' = transform2 $ map3 f' c
    h' = transform3 $ map3 (g'.f') c
  in h'.g'.f' $ r

configToVector :: (Vector Double, Vector Double, Vector Double, Vector Double) -> Vector Double
configToVector = fromList . concat . list4 . map4 toList

--takes in 1 phase space vector and returns another, alligned in the HOCH plane
centerAndRotate :: Vector Double -> Vector Double
centerAndRotate g = transformCSpace (whichRoamer g) g


whichRoamer :: Vector Double -> Center
whichRoamer p = let
  (rH1, _, _, rH2) = map4 (p#) (H1,O,C,H2)
  com = cm p
  in if norm_2 (rH1 - com) > norm_2 (rH2 - com)
     then H1
     else H2


-- Applies toHOCPlane to the total configuration space vector
transformCSpace :: Center -> Vector Double -> Vector Double
transformCSpace r p = let
  (rH1, rO, rC, rH2) = fromConfiguration p
  f = case r of
    H1 -> toHOCPlane (rH2, rO, rC)
    H2 -> toHOCPlane (rH1, rO, rC)
  xf = map4 f (rH1, rO, rC, rH2)
  in configToVector xf


-- Takes a point in configuration space and returns a tuple of real-space vectors
fromConfiguration :: Vector Double -> (Vector Double, Vector Double, Vector Double, Vector Double)
fromConfiguration v = let
  h1 = subVector 0 3 v
  o  = subVector 3 3 v
  c  = subVector 6 3 v
  h2 = subVector 9 3 v
    in (h1, o, c, h2)


-- Helper Functions

list4 :: (a,a,a,a) -> [a]
list4 (x1, x2, x3, x4) = [x1,x2,x3,x4]
  
map4 :: (a ->b) -> (a,a,a,a) ->  (b,b,b,b)
map4 f (x1, x2, x3, x4) = (f x1, f x2, f x3, f x4)

map3 :: (a ->b) -> (a,a,a)  ->  (b,b,b)
map3 f (x1, x2, x3) = (f x1, f x2, f x3)
