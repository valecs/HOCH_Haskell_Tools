{-
Implements 2014.06.06:149
-}

module Formaldehyde.Invariant (
  toHOCPlane
  ,centerAndRotate
  ) where

import Numeric.LinearAlgebra

data Direction = Clockwise | AntiClockwise
                             deriving (Show, Eq)

(@>) :: Vector Double -> Int -> Double
(@>) v n = subVector n 1 v

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
                                       where  sinTheta = (rotSign dir) * (sqrt $ 1 - cosTheta^(2 :: Int) )

rotSign :: Fractional a => Direction -> a
rotSign Clockwise = 1.0
rotSign AntiClockwise = -1.0

sign :: (Num a, Ord a) => a -> Direction
sign n
  | n <  0 = AntiClockwise
  | n >= 0 = Clockwise
  | otherwise = error "foo"

-- "rotation formula" from Goldstein (1980), p. 165
-- rotationFormula :: Vector Double -> Double -> Vector Double -> Vector Double
-- rotationFormula a cosPhi r = rotCP <> r
--                           where rotCP = (cosPhi * (ident 3))
--                                         + ((1 - cosPhi) * (outer a a)) -- (outer a b) forms the dyad ab
--                                         - (sinPhi * (crossMatrix a))
--                                   where sinPhi = sqrt $ 1 - cosPhi^2

-- "rotation formula" from Goldstein (1980), p. 165
rotationFormula :: Vector Double -> Double -> Matrix Double
rotationFormula a cosPhi = let
  i3 = ident 3 :: Matrix Double
  sinPhi = sqrt $ 1 - cosPhi^(2 :: Int)
  in  (scale cosPhi i3)
      + (scale (1 - cosPhi) (outer a a)) -- (outer a b) forms the dyad ab
      - (scale sinPhi (crossMatrix a))

-- (crossMatrix a) <> b = a x b
crossMatrix :: Vector Double -> Matrix Double
crossMatrix v = fromLists [[0, - v @> z, v @> y]
                          ,[v @> z ,0,- v @> x]
                          ,[-v @> y, v @> x,0]]
  where x = 0
        y = 1
        z = 2


cx :: Vector Double -> Vector Double -> Vector Double
cx a b = (crossMatrix a) <> b

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
  in (rotationFormula a cosPhi) <> r'

transform3 :: (Vector Double, Vector Double, Vector Double) -> Vector Double -> Vector Double
transform3 (_, rO'', rC'') r'' = let
  y = 3 |> [0,1,0] :: Vector Double
  x = 3 |> [1,0,0] :: Vector Double
  rOC'' = rO'' - rC''
  cosTheta = (rOC'' <.> y) / (norm_2 rOC'')
  in (rotZ (sign $  rOC'' <.> x) cosTheta) <> r''

-- returns (a x b) / |a x b|
-- unless | a x b | = 0, in which case, we return |a x b|
-- this does no harm in this application to rotation
normdX :: Vector Double -> Vector Double -> Vector Double
normdX a b = let
  cross = cx a b
  norm = norm_2 cross
  in if norm /= 0
     then scale (1/norm) cross
     else cross
--normdX a b = normalize $ cx a b
-- normdX a b = let
--   axb = cx a b
--   in axb / (norm_2 axb)

--normalize :: Vector Double -> Vector Double
--normalize v = scale (1/(norm_2 v)) v

toHOCPlane :: (Vector Double, Vector Double, Vector Double) -> Vector Double -> Vector Double
toHOCPlane c r =
  let
    f' = transform1 c
    g' = transform2 $ map3 f' c
    h' = transform3 $ map3 (g'.f') c
  in h'.g'.f' $ r

-- testCase :: (Vector Double, Vector Double, Vector Double) -> Vector Double -> Vector Double
-- testCase c r =
--   let
--     g' = g $ c
--     h' = h $ map3 g' c
--   in h'.g' $ r

data Center = H1 | O | C | H2
              deriving (Show,Eq,Ord,Enum)

getCenter:: Center -> Vector Double -> Vector Double
getCenter H1 = subVector 0 3
getCenter O  = subVector 3 3
getCenter C  = subVector 6 3
getCenter H2 = subVector 9 3

whichNotRoamer :: Vector Double -> Center
whichNotRoamer p = let
  (rH1, rO, rC, rH2) = map4 (flip getCenter p) (H1,O,C,H2)
  com = cm (rH1, rO, rC, rH2)
  in if (norm_2 $ rH1 - com) > (norm_2 $ rH2 - com)
     then H2
     else H1

cm :: (Vector Double, Vector Double, Vector Double, Vector Double) -> Vector Double
cm (rH1, rO, rC, rH2)= ((rH1 * mH) + (rO * mO) + (rC * mC) + (rH2 * mH)) / (mH + mO + mC + mH)
  where mH = 1837.15
        mO = 29156.95
        mC = 21874.66

configToVector :: (Vector Double, Vector Double, Vector Double, Vector Double) -> Vector Double
configToVector = fromList . concat . list4 . (map4 toList)

--takes in 1 phase space vector and returns another, alligned in the HOCH plane
centerAndRotate :: Vector Double -> Vector Double
centerAndRotate g = let
  c = map3 (flip getCenter g) ((whichNotRoamer g), O , C)
  transformed = map4 (toHOCPlane c) $ map4 (flip getCenter g) (H1, O, C, H2)
  in configToVector transformed

-- Helper Functions

list4 :: (a,a,a,a) -> [a]
list4 (x1, x2, x3, x4) = x1:x2:x3:x4:[]
  
map4 :: (a ->b) -> (a,a,a,a) ->  (b,b,b,b)
map4 f (x1, x2, x3, x4) = (f x1, f x2, f x3, f x4)

map3 :: (a ->b) -> (a,a,a)  ->  (b,b,b)
map3 f (x1, x2, x3) = (f x1, f x2, f x3)