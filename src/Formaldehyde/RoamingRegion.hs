{-# OPTIONS -Wall #-}

module Formaldehyde.RoamingRegion (
  isRoaming
  ,forceOnCenter
 ) where

import Numeric.LinearAlgebra
import Formaldehyde.Helpers (getCenter, whichRoamer, cm)
import Formaldehyde.Data (Center(..))
import qualified Formaldehyde.Potential as HOCH 

-- documented 2014.08.14:174
isRoaming :: Vector Double -> Bool 
isRoaming g = let
  roamingH = whichRoamer g
  get = (flip getCenter) g
  rHr = get roamingH
  rH0 = (get.notR) roamingH
  rHH = (norm_2 (rHr - rH0))
  in (rHH > 5.8672) && (rHH < 9.0) && ((forceOnCenter roamingH g)> (-2.5e-4))


-- Computes the force on a center towards the center of mass
forceOnCenter :: Center -> Vector Double -> Double
forceOnCenter c  v = let
  f = negate $ getCenter c $  gradient 1e-4 HOCH.potential v
  v' = (getCenter c v) - (cm v)
  norm = norm_2 v'
  in (dot f v') / norm

-- Force on the roaming hydrogen towards the center of mass
forceOnRoamingH :: Vector Double -> Double
forceOnRoamingH v = forceOnCenter (whichRoamer v) v

notR :: Center -> Center
notR H1 = H2
notR H2 = H1
notR _  = error "invalid center"


-- Helper function to find the partial derivative of f at a point r along v
-- from A&S
directionalDerivative :: Double -> (Vector Double -> Double) -> Vector Double -> Vector Double -> Double
directionalDerivative h f r v = let
  v'  = scale (1/(norm_2 v)) v
  f1 = f $ r - (scale $ 2*h) v'
  f2 = f $ r - (scale $ 1*h) v'
  f4 = f $ r + (scale $ 1*h) v'
  f5 = f $ r + (scale $ 2*h) v'
  in (1/(12*h)) * (f1 - 8*f2 +8*f4 -f5)


gradient :: Double -> (Vector Double -> Double) -> Vector Double -> Vector Double
gradient h f r = fromList $ map (directionalDerivative h f r) basis
                 where basis = map fromList $ toLists (ident $ (length . toList) r :: Matrix Double)
