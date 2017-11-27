module Formaldehyde.Properties (
  mass
  ,muMass
  ,massOf
  ,massTotal
  ,massM
  ) where

import Formaldehyde.Data (Center(..))
import Numeric.LinearAlgebra

mass :: (Fractional a) => Center -> a
mass H1 = 1837.15
mass O  = 29156.95
mass C  = 21874.66
mass H2 = 1837.15 --mass H1

massTotal :: (Fractional a) => a
massTotal = 54705.91

muMass :: (Fractional a) => (Center, Center) -> a
muMass (a,b) = (ma * mb) / (ma + mb) where
  ma = mass a
  mb = mass b

massOf :: (Fractional a) => [Center] -> a
massOf cs = foldl (flip $ (+).mass) 0.0 cs

massM :: Matrix Double
massM = (12><12) [
  1837.15,0,0,0,0,0,0,0,0,0,0,0
  ,0, 1837.15,0,0,0,0,0,0,0,0,0,0
  ,0,0, 1837.15,0,0,0,0,0,0,0,0,0
  ,0,0,0,29156.95,0,0,0,0,0,0,0,0
  ,0,0,0,0,29156.95,0,0,0,0,0,0,0
  ,0,0,0,0,0,29156.95,0,0,0,0,0,0
  ,0,0,0,0,0,0,21874.66,0,0,0,0,0
  ,0,0,0,0,0,0,0,21874.66,0,0,0,0
  ,0,0,0,0,0,0,0,0,21874.66,0,0,0
  ,0,0,0,0,0,0,0,0,0, 1837.15,0,0
  ,0,0,0,0,0,0,0,0,0,0, 1837.15,0
  ,0,0,0,0,0,0,0,0,0,0,0, 1837.15
  ]

