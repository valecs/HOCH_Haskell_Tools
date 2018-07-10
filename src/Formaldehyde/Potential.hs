module Formaldehyde.Potential (potential, globalMinimum, boundary) where

import Foreign (Ptr)
import qualified Foreign.Marshal.Unsafe as FMU (unsafeLocalState)
import Numeric.LinearAlgebra (Vector, fromList)
import qualified Data.Vector.Storable as DVS (unsafeWith)

-- | Global minimum on the HOCH potential
globalMinimum :: Vector Double
globalMinimum = fromList [-1.12460784,  1.79389857, 0.00000000
                               ,2.28820140,  0.00000000, 0.00000000
                               ,0.00000000,  0.00000000, 0.00000000
                               ,-1.12460784, -1.79389857, 0.00000000
                               ] :: Vector Double


-- N.B. we don't use Foreign.C.Types.CDouble, the lack of which could cause weirdness if
-- NaN or Inf were to be returned. This should not be the case here.

foreign import ccall unsafe "potentialFFI.h getFormaldehydePotential"
  c_getFormaldehydePotential :: Ptr Double -> IO Double

-- We can safely specify this as pure (via the call to unsafeLocalState) because
-- "getFormaldehydePotential" as defined in potentialFFI.h is pure:
-- double getFormaldehydePotential(double position[12]);

-- | Returns the energy of the given Vector
potential :: Vector Double -> Double
potential v = FMU.unsafeLocalState $ DVS.unsafeWith v c_getFormaldehydePotential

-- | use like, e.g.: (HOCH.boundary 1e-4 el)
boundary :: Double -> Double -> Vector Double -> Bool
boundary delta el r = delta > abs ( 1 - potential r / el)
