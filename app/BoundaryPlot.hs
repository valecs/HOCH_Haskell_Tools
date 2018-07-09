import Formaldehyde.Helpers
import Formaldehyde.Data
import qualified Formaldehyde.Potential as HOCH
import Numeric.LinearAlgebra
import Formaldehyde.BirdsNest (inBirdsNest,filter')

import System.Environment (getArgs)
import System.IO (getContents)

{-
   BoundaryPlot-exe EL < geodesic.path

   Prints the COHH-HH coordinates of the geodesic on the boundary
   while in the bird's nest. EL pased in wavenumbers.
-}

-- wavenumbers per hartree
hc :: Double
hc = 219474.6313705

main :: IO ()
main = do
  el' <- fmap (read.head) getArgs :: IO Double
  vs' <- fmap readPath getContents

  let
    el = el' / hc
    -- Cannot combine filters since filter' stops on first failure
    vs  = filter' inBirdsNest vs'
    fvs = filter (boundary 1e-4 el) vs

  mapM_ f fvs where
    f = putStrLn . fmtVectorWith "\t" 16 . toCOHH

-- 
boundary :: Double -> Double -> Vector Double -> Bool
boundary delta el r = delta > abs ( 1 - HOCH.potential r / el)

toCOHH :: Vector Double -> Vector Double
toCOHH v = fromList [rCOHH, rHH] where
  rCOHH = norm_2 $ (v ##@ [C,O]) - (v ##@ [H1,H2])
  rHH   = v ##$ (H1,H2)
