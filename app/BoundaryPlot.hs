import Formaldehyde.Helpers
import Formaldehyde.Properties (wn2hartree)
import qualified Formaldehyde.Potential as HOCH (boundary)
import Formaldehyde.BirdsNest (inBirdsNest, filter', toHHCOHH)

import System.Environment (getArgs)
import System.IO (getContents)

{-
   BoundaryPlot-exe EL < geodesic.path

   Prints the COHH-HH coordinates of the geodesic on the boundary
   while in the bird's nest. EL pased in wavenumbers.
-}

main :: IO ()
main = do
  el' <- fmap (read.head) getArgs :: IO Double
  vs' <- fmap readPath getContents

  let
    el = el' / wn2hartree
    -- Cannot combine filters since filter' stops on first failure
    vs  = filter' inBirdsNest vs'
    fvs = filter (HOCH.boundary 1e-4 el) vs

  mapM_ f fvs where
    f = putStrLn . fmtVectorWith "\t" 16 . toHHCOHH

 

