import Formaldehyde.Helpers (readPath, fmtVectorWith)
import Formaldehyde.BirdsNest (filter', inBirdsNest)
import qualified Formaldehyde.Potential as HOCH (boundary)
import Formaldehyde.Properties (wn2hartree)

import System.Environment (getArgs)
import System.IO (getContents)


{-
  extracts the last configuration in the bird's nest that is also on a boundary
-}
main :: IO ()
main = do
  el' <- fmap (read.head) getArgs :: IO Double
  vs  <- fmap readPath getContents

  let
    el = el' / wn2hartree
    fvs' = filter' inBirdsNest vs
    fvs = filter (HOCH.boundary 1e-4 el) fvs'
    v = last fvs
  putStrLn $ fmtVectorWith "\n" 16 v
