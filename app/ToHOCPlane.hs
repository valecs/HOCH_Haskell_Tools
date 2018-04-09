{-# OPTIONS -Wall #-} 

import Numeric.LinearAlgebra (Vector)
import Formaldehyde.Invariant(centerAndRotate)
import Formaldehyde.Helpers (readPath, fmtVector, mapv, (#))
import Formaldehyde.Data
import System.IO (getContents)

main :: IO ()
main = do
  points <- fmap readPath System.IO.getContents
  let positions = map (cto0.centerAndRotate) points 
  mapM_ (putStrLn . (fmtVector 16)) positions


-- transforms the c-vector such that C is at the origin
cto0 :: Vector Double -> Vector Double
cto0 v = mapv (flip (-) (v#C)) v
