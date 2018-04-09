{-# OPTIONS -Wall #-} 

import Numeric.LinearAlgebra (Vector, (#>), (<.>), (|>), unitary)
import Formaldehyde.Invariant(centerAndRotate, rotationFormula)
import Formaldehyde.Helpers (readPath, fmtVector, mapv, (#))
import Formaldehyde.Data
import System.IO (getContents)
-- import Debug.Trace (trace)
-- tr :: (Show a) => a -> a
-- tr a = trace (show a) a

main :: IO ()
main = do
  points <- fmap readPath System.IO.getContents
  let positions = map (cto0.centerAndRotate) points 
  mapM_ (putStrLn . (fmtVector 16)) positions


-- transforms the c-vector such that C is at the origin
cto0 :: Vector Double -> Vector Double
cto0 v = mapv (flip (-) (v#C)) v

-- busted still!!
-- rotates c-vector about z so O is along y-axis
otox :: Vector Double -> Vector Double
otox v = mapv ((rotationFormula z ((unitary v#O) <.> y ))#>) v where
  z = 3|>[0,0,1]
  y = 3|>[0,1,0]


