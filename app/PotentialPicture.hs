module Main where

import Formaldehyde.Data
import Formaldehyde.Helpers (fmtDoublesWith, (##@), (#))
import Formaldehyde.Potential as V
import Numeric.LinearAlgebra (Vector, norm_2, vjoin, (|>)) --, subVector, 
import qualified System.Environment as SE (getArgs)
-- import qualified System.IO as SIO (getContents)
-- --import Control.Monad
-- import qualified Control.Arrow as CA (second)
-- import qualified Data.Char (toLower)

rmin :: Double
rmin = 0.0
rmax :: Double
rmax = 12.0

main :: IO ()
main = do
  spacing <- fmap (read.head) SE.getArgs
  let
    row = [rmin, rmin + spacing .. rmax + spacing] --make sure we get the last point included
    points = [(x,y) | x<-row, y<-row]
    potentialPoints = map getPotential points
  mapM_ (putStrLn.(fmtDoublesWith "\t" 18)) potentialPoints

getPotential:: (Double, Double) -> [Double]
getPotential (x,y) = [x, y, V.potential $ getTotal x y]

cox :: Double
cox = norm_2 $ V.globalMinimum##@[C,O]

getTotal :: Double -> Double -> Vector Double
getTotal r rHH = vjoin [ 3 |> [cox - r,  rHH/2, 0]
                       , globalMinimum#O
                       , globalMinimum#C
                       , 3 |> [cox - r, -rHH/2, 0]
                       ]
