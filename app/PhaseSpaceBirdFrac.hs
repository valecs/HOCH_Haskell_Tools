module Main where

import Numeric.LinearAlgebra
import Formaldehyde.Helpers(fileInput, readTrajectory, fmtDouble)
import Formaldehyde.BirdsNest (inBirdsNest)
import Control.Monad

import PhaseSpaceSums

main :: IO ()
main = do
  traj <- liftM readTrajectory fileInput
  let (f, n) = integrCount (bti.inBirdsNest) traj
  putStrLn $ (fmtDouble 16 f) ++ "\t" ++ show n


bti :: (Num a) => Bool -> a
bti True  = 1
bti False = 0
