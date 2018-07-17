module Main where

import Formaldehyde.Helpers (readTrajectoryTimes, fmtDouble, fileInput)
import Formaldehyde.BirdsNest (inBirdsNest, filter')

{-
From cli:
BirdIncubationTime < trajectory
-}

-- returns the time of first exit from the bird's nest
main :: IO ()
main = do
  gs <- fmap readTrajectoryTimes fileInput
  let
    (time, _) = last $ filter' (inBirdsNest.snd) gs
  putStrLn $ fmtDouble 5 time
