module Formaldehyde.Helpers (
  readPath
  ,readTrajectory
  ,readTrajectoryTimes
  ,fileInput
  ,(#), (##@), (##), (##$)
  ,cm
  ,mapv 
  ,whichRoamer
  ,pathRoamer
  ,fmtDouble
  ,fmtVector
  ,fmtDoublesWith
  ,fmtVectorWith
  ,getCenter
  ) where

import Formaldehyde.Data (Center(..))
import Formaldehyde.Properties
import Numeric.LinearAlgebra
import Data.List (intersperse,maximumBy,foldl')
import Data.Ord (comparing)
import Numeric (showFFloat)
import qualified System.IO (getContents)
import qualified System.Environment (getArgs)

getCenter:: Center -> Vector Double -> Vector Double
getCenter H1 = subVector 0 3
getCenter O  = subVector 3 3
getCenter C  = subVector 6 3
getCenter H2 = subVector 9 3

-- | convenient subvector extraction
(#) :: Vector Double -> Center -> Vector Double
g # c = getCenter c g

-- | G ## (A,B) -> rB - rA
-- G ## (A,B) points from A to B
(##) :: Vector Double -> (Center, Center) ->  Vector Double
g ## (c1,c2) = (g # c2) - (g # c1)

-- | G ##$ (A,B) -> |rB - rA|
(##$) :: Vector Double -> (Center, Center) ->  Double
g ##$ (c1,c2) = norm_2 $ g##(c1,c2)
  
-- | center of mass calculation
(##@) :: Vector Double -> [Center] -> Vector Double
g ##@ cs = (foldl' (+) origin [(g # c) * (mass c) | c<-cs]) / massT
  where origin = 3|>repeat 0
        massT = massOf cs

-- | applies the given function to each center in the c-vector
mapv :: (Vector Double -> Vector Double) -> Vector Double -> Vector Double
mapv f v = fromList $ concat $ map (toList.f) $ map (v#) [H1 ..]

-- Returns the identiy of the hydrogen farthest from the center of mass
whichRoamer :: Vector Double -> Center
whichRoamer p = let
  com = cm p
  in if (norm_2 $ (p#H1) - com) > (norm_2 $ (p#H2) - com)
     then H1
     else H2

-- Returns the idenity of the hydrogen farthest from the center of
-- mass at the point in the trajectory with the largest H-H separation
pathRoamer :: [Vector Double] -> Center
pathRoamer vs = whichRoamer $ maximumBy (comparing $ (flip (##$))(H1,H2)) vs

-- | Computes the HOCH center of mass from a configuration space vector
cm :: Vector Double -> Vector Double
cm g = g ##@ [H1 ..]
      
-- Printing Functions
fmtDouble :: RealFloat a => Int -> a -> String
fmtDouble n r = showFFloat (Just n) r ""

fmtDoublesWith :: String -> Int -> [Double] -> String
fmtDoublesWith s n ds = foldl' (++) "" (intersperse s $ map (fmtDouble n) ds)

fmtVectorWith :: String -> Int -> Vector Double -> String
fmtVectorWith s n v = fmtDoublesWith s n $ toList v

fmtVector :: Int -> Vector Double -> String
fmtVector = fmtVectorWith "\n"

-- Loading Functions
fileInput :: IO String
fileInput = System.Environment.getArgs>>=readFirst where
  readFirst :: [FilePath] -> IO String
  readFirst []      = System.IO.getContents
  readFirst ("-":_) = System.IO.getContents
  readFirst (x:_)   = readFile x


readPath :: String -> [Vector Double]
readPath  = (toVectorsWith 0 12).(map read).lines

readTrajectory :: String -> [Vector Double]
readTrajectory = (toVectorsWith 1 24).(map read).lines

-- | yields vectors after dropping k interleaved headder lines
toVectorsWith ::  Int -> Int -> [Double] -> [Vector Double]
toVectorsWith _ _ [] = []
toVectorsWith k n xs = let (as',bs) = splitAt (n+k) xs
                           as = drop k as'
                       in (n|>) as : toVectorsWith k n bs

readTrajectoryTimes :: String -> [(Double, Vector Double)]
readTrajectoryTimes = loadTrajectoryTimes.(map read).lines

loadTrajectoryTimes :: [Double] -> [(Double, Vector Double)]
loadTrajectoryTimes [] = []
loadTrajectoryTimes xs = (t, (n|>) g) : loadTrajectoryTimes vs
  where n = 24
        (v1, vs) = splitAt (n+1) xs
        ([t], g) = splitAt 1 v1
