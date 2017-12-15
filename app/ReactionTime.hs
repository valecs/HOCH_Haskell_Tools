module Main where

import Formaldehyde.Data
import Formaldehyde.Helpers (readTrajectoryTimes, fmtDouble, (##@), (##$))
import Numeric.LinearAlgebra (norm_2, subVector, Vector)
import qualified System.IO as SIO (getContents)
import qualified System.Environment as SE (getArgs) 
--import Control.Monad
import qualified Control.Arrow as CA (second)
import qualified Data.Char (toLower)

{-
From cli:
ReactionTime { D | A | R } rLim < trajectory
-}

data Config =
  Config {
  chan :: Channel,
  lim  :: Double
  }
  deriving (Show)

main :: IO ()
main = do
  gs <- fmap readTrajectoryTimes SIO.getContents
  (Config c l) <- readArgs
  let
    vs = map (CA.second (subVector 0 12)) gs
  putStrLn $ fmtDouble 5 $ findReactionTime (predicateBuilder c l) vs

predicateBuilder :: Channel -> Double -> (Vector Double -> Bool)
predicateBuilder c l =
  let closed x = norm_2 (x ##@ [C,O] - x ##@ [H1,H2]) > l
  in case c of
    Roaming -> closed
    Direct  -> closed
    Radical -> \x ->  x ##$ (H1,H2) > l

-- returns the last frame (counting in reverse) for which predicate is true 
findReactionTime :: (Vector Double -> Bool) -> [(Double, Vector Double)] -> Double
findReactionTime p vs = case lastTruth (snd . CA.second p) (reverse vs) of
  Nothing -> (-1) --this isn't correct, but provides a sentinel value
  Just a  -> fst a

-- returns the the last a s.t. the predicate is true
lastTruth :: (a -> Bool) -> [a] -> Maybe a
lastTruth _ [] = Nothing
lastTruth p (x:xs)
  | p x       = lastTruth' xs (Just x)
  | otherwise = Nothing
  where
    lastTruth' [] result = result
    lastTruth' (x:xs) result
      | p x       = lastTruth' xs (Just x)
      | otherwise = result

readC :: String -> Channel
readC s
  | any ($ s) [(=="D"), (=="direct").toLower'  ] = Direct
  | any ($ s) [(=="A"), (=="radical").toLower' ] = Radical
  | any ($ s) [(=="R"), (=="roaming").toLower', (=="roam").toLower' ] = Roaming
  | otherwise   = undefined
  where
    toLower' = map Data.Char.toLower

-- Load arguments from command line. Expected to be like:
-- ReactionTime-exe { D | A | R } rLim < trajectory
-- errors out otherwise
readArgs :: IO Config
readArgs = do
  [c', l'] <- SE.getArgs
  let
    c = readC c'
    l = read l'
    in return $ Config c l
 
