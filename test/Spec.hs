import System.Exit

import PhaseSpaceSums

-- Computes phase space fractions for qstar=(0..1) via the phase space approximation and compares them the the analytical results.

main :: IO ()
main =  do
  let tests = map ((testZero 1e-4).shoFracErr) [0.01, 0.02 .. 0.99 ]
  mapM_ print tests
  let (bools, _) = unzip tests
  let success = and bools
  print success
  if success
    then exitSuccess
    else exitFailure

shoFracErr :: Double -> Double
shoFracErr qstar = (actual - expected) / expected where
  expected = acos qstar / pi
  actual = uncurry (/) $ integrCount (\x -> θ (x - qstar)) $ map cos [1e-4, 2e-4 .. 2 * pi]

testZero :: Double -> Double -> (Bool, Double)
testZero eps x = (abs x < eps, x)
