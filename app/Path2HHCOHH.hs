import Formaldehyde.Helpers (readPath, fileInput, fmtVectorWith)
import Formaldehyde.BirdsNest (toHHCOHH)

{-
  transforms a path into our prefered CO-HH --- H-H representation
-}
main :: IO ()
main = do
  vs <- fmap readPath fileInput
  mapM_ (putStrLn . fmtVectorWith "\t" 16 . toHHCOHH) vs
