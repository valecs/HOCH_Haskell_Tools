import Formaldehyde.Helpers (readPath, fileInput, fmtDoublesWith)
import Formaldehyde.BirdsNest (toHHCOHHa)

{-
  transforms a path into our prefered CO-HH --- H-H representation
-}
main :: IO ()
main = do
  vs <- fmap readPath fileInput
  mapM_ (putStrLn . fmtDoublesWith "\t" 16 . toHHCOHHa) vs
