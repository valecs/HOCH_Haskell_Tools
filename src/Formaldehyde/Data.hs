module Formaldehyde.Data (
  Center(..) -- (..) to also export construcors
  ,Channel(..)
  ) where

data Center = H1 | O | C | H2
              deriving (Show,Eq,Ord,Enum)

data Channel = Direct | Radical | Roaming
               deriving (Show,Read,Eq,Ord,Enum)
