module Formaldehyde.Data (
  Center(..) -- (..) to also export construcors
  ) where

data Center = H1 | O | C | H2
              deriving (Show,Eq,Ord,Enum)

