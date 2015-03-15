module Web.Giflib.Types where

import Data.Date (Date())

type URI = String
type Tag = String

data Entry = Entry { uri :: URI
                   , tags :: [Tag]
                   , date :: Date
                   }
