module Web.Giflib.Types where

import Data.Date (Date())

type URI = String
type Tag = String

type Entry = { uri :: URI
             , tags :: [Tag]
             , date :: Date
             }
