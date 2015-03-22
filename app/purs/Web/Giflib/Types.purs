module Web.Giflib.Types where

import Data.Date (Date())

type URI = String
type Tag = String

type Entry = { id :: String
             , uri :: URI
             , tags :: [Tag]
             , date :: Date
             }
