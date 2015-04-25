module Web.Giflib.Types where

import Data.Date (Date())
import qualified Node.UUID as UUID

type URI = String
type Tag = String

type Entry = { id :: UUID.UUID
             , uri :: URI
             , tags :: [Tag]
             , date :: Date
             }
