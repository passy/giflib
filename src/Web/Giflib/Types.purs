module Web.Giflib.Types where

import Data.Date (Date())
import Data.Set (Set())
import Halogen.HTML.Target (URL())
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Web.Giflib.Internal.Unsafe (undefined)
import qualified Node.UUID as UUID

type Tag = String

newtype Entry = Entry { id :: UUID.UUID
                      , url :: URL
                      , tags :: Set Tag
                      , date :: Date
                      }

instance decodeJsonEntry :: DecodeJson [Entry] where
  decodeJson json = undefined
