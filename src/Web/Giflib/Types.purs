module Web.Giflib.Types where

import Data.Array (snoc)
import Data.Either (Either(Left))
import Halogen.HTML.Target (URL(), url)
import Data.Argonaut (foldJsonObject)
import Data.Argonaut.Combinators ((.?), (?>>=))
import Data.Argonaut.Core (Json(..), JArray(..), JObject(..))
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Web.Giflib.Internal.Unsafe (undefined)

import qualified Data.StrMap as StrMap
import qualified Data.Set as Set
import qualified Data.Date as Date
import qualified Data.Time as Time

type Tag = String

newtype Entry = Entry { id :: UUID
                      , url :: URL
                      , tags :: Set.Set Tag
                      , date :: Date.Date
                      }

instance eqEntry :: Eq Entry where
  (==) (Entry a) (Entry b) = a.id == b.id &&
                             a.url == b.url &&
                             a.tags == b.tags &&
                             a.date == b.date
  (/=) a         b         = not (a == b)

instance showEntry :: Show Entry where
  show (Entry a) = "Entry { id: " ++ show a.id
                      ++ ", url: " ++ show a.url
                      ++ ", tags: " ++ show a.tags
                      ++ ", date: " ++ show a.date
                      ++ "}"

newtype UUID = UUID String

instance eqUUID :: Eq UUID where
  (==) (UUID a) (UUID b) = a == b
  (/=) a        b        = not (a == b)

instance showUUID :: Show UUID where
  show (UUID a) = "uuid " ++ show a

uuid :: String -> UUID
uuid = UUID

instance decodeJsonEntry :: DecodeJson [Entry] where
  decodeJson = foldJsonObject (Left "Top-level entries not an object") decodeEntry

decodeEntry :: StrMap.StrMap Json -> Either String [Entry]
decodeEntry json =
  StrMap.foldM parse [] json
  where
    parse :: [Entry] -> String -> Json -> Either String [Entry]
    parse acc key json = do
      -- TODO: Investigate if applicative would suffice here.
      obj <- decodeJson json
      url' <- (obj .? "uri") :: Either String String
      tstamp <- (obj .? "date") :: Either String Number
      tags <- (obj .? "tags") :: Either String [Tag]
      date <- (Date.fromEpochMilliseconds <<< Time.Milliseconds $ tstamp) ?>>= "date"

      return $ snoc acc $ Entry { id: uuid key
                                , url: url url'
                                , tags: Set.fromList tags
                                , date: date
                                }
