module Web.Giflib.Types where

import Data.Array (snoc)
import Data.Either (Either(Left))
import Halogen.HTML.Target (URL(), url, runURL)
import Data.Argonaut (foldJsonObject)
import Data.Argonaut.Combinators ((.?), (?>>=), (~>), (:=))
import Data.Argonaut.Core (Json(..), JArray(..), JObject(..), jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson)
import Data.Foldable (foldl)
import Web.Giflib.Internal.Unsafe (undefined)

import qualified Data.Date as Date
import qualified Data.Set as Set
import qualified Data.StrMap as StrMap
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

instance decodeJsonEntries :: DecodeJson [Entry] where
  decodeJson = foldJsonObject (Left "Top-level entries not an object") decodeEntries

decodeEntries :: StrMap.StrMap Json -> Either String [Entry]
decodeEntries json =
  StrMap.foldM parse [] json
  where
    parse :: [Entry] -> String -> Json -> Either String [Entry]
    parse acc key json = do
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

encodeEntriesObject :: [Entry] -> Json
encodeEntriesObject = foldl encodeEntry jsonEmptyObject

encodeEntry :: Json -> Entry -> Json
encodeEntry acc ex@(Entry e)
  =  (strId e.id) := ( encodeEntryInner ex )
  ~> acc
  where
    strId :: UUID -> String
    strId (UUID i) = i

encodeEntryInner :: Entry -> Json
encodeEntryInner (Entry e) =  "uri"  := runURL e.url
                           ~> "tags" := Set.toList e.tags
                           ~> "date" := (runMilliseconds $ Date.toEpochMilliseconds e.date)
                           ~> jsonEmptyObject
  where
    runMilliseconds :: Time.Milliseconds -> Number
    runMilliseconds (Time.Milliseconds n) = n

instance encodeJsonEntry :: EncodeJson Entry where
  encodeJson = encodeEntryInner
