module Web.Giflib.Types where

import Prelude
import Data.Array (snoc)
import Data.Either (Either(Left))
import Halogen.HTML.Target (URL(), url, runURL)
import Data.Argonaut.Combinators ((.?), (?>>=), (~>), (:=))
import Data.Argonaut.Core (Json(..), JArray(..), JObject(..), jsonEmptyObject, foldJsonObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson)
import Data.Foldable (foldl)
import Web.Giflib.Internal.Unsafe (undefined)

import qualified Data.Date as Date
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.StrMap as StrMap
import qualified Data.Time as Time

type Tag = String

newtype Entry = Entry { id :: UUID
                      , url :: URL
                      , tags :: Set.Set Tag
                      , date :: Date.Date
                      }

instance eqEntry :: Eq Entry where
  eq (Entry a) (Entry b) = a.id == b.id &&
                           a.url == b.url &&
                           a.tags == b.tags &&
                           a.date == b.date

instance showEntry :: Show Entry where
  show (Entry a) = "Entry { id: " ++ show a.id
                      ++ ", url: " ++ show a.url
                      ++ ", tags: " ++ show a.tags
                      ++ ", date: " ++ show a.date
                      ++ "}"

newtype UUID = UUID String

instance eqUUID :: Eq UUID where
  eq (UUID a) (UUID b) = a == b

instance showUUID :: Show UUID where
  show (UUID a) = "uuid " ++ show a

uuid :: String -> UUID
uuid = UUID

runUUID :: UUID -> String
runUUID (UUID s) = s

instance decodeJsonEntries :: DecodeJson (Array Entry) where
  decodeJson = foldJsonObject (Left "Top-level entries not an object") decodeEntries

decodeEntries :: StrMap.StrMap Json -> Either String (Array Entry)
decodeEntries json =
  StrMap.foldM parse [] json
  where
    parse :: (Array Entry) -> String -> Json -> Either String (Array Entry)
    parse acc key json = do
      obj <- decodeJson json
      url' <- (obj .? "uri") :: Either String String
      tstamp <- (obj .? "date") :: Either String Int
      tags <- (obj .? "tags") :: Either String (Array Tag)
      date <- (Date.fromEpochMilliseconds <<< Time.Milliseconds $ tstamp) ?>>= "date"

      return $ snoc acc $ Entry { id: uuid key
                                , url: url url'
                                , tags: Set.fromList $ List.toList $ tags
                                , date: date
                                }

encodeEntriesObject :: (Array Entry) -> Json
encodeEntriesObject = foldl encodeEntry jsonEmptyObject

encodeEntry :: Json -> Entry -> Json
encodeEntry acc ex@(Entry e)
  =  (runUUID e.id) := ( encodeEntryInner ex )
  ~> acc

encodeEntryInner :: Entry -> Json
encodeEntryInner (Entry e) =  "uri"  := runURL e.url
                           ~> "tags" := Set.toList e.tags
                           ~> "date" := (runMilliseconds $ Date.toEpochMilliseconds e.date)
                           ~> jsonEmptyObject
  where
    runMilliseconds :: Time.Milliseconds -> Int
    runMilliseconds (Time.Milliseconds n) = n

instance encodeJsonEntry :: EncodeJson Entry where
  encodeJson = encodeEntryInner
