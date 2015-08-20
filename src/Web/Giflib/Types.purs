module Web.Giflib.Types where

import Prelude
import Data.Argonaut.Combinators ((.?), (?>>=), (~>), (:=))
import Data.Argonaut.Core (Json(..), JArray(..), JObject(..), jsonEmptyObject, foldJsonObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson)
import Data.Array (snoc)
import Data.Either (Either(Left, Right))
import Data.Foldable (foldl)
import Data.Maybe (maybe)
import Data.URI (printURI, parseURI, runParseURI)
import Data.URI.Types (URI())
import Web.Giflib.Internal.Unsafe -- (undefined, unsafePrintId)

import qualified Data.Date as Date
import qualified Data.Int as Int
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.StrMap as StrMap
import qualified Data.Time as Time
import qualified Math as Math

type Tag = String

newtype Entry = Entry { id :: UUID
                      , uri :: URI
                      , tags :: Set.Set Tag
                      , date :: Date.Date
                      }

instance eqEntry :: Eq Entry where
  eq (Entry a) (Entry b) = a.id == b.id &&
                           a.uri == b.uri &&
                           a.tags == b.tags &&
                           a.date == b.date

instance showEntry :: Show Entry where
  show (Entry a) = "Entry { id: " ++ show a.id
                      ++ ", uri: " ++ show a.uri
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
      uri <- (obj .? "uri") :: Either String String
      uri' <- runParseURI $ parseURI uri
      tstamp <- (obj .? "date") :: Either String Number
      tags <- (obj .? "tags") :: Either String (Array Tag)
      date <- (Date.fromEpochMilliseconds <<< Time.Milliseconds $ tstamp) ?>>= "date"

      return $ snoc acc $ Entry { id: uuid key
                                , uri: uri'
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
encodeEntryInner (Entry e) =  "uri"  := printURI e.url
                           ~> "tags" := Set.toList e.tags
                           ~> "date" := (runMilliseconds $ Date.toEpochMilliseconds e.date)
                           ~> jsonEmptyObject
  where
    runMilliseconds :: Time.Milliseconds -> Number
    runMilliseconds (Time.Milliseconds n) = n

instance encodeJsonEntry :: EncodeJson Entry where
  encodeJson = encodeEntryInner
