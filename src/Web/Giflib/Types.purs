module Web.Giflib.Types where

import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Either
import Data.Foldable
import Data.Traversable
import Halogen.HTML.Target (URL(), url)
import Data.Argonaut
import Data.Argonaut.Core (Json(..), JArray(..), JObject(..))
import Data.Argonaut.Decode (DecodeJson)
import Web.Giflib.Internal.Unsafe (undefined)

import qualified Data.StrMap as StrMap
import qualified Data.Set as Set
import qualified Data.Date as Date

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

newtype UUID = UUID String

instance eqUUID :: Eq UUID where
  (==) (UUID a) (UUID b) = a == b
  (/=) a        b        = not (a == b)

uuid :: String -> UUID
uuid = UUID

instance decodeJsonEntry :: DecodeJson [Entry] where
  decodeJson = foldJsonObject (Left "Top-level entries not an object") decodeEntry

decodeEntry :: StrMap.StrMap Json -> Either String [Entry]
decodeEntry json =
  StrMap.foldM go [] json
  where
    -- TODO: Is this defined somewhere?
    snoc = flip (:)

    go :: [Entry] -> String -> Json -> Either String [Entry]
    go acc key json = do
      -- TODO: Investigate if applicative would suffice here.
      obj <- (decodeJson json :: Either String (StrMap.StrMap Json))
      url' <- obj .? "uri"
      tstamp <- obj .? "date"

      return $ snoc acc $ Entry { id: uuid key
                                , url: url "text" -- url url'
                                , tags: Set.fromList [ "hamster", "party", "animals" ]
                                , date: fromJust $ Date.date 2015 Date.January 1
                                }
