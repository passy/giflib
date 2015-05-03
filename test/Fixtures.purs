module Test.Fixtures where

import Web.Giflib.Types (Entry(..), uuid)
import Halogen.HTML.Target (url)
import Data.Maybe.Unsafe (fromJust)

import qualified Data.Date as Date
import qualified Data.Time as Time
import qualified Data.Set as Set

validEntriesJson :: String
validEntriesJson = """{
  "-JnknoQihfbdX5VZA9Z_": {
    "date": 1429969259654,
    "tags": [
      "hamster",
      "party",
      "animal",
      "test"
    ],
    "uri": "http://media.giphy.com/media/JdCz7YXOZAURq/giphy.gif"
  },
  "-Jnknr0LLiYo2pqbO9D-": {
    "date": 1429969270254,
    "tags": [
      "excited",
      "ooh"
    ],
    "uri": "http://media.giphy.com/media/cbG9wtoO8QScw/giphy.gif"
  },
  "-JnkntKgOTda5_FbgTWs": {
    "date": 1429969279748,
    "tags": [
      "michael",
      "no"
    ],
    "uri": "http://media.giphy.com/media/12XMGIWtrHBl5e/giphy.gif"
  }
}
"""

invalidEntriesJson :: String
invalidEntriesJson = """{
  "-JnknoQixfbdX5VZA9Z_": {
    "date": "not a date",
    "tags": [
      "hamster",
      "party",
      "animal",
      "test"
    ],
    "uri": "http://media.giphy.com/media/JdCz7YXOZAURq/giphy.gif"
  }
}
"""

validEntriesRecord :: [Entry]
validEntriesRecord = Entry <$>
                [ { id: uuid "-JnknoQihfbdX5VZA9Z_"
                  , url: url "http://media.giphy.com/media/JdCz7YXOZAURq/giphy.gif"
                  , tags: Set.fromList [ "animal", "hamster", "party", "test" ]
                  , date: fromJust (Date.fromEpochMilliseconds (Time.Milliseconds 1429969259654)) }
                , { id: uuid "-Jnknr0LLiYo2pqbO9D-"
                  , url: url "http://media.giphy.com/media/cbG9wtoO8QScw/giphy.gif"
                  , tags: Set.fromList [ "excited", "ooh" ]
                  , date: fromJust (Date.fromEpochMilliseconds (Time.Milliseconds 1429969270254)) }
                , { id: uuid "-JnkntKgOTda5_FbgTWs"
                  , url: url "http://media.giphy.com/media/12XMGIWtrHBl5e/giphy.gif"
                  , tags: Set.fromList [ "michael", "no" ]
                  , date: fromJust (Date.fromEpochMilliseconds (Time.Milliseconds 1429969279748)) }
                ]
