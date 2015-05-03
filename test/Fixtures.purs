module Test.Fixtures where

import Web.Giflib.Types (Entry(..), uuid)
import Halogen.HTML.Target (url)
import Data.Maybe.Unsafe (fromJust)

import qualified Data.Date as Date
import qualified Data.Set as Set

entriesJson :: String
entriesJson = """{
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

entriesRecord = Entry <$>
    [ { id: uuid "CDF20EF7-A181-47B7-AB6B-5E0B994F6176"
      , url: url "http://media.giphy.com/media/JdCz7YXOZAURq/giphy.gif"
      , tags: Set.fromList [ "hamster", "party", "animals" ]
      , date: fromJust $ Date.fromString "2015-01-01 00:00:00" }
    , { id: uuid "EA72E9A5-0EFA-44A3-98AA-7598C8E5CD14"
      , url: url "http://media.giphy.com/media/lkimmb3hVhjvWF0KA/giphy.gif"
      , tags: Set.fromList [ "cat", "wiggle", "animals" ]
      , date: fromJust $ Date.fromString "2015-02-01 00:00:00" }
    ]
