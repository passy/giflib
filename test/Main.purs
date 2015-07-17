module Test.Main where

import Prelude
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(), isRight, isLeft)
import Data.Either.Unsafe (fromRight, fromLeft)
import Test.Fixtures (validEntriesJson, validEntriesRecord, invalidEntriesJson)
import Web.Giflib.Types (Entry(..), encodeEntriesObject)

import Control.Monad.Eff.Class
import Test.Unit
import Web.Giflib.Internal.Unsafe

main = runTest do
    test "decode a list of entries" do
        let result = decodeEntries validEntriesJson
        assert "Result could be parsed" $ isRight result
        assert "Decoded entry matches record" $ fromRight result == validEntriesRecord

    test "fails decoding invalid entries" do
        let result = decodeEntries invalidEntriesJson
        assert "Result fails to be parsed" $ isLeft result

    test "encode list of entries" do
        let entries = (decodeEntries <<< encodeEntries) validEntriesRecord
        assert "Result can be encoded back and forth" $
            validEntriesRecord == (fromRight entries)

decodeEntries :: String -> Either String (Array Entry)
decodeEntries v = jsonParser v >>= decodeJson

encodeEntries :: Array Entry -> String
encodeEntries = show <<< encodeEntriesObject
