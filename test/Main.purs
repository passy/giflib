module Test.Main where

import Test.Unit
import Web.Giflib.Types (Entry(..))
import Test.Fixtures (validEntriesJson, validEntriesRecord, invalidEntriesJson)
import Data.Argonaut (decodeJson, jsonParser)
import Data.Either (Either(), isRight, isLeft)
import Data.Either.Unsafe (fromRight, fromLeft)

import Debug.Trace
import Control.Monad.Eff.Class
import Web.Giflib.Internal.Debug
import Web.Giflib.Internal.Unsafe

main = runTest do
    test "decode a list of entries" do
        let result = decodeEntries validEntriesJson
        assert "Result could be parsed" $ isRight result
        assert "Decoded entry matches record" $ (unsafeShowPrintId $ fromRight result) == validEntriesRecord

    test "fails decoding invalid entries" do
        let result = decodeEntries invalidEntriesJson
        assert "Result fails to be parsed" $ isLeft result

decodeEntries :: String -> Either String [Entry]
decodeEntries v = jsonParser v >>= decodeJson
