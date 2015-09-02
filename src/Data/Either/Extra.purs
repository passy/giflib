module Data.Either.Extra
  ( rightToMaybe
  ) where

import Prelude
import Data.Either (Either(), either)
import Data.Maybe (Maybe(Just, Nothing))

rightToMaybe :: forall a b. Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just
