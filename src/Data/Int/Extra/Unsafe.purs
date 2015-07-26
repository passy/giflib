module Data.Int.Extra.Unsafe
  ( unsafeFromNumber
  ) where

import Data.Maybe (Maybe(..))

foreign import unsafeFromNumberImpl :: (forall a. a -> Maybe a)
                                    -> (forall a. Maybe a)
                                    -> Number
                                    -> Maybe Int

-- | Creates an `Int` from a `Number` value, only verifying that there is
-- | no information loss due to fraction cut-off.
unsafeFromNumber :: Number -> Maybe Int
unsafeFromNumber = unsafeFromNumberImpl Just Nothing
