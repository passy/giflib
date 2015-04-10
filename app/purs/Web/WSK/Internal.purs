module WSK.Internal where

import Data.Maybe
import Data.Monoid

mip :: forall a b f. (Monoid (f b), Applicative f) => (a -> b) -> Maybe a -> f b
mip f = maybe mempty (pure <<< f)
