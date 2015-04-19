module WSK.Internal where

import Data.Maybe
import Data.Monoid

-- | Apply a function on a Maybe value inside a monoid or convert to an empty
-- monoid instead. Useful for optional attributes to elements. Don't ask me
-- what the name stands for. It's short.
-- > button t = H.button ([ A.classes btnClasses ] <>
-- >                      (mip A.id_ t.id)) [ H.text t.text ]
mip :: forall a b f. (Monoid (f b), Applicative f) => (a -> b) -> Maybe a -> f b
mip f = maybe mempty (pure <<< f)
