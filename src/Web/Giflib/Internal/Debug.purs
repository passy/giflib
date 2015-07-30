module Web.Giflib.Internal.Debug
( log
, Console()
) where

import Prelude
import Control.Monad.Eff (Eff())

-- | The `Console` effect represents an action in the `window.console`.
foreign import data Console :: !

-- | We can log pretty much everything, no matter if it's `Show`able or not
--   directly.
foreign import log :: forall a eff. a -> Eff (console :: Console | eff) Unit
