module Web.Giflib.Internal.Unsafe
  ( unsafePerformEff
  , unsafePrintId
  , undefined
  ) where

import Data.Foreign (Foreign(), toForeign)
import Debug.Trace (trace)
import Control.Monad.Eff (Eff())


foreign import unsafePerformEff
  """
  function unsafePerformEff(f) {
    return f();
  }
  """ :: forall a eff. Eff eff a -> a

foreign import showForeign
  """
  function showForeign(a) {
    return JSON.stringify(a);
  }
  """ :: Foreign -> String

foreign import undefined :: forall a. a

unsafePrintId :: forall a. a -> a
unsafePrintId o = unsafePerformEff $ do
  trace <<< showForeign <<< toForeign $ o
  return o
