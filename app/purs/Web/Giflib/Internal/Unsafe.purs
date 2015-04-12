module Web.Giflib.Internal.Unsafe
  ( unsafePerformEff
  , unsafePrintId
  ) where

import Data.Foreign (Foreign(), toForeign)
import Debug.Trace (trace)
import Control.Monad.Eff (Eff())


foreign import unsafePerformEff
    """
    function unsafePerformEff(f) {
      return function() {
        return f;
      };
    }
    """ :: forall a eff. Eff eff a -> a

foreign import showForeign
  """
    function showForeign(a){
      return JSON.stringify(a);
    }
  """ :: Foreign -> String

unsafePrintId :: forall a. a -> a
unsafePrintId o = unsafePerformEff $ do
  trace <<< showForeign <<< toForeign $ o
  return o
